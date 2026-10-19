use extendr_api::prelude::*;
use extendr_api::SEXP;
use rayon::prelude::*;
use rustc_hash::{FxBuildHasher, FxHashMap as HashMap};
use std::hash::{BuildHasher, Hash};
use std::os::raw::{c_char, c_int, c_uint};
use std::sync::{Arc, Mutex};

// Public R API that extendr doesn't wrap. SEXP is opaque, so the lint is moot.
#[allow(improper_ctypes)]
extern "C" {
    fn STRING_PTR_RO(x: SEXP) -> *const SEXP;
    fn Rf_getCharCE(x: SEXP) -> c_int;
    fn R_CHAR(x: SEXP) -> *const c_char;
    fn LENGTH(x: SEXP) -> c_int;
    fn Rf_mkCharLenCE(s: *const c_char, len: c_int, enc: c_uint) -> SEXP;
    fn Rf_allocVector(t: c_uint, n: isize) -> SEXP;
    fn INTEGER(x: SEXP) -> *mut c_int;
    fn SET_STRING_ELT(x: SEXP, i: isize, v: SEXP);
    fn SET_VECTOR_ELT(x: SEXP, i: isize, v: SEXP) -> SEXP;
    static R_NaString: SEXP;
}

const NA_INT: i32 = i32::MIN;
const NA_BITS: u64 = 0x7FF00000000007A2; // R's NA_real_: a NaN with 1954 in its low word
const NA_REAL: f64 = f64::from_bits(NA_BITS);

/// A pool of `nthreads` threads (0 = `RAYON_NUM_THREADS`, else all cores).
/// Starting threads costs about 100us, so pools are kept between calls, one
/// per requested count. Waking a thread costs about 10us, so callers keep
/// small inputs on fewer threads with `with_min_len()` rather than a smaller
/// pool: one pool per input size would pile up idle threads. A forked child
/// (`parallel::mclapply()`) inherits the pool structs but not their threads,
/// so a new pid starts over, leaking the dead pools rather than waiting on
/// them.
fn pool(nthreads: i32) -> Arc<rayon::ThreadPool> {
    static POOLS: Mutex<Option<(u32, Vec<(usize, Arc<rayon::ThreadPool>)>)>> = Mutex::new(None);
    // More threads than cores only add overhead, and capping them keeps
    // the pools from piling up past the system's limit on threads.
    let cores = std::thread::available_parallelism().map_or(1, |n| n.get());
    let n = if nthreads > 0 {
        nthreads as usize
    } else {
        std::env::var("RAYON_NUM_THREADS")
            .ok()
            .and_then(|s| s.parse().ok())
            .filter(|&n| n > 0)
            .unwrap_or(cores)
    }
    .min(cores);
    let mut cache = POOLS.lock().unwrap_or_else(|e| e.into_inner());
    let pid = std::process::id();
    let pools = match cache.as_mut() {
        Some((p, pools)) if *p == pid => pools,
        _ => {
            std::mem::forget(cache.take());
            &mut cache.insert((pid, Vec::new())).1
        }
    };
    if let Some((_, p)) = pools.iter().find(|(m, _)| *m == n) {
        return p.clone();
    }
    let p = Arc::new(
        rayon::ThreadPoolBuilder::new()
            .num_threads(n)
            .build()
            .unwrap(),
    );
    pools.push((n, p.clone()));
    p
}

/// Elements per thread below which `match()` gains nothing from another.
const MATCH_PER_THREAD: usize = 10_000;

/// `match(x, table)` for any element types whose keys are equal exactly
/// when base R considers the elements equal. 1-based, first match wins, no
/// match gives `nomatch`.
fn par_match<X: Sync, T: Sync, K: Copy + Hash + Eq + Send + Sync>(
    x: &[X],
    table: &[T],
    xkey: impl Fn(&X) -> K + Sync,
    tkey: impl Fn(&T) -> K + Sync,
    nomatch: Rint,
    pool: &rayon::ThreadPool,
) -> Integers {
    let nomatch = Option::<i32>::from(nomatch).unwrap_or(NA_INT);
    if x.is_empty() {
        return Integers::new(0);
    }
    if x.len() <= table.len() / SCAN_RATIO {
        return scan(x, table, xkey, tkey, nomatch, pool);
    }
    let maps = pool.install(|| build(table, &tkey));
    let mut out = Integers::new(x.len());
    let slice: &mut [Rint] = &mut out; // threads may share this, not `out`
    pool.install(|| {
        slice
            .par_iter_mut()
            .zip(x)
            .with_min_len(MATCH_PER_THREAD)
            .for_each(|(o, v)| {
                let k = xkey(v);
                *o = (*maps[bucket(&k, maps.len())].get(&k).unwrap_or(&nomatch)).into();
            })
    });
    out
}

/// `x` at least this many times shorter than the table is hashed instead of
/// the table, which is then scanned: at 1e7 table elements the two paths
/// tie near 1e6, and `scan()` takes 3ms for one element where `build()`
/// takes 30ms.
const SCAN_RATIO: usize = 10;

/// `par_match()` for a short `x`: hash `x` and scan the table, in one chunk
/// per thread, each recording the first position it sees for every element
/// of `x`. The table is never hashed, so this saves the time and memory of
/// `build()`. Base does this for one element: a lookup of a few keys into a
/// large table.
fn scan<X: Sync, T: Sync, K: Copy + Hash + Eq + Send + Sync>(
    x: &[X],
    table: &[T],
    xkey: impl Fn(&X) -> K + Sync,
    tkey: impl Fn(&T) -> K + Sync,
    nomatch: i32,
    pool: &rayon::ThreadPool,
) -> Integers {
    // Each key gets the index of its first element in `x`.
    let mut ids = map(x.len(), x.len());
    for (i, v) in x.iter().enumerate().rev() {
        ids.insert(xkey(v), i);
    }
    let first = pool.install(|| {
        let chunk = table
            .len()
            .div_ceil(rayon::current_num_threads())
            .max(MATCH_PER_THREAD);
        table
            .par_chunks(chunk)
            .enumerate()
            .map(|(c, t)| {
                let mut first = vec![i32::MAX; x.len()];
                for (j, v) in t.iter().enumerate() {
                    if let Some(&i) = ids.get(&tkey(v)) {
                        if first[i] == i32::MAX {
                            first[i] = (c * chunk + j) as i32 + 1;
                        }
                    }
                }
                first
            })
            .reduce(
                || vec![i32::MAX; x.len()],
                |mut a, b| {
                    a.iter_mut().zip(b).for_each(|(a, b)| *a = (*a).min(b));
                    a
                },
            )
    });
    let mut out = Integers::new(x.len());
    let slice: &mut [Rint] = &mut out;
    slice.iter_mut().zip(x).for_each(|(o, v)| {
        let p = first[ids[&xkey(v)]];
        *o = (if p == i32::MAX { nomatch } else { p }).into();
    });
    out
}

/// Table position (1-based) by key, first position winning. A large table
/// is split by hash into one map per bucket, so the build runs in parallel
/// too: each bucket's map is filled from its runs in table order.
fn build<T: Sync, K: Copy + Hash + Eq + Send + Sync>(
    table: &[T],
    key: &(impl Fn(&T) -> K + Sync),
) -> Vec<HashMap<K, i32>> {
    let Some((nb, runs)) = partition(table, key) else {
        let mut m = map(table.len(), table.len());
        // Reverse so earlier positions overwrite later ones.
        for (i, v) in table.iter().enumerate().rev() {
            m.insert(key(v), i as i32 + 1);
        }
        return vec![m];
    };
    (0..nb)
        .into_par_iter()
        .map(|b| {
            let n = runs.iter().map(|r| r[b].len()).sum();
            let mut m = map(n, table.len());
            for (k, i) in runs.iter().flat_map(|r| &r[b]) {
                m.entry(*k).or_insert(*i);
            }
            m
        })
        .collect()
}

/// Below this many table elements, one map built on one thread is fastest.
const PARTITION_MIN: usize = 1 << 16;

/// A large table split by hash into `nb` buckets, a power of two: each
/// chunk of the table, 4 per thread, is sorted into one run per bucket of
/// (key, 1-based position), in table order. `None` for a table below
/// `PARTITION_MIN`, or too small for two threads.
#[allow(clippy::type_complexity)]
fn partition<T: Sync, K: Copy + Hash + Eq + Send + Sync>(
    table: &[T],
    key: &(impl Fn(&T) -> K + Sync),
) -> Option<(usize, Vec<Vec<Vec<(K, i32)>>>)> {
    let nt = rayon::current_num_threads().min(table.len() / MATCH_PER_THREAD);
    if table.len() < PARTITION_MIN || nt <= 1 {
        return None;
    }
    let nb = (nt * 4).next_power_of_two().min(256);
    let chunk = table.len().div_ceil(nt * 4);
    let runs = table
        .par_chunks(chunk)
        .enumerate()
        .map(|(c, t)| {
            let mut runs: Vec<Vec<(K, i32)>> =
                (0..nb).map(|_| Vec::with_capacity(t.len() / nb)).collect();
            for (j, v) in t.iter().enumerate() {
                let k = key(v);
                runs[bucket(&k, nb)].push((k, (c * chunk + j) as i32 + 1));
            }
            runs
        })
        .collect();
    Some((nb, runs))
}

/// `duplicated(x)`: whether an equal element comes earlier, or later if
/// `from_last`.
fn duplicated<X: Sync, K: Copy + Hash + Eq + Send + Sync>(
    x: &[X],
    key: impl Fn(&X) -> K + Sync,
    from_last: bool,
    pool: &rayon::ThreadPool,
) -> Logicals {
    let mut out = Logicals::new(x.len()); // all FALSE
    mark(
        x,
        key,
        from_last,
        false,
        Rbool::true_value(),
        &mut out,
        pool,
    );
    out
}

/// Sets `out` to `v` at the first element of each key in `x` if `first`,
/// else at the others, which `duplicated()` marks; the first is the last if
/// `from_last`. A large `x` is split by hash as in `build()`, and each
/// bucket marks its own keys, so no second pass over `x` looks them up, as
/// `par_match(x, x)` would.
fn mark<X: Sync, K: Copy + Hash + Eq + Send + Sync, V: Copy + Send + Sync>(
    x: &[X],
    key: impl Fn(&X) -> K + Sync,
    from_last: bool,
    first: bool,
    v: V,
    out: &mut [V],
    pool: &rayon::ThreadPool,
) {
    pool.install(|| {
        let Some((nb, runs)) = partition(x, &key) else {
            let mut seen = map(x.len(), x.len());
            let mut mark = |(o, e): (&mut V, &X)| {
                if seen.insert(key(e), ()).is_none() == first {
                    *o = v
                }
            };
            if from_last {
                out.iter_mut().zip(x).rev().for_each(&mut mark)
            } else {
                out.iter_mut().zip(x).for_each(&mut mark)
            }
            return;
        };
        let out = Shared(out.as_mut_ptr());
        (0..nb).into_par_iter().for_each(|b| {
            let n = runs.iter().map(|r| r[b].len()).sum();
            let mut seen = map(n, x.len());
            let mut mark = |&(k, i): &(K, i32)| {
                if seen.insert(k, ()).is_none() == first {
                    // Each position is in one bucket only, so no two
                    // threads write the same element.
                    unsafe { *out.get().add(i as usize - 1) = v }
                }
            };
            let run = runs.iter().flat_map(|r| &r[b]);
            if from_last {
                run.rev().for_each(&mut mark)
            } else {
                run.for_each(&mut mark)
            }
        })
    });
}

/// A pointer that threads may share, for writes to disjoint elements.
struct Shared<T>(*mut T);
unsafe impl<T> Sync for Shared<T> {}
impl<T> Shared<T> {
    // A method, so closures capture the whole struct, not the raw pointer.
    fn get(&self) -> *mut T {
        self.0
    }
}

/// A map for `n` keys, one of the maps for `total` keys. hashbrown rounds
/// its capacity up to a power of two and fills it to 7/8, and a lookup that
/// misses in a map that full probes several groups: 2-5x slower than in one
/// half full, at every size. Below 2^18 keys in all the maps fit in cache
/// and their probes are the whole cost, so each gets twice the room. Above,
/// memory is the cost, and doubling it costs page faults (1e6 keys into
/// 1e6: 4.8ms to 6.2ms), so each gets only enough room to stay under 2/3
/// full.
fn map<K, V>(n: usize, total: usize) -> HashMap<K, V> {
    let cap = if total < 1 << 18 { 2 * n } else { n + n / 3 };
    HashMap::with_capacity_and_hasher(cap, FxBuildHasher)
}

/// Which of `nb` maps (a power of two) holds `k`: the top bits of its hash,
/// mixed once more so they don't depend on how the map's own hasher is made.
fn bucket<K: Hash>(k: &K, nb: usize) -> usize {
    if nb == 1 {
        return 0;
    }
    let h = FxBuildHasher.hash_one(k).wrapping_mul(0x9E3779B97F4A7C15);
    (h >> (64 - nb.trailing_zeros())) as usize
}

/// @noRd
#[extendr]
fn par_match_int(x: &[i32], table: &[i32], nomatch: Rint, nthreads: i32) -> Integers {
    let key = |v: &i32| *v;
    par_match(x, table, key, key, nomatch, &pool(nthreads))
}

/// `m[codes]` for factor codes, with `NA` codes taking `m`'s last element.
/// `m` is integer or logical: a result per level, from a function that only
/// had to run on the levels. Codes outside the levels error, as in base.
/// @noRd
#[extendr]
fn index_codes(codes: &[i32], m: Robj, nthreads: i32) -> extendr_api::Result<Robj> {
    let ok = if let Ok(m) = Logicals::try_from(&m) {
        let mut out = Logicals::new(codes.len());
        index(codes, &m, &mut out, nthreads).map(|_| out.into())
    } else {
        let m = Integers::try_from(&m)?;
        let mut out = Integers::new(codes.len());
        index(codes, &m, &mut out, nthreads).map(|_| out.into())
    };
    ok.ok_or_else(|| Error::Other("malformed factor".into()))
}

/// `index_codes()` for either type; `None` if a code is outside the levels.
fn index<T: Copy + Send + Sync>(
    codes: &[i32],
    m: &[T],
    out: &mut [T],
    nthreads: i32,
) -> Option<()> {
    let (&na, levels) = m.split_last().unwrap();
    pool(nthreads).install(|| {
        out.par_iter_mut()
            .zip(codes)
            .with_min_len(MATCH_PER_THREAD)
            .try_for_each(|(o, &c)| {
                *o = if c == NA_INT {
                    na
                } else {
                    *levels.get((c as usize).wrapping_sub(1))?
                };
                Some(())
            })
    })
}

/// Base matches Dates by their printed day, so 1.5 matches 1: `as_date`
/// floors first. That only holds while Dates print as days, up to about
/// 1e11; beyond 1e10 this returns `NULL` and R falls back to base.
/// @noRd
#[extendr]
fn par_match_dbl(x: &[f64], table: &[f64], nomatch: Rint, nthreads: i32, as_date: bool) -> Robj {
    let pool = pool(nthreads);
    if !as_date {
        let key = |v: &f64| dbl_key(*v);
        return par_match(x, table, key, key, nomatch, &pool).into();
    }
    let far = |v: &f64| v.is_finite() && v.abs() > 1e10;
    let any_far = |s: &[f64]| s.par_iter().with_min_len(MATCH_PER_THREAD).any(far);
    if pool.install(|| any_far(table) || any_far(x)) {
        return ().into();
    }
    let key = |v: &f64| dbl_key(v.floor());
    par_match(x, table, key, key, nomatch, &pool).into()
}

/// One side integer, the other double: integers are converted as they are
/// hashed, rather than copied to a double vector first.
/// @noRd
#[extendr]
fn par_match_mixed(x: Robj, table: Robj, nomatch: Rint, nthreads: i32) -> Integers {
    let pool = pool(nthreads);
    let int = |v: &i32| dbl_key(if *v == NA_INT { NA_REAL } else { *v as f64 });
    let dbl = |v: &f64| dbl_key(*v);
    match (x.as_integer_slice(), table.as_real_slice()) {
        (Some(x), Some(table)) => par_match(x, table, int, dbl, nomatch, &pool),
        _ => {
            let (x, table) = (
                x.as_real_slice().unwrap(),
                table.as_integer_slice().unwrap(),
            );
            par_match(x, table, dbl, int, nomatch, &pool)
        }
    }
}

/// Bits that are equal exactly when base `match()` sees equal doubles:
/// `-0` equals `0`, every `NA` equals every `NA`, and every other `NaN`
/// equals every other `NaN`.
fn dbl_key(v: f64) -> u64 {
    if v == 0.0 {
        0
    } else if v.is_nan() {
        if v.to_bits() as u32 == 1954 {
            NA_BITS
        } else {
            f64::NAN.to_bits()
        }
    } else {
        v.to_bits()
    }
}

/// Matches strings by pointer: R stores each string once per encoding, so
/// equal pointers mean equal strings. That could miss a match only between
/// non-ASCII strings (ASCII is never marked with an encoding), so if either
/// side is all ASCII the pointers are enough. Otherwise every string is
/// checked, and this returns `NULL`, for R to fall back to base, if there
/// are latin1 or bytes strings, or non-ASCII strings both marked UTF-8 and
/// unmarked.
/// @noRd
#[extendr]
fn par_match_chr(x: Robj, table: Robj, nomatch: Rint, nthreads: i32) -> Robj {
    let (x, table) = unsafe { (charsxps(&x), charsxps(&table)) };
    let pool = pool(nthreads);
    if !pointers_suffice(x, table, &pool) {
        return ().into();
    }
    let key = |p: &usize| *p;
    par_match(x, table, key, key, nomatch, &pool).into()
}

/// Whether equal string pointers mean equal strings between `x` and
/// `table`, as `par_match_chr()` explains.
fn pointers_suffice(x: &[usize], table: &[usize], pool: &rayon::ThreadPool) -> bool {
    let ascii = |p: &usize| unsafe { bytes(*p).is_ascii() };
    let ce = |p: &usize| unsafe { Rf_getCharCE(*p as SEXP) };
    // The threads read the strings themselves, as par_detect_rs() does.
    // Reading each one is a cache miss, so this scans the shorter side.
    pool.install(|| {
        let both = || x.par_iter().chain(table).with_min_len(MATCH_PER_THREAD);
        if x.len() < table.len() { x } else { table }
            .par_iter()
            .with_min_len(MATCH_PER_THREAD)
            .all(ascii)
            || {
                // Encodings seen, as bits: 1 native, 2 UTF-8, 4 anything else.
                let seen = both()
                    .map(|p| 1u8 << ce(p).min(2))
                    .reduce(|| 0, |a, b| a | b);
                seen & 4 == 0 && (seen != 3 || both().all(|p| ce(p) == 1 || ascii(p)))
            }
    })
}

/// `pointers_suffice()` for R, which decides how to match each column of a
/// row before calling `par_match_rows_rs()`.
/// @noRd
#[extendr]
fn par_pointers_suffice(x: Robj, table: Robj, nthreads: i32) -> bool {
    let (x, table) = unsafe { (charsxps(&x), charsxps(&table)) };
    pointers_suffice(x, table, &pool(nthreads))
}

/// `duplicated(x)` for an integer, double, or string vector; `NULL` for
/// strings that `par_match_chr()` would leave to base.
/// @noRd
#[extendr]
fn par_duplicated_rs(x: Robj, from_last: bool, nthreads: i32) -> Robj {
    let pool = pool(nthreads);
    if let Some(v) = x.as_integer_slice() {
        duplicated(v, |v| *v, from_last, &pool).into()
    } else if let Some(v) = x.as_real_slice() {
        duplicated(v, |v| dbl_key(*v), from_last, &pool).into()
    } else {
        let v = unsafe { charsxps(&x) };
        if !pointers_suffice(v, v, &pool) {
            return ().into();
        }
        duplicated(v, |p| *p, from_last, &pool).into()
    }
}

/// `unique(x)` for an integer, double, or string vector, without
/// attributes; `NULL` for strings that `par_match_chr()` would leave to
/// base. The first occurrences are marked in a byte per element, as
/// `duplicated()` marks the others, and the marked elements then gathered
/// in parallel, where subsetting by the result of `duplicated()` in R reads
/// four bytes per element on one thread.
/// @noRd
#[extendr]
fn par_unique_rs(x: Robj, from_last: bool, nthreads: i32) -> Robj {
    let pool = pool(nthreads);
    fn firsts<T: Copy + Send + Sync, K: Copy + Hash + Eq + Send + Sync>(
        x: &[T],
        key: impl Fn(&T) -> K + Sync,
        from_last: bool,
        pool: &rayon::ThreadPool,
    ) -> Vec<T> {
        let mut keep = vec![false; x.len()];
        mark(x, key, from_last, true, true, &mut keep, pool);
        pool.install(|| {
            x.par_iter()
                .zip(keep)
                .with_min_len(MATCH_PER_THREAD)
                .filter_map(|(&e, k)| k.then_some(e))
                .collect()
        })
    }
    if let Some(v) = x.as_integer_slice() {
        Integers::from_values(firsts(v, |v| *v, from_last, &pool)).into()
    } else if let Some(v) = x.as_real_slice() {
        Doubles::from_values(firsts(v, |v| dbl_key(*v), from_last, &pool)).into()
    } else {
        let v = unsafe { charsxps(&x) };
        if !pointers_suffice(v, v, &pool) {
            return ().into();
        }
        let u = firsts(v, |p| *p, from_last, &pool);
        // Only R's thread may set the elements of a character vector.
        let out: Robj = Strings::new(u.len()).into();
        for (i, &p) in u.iter().enumerate() {
            unsafe { SET_STRING_ELT(out.get(), i as isize, p as SEXP) };
        }
        out
    }
}

/// One column of a row key, its keys equal exactly where `par_match()`
/// would match. R passes each pair of columns as two integer, two double,
/// or two string vectors whose pointers suffice; `Day` is a Date matched
/// by day.
enum Col<'a> {
    Int(&'a [i32]),
    Dbl(&'a [f64]),
    Day(&'a [f64]),
    Chr(&'a [usize]),
}

impl Col<'_> {
    fn key(&self, i: usize) -> u64 {
        match self {
            Col::Int(v) => v[i] as u32 as u64,
            Col::Dbl(v) => dbl_key(v[i]),
            Col::Day(v) => dbl_key(v[i].floor()),
            Col::Chr(v) => v[i] as u64,
        }
    }
}

/// The columns of a list, `day` marking those that are Dates.
fn columns<'a>(cols: &'a [Robj], day: &[Rbool]) -> Vec<Col<'a>> {
    cols.iter()
        .enumerate()
        .map(|(j, c)| {
            if let Some(v) = c.as_integer_slice() {
                Col::Int(v)
            } else if let Some(v) = c.as_real_slice() {
                if day.get(j).is_some_and(|d| d.is_true()) {
                    Col::Day(v)
                } else {
                    Col::Dbl(v)
                }
            } else {
                assert!(
                    c.is_string(),
                    "columns must be integer, double, or character"
                );
                Col::Chr(unsafe { charsxps(c) })
            }
        })
        .collect()
}

/// Row `i` of a list of columns, as a key: its hash is computed once, from
/// every column, and rows compare equal if every column does. A row of `x`
/// and a row of `table` compare by their own columns, which R made
/// compatible.
#[derive(Clone, Copy)]
struct Row<'a> {
    cols: &'a Vec<Col<'a>>,
    i: u32,
    h: u64,
}

fn row<'a>(cols: &'a Vec<Col<'a>>, i: u32) -> Row<'a> {
    let h = cols.iter().fold(0u64, |h, c| {
        (h.rotate_left(5) ^ c.key(i as usize)).wrapping_mul(0x517CC1B727220A95)
    });
    Row { cols, i, h }
}

impl Hash for Row<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.h)
    }
}

impl PartialEq for Row<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.h == other.h
            && self
                .cols
                .iter()
                .zip(other.cols)
                .all(|(a, b)| a.key(self.i as usize) == b.key(other.i as usize))
    }
}

impl Eq for Row<'_> {}

/// A row's key from one column.
fn key1<'a>(c: &'a [Col<'a>]) -> impl Fn(&u32) -> u64 + Sync + 'a {
    move |&i| c[0].key(i as usize)
}

/// A row's key from two columns.
fn key2<'a>(c: &'a [Col<'a>]) -> impl Fn(&u32) -> (u64, u64) + Sync + 'a {
    move |&i| (c[0].key(i as usize), c[1].key(i as usize))
}

/// `match()` on rows: for each row of `x`, the first row of `table` equal
/// to it in every column, or `nomatch`. Both are lists of columns, which R
/// prepares as `Col` describes; `day` marks columns of Dates. Returns
/// `NULL` if one of those holds a Date beyond 1e10 days, as
/// `par_match_dbl()` does.
/// @noRd
#[extendr]
fn par_match_rows_rs(x: List, table: List, day: &[Rbool], nomatch: Rint, nthreads: i32) -> Robj {
    let (x, table): (Vec<Robj>, Vec<Robj>) = (x.values().collect(), table.values().collect());
    let (xc, tc) = (columns(&x, day), columns(&table, day));
    let pool = pool(nthreads);
    let far = |c: &Col| match c {
        Col::Day(v) => v
            .par_iter()
            .with_min_len(MATCH_PER_THREAD)
            .any(|v| v.is_finite() && v.abs() > 1e10),
        _ => false,
    };
    if pool.install(|| xc.iter().chain(&tc).any(far)) {
        return ().into();
    }
    let (nx, nt) = (x[0].len(), table[0].len());
    // Rows are passed around by index, so par_match() can take them as
    // slices.
    let rows: Vec<u32> = (0..nx.max(nt) as u32).collect();
    let (xr, tr) = (&rows[..nx], &rows[..nt]);
    // Up to two columns, the keys themselves fit in the space a `Row`
    // takes, and comparing them needs no read of the columns at random rows:
    // with many repeated rows that halves the time.
    let out = match xc.len() {
        1 => par_match(xr, tr, key1(&xc), key1(&tc), nomatch, &pool),
        2 => par_match(xr, tr, key2(&xc), key2(&tc), nomatch, &pool),
        _ => par_match(xr, tr, |&i| row(&xc, i), |&i| row(&tc, i), nomatch, &pool),
    };
    out.into()
}

/// `duplicated()` on rows of a list of columns, prepared as for
/// `par_match_rows_rs()`, with no Dates matched by day.
/// @noRd
#[extendr]
fn par_duplicated_rows_rs(x: List, from_last: bool, nthreads: i32) -> Logicals {
    let x: Vec<Robj> = x.values().collect();
    let xc = columns(&x, &[]);
    let rows: Vec<u32> = (0..x[0].len() as u32).collect();
    let pool = pool(nthreads);
    // As in par_match_rows_rs(); R sends one column as a vector instead.
    if xc.len() == 2 {
        return duplicated(&rows, key2(&xc), from_last, &pool);
    }
    duplicated(&rows, |&i| row(&xc, i), from_last, &pool)
}

/// Strings per thread below which a regex gains nothing from another.
const REGEX_PER_THREAD: usize = 2500;

/// Compiling a regex costs about 70us, more than matching a few hundred
/// strings, so the last one is kept for calls that repeat the pattern.
fn regex(pattern: &str) -> extendr_api::Result<regex::bytes::Regex> {
    static LAST: Mutex<Option<(String, regex::bytes::Regex)>> = Mutex::new(None);
    let mut last = LAST.lock().unwrap_or_else(|e| e.into_inner());
    if let Some((p, re)) = last.as_ref() {
        if p == pattern {
            return Ok(re.clone());
        }
    }
    let re = regex::bytes::Regex::new(pattern).map_err(|e| Error::Other(e.to_string()))?;
    *last = Some((pattern.to_string(), re.clone()));
    Ok(re)
}

/// Runs `f` on each string of `x`, by its index and bytes, into `out`; `NA`
/// gives `na`. The threads read the strings themselves: R stores them
/// scattered in memory, so reading them on one thread would take most of
/// the time. R_CHAR() and LENGTH() only read memory; R doesn't promise they
/// are thread-safe, but data.table has read strings from threads for years.
/// Each chunk gets its own state from `init`, such as regex clones: threads
/// sharing one regex contend for its cache, and cloning per rayon task
/// costs more than it saves. Latin1 strings are converted, as `utf8()` says.
fn per_string<T: Clone + Send + Sync, S>(
    out: &mut [T],
    x: &[usize],
    na: T,
    nthreads: i32,
    init: impl Fn() -> S + Sync,
    f: impl Fn(&mut S, usize, &[u8]) -> T + Sync,
) {
    let na_ptr = unsafe { R_NaString } as usize;
    let run = |start: usize, out: &mut [T], x: &[usize]| {
        let mut s = init();
        let mut buf = Vec::new();
        for (j, (o, &p)) in out.iter_mut().zip(x).enumerate() {
            *o = if p == na_ptr {
                na.clone()
            } else {
                f(&mut s, start + j, unsafe { utf8(p, &mut buf) })
            };
        }
    };
    if x.len() < 2 * REGEX_PER_THREAD {
        return run(0, out, x);
    }
    pool(nthreads).install(|| {
        let nt = rayon::current_num_threads().min(x.len() / REGEX_PER_THREAD);
        let chunk = x.len().div_ceil(4 * nt);
        out.par_chunks_mut(chunk)
            .zip(x.par_chunks(chunk))
            .enumerate()
            .for_each(|(c, (out, x))| run(c * chunk, out, x))
    });
}

type Re = regex::bytes::Regex;

/// The regexes of a `pattern` that holds one for all strings, or one per
/// string: `res` holds each distinct pattern once, compiled on the threads
/// if there are several (`None` for `NA`), and `which` the one each string
/// takes, if there are several.
struct Patterns {
    res: Vec<Option<Re>>,
    which: Option<Vec<u32>>,
}

fn patterns(pattern: &Robj, fixed: bool, nthreads: i32) -> extendr_api::Result<Patterns> {
    let pattern = unsafe { charsxps(pattern) };
    if pattern.is_empty() {
        // As long as `x`, so there is nothing to match.
        return Ok(Patterns {
            res: Vec::new(),
            which: None,
        });
    }
    let mut distinct = Vec::new();
    let which: Option<Vec<u32>> = (pattern.len() > 1).then(|| {
        let mut ids = HashMap::default();
        pattern
            .iter()
            .map(|&p| {
                *ids.entry(p).or_insert_with(|| {
                    distinct.push(p);
                    distinct.len() as u32 - 1
                })
            })
            .collect()
    });
    if which.is_none() {
        distinct.push(pattern[0]);
    }
    let na_ptr = unsafe { R_NaString } as usize;
    let source = |p: usize| -> std::result::Result<Option<String>, String> {
        if p == na_ptr {
            return Ok(None);
        }
        let p = std::str::from_utf8(unsafe { bytes(p) }).map_err(|e| e.to_string())?;
        Ok(Some(if fixed {
            regex::escape(p)
        } else {
            p.to_string()
        }))
    };
    let res: Vec<Option<Re>> = if distinct.len() == 1 {
        let p = source(distinct[0]).map_err(Error::Other)?;
        vec![p.map(|p| regex(&p)).transpose()?]
    } else {
        pool(nthreads)
            .install(|| {
                distinct
                    .par_iter()
                    .map(|&p| {
                        source(p)?
                            .map(|p| Re::new(&p).map_err(|e| e.to_string()))
                            .transpose()
                    })
                    .collect::<std::result::Result<_, _>>()
            })
            .map_err(Error::Other)?
    };
    Ok(Patterns { res, which })
}

/// Runs `f` on each string of `x` with its regex from `p`. Each chunk clones
/// a regex the first time it needs it, and each clone builds its own cache;
/// an `NA` pattern gives `na`. With many distinct patterns, the strings are
/// sorted by pattern first, so each chunk meets only a few of them.
fn per_pattern<T: Clone + Send + Sync>(
    out: &mut [T],
    x: &[usize],
    p: &Patterns,
    na: T,
    nthreads: i32,
    f: impl Fn(&Re, &[u8]) -> T + Sync,
) {
    // `ids[i]` is the pattern of string `i`, or empty for one pattern.
    let run = |out: &mut [T], x: &[usize], ids: &[u32]| {
        per_string(
            out,
            x,
            na.clone(),
            nthreads,
            || vec![None; p.res.len()],
            |cache: &mut Vec<Option<Re>>, i, s| {
                let id = ids.get(i).map_or(0, |&id| id as usize);
                match &p.res[id] {
                    None => na.clone(),
                    Some(re) => f(cache[id].get_or_insert_with(|| re.clone()), s),
                }
            },
        )
    };
    let which = match &p.which {
        Some(w) if p.res.len() >= GROUP_MIN => w,
        w => return run(out, x, w.as_deref().unwrap_or(&[])),
    };
    // A counting sort: `order` lists the strings of the first pattern, then
    // those of the second, and so on.
    let mut start = vec![0usize; p.res.len() + 1];
    for &w in which {
        start[w as usize + 1] += 1;
    }
    for k in 1..start.len() {
        start[k] += start[k - 1];
    }
    let mut order = vec![0u32; x.len()];
    for (i, &w) in which.iter().enumerate() {
        order[start[w as usize]] = i as u32;
        start[w as usize] += 1;
    }
    let pool = pool(nthreads);
    let (xs, ids): (Vec<usize>, Vec<u32>) = pool.install(|| {
        order
            .par_iter()
            .map(|&i| (x[i as usize], which[i as usize]))
            .unzip()
    });
    let mut sorted = vec![na.clone(); x.len()];
    run(&mut sorted, &xs, &ids);
    let dst = Shared(out.as_mut_ptr());
    pool.install(|| {
        sorted
            .into_par_iter()
            .zip(order)
            .with_min_len(MATCH_PER_THREAD)
            // `order` holds each position once, so no two threads write the
            // same element.
            .for_each(|(v, i)| unsafe { *dst.get().add(i as usize) = v })
    });
}

/// Distinct patterns from which `per_pattern()` sorts the strings by
/// pattern. Sorting costs about 15ms per 1e6 strings and saves building
/// the regexes' caches again in every chunk: with 1e4 patterns in 1e6
/// strings, 0.63s become 0.1s, but with 10 it doubles the time, 14ms to
/// 29ms. The two tie near 300.
const GROUP_MIN: usize = 256;

/// `grepl(pattern, x)` with Rust's regex syntax, or a literal `pattern` if
/// `fixed`. `pattern` is one string, or one per string of `x`. Strings are
/// matched as bytes, so R should pass both through `enc2utf8()` first. `NA`
/// in either gives `NA`.
/// @noRd
#[extendr]
fn par_detect_rs(
    x: Robj,
    pattern: Robj,
    fixed: bool,
    nthreads: i32,
) -> extendr_api::Result<Logicals> {
    let p = patterns(&pattern, fixed, nthreads)?;
    let mut out = Logicals::new(x.len());
    per_pattern(
        &mut out,
        unsafe { charsxps(&x) },
        &p,
        Rbool::na_value(),
        nthreads,
        |re, s| re.is_match(s).into(),
    );
    Ok(out)
}

/// The number of non-overlapping matches of `pattern` in each string, as
/// `gregexpr()` finds them; otherwise as `par_detect_rs()`.
/// @noRd
#[extendr]
fn par_count_rs(
    x: Robj,
    pattern: Robj,
    fixed: bool,
    nthreads: i32,
) -> extendr_api::Result<Integers> {
    let p = patterns(&pattern, fixed, nthreads)?;
    let mut out = Integers::new(x.len());
    per_pattern(
        &mut out,
        unsafe { charsxps(&x) },
        &p,
        Rint::na(),
        nthreads,
        |re, s| {
            count(
                re.find_iter(s)
                    .filter(|m| !splits_char(s, m.start(), m.end())),
            )
        },
    );
    Ok(out)
}

/// Whether a match from `start` to `end` is an empty one between the bytes
/// of one UTF-8 character. Regexes on bytes and the automaton report those,
/// and R's functions, which match by character, don't.
fn splits_char(s: &[u8], start: usize, end: usize) -> bool {
    start == end && s.get(start).is_some_and(|b| b & 0xC0 == 0x80)
}

/// The length of an iterator as an R integer, capped at R's largest.
fn count(it: impl Iterator) -> Rint {
    (it.count().min(i32::MAX as usize) as i32).into()
}

/// A string of a result, found on the threads and made on R's thread, which
/// alone may make R strings: `Same` is the string of `x` itself, and `Span`
/// a part of it.
#[derive(Clone)]
enum Piece {
    Na,
    Same,
    Span(usize, usize),
    New(Vec<u8>),
}

/// The strings `out` describes, each made from the string of `x` at its
/// position. They are UTF-8, latin1 strings converted as `utf8()` does,
/// except those made from bytes-encoded strings, which stay bytes.
fn strings(x: &[usize], out: Vec<Piece>) -> extendr_api::Result<Robj> {
    let res: Robj = Strings::new(out.len()).into();
    for (i, (piece, &p)) in out.into_iter().zip(x).enumerate() {
        unsafe { SET_STRING_ELT(res.get(), i as isize, string(piece, p)?) };
    }
    Ok(res)
}

/// The string `piece` describes, made from the string at `p`, as in
/// `strings()`.
unsafe fn string(piece: Piece, p: usize) -> extendr_api::Result<SEXP> {
    let ce = Rf_getCharCE(p as SEXP);
    let make = |s: &[u8]| -> extendr_api::Result<SEXP> {
        let n = c_int::try_from(s.len()).map_err(|_| {
            Error::Other("A result is longer than R's limit of 2^31 - 1 bytes.".into())
        })?;
        Ok(Rf_mkCharLenCE(
            s.as_ptr() as *const c_char,
            n,
            if ce == 3 { 3 } else { 1 },
        ))
    };
    let mut buf = Vec::new();
    let s = utf8(p, &mut buf);
    // A latin1 string is made again, as UTF-8.
    let whole = || if ce == 2 { make(s) } else { Ok(p as SEXP) };
    Ok(match piece {
        Piece::Na => R_NaString,
        Piece::Same => whole()?,
        Piece::Span(a, b) if a == 0 && b == s.len() => whole()?,
        Piece::Span(a, b) => make(&s[a..b])?,
        Piece::New(v) => make(&v)?,
    })
}

/// An error unless every regex of `p` has a capture group `group`.
fn check_group(p: &Patterns, group: i32) -> extendr_api::Result<()> {
    if p.res
        .iter()
        .flatten()
        .any(|re| group as usize >= re.captures_len())
    {
        return Err(Error::Other(format!("`pattern` has no group {group}.")));
    }
    Ok(())
}

/// The part of a string a match or group spans, `Na` if it took no part.
fn span(m: Option<regex::bytes::Match>) -> Piece {
    m.map_or(Piece::Na, |m| Piece::Span(m.start(), m.end()))
}

/// The first match of `pattern` in each string, or its capture group
/// `group` (0 for the whole match); `NA` where there is none. Otherwise as
/// `par_detect_rs()`.
/// @noRd
#[extendr]
fn par_extract_rs(
    x: Robj,
    pattern: Robj,
    group: i32,
    fixed: bool,
    nthreads: i32,
) -> extendr_api::Result<Robj> {
    let p = patterns(&pattern, fixed, nthreads)?;
    check_group(&p, group)?;
    let g = group as usize;
    let x = unsafe { charsxps(&x) };
    let mut out = vec![Piece::Na; x.len()];
    per_pattern(&mut out, x, &p, Piece::Na, nthreads, |re, s| {
        span(if g == 0 {
            re.find(s)
        } else {
            re.captures(s).and_then(|c| c.get(g))
        })
    });
    strings(x, out)
}

/// Every match of `pattern` in each string, as `par_count_rs()` finds them,
/// or capture group `group` of each, `NA` where it took no part: a list of
/// character vectors, `NA` where `x` or `pattern` is `NA`. Otherwise as
/// `par_extract_rs()`.
/// @noRd
#[extendr]
fn par_extract_all_rs(
    x: Robj,
    pattern: Robj,
    group: i32,
    fixed: bool,
    nthreads: i32,
) -> extendr_api::Result<Robj> {
    let p = patterns(&pattern, fixed, nthreads)?;
    check_group(&p, group)?;
    let g = group as usize;
    let x = unsafe { charsxps(&x) };
    let mut out: Vec<Option<Vec<Piece>>> = vec![None; x.len()];
    per_pattern(&mut out, x, &p, None, nthreads, |re, s| {
        let whole = |m: &regex::bytes::Match| !splits_char(s, m.start(), m.end());
        Some(if g == 0 {
            re.find_iter(s)
                .filter(whole)
                .map(|m| span(Some(m)))
                .collect()
        } else {
            re.captures_iter(s)
                .filter(|c| whole(&c.get(0).unwrap()))
                .map(|c| span(c.get(g)))
                .collect()
        })
    });
    // Only R's thread may make R strings. Each vector goes into the list
    // before its strings are made, which keeps it from R's garbage collector.
    let res: Robj = List::new(out.len()).into();
    for (i, (pieces, &p)) in out.into_iter().zip(x).enumerate() {
        unsafe {
            let v = Rf_allocVector(16, pieces.as_ref().map_or(1, |v| v.len()) as isize);
            SET_VECTOR_ELT(res.get(), i as isize, v);
            for (k, piece) in pieces.unwrap_or(vec![Piece::Na]).into_iter().enumerate() {
                SET_STRING_ELT(v, k as isize, string(piece, p)?);
            }
        }
    }
    Ok(res)
}

/// Every match of `pattern` in each string replaced by `replacement`, or
/// only the first unless `all`, in which `$1` or `${name}` stands for a
/// capture group unless `fixed`. Otherwise as `par_detect_rs()`.
/// @noRd
#[extendr]
fn par_replace_rs(
    x: Robj,
    pattern: Robj,
    replacement: Robj,
    fixed: bool,
    all: bool,
    nthreads: i32,
) -> extendr_api::Result<Robj> {
    let p = patterns(&pattern, fixed, nthreads)?;
    let rep = unsafe { bytes(charsxps(&replacement)[0]) };
    let x = unsafe { charsxps(&x) };
    let mut out = vec![Piece::Na; x.len()];
    // As `regex::bytes::Regex::replace_all()`, which expands groups only if
    // the replacement has a `$`, but without the empty matches inside a
    // character that it would also replace.
    let expand = !fixed && rep.contains(&b'$');
    per_pattern(&mut out, x, &p, Piece::Na, nthreads, |re, s| {
        let mut new = Vec::new();
        let mut last = None;
        let mut put = |new: &mut Vec<u8>, m: regex::bytes::Match| -> bool {
            if splits_char(s, m.start(), m.end()) {
                return false;
            }
            new.extend_from_slice(&s[last.unwrap_or(0)..m.start()]);
            last = Some(m.end());
            true
        };
        if expand {
            for c in re.captures_iter(s) {
                if put(&mut new, c.get(0).unwrap()) {
                    c.expand(rep, &mut new);
                    if !all {
                        break;
                    }
                }
            }
        } else {
            for m in re.find_iter(s) {
                if put(&mut new, m) {
                    new.extend_from_slice(rep);
                    if !all {
                        break;
                    }
                }
            }
        }
        match last {
            None => Piece::Same,
            Some(last) => {
                new.extend_from_slice(&s[last..]);
                Piece::New(new)
            }
        }
    });
    strings(x, out)
}

/// The terms of `par_match_term()` and the like: literal ones in one
/// automaton, or regexes in one set, which says which of them match a
/// string but not where or how often, so each also has its own regex to
/// count its matches. With `Bounds`, the regexes require no word character
/// on the sides it names, which the automaton's hits are checked for
/// afterwards.
#[derive(Clone)]
enum Terms {
    Fixed(aho_corasick::AhoCorasick),
    Regex(Vec<regex::bytes::RegexSet>, Vec<Re>),
}

/// The `Terms` for `terms`. Building an automaton takes about 0.9ms for
/// 1000 terms, most of a call below 1e4 strings, so the last one is kept
/// for calls that repeat the terms. They are compared by their string
/// pointers, as `par_match_chr()` compares strings, and a copy of the
/// vector keeps those strings alive: otherwise R could free one and put
/// another string at its address.
fn compile_terms(
    terms: &Robj,
    ignore_case: bool,
    bounds: Bounds,
    fixed: bool,
) -> extendr_api::Result<Terms> {
    struct Last(Robj, (bool, bool, Option<Bounds>), Terms);
    // Only R's thread ever touches it.
    unsafe impl Send for Last {}
    static LAST: Mutex<Option<Last>> = Mutex::new(None);
    // R calls can jump past Rust's cleanup on an error, so none run while
    // the lock is held, or it would never be released.
    let p = unsafe { charsxps(terms) };
    let copy = terms.duplicate();
    let key = (ignore_case, fixed, (!fixed).then_some(bounds));
    let mut last = LAST.lock().unwrap_or_else(|e| e.into_inner());
    if let Some(Last(t, k, m)) = last.as_ref() {
        if *k == key && unsafe { charsxps(t) } == p {
            return Ok(m.clone());
        }
    }
    let err = |e: &dyn std::fmt::Display| Error::Other(e.to_string());
    let m = if fixed {
        Terms::Fixed(
            aho_corasick::AhoCorasick::builder()
                .ascii_case_insensitive(ignore_case)
                .build(p.iter().map(|&p| unsafe { bytes(p) }))
                .map_err(|e| err(&e))?,
        )
    } else {
        let src = p
            .iter()
            .map(|&p| {
                let t = std::str::from_utf8(unsafe { bytes(p) }).map_err(|e| err(&e))?;
                if !bounds.start && !bounds.end {
                    return Ok(t.to_string());
                }
                // The term is checked on its own first: one with unbalanced
                // parentheses, such as `a)|(b`, would escape the wrapper.
                let ast = regex_syntax::ast::parse::Parser::new()
                    .parse_with_comments(t)
                    .map_err(|e| err(&e))?;
                // In `(?x)` mode, a comment at the end would swallow the `)`.
                let end = ast
                    .comments
                    .last()
                    .is_some_and(|c| c.span.end.offset == t.len());
                let end = if end { "\n" } else { "" };
                let before = if bounds.start { r"\b{start-half}" } else { "" };
                let after = if bounds.end { r"\b{end-half}" } else { "" };
                Ok(format!("{before}(?:{t}{end}){after}"))
            })
            .collect::<extendr_api::Result<Vec<_>>>()?;
        let res = src
            .iter()
            .map(|t| {
                regex::bytes::RegexBuilder::new(t)
                    .case_insensitive(ignore_case)
                    .build()
                    .map_err(|e| err(&e))
            })
            .collect::<extendr_api::Result<_>>()?;
        let set = regex::bytes::RegexSetBuilder::new(&src)
            .dfa_size_limit(DFA_SIZE)
            .case_insensitive(ignore_case)
            .build()
            .map_err(|e| err(&e))?;
        // One clone per core, as `Search` explains.
        let cores = std::thread::available_parallelism().map_or(1, |n| n.get());
        Terms::Regex(vec![set; cores], res)
    };
    *last = Some(Last(copy, key, m.clone()));
    Ok(m)
}

/// The lazy DFA cache of a regex set, per thread. At the default 2MB, a set
/// of 1000 terms fills it again and again: 1e5 strings took 4s and 1e6
/// took 57s, where 16MB gives about 1s and 1.4s. 64MB saves 15% more.
const DFA_SIZE: usize = 16 << 20;

/// A chunk's searcher for `Terms`. The automaton has no cache, so threads
/// share it. Each thread takes its own clone of the regex set, by its index
/// in the pool (only one pool runs at a time), and the clones are kept with
/// the terms, so their caches stay built between calls: building the DFA
/// is most of the time for many terms, and a clone per chunk took 2-3x
/// longer for 1000 terms, while one set shared by all threads took 2x
/// longer for 10, from contention on its caches. A term's regex is cloned
/// the first time a chunk counts its matches, as in `per_pattern()`.
struct Search<'a> {
    terms: &'a Terms,
    res: Vec<Option<Re>>,
}

impl<'a> Search<'a> {
    fn new(terms: &'a Terms) -> Self {
        let res = match terms {
            Terms::Fixed(_) => Vec::new(),
            Terms::Regex(_, res) => vec![None; res.len()],
        };
        Search { terms, res }
    }

    /// Calls `f` with the index of each term found in `s` until it returns
    /// `false`. With `every`, once per hit: for literal terms every
    /// occurrence, also where they overlap, as `hits()` finds them, and for
    /// regexes every match, as `par_count_rs()` finds them. Otherwise at
    /// least once per term found, in no particular order.
    fn each(&mut self, s: &[u8], bounds: Bounds, every: bool, mut f: impl FnMut(u32) -> bool) {
        match self.terms {
            Terms::Fixed(ac) => {
                for t in hits(ac, s, bounds) {
                    if !f(t) {
                        return;
                    }
                }
            }
            Terms::Regex(sets, res) => {
                let set = &sets[rayon::current_thread_index().unwrap_or(0) % sets.len()];
                // The regexes' `\b` doesn't see an apostrophe join a match to
                // a word, so where there may be one, matches are checked.
                // 0x99 ends the curly apostrophe, and some other characters.
                // ponytail: a match that fails this check hides any other
                // match at the same start, as "won" does "won't" in
                // "won|won't"; list the longer alternative first.
                let check =
                    (bounds.start || bounds.end) && s.iter().any(|&b| b == b'\'' || b == 0x99);
                for t in set.matches(s).iter() {
                    let n = if every || check {
                        let re = self.res[t].get_or_insert_with(|| res[t].clone());
                        let ok = re.find_iter(s).filter(|m| {
                            !splits_char(s, m.start(), m.end())
                                && !(check && bounds.joins(s, m.start(), m.end()))
                        });
                        if every {
                            ok.count()
                        } else {
                            ok.take(1).count()
                        }
                    } else {
                        1
                    };
                    for _ in 0..n {
                        if !f(t as u32) {
                            return;
                        }
                    }
                }
            }
        }
    }
}

/// The index of each term found in `s`, in the order the automaton finds
/// them: every hit, also where hits overlap, unless it runs on into a word
/// on a side that `bounds` names.
fn hits<'a>(
    ac: &'a aho_corasick::AhoCorasick,
    s: &'a [u8],
    bounds: Bounds,
) -> impl Iterator<Item = u32> + 'a {
    ac.find_overlapping_iter(s)
        .filter(move |m| !splits_char(s, m.start(), m.end()))
        .filter(move |m| !bounds.joins(s, m.start(), m.end()))
        .map(|m| m.pattern().as_u32())
}

/// The sides of a hit where `whole_word` wants a word to end: both for
/// `TRUE`, only the start for `"start"`, so the term may begin a longer
/// word.
#[derive(Clone, Copy, PartialEq)]
struct Bounds {
    start: bool,
    end: bool,
}

impl Bounds {
    /// Whether the hit `s[i..j]` runs on into a word on a side that must
    /// end one: a word character touches it, or an apostrophe joins it to
    /// one, as in "won't", where "won" isn't a whole word.
    fn joins(self, s: &[u8], i: usize, j: usize) -> bool {
        let hit = &s[i..j];
        let before = &s[..i];
        let after = &s[j..];
        let start = || match last_char(before) {
            Some(c @ ('\'' | '\u{2019}')) => {
                is_word(first_char(hit)) && is_word(last_char(&before[..i - c.len_utf8()]))
            }
            c => is_word(c),
        };
        let end = || match first_char(after) {
            Some(c @ ('\'' | '\u{2019}')) => {
                is_word(last_char(hit)) && is_word(first_char(&after[c.len_utf8()..]))
            }
            c => is_word(c),
        };
        (self.start && start()) || (self.end && end())
    }
}

/// For each string, the 1-based index of the first of `terms` it contains,
/// or `NA`. One automaton finds every occurrence of every term in a single
/// pass, and the lowest index wins, so term order sets priority, not where
/// in the string a term occurs. With `word_start` and `word_end`, a hit
/// counts only if it doesn't run on into a word on those sides, as
/// `Bounds::joins()` says. Unless `fixed`, terms are
/// regexes, found in one pass by a set. Strings are matched as bytes, so R
/// should pass both sides through `enc2utf8()` first. `NA` gives `NA`.
/// @noRd
#[extendr]
fn par_match_term_rs(
    x: Robj,
    terms: Robj,
    ignore_case: bool,
    word_start: bool,
    word_end: bool,
    fixed: bool,
    nthreads: i32,
) -> extendr_api::Result<Integers> {
    let bounds = Bounds {
        start: word_start,
        end: word_end,
    };
    let m = compile_terms(&terms, ignore_case, bounds, fixed)?;
    let mut out = Integers::new(x.len());
    per_string(
        &mut out,
        unsafe { charsxps(&x) },
        Rint::na(),
        nthreads,
        || Search::new(&m),
        |search, _, s| {
            let mut best = u32::MAX;
            search.each(s, bounds, false, |t| {
                best = best.min(t);
                best != 0
            });
            if best == u32::MAX {
                Rint::na()
            } else {
                (best as i32 + 1).into()
            }
        },
    );
    Ok(out)
}

/// For each string, how many hits of `terms` it contains: every occurrence
/// of every literal term, also where they overlap, or every match of every
/// regex; otherwise as `par_match_term_rs()`.
/// @noRd
#[extendr]
fn par_count_term_rs(
    x: Robj,
    terms: Robj,
    ignore_case: bool,
    word_start: bool,
    word_end: bool,
    fixed: bool,
    nthreads: i32,
) -> extendr_api::Result<Integers> {
    let bounds = Bounds {
        start: word_start,
        end: word_end,
    };
    let m = compile_terms(&terms, ignore_case, bounds, fixed)?;
    let mut out = Integers::new(x.len());
    per_string(
        &mut out,
        unsafe { charsxps(&x) },
        Rint::na(),
        nthreads,
        || Search::new(&m),
        |search, _, s| {
            let mut n = 0usize;
            search.each(s, bounds, true, |_| {
                n += 1;
                true
            });
            (n.min(i32::MAX as usize) as i32).into()
        },
    );
    Ok(out)
}

/// For each string, the 1-based indices of every term it contains, in
/// `terms` order, each once; `NA` for `NA`. Otherwise as
/// `par_match_term_rs()`.
/// @noRd
#[extendr]
fn par_which_term_rs(
    x: Robj,
    terms: Robj,
    ignore_case: bool,
    word_start: bool,
    word_end: bool,
    fixed: bool,
    nthreads: i32,
) -> extendr_api::Result<Robj> {
    let bounds = Bounds {
        start: word_start,
        end: word_end,
    };
    let m = compile_terms(&terms, ignore_case, bounds, fixed)?;
    let x = unsafe { charsxps(&x) };
    let mut out: Vec<Option<Vec<u32>>> = vec![None; x.len()];
    per_string(
        &mut out,
        x,
        None,
        nthreads,
        || Search::new(&m),
        |search, _, s| {
            let mut t = Vec::new();
            search.each(s, bounds, false, |i| {
                t.push(i);
                true
            });
            t.sort_unstable();
            t.dedup();
            Some(t)
        },
    );
    // Only R's thread may make R vectors.
    let res: Robj = List::new(out.len()).into();
    for (i, t) in out.into_iter().enumerate() {
        unsafe {
            let v = Rf_allocVector(13, t.as_ref().map_or(1, |t| t.len()) as isize);
            // No slice: R gives an empty vector the misaligned data pointer 1.
            let ints = INTEGER(v);
            match t {
                None => *ints = NA_INT,
                Some(t) => t
                    .iter()
                    .enumerate()
                    .for_each(|(k, &t)| *ints.add(k) = t as i32 + 1),
            }
            SET_VECTOR_ELT(res.get(), i as isize, v);
        }
    }
    Ok(res)
}

/// `findInterval(x, vec)`: how many of the sorted `vec` are at most each
/// `x` (below it, if `left_open`), with base's adjustments at the ends.
/// `NaN` gives `NA`. `vec` must not be empty.
/// @noRd
#[extendr]
fn par_find_interval_rs(
    x: &[f64],
    vec: &[f64],
    rightmost_closed: bool,
    all_inside: bool,
    left_open: bool,
    nthreads: i32,
) -> Integers {
    let n = vec.len();
    let find = |x: f64| -> Rint {
        if x.is_nan() {
            return Rint::na();
        }
        let closed = |edge: f64| rightmost_closed && x == edge;
        let i = if left_open {
            match vec.partition_point(|&v| v < x) {
                0 if all_inside || closed(vec[0]) => 1,
                i if i == n && all_inside => n - 1,
                i => i,
            }
        } else {
            match vec.partition_point(|&v| v <= x) {
                i if i == n && (all_inside || closed(vec[n - 1])) => n - 1,
                0 if all_inside => 1,
                i => i,
            }
        };
        (i as i32).into()
    };
    let mut out = Integers::new(x.len());
    let slice: &mut [Rint] = &mut out;
    pool(nthreads).install(|| {
        slice
            .par_iter_mut()
            .zip(x)
            .with_min_len(MATCH_PER_THREAD)
            .for_each(|(o, &v)| *o = find(v))
    });
    out
}

/// Whether a character counts as part of a word for `whole_word`: a letter
/// or digit in any script, or `_`, as in a Unicode `\w`. `None` (the edge of
/// the string, or bytes that aren't UTF-8) doesn't.
fn is_word(c: Option<char>) -> bool {
    c.is_some_and(|c| c.is_alphanumeric() || c == '_')
}

/// The first character of UTF-8 bytes.
fn first_char(s: &[u8]) -> Option<char> {
    s.utf8_chunks().next()?.valid().chars().next()
}

/// The last character of UTF-8 bytes. A character takes at most 4 bytes, so
/// only those are decoded; a cut-off character at their start is skipped.
fn last_char(s: &[u8]) -> Option<char> {
    let last = s[s.len().saturating_sub(4)..].utf8_chunks().last()?;
    if last.invalid().is_empty() {
        last.valid().chars().next_back()
    } else {
        None
    }
}

/// A string's bytes, by its pointer, as UTF-8: a latin1 string is converted
/// into `buf` as `enc2utf8()` converts it, so R needn't run that first, on
/// one thread. R reads latin1 as Windows-1252, and writes the 5 bytes that
/// has no character for as `<81>` and the like.
unsafe fn utf8(p: usize, buf: &mut Vec<u8>) -> &[u8] {
    // R never marks an ASCII string as latin1.
    if Rf_getCharCE(p as SEXP) != 2 {
        return bytes(p);
    }
    buf.clear();
    for &b in bytes(p) {
        let c = if (0x80..0xA0).contains(&b) {
            CP1252[b as usize - 0x80]
        } else {
            b as char
        };
        if c == '\0' {
            buf.extend_from_slice(format!("<{b:02x}>").as_bytes());
        } else {
            buf.extend_from_slice(c.encode_utf8(&mut [0; 4]).as_bytes());
        }
    }
    buf
}

/// Windows-1252 from 0x80 to 0x9F, where latin1 has control characters;
/// `'\0'` where it has none either.
#[rustfmt::skip]
const CP1252: [char; 32] = [
        '\u{20AC}', '\0', '\u{201A}', '\u{0192}', '\u{201E}', '\u{2026}', '\u{2020}', '\u{2021}',
        '\u{02C6}', '\u{2030}', '\u{0160}', '\u{2039}', '\u{0152}', '\0', '\u{017D}', '\0', '\0',
        '\u{2018}', '\u{2019}', '\u{201C}', '\u{201D}', '\u{2022}', '\u{2013}', '\u{2014}',
        '\u{02DC}', '\u{2122}', '\u{0161}', '\u{203A}', '\u{0153}', '\0', '\u{017E}', '\u{0178}',
];

/// A character vector's string pointers, as integers that threads can share.
unsafe fn charsxps(x: &Robj) -> &[usize] {
    if x.len() == 0 {
        return &[];
    }
    std::slice::from_raw_parts(STRING_PTR_RO(x.get()) as *const usize, x.len())
}

/// A string's bytes, by its pointer.
unsafe fn bytes<'a>(p: usize) -> &'a [u8] {
    let p = p as SEXP;
    std::slice::from_raw_parts(R_CHAR(p) as *const u8, LENGTH(p) as usize)
}

extendr_module! {
    mod parvenu;
    fn par_match_int;
    fn index_codes;
    fn par_match_dbl;
    fn par_match_mixed;
    fn par_match_chr;
    fn par_pointers_suffice;
    fn par_duplicated_rs;
    fn par_match_rows_rs;
    fn par_duplicated_rows_rs;
    fn par_detect_rs;
    fn par_count_rs;
    fn par_extract_rs;
    fn par_replace_rs;
    fn par_match_term_rs;
    fn par_count_term_rs;
    fn par_which_term_rs;
    fn par_find_interval_rs;
    fn par_unique_rs;
    fn par_extract_all_rs;
}
