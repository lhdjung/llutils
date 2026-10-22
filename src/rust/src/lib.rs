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
    fn Rf_setAttrib(x: SEXP, name: SEXP, v: SEXP) -> SEXP;
    static R_NaString: SEXP;
    static R_DimSymbol: SEXP;
    static R_DimNamesSymbol: SEXP;
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
/// `group` (0 for the whole match); `NA` where there is none. With several
/// groups, a string vector of the strings' first group, then their second,
/// and so on: a matrix for R to shape. Otherwise as `par_detect_rs()`.
/// @noRd
#[extendr]
fn par_extract_rs(
    x: Robj,
    pattern: Robj,
    group: &[i32],
    fixed: bool,
    nthreads: i32,
) -> extendr_api::Result<Robj> {
    let p = patterns(&pattern, fixed, nthreads)?;
    for &g in group {
        check_group(&p, g)?;
    }
    let x = unsafe { charsxps(&x) };
    if let [g] = *group {
        let g = g as usize;
        let mut out = vec![Piece::Na; x.len()];
        per_pattern(&mut out, x, &p, Piece::Na, nthreads, |re, s| {
            span(if g == 0 {
                re.find(s)
            } else {
                re.captures(s).and_then(|c| c.get(g))
            })
        });
        return strings(x, out);
    }
    let na = vec![Piece::Na; group.len()];
    let mut out = vec![na.clone(); x.len()];
    per_pattern(&mut out, x, &p, na, nthreads, |re, s| {
        match re.captures(s) {
            None => vec![Piece::Na; group.len()],
            Some(c) => group.iter().map(|&g| span(c.get(g as usize))).collect(),
        }
    });
    let n = x.len();
    let res: Robj = Strings::new(n * group.len()).into();
    for (i, (pieces, &p)) in out.into_iter().zip(x).enumerate() {
        for (j, piece) in pieces.into_iter().enumerate() {
            unsafe { SET_STRING_ELT(res.get(), (i + n * j) as isize, string(piece, p)?) };
        }
    }
    Ok(res)
}

/// Where the first match of `pattern` is in each string, or its capture
/// group `group`: the positions of its first and last characters, 1-based,
/// as the starts of all strings, then their ends; `NA` where there is none.
/// Otherwise as `par_extract_rs()`.
/// @noRd
#[extendr]
fn par_locate_rs(
    x: Robj,
    pattern: Robj,
    group: i32,
    fixed: bool,
    nthreads: i32,
) -> extendr_api::Result<Integers> {
    let p = patterns(&pattern, fixed, nthreads)?;
    check_group(&p, group)?;
    let g = group as usize;
    let x = unsafe { charsxps(&x) };
    let mut out = vec![[NA_INT; 2]; x.len()];
    per_pattern(&mut out, x, &p, [NA_INT; 2], nthreads, |re, s| {
        let m = if g == 0 {
            re.find(s)
        } else {
            re.captures(s).and_then(|c| c.get(g))
        };
        m.map_or([NA_INT; 2], |m| {
            let chars = |s: &[u8]| s.iter().filter(|&&b| b & 0xC0 != 0x80).count() as i32;
            let start = chars(&s[..m.start()]) + 1;
            [start, start + chars(m.as_bytes()) - 1]
        })
    });
    let col = |k: usize| out.iter().map(move |o| o[k]);
    Ok(Integers::from_values(
        col(0).chain(col(1)).collect::<Vec<_>>(),
    ))
}

/// Where every match of `pattern` is in each string, as `par_count_rs()`
/// finds them, or capture group `group` of each: a list of integer
/// matrices with columns `start` and `end`, a row per match, as
/// `par_locate_rs()` counts positions, `NA` where the group took no part;
/// one row of `NA` where `x` or `pattern` is `NA`. Otherwise as
/// `par_extract_rs()`.
/// @noRd
#[extendr]
fn par_locate_all_rs(
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
    let mut out: Vec<Option<Vec<[i32; 2]>>> = vec![None; x.len()];
    per_pattern(&mut out, x, &p, None, nthreads, |re, s| {
        let whole = |m: &regex::bytes::Match| !splits_char(s, m.start(), m.end());
        let spans: Vec<Option<(usize, usize)>> = if g == 0 {
            re.find_iter(s)
                .filter(whole)
                .map(|m| Some((m.start(), m.end())))
                .collect()
        } else {
            re.captures_iter(s)
                .filter(|c| whole(&c.get(0).unwrap()))
                .map(|c| c.get(g).map(|m| (m.start(), m.end())))
                .collect()
        };
        // The matches come in order, so the characters are counted once.
        let (mut at, mut chars) = (0, 0);
        let mut upto = |b: usize| {
            chars += s[at..b].iter().filter(|&&c| c & 0xC0 != 0x80).count() as i32;
            at = b;
            chars
        };
        Some(
            spans
                .into_iter()
                .map(|m| m.map_or([NA_INT; 2], |(a, b)| [upto(a) + 1, upto(b)]))
                .collect(),
        )
    });
    // Only R's thread may make R vectors. The matrices share their
    // `dimnames`, and those with few rows their `dim`, as attributes may be.
    let dimnames: Robj = List::from_values([Robj::from(()), Robj::from(["start", "end"])]).into();
    let dims: Vec<Robj> = (0..=16).map(|m| Robj::from([m as i32, 2])).collect();
    let res: Robj = List::new(out.len()).into();
    for (i, rows) in out.into_iter().enumerate() {
        let rows = rows.unwrap_or(vec![[NA_INT; 2]]);
        unsafe {
            let m = rows.len();
            let v = Rf_allocVector(13, 2 * m as isize);
            SET_VECTOR_ELT(res.get(), i as isize, v);
            let ints = INTEGER(v);
            for (k, [a, b]) in rows.into_iter().enumerate() {
                *ints.add(k) = a;
                *ints.add(m + k) = b;
            }
            let dim = match dims.get(m) {
                Some(dim) => dim.clone(),
                None => Robj::from([m as i32, 2]),
            };
            Rf_setAttrib(v, R_DimSymbol, dim.get());
            Rf_setAttrib(v, R_DimNamesSymbol, dimnames.get());
        }
    }
    Ok(res)
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
/// `Fixed` holds whether to lowercase strings that aren't ASCII, for
/// `ignore_case`.
#[derive(Clone)]
enum Terms {
    Fixed(aho_corasick::AhoCorasick, bool),
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
) -> extendr_api::Result<Arc<Terms>> {
    // An `Arc`, not a clone: a cloned regex starts with empty caches.
    struct Last(Robj, (bool, bool, Option<Bounds>), Arc<Terms>);
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
    let m = Arc::new(if fixed {
        // The automaton ignores ASCII case itself; other letters are
        // lowercased on both sides.
        let (mut buf, mut at) = (Vec::new(), Vec::new());
        let p: Vec<std::borrow::Cow<[u8]>> = p
            .iter()
            .map(|&p| {
                let s = unsafe { bytes(p) };
                if ignore_case && !s.is_ascii() {
                    lower(s, &mut buf, &mut at);
                    buf.clone().into()
                } else {
                    s.into()
                }
            })
            .collect();
        Terms::Fixed(
            aho_corasick::AhoCorasick::builder()
                .ascii_case_insensitive(ignore_case)
                .build(p)
                .map_err(|e| err(&e))?,
            ignore_case,
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
    });
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
/// the first time a chunk counts its matches, as in `per_pattern()`. `buf`
/// and `at` hold a string lowercased by `lower()`.
struct Search<'a> {
    terms: &'a Terms,
    res: Vec<Option<Re>>,
    buf: Vec<u8>,
    at: Vec<u32>,
}

impl<'a> Search<'a> {
    fn new(terms: &'a Terms) -> Self {
        let res = match terms {
            Terms::Fixed(..) => Vec::new(),
            Terms::Regex(_, res) => vec![None; res.len()],
        };
        Search {
            terms,
            res,
            buf: Vec::new(),
            at: Vec::new(),
        }
    }

    /// Calls `f` with the index of each term found in `s`, and where in `s`
    /// it starts and ends, until it returns `false`. With `every`, once per
    /// hit: for literal terms every occurrence, also where they overlap, as
    /// `hits()` finds them, and for regexes every match, as `par_count_rs()`
    /// finds them. Otherwise at least once per term found, in no particular
    /// order, and for regexes at position 0.
    fn each(
        &mut self,
        s: &[u8],
        bounds: Bounds,
        every: bool,
        mut f: impl FnMut(u32, usize, usize) -> bool,
    ) {
        let Search {
            terms,
            res,
            buf,
            at,
        } = self;
        match terms {
            Terms::Fixed(ac, fold) => {
                let folded = *fold && !s.is_ascii();
                let t = if folded {
                    lower(s, buf, at);
                    &buf[..]
                } else {
                    s
                };
                for m in hits(ac, t, bounds) {
                    let (i, j) = if folded {
                        // A hit on part of a character that lowercases to
                        // several, as "i" in "İ", is none.
                        let inside = |k: usize| k > 0 && at[k] == at[k - 1];
                        if inside(m.start()) || inside(m.end()) {
                            continue;
                        }
                        (at[m.start()] as usize, at[m.end()] as usize)
                    } else {
                        (m.start(), m.end())
                    };
                    if !f(m.pattern().as_u32(), i, j) {
                        return;
                    }
                }
            }
            Terms::Regex(sets, regexes) => {
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
                    if !every && !check {
                        if !f(t as u32, 0, 0) {
                            return;
                        }
                        continue;
                    }
                    let re = res[t].get_or_insert_with(|| regexes[t].clone());
                    let ok = re.find_iter(s).filter(|m| {
                        !splits_char(s, m.start(), m.end())
                            && !(check && bounds.joins(s, m.start(), m.end()))
                    });
                    for m in ok.take(if every { usize::MAX } else { 1 }) {
                        if !f(t as u32, m.start(), m.end()) {
                            return;
                        }
                    }
                }
            }
        }
    }

    /// Appends the replacement for term `t`'s hit at `i` of `s`: `rep` as it
    /// is, or for a regex, with `$1` and the like expanded from the hit's
    /// groups, as in `par_replace_rs()`.
    fn expand(&mut self, s: &[u8], t: u32, i: usize, rep: &[u8], new: &mut Vec<u8>) {
        match self.terms {
            Terms::Regex(_, regexes) if rep.contains(&b'$') => {
                let t = t as usize;
                let re = self.res[t].get_or_insert_with(|| regexes[t].clone());
                // The search from `i` finds the same match the hit was.
                re.captures_at(s, i).unwrap().expand(rep, new);
            }
            _ => new.extend_from_slice(rep),
        }
    }
}

/// `s` lowercased into `buf`, for `ignore_case` beyond ASCII, with `at[i]`
/// the position in `s` of the character that byte `i` of `buf` comes from,
/// and one more for the end of `s`. Final sigma becomes σ, as Unicode's case
/// folding has it, so "ΟΔΟΣ" matches "οδος".
fn lower(s: &[u8], buf: &mut Vec<u8>, at: &mut Vec<u32>) {
    buf.clear();
    at.clear();
    let mut i = 0;
    for chunk in s.utf8_chunks() {
        for c in chunk.valid().chars() {
            for l in c.to_lowercase() {
                let l = if l == '\u{3C2}' { '\u{3C3}' } else { l };
                buf.extend_from_slice(l.encode_utf8(&mut [0; 4]).as_bytes());
            }
            at.resize(buf.len(), i as u32);
            i += c.len_utf8();
        }
        for &b in chunk.invalid() {
            buf.push(b);
            at.push(i as u32);
            i += 1;
        }
    }
    at.push(i as u32);
}

/// Each hit of a term in `s`, in the order the automaton finds them: every
/// hit, also where hits overlap, unless it runs on into a word on a side
/// that `bounds` names.
fn hits<'a>(
    ac: &'a aho_corasick::AhoCorasick,
    s: &'a [u8],
    bounds: Bounds,
) -> impl Iterator<Item = aho_corasick::Match> + 'a {
    // The automaton reports an empty term twice at a position it reaches
    // through two or more bytes of another term; the repeat is dropped.
    let mut last = None;
    ac.find_overlapping_iter(s)
        .filter(move |m| {
            let hit = (m.pattern(), m.start(), m.end());
            std::mem::replace(&mut last, Some(hit)) != Some(hit)
        })
        .filter(move |m| !splits_char(s, m.start(), m.end()))
        .filter(move |m| !bounds.joins(s, m.start(), m.end()))
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
            search.each(s, bounds, false, |t, _, _| {
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
/// regex. With `weights`, a double per term, the sum of their weights
/// instead. Otherwise as `par_match_term_rs()`.
/// @noRd
#[extendr]
#[allow(clippy::too_many_arguments)]
fn par_count_term_rs(
    x: Robj,
    terms: Robj,
    ignore_case: bool,
    word_start: bool,
    word_end: bool,
    fixed: bool,
    nthreads: i32,
    weights: Robj,
) -> extendr_api::Result<Robj> {
    let bounds = Bounds {
        start: word_start,
        end: word_end,
    };
    let m = compile_terms(&terms, ignore_case, bounds, fixed)?;
    let x = unsafe { charsxps(&x) };
    if let Some(w) = weights.as_real_slice() {
        let mut out = Doubles::new(x.len());
        per_string(
            &mut out,
            x,
            Rfloat::na(),
            nthreads,
            || Search::new(&m),
            |search, _, s| {
                let mut sum = 0.0;
                search.each(s, bounds, true, |t, _, _| {
                    sum += w[t as usize];
                    true
                });
                sum.into()
            },
        );
        return Ok(out.into());
    }
    let mut out = Integers::new(x.len());
    per_string(
        &mut out,
        x,
        Rint::na(),
        nthreads,
        || Search::new(&m),
        |search, _, s| {
            let mut n = 0usize;
            search.each(s, bounds, true, |_, _, _| {
                n += 1;
                true
            });
            (n.min(i32::MAX as usize) as i32).into()
        },
    );
    Ok(out.into())
}

/// Every hit of `terms` in each string replaced by its `replacement`, one
/// per term or one for all: the leftmost hit first, the earliest term
/// winning among those that start there, then the search goes on after it.
/// For regex terms, `$1` and the like expand as in `par_replace_rs()`.
/// Otherwise as `par_match_term_rs()`.
/// @noRd
#[extendr]
#[allow(clippy::too_many_arguments)]
fn par_replace_term_rs(
    x: Robj,
    terms: Robj,
    ignore_case: bool,
    word_start: bool,
    word_end: bool,
    fixed: bool,
    nthreads: i32,
    replacement: Robj,
) -> extendr_api::Result<Robj> {
    let bounds = Bounds {
        start: word_start,
        end: word_end,
    };
    let m = compile_terms(&terms, ignore_case, bounds, fixed)?;
    let reps: Vec<&[u8]> = unsafe { charsxps(&replacement).iter().map(|&p| bytes(p)).collect() };
    let rep = |t: u32| {
        if reps.len() == 1 {
            reps[0]
        } else {
            reps[t as usize]
        }
    };
    let x = unsafe { charsxps(&x) };
    let mut out = vec![Piece::Na; x.len()];
    per_string(
        &mut out,
        x,
        Piece::Na,
        nthreads,
        || Search::new(&m),
        |search, _, s| {
            let mut hits = Vec::new();
            search.each(s, bounds, true, |t, i, j| {
                hits.push((i, t, j));
                true
            });
            if hits.is_empty() {
                return Piece::Same;
            }
            hits.sort_unstable();
            let mut new = Vec::new();
            // The end of the last hit replaced, and where it started, so no
            // other hit starts there, not even an empty one.
            let (mut end, mut start) = (0, None);
            for (i, t, j) in hits {
                if i < end || start == Some(i) {
                    continue;
                }
                new.extend_from_slice(&s[end..i]);
                search.expand(s, t, i, rep(t), &mut new);
                (end, start) = (j, Some(i));
            }
            new.extend_from_slice(&s[end..]);
            Piece::New(new)
        },
    );
    strings(x, out)
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
            search.each(s, bounds, false, |i, _, _| {
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

/// Every pair of a string and a term it contains, with how many hits the
/// term has in it, as `par_count_term_rs()` counts them: a list of three
/// integer vectors, the 1-based positions in `x` and in `terms` and the
/// counts, sorted by the first and then the second. `NA` has no pairs.
/// Otherwise as `par_match_term_rs()`.
/// @noRd
#[extendr]
fn par_match_term_all_rs(
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
    let mut out: Vec<Vec<(u32, u32)>> = vec![Vec::new(); x.len()];
    per_string(
        &mut out,
        x,
        Vec::new(),
        nthreads,
        || Search::new(&m),
        |search, _, s| {
            let mut t = Vec::new();
            search.each(s, bounds, true, |i, _, _| {
                t.push(i);
                true
            });
            t.sort_unstable();
            let mut pairs: Vec<(u32, u32)> = Vec::new();
            for i in t {
                match pairs.last_mut() {
                    Some((j, n)) if *j == i => *n = n.saturating_add(1),
                    _ => pairs.push((i, 1)),
                }
            }
            pairs
        },
    );
    let total: usize = out.iter().map(|v| v.len()).sum();
    if total > i32::MAX as usize {
        return Err(Error::Other("More than 2^31 - 1 pairs match.".into()));
    }
    let (mut xs, mut ts, mut ns) = (
        Vec::with_capacity(total),
        Vec::with_capacity(total),
        Vec::with_capacity(total),
    );
    for (i, pairs) in out.iter().enumerate() {
        for &(t, n) in pairs {
            xs.push(i as i32 + 1);
            ts.push(t as i32 + 1);
            ns.push(n.min(i32::MAX as u32) as i32);
        }
    }
    Ok(List::from_names_and_values(
        ["x", "term", "n"],
        [
            Integers::from_values(xs),
            Integers::from_values(ts),
            Integers::from_values(ns),
        ],
    )?
    .into())
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

/// A part of a time format that `parse_time()` reads as R's `strptime()`
/// does: `%Y`, `%y`, `%m`, `%b` (also `%B` and `%h`, English names), `%d`
/// (also `%e`), `%j`, `%H`, `%I`, `%p`, `%M`, `%S`, `%OS`, `%z`, whitespace,
/// which matches any amount, and other ASCII characters, which match
/// themselves. `%F`, `%T`, and `%R` are spelled out.
#[derive(Clone, Copy)]
enum Part {
    Year,
    Year2,
    Month,
    MonthName,
    Day,
    Yday,
    Hour,
    Hour12,
    AmPm,
    Min,
    Sec,
    SecFrac,
    Offset,
    Space,
    Lit(u8),
}

/// The parts of `format`; `None` if it has any other, or lacks the year,
/// or the month and day or the day of the year, where R fills in what is
/// missing in ways not copied here.
fn time_format(format: &[u8]) -> Option<Vec<Part>> {
    use Part::*;
    let mut parts = Vec::new();
    let mut i = 0;
    while i < format.len() {
        let (part, len) = match &format[i..] {
            [b'%', b'Y', ..] => (Year, 2),
            [b'%', b'y', ..] => (Year2, 2),
            [b'%', b'm', ..] => (Month, 2),
            [b'%', b'b' | b'B' | b'h', ..] => (MonthName, 2),
            [b'%', b'd' | b'e', ..] => (Day, 2),
            [b'%', b'j', ..] => (Yday, 2),
            [b'%', b'H', ..] => (Hour, 2),
            [b'%', b'I', ..] => (Hour12, 2),
            [b'%', b'p', ..] => (AmPm, 2),
            [b'%', b'M', ..] => (Min, 2),
            [b'%', b'S', ..] => (Sec, 2),
            [b'%', b'O', b'S', ..] => (SecFrac, 3),
            [b'%', b'z', ..] => (Offset, 2),
            [b'%', b'%', ..] => (Lit(b'%'), 2),
            [b'%', b'F', ..] => {
                parts.extend([Year, Lit(b'-'), Month, Lit(b'-'), Day]);
                i += 2;
                continue;
            }
            [b'%', b'T', ..] => {
                parts.extend([Hour, Lit(b':'), Min, Lit(b':'), Sec]);
                i += 2;
                continue;
            }
            [b'%', b'R', ..] => {
                parts.extend([Hour, Lit(b':'), Min]);
                i += 2;
                continue;
            }
            [b'%', ..] => return None,
            [c, ..] if is_space(*c) => (Space, 1),
            [c, ..] if c.is_ascii() => (Lit(*c), 1),
            _ => return None,
        };
        parts.push(part);
        i += len;
    }
    let has = |f: fn(&Part) -> bool| parts.iter().any(f);
    (has(|p| matches!(p, Year | Year2))
        && ((has(|p| matches!(p, Month | MonthName)) && has(|p| matches!(p, Day)))
            || has(|p| matches!(p, Yday))))
    .then_some(parts)
}

/// English month names, as R's `strptime()` reads them in the C locale:
/// the full name first, then its first three letters, both ignoring case.
const MONTHS: [&[u8]; 12] = [
    b"january",
    b"february",
    b"march",
    b"april",
    b"may",
    b"june",
    b"july",
    b"august",
    b"september",
    b"october",
    b"november",
    b"december",
];

/// C's `isspace()`.
fn is_space(c: u8) -> bool {
    c == b' ' || (9..=13).contains(&c)
}

/// What `parse_time()` makes of a string.
enum Parsed {
    /// Whole seconds or days, and the fraction of a second.
    Time(f64, f64),
    Na,
    /// Something `strtod()` leaves to R.
    Unsupported,
}

/// A string read with `parts` as `strptime()` reads it in UTC, then turned
/// into seconds since 1970 as `as.POSIXct()` does, or days as `as.Date()`
/// does if `date`. As in R: numbers may start with spaces and take up to
/// their width, a format's whitespace matches any amount, text after the
/// format is ignored, numbers take all the digits their width allows, even
/// past their range, `%H` takes 24 for 24:00:00, the next day, and `%OS`
/// leaves the seconds at 0 where it reads no number from 0 to 61. `%y`
/// below 69 is 20xx, `%I` is taken modulo 12 and `%p` adds 12 for PM, `%j`
/// fills in the month and day where they aren't given, and `%z` is an
/// offset `+hhmm`, subtracted from the time.
fn parse_time(s: &[u8], parts: &[Part], date: bool) -> Parsed {
    // R's `get_number()`.
    let number = |p: &mut usize, from: i32, to: i32, width: u32| -> Option<i32> {
        while s.get(*p) == Some(&b' ') {
            *p += 1;
        }
        let digit = |p: usize| {
            s.get(p)
                .filter(|c| c.is_ascii_digit())
                .map(|c| (c - b'0') as i32)
        };
        let mut val = digit(*p)?;
        *p += 1;
        let mut n = width - 1;
        while n > 0 {
            let Some(d) = digit(*p) else { break };
            val = val * 10 + d;
            *p += 1;
            n -= 1;
        }
        (from..=to).contains(&val).then_some(val)
    };
    let (mut year, mut mon, mut day, mut hour, mut min, mut sec) = (0, 1, 1, 0, 0, 0);
    let (mut yday, mut have_mon, mut have_day) = (None, false, false);
    let (mut hour12, mut pm, mut off) = (false, false, None);
    let mut psecs = 0.0;
    let mut p = 0;
    let rest = |p: usize| &s[p.min(s.len())..];
    let starts =
        |p: usize, w: &[u8]| rest(p).len() >= w.len() && rest(p)[..w.len()].eq_ignore_ascii_case(w);
    for part in parts {
        let field = match part {
            Part::Space => {
                while s.get(p).is_some_and(|&c| is_space(c)) {
                    p += 1;
                }
                // R reads UTF-8 as wide characters, and its `iswspace()`
                // takes some Unicode spaces too, which ones depending on
                // the platform; so R decides.
                if s.get(p).is_some_and(|&c| c >= 0x80) {
                    return Parsed::Unsupported;
                }
                continue;
            }
            Part::Lit(c) => {
                if s.get(p) != Some(c) {
                    return Parsed::Na;
                }
                p += 1;
                continue;
            }
            Part::SecFrac => {
                match strtod(s, p) {
                    Err(()) => return Parsed::Unsupported,
                    // R moves past the number even where it keeps 0.
                    Ok(Some((v, end))) => {
                        if (0.0..=61.0).contains(&v) {
                            sec = v as i32;
                            psecs = v;
                        }
                        p = end;
                    }
                    Ok(None) => {}
                }
                continue;
            }
            Part::Year2 => {
                let Some(v) = number(&mut p, 0, 99, 2) else {
                    return Parsed::Na;
                };
                year = if v < 69 { 2000 + v } else { 1900 + v };
                continue;
            }
            Part::MonthName => {
                let Some(k) = MONTHS.iter().position(|&m| {
                    for w in [m, &m[..3]] {
                        if starts(p, w) {
                            p += w.len();
                            return true;
                        }
                    }
                    false
                }) else {
                    return Parsed::Na;
                };
                (mon, have_mon) = (k as i32 + 1, true);
                continue;
            }
            Part::Yday => {
                let Some(v) = number(&mut p, 1, 366, 3) else {
                    return Parsed::Na;
                };
                yday = Some(v - 1);
                continue;
            }
            Part::Hour12 => {
                let Some(v) = number(&mut p, 1, 12, 2) else {
                    return Parsed::Na;
                };
                (hour, hour12) = (v % 12, true);
                continue;
            }
            Part::AmPm => {
                if starts(p, b"am") {
                    p += 2;
                } else if starts(p, b"pm") {
                    (p, pm) = (p + 2, true);
                } else {
                    return Parsed::Na;
                }
                continue;
            }
            Part::Offset => {
                while s.get(p) == Some(&b' ') {
                    p += 1;
                }
                let neg = match s.get(p) {
                    Some(b'+') => false,
                    Some(b'-') => true,
                    _ => return Parsed::Na,
                };
                p += 1;
                let (mut val, mut n) = (0i64, 0);
                while n < 4 && s.get(p).is_some_and(|c| c.is_ascii_digit()) {
                    val = val * 10 + (s[p] - b'0') as i64;
                    (p, n) = (p + 1, n + 1);
                }
                if n != 4 || val % 100 >= 60 || val > 1400 {
                    return Parsed::Na;
                }
                // R turns the minutes into hundredths of an hour with
                // integer division, so +0501 is 5h 0m 36s.
                let val = (val / 100) * 100 + (val % 100) * 50 / 30;
                off = Some(if neg { -val * 36 } else { val * 36 });
                continue;
            }
            Part::Year => (&mut year, number(&mut p, 0, 9999, 4)),
            Part::Month => (&mut mon, number(&mut p, 1, 12, 2)),
            Part::Day => (&mut day, number(&mut p, 1, 31, 2)),
            Part::Hour => (&mut hour, number(&mut p, 0, 24, 2)),
            Part::Min => (&mut min, number(&mut p, 0, 59, 2)),
            Part::Sec => (&mut sec, number(&mut p, 0, 61, 2)),
        };
        match field {
            (f, Some(v)) => *f = v,
            (_, None) => return Parsed::Na,
        }
        match part {
            Part::Month => have_mon = true,
            Part::Day => have_day = true,
            _ => {}
        }
    }
    if hour12 && pm {
        hour += 12;
    }
    let leap = |year: i32| year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
    if let Some(yday) = yday.filter(|_| !(have_mon && have_day)) {
        // As R: the month the day of the year falls in, and its day of the
        // month, fill in what the format didn't give.
        let cum = if leap(year) {
            [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366]
        } else {
            [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365]
        };
        if yday >= cum[12] {
            return Parsed::Na;
        }
        let t_mon = cum.iter().filter(|&&c| c <= yday).count();
        if !have_mon {
            mon = t_mon as i32;
        }
        if !have_day {
            day = yday - cum[t_mon - 1] + 1;
        }
    }
    // R's `validate_tm()`.
    let days_in = |mon: i32, year: i32| {
        [
            31,
            28 + leap(year) as i32,
            31,
            30,
            31,
            30,
            31,
            31,
            30,
            31,
            30,
            31,
        ][mon as usize - 1]
    };
    if sec > 60 {
        return Parsed::Na;
    }
    if hour == 24 && min == 0 && sec == 0 {
        hour = 0;
        day += 1;
        if day > days_in(mon, year) {
            (mon, day) = (mon + 1, 1);
            if mon == 13 {
                (year, mon) = (year + 1, 1);
            }
        }
    }
    if hour > 23 || day > days_in(mon, year) {
        return Parsed::Na;
    }
    let days = days_from_civil(year, mon, day);
    let whole = (sec + min * 60 + hour * 3600) as i64 + days * 86400 - off.unwrap_or(0);
    if date {
        // A date takes the day as read, unless an offset moved the instant.
        let days = if off.is_some() {
            whole.div_euclid(86400)
        } else {
            days
        };
        return Parsed::Time(days as f64, 0.0);
    }
    // As `as.POSIXct()`: whole seconds, then the fraction `strptime()` kept.
    let secs = sec as f64 + (psecs - psecs.floor());
    Parsed::Time(whole as f64, secs - secs.floor())
}

/// Days from 1970-01-01 to a date of the proleptic Gregorian calendar, as
/// R counts them.
fn days_from_civil(year: i32, mon: i32, day: i32) -> i64 {
    let y = year as i64 - (mon <= 2) as i64;
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let m = mon as i64;
    let doy = (153 * (m + if m > 2 { -3 } else { 9 }) + 2) / 5 + day as i64 - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146097 + doe - 719468
}

/// R's `R_strtod()` on `s` from `p`, as `strptime()` reads seconds: the
/// number and where it ends, which may be NaN or infinity, or `None` for no
/// number. `Err` for hexadecimal, more than 15 digits, or an exponent beyond
/// 22, where R's long doubles could round differently, and for `nan(...)`,
/// whose end C's `strtod()` decides.
#[allow(clippy::result_unit_err)]
fn strtod(s: &[u8], mut p: usize) -> Result<Option<(f64, usize)>, ()> {
    let at = |p: usize| s.get(p).copied().unwrap_or(0);
    while is_space(at(p)) {
        p += 1;
    }
    let sign = match at(p) {
        b'-' => {
            p += 1;
            -1.0
        }
        b'+' => {
            p += 1;
            1.0
        }
        _ => 1.0,
    };
    let rest = &s[p.min(s.len())..];
    let starts = |w: &[u8]| rest.len() >= w.len() && rest[..w.len()].eq_ignore_ascii_case(w);
    if starts(b"nan") {
        if rest.get(3) == Some(&b'(') {
            return Err(());
        }
        return Ok(Some((f64::NAN, p + 3)));
    }
    if starts(b"inf") {
        let len = if starts(b"infinity") { 8 } else { 3 };
        return Ok(Some((sign * f64::INFINITY, p + len)));
    }
    if rest.len() > 2 && rest[0] == b'0' && (rest[1] | 0x20) == b'x' {
        return Err(());
    }
    let (mut ans, mut digits, mut expn) = (0.0f64, 0, 0i32);
    while at(p).is_ascii_digit() {
        ans = 10.0 * ans + (at(p) - b'0') as f64;
        (p, digits) = (p + 1, digits + 1);
    }
    if at(p) == b'.' {
        p += 1;
        while at(p).is_ascii_digit() {
            ans = 10.0 * ans + (at(p) - b'0') as f64;
            (p, digits, expn) = (p + 1, digits + 1, expn - 1);
        }
    }
    if digits == 0 {
        return Ok(None);
    }
    if digits > 15 {
        return Err(());
    }
    // Without a digit, the `e` and its sign are not part of the number.
    let sign_len = matches!(at(p + 1), b'-' | b'+') as usize;
    if at(p) | 0x20 == b'e' && at(p + 1 + sign_len).is_ascii_digit() {
        let exp_sign = if at(p + 1) == b'-' { -1 } else { 1 };
        p += 1 + sign_len;
        let mut n = 0;
        while at(p).is_ascii_digit() {
            n = if n < 9999 {
                n * 10 + (at(p) - b'0') as i32
            } else {
                n
            };
            p += 1;
        }
        expn += exp_sign * n;
    }
    if expn.abs() > 22 {
        return Err(());
    }
    // Powers of 10 up to 22 are exact, by squaring as in R.
    let (mut p10, mut fac, mut n) = (10.0f64, 1.0f64, expn.unsigned_abs());
    while n > 0 {
        if n & 1 == 1 {
            fac *= p10;
        }
        (n, p10) = (n >> 1, p10 * p10);
    }
    if expn < 0 {
        ans /= fac;
    } else if ans != 0.0 {
        ans *= fac;
    }
    Ok(Some((sign * ans, p)))
}

/// `as.Date()` (if `date`) or `as.POSIXct(tz = "UTC")` on strings: the
/// days or seconds, `NA` where a string doesn't parse. If `try_formats`,
/// the format is the first of `formats` that reads, for a date, the first
/// string that isn't `NA` or empty, else every string that isn't `NA`.
/// `NULL` where R must decide, as for a format or a number `parse_time()`
/// doesn't take, or no format fitting, which is an error in R. With
/// `split`, a list of the whole seconds and the fractions instead, for R to
/// add in base's order after placing the times in a time zone.
/// @noRd
#[extendr]
fn par_parse_time_rs(
    x: Robj,
    formats: Robj,
    try_formats: bool,
    date: bool,
    split: bool,
    nthreads: i32,
) -> Robj {
    let x = unsafe { charsxps(&x) };
    let Some(formats) = unsafe { charsxps(&formats) }
        .iter()
        .map(|&f| time_format(unsafe { bytes(f) }))
        .collect::<Option<Vec<_>>>()
    else {
        return ().into();
    };
    let na_ptr = unsafe { R_NaString } as usize;
    let unsupported = std::sync::atomic::AtomicBool::new(false);
    let na = [NA_REAL, 0.0];
    let parse_all = |parts: &[Part]| {
        let mut out = vec![na; x.len()];
        per_string(
            &mut out,
            x,
            na,
            nthreads,
            || (),
            |_, _, s| match parse_time(s, parts, date) {
                Parsed::Time(w, f) => [w, f],
                Parsed::Na => na,
                Parsed::Unsupported => {
                    unsupported.store(true, std::sync::atomic::Ordering::Relaxed);
                    na
                }
            },
        );
        out
    };
    let unsupported = || unsupported.load(std::sync::atomic::Ordering::Relaxed);
    let out = if !try_formats {
        Some(parse_all(&formats[0]))
    } else if date {
        // Empty strings count as `NA` here.
        let first = x
            .iter()
            .find(|&&p| p != na_ptr && unsafe { !bytes(p).is_empty() });
        match first {
            None => Some(vec![na; x.len()]),
            Some(&p) => {
                let mut buf = Vec::new();
                let s = unsafe { utf8(p, &mut buf) };
                let mut chosen = None;
                for f in &formats {
                    match parse_time(s, f, date) {
                        Parsed::Time(..) => {
                            chosen = Some(f);
                            break;
                        }
                        Parsed::Unsupported => return ().into(),
                        Parsed::Na => {}
                    }
                }
                chosen.map(|f| parse_all(f))
            }
        }
    } else if x.iter().all(|&p| p == na_ptr) {
        Some(vec![na; x.len()])
    } else {
        formats.iter().find_map(|f| {
            let out = parse_all(f);
            let all = pool(nthreads).install(|| {
                out.par_iter()
                    .zip(x)
                    .with_min_len(MATCH_PER_THREAD)
                    .all(|(v, &p)| p == na_ptr || !v[0].is_nan())
            });
            (all || unsupported()).then_some(out)
        })
    };
    match out {
        Some(out) if !unsupported() && split => {
            let col = |k: usize| Doubles::from_values(out.iter().map(|v| v[k]));
            List::from_names_and_values(["whole", "frac"], [col(0), col(1)])
                .unwrap()
                .into()
        }
        Some(out) if !unsupported() => Doubles::from_values(out.iter().map(|[w, f]| w + f)).into(),
        _ => ().into(),
    }
}

/// `rank(x)` for an integer or double vector, `NA` and `NaN` taking ranks
/// after the others (`na_last` 1), before them (2), or staying `NA` (0).
/// `ties` is "average", "first", "last", "min", or "max". The elements are
/// sorted with their positions, in parallel, and each run of equal ones
/// ranked at once: ties are equal, as in R, so `-0` ties with `0`.
/// @noRd
#[extendr]
fn par_rank_rs(x: Robj, ties: &str, na_last: i32, nthreads: i32) -> Robj {
    let pool = pool(nthreads);
    if let Some(v) = x.as_integer_slice() {
        let key = |v: &i32| (*v != NA_INT).then_some(*v);
        rank(v, key, ties, na_last, &pool)
    } else {
        let v = x.as_real_slice().unwrap();
        // Bits that sort as the numbers do, with -0 as 0.
        let key = |v: &f64| {
            (!v.is_nan()).then(|| {
                let b = (v + 0.0).to_bits();
                if b >> 63 == 1 {
                    !b
                } else {
                    b | 1 << 63
                }
            })
        };
        rank(v, key, ties, na_last, &pool)
    }
}

fn rank<T: Sync, K: Ord + Copy + Send + Sync>(
    x: &[T],
    key: impl Fn(&T) -> Option<K> + Sync,
    ties: &str,
    na_last: i32,
    pool: &rayon::ThreadPool,
) -> Robj {
    let (mut sorted, nas): (Vec<(K, u32)>, Vec<u32>) = pool.install(|| {
        let keyed = || x.par_iter().enumerate().with_min_len(MATCH_PER_THREAD);
        let mut sorted: Vec<_> = keyed()
            .filter_map(|(i, v)| key(v).map(|k| (k, i as u32)))
            .collect();
        // Positions break ties, so the order is as stable as R's.
        sorted.par_sort_unstable();
        let nas = keyed()
            .filter(|(_, v)| key(v).is_none())
            .map(|(i, _)| i as u32)
            .collect();
        (sorted, nas)
    });
    let shift = if na_last == 2 { nas.len() } else { 0 };
    // The rank of the `k`th sorted element, in a run of ties from `i` to `j`.
    let ranks = |out: &mut [f64], at: &(dyn Fn(usize, usize, usize) -> f64 + Sync)| {
        let (m, out) = (sorted.len(), Shared(out.as_mut_ptr()));
        let nt = pool.current_num_threads().min(m / MATCH_PER_THREAD).max(1);
        // Chunks start where a run does, so each thread ranks whole runs.
        let start = |c: usize| {
            let mut b = c * m / nt;
            while b > 0 && b < m && sorted[b].0 == sorted[b - 1].0 {
                b += 1;
            }
            b
        };
        pool.install(|| {
            (0..nt).into_par_iter().for_each(|c| {
                let (mut i, end) = (start(c), start(c + 1));
                while i < end {
                    let mut j = i + 1;
                    while j < m && sorted[j].0 == sorted[i].0 {
                        j += 1;
                    }
                    for (k, &(_, pos)) in sorted.iter().enumerate().take(j).skip(i) {
                        // Each position is in one run only.
                        unsafe { *out.get().add(pos as usize) = at(i, j, k) + shift as f64 };
                    }
                    i = j;
                }
            })
        });
    };
    let mut out = vec![NA_REAL; x.len()];
    match ties {
        "average" => ranks(&mut out, &|i, j, _| (i + j + 1) as f64 / 2.0),
        "first" => ranks(&mut out, &|_, _, k| (k + 1) as f64),
        "last" => ranks(&mut out, &|i, j, k| (i + j - k) as f64),
        "min" => ranks(&mut out, &|i, _, _| (i + 1) as f64),
        _ => ranks(&mut out, &|_, j, _| j as f64),
    }
    if na_last != 0 {
        let first = if na_last == 1 { sorted.len() } else { 0 };
        for (t, &i) in nas.iter().enumerate() {
            out[i as usize] = (first + t + 1) as f64;
        }
    }
    sorted.clear();
    if ties == "average" {
        return Doubles::from_values(out).into();
    }
    let ints: Vec<i32> = pool.install(|| {
        out.par_iter()
            .with_min_len(MATCH_PER_THREAD)
            .map(|&v| if v.is_nan() { NA_INT } else { v as i32 })
            .collect()
    });
    Integers::from_values(ints).into()
}

/// `rowsum(x, group)` for an integer or double vector or matrix `x`, given
/// each row's group as a 1-based `code` among `ng`: the sums, a column per
/// column of `x`. As in R, each group's rows are added in their order, so
/// the sums are R's to the bit, and integer sums that overflow give `NA`.
/// The rows are split by group as `par_duplicated()` splits elements, and
/// each thread sums its own groups. The sums are written through a pointer,
/// so codes outside the groups, or a `x` that isn't a whole number of rows,
/// are an error rather than a write out of bounds.
/// @noRd
#[extendr]
fn par_rowsum_rs(
    x: Robj,
    codes: &[i32],
    ng: i32,
    na_rm: bool,
    nthreads: i32,
) -> extendr_api::Result<Robj> {
    fn sums<T: Copy + Send + Sync>(
        x: &[T],
        codes: &[i32],
        ng: usize,
        zero: T,
        add: impl Fn(&mut T, T) + Sync,
        pool: &rayon::ThreadPool,
    ) -> Vec<T> {
        let n = codes.len();
        let mut out = vec![zero; ng * (x.len() / n.max(1))];
        let dst = Shared(out.as_mut_ptr());
        // Each group is in one bucket only, so no two threads add to the
        // same sum.
        let sum = |code: i32, row: usize| {
            for (j, &v) in x[row..].iter().step_by(n).enumerate() {
                add(
                    unsafe { &mut *dst.get().add(code as usize - 1 + j * ng) },
                    v,
                );
            }
        };
        pool.install(|| match partition(codes, &|c: &i32| *c) {
            None => codes.iter().enumerate().for_each(|(i, &c)| sum(c, i)),
            Some((nb, runs)) => (0..nb).into_par_iter().for_each(|b| {
                for &(c, i) in runs.iter().flat_map(|r| &r[b]) {
                    sum(c, i as usize - 1)
                }
            }),
        });
        out
    }
    let pool = pool(nthreads);
    let ok = pool.install(|| {
        codes
            .par_iter()
            .with_min_len(MATCH_PER_THREAD)
            .all(|&c| 1 <= c && c <= ng)
    });
    if !ok || x.len() % codes.len().max(1) != 0 {
        return Err(Error::Other("malformed groups".into()));
    }
    let ng = ng as usize;
    Ok(if let Some(v) = x.as_real_slice() {
        let add = |s: &mut f64, v: f64| {
            if !na_rm || !v.is_nan() {
                *s += v
            }
        };
        Doubles::from_values(sums(v, codes, ng, 0.0, add, &pool)).into()
    } else {
        let add = |s: &mut i32, v: i32| {
            if v == NA_INT {
                if !na_rm {
                    *s = NA_INT
                }
            } else if *s != NA_INT {
                let sum = *s as i64 + v as i64;
                *s = if sum < i32::MIN as i64 || sum > i32::MAX as i64 {
                    NA_INT
                } else {
                    sum as i32
                }
            }
        };
        let v = x.as_integer_slice().unwrap();
        Integers::from_values(sums(v, codes, ng, 0, add, &pool)).into()
    })
}

/// `approx()`'s interpolation at each of `xout`, from `x` sorted, without
/// `NA`, and `y`, as R's `approx1()` does it: `method` 1 is linear, 2
/// constant; `ends` holds `yleft`, `yright`, and `f`, which may be `NA`. `NA`
/// and `NaN` in `xout` stay as they are.
/// @noRd
#[extendr]
fn par_approx_rs(
    x: &[f64],
    y: &[f64],
    xout: &[f64],
    method: i32,
    ends: &[f64],
    nthreads: i32,
) -> Doubles {
    let [yleft, yright, f] = ends.try_into().unwrap();
    let (f1, f2) = (1.0 - f, f);
    let approx1 = |v: f64| -> f64 {
        if v.is_nan() {
            return v;
        }
        if x.is_empty() {
            return f64::NAN;
        }
        let (mut i, mut j) = (0, x.len() - 1);
        if v < x[i] {
            return yleft;
        }
        if v > x[j] {
            return yright;
        }
        while i + 1 < j {
            let ij = (i + j) / 2;
            if v < x[ij] {
                j = ij
            } else {
                i = ij
            }
        }
        if v == x[j] {
            return y[j];
        }
        if v == x[i] {
            return y[i];
        }
        if method == 1 {
            let (d, t) = (y[j] - y[i], (v - x[i]) / (x[j] - x[i]));
            // C compilers fuse this multiply and add into one instruction on
            // arm64, which rounds once, and R is compiled so; on x86-64 they
            // don't, without flags for newer processors.
            if cfg!(target_arch = "aarch64") {
                d.mul_add(t, y[i])
            } else {
                y[i] + d * t
            }
        } else {
            (if f1 != 0.0 { y[i] * f1 } else { 0.0 }) + (if f2 != 0.0 { y[j] * f2 } else { 0.0 })
        }
    };
    let mut out = vec![0.0; xout.len()];
    pool(nthreads).install(|| {
        out.par_iter_mut()
            .zip(xout)
            .with_min_len(MATCH_PER_THREAD)
            .for_each(|(o, &v)| *o = approx1(v))
    });
    Doubles::from_values(out)
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
    fn par_match_term_all_rs;
    fn par_find_interval_rs;
    fn par_unique_rs;
    fn par_extract_all_rs;
    fn par_locate_rs;
    fn par_locate_all_rs;
    fn par_replace_term_rs;
    fn par_parse_time_rs;
    fn par_rank_rs;
    fn par_rowsum_rs;
    fn par_approx_rs;
}
