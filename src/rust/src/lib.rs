use extendr_api::prelude::*;
use extendr_api::SEXP;
use rayon::prelude::*;
use rustc_hash::{FxBuildHasher, FxHashMap as HashMap};
use std::hash::{BuildHasher, Hash};
use std::os::raw::{c_char, c_int};
use std::sync::{Arc, Mutex};

// Public R API that extendr doesn't wrap. SEXP is opaque, so the lint is moot.
#[allow(improper_ctypes)]
extern "C" {
    fn STRING_PTR_RO(x: SEXP) -> *const SEXP;
    fn Rf_getCharCE(x: SEXP) -> c_int;
    fn R_CHAR(x: SEXP) -> *const c_char;
    fn LENGTH(x: SEXP) -> c_int;
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
/// `from_last`. A large `x` is split by hash as in `build()`, and each
/// bucket marks the duplicates among its own keys, so no second pass over
/// `x` looks them up, as `par_match(x, x)` would.
fn duplicated<X: Sync, K: Copy + Hash + Eq + Send + Sync>(
    x: &[X],
    key: impl Fn(&X) -> K + Sync,
    from_last: bool,
    pool: &rayon::ThreadPool,
) -> Logicals {
    let mut out = Logicals::new(x.len()); // all FALSE
    let slice: &mut [Rbool] = &mut out;
    pool.install(|| {
        let Some((nb, runs)) = partition(x, &key) else {
            let mut seen = map(x.len(), x.len());
            let mut mark = |(o, v): (&mut Rbool, &X)| *o = seen.insert(key(v), ()).is_some().into();
            if from_last {
                slice.iter_mut().zip(x).rev().for_each(&mut mark)
            } else {
                slice.iter_mut().zip(x).for_each(&mut mark)
            }
            return;
        };
        let out = Shared(slice.as_mut_ptr());
        (0..nb).into_par_iter().for_each(|b| {
            let n = runs.iter().map(|r| r[b].len()).sum();
            let mut seen = map(n, x.len());
            let mut mark = |&(k, i): &(K, i32)| {
                if seen.insert(k, ()).is_some() {
                    // Each position is in one bucket only, so no two
                    // threads write the same element.
                    unsafe { *out.get().add(i as usize - 1) = Rbool::true_value() }
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
    out
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
/// costs more than it saves.
fn per_string<T: Copy + Send + Sync, S>(
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
        for (j, (o, &p)) in out.iter_mut().zip(x).enumerate() {
            *o = if p == na_ptr {
                na
            } else {
                f(&mut s, start + j, unsafe { bytes(p) })
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

/// Runs `f` on each string of `x` with its regex: `pattern` holds one for
/// all strings, or one per string. Each distinct pattern is compiled once,
/// on the threads if there are several, and cloned into a chunk the first
/// time the chunk needs it. An `NA` pattern gives `na`.
fn per_pattern<T: Copy + Send + Sync>(
    out: &mut [T],
    x: &Robj,
    pattern: &Robj,
    fixed: bool,
    na: T,
    nthreads: i32,
    f: impl Fn(&regex::bytes::Regex, &[u8]) -> T + Sync,
) -> extendr_api::Result<()> {
    type Re = regex::bytes::Regex;
    let (x, pattern) = unsafe { (charsxps(x), charsxps(pattern)) };
    if x.is_empty() {
        return Ok(());
    }
    // Which distinct pattern each string takes, if there are several.
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
    per_string(
        out,
        x,
        na,
        nthreads,
        || vec![None; res.len()],
        // ponytail: each chunk clones every regex it meets, and each clone
        // builds its own cache: 1e4 distinct patterns in 1e6 strings take
        // 0.7s (stringi 2.7s), against 14ms for 10. Grouping the strings by
        // pattern would build each cache about once, if that case matters.
        |cache: &mut Vec<Option<Re>>, i, s| {
            let id = which.as_ref().map_or(0, |w| w[i] as usize);
            match &res[id] {
                None => na,
                Some(re) => f(cache[id].get_or_insert_with(|| re.clone()), s),
            }
        },
    );
    Ok(())
}

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
    let mut out = Logicals::new(x.len());
    per_pattern(
        &mut out,
        &x,
        &pattern,
        fixed,
        Rbool::na_value(),
        nthreads,
        |re, s| re.is_match(s).into(),
    )?;
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
    let mut out = Integers::new(x.len());
    per_pattern(
        &mut out,
        &x,
        &pattern,
        fixed,
        Rint::na(),
        nthreads,
        |re, s| count(re.find_iter(s)),
    )?;
    Ok(out)
}

/// The length of an iterator as an R integer, capped at R's largest.
fn count(it: impl Iterator) -> Rint {
    (it.count().min(i32::MAX as usize) as i32).into()
}

/// The automaton for `terms`. Building one takes about 0.9ms for 1000
/// terms, most of a call below 1e4 strings, so the last one is kept for
/// calls that repeat the terms. They are compared by their string pointers,
/// as `par_match_chr()` compares strings, and a copy of the vector keeps
/// those strings alive: otherwise R could free one and put another string
/// at its address.
fn automaton(terms: &Robj, ignore_case: bool) -> extendr_api::Result<aho_corasick::AhoCorasick> {
    struct Last(Robj, bool, aho_corasick::AhoCorasick);
    // Only R's thread ever touches it.
    unsafe impl Send for Last {}
    static LAST: Mutex<Option<Last>> = Mutex::new(None);
    // R calls can jump past Rust's cleanup on an error, so none run while
    // the lock is held, or it would never be released.
    let p = unsafe { charsxps(terms) };
    let copy = terms.duplicate();
    let mut last = LAST.lock().unwrap_or_else(|e| e.into_inner());
    if let Some(Last(t, ic, ac)) = last.as_ref() {
        if *ic == ignore_case && unsafe { charsxps(t) } == p {
            return Ok(ac.clone());
        }
    }
    let ac = aho_corasick::AhoCorasick::builder()
        .ascii_case_insensitive(ignore_case)
        .build(p.iter().map(|&p| unsafe { bytes(p) }))
        .map_err(|e| Error::Other(e.to_string()))?;
    *last = Some(Last(copy, ignore_case, ac.clone()));
    Ok(ac)
}

/// The index of each term found in `s`, in the order the automaton finds
/// them: every hit, also where hits overlap. With `whole_word`, a hit counts
/// only if no word character touches it on either side.
fn hits<'a>(
    ac: &'a aho_corasick::AhoCorasick,
    s: &'a [u8],
    whole_word: bool,
) -> impl Iterator<Item = u32> + 'a {
    ac.find_overlapping_iter(s)
        // An empty term also matches between the bytes of one character.
        .filter(move |m| !(m.is_empty() && s.get(m.start()).is_some_and(|b| b & 0xC0 == 0x80)))
        .filter(move |m| {
            !whole_word
                || !(is_word(last_char(&s[..m.start()])) || is_word(first_char(&s[m.end()..])))
        })
        .map(|m| m.pattern().as_u32())
}

/// For each string, the 1-based index of the first of `terms` it contains,
/// or `NA`. One automaton finds every occurrence of every term in a single
/// pass, and the lowest index wins, so term order sets priority, not where
/// in the string a term occurs. With `whole_word`, a hit counts only if no
/// word character touches it on either side. Strings are matched as bytes,
/// so R should pass both sides through `enc2utf8()` first. `NA` gives `NA`.
/// @noRd
#[extendr]
fn par_match_term_rs(
    x: Robj,
    terms: Robj,
    ignore_case: bool,
    whole_word: bool,
    nthreads: i32,
) -> extendr_api::Result<Integers> {
    let ac = automaton(&terms, ignore_case)?;
    let mut out = Integers::new(x.len());
    // Unlike a regex, the automaton has no cache, so threads share one.
    per_string(
        &mut out,
        unsafe { charsxps(&x) },
        Rint::na(),
        nthreads,
        || (),
        |_, _, s| {
            let mut best = u32::MAX;
            for t in hits(&ac, s, whole_word) {
                best = best.min(t);
                if best == 0 {
                    break;
                }
            }
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
/// of every term, also where they overlap; otherwise as
/// `par_match_term_rs()`.
/// @noRd
#[extendr]
fn par_count_term_rs(
    x: Robj,
    terms: Robj,
    ignore_case: bool,
    whole_word: bool,
    nthreads: i32,
) -> extendr_api::Result<Integers> {
    let ac = automaton(&terms, ignore_case)?;
    let mut out = Integers::new(x.len());
    per_string(
        &mut out,
        unsafe { charsxps(&x) },
        Rint::na(),
        nthreads,
        || (),
        |_, _, s| count(hits(&ac, s, whole_word)),
    );
    Ok(out)
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
    mod llutils;
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
    fn par_match_term_rs;
    fn par_count_term_rs;
    fn par_find_interval_rs;
}
