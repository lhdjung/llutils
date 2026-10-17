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
    let n = if nthreads > 0 {
        nthreads as usize
    } else {
        std::env::var("RAYON_NUM_THREADS")
            .ok()
            .and_then(|s| s.parse().ok())
            .filter(|&n| n > 0)
            .unwrap_or_else(|| std::thread::available_parallelism().map_or(1, |n| n.get()))
    };
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
    let maps = pool.install(|| build(table, &tkey));
    let mut out = Integers::new(x.len());
    let slice: &mut [Rint] = &mut out; // threads may share this, not `out`
    pool.install(|| {
        slice.par_iter_mut().zip(x).with_min_len(MATCH_PER_THREAD).for_each(|(o, v)| {
            let k = xkey(v);
            *o = (*maps[bucket(&k, maps.len())].get(&k).unwrap_or(&nomatch)).into();
        })
    });
    out
}

/// Below this many table elements, one map built on one thread is fastest.
const PARTITION_MIN: usize = 1 << 16;

/// Table position (1-based) by key, first position winning. A large table
/// is split by hash into one map per bucket, so the build runs in parallel
/// too: each chunk of the table is sorted into per-bucket runs, then each
/// bucket's map is filled from its runs in table order.
fn build<T: Sync, K: Copy + Hash + Eq + Send + Sync>(
    table: &[T],
    key: &(impl Fn(&T) -> K + Sync),
) -> Vec<HashMap<K, i32>> {
    let nt = rayon::current_num_threads().min(table.len() / MATCH_PER_THREAD);
    if table.len() < PARTITION_MIN || nt <= 1 {
        let mut m = HashMap::with_capacity_and_hasher(table.len(), Default::default());
        // Reverse so earlier positions overwrite later ones.
        for (i, v) in table.iter().enumerate().rev() {
            m.insert(key(v), i as i32 + 1);
        }
        return vec![m];
    }
    let nb = (nt * 4).next_power_of_two().min(256);
    let chunk = table.len().div_ceil(nt * 4);
    let runs: Vec<Vec<Vec<(K, i32)>>> = table
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
    (0..nb)
        .into_par_iter()
        .map(|b| {
            let n = runs.iter().map(|r| r[b].len()).sum();
            let mut m = HashMap::with_capacity_and_hasher(n, Default::default());
            for (k, i) in runs.iter().flat_map(|r| &r[b]) {
                m.entry(*k).or_insert(*i);
            }
            m
        })
        .collect()
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
    par_match(
        x,
        table,
        key,
        key,
        nomatch,
        &pool(nthreads),
    )
}

/// `m[codes]` for factor codes, with `NA` codes taking `m`'s last element.
/// Codes outside the levels error, as in base.
/// @noRd
#[extendr]
fn index_codes(codes: &[i32], m: &[i32], nthreads: i32) -> extendr_api::Result<Integers> {
    let (&na, levels) = m.split_last().unwrap();
    let mut out = Integers::new(codes.len());
    let slice: &mut [Rint] = &mut out;
    pool(nthreads)
        .install(|| {
            slice
                .par_iter_mut()
                .zip(codes)
                .with_min_len(MATCH_PER_THREAD)
                .try_for_each(|(o, &c)| {
                    *o = if c == NA_INT { na } else { *levels.get((c as usize).wrapping_sub(1))? }.into();
                    Some(())
                })
        })
        .ok_or_else(|| Error::Other("malformed factor".into()))?;
    Ok(out)
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
    let key = |p: &usize| *p;
    let ascii = |p: &usize| unsafe { bytes(*p).is_ascii() };
    let ce = |p: &usize| unsafe { Rf_getCharCE(*p as SEXP) };
    // The threads read the strings themselves, as detect_regex_rs() does.
    // Reading each one is a cache miss, so this scans the shorter side.
    let ok = pool.install(|| {
        let both = || x.par_iter().chain(table).with_min_len(MATCH_PER_THREAD);
        if x.len() < table.len() { x } else { table }
            .par_iter()
            .with_min_len(MATCH_PER_THREAD)
            .all(ascii)
            || {
                // Encodings seen, as bits: 1 native, 2 UTF-8, 4 anything else.
                let seen = both().map(|p| 1u8 << ce(p).min(2)).reduce(|| 0, |a, b| a | b);
                seen & 4 == 0 && (seen != 3 || both().all(|p| ce(p) == 1 || ascii(p)))
            }
    });
    if !ok {
        return ().into();
    }
    par_match(x, table, key, key, nomatch, &pool).into()
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

/// `grepl(pattern, x)` with Rust's regex syntax. Strings are matched as
/// bytes, so R should pass them through `enc2utf8()` first. `NA` gives `NA`.
/// @noRd
#[extendr]
fn detect_regex_rs(x: Robj, pattern: &str, nthreads: i32) -> extendr_api::Result<Logicals> {
    let re = regex(pattern)?;
    let x = unsafe { charsxps(&x) };
    let na = unsafe { R_NaString } as usize;
    // The threads read the strings themselves: R stores them scattered in
    // memory, so reading them on one thread would take most of the time.
    // R_CHAR() and LENGTH() only read memory; R doesn't promise they are
    // thread-safe, but data.table has read strings from threads for years.
    let detect = |re: &mut regex::bytes::Regex, (out, &p): (&mut Rbool, &usize)| {
        *out = if p == na {
            Rbool::na_value()
        } else {
            re.is_match(unsafe { bytes(p) }).into()
        }
    };
    let mut out = Logicals::new(x.len());
    let slice: &mut [Rbool] = &mut out; // threads may share this, not `out`
    if x.len() < 2 * REGEX_PER_THREAD {
        let mut re = re;
        slice
            .iter_mut()
            .zip(x)
            .for_each(|item| detect(&mut re, item));
    } else {
        // A clone per chunk: threads sharing one regex contend for its cache,
        // and cloning per rayon task costs more than it saves.
        pool(nthreads).install(|| {
            let nt = rayon::current_num_threads().min(x.len() / REGEX_PER_THREAD);
            let chunk = x.len().div_ceil(4 * nt);
            slice
                .par_chunks_mut(chunk)
                .zip(x.par_chunks(chunk))
                .for_each(|(out, x)| {
                    let mut re = re.clone();
                    out.iter_mut().zip(x).for_each(|item| detect(&mut re, item));
                })
        });
    }
    Ok(out)
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
    fn detect_regex_rs;
}
