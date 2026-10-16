use extendr_api::prelude::*;
use extendr_api::SEXP;
use rayon::prelude::*;
use rustc_hash::FxHashMap as HashMap;
use std::ffi::CStr;
use std::hash::Hash;
use std::os::raw::{c_char, c_int};

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

/// `nthreads` threads; 0 = rayon's default: `RAYON_NUM_THREADS`, else all
/// cores.
fn pool(nthreads: i32) -> rayon::ThreadPool {
    rayon::ThreadPoolBuilder::new()
        .num_threads(nthreads as usize)
        .build()
        .unwrap()
}

/// `match(x, table)` for any element types whose keys are equal exactly
/// when base R considers the elements equal. 1-based, first match wins, no
/// match gives `nomatch`.
fn par_match<X: Sync, T, K: Hash + Eq + Send + Sync>(
    x: &[X],
    table: &[T],
    xkey: impl Fn(&X) -> K + Sync,
    tkey: impl Fn(&T) -> K,
    nomatch: Rint,
    pool: &rayon::ThreadPool,
) -> Vec<i32> {
    let nomatch = Option::<i32>::from(nomatch).unwrap_or(NA_INT);
    let mut first = HashMap::with_capacity_and_hasher(table.len(), Default::default());
    // Reverse so earlier positions overwrite later ones.
    for (i, v) in table.iter().enumerate().rev() {
        first.insert(tkey(v), i as i32 + 1);
    }
    pool.install(|| {
        x.par_iter()
            .map(|v| *first.get(&xkey(v)).unwrap_or(&nomatch))
            .collect()
    })
}

/// @noRd
#[extendr]
fn par_match_int(x: &[i32], table: &[i32], nomatch: Rint, nthreads: i32) -> Vec<i32> {
    let key = |v: &i32| *v;
    par_match(x, table, key, key, nomatch, &pool(nthreads))
}

/// `m[codes]` for factor codes, with `NA` codes taking `m`'s last element.
/// @noRd
#[extendr]
fn index_codes(codes: &[i32], m: &[i32], nthreads: i32) -> Vec<i32> {
    let na = m[m.len() - 1];
    pool(nthreads).install(|| {
        codes
            .par_iter()
            .map(|&c| if c == NA_INT { na } else { m[c as usize - 1] })
            .collect()
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
    if table.iter().any(far) || pool.install(|| x.par_iter().any(far)) {
        return ().into();
    }
    let key = |v: &f64| dbl_key(v.floor());
    par_match(x, table, key, key, nomatch, &pool).into()
}

/// One side integer, the other double: integers are converted as they are
/// hashed, rather than copied to a double vector first.
/// @noRd
#[extendr]
fn par_match_mixed(x: Robj, table: Robj, nomatch: Rint, nthreads: i32) -> Vec<i32> {
    let pool = pool(nthreads);
    let int = |v: &i32| dbl_key(if *v == NA_INT { NA_REAL } else { *v as f64 });
    let dbl = |v: &f64| dbl_key(*v);
    match (x.as_integer_slice(), table.as_real_slice()) {
        (Some(x), Some(table)) => par_match(x, table, int, dbl, nomatch, &pool),
        _ => {
            let (x, table) = (x.as_real_slice().unwrap(), table.as_integer_slice().unwrap());
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
        if v.to_bits() as u32 == 1954 { NA_BITS } else { f64::NAN.to_bits() }
    } else {
        v.to_bits()
    }
}

/// Matches strings by pointer: R stores each string once per encoding, so
/// equal pointers mean equal strings. That could miss a match only for
/// non-ASCII strings in `table` (ASCII is never marked with an encoding):
/// then every string is checked, and this returns `NULL`, for R to fall back
/// to base, if there are latin1 or bytes strings, or non-ASCII strings both
/// marked UTF-8 and unmarked.
/// @noRd
#[extendr]
fn par_match_chr(x: Robj, table: Robj, nomatch: Rint, nthreads: i32) -> Robj {
    let (x, table) = unsafe { (charsxps(&x), charsxps(&table)) };
    let pool = pool(nthreads);
    let key = |p: &usize| *p;
    let ascii = |p: &usize| unsafe { CStr::from_ptr(R_CHAR(*p as SEXP)).to_bytes().is_ascii() };
    if table.iter().all(ascii) {
        return par_match(x, table, key, key, nomatch, &pool).into();
    }
    let ce = |p: &usize| unsafe { Rf_getCharCE(*p as SEXP) };
    let (mut native, mut utf8) = (false, false);
    for p in x.iter().chain(table) {
        match ce(p) {
            0 => native = true,
            1 => utf8 = true,
            _ => return ().into(),
        }
    }
    if native && utf8 && !x.iter().chain(table).all(|p| ce(p) == 1 || ascii(p)) {
        return ().into();
    }
    par_match(x, table, key, key, nomatch, &pool).into()
}

/// Below this many strings, starting threads costs more than they save.
const REGEX_MIN_PAR: usize = 5000;

/// `grepl(pattern, x)` with Rust's regex syntax. Strings are matched as
/// bytes, so R should pass them through `enc2utf8()` first. `NA` gives `NA`.
/// @noRd
#[extendr]
fn detect_regex_rs(x: Robj, pattern: &str, nthreads: i32) -> extendr_api::Result<Logicals> {
    let re = regex::bytes::Regex::new(pattern).map_err(|e| Error::Other(e.to_string()))?;
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
            let p = p as SEXP;
            let b = unsafe { std::slice::from_raw_parts(R_CHAR(p) as *const u8, LENGTH(p) as usize) };
            re.is_match(b).into()
        }
    };
    let mut out = Logicals::new(x.len());
    let slice: &mut [Rbool] = &mut out; // threads may share this, not `out`
    if x.len() < REGEX_MIN_PAR {
        let mut re = re;
        slice.iter_mut().zip(x).for_each(|item| detect(&mut re, item));
    } else {
        // A clone per chunk: threads sharing one regex contend for its cache,
        // and cloning per rayon task costs more than it saves.
        pool(nthreads).install(|| {
            let chunk = x.len().div_ceil(4 * rayon::current_num_threads());
            slice.par_chunks_mut(chunk).zip(x.par_chunks(chunk)).for_each(|(out, x)| {
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

extendr_module! {
    mod llutils;
    fn par_match_int;
    fn index_codes;
    fn par_match_dbl;
    fn par_match_mixed;
    fn par_match_chr;
    fn detect_regex_rs;
}
