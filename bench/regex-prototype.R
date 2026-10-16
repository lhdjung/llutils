# Prototype for a parallel grepl(); not part of the package. Run with
# Rscript, needs rextendr and stringi. Ignores NA and encodings.
rextendr::rust_source(code = r"(
use extendr_api::prelude::*;
use extendr_api::SEXP;
use rayon::prelude::*;
#[allow(improper_ctypes)]
extern "C" {
    fn STRING_PTR_RO(x: SEXP) -> *const SEXP;
    fn R_CHAR(x: SEXP) -> *const u8;
    fn LENGTH(x: SEXP) -> i32;
}
// Prototype: ignores NA and encodings.
#[extendr]
fn par_grepl(pattern: &str, x: Robj) -> Vec<bool> {
    let re = regex::bytes::Regex::new(pattern).unwrap();
    let s: Vec<&[u8]> = unsafe {
        let p = STRING_PTR_RO(x.get());
        (0..x.len()).map(|i| { let c = *p.add(i); std::slice::from_raw_parts(R_CHAR(c), LENGTH(c) as usize) }).collect()
    };
    s.par_iter().map(|b| re.is_match(b)).collect()
}
)", dependencies = list(rayon = "1", regex = "1"), profile = "release")
set.seed(1)
words <- vapply(1:1e4, function(i) paste(sample(letters, 12, TRUE), collapse = ""), "")
x <- paste(sample(words, 1e6, TRUE), sample(words, 1e6, TRUE), sample(1:999, 1e6, TRUE))
for (pat in c("abc", "[aeiou]{3}\\d", "^q.*z\\s")) {
  cat("\npattern:", pat, "  n = 1e6\n")
  stopifnot(identical(par_grepl(pat, x), grepl(pat, x, perl = TRUE)))
  print(bench::mark(base = grepl(pat, x), base_perl = grepl(pat, x, perl = TRUE),
    stringi = stringi::stri_detect_regex(x, pat), rust8 = par_grepl(pat, x),
    min_iterations = 3, filter_gc = FALSE)[, c("expression", "median")])
}
