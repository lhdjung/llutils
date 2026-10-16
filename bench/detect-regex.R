# detect_regex() against base and stringi; run after installing llutils.
library(llutils)
set.seed(1)
words <- vapply(1:1e4, function(i) paste(sample(letters, 12, TRUE), collapse = ""), "")
x <- paste(sample(words, 1e6, TRUE), sample(words, 1e6, TRUE), sample(1:999, 1e6, TRUE))
for (pat in c("abc", "[aeiou]{3}[0-9]", "^q.*z\\s", "(?i)QU[a-z]+ ")) {
  cat("\npattern:", pat, "  n = 1e6\n")
  perl <- sub("(?i)", "", pat, fixed = TRUE)
  print(bench::mark(
    base = grepl(perl, x, ignore.case = perl != pat),
    base_perl = grepl(perl, x, ignore.case = perl != pat, perl = TRUE),
    stringi = stringi::stri_detect_regex(x, pat),
    llutils_2 = detect_regex(x, pat, nthreads = 2),
    llutils_all = detect_regex(x, pat),
    min_iterations = 3, filter_gc = FALSE
  )[, c("expression", "median")])
}
cat("\nSmall inputs, pattern repeated so llutils reuses the compiled regex\n")
for (n in c(100, 1000, 3000, 1e4, 3e4)) {
  y <- x[1:n]
  r <- bench::mark(base_perl = grepl("[aeiou]{3}[0-9]", y, perl = TRUE),
    llutils_2 = detect_regex(y, "[aeiou]{3}[0-9]", nthreads = 2),
    llutils_all = detect_regex(y, "[aeiou]{3}[0-9]"), check = FALSE)
  cat("n =", n, ":", format(r$median), "\n")
}
cat("\n"); print(sessionInfo())
