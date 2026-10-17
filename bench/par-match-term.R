# par_match_term() against base, stringi, and AhoCorasickTrie; run after
# installing llutils. Only stringi_loop has par_match_term()'s exact semantics
# (first term in `terms` order). The extract + match() rows find the leftmost
# term instead, and AhoCorasickTrie returns a nested list of every hit, timed
# here without the R code that would turn it into term indices.
library(llutils)
set.seed(1)
word <- function(n) {
  vapply(
    seq_len(n),
    function(i) paste(sample(letters, 8, TRUE), collapse = ""),
    ""
  )
}
vocabulary <- word(1e4)
n <- 1e5
x <- paste(
  sample(vocabulary, n, TRUE),
  sample(vocabulary, n, TRUE),
  sample(999, n, TRUE)
)
terms <- sample(vocabulary, 1000)
pattern <- paste(terms, collapse = "|")
X <- toupper(x)

stringi_loop <- function(x, terms) {
  out <- rep(NA_integer_, length(x))
  for (i in rev(seq_along(terms))) {
    out[stringi::stri_detect_fixed(x, terms[i])] <- i
  }
  out
}
stopifnot(identical(par_match_term(x, terms), stringi_loop(x, terms)))
stopifnot(identical(
  par_match_term(X, terms, ignore_case = TRUE),
  par_match_term(x, terms)
))

cat("1e5 strings, 1000 terms\n")
print(bench::mark(
  base_perl = match(regmatches(x, regexpr(pattern, x, perl = TRUE)), terms),
  stringi = match(stringi::stri_extract_first_regex(x, pattern), terms),
  stringi_loop = stringi_loop(x, terms),
  AhoCorasickTrie = AhoCorasickTrie::AhoCorasickSearch(terms, x),
  par_detect = par_detect(x, pattern),
  llutils_2 = par_match_term(x, terms, nthreads = 2),
  llutils_all = par_match_term(x, terms),
  check = FALSE,
  min_iterations = 3,
  filter_gc = FALSE
)[, c("expression", "median")])

cat("\nSame, uppercased, ignoring case\n")
print(bench::mark(
  base_perl = grepl(pattern, X, perl = TRUE, ignore.case = TRUE),
  stringi = match(
    tolower(stringi::stri_extract_first_regex(
      X,
      pattern,
      case_insensitive = TRUE
    )),
    terms
  ),
  par_detect = par_detect(X, paste0("(?i)", pattern)),
  par_detect_lower = par_detect(tolower(X), pattern),
  llutils_all = par_match_term(X, terms, ignore_case = TRUE),
  check = FALSE,
  min_iterations = 3,
  filter_gc = FALSE
)[, c("expression", "median")])

cat("\nScaling with terms (1e6 strings, all threads)\n")
x6 <- paste(
  sample(vocabulary, 1e6, TRUE),
  sample(vocabulary, 1e6, TRUE),
  sample(999, 1e6, TRUE)
)
for (k in c(10, 1000, 1e4)) {
  t <- sample(vocabulary, k)
  r <- bench::mark(par_match_term(x6, t), min_iterations = 3, filter_gc = FALSE)
  cat(k, "terms:", format(r$median), "\n")
}

cat("\nWhole words: half the terms are words, half prefixes of words\n")
terms_ww <- c(sample(vocabulary, 500), substr(sample(vocabulary, 500), 1, 5))
pattern_ww <- paste0("\\b(", paste(terms_ww, collapse = "|"), ")\\b")
stringi_loop_ww <- function(x, terms) {
  out <- rep(NA_integer_, length(x))
  for (i in rev(seq_along(terms))) {
    out[stringi::stri_detect_regex(x, paste0("\\b", terms[i], "\\b"))] <- i
  }
  out
}
stopifnot(identical(
  par_match_term(x, terms_ww, whole_word = TRUE),
  stringi_loop_ww(x, terms_ww)
))
print(bench::mark(
  stringi_loop = stringi_loop_ww(x, terms_ww),
  base_perl_detect = grepl(pattern_ww, x, perl = TRUE),
  par_detect = par_detect(x, pattern_ww),
  substring = par_match_term(x, terms_ww),
  whole_word = par_match_term(x, terms_ww, whole_word = TRUE),
  check = FALSE, min_iterations = 3, filter_gc = FALSE
)[, c("expression", "median")])

cat("\nSmall inputs, 1000 terms\n")
for (m in c(100, 1000, 1e4)) {
  r <- bench::mark(
    stringi = match(stringi::stri_extract_first_regex(x[1:m], pattern), terms),
    llutils_all = par_match_term(x[1:m], terms),
    check = FALSE
  )
  cat("n =", m, ":", format(r$median), "\n")
}
cat("\n")
print(sessionInfo())
