# par_which_term() against stringi; run after installing llutils.
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
# Four words each, so strings hold several terms.
x <- do.call(paste, replicate(4, sample(vocabulary, n, TRUE), simplify = FALSE))
# Every term of a string, in `terms` order: one stri_detect_fixed() per term.
stringi_loop <- function(x, terms) {
  hit <- vapply(
    terms,
    function(t) stringi::stri_detect_fixed(x, t),
    logical(length(x))
  )
  lapply(seq_along(x), function(i) unname(which(hit[i, ])))
}
for (k in c(100, 1000)) {
  terms <- sample(vocabulary, k)
  stopifnot(identical(par_which_term(x, terms), stringi_loop(x, terms)))
  cat("\n1e5 strings,", k, "terms\n")
  print(bench::mark(
    stringi_loop = stringi_loop(x, terms),
    stringi_extract = stringi::stri_extract_all_regex(
      x,
      paste(terms, collapse = "|")
    ),
    par_match_term = par_match_term(x, terms),
    llutils_2 = par_which_term(x, terms, nthreads = 2),
    llutils_all = par_which_term(x, terms),
    check = FALSE,
    min_iterations = 3,
    filter_gc = FALSE
  )[, c("expression", "median")])
}
x6 <- do.call(
  paste,
  replicate(4, sample(vocabulary, 1e6, TRUE), simplify = FALSE)
)
terms <- sample(vocabulary, 1000)
cat("\n1e6 strings, 1000 terms\n")
print(bench::mark(
  par_match_term = par_match_term(x6, terms),
  par_which_term = par_which_term(x6, terms),
  check = FALSE,
  min_iterations = 3,
  filter_gc = FALSE
)[, c("expression", "median")])
cat("\n")
print(sessionInfo())
