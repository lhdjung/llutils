# par_count() and par_count_term() against base and stringi; run after
# installing llutils.
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
x <- paste(
  sample(vocabulary, 1e6, TRUE),
  sample(vocabulary, 1e6, TRUE),
  sample(999, 1e6, TRUE)
)
b <- function(...) {
  print(bench::mark(..., min_iterations = 3, filter_gc = FALSE)[,
    c("expression", "median")
  ])
}

cat("Regex matches per string, 1e6 strings\n")
b(
  base_perl = lengths(regmatches(x, gregexpr("[aeiou]{2}", x, perl = TRUE))),
  stringi = stringi::stri_count_regex(x, "[aeiou]{2}"),
  llutils_2 = par_count(x, "[aeiou]{2}", nthreads = 2),
  llutils_all = par_count(x, "[aeiou]{2}")
)
b(
  stringi_fixed = stringi::stri_count_fixed(x, "a"),
  llutils_2 = par_count(x, "a", fixed = TRUE, nthreads = 2),
  llutils_all = par_count(x, "a", fixed = TRUE)
)

cat("\nDictionary hits per string, 1e5 strings, 1000 terms\n")
x5 <- x[1:1e5]
terms <- sample(vocabulary, 1000)
stringi_loop <- function(x, terms) {
  out <- integer(length(x))
  for (t in terms) {
    out <- out + stringi::stri_count_fixed(x, t)
  }
  out
}
b(
  stringi_loop = stringi_loop(x5, terms),
  llutils_2 = par_count_term(x5, terms, nthreads = 2),
  llutils_all = par_count_term(x5, terms),
  whole_word = par_count_term(x5, terms, whole_word = TRUE)
)
cat("\n")
print(sessionInfo())
