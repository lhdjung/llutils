# par_extract(), par_extract_all(), par_replace(), and par_replace_all()
# against base and stringi; run after installing parvenu.
library(parvenu)
set.seed(1)
words <- vapply(
  1:1e4,
  function(i) paste(sample(letters, 12, TRUE), collapse = ""),
  ""
)
x <- paste(
  sample(words, 1e6, TRUE),
  sample(words, 1e6, TRUE),
  sample(1:999, 1e6, TRUE)
)
b <- function(...) {
  print(bench::mark(..., min_iterations = 3, filter_gc = FALSE)[,
    c("expression", "median")
  ])
}
first <- function(x, m) {
  out <- rep(NA_character_, length(x))
  out[m > 0] <- regmatches(x, m)
  out
}
for (pat in c("[aeiou]{3}[0-9]", "[0-9]+$")) {
  cat("\nExtract the first match of", pat, "  n = 1e6\n")
  b(
    base_perl = first(x, regexpr(pat, x, perl = TRUE)),
    stringi = stringi::stri_extract_first_regex(x, pat),
    parvenu_2 = par_extract(x, pat, nthreads = 2),
    parvenu_all = par_extract(x, pat)
  )
}
cat("\nExtract a group: the word before the number\n")
b(
  base_perl = regmatches(x, regexec("(\\w+) [0-9]+$", x, perl = TRUE)),
  stringi = stringi::stri_match_first_regex(x, "(\\w+) [0-9]+$")[, 2],
  parvenu_2 = par_extract(x, "(\\w+) [0-9]+$", group = 1, nthreads = 2),
  parvenu_all = par_extract(x, "(\\w+) [0-9]+$", group = 1),
  check = FALSE
)
for (pat in c("[aeiou]{3}", "[0-9]", "zzz")) {
  cat("\nReplace every match of", pat, "  n = 1e6\n")
  b(
    base = gsub(pat, "_", x),
    base_perl = gsub(pat, "_", x, perl = TRUE),
    stringi = stringi::stri_replace_all_regex(x, pat, "_"),
    parvenu_2 = par_replace_all(x, pat, "_", nthreads = 2),
    parvenu_all = par_replace_all(x, pat, "_")
  )
}
cat("\nExtract every match of [0-9]: 3 per string on average\n")
b(
  base_perl = regmatches(x, gregexpr("[0-9]", x, perl = TRUE)),
  stringi = stringi::stri_extract_all_regex(x, "[0-9]"),
  parvenu_2 = par_extract_all(x, "[0-9]", nthreads = 2),
  parvenu_all = par_extract_all(x, "[0-9]"),
  check = FALSE
)
for (pat in c("[aeiou]{3}", "[0-9]")) {
  cat("\nReplace the first match of", pat, "  n = 1e6\n")
  b(
    base_perl = sub(pat, "_", x, perl = TRUE),
    stringi = stringi::stri_replace_first_regex(x, pat, "_"),
    parvenu_2 = par_replace(x, pat, "_", nthreads = 2),
    parvenu_all = par_replace(x, pat, "_")
  )
}
cat("\nSwap two groups\n")
b(
  base_perl = gsub("^(\\w+) (\\w+)", "\\2 \\1", x, perl = TRUE),
  stringi = stringi::stri_replace_all_regex(x, "^(\\w+) (\\w+)", "$2 $1"),
  parvenu_all = par_replace_all(x, "^(\\w+) (\\w+)", "$2 $1")
)
cat("\n")
print(sessionInfo())
