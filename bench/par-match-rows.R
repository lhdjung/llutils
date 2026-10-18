# par_match_rows() against collapse, vctrs, data.table, and pasted keys; run
# after installing llutils.
library(llutils)
set.seed(1)
n <- 1e7
b <- function(...) {
  print(bench::mark(..., min_iterations = 3, filter_gc = FALSE)[,
    c("expression", "median")
  ])
}
dt_match <- function(x, table) {
  t <- data.table::as.data.table(table)[, .i := .I]
  t[data.table::as.data.table(x), on = names(x), mult = "first", x..i]
}
for (hi in c(FALSE, TRUE)) {
  m <- if (hi) 1e7 else 1e4
  x <- data.frame(a = sample.int(m, n, TRUE), b = sample.int(1e3, n, TRUE))
  table <- data.frame(a = sample.int(m, n, TRUE), b = sample.int(1e3, n, TRUE))
  cat("\nTwo integer columns, 1e7 rows into 1e7; first column up to", m, "\n")
  stopifnot(identical(par_match_rows(x, table), collapse::fmatch(x, table)))
  b(
    collapse = collapse::fmatch(x, table),
    vctrs = vctrs::vec_match(x, table),
    data.table = dt_match(x, table),
    llutils_2 = par_match_rows(x, table, nthreads = 2),
    llutils_all = par_match_rows(x, table),
    check = FALSE
  )
}
cat("\nAdd a string column\n")
x$s <- as.character(x$b)
table$s <- as.character(table$b)
b(
  collapse = collapse::fmatch(x, table),
  vctrs = vctrs::vec_match(x, table),
  llutils_all = par_match_rows(x, table)
)
cat("\nPasted keys, as in base R, 1e6 rows\n")
x6 <- x[1:1e6, ]
t6 <- table[1:1e6, ]
b(
  base_paste = match(paste(x6$a, x6$b, x6$s), paste(t6$a, t6$b, t6$s)),
  llutils_all = par_match_rows(x6, t6),
  check = FALSE
)
cat("\n")
print(sessionInfo())
