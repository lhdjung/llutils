# par_unique() against base and collapse, and distinct counts against base,
# collapse, and data.table; run after installing llutils.
library(llutils)
set.seed(1)
n <- 1e7
xi <- sample.int(1e6, n, TRUE)
for (x in list(
  integers = xi,
  doubles = runif(1e6)[xi],
  strings = as.character(xi)
)) {
  cat("\n1e7 elements, 1e6 distinct:", typeof(x), "\n")
  print(bench::mark(
    base = unique(x),
    collapse = collapse::funique(x, sort = FALSE),
    llutils_2 = par_unique(x, nthreads = 2),
    llutils_all = par_unique(x),
    min_iterations = 5,
    filter_gc = FALSE
  )[, c("expression", "median")])
}
cat("\nDistinct count, integers\n")
print(bench::mark(
  base = length(unique(xi)),
  collapse = collapse::fnunique(xi),
  data.table = data.table::uniqueN(xi),
  llutils_all = sum(!par_duplicated(xi)),
  min_iterations = 5,
  filter_gc = FALSE
)[, c("expression", "median")])
cat("\n")
print(sessionInfo())
