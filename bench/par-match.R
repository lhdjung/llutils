# par_match() against base and collapse; run after installing llutils.
# Lookups into 1e4 keys, as in the plan's baseline.
library(llutils)
set.seed(1)
b <- function(x, table) {
  print(bench::mark(
    base = match(x, table),
    collapse = collapse::fmatch(x, table),
    llutils_2 = par_match(x, table, nthreads = 2),
    llutils_all = par_match(x, table),
    min_iterations = 3, filter_gc = FALSE
  )[, c("expression", "median")])
}
keys <- sample.int(1e6, 1e4)
for (n in 10^(4:8)) {
  cat("\nintegers, n =", format(n, scientific = TRUE), "\n")
  b(sample.int(1e6, n, TRUE), keys)
}
x <- sample.int(1e6, 1e7, TRUE)
cat("\ndoubles, n = 1e7\n"); b(as.double(x), as.double(keys))
cat("\ninteger against double, n = 1e7\n"); b(x, as.double(keys))
cat("\ncharacter, n = 1e7\n"); b(as.character(x), as.character(keys))
cat("\nfactor (100 levels), n = 1e7\n")
b(factor(sample(as.character(keys[1:100]), 1e7, TRUE)), as.character(keys))
cat("\nDate, n = 1e7\n"); b(as.Date(x), as.Date(keys))
cat("\nPOSIXct, n = 1e7\n"); b(as.POSIXct(x, tz = "UTC"), as.POSIXct(keys, tz = "UTC"))
