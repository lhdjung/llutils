# par_duplicated() against base, collapse, and data.table; run after
# installing parvenu.
library(parvenu)
set.seed(1)
n <- 1e7
b <- function(...) {
  print(bench::mark(..., min_iterations = 5, filter_gc = FALSE)[,
    c("expression", "median")
  ])
}
xi <- sample.int(1e6, n, TRUE)
xd <- runif(1e6)[xi]
xs <- as.character(xi)
for (x in list(integers = xi, doubles = xd, strings = xs)) {
  cat("\n1e7 elements, 1e6 distinct:", typeof(x), "\n")
  b(
    base = duplicated(x),
    collapse = collapse::fduplicated(x),
    data.table = duplicated(data.table::data.table(x)),
    parvenu_2 = par_duplicated(x, nthreads = 2),
    parvenu_all = par_duplicated(x)
  )
}
cat("\nRows of a data frame: integer, double, and string columns, 1e7 rows\n")
df <- data.frame(
  i = sample.int(1e3, n, TRUE),
  d = sample(1:100 / 4, n, TRUE),
  s = sample(as.character(1:100), n, TRUE)
)
b(
  collapse = collapse::fduplicated(df),
  data.table = duplicated(data.table::as.data.table(df)),
  parvenu_2 = par_duplicated(df, nthreads = 2),
  parvenu_all = par_duplicated(df),
  check = FALSE # collapse::fduplicated() returns an unnamed logical too, but
  # data.table checks fail on attributes
)
cat("\n")
print(sessionInfo())
