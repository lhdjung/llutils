# par_find_interval() against base and a data.table rolling join; run after
# installing parvenu.
library(parvenu)
set.seed(1)
x <- runif(1e7)
b <- function(...) {
  print(bench::mark(..., min_iterations = 3, filter_gc = FALSE)[,
    c("expression", "median")
  ])
}
for (m in c(100, 1e4, 1e6)) {
  breaks <- sort(runif(m))
  cat("\n1e7 values into", m, "breaks\n")
  b(
    base = findInterval(x, breaks),
    data.table = data.table::data.table(b = breaks, i = seq_along(breaks))[
      data.table::data.table(b = x),
      on = "b",
      roll = TRUE,
      x.i
    ],
    parvenu_2 = par_find_interval(x, breaks, nthreads = 2),
    parvenu_all = par_find_interval(x, breaks),
    check = FALSE # data.table gives NA where base gives 0
  )
}
cat("\nSorted x, 1e4 breaks: base needs no search from scratch\n")
xs <- sort(x)
breaks <- sort(runif(1e4))
b(
  base = findInterval(xs, breaks),
  parvenu_all = par_find_interval(xs, breaks)
)
cat("\n")
print(sessionInfo())
