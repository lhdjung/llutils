withr::local_options(parvenu.threads = 2) # CRAN allows at most 2 cores

test_that("the set operations agree with base", {
  set.seed(8)
  n <- 3e4
  pairs <- list(
    list(sample(1e4, n, TRUE), sample(5e3, n, TRUE) + 7000L),
    list(sample(c(runif(50), NA, NaN, 0, -0), n, TRUE), c(runif(30), NA)),
    list(sample(1e4, n, TRUE), c(1, 2.5, 20)), # mixed types
    list(as.character(sample(1e4, n, TRUE)), c("5", "x", NA)),
    list(
      as.Date("2026-01-01") + sample(0:40, n, TRUE),
      as.Date("2026-01-30") + 0:20
    ),
    list(factor(sample(letters, n, TRUE)), factor(c("a", "z", "A"))),
    list(setNames(sample(100, n, TRUE), seq_len(n)), 1:50), # names dropped
    list(matrix(sample(100, n, TRUE), 2), 1:50),
    list(sample(100, 20, TRUE), sample(100, 20, TRUE)) # short
  )
  for (p in pairs) {
    x <- p[[1]]
    y <- p[[2]]
    expect_identical(par_union(x, y), union(x, y))
    expect_identical(par_union(y, x), union(y, x))
    expect_identical(par_intersect(x, y), intersect(x, y))
    expect_identical(par_intersect(y, x), intersect(y, x))
    expect_identical(par_setdiff(x, y), setdiff(x, y))
    expect_identical(par_setdiff(y, x), setdiff(y, x))
  }
  expect_identical(par_union(NULL, 1:3), union(NULL, 1:3))
  expect_identical(par_intersect(NULL, 1:3), intersect(NULL, 1:3))
  expect_identical(par_setdiff(NULL, 1:3), setdiff(NULL, 1:3))
  expect_identical(par_setdiff(1:3, NULL), setdiff(1:3, NULL))
})
