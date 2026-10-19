test_that("par_rank() agrees with rank()", {
  set.seed(1)
  n <- 5e4
  xs <- list(
    sample(c(runif(100), NA, NaN, 0, -0, Inf, -Inf), n, TRUE),
    sample(c(1:50, NA), n, TRUE),
    rnorm(n),
    factor(sample(c(letters, NA), n, TRUE)),
    setNames(sample(10, n, TRUE) / 2, sample(letters, n, TRUE))
  )
  for (x in xs) {
    for (ties in c("average", "first", "last", "min", "max")) {
      for (na_last in list(TRUE, FALSE, NA, "keep")) {
        expect_identical(
          par_rank(x, na.last = na_last, ties.method = ties),
          rank(x, na.last = na_last, ties.method = ties)
        )
      }
    }
  }
})
