test_that("par_approx() agrees with approx()", {
  set.seed(5)
  xout <- c(runif(1e5, -1, 12), NA, NaN, 0, 10, Inf)
  points <- list(
    list(x = sort(runif(50, 0, 10)), y = rnorm(50)),
    list(x = c(3, 1, 2, 2, 5, NA), y = c(1, 2, 3, 4, NA, 6)),
    list(x = 1:10, y = (1:10)^2)
  )
  options <- list(
    list(),
    list(rule = 2),
    list(rule = c(1, 2)),
    list(method = "constant", f = 0.3),
    list(yleft = -1, yright = 99),
    list(ties = max)
  )
  for (p in points) {
    for (o in options) {
      expect_identical(
        suppressWarnings(do.call(
          par_approx,
          c(list(p$x, p$y, xout = xout), o)
        )),
        suppressWarnings(do.call(approx, c(list(p$x, p$y, xout = xout), o)))
      )
    }
  }
  expect_identical(par_approx(1:10, xout = xout), approx(1:10, xout = xout))
  expect_identical(par_approx(1:3, n = 5), approx(1:3, n = 5))
  expect_error(par_approx(1, 1, xout = xout), "two non-NA values")
})
