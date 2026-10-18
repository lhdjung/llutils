withr::local_options(llutils.threads = 2) # CRAN allows at most 2 cores

test_that("par_find_interval() agrees with findInterval()", {
  set.seed(6)
  x <- c(sample(0:12, 2e4, TRUE) / 2, NA, NaN, Inf, -Inf)
  for (vec in list(c(1, 2, 2, 3.5, 5), 1:5, 3, c(2, 2), c(-Inf, 0, Inf))) {
    for (args in expand.grid(
      rc = c(FALSE, TRUE),
      ai = c(FALSE, TRUE),
      lo = c(FALSE, TRUE)
    ) |>
      split(1:8)) {
      expect_identical(
        par_find_interval(x, vec, args$rc, args$ai, args$lo),
        findInterval(x, vec, args$rc, args$ai, args$lo),
        info = paste(c(vec, unlist(args)), collapse = ",")
      )
    }
  }
  xi <- sample.int(100, 2e4, TRUE)
  expect_identical(
    par_find_interval(xi, c(10L, 50L)),
    findInterval(xi, c(10L, 50L))
  )
  d <- as.Date("2026-01-01") + sample(0:365, 2e4, TRUE)
  breaks <- as.Date(c("2026-03-01", "2026-06-01", "2026-09-01"))
  expect_identical(par_find_interval(d, breaks), findInterval(d, breaks))
})

test_that("other inputs go to findInterval()", {
  x <- runif(2e4)
  expect_identical(par_find_interval(x, numeric()), findInterval(x, numeric()))
  expect_identical(par_find_interval(1:3, 2), findInterval(1:3, 2)) # short
  expect_identical(par_find_interval(x > 0.5, 0.5), findInterval(x > 0.5, 0.5))
  expect_error(par_find_interval(x, c(2, 1)), "sorted")
  expect_error(par_find_interval(x, c(1, NA)), "sorted")
  expect_error(par_find_interval(x, 1, nthreads = -1), "nthreads")
})
