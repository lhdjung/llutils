withr::local_options(parvenu.threads = 2) # CRAN allows at most 2 cores

test_that("par_duplicated() agrees with duplicated() on vectors", {
  set.seed(7)
  n <- 1e5 # long enough to split by hash
  d <- c(sample(c(-2:2 / 4, -0, NA, NaN, Inf), n, TRUE))
  xs <- list(
    sample.int(1000, n, TRUE),
    c(sample.int(1000, n, TRUE), NA),
    d,
    as.character(sample.int(1000, n, TRUE)),
    factor(sample(c(letters, NA), n, TRUE)),
    as.Date("2026-01-01") + sample(0:3, n, TRUE) / 2, # not matched by day
    as.POSIXct("2026-01-01", tz = "UTC") + sample(1e3, n, TRUE),
    sample.int(1000, 2e4, TRUE) # one map, on one thread
  )
  for (x in xs) {
    for (fl in c(FALSE, TRUE)) {
      expect_identical(
        par_duplicated(x, fromLast = fl),
        duplicated(x, fromLast = fl)
      )
    }
  }
})

test_that("strings match by character, in any encoding", {
  e <- intToUtf8(233)
  latin1 <- iconv(e, "UTF-8", "latin1")
  x <- rep(c(e, latin1, "e", NA), 5000)
  expect_identical(par_duplicated(x), duplicated(x))
})

test_that("par_duplicated() agrees with duplicated() on data frame rows", {
  set.seed(8)
  n <- 1e5
  df <- data.frame(
    i = sample.int(30, n, TRUE),
    d = sample(c(0, -0, NA, NaN, 0.5), n, TRUE),
    s = sample(c("a", "b", NA), n, TRUE),
    f = factor(sample(c("x", "y"), n, TRUE)),
    l = sample(c(TRUE, FALSE, NA), n, TRUE)
  )
  for (fl in c(FALSE, TRUE)) {
    expect_identical(
      par_duplicated(df, fromLast = fl),
      duplicated(df, fromLast = fl)
    )
  }
  expect_identical(par_duplicated(df["i"]), duplicated(df["i"]))
  expect_identical(par_duplicated(df[c("i", "s")]), duplicated(df[c("i", "s")]))
  e <- intToUtf8(233)
  df$s <- rep(c(e, iconv(e, "UTF-8", "latin1")), n / 2)
  expect_identical(par_duplicated(df), duplicated(df))
})

test_that("other inputs go to duplicated()", {
  x <- sample.int(10, 2e4, TRUE)
  expect_identical(
    par_duplicated(x, incomparables = 1),
    duplicated(x, incomparables = 1)
  )
  expect_identical(par_duplicated(x > 5), duplicated(x > 5))
  m <- matrix(x, ncol = 2)
  expect_identical(par_duplicated(m), duplicated(m))
  expect_identical(par_duplicated(list(1, 2, 1)), c(FALSE, FALSE, TRUE))
  expect_identical(par_duplicated(c(1, 2, 1)), c(FALSE, FALSE, TRUE))
  expect_error(par_duplicated(x, nthreads = -1), "nthreads")
})

test_that("par_unique() agrees with unique()", {
  set.seed(3)
  n <- 3e4
  xs <- list(
    sample.int(1000, n, TRUE),
    c(sample(c(-0, 0, NA, NaN, 1.5), n, TRUE)),
    as.character(sample.int(1000, n, TRUE)),
    factor(sample(c(letters, NA), n, TRUE)),
    factor(sample(letters, n, TRUE), ordered = TRUE),
    as.Date("2026-01-01") + sample(0:30, n, TRUE),
    as.POSIXct("2026-01-01", tz = "UTC") + sample(1e3, n, TRUE),
    setNames(sample(5, n, TRUE), rep("a", n)), # names dropped
    c(3, 1, 3), # base
    matrix(c(1, 1, 2, 2), 2), # unique() by rows
    list(1, 1, "a"),
    structure(factor(sample(letters, n, TRUE)), class = c("myf", "factor")),
    `contrasts<-`(factor(sample(letters[1:3], n, TRUE)), value = contr.sum(3)),
    data.frame(a = sample(3, n, TRUE), b = sample(letters[1:3], n, TRUE))
  )
  for (x in xs) {
    for (fl in c(FALSE, TRUE)) {
      expect_identical(par_unique(x, fromLast = fl), unique(x, fromLast = fl))
    }
  }
})
