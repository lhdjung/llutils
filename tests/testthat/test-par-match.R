withr::local_options(llutils.threads = 2) # CRAN allows at most 2 cores

# Long enough for the parallel path.
big <- function(v) rep_len(v, 1e5)

expect_base <- function(x, table) {
  expect_identical(par_match(x, table), match(x, table))
  expect_identical(par_match(x, table, nomatch = 0), match(x, table, 0))
  expect_identical(x %par_in% table, x %in% table)
}

test_that("integers and doubles match as in base", {
  set.seed(1)
  expect_base(
    big(c(sample.int(1e3, 500), NA)),
    c(sample.int(2e3, 500, TRUE), NA)
  )
  dbl <- c(0, -0, NA, NaN, Inf, -Inf, 0.1 + 0.2, 0.3, 1.5, 2)
  expect_base(big(dbl), rev(dbl))
  expect_base(big(dbl), c(NaN, 0.3, 2)) # NaN isn't NA, -0 is 0
  expect_base(big(1:5), c(1.5, 2, 3)) # integer against double
  expect_base(big(dbl), 1:3)
})

test_that("strings match as in base, in any encoding", {
  utf8 <- intToUtf8(233) # e with acute accent
  native <- utf8
  Encoding(native) <- "unknown" # same bytes, unmarked
  latin1 <- iconv(utf8, "UTF-8", "latin1")
  expect_base(big(c("a", NA, "b", utf8)), c(utf8, NA, "b"))
  expect_base(big(c("a", native)), c("b", utf8))
  expect_base(big(c("a", utf8)), c("a", latin1))
  expect_base(big(c("a", "b")), factor(c("b", NA)))
})

test_that("Dates and POSIXct match as in base", {
  d <- as.Date(c(1, 1.5, -0.5, NA, NaN, Inf, 20000))
  expect_base(big(d), as.Date(c(-1, 1, 20000.9, NA, NaN)))
  expect_base(big(as.Date(c(1e15, 1))), as.Date(1e15 + 1)) # printed as numbers
  expect_base(big(d), c(1, 20000)) # Date against double
  t <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC") + c(0, 0.5, NA)
  expect_base(big(t), `attr<-`(t[2:1], "tzone", "Europe/Berlin"))
  expect_base(big(t), as.vector(t))
})

test_that("factors match by level", {
  f <- factor(c("b", NA, "a", "c"), levels = c("c", "b", "a", "z"))
  expect_base(big(f), c("a", NA))
  expect_base(big(f), c("a", "b"))
  expect_base(f, factor(c("z", "c")))
})

test_that("short and unsupported inputs go to base", {
  expect_base(1:3, 2L)
  expect_base(big(c(TRUE, NA)), c(NA, TRUE))
  expect_base(big(1:3), c("2", "3"))
  expect_base(big(list(1, "a")), list("a"))
})

test_that("nthreads is validated", {
  x <- big(1L)
  expect_identical(par_match(x, 1L, nthreads = 1), x)
  expect_error(par_match(1L, 1L, nthreads = -1), "nthreads")
  expect_error(par_match(x, 1L, nthreads = 1.5), "nthreads")
})

test_that("Rust takes the cases it can and declines the rest", {
  utf8 <- intToUtf8(233) # e with acute accent
  native <- utf8
  Encoding(native) <- "unknown"
  chr <- function(x, table) par_match_chr(x, table, NA_integer_, 2L)
  expect_identical(chr(c("a", utf8), utf8), c(NA, 1L))
  expect_identical(chr(c("a", native), "a"), c(1L, NA))
  expect_null(chr(c("a", native), utf8))
  expect_null(chr("a", iconv(utf8, "UTF-8", "latin1")))
  dates <- function(x) par_match_dbl(x, 1, NA_integer_, 2L, TRUE)
  expect_identical(dates(c(1.5, 2)), c(1L, NA))
  expect_null(dates(1e15))
})
