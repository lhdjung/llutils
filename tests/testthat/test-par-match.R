withr::local_options(llutils.threads = 2) # CRAN allows at most 2 cores

# Long enough for two threads.
big <- function(v) rep_len(v, 2e4)

expect_base <- function(x, table) {
  expect_identical(par_match(x, table), match(x, table))
  expect_identical(par_match(x, table, nomatch = 0), match(x, table, 0))
  expect_identical(x %par_in% table, x %in% table)
  expect_identical(par_in(x, table), x %in% table)
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
  expect_base(big(c(1:5, NA)), c(1.5, 2, NaN, NA, 3)) # integer against double
  expect_base(big(dbl), c(1:3, NA))
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
  bad <- structure(c(1L, 3L), levels = c("a", "b"), class = "factor")
  expect_error(par_match(bad, "a"), "malformed factor")
  expect_error(par_match(big(bad), "a"), "malformed factor")
})

test_that("short and unsupported inputs go to base", {
  expect_base(1:3, 2L)
  expect_base(big(c(TRUE, NA)), c(NA, TRUE))
  expect_base(big(1:3), c("2", "3"))
  expect_base(big(list(1, "a")), list("a"))
})

test_that("incomparables goes to base, in base's position", {
  x <- big(c(1:3, NA))
  for (inc in list(2L, NA, TRUE, "1", FALSE)) {
    expect_identical(
      par_match(x, 3:1, 0L, inc),
      match(x, 3:1, 0L, inc),
      info = deparse(inc)
    )
  }
  expect_identical(
    par_match(factor(c("a", "b")), "a", incomparables = "a"),
    c(NA_integer_, NA)
  )
})

test_that("nthreads is validated", {
  x <- big(1L)
  expect_identical(par_match(x, 1L, nthreads = 1), x)
  expect_error(par_match(1L, 1L, nthreads = -1), "nthreads")
  expect_error(par_match(x, 1L, nthreads = 1.5), "nthreads")
  expect_error(par_match(x, 1L, nthreads = 1e10), "nthreads")
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

test_that("large tables, built in parallel, still give the first match", {
  set.seed(2)
  keys <- sample.int(1e6, 1e5)
  table <- c(keys, keys) # every key twice; the first must win
  expect_base(sample(c(keys, 0L), 1e5, TRUE), table)
  expect_base(as.character(sample(c(keys, 0L), 1e5, TRUE)), as.character(table))
  # A short `x` is hashed instead, and the table scanned in chunks: keys[1]
  # is in both chunks, and the first position must win.
  expect_base(1L, table)
  expect_base(c(keys[1], NA, 0L, keys[1], keys[1e5]), table)
  expect_base(sample(c(keys, 0L, NA), 1e4, TRUE), table)
  expect_base(as.character(c(keys[1], NA, 0L, keys[1])), as.character(table))
})

test_that("a forked child works after the parent has used a pool", {
  skip_on_os("windows")
  x <- big(1:3)
  expect_identical(par_match(x, 2:3), match(x, 2:3))
  # Some IDEs (Positron) refuse to fork. Errors in the children come back as
  # values, so this only skips when there was no fork to test.
  r <- tryCatch(
    parallel::mclapply(1:2, function(i) par_match(x, 2:3), mc.cores = 2),
    error = function(e) skip(paste("Can't fork:", conditionMessage(e)))
  )
  expect_identical(r, rep(list(match(x, 2:3)), 2))
})

test_that("more threads than cores count as all cores", {
  x <- sample.int(100L, 1e5, TRUE)
  expect_identical(par_match(x, 1:50, nthreads = 1e4), match(x, 1:50))
})
