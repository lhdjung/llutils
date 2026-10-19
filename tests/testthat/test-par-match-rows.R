withr::local_options(parvenu.threads = 2) # CRAN allows at most 2 cores

# The definition, for columns whose printed values are exact: match() on
# rows pasted into strings.
match_rows_base <- function(x, table, nomatch = NA_integer_) {
  paste_rows <- function(d) do.call(paste, c(unname(as.list(d)), sep = "\r"))
  match(paste_rows(x), paste_rows(table), nomatch)
}

test_that("par_match_rows() matches rows in every column", {
  set.seed(9)
  rows <- function(n) {
    data.frame(
      i = sample(c(1:20, NA), n, TRUE),
      d = sample(c(-2:2 / 4, NA, NaN), n, TRUE),
      s = sample(c("a", "b", "c"), n, TRUE),
      date = as.Date("2026-01-01") + sample(0:5, n, TRUE)
    )
  }
  x <- rows(1e5)
  table <- rows(2e5) # long enough to split by hash
  expect_identical(par_match_rows(x, table), match_rows_base(x, table))
  expect_identical(par_match_rows(x, table, 0L), match_rows_base(x, table, 0L))
  expect_identical(par_match_rows(table, x), match_rows_base(table, x))
  expect_identical(
    par_match_rows(x[1:2], table[1:2]),
    match_rows_base(x[1:2], table[1:2])
  )
  expect_identical(
    par_match_rows(x[3], table[3]),
    match_rows_base(x[3], table[3])
  )
  expect_identical(
    par_match_rows(x[1:10, ], table[1:50, ]),
    match_rows_base(x[1:10, ], table[1:50, ])
  )
  expect_identical(
    par_match_rows(x[1:10, ], table),
    match_rows_base(x[1:10, ], table)
  ) # x short: scan
})

test_that("columns pair as par_match() pairs vectors", {
  x <- list(c(1L, 2L, 3L), factor(c("a", "b", "a")))
  table <- data.frame(n = c(3, 2, 1, 1), f = c("a", "b", "b", "a"))
  expect_identical(par_match_rows(x, table), c(4L, 2L, 1L))
  d <- as.Date(c(1.5, 2, 1))
  expect_identical(
    par_match_rows(list(d), list(as.Date(c(1, 2)))),
    match(d, as.Date(c(1, 2))) # by day before R 4.6.0, by number since
  )
  # Mixed or unhandled types go through par_match() first
  expect_identical(par_match_rows(list(c("1", "2")), list(1:2)), c(1L, 2L))
  expect_identical(
    par_match_rows(list(c(TRUE, NA)), list(c(NA, TRUE))),
    c(2L, 1L)
  )
  e <- intToUtf8(233)
  expect_identical(
    par_match_rows(
      list(e, 1L),
      list(c("e", iconv(e, "UTF-8", "latin1")), c(1L, 1L))
    ),
    2L
  )
})

test_that("bad arguments error", {
  expect_error(
    par_match_rows(data.frame(a = 1), data.frame(a = 1, b = 2)),
    "same number of columns"
  )
  expect_error(par_match_rows(list(1:2, 1), list(1, 1)), "same length")
  expect_error(par_match_rows(1:3, list(1)), "data frame")
  expect_error(par_match_rows(list(1), list(1), nthreads = -1), "nthreads")
})

test_that("par_match_rows() matches Dates beyond 1e10 days as base does", {
  # Before R 4.6.0, base matches these by their printed numbers, so 1e15 and
  # 1e15 + 1 are equal; since, by the numbers themselves.
  x <- list(.Date(c(1e15, rep(1, 2e4))), 1:20001)
  table <- list(.Date(c(1e15 + 1, rep(2, 2e4))), 1:20001)
  # Rows are equal where each column's first match in `table` is.
  key <- function(l) paste(match(l[[1]], table[[1]]), l[[2]])
  expect_identical(par_match_rows(x, table), match(key(x), key(table)))
})
