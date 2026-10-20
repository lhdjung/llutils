withr::local_options(parvenu.threads = 2) # CRAN allows at most 2 cores

# The definition: every pair of positions whose elements match() the same
# element of `table`, sorted by `x` and then `table`.
match_all_base <- function(x_first, table_first) {
  pairs <- expand.grid(table = seq_along(table_first), x = seq_along(x_first))
  same <- x_first[pairs$x] == table_first[pairs$table]
  pairs <- pairs[!is.na(same) & same, c("x", "table")]
  rownames(pairs) <- NULL
  pairs
}

test_that("par_match_all() pairs every equal element", {
  set.seed(3)
  x <- sample(c(1:30, NA, NaN), 400, TRUE)
  table <- sample(c(1:40, NA), 300, TRUE)
  expect_identical(
    par_match_all(x, table),
    match_all_base(match(x, table), match(table, table))
  )
  expect_identical(
    par_match_all(table, table),
    match_all_base(match(table, table), match(table, table))
  )
  # Long enough for the Rust path and a split by hash
  x <- sample(1e5, 2e5, TRUE)
  table <- sample(1e5, 7e4, TRUE)
  pairs <- par_match_all(x, table)
  expect_true(all(x[pairs$x] == table[pairs$table]))
  expect_identical(
    nrow(pairs),
    sum(tabulate(match(table, table))[match(x, table)], na.rm = TRUE)
  )
  expect_false(is.unsorted(pairs$x * 1e6 + pairs$table, strictly = TRUE))
})

test_that("par_match_rows_all() pairs every equal row", {
  set.seed(4)
  rows <- function(n) {
    data.frame(
      i = sample(c(1:3, NA), n, TRUE),
      s = sample(c("a", "b"), n, TRUE),
      d = sample(c(0.5, -0, 0, NaN), n, TRUE)
    )
  }
  x <- rows(200)
  table <- rows(150)
  expect_identical(
    par_match_rows_all(x, table),
    match_all_base(par_match_rows(x, table), par_match_rows(table, table))
  )
})

test_that("par_match_all() handles no matches and empty inputs", {
  empty <- data.frame(x = integer(), table = integer())
  expect_identical(par_match_all(1:3, 4:6), empty)
  expect_identical(par_match_all(integer(), 1:3), empty)
  expect_identical(par_match_all(1:3, integer()), empty)
  expect_identical(
    par_match_all(c(2, 2), c(2, 2)),
    data.frame(
      x = c(1L, 1L, 2L, 2L),
      table = c(1L, 2L, 1L, 2L)
    )
  )
})
