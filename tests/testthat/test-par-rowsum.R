test_that("par_rowsum() agrees with rowsum() to the bit", {
  set.seed(4)
  n <- 1e5
  groups <- list(
    sample(c(1:300, NA), n, TRUE),
    sample(c(letters, NA), n, TRUE),
    sample(c(0.5, 1, -0, 0, NaN, NA), n, TRUE),
    factor(sample(letters[1:5], n, TRUE), levels = letters[5:1]),
    # Groups match by their numbers, not as match() compares these
    as.difftime(sample(c(1, 1 + 2^-50), n, TRUE), units = "secs"),
    .Date(sample(c(1, 1.5, 2), n, TRUE))
  )
  xs <- list(
    c(rnorm(n - 3) * 1e10, NA, NaN, Inf),
    sample(c(-1e9L, 1e9L, 3L, NA), n, TRUE),
    matrix(rnorm(2 * n), n, dimnames = list(NULL, c("a", "b"))),
    matrix(sample(c(1:9, NA), 2 * n, TRUE), n)
  )
  for (g in groups) {
    for (x in xs) {
      for (na_rm in c(FALSE, TRUE)) {
        for (reorder in c(TRUE, FALSE)) {
          expect_identical(
            suppressWarnings(par_rowsum(x, g, reorder, na_rm)),
            suppressWarnings(rowsum(x, g, reorder, na_rm))
          )
        }
      }
    }
  }
  expect_warning(par_rowsum(xs[[1]], groups[[1]]), "missing values")
})
