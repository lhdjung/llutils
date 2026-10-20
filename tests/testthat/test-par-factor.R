withr::local_options(parvenu.threads = 2) # CRAN allows at most 2 cores

set.seed(10)
n <- 3e4
xs <- list(
  sample(c(letters, NA), n, TRUE),
  sample(c(1:50, NA), n, TRUE),
  sample(c(runif(20), NA, NaN, 0, -0, 0.1 + 0.2, 0.3), n, TRUE),
  sample(c(TRUE, FALSE, NA), n, TRUE),
  as.Date("2026-01-01") + sample(0:30, n, TRUE),
  factor(sample(c("b", "a", NA), n, TRUE), levels = c("b", "a", "zz")),
  setNames(sample(5, n, TRUE), seq_len(n)),
  sample(letters, 20, TRUE) # short
)

test_that("par_factor() agrees with factor()", {
  for (x in xs) {
    expect_identical(par_factor(x), factor(x))
    expect_identical(par_factor(x, exclude = NULL), factor(x, exclude = NULL))
    expect_identical(par_factor(x, ordered = TRUE), factor(x, ordered = TRUE))
    expect_identical(par_factor(x, labels = "L"), factor(x, labels = "L"))
    lv <- rev(levels(factor(x)))
    expect_identical(par_factor(x, lv), factor(x, lv))
    expect_identical(
      par_factor(x, exclude = lv[1:2]),
      factor(x, exclude = lv[1:2])
    )
    lab <- rep(c("p", "q"), length.out = length(lv)) # duplicated labels merge
    expect_identical(par_factor(x, lv, lab), factor(x, lv, lab))
  }
  expect_identical(par_factor(NULL), factor(NULL))
  expect_identical(par_factor(), factor())
  expect_error(par_factor(1:3, labels = c("a", "b")), "invalid 'labels'")
})

test_that("par_table() agrees with table()", {
  for (x in xs[1:7]) {
    expect_identical(par_table(x), table(x))
    expect_identical(par_table(x, useNA = "ifany"), table(x, useNA = "ifany"))
    expect_identical(par_table(x, useNA = "always"), table(x, useNA = "always"))
    expect_identical(par_table(x, exclude = NULL), table(x, exclude = NULL))
    v <- levels(factor(x))[1]
    expect_identical(par_table(x, exclude = v), table(x, exclude = v))
    expect_identical(par_table(x, dnn = "v"), table(x, dnn = "v"))
    expect_identical(par_table(key = x), table(key = x))
  }
  a <- xs[[1]]
  b <- xs[[2]]
  expect_identical(par_table(a, b), table(a, b))
  expect_identical(
    par_table(a, b, useNA = "ifany"),
    table(a, b, useNA = "ifany")
  )
  expect_identical(par_table(list(a, b)), table(list(a, b)))
  expect_identical(par_table(data.frame(a, b)), table(data.frame(a, b)))
  expect_identical(par_table(a[1:10]), table(a[1:10]))
  expect_error(par_table(a, 1:3), "same length")
  expect_error(par_table(), "nothing to tabulate")
})
