withr::local_options(llutils.threads = 2) # CRAN allows at most 2 cores

test_that("par_detect() agrees with grepl() where the syntaxes agree", {
  set.seed(1)
  x <- vapply(
    1:6000,
    function(i) paste(sample(c(letters, " ", 0:9), 12, TRUE), collapse = ""),
    ""
  )
  for (p in c(
    "abc",
    "^[a-f]+",
    "\\d{2}$",
    "a.c|x.z",
    "(ab)+",
    "[^a-z ]{3}",
    ""
  )) {
    expect_identical(par_detect(x, p), grepl(p, x, perl = TRUE), info = p)
  }
  expect_identical(par_detect(x[1:10], "a"), grepl("a", x[1:10])) # serial path
})

test_that("fixed = TRUE finds the pattern as it is", {
  x <- rep(c("a.c", "abc", "(a)", NA), 2000) # long enough for the parallel path
  for (p in c(".", "a.c", "(a)", "(", "\\")) {
    expected <- grepl(p, x, fixed = TRUE)
    expected[is.na(x)] <- NA
    expect_identical(par_detect(x, p, fixed = TRUE), expected, info = p)
  }
  expect_identical(par_detect("a.c", "a.c", fixed = TRUE), TRUE)
  expect_error(par_detect("a", "a", fixed = NA), "fixed")
})

test_that("NA gives NA, and other inputs are converted", {
  expect_identical(par_detect(c("a", NA, "b"), "a"), c(TRUE, NA, FALSE))
  expect_identical(par_detect(factor(c("ab", "cd")), "b"), c(TRUE, FALSE))
  expect_identical(par_detect(c(10, 25), "^2"), c(FALSE, TRUE))
  expect_identical(par_detect(character(), "a"), logical())
})

test_that("strings match by character, in any encoding", {
  e <- intToUtf8(233) # e with acute accent
  latin1 <- iconv(e, "UTF-8", "latin1")
  expect_identical(
    par_detect(c(e, latin1, "e"), paste0("^", e, "$")),
    c(TRUE, TRUE, FALSE)
  )
  expect_identical(par_detect(c(e, latin1), "^.$"), c(TRUE, TRUE))
  big <- rep(c(e, latin1, "e", NA), 2000) # the parallel path
  expect_identical(
    par_detect(big, paste0("^", e, "$")),
    rep(c(TRUE, TRUE, FALSE, NA), 2000)
  )
  expect_identical(par_detect(c("\u0663", "3"), "\\d"), c(TRUE, TRUE)) # Arabic-Indic 3
  expect_identical(par_detect("ABC", "(?i)b"), TRUE)
})

test_that("bad patterns and arguments error", {
  expect_error(par_detect("a", "("), "regex parse error")
  expect_error(par_detect("a", "(a)\\1"), "backreferences")
  expect_error(par_detect("a", c("a", "b")), "single string")
  expect_error(par_detect("a", NA_character_), "single string")
  expect_error(par_detect("a", "a", nthreads = -1), "nthreads")
})

test_that("factors are matched by their levels", {
  f <- factor(rep(c("ab", NA, "cd", "ab"), 5000), levels = c("cd", "ab", "zz"))
  expect_identical(par_detect(f, "b"), par_detect(as.character(f), "b"))
  expect_identical(par_detect(f[1:3], "b"), c(TRUE, NA, FALSE))
})
