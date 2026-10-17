withr::local_options(llutils.threads = 2) # CRAN allows at most 2 cores

test_that("detect_regex() agrees with grepl() where the syntaxes agree", {
  set.seed(1)
  x <- vapply(1:6000, function(i) paste(sample(c(letters, " ", 0:9), 12, TRUE), collapse = ""), "")
  for (p in c("abc", "^[a-f]+", "\\d{2}$", "a.c|x.z", "(ab)+", "[^a-z ]{3}", "")) {
    expect_identical(detect_regex(x, p), grepl(p, x, perl = TRUE), info = p)
  }
  expect_identical(detect_regex(x[1:10], "a"), grepl("a", x[1:10])) # serial path
})

test_that("NA gives NA, and other inputs are converted", {
  expect_identical(detect_regex(c("a", NA, "b"), "a"), c(TRUE, NA, FALSE))
  expect_identical(detect_regex(factor(c("ab", "cd")), "b"), c(TRUE, FALSE))
  expect_identical(detect_regex(c(10, 25), "^2"), c(FALSE, TRUE))
  expect_identical(detect_regex(character(), "a"), logical())
})

test_that("strings match by character, in any encoding", {
  e <- intToUtf8(233) # e with acute accent
  latin1 <- iconv(e, "UTF-8", "latin1")
  expect_identical(detect_regex(c(e, latin1, "e"), paste0("^", e, "$")), c(TRUE, TRUE, FALSE))
  expect_identical(detect_regex(c(e, latin1), "^.$"), c(TRUE, TRUE))
  big <- rep(c(e, latin1, "e", NA), 2000) # the parallel path
  expect_identical(detect_regex(big, paste0("^", e, "$")), rep(c(TRUE, TRUE, FALSE, NA), 2000))
  expect_identical(detect_regex(c("\u0663", "3"), "\\d"), c(TRUE, TRUE)) # Arabic-Indic 3
  expect_identical(detect_regex("ABC", "(?i)b"), TRUE)
})

test_that("bad patterns and arguments error", {
  expect_error(detect_regex("a", "("), "regex parse error")
  expect_error(detect_regex("a", "(a)\\1"), "backreferences")
  expect_error(detect_regex("a", c("a", "b")), "single string")
  expect_error(detect_regex("a", NA_character_), "single string")
  expect_error(detect_regex("a", "a", nthreads = -1), "nthreads")
})
