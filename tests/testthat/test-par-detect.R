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
  expect_error(par_detect("a", c("a", "b")), "as long as `x`")
  expect_error(par_detect("a", 1), "as long as `x`")
  expect_error(par_detect(c("a", "b"), c("a", "(")), "regex parse error")
  expect_error(par_detect("a", "a", nthreads = -1), "nthreads")
})

test_that("factors are matched by their levels", {
  f <- factor(rep(c("ab", NA, "cd", "ab"), 5000), levels = c("cd", "ab", "zz"))
  expect_identical(par_detect(f, "b"), par_detect(as.character(f), "b"))
  expect_identical(par_detect(f[1:3], "b"), c(TRUE, NA, FALSE))
})

test_that("pattern can have one element per string", {
  set.seed(3)
  x <- vapply(
    1:6000,
    function(i) paste(sample(c(letters[1:4], " "), 6, TRUE), collapse = ""),
    ""
  )
  x[9] <- NA
  p <- sample(c("ab", "^c", "d$", "a.b", " "), 6000, TRUE)
  p[c(1, 9, 10)] <- NA
  expected <- mapply(
    grepl,
    p,
    x,
    MoreArgs = list(perl = TRUE),
    USE.NAMES = FALSE
  )
  expected[is.na(x) | is.na(p)] <- NA
  expect_identical(par_detect(x, p), expected)
  expect_identical(par_detect(x[1:20], p[1:20]), expected[1:20]) # serial path
  expect_identical(par_detect(x, rep("ab", 6000)), par_detect(x, "ab"))
  expect_identical(
    par_detect(c("a.c", "abc"), c(".", "."), fixed = TRUE),
    c(TRUE, FALSE)
  )
  expect_identical(par_detect(c("a", "b"), NA_character_), c(NA, NA))
  f <- factor(c("ab", "cd", "ab"))
  expect_identical(par_detect(f, c("a", "a", "b")), c(TRUE, FALSE, TRUE))
})

test_that("par_count() counts non-overlapping matches, as gregexpr() does", {
  count_base <- function(x, p, ...) {
    out <- vapply(gregexpr(p, x, ...), function(m) sum(m > 0), 1L)
    out[is.na(x)] <- NA
    out
  }
  set.seed(4)
  x <- vapply(
    1:6000,
    function(i) {
      paste(sample(c(letters[1:3], " ", 0:2), 12, TRUE), collapse = "")
    },
    ""
  )
  x[c(3, 3000)] <- NA
  for (p in c("a", "ab|ba", "[0-9]+", "\\bc", "aa")) {
    expect_identical(par_count(x, p), count_base(x, p, perl = TRUE), info = p)
  }
  expect_identical(
    par_count(x, ".", fixed = TRUE),
    count_base(x, ".", fixed = TRUE)
  )
  expect_identical(
    par_count(c("aaaa", "banana", NA), c("aa", "an", "a")),
    c(2L, 2L, NA)
  )
  f <- factor(rep(c("banana", NA, "kiwi"), 4000))
  expect_identical(par_count(f, "an"), par_count(as.character(f), "an"))
  expect_identical(par_count(character(), "a"), integer())
})

test_that("par_count() treats empty matches as Rust does, unlike gregexpr()", {
  # Rust counts one at the end of a non-empty string, but not one right
  # after a non-empty match; gregexpr() does the opposite.
  x <- c("abc", "banana", "")
  expect_identical(par_count(x, ""), c(4L, 7L, 1L))
  expect_identical(par_count(x, "a*"), c(3L, 4L, 1L))
})
