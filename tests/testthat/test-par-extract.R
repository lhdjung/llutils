withr::local_options(parvenu.threads = 2) # CRAN allows at most 2 cores

set.seed(1)
x <- vapply(
  1:6000,
  function(i) paste(sample(c(letters[1:6], " ", 0:2), 12, TRUE), collapse = ""),
  ""
)
x[c(5, 500)] <- NA

test_that("par_extract() gives the first match, or NA", {
  first <- function(x, p) {
    m <- regexpr(p, x, perl = TRUE)
    out <- rep(NA_character_, length(x))
    out[!is.na(m) & m > 0] <- regmatches(x, m)
    out
  }
  for (p in c("[0-9]+", "a.c", "^b", "")) {
    expect_identical(par_extract(x, p), first(x, p), info = p)
  }
  expect_identical(par_extract(x[1:10], "[a-c]+"), first(x[1:10], "[a-c]+"))
  expect_identical(
    par_extract(c("a.c", "abc"), ".", fixed = TRUE),
    c(".", NA)
  )
})

test_that("group = n extracts a capture group", {
  skip_if_not_installed("stringi")
  p <- "([a-c]+)( )?([0-9])"
  m <- stringi::stri_match_first_regex(x, p)
  for (g in 0:3) {
    expect_identical(par_extract(x, p, group = g), m[, g + 1], info = g)
  }
  expect_identical(
    par_extract("born 1999", "born (?<year>[0-9]+)", group = 1),
    "1999"
  )
  expect_error(par_extract("a", "(a)", group = 2), "no group 2")
  expect_error(par_extract("a", "a", group = -1), "group")
  expect_error(par_extract("a", "a", group = 1.5), "group")
})

test_that("par_replace() agrees with gsub()", {
  for (p in c("[0-9]+", "a", " ", "^", "b*")) {
    expect_identical(
      par_replace(x, p, "<$0>"),
      gsub(paste0("(", p, ")"), "<\\1>", x),
      info = p
    )
  }
  expect_identical(
    par_replace(x, "([a-c])([0-9])", "$2${1}x"),
    gsub("([a-c])([0-9])", "\\2\\1x", x)
  )
  expect_identical(
    par_replace(x, ".", "$1", fixed = TRUE),
    gsub(".", "$1", x, fixed = TRUE)
  )
  expect_identical(
    par_replace(c("a$b", NA), "$", "$$", fixed = TRUE),
    c("a$$b", NA)
  )
  expect_identical(par_replace("ab", "b", "$$"), "a$")
  expect_error(par_replace("a", "a", NA_character_), "replacement")
  expect_error(par_replace("a", "a", c("b", "c")), "replacement")
})

test_that("results are UTF-8, from any encoding", {
  e <- intToUtf8(233)
  latin1 <- iconv(paste0("caf", e, " 22"), "UTF-8", "latin1")
  big <- rep(c(paste0(e, "b2"), latin1, "e", NA), 2000) # the parallel path
  expect_identical(
    par_replace(big, "2", "X"),
    rep(c(paste0(e, "bX"), paste0("caf", e, " XX"), "e", NA), 2000)
  )
  expect_identical(
    par_extract(big, "[^ 0-9]+"),
    rep(c(paste0(e, "b"), paste0("caf", e), "e", NA), 2000)
  )
  expect_identical(Encoding(par_replace(latin1, "2", "X")), "UTF-8")
})

test_that("factors and one pattern per string work", {
  skip_if_not_installed("stringi")
  f <- factor(sample(c("a1", "b22", NA), 3e4, TRUE))
  expect_identical(
    par_extract(f, "[0-9]+"),
    par_extract(as.character(f), "[0-9]+")
  )
  expect_identical(par_replace(f, "[0-9]", "#"), gsub("[0-9]", "#", f))
  p <- rep(c("[0-9]", "[a-c]", NA), 2000)
  expect_identical(
    par_replace(x, p, "_"),
    stringi::stri_replace_all_regex(x, p, "_")
  )
  expect_identical(
    par_extract(x, p),
    stringi::stri_extract_first_regex(x, p)
  )
})

test_that("empty inputs give empty results", {
  expect_identical(par_detect(character(), character()), logical())
  expect_identical(par_extract(character(), character()), character())
  expect_identical(par_replace(character(), "a", "b"), character())
})

test_that("empty matches never split a character", {
  e <- intToUtf8(233)
  expect_identical(par_replace(e, "", "-"), gsub("", "-", e))
  expect_identical(
    par_replace(paste0(e, "x", e), "x*", "-"),
    paste0("-", e, "-", e, "-")
  )
  expect_identical(par_replace(e, "", "-", fixed = TRUE), paste0("-", e, "-"))
  expect_identical(par_count(e, ""), 2L)
  expect_identical(par_count(rep(e, 6000), "x*"), rep(2L, 6000)) # parallel
})

test_that("par_replace() keeps names, as gsub() does", {
  x <- c(a = "x1", b = "y")
  expect_identical(par_replace(x, "[0-9]", "#"), gsub("[0-9]", "#", x))
})
