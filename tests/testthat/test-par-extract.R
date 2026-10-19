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

test_that("par_replace_all() agrees with gsub()", {
  for (p in c("[0-9]+", "a", " ", "^", "b*")) {
    expect_identical(
      par_replace_all(x, p, "<$0>"),
      gsub(paste0("(", p, ")"), "<\\1>", x),
      info = p
    )
  }
  expect_identical(
    par_replace_all(x, "([a-c])([0-9])", "$2${1}x"),
    gsub("([a-c])([0-9])", "\\2\\1x", x)
  )
  expect_identical(
    par_replace_all(x, ".", "$1", fixed = TRUE),
    gsub(".", "$1", x, fixed = TRUE)
  )
  expect_identical(
    par_replace_all(c("a$b", NA), "$", "$$", fixed = TRUE),
    c("a$$b", NA)
  )
  expect_identical(par_replace_all("ab", "b", "$$"), "a$")
  expect_error(par_replace_all("a", "a", NA_character_), "replacement")
  expect_error(par_replace_all("a", "a", c("b", "c")), "replacement")
})

test_that("par_replace() agrees with sub()", {
  for (p in c("[0-9]+", "a", " ", "^", "b*", "$")) {
    expect_identical(
      par_replace(x, p, "<$0>"),
      sub(paste0("(", p, ")"), "<\\1>", x),
      info = p
    )
  }
  expect_identical(
    par_replace(x, ".", "$1", fixed = TRUE),
    sub(".", "$1", x, fixed = TRUE)
  )
  e <- intToUtf8(233)
  expect_identical(par_replace(e, "", "-"), sub("", "-", e))
  expect_identical(par_replace(paste0(e, "x"), "x*", "-"), paste0("-", e, "x"))
  y <- c(a = "x1", b = "y2")
  expect_identical(par_replace(y, "[0-9]", "#"), sub("[0-9]", "#", y))
  expect_error(par_replace("a", "a", NA_character_), "replacement")
})

test_that("par_extract_all() gives every match", {
  skip_if_not_installed("stringi")
  for (p in c("[0-9]+", "a.c", "^b", "[a-c]")) {
    expect_identical(
      par_extract_all(x, p),
      lapply(stringi::stri_extract_all_regex(x, p, omit_no_match = TRUE), \(m) {
        if (identical(m, NA_character_)) m else as.character(m)
      }),
      info = p
    )
  }
  for (p in c("", "a*", "\\b", "[0-9]")) {
    expect_identical(
      lengths(par_extract_all(x, p))[!is.na(x)],
      par_count(x, p)[!is.na(x)],
      info = p
    )
  }
  p <- "([a-c])( )?([0-9])"
  m <- stringi::stri_match_all_regex(x, p, omit_no_match = TRUE)
  for (g in 0:3) {
    want <- lapply(m, \(m) {
      if (nrow(m) && is.na(m[1, 1])) NA_character_ else m[, g + 1]
    })
    expect_identical(par_extract_all(x, p, group = g), want, info = paste(g))
  }
  expect_identical(
    par_extract_all(c("#a and #b", "none", NA), "#(\\w+)", group = 1),
    list(c("a", "b"), character(), NA_character_)
  )
  expect_identical(par_extract_all("a.c", ".", fixed = TRUE), list("."))
  expect_identical(par_extract_all(intToUtf8(233), ""), list(c("", "")))
  expect_error(par_extract_all("a", "(a)", group = 2), "no group 2")
  expect_identical(par_extract_all(character(), "a"), list())
})

test_that("results are UTF-8, from any encoding", {
  e <- intToUtf8(233)
  latin1 <- iconv(paste0("caf", e, " 22"), "UTF-8", "latin1")
  big <- rep(c(paste0(e, "b2"), latin1, "e", NA), 2000) # the parallel path
  expect_identical(
    par_replace_all(big, "2", "X"),
    rep(c(paste0(e, "bX"), paste0("caf", e, " XX"), "e", NA), 2000)
  )
  expect_identical(
    par_extract(big, "[^ 0-9]+"),
    rep(c(paste0(e, "b"), paste0("caf", e), "e", NA), 2000)
  )
  expect_identical(Encoding(par_replace_all(latin1, "2", "X")), "UTF-8")
})

test_that("factors and one pattern per string work", {
  skip_if_not_installed("stringi")
  f <- factor(sample(c("a1", "b22", NA), 3e4, TRUE))
  expect_identical(
    par_extract(f, "[0-9]+"),
    par_extract(as.character(f), "[0-9]+")
  )
  expect_identical(par_replace_all(f, "[0-9]", "#"), gsub("[0-9]", "#", f))
  expect_identical(
    par_extract_all(f, "[0-9]"),
    par_extract_all(as.character(f), "[0-9]")
  )
  p <- rep(c("[0-9]", "[a-c]", NA), 2000)
  expect_identical(
    par_replace_all(x, p, "_"),
    stringi::stri_replace_all_regex(x, p, "_")
  )
  expect_identical(
    par_extract(x, p),
    stringi::stri_extract_first_regex(x, p)
  )
  expect_identical(
    par_replace(x, p, "_"),
    stringi::stri_replace_first_regex(x, p, "_")
  )
  expect_identical(
    par_extract_all(x, p),
    lapply(seq_along(x), \(i) {
      if (is.na(p[i])) NA_character_ else par_extract_all(x[i], p[i])[[1]]
    })
  )
})

test_that("empty inputs give empty results", {
  expect_identical(par_detect(character(), character()), logical())
  expect_identical(par_extract(character(), character()), character())
  expect_identical(par_replace_all(character(), "a", "b"), character())
})

test_that("empty matches never split a character", {
  e <- intToUtf8(233)
  expect_identical(par_replace_all(e, "", "-"), gsub("", "-", e))
  expect_identical(
    par_replace_all(paste0(e, "x", e), "x*", "-"),
    paste0("-", e, "-", e, "-")
  )
  expect_identical(
    par_replace_all(e, "", "-", fixed = TRUE),
    paste0("-", e, "-")
  )
  expect_identical(par_count(e, ""), 2L)
  expect_identical(par_count(rep(e, 6000), "x*"), rep(2L, 6000)) # parallel
})

test_that("all keep names, as regmatches(), sub(), and gsub() do", {
  x <- c(a = "x1", b = "y")
  expect_identical(par_replace_all(x, "[0-9]", "#"), gsub("[0-9]", "#", x))
  expect_identical(par_extract(x, "[0-9]"), c(a = "1", b = NA))
  expect_identical(
    par_extract_all(x, "[0-9]"),
    regmatches(x, gregexpr("[0-9]", x))
  )
})

test_that("par_locate() agrees with regexpr()", {
  locate <- function(x, pattern) {
    r <- regexpr(pattern, x, perl = TRUE)
    start <- as.integer(r)
    start[start == -1L] <- NA
    cbind(start = start, end = start + attr(r, "match.length") - 1L)
  }
  x <- c("born 1999 in Oslo", "no year", NA, "\u00e91234", "", "ab")
  expect_identical(par_locate(x, "[0-9]{4}"), locate(x, "[0-9]{4}"))
  expect_identical(par_locate(x, "x*"), locate(x, "x*"))
  x <- rep(x, 3000)
  expect_identical(par_locate(x, "[0-9]+|\u00e9"), locate(x, "[0-9]+|\u00e9"))
  out <- par_locate(c(a = "born 1999", b = "x"), "born ([0-9]+)", group = 1)
  expect_identical(
    out,
    matrix(
      c(6L, NA, 9L, NA),
      2,
      dimnames = list(c("a", "b"), c("start", "end"))
    )
  )
  expect_identical(
    par_locate(factor(c("x1", "y", "x1")), "[0-9]")[, "start"],
    c(2L, NA, 2L)
  )
})

test_that("latin1 strings, converted on the threads, give UTF-8's results", {
  x <- c("caf\xe9", "\x80uro", "a\x81b\x9dc", "na\xefve", "plain", NA)
  Encoding(x) <- "latin1"
  for (x in list(x, rep(x, 2000))) {
    u <- enc2utf8(x)
    expect_identical(
      par_detect(x, "\u00e9|\u20ac|<81>"),
      par_detect(u, "\u00e9|\u20ac|<81>")
    )
    expect_identical(par_count(x, "."), par_count(u, "."))
    expect_identical(par_extract(x, ".{2}$"), par_extract(u, ".{2}$"))
    expect_identical(par_extract_all(x, "."), par_extract_all(u, "."))
    expect_identical(par_replace_all(x, "a", "A"), par_replace_all(u, "a", "A"))
    expect_identical(Encoding(par_replace_all(x, "zzz", "A")), Encoding(u))
    expect_identical(par_locate(x, "\u00e9|<9d>"), par_locate(u, "\u00e9|<9d>"))
    expect_identical(
      par_match_term(x, c("\u00e9", "\u20ac")),
      par_match_term(u, c("\u00e9", "\u20ac"))
    )
  }
})
