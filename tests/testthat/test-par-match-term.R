withr::local_options(parvenu.threads = 2) # CRAN allows at most 2 cores

# The definition: the first term that grepl(fixed = TRUE) finds.
match_term_base <- function(x, terms) {
  vapply(
    x,
    function(s) {
      if (is.na(s)) {
        return(NA_integer_)
      }
      hit <- which(vapply(terms, grepl, NA, s, fixed = TRUE))
      if (length(hit)) hit[1] else NA_integer_
    },
    1L,
    USE.NAMES = FALSE
  )
}

test_that("par_match_term() gives the first term that grepl(fixed = TRUE) finds", {
  set.seed(1)
  x <- vapply(
    1:6000,
    function(i) paste(sample(c(letters[1:6], " "), 10, TRUE), collapse = ""),
    ""
  )
  x[c(5, 500)] <- NA
  terms <- c("abc", "a", "fed", "bb", "ab", "a", "", "zz")
  for (t in list(terms, terms[-c(2, 6, 7)], rev(terms), "zz", character())) {
    expect_identical(
      par_match_term(x, t),
      match_term_base(x, t),
      info = paste(t, collapse = ",")
    )
  }
  expect_identical(
    par_match_term(x[1:10], terms[-2]),
    match_term_base(x[1:10], terms[-2])
  )
})

test_that("term order sets priority, not position in the string", {
  cities <- c("new york", "york", "paris")
  expect_identical(
    par_match_term(c("york and new york", "paris, texas", "rome", NA), cities),
    c(1L, 3L, NA, NA)
  )
  expect_identical(par_match_term("1+1=(2)", c("(", "+")), 1L) # no escaping
  expect_identical(par_match_term(character(), "a"), integer())
})

test_that("ignore_case folds ASCII only", {
  e <- intToUtf8(233)
  E <- intToUtf8(201)
  expect_identical(
    par_match_term(c("NEW York", "york"), "new york", ignore_case = TRUE),
    c(1L, NA)
  )
  expect_identical(par_match_term(E, e, ignore_case = TRUE), NA_integer_)
  expect_identical(par_match_term("Ab", "aB"), NA_integer_)
})

test_that("strings match by character, in any encoding", {
  e <- intToUtf8(233)
  latin1 <- iconv(e, "UTF-8", "latin1")
  expect_identical(
    par_match_term(c(e, latin1, "e"), c("x", latin1)),
    c(2L, 2L, NA)
  )
  expect_identical(
    par_match_term(factor(c("ab", "cd")), c("d", "b")),
    c(2L, 1L)
  )
  expect_identical(par_match_term(c(10, 25), "2"), c(NA, 1L))
})

test_that("bad arguments error", {
  expect_error(par_match_term("a", NA_character_), "without `NA`")
  expect_error(par_match_term("a", 1), "character vector")
  expect_error(par_match_term("a", "a", ignore_case = NA), "ignore_case")
  expect_error(par_match_term("a", "a", nthreads = -1), "nthreads")
})

test_that("factors are matched by their levels", {
  f <- factor(rep(c("ab", NA, "cd", "ab"), 5000), levels = c("cd", "ab", "zz"))
  terms <- c("d", "b")
  expect_identical(
    par_match_term(f, terms),
    par_match_term(as.character(f), terms)
  )
  expect_identical(par_match_term(f[1:3], terms), c(2L, NA, 1L))
})

test_that("whole_word = TRUE agrees with PCRE lookarounds on ASCII", {
  # The definition: no word character right before or after the term.
  whole_word_base <- function(x, terms) {
    patterns <- paste0("(?<!\\w)\\Q", terms, "\\E(?!\\w)")
    vapply(
      x,
      function(s) {
        if (is.na(s)) {
          return(NA_integer_)
        }
        hit <- which(vapply(patterns, grepl, NA, s, perl = TRUE))
        if (length(hit)) hit[1] else NA_integer_
      },
      1L,
      USE.NAMES = FALSE
    )
  }
  set.seed(2)
  x <- vapply(
    1:6000,
    function(i) {
      paste(
        sample(c("a", "b", "_", "1", " ", "-", "."), 8, TRUE),
        collapse = ""
      )
    },
    ""
  )
  x[7] <- NA
  terms <- c("ab", "a", "b-", "-a", "1", "a.b", "_", "", "zz")
  for (t in list(terms, rev(terms), terms[-8])) {
    expect_identical(
      par_match_term(x, t, whole_word = TRUE),
      whole_word_base(x, t),
      info = paste(t, collapse = ",")
    )
  }
  expect_identical(
    par_match_term(x[1:20], terms[-8], whole_word = TRUE),
    whole_word_base(x[1:20], terms[-8])
  )
})

test_that("whole_word = TRUE skips hits inside longer words, in any script", {
  cities <- c("new york", "york")
  x <- c("yorkshire", "york, uk", "in new york.", "newyork", "NEW YORK")
  expect_identical(
    par_match_term(x, cities, whole_word = TRUE),
    c(NA, 2L, 1L, NA, NA)
  )
  expect_identical(
    par_match_term(x, cities, ignore_case = TRUE, whole_word = TRUE),
    c(NA, 2L, 1L, NA, 1L)
  )
  e <- intToUtf8(233)
  expect_identical(
    par_match_term(
      paste0(c("caf", "a caf ", "αcaf"), c(e, "", "")),
      "caf",
      whole_word = TRUE
    ),
    c(NA, 1L, NA)
  )
  expect_identical(
    par_match_term(c("c++ code", "abc++"), "c++", whole_word = TRUE),
    c(1L, NA)
  )
  # The 4 bytes before "caf" start inside the e-acute: the CJK letter still counts
  expect_identical(
    par_match_term(paste0(e, "\u4e2dcaf"), "caf", whole_word = TRUE),
    NA_integer_
  )
  # A later hit counts when an earlier one is inside a word
  expect_identical(par_match_term("artist art", "art", whole_word = TRUE), 1L)
  f <- factor(rep(c("yorkshire", "york"), 3))
  expect_identical(
    par_match_term(f, "york", whole_word = TRUE),
    rep(c(NA, 1L), 3)
  )
  expect_error(par_match_term("a", "a", whole_word = NA), "whole_word")
})

test_that("par_count_term() counts every hit of every term", {
  # The definition: for each term, the positions where it starts, summed.
  count_term_base <- function(x, terms, whole_word = FALSE) {
    out <- integer(length(x))
    for (t in terms) {
      p <- paste0("\\Q", t, "\\E")
      if (whole_word) {
        p <- paste0("(?<!\\w)", p, "(?!\\w)")
      }
      m <- gregexpr(paste0("(?=", p, ")"), x, perl = TRUE)
      out <- out + vapply(m, function(m) sum(m > 0), 1L)
    }
    out[is.na(x)] <- NA
    out
  }
  set.seed(5)
  x <- vapply(
    1:6000,
    function(i) paste(sample(c("a", "b", " ", "-"), 10, TRUE), collapse = ""),
    ""
  )
  x[c(2, 4000)] <- NA
  terms <- c("ab", "a", "b-", "aa", "ab", "zz")
  for (ww in c(FALSE, TRUE)) {
    expect_identical(
      par_count_term(x, terms, whole_word = ww),
      count_term_base(x, terms, ww),
      info = ww
    )
  }
  expect_identical(
    par_count_term(
      c("new york", "NEW YORK", NA),
      c("new york", "york"),
      ignore_case = TRUE
    ),
    c(2L, 2L, NA)
  )
  expect_identical(
    par_count_term(
      "good food, not bad",
      c("good", "bad", "foo"),
      whole_word = TRUE
    ),
    2L
  )
  f <- factor(rep(c("ab ab", NA, "b"), 4000))
  expect_identical(
    par_count_term(f, "ab"),
    par_count_term(as.character(f), "ab")
  )
  expect_identical(par_count_term(character(), "a"), integer())
  expect_error(par_count_term("a", NA_character_), "without `NA`")
})

test_that("the kept automaton is rebuilt when terms or ignore_case change", {
  expect_identical(par_match_term("A", "a"), NA_integer_)
  expect_identical(par_match_term("A", "a", ignore_case = TRUE), 1L)
  expect_identical(par_match_term("A", "a"), NA_integer_)
  # Each term is freed after its iteration unless the kept copy holds it, so
  # a new term could otherwise reuse an old one's address.
  for (i in 1:50) {
    t <- paste0("w", i)
    expect_identical(par_match_term(c(t, "w0"), t), c(1L, NA))
    gc(full = FALSE)
  }
  terms <- c("a", "b")
  expect_identical(par_match_term("b", terms), 2L)
  terms[2] <- "c"
  expect_identical(par_match_term("b", terms), NA_integer_)
})

test_that("an empty term matches only between characters", {
  e <- intToUtf8(233)
  expect_identical(
    par_count_term(c("ab", e, paste0(e, "\u4e2d"), ""), ""),
    c(3L, 2L, 3L, 1L)
  )
  expect_identical(par_match_term(e, "", whole_word = TRUE), NA_integer_)
})

test_that("par_which_term() gives every term grepl(fixed = TRUE) finds", {
  set.seed(2)
  x <- vapply(
    1:6000,
    function(i) paste(sample(c(letters[1:6], " "), 10, TRUE), collapse = ""),
    ""
  )
  x[c(5, 500)] <- NA
  terms <- c("abc", "a", "fed", "bb", "ab", "a", "", "zz")
  expected <- lapply(x, function(s) {
    if (is.na(s)) {
      return(NA_integer_)
    }
    which(vapply(terms, grepl, NA, s, fixed = TRUE, USE.NAMES = FALSE))
  })
  expect_identical(par_which_term(x, terms), expected)
  expect_identical(par_which_term(x[1:10], terms), expected[1:10])
  expect_identical(
    par_which_term(c("york and new york", "rome", NA), c("new york", "york")),
    list(1:2, integer(), NA_integer_)
  )
  expect_identical(
    par_which_term(
      c("yorkshire", "York"),
      "york",
      ignore_case = TRUE,
      whole_word = TRUE
    ),
    list(integer(), 1L)
  )
  expect_identical(par_which_term(character(), "a"), list())
})

test_that("par_which_term() matches factors by their levels", {
  f <- factor(rep(c("ab", NA, "cd", "ab"), 5000), levels = c("cd", "ab", "zz"))
  expect_identical(
    par_which_term(f, c("d", "b", "a")),
    par_which_term(as.character(f), c("d", "b", "a"))
  )
})
