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

test_that("ignore_case folds case in any script", {
  e <- intToUtf8(233)
  E <- intToUtf8(201)
  expect_identical(
    par_match_term(c("NEW York", "york"), "new york", ignore_case = TRUE),
    c(1L, NA)
  )
  expect_identical(par_match_term(E, e, ignore_case = TRUE), 1L)
  expect_identical(par_match_term(E, e), NA_integer_)
  expect_identical(par_match_term("Ab", "aB"), NA_integer_)
  # Final sigma, the Kelvin sign, and sharp s, which doesn't become "ss"
  x <- c("\u039f\u0394\u039f\u03a3", "\u212a", "Stra\u00dfe")
  terms <- c("\u03bf\u03b4\u03bf\u03c2", "k", "STRASSE")
  expect_identical(par_match_term(x, terms, ignore_case = TRUE), c(1L, 2L, NA))
  # "\u0130" lowercases to "i" and a combining dot, and "i" is only part of it
  expect_identical(
    par_match_term("\u0130stanbul", "i", ignore_case = TRUE),
    NA_integer_
  )
  expect_identical(
    par_replace_term("\u0130stanbul", "i", "X", ignore_case = TRUE),
    "\u0130stanbul"
  )
  # Lowercased strings keep word bounds
  x <- c(paste0(E, "COLES"), paste0("L'", E, "COLE"), paste0("- ", E, "COLE"))
  expect_identical(
    par_match_term(x, paste0(e, "cole"), ignore_case = TRUE, whole_word = TRUE),
    c(NA, NA, 1L)
  )
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

test_that("whole_word agrees with PCRE lookarounds on ASCII", {
  # The definition: no word character right before (or after) the term, nor
  # an apostrophe that joins one to it.
  whole_word_base <- function(x, terms, whole_word) {
    a <- "['\u2019]"
    before <- paste0("(?<!\\w)(?!(?<=\\w", a, ")\\w)")
    after <- if (isTRUE(whole_word)) paste0("(?!\\w)(?!(?<=\\w)", a, "\\w)")
    patterns <- paste0(before, "\\Q", terms, "\\E", after)
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
        sample(c("a", "b", "_", "1", " ", "-", ".", "'", "\u2019"), 8, TRUE),
        collapse = ""
      )
    },
    ""
  )
  x[7] <- NA
  terms <- c("ab", "a", "b-", "-a", "1", "a.b", "_", "", "zz", "a'b", "'a")
  for (ww in list(TRUE, "start")) {
    for (t in list(terms, rev(terms), terms[-8])) {
      expect_identical(
        par_match_term(x, t, whole_word = ww),
        whole_word_base(x, t, ww),
        info = paste(ww, paste(t, collapse = ","))
      )
    }
    expect_identical(
      par_match_term(x[1:20], terms[-8], whole_word = ww),
      whole_word_base(x[1:20], terms[-8], ww)
    )
  }
})

test_that("an apostrophe inside a word joins it, for literals and regexes", {
  x <- c("won't", "won\u2019t", "heaven's", "'won'", "rock 'n' roll", "won")
  for (fixed in c(TRUE, FALSE)) {
    expect_identical(
      par_count_term(
        x,
        c("won", "n", "heaven"),
        whole_word = TRUE,
        fixed = fixed
      ),
      c(0L, 0L, 0L, 1L, 1L, 1L),
      info = fixed
    )
    expect_identical(
      par_match_term(
        x,
        c("won't", "won\u2019t"),
        whole_word = TRUE,
        fixed = fixed
      ),
      c(1L, 2L, NA, NA, NA, NA),
      info = fixed
    )
  }
  # Without whole_word, apostrophes don't matter
  expect_identical(par_count_term("won't", "won"), 1L)
})

test_that('whole_word = "start" matches prefixes', {
  x <- c("happy", "unhappy", "happiness, happ", "Happy-go-lucky", NA)
  for (fixed in c(TRUE, FALSE)) {
    expect_identical(
      par_count_term(x, "happ", whole_word = "start", fixed = fixed),
      c(1L, 0L, 2L, 0L, NA),
      info = fixed
    )
  }
  expect_identical(
    par_count_term(x[4], "happ", ignore_case = TRUE, whole_word = "start"),
    1L
  )
  # The automaton kept for repeated terms isn't reused for another mode
  expect_identical(
    par_match_term("happy", "happ", whole_word = TRUE),
    NA_integer_
  )
  expect_error(par_match_term("a", "a", whole_word = "end"), "whole_word")
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

test_that("fixed = FALSE takes regexes", {
  set.seed(4)
  x <- vapply(
    1:6000,
    function(i) {
      paste(sample(c(letters[1:6], " ", 0:2), 10, TRUE), collapse = "")
    },
    ""
  )
  x[c(5, 500)] <- NA
  terms <- c("[0-9]{2}", "a.b", "^f", "c", "zz", "[0-9]")
  detected <- unname(vapply(terms, \(t) par_detect(x, t), logical(length(x))))
  expect_identical(
    par_match_term(x, terms, fixed = FALSE),
    apply(detected, 1, \(d) if (anyNA(d)) NA_integer_ else which(d)[1])
  )
  expect_identical(
    par_which_term(x, terms, fixed = FALSE),
    apply(
      detected,
      1,
      \(d) if (anyNA(d)) NA_integer_ else which(d),
      simplify = FALSE
    )
  )
  expect_identical(
    par_count_term(x, terms, fixed = FALSE),
    Reduce(`+`, lapply(terms, \(t) par_count(x, t)))
  )
  # Literal terms without metacharacters give the same results as regexes,
  # also for whole words.
  lit <- c("ab", "a", "c d", "1")
  for (ww in list(FALSE, TRUE, "start")) {
    expect_identical(
      par_match_term(x, lit, whole_word = ww, fixed = FALSE),
      par_match_term(x, lit, whole_word = ww),
      info = ww
    )
  }
  expect_identical(
    par_match_term(
      c("yorkshire", "york, uk"),
      "y.rk",
      whole_word = TRUE,
      fixed = FALSE
    ),
    c(NA, 1L)
  )
})

test_that("regex terms ignore case in any script, and bad ones error", {
  e <- intToUtf8(233)
  E <- intToUtf8(201)
  expect_identical(
    par_match_term(c(E, "AB"), c(e, "ab"), ignore_case = TRUE, fixed = FALSE),
    1:2
  )
  expect_identical(par_match_term("1+1", "+"), 1L)
  expect_error(par_match_term("1+1", "+", fixed = FALSE), "repetition")
  expect_error(par_match_term("a", "a", fixed = NA), "fixed")
  # whole_word must not let a term escape its wrapper.
  expect_error(
    par_match_term("x b", "x)|(y", whole_word = TRUE, fixed = FALSE),
    "unopened group"
  )
  expect_identical(
    par_match_term(
      c("a b", "ab"),
      "(?x)a # a comment",
      whole_word = TRUE,
      fixed = FALSE
    ),
    c(1L, NA)
  )
  f <- factor(sample(c("a1", "b2", NA), 3e4, TRUE))
  expect_identical(
    par_which_term(f, c("[0-9]", "b"), fixed = FALSE),
    par_which_term(as.character(f), c("[0-9]", "b"), fixed = FALSE)
  )
})

test_that("par_count_term() sums weights", {
  terms <- c("good", "bad", "great")
  w <- c(1, -2, 1.5)
  x <- c("good good bad", "great", "none", NA, "goodbad")
  expected <- c(0, 1.5, 0, NA, -1)
  expect_identical(par_count_term(x, terms, weights = w), expected)
  expect_identical(
    par_count_term(rep(x, 3000), terms, weights = w),
    rep(expected, 3000)
  )
  expect_identical(par_count_term(factor(x), terms, weights = w), expected)
  expect_identical(
    par_count_term(x, c("go+d", "ba."), weights = c(2L, 3L), fixed = FALSE),
    c(7, 0, 0, NA, 5)
  )
  expect_error(par_count_term(x, terms, weights = 1:2), "weights")
  expect_error(par_count_term(x, terms, weights = c(1, NA, 1)), "weights")
})

test_that("par_replace_term() replaces the leftmost hit, earliest term first", {
  x <- c(a = "NYC and LA", b = "la la land", c = NA, d = "LAX")
  expect_identical(
    par_replace_term(
      x,
      c("NYC", "LA"),
      c("NY", "Los Angeles"),
      whole_word = TRUE
    ),
    c(a = "NY and Los Angeles", b = "la la land", c = NA, d = "LAX")
  )
  x <- "new york, new jersey"
  expect_identical(
    par_replace_term(x, c("new york", "new"), c("NY", "N.")),
    "NY, N. jersey"
  )
  expect_identical(
    par_replace_term(x, c("new", "new york"), c("N.", "NY")),
    "N. york, N. jersey"
  )
  expect_identical(
    par_replace_term("york and new york", c("new york", "york"), c("A", "B")),
    "B and A"
  )
  expect_identical(par_replace_term("aaa", c("a", "aa"), c("1", "2")), "111")
  expect_identical(
    par_replace_term("won't win", "won", "X", whole_word = TRUE),
    "won't win"
  )
  expect_identical(par_replace_term("abc", character(0), character(0)), "abc")
  expect_identical(
    par_replace_term(factor(c("a b", "b", "a b")), c("a", "b"), c("1", "2")),
    c("1 2", "2", "1 2")
  )
  x <- rep(c("the cat sat", "dog", "cat dog cat", "\u00e9 cat"), 5000)
  expect_identical(
    par_replace_term(x, c("cat", "dog"), c("CAT", "DOG")),
    gsub("dog", "DOG", gsub("cat", "CAT", x, fixed = TRUE), fixed = TRUE)
  )
  expect_error(par_replace_term("a", c("a", ""), "x"), "must not contain")
  expect_error(
    par_replace_term("a", c("a", "b"), c("x", "y", "z")),
    "replacement"
  )
})

test_that("par_replace_term() ignores case in any script, and expands groups", {
  e <- intToUtf8(233)
  E <- intToUtf8(201)
  expect_identical(
    par_replace_term(
      paste("Ann met BOB and", E),
      c("ann", "bob", e),
      "[x]",
      ignore_case = TRUE
    ),
    "[x] met [x] and [x]"
  )
  # Lowercasing the Kelvin sign shortens it, and replacements still land.
  expect_identical(
    par_replace_term(
      paste0("A", E, " \u212aB ", E, E),
      c(e, "kb"),
      c("e", "KB"),
      ignore_case = TRUE
    ),
    "Ae KB ee"
  )
  expect_identical(
    par_replace_term(
      "Smith, John; Doe, Jane",
      "(\\w+), (\\w+)",
      "$2 $1",
      fixed = FALSE
    ),
    "John Smith; Jane Doe"
  )
})
