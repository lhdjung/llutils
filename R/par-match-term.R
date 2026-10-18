#' Find which term each string contains, in parallel
#'
#' @description For each string in `x`, the index of the first of `terms` it
#'   contains, like `match()` for substrings. Terms are literal, as in
#'   `grepl(fixed = TRUE)`, so no character needs escaping.
#'
#'   Order sets priority: if a string contains several terms, the one earliest
#'   in `terms` wins, wherever it occurs in the string. Put specific terms
#'   before general ones, such as `"new york"` before `"york"`.
#'
#'   `par_count_term()` counts every hit of every term instead, for scoring
#'   strings against a lexicon, such as positive and negative words.
#'
#' @details All terms are compiled into one Aho-Corasick automaton, which finds
#'   every occurrence of every term in a single pass over each string, however
#'   many terms there are. The threads read the strings straight from R's
#'   memory, as data.table does. The last automaton is kept, so calls that
#'   repeat the same `terms` don't build it again.
#'
#' @param x A character vector, or something [as.character()] converts.
#' @param terms A character vector of literal strings to find, without `NA`.
#' @param ignore_case If `TRUE`, ASCII letters match regardless of case.
#'   Other letters, such as `"é"` and `"É"`, still match only
#'   themselves.
#' @param whole_word If `TRUE`, a term counts only where it isn't part of a
#'   longer word: the characters right before and after it must not be
#'   letters, digits, or `_`, in any script. `"york"` then matches
#'   `"new york"` and `"york, uk"`, but not `"yorkshire"`.
#' @inheritParams par_match
#'
#' @returns An integer vector as long as `x`, `NA` where `x` is `NA`:
#'   - `par_match_term()`: the position in `terms` of the first term found in
#'   each string, or `NA` if none is found. Use
#'   `terms[par_match_term(x, terms)]` to get the terms themselves.
#'   - `par_count_term()`: the number of hits in each string. Every
#'   occurrence of every term counts, also where they overlap: `"new york"`
#'   has two hits for `c("new york", "york")`, and a term listed twice counts
#'   twice.
#'
#' @export
#'
#' @examples
#' cities <- c("new york", "york", "paris")
#' par_match_term(c("york and new york", "paris, texas", "rome", NA), cities)
#' par_match_term("NEW YORK", cities, ignore_case = TRUE)
#' par_match_term(c("yorkshire", "york, uk"), cities, whole_word = TRUE)
#' par_match_term("1+1=2", c("+", "("))
#' par_count_term("good food, not bad", c("good", "bad"), whole_word = TRUE)
par_match_term <- function(
  x,
  terms,
  ignore_case = FALSE,
  whole_word = FALSE,
  nthreads = getOption("llutils.threads", 0L)
) {
  run_terms(par_match_term_rs, x, terms, ignore_case, whole_word, nthreads)
}

#' @rdname par_match_term
#' @export
par_count_term <- function(
  x,
  terms,
  ignore_case = FALSE,
  whole_word = FALSE,
  nthreads = getOption("llutils.threads", 0L)
) {
  run_terms(par_count_term_rs, x, terms, ignore_case, whole_word, nthreads)
}

# Checks the arguments of par_match_term() and par_count_term(), and calls
# `rs`.
run_terms <- function(rs, x, terms, ignore_case, whole_word, nthreads) {
  nthreads <- check_nthreads(nthreads)

  if (!is.character(terms) || anyNA(terms)) {
    stop("`terms` must be a character vector without `NA`.")
  }
  if (!is_bool(ignore_case)) {
    stop("`ignore_case` must be `TRUE` or `FALSE`.")
  }
  if (!is_bool(whole_word)) {
    stop("`whole_word` must be `TRUE` or `FALSE`.")
  }

  if (is.factor(x) && nlevels(x) < length(x)) {
    # Matching the levels is enough. The extra `NA` is for missing values.
    return(index_codes(
      x,
      c(run_terms(rs, levels(x), terms, ignore_case, whole_word, nthreads), NA),
      nthreads
    ))
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  rs(enc2utf8(x), enc2utf8(terms), ignore_case, whole_word, nthreads)
}
