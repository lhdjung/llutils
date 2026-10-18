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
#' @details All terms are compiled into one Aho-Corasick automaton, which finds
#'   every occurrence of every term in a single pass over each string, however
#'   many terms there are. The threads read the strings straight from R's
#'   memory, as data.table does.
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
#' @returns An integer vector as long as `x`: the position in `terms` of the
#'   first term found in each string, or `NA` if none is found or `x` is `NA`.
#'   Use `terms[par_match_term(x, terms)]` to get the terms themselves.
#'
#' @export
#'
#' @examples
#' cities <- c("new york", "york", "paris")
#' par_match_term(c("york and new york", "paris, texas", "rome", NA), cities)
#' par_match_term("NEW YORK", cities, ignore_case = TRUE)
#' par_match_term(c("yorkshire", "york, uk"), cities, whole_word = TRUE)
#' par_match_term("1+1=2", c("+", "("))
par_match_term <- function(
  x,
  terms,
  ignore_case = FALSE,
  whole_word = FALSE,
  nthreads = getOption("llutils.threads", 0L)
) {
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
      c(
        par_match_term(levels(x), terms, ignore_case, whole_word, nthreads),
        NA
      ),
      nthreads
    ))
  }
  if (!is.character(x)) {
    x <- as.character(x)
  }
  par_match_term_rs(
    enc2utf8(x),
    enc2utf8(terms),
    ignore_case,
    whole_word,
    nthreads
  )
}
