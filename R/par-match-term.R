#' Find which term each string contains, in parallel
#'
#' @description For each string in `x`:
#'
#' - `par_match_term()` finds the index of the first term in `terms` it
#'   contains. It is like `match()` for substrings.
#' - `par_count_term()` counts every hit of every term instead, for scoring
#'   strings against a lexicon, such as positive and negative words. With
#'   `weights`, it sums a score per hit instead, as for a lexicon that rates
#'   words from -5 to 5.
#' - `par_which_term()` gives every term each string contains, not just the
#'   first, for tagging: an abstract with every drug it names.
#'
#' @details Terms are literal, as in `grepl(fixed = TRUE)`, so no character
#'   needs escaping by default. However, with `fixed = FALSE`, the terms are
#'   regular expressions.
#'
#'   Order sets priority: if a string contains several terms, the one earliest
#'   in `terms` wins, wherever it occurs in the string. Put specific terms
#'   before general ones, such as `"new york"` before `"york"`.
#'
#'   All terms are compiled into one Aho-Corasick automaton, which finds every
#'   occurrence of every term in a single pass over each string, however many
#'   terms there are. The threads read the strings straight from R's memory, as
#'   data.table does. The last automaton is kept, so calls that repeat the same
#'   `terms` don't build it again.
#'
#'   With `fixed = FALSE`, the terms are compiled into one regex set instead,
#'   which also says in one pass which of them match a string. That is much
#'   slower than the automaton, but much faster than a regex per term.
#'
#' @param x A string vector, or an object convertible by [as.character()].
#' @param terms A string vector of literal strings to find. `NA` is not
#'   allowed.
#' @param ignore_case If `TRUE`, letters match regardless of case, in any
#'   script: `"é"` matches `"É"`. ASCII letters cost nothing extra; strings
#'   with other characters are lowercased first. With `fixed = FALSE`, this
#'   is `(?i)`.
#' @param whole_word If `TRUE`, a term counts only where it isn't part of a
#'   longer word: the characters right before and after it must not be
#'   letters, digits, or `_`, in any script. `"york"` then matches
#'   `"new york"` and `"york, uk"`, but not `"yorkshire"`. An apostrophe
#'   between two such characters joins them into one word, so `"won"`
#'   doesn't match `"won't"`, nor `"heaven"` `"heaven's"`; this holds for
#'   `'` and the curly `\u2019`. With `fixed = FALSE`, the same goes for a
#'   term's match.
#'
#'   If `"start"`, a term must start a word but may end inside one, as a
#'   prefix: `"happ"` matches `"happy"` and `"happiness"`, but not
#'   `"unhappy"`. This is the `"happ*"` of dictionaries such as LIWC.
#' @param fixed If `FALSE`, `terms` are regexes, in the syntax of
#'   [par_detect()].
#' @param weights For `par_count_term()`: `NULL` to count hits, or a numeric
#'   vector as long as `terms`, without `NA`, to sum the weight of each term
#'   at each of its hits.
#' @inheritParams par_match
#'
#' @returns An integer vector as long as `x`, `NA` where `x` is `NA`:
#'   - `par_match_term()`: the position in `terms` of the first term found in
#'   each string, or `NA` if none is found. Use
#'   `terms[par_match_term(x, terms)]` to get the terms themselves.
#'   - `par_count_term()`: the number of hits in each string. Every
#'   occurrence of every term counts, also where they overlap: `"new york"`
#'   has two hits for `c("new york", "york")`, and a term listed twice counts
#'   twice. For regexes, each term's matches are counted as [par_count()]
#'   counts them, which don't overlap. With `weights`, a double vector: the
#'   sum of the weights of the hits, `0` if none.
#'
#'   `par_which_term()` returns a list as long as `x` instead: for each
#'   string, the positions in `terms` of every term found in it, in
#'   increasing order and each once, `integer(0)` if none, or `NA` where `x`
#'   is `NA`.
#'
#' @export
#'
#' @examples
#' cities <- c("new york", "york", "paris")
#'
#' par_match_term(c("york and new york", "paris, texas", "rome", NA), cities)
#'
#' par_match_term("NEW YORK", cities, ignore_case = TRUE)
#'
#' par_match_term(c("yorkshire", "york, uk"), cities, whole_word = TRUE)
#'
#' par_match_term("1+1=2", c("+", "("))
#'
#' par_count_term("good food, not bad", c("good", "bad"), whole_word = TRUE)
#'
#' par_count_term("happy, unhappy, happiness", "happ", whole_word = "start")
#'
#' par_count_term("good food, not bad", c("good", "bad"), weights = c(1, -2))
#'
#' par_which_term(c("york and new york", "rome", NA), cities)
#'
#' par_match_term(c("call 555-1234", "mail a@b.org"), c("\\d{3}-\\d{4}", "@"), fixed = FALSE)
par_match_term <- function(
  x,
  terms,
  ignore_case = FALSE,
  whole_word = FALSE,
  fixed = TRUE,
  nthreads = getOption("parvenu.threads", 0L)
) {
  run_terms(
    par_match_term_rs,
    x,
    terms,
    ignore_case,
    whole_word,
    fixed,
    nthreads
  )
}

#' @rdname par_match_term
#' @export
par_count_term <- function(
  x,
  terms,
  ignore_case = FALSE,
  whole_word = FALSE,
  fixed = TRUE,
  weights = NULL,
  nthreads = getOption("parvenu.threads", 0L)
) {
  if (
    !is.null(weights) &&
      (!is.numeric(weights) ||
        length(weights) != length(terms) ||
        anyNA(weights))
  ) {
    stop(
      "`weights` must be `NULL`, or numbers as long as `terms`, without `NA`."
    )
  }
  weights <- if (!is.null(weights)) as.double(weights)
  rs <- function(...) par_count_term_rs(..., weights)
  run_terms(
    rs,
    x,
    terms,
    ignore_case,
    whole_word,
    fixed,
    nthreads
  )
}

#' @rdname par_match_term
#' @export
par_which_term <- function(
  x,
  terms,
  ignore_case = FALSE,
  whole_word = FALSE,
  fixed = TRUE,
  nthreads = getOption("parvenu.threads", 0L)
) {
  run_terms(
    par_which_term_rs,
    x,
    terms,
    ignore_case,
    whole_word,
    fixed,
    nthreads
  )
}

#' Replace dictionary terms, in parallel
#'
#' @description Replaces every term of `terms` in each string with its
#'   replacement, in one pass however many terms there are, such as to
#'   normalize spellings, expand abbreviations, or redact names. It does
#'   what a `gsub(fixed = TRUE)` per term would, but in one pass, so a
#'   replacement is never searched again for the terms after it.
#'
#' @details Hits don't overlap. The leftmost hit is replaced first, and the
#'   search goes on after it. If several terms start at the same place, the
#'   one earliest in `terms` wins, so put longer terms before their prefixes,
#'   such as `"new york"` before `"new"`.
#'
#' @param terms A string vector of literal strings to find, without `NA`,
#'   or `""` unless `fixed = FALSE`.
#' @param replacement A string vector as long as `terms`, the replacement for
#'   each, or one string for all of them. With `fixed = FALSE`, `$1` stands
#'   for the first capture group of the term's match, as in
#'   [par_replace_all()].
#' @inheritParams par_match_term
#'
#' @returns A string vector as long as `x`, `NA` where `x` is `NA`. It keeps
#'   the names of `x`, as [gsub()] does.
#'
#' @export
#'
#' @examples
#' par_replace_term(
#'   c("NYC and LA", "la la land"),
#'   c("NYC", "LA"),
#'   c("New York City", "Los Angeles"),
#'   whole_word = TRUE
#' )
#'
#' # Redaction
#' par_replace_term("Ann met Bob", c("ann", "bob"), "[name]", ignore_case = TRUE)
#'
#' # Longer terms first
#' par_replace_term("new york, new jersey", c("new york", "new"), c("NY", "N."))
par_replace_term <- function(
  x,
  terms,
  replacement,
  ignore_case = FALSE,
  whole_word = FALSE,
  fixed = TRUE,
  nthreads = getOption("parvenu.threads", 0L)
) {
  if (
    !is.character(replacement) ||
      !(length(replacement) %in% c(1L, length(terms))) ||
      anyNA(replacement)
  ) {
    stop(
      "`replacement` must be a string vector as long as `terms`, or a string."
    )
  }
  if (isTRUE(fixed) && !all(nzchar(terms))) {
    stop("`terms` must not contain `\"\"`.")
  }
  replacement <- enc2utf8(replacement)
  rs <- function(...) par_replace_term_rs(..., replacement)
  out <- run_terms(rs, x, terms, ignore_case, whole_word, fixed, nthreads)
  names(out) <- if (!is.factor(x)) names(x) # as gsub() keeps them
  out
}

# Checks the arguments of par_match_term() and the like, and calls `rs`.
run_terms <- function(rs, x, terms, ignore_case, whole_word, fixed, nthreads) {
  nthreads <- check_nthreads(nthreads)

  if (!is.character(terms) || anyNA(terms)) {
    stop("`terms` must be a character vector without `NA`.")
  }
  if (!is_bool(ignore_case)) {
    stop("`ignore_case` must be `TRUE` or `FALSE`.")
  }

  if (identical(whole_word, "start")) {
    bounds <- c(TRUE, FALSE)
  } else if (is_bool(whole_word)) {
    bounds <- c(whole_word, whole_word)
  } else {
    stop("`whole_word` must be `TRUE`, `FALSE`, or `\"start\"`.")
  }

  if (!is_bool(fixed)) {
    stop("`fixed` must be `TRUE` or `FALSE`.")
  }

  if (is.factor(x) && nlevels(x) < length(x)) {
    # Matching the levels is enough. The extra `NA` is for missing values.
    m <- run_terms(
      rs,
      levels(x),
      terms,
      ignore_case,
      whole_word,
      fixed,
      nthreads
    )
    na <- if (is.list(m)) list(NA_integer_) else NA
    return(index_levels(x, c(m, na), nthreads))
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  rs(
    as_utf8(x),
    enc2utf8(terms),
    ignore_case,
    bounds[1],
    bounds[2],
    fixed,
    nthreads
  )
}
