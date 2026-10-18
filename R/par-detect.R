#' Detect regex matches, in parallel
#'
#' @description Like `grepl(pattern, x)`, run over several threads, but with
#'   the syntax of Rust's [regex](https://docs.rs/regex/latest/regex/#syntax)
#'   crate instead of R's. Most patterns mean the same in both. The
#'   differences:
#'
#'   - No backreferences (`\\1`) or lookaround (`(?=...)`). In return, time
#'   grows only linearly with the length of the string. Engines that
#'   backtrack, such as PCRE (`grepl(perl = TRUE)`), try every way a pattern
#'   can split the string, which can take exponentially long: `^(a+)+$`
#'   against 25 `a`s and a `!` exceeds PCRE's match limit.
#'   - Unicode by default: `\\d`, `\\w`, and `\\s` match any Unicode digit,
#'   word, or space character. Write `[0-9]` or `(?-u:\\d)` for ASCII only.
#'   - Flags go in the pattern: `(?i)` ignores case.
#'   - `NA` gives `NA`, where [grepl()] gives `FALSE`.
#'
#'   `par_count()` counts the matches instead: how many [gregexpr()] would
#'   find, without building them. The counts differ only for patterns that
#'   can match the empty string, such as `""`, `"a*"`, or `"\\b"`, because
#'   the two engines treat empty matches in opposite ways:
#'
#'   - At the end of a string, `par_count()` counts an empty match and
#'   [gregexpr()] doesn't (unless the string itself is empty): `""` has 4
#'   matches in `"abc"`, where [gregexpr()] finds 3.
#'   - Right after a non-empty match, [gregexpr()] counts an empty match and
#'   `par_count()` doesn't: `"a*"` has 4 matches in `"banana"`, an empty one
#'   before the `"b"` and the three `"a"`s. [gregexpr()] also finds an empty
#'   one after each of the first two `"a"`s, so 6.
#'
#' @details The threads read the strings straight from R's memory, as
#'   data.table does.
#'
#' @param x A character vector, or something [as.character()] converts.
#' @param pattern The regex, or the literal string to find if `fixed = TRUE`:
#'   one string, or one per string of `x`, as in
#'   [stringi::stri_detect_regex()]. Each distinct pattern is compiled once.
#' @param fixed If `TRUE`, `pattern` is found as it is, as in
#'   `grepl(fixed = TRUE)`: no character has a special meaning.
#' @inheritParams par_match
#'
#' @returns A vector as long as `x`, `NA` where `x` or `pattern` is `NA`:
#'   - `par_detect()`: logical, `TRUE` where `pattern` matches and `FALSE`
#'   where it doesn't.
#'   - `par_count()`: integer, the number of non-overlapping matches.
#'
#' @export
#'
#' @examples
#' par_detect(c("apple", "banana", NA), "an+a", nthreads = 2)
#' par_detect("ABC", "(?i)b")
#' par_detect(c("a.c", "abc"), ".", fixed = TRUE)
#' par_detect(c("apple", "banana", "cherry"), c("^a", "an", "z")) # one each
#' par_count(c("banana", "kiwi", NA), "an")
par_detect <- function(
  x,
  pattern,
  fixed = FALSE,
  nthreads = getOption("llutils.threads", 0L)
) {
  run_pattern(par_detect_rs, x, pattern, fixed, nthreads)
}

#' @rdname par_detect
#' @export
par_count <- function(
  x,
  pattern,
  fixed = FALSE,
  nthreads = getOption("llutils.threads", 0L)
) {
  run_pattern(par_count_rs, x, pattern, fixed, nthreads)
}

# Checks the arguments of par_detect() and par_count(), and calls `rs`.
run_pattern <- function(rs, x, pattern, fixed, nthreads) {
  nthreads <- check_nthreads(nthreads)

  if (
    !is.character(pattern) ||
      (length(pattern) != 1L && length(pattern) != length(x))
  ) {
    stop("`pattern` must be a string, or a character vector as long as `x`.")
  }

  if (!is_bool(fixed)) {
    stop("`fixed` must be `TRUE` or `FALSE`.")
  }

  if (is.factor(x) && nlevels(x) < length(x) && length(pattern) == 1L) {
    # Matching the levels is enough. The extra `NA` is for missing values.
    return(index_levels(
      x,
      c(run_pattern(rs, levels(x), pattern, fixed, nthreads), NA),
      nthreads
    ))
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  # Rust reads the bytes; this makes them UTF-8, without copying strings that
  # are ASCII or UTF-8 already.
  rs(enc2utf8(x), enc2utf8(pattern), fixed, nthreads)
}
