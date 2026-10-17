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
#' @details The threads read the strings straight from R's memory, as
#'   data.table does.
#'
#' @param x A character vector, or something [as.character()] converts.
#' @param pattern A single string: the regex.
#' @inheritParams par_match
#'
#' @returns A logical vector as long as `x`: `TRUE` where `pattern` matches,
#'   `FALSE` where it doesn't, and `NA` where `x` is `NA`.
#'
#' @export
#'
#' @examples
#' par_detect(c("apple", "banana", NA), "an+a", nthreads = 2)
#' par_detect("ABC", "(?i)b")
par_detect <- function(
  x,
  pattern,
  nthreads = getOption("llutils.threads", 0L)
) {
  nthreads <- check_nthreads(nthreads)
  if (!is.character(pattern) || length(pattern) != 1 || is.na(pattern)) {
    stop("`pattern` must be a single string.")
  }
  if (!is.character(x)) {
    x <- as.character(x)
  }
  # Rust reads the bytes; this makes them UTF-8, without copying strings
  # that are ASCII or UTF-8 already.
  par_detect_rs(enc2utf8(x), enc2utf8(pattern), nthreads)
}
