#' Detect regex matches, in parallel
#'
#' Like `grepl(pattern, x)`, run over several threads, but with the syntax of
#' Rust's regex crate (<https://docs.rs/regex/latest/regex/#syntax>) instead
#' of R's. Most patterns mean the same in both. The differences:
#' - No backreferences (`\\1`) or lookaround (`(?=...)`): Rust's regexes
#'   run in linear time, with no catastrophic backtracking.
#' - Unicode by default: `\\d`, `\\w`, and `\\s` match any Unicode digit,
#'   word, or space character. Write `[0-9]` or `(?-u:\\d)` for ASCII only.
#' - Flags go in the pattern: `(?i)` ignores case.
#' - `NA` gives `NA`, where [grepl()] gives `FALSE`.
#'
#' The threads read the strings straight from R's memory, as data.table
#' does.
#'
#' @param x A character vector, or something [as.character()] converts.
#' @param pattern A single string: the regex.
#' @inheritParams par_match
#' @returns A logical vector as long as `x`.
#' @export
#' @examples
#' detect_regex(c("apple", "banana", NA), "an+a", nthreads = 2)
#' detect_regex("ABC", "(?i)b")
detect_regex <- function(
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
  detect_regex_rs(enc2utf8(x), enc2utf8(pattern), nthreads)
}
