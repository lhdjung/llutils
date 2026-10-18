#' Extract or replace regex matches, in parallel
#'
#' @description `par_extract()` gives the first match of `pattern` in each
#'   string, like `regmatches(x, regexpr(pattern, x))` but with `NA` where
#'   nothing matches, or a capture group of it, which takes the place of
#'   lookaround. `par_replace()` replaces every match, like
#'   `gsub(pattern, replacement, x)`. Both use the syntax of Rust's regex
#'   crate, as [par_detect()] does.
#'
#' @details The threads find the matches and build the replaced strings, but
#'   only R's thread may turn them into R strings, so that part runs
#'   serially. Strings that `par_replace()` leaves unchanged are not copied.
#'
#' @param group The capture group to extract: 0 for the whole match, 1 for
#'   the first group, and so on.
#' @param replacement The string to put in place of each match. Unless
#'   `fixed = TRUE`, `$1` stands for the first capture group, `${name}` for a
#'   named one, and `$$` for a `$`. Write `${1}a`, not `$1a`, which means a
#'   group named `1a`. R's `\\1` is not a group here.
#' @inheritParams par_detect
#'
#' @returns A character vector as long as `x`, `NA` where `x` or `pattern`
#'   is `NA`, and for `par_extract()`, also where nothing matches, or the
#'   group took no part in the match. `par_replace()` keeps the names of
#'   `x`, as [gsub()] does.
#'
#' @export
#'
#' @examples
#' x <- c("born 1999 in Oslo", "no year", NA)
#' par_extract(x, "[0-9]{4}")
#' par_extract(x, "born ([0-9]+)", group = 1)
#' par_replace(x, "[0-9]", "#")
#' par_replace("Smith, John", "(\\w+), (\\w+)", "$2 $1")
par_extract <- function(
  x,
  pattern,
  group = 0L,
  fixed = FALSE,
  nthreads = getOption("llutils.threads", 0L)
) {
  if (
    !is.numeric(group) ||
      length(group) != 1L ||
      is.na(group) ||
      group < 0 ||
      group > .Machine$integer.max ||
      group != trunc(group)
  ) {
    stop("`group` must be a single non-negative whole number.")
  }
  group <- as.integer(group)
  rs <- function(x, pattern, fixed, nthreads) {
    par_extract_rs(x, pattern, group, fixed, nthreads)
  }
  run_pattern(rs, x, pattern, fixed, nthreads)
}

#' @rdname par_extract
#' @export
par_replace <- function(
  x,
  pattern,
  replacement,
  fixed = FALSE,
  nthreads = getOption("llutils.threads", 0L)
) {
  if (
    !is.character(replacement) ||
      length(replacement) != 1L ||
      is.na(replacement)
  ) {
    stop("`replacement` must be a string.")
  }
  replacement <- enc2utf8(replacement)
  rs <- function(x, pattern, fixed, nthreads) {
    par_replace_rs(x, pattern, replacement, fixed, nthreads)
  }
  out <- run_pattern(rs, x, pattern, fixed, nthreads)
  names(out) <- names(x) # as gsub() keeps them
  out
}
