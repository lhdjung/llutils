#' Extract or replace regex matches, in parallel
#'
#' @description `par_extract()` gives the first match of `pattern` in each
#'   string, like `regmatches(x, regexpr(pattern, x))` but with `NA` where
#'   nothing matches, or a capture group of it, which takes the place of
#'   lookaround. `par_extract_all()` gives every match, like
#'   `regmatches(x, gregexpr(pattern, x))`.
#'
#'   `par_replace()` replaces the first match, like
#'   `sub(pattern, replacement, x)`, and `par_replace_all()` every match, like
#'   `gsub(pattern, replacement, x)`.
#'
#'   All use the syntax of Rust's regex crate, as [par_detect()] does.
#'
#' @details The threads find the matches and build the replaced strings, but
#'   only R's thread may turn them into R strings, so that part runs
#'   serially. Strings that `par_replace()` and `par_replace_all()` leave
#'   unchanged are not copied.
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
#'   group took no part in the match. `par_replace()` and `par_replace_all()`
#'   keep the names of `x`, as [sub()] and [gsub()] do.
#'
#'   `par_extract_all()` returns a list as long as `x` instead: for each
#'   string, a character vector of its matches, `character(0)` if none, or
#'   `NA` where `x` or `pattern` is `NA`. It finds the matches [par_count()]
#'   counts, so the lengths are the counts, and for a group, `NA` where it
#'   took no part in a match.
#'
#' @export
#'
#' @examples
#' x <- c("born 1999 in Oslo", "no year", NA)
#' par_extract(x, "[0-9]{4}")
#' par_extract(x, "born ([0-9]+)", group = 1)
#' par_extract_all(c("#rstats and #rust", "none", NA), "#(\\w+)", group = 1)
#' par_replace(x, "[0-9]", "#")
#' par_replace_all(x, "[0-9]", "#")
#' par_replace_all("Smith, John", "(\\w+), (\\w+)", "$2 $1")
par_extract <- function(
  x,
  pattern,
  group = 0L,
  fixed = FALSE,
  nthreads = getOption("parvenu.threads", 0L)
) {
  run_extract(par_extract_rs, x, pattern, group, fixed, nthreads)
}

#' @rdname par_extract
#' @export
par_extract_all <- function(
  x,
  pattern,
  group = 0L,
  fixed = FALSE,
  nthreads = getOption("parvenu.threads", 0L)
) {
  run_extract(par_extract_all_rs, x, pattern, group, fixed, nthreads)
}

#' @rdname par_extract
#' @export
par_replace <- function(
  x,
  pattern,
  replacement,
  fixed = FALSE,
  nthreads = getOption("parvenu.threads", 0L)
) {
  run_replace(x, pattern, replacement, fixed, FALSE, nthreads)
}

#' @rdname par_extract
#' @export
par_replace_all <- function(
  x,
  pattern,
  replacement,
  fixed = FALSE,
  nthreads = getOption("parvenu.threads", 0L)
) {
  run_replace(x, pattern, replacement, fixed, TRUE, nthreads)
}

# Checks `group` and calls `rs`, par_extract_rs() or par_extract_all_rs().
run_extract <- function(rs, x, pattern, group, fixed, nthreads) {
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
  rs_group <- function(x, pattern, fixed, nthreads) {
    rs(x, pattern, group, fixed, nthreads)
  }
  run_pattern(rs_group, x, pattern, fixed, nthreads)
}

# Checks `replacement` and replaces every match if `all`, else the first.
run_replace <- function(x, pattern, replacement, fixed, all, nthreads) {
  if (
    !is.character(replacement) ||
      length(replacement) != 1L ||
      is.na(replacement)
  ) {
    stop("`replacement` must be a string.")
  }
  replacement <- enc2utf8(replacement)
  rs <- function(x, pattern, fixed, nthreads) {
    par_replace_rs(x, pattern, replacement, fixed, all, nthreads)
  }
  out <- run_pattern(rs, x, pattern, fixed, nthreads)
  names(out) <- names(x) # as sub() and gsub() keep them
  out
}
