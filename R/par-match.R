#' Multithreaded `match()` and `%in%`
#'
#' @description Same results as base [match()] and `%in%`, but much faster for
#'   large inputs. Use as drop-in replacements:
#'
#'   - `par_match()` for `match()`.
#'   - `%par_in%` for `%in%`.
#'
#'   Also, `par_in()` is `%par_in%` as a regular function. It combines better
#'   with the pipe and takes an `nthreads` argument, so you can fine-tune its
#'   performance.
#'
#' @details Speedups result from a Rust implementation using
#' [rayon](https://docs.rs/rayon/latest/rayon/).
#'
#'   These functions are computed in parallel when `x` or `table` has at least
#'   1e4 elements and both sides are integer, double (in any mix), character,
#'   Date, or POSIXct. Factors are matched by their levels. Anything else goes
#'   to base [match()] unchanged. Strings are matched by their pointers, which
#'   the threads read straight from R's memory, as data.table does.
#'
#'   `incomparables` has some quirks and is rarely used, so it gets no Rust
#'   implementation: if it is anything other than `NULL` or `FALSE`, the call
#'   goes to base [match()].
#'
#' @param x Values to look up.
#' @param table Values to look them up in.
#' @param nomatch The value returned where there is no match, coerced to
#'   integer.
#' @param incomparables Values that cannot be matched, as in base [match()].
#'   `NULL` or `FALSE` means none. Note that using this argument makes the
#'   function fall back to base R, so it gets no speedup.
#' @param nthreads A whole number: the max number of threads to use. `0` means
#'   the `RAYON_NUM_THREADS` environment variable, or else all cores. The
#'   default is the `parvenu.threads` option, or `0` if that isn't set. Inputs
#'   too small to keep that many threads busy get fewer, and more threads than
#'   cores count as all cores.
#'
#' @returns Just like base `match()` and `%in%`:
#'   - `par_match()` returns an integer vector containing the first positions of
#'   the elements of `x` in `table`, or `nomatch` if an element is not in
#'   `table`.
#'   - `%par_in%` and `par_in()` return a logical vector: `TRUE` if an element
#'   of `x` is in `table`, and `FALSE` if not. They never return `NA`: an `NA`
#'   in `x` is `TRUE` if `table` contains `NA`, and `FALSE` if not.
#'
#' @export
#'
#' @examples
#' par_match(c(3, 1, 9), 1:5, nthreads = 2)
#' c("b", "z") %par_in% letters
#' par_in(c("b", "z"), letters, nthreads = 2)
par_match <- function(
  x,
  table,
  nomatch = NA_integer_,
  incomparables = NULL,
  nthreads = getOption("parvenu.threads", 0L)
) {
  nthreads <- check_nthreads(nthreads)
  nomatch <- as.integer(nomatch)[1L]

  if (!is.null(incomparables) && !isFALSE(incomparables)) {
    return(match(x, table, nomatch, incomparables))
  }

  if (is.factor(x) && length(x) >= RUST_MIN) {
    # Base converts all of `x` to strings; matching the levels is enough.
    # The extra `NA` level is for missing values.
    return(index_codes(
      x,
      match(c(levels(x), NA_character_), table, nomatch),
      nthreads
    ))
  }

  # Rust returns 32-bit positions, and uses the largest as "no match", so
  # longer tables go to base.
  out <- if (
    max(length(x), length(table)) >= RUST_MIN &&
      length(table) < .Machine$integer.max
  ) {
    par_match_fast(x, table, nomatch, nthreads)
  }

  if (is.null(out)) match(x, table, nomatch) else out
}

#' @rdname par_match
#' @export
`%par_in%` <- function(x, table) par_match(x, table, nomatch = 0L) > 0L

#' @rdname par_match
#' @export
par_in <- function(x, table, nthreads = getOption("parvenu.threads", 0L)) {
  par_match(x, table, nomatch = 0L, nthreads = nthreads) > 0L
}

# NULL where base must take over: other types, or a case Rust declines.
par_match_fast <- function(x, table, nomatch, nthreads) {
  if (identical(class(x), "Date") && identical(class(table), "Date")) {
    return(par_match_dbl(
      as_dbl(x),
      as_dbl(table),
      nomatch,
      nthreads,
      dates_by_day()
    ))
  }

  x <- as_plain(x)
  table <- as_plain(table)

  if (is.integer(x) && is.integer(table)) {
    par_match_int(x, table, nomatch, nthreads)
  } else if (is.double(x) && is.double(table)) {
    par_match_dbl(x, table, nomatch, nthreads, FALSE)
  } else if (is_num(x) && is_num(table)) {
    par_match_mixed(x, table, nomatch, nthreads)
  } else if (is.character(x) && is.character(table)) {
    par_match_chr(x, table, nomatch, nthreads)
  }
}

# What base's mtfrm() gives for the classes handled here; NULL for others.
# POSIXct is kept as is: Rust ignores its class, and stripping it would copy.
as_plain <- function(x) {
  if (!is.object(x) || identical(class(x), c("POSIXct", "POSIXt"))) {
    x
  } else if (is.factor(x)) {
    as.character(x)
  }
}
