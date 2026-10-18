#' Multithreaded `findInterval()`
#'
#' @description Same results as base [findInterval()], but much faster for
#'   large, unsorted `x`: each element is found by a binary search in `vec`,
#'   on several threads. Use it to bin values by sorted breaks, or to find
#'   which of a set of sorted ranges each value falls into.
#'
#' @details Integer and double `x` and `vec` (including Dates and POSIXct)
#'   are handled in Rust when `x` has at least 1e4 elements. Anything else,
#'   including a `vec` that is unsorted or contains `NA`, goes to base
#'   [findInterval()], which also gives its errors. Base is already fast for
#'   sorted `x`, where it doesn't need to search from scratch.
#'
#' @inheritParams base::findInterval
#' @inheritParams par_match
#'
#' @returns An integer vector as long as `x`, as in [findInterval()]: for
#'   each element, the number of `vec` at most that element, adjusted by
#'   `rightmost.closed`, `all.inside`, and `left.open`. `NA` where `x` is
#'   `NA`.
#'
#' @export
#'
#' @examples
#' par_find_interval(c(1.5, 3.2, 0, 4), vec = 1:4, nthreads = 2)
#' par_find_interval(c(1.5, 3.2, 0, 4), vec = 1:4, rightmost.closed = TRUE)
par_find_interval <- function(
  x,
  vec,
  rightmost.closed = FALSE,
  all.inside = FALSE,
  left.open = FALSE,
  checkSorted = TRUE,
  checkNA = TRUE,
  nthreads = getOption("llutils.threads", 0L)
) {
  nthreads <- check_nthreads(nthreads)

  if (
    length(x) < RUST_MIN ||
      !is_num(x) ||
      !is_num(vec) ||
      length(vec) == 0L ||
      length(vec) > .Machine$integer.max ||
      !is_bool(rightmost.closed) ||
      !is_bool(all.inside) ||
      !is_bool(left.open) ||
      anyNA(vec) ||
      is.unsorted(vec)
  ) {
    findInterval(
      x,
      vec,
      rightmost.closed,
      all.inside,
      left.open,
      checkSorted,
      checkNA
    )
  } else {
    par_find_interval_rs(
      as_dbl(x),
      as_dbl(vec),
      rightmost.closed,
      all.inside,
      left.open,
      nthreads
    )
  }
}
