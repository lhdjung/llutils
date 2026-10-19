#' Bin values by sorted breaks, in parallel
#'
#' @description For each value, the number of the interval it falls into:
#'   how many of the sorted breaks in `vec` are at or below it. Use it to put
#'   ages into age groups, timestamps into shifts, or prices into tax
#'   brackets, and to index a vector of labels with the result. It is
#'   `cut(x, vec, right = FALSE, labels = FALSE)`, except that values below
#'   the first break give `0` and values at or above the last give
#'   `length(vec)`, where [cut()] gives `NA`.
#'
#'   It gives the same results as base [findInterval()], so it is also a
#'   drop-in replacement for that, much faster for large, unsorted `x`: each
#'   element is found by a binary search in `vec`, on several threads.
#'
#' @details Integer and double `x` and `vec` (including Dates and POSIXct)
#'   are handled in Rust when `x` has at least 1e4 elements. Anything else,
#'   including a `vec` that is unsorted or contains `NA`, goes to base
#'   [findInterval()], which also gives its errors. Base is already fast for
#'   sorted `x`, where it doesn't need to search from scratch. `checkSorted`
#'   and `checkNA` are passed to base, from R 4.5.0, which added them; Rust
#'   always checks.
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
#' age <- c(4, 17, 18, 45, 67, NA)
#' groups <- c("child", "adult", "senior")
#' groups[par_find_interval(age, vec = c(0, 18, 65))]
#'
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
  nthreads = getOption("parvenu.threads", 0L)
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
    # checkSorted and checkNA arrived in R 4.5.0. A local copy keeps R CMD
    # check on older R from flagging them, and errors still name it.
    findInterval <- base::findInterval
    if (getRversion() < "4.5.0") {
      findInterval(x, vec, rightmost.closed, all.inside, left.open)
    } else {
      findInterval(
        x,
        vec,
        rightmost.closed,
        all.inside,
        left.open,
        checkSorted,
        checkNA
      )
    }
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
