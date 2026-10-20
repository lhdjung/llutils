#' Rank values, in parallel
#'
#' @description Like [base::rank()], with the same results, but the values are
#'   sorted on several threads. Base R's `rank()` is slow where values tie:
#'   it takes seconds for a million doubles.
#'
#' @details Integer and double vectors and factors of at least 1e4 elements
#'   are ranked in Rust. Anything else goes to base R, as does
#'   `ties.method = "random"`, which draws random numbers.
#'
#' @inheritParams base::rank
#' @inheritParams par_match
#'
#' @returns As [base::rank()]: the ranks, a double vector for `"average"` and an
#'   integer one otherwise, with the names of `x`.
#'
#' @export
#'
#' @examples
#' x <- c(b = 3, a = 1, c = 3, d = NA)
#' par_rank(x)
#' par_rank(x, ties.method = "min", na.last = "keep")
par_rank <- function(
  x,
  na.last = TRUE,
  ties.method = c("average", "first", "last", "random", "max", "min"),
  nthreads = getOption("parvenu.threads", 0L)
) {
  nthreads <- check_nthreads(nthreads)
  ties.method <- match.arg(ties.method)
  plain <- is.factor(x) || (!is.object(x) && is_num(x))
  if (
    !plain ||
      length(x) < RUST_MIN ||
      length(x) > .Machine$integer.max ||
      ties.method == "random" ||
      length(na.last) != 1L ||
      !(is.logical(na.last) || identical(na.last, "keep"))
  ) {
    return(rank(x, na.last = na.last, ties.method = ties.method))
  }
  nm <- names(x)
  if (is.factor(x)) {
    x <- as.integer(x)
  }
  if (is.na(na.last)) {
    keep <- !is.na(x)
    x <- x[keep]
    nm <- nm[keep]
  }
  # 0 keeps NA, 1 ranks it last, 2 first.
  code <- if (is.na(na.last) || identical(na.last, "keep")) {
    0L
  } else if (na.last) {
    1L
  } else {
    2L
  }
  out <- par_rank_rs(x, ties.method, code, nthreads)
  names(out) <- nm
  out
}
