#' Sum by group, in parallel
#'
#' @description Like [rowsum()], with the same results: the sum of `x` within
#'   each group, or of each column of a matrix. Base R takes about a second
#'   for 10 million values; this sums the groups on several threads, each
#'   group's values in their order, as base R does, so the sums agree to the
#'   last bit.
#'
#' @details Integer and double vectors and matrices with at least 1e4 rows
#'   are summed in Rust. Anything else goes to base R, such as data frames.
#'   Integer sums that overflow give `NA`, as in base R.
#'
#' @inheritParams base::rowsum
#' @inheritParams par_match
#'
#' @returns As [rowsum()]: a matrix with a row per group, named by the
#'   groups, sorted unless `reorder = FALSE`, and a column per column of `x`.
#'
#' @export
#'
#' @examples
#' par_rowsum(c(1, 2, 3, 4), c("b", "a", "b", "a"))
#' par_rowsum(matrix(1:6, 3, dimnames = list(NULL, c("p", "q"))), c(2, 1, 2))
par_rowsum <- function(
  x,
  group,
  reorder = TRUE,
  na.rm = FALSE,
  ...,
  nthreads = getOption("parvenu.threads", 0L)
) {
  nthreads <- check_nthreads(nthreads)
  if (
    !is.numeric(x) ||
      is.object(x) ||
      length(dim(x)) > 2L ||
      NROW(x) < RUST_MIN ||
      NROW(x) > .Machine$integer.max ||
      length(group) != NROW(x) ||
      !is_bool(reorder) ||
      !is_bool(na.rm)
  ) {
    return(rowsum(x, group, reorder = reorder, na.rm = na.rm, ...))
  }
  if (anyNA(group)) {
    warning("missing values for 'group'")
  }
  groups <- par_unique(group, nthreads = nthreads)
  if (reorder) {
    groups <- sort(groups, na.last = TRUE, method = "quick")
  }
  codes <- par_match(group, groups, nthreads = nthreads)
  out <- par_rowsum_rs(x, codes, length(groups), na.rm, nthreads)
  dim(out) <- c(length(groups), NCOL(x))
  dimnames(out) <- list(as.character(groups), colnames(x))
  out
}
