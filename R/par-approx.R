#' Interpolate linearly, in parallel
#'
#' @description Like [approx()], with the same results: interpolates between
#'   the points `x` and `y` at each of `xout`, with a binary search per point
#'   on several threads.
#'
#' @details With at least 1e4 numbers in `xout` and `na.rm = TRUE`, the
#'   points are prepared by base R's [approxfun()], which also checks the
#'   arguments, and Rust interpolates. Anything else goes to [approx()].
#'
#' @inheritParams stats::approx
#' @inheritParams par_match
#'
#' @returns As [approx()]: a list of `x`, the points `xout`, and `y`, the
#'   values there.
#'
#' @export
#'
#' @examples
#' par_approx(c(1, 2, 4), c(10, 20, 0), xout = c(1.5, 3, 5))
#' par_approx(c(1, 2, 4), c(10, 20, 0), xout = c(1.5, 3, 5), rule = 2)
par_approx <- function(
  x,
  y = NULL,
  xout,
  method = "linear",
  n = 50,
  yleft,
  yright,
  rule = 1,
  f = 0,
  ties = mean,
  na.rm = TRUE,
  nthreads = getOption("parvenu.threads", 0L)
) {
  nthreads <- check_nthreads(nthreads)
  # The arguments go on to base as they were given, so missing ones stay
  # missing. `xout` is evaluated once, under a name no argument can mean.
  call <- match.call()
  call$nthreads <- NULL
  env <- new.env(parent = parent.frame())
  if (!missing(xout)) {
    env$.parvenu_xout <- xout
    call$xout <- quote(.parvenu_xout)
  }
  if (
    missing(xout) ||
      !is.numeric(xout) ||
      is.object(xout) ||
      length(xout) < RUST_MIN ||
      !isTRUE(na.rm)
  ) {
    call[[1L]] <- quote(stats::approx)
    return(eval(call, env))
  }
  call[[1L]] <- quote(stats::approxfun)
  call$xout <- NULL
  call$n <- NULL
  # approxfun() sorts the points, merges ties, and keeps what approx() would
  # pass to C in the function's environment.
  fun <- environment(eval(call, env))
  yout <- par_approx_rs(
    fun$x,
    fun$y,
    as_dbl(xout),
    fun$method,
    as.double(c(fun$yleft, fun$yright, fun$f)),
    nthreads
  )
  list(x = xout, y = yout)
}
