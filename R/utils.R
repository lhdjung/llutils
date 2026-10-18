is_bool <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)

# Can't use is.numeric() because that's FALSE for POSIXct
is_num <- function(x) is.integer(x) || is.double(x)

# Doubles go to Rust as they are, attributes and all, without a copy.
as_dbl <- function(x) if (is.double(x)) x else as.double(x)

check_nthreads <- function(nthreads) {
  if (
    !is.numeric(nthreads) ||
      length(nthreads) != 1L ||
      is.na(nthreads) ||
      nthreads < 0 ||
      nthreads > .Machine$integer.max ||
      nthreads != trunc(nthreads)
  ) {
    stop("`nthreads` must be a single non-negative whole number.")
  }
  as.integer(nthreads)
}
