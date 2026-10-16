#' Multithreaded `match()` and `%in%`
#'
#' Same results as base [match()] and `%in%`, computed in parallel when `x`
#' or `table` has at least 1e4 elements and both sides are integer, double
#' (in any mix), character, Date, or POSIXct. Factors are matched by their
#' levels. Anything else goes to base [match()] unchanged. Strings are
#' matched by their pointers, which the threads read straight from R's
#' memory, as data.table does.
#'
#' @param x Values to look up.
#' @param table Values to look them up in.
#' @param nomatch The value returned where there is no match, coerced to
#'   integer.
#' @param nthreads Most threads to use. `0` (the default, unless the
#'   `llutils.threads` option is set) means the `RAYON_NUM_THREADS`
#'   environment variable, or else all cores. Inputs too small to keep that
#'   many busy get fewer.
#' @returns Like [match()]: an integer vector of first positions in `table`,
#'   `nomatch` where there is none. `%par_in%` returns a logical vector.
#' @export
#' @examples
#' par_match(c(3, 1, 9), 1:5, nthreads = 2)
#' c("b", "z") %par_in% letters
par_match <- function(
  x,
  table,
  nomatch = NA_integer_,
  nthreads = getOption("llutils.threads", 0L)
) {
  nthreads <- check_nthreads(nthreads)
  nomatch <- as.integer(nomatch)[1]
  if (is.factor(x)) {
    # Base converts all of `x` to strings; matching the levels is enough.
    # The extra `NA` level is for missing values.
    m <- match(c(levels(x), NA), table, nomatch)
    if (length(x) >= RUST_MIN) {
      return(index_codes(x, m, nthreads))
    }
    i <- as.integer(x)
    i[is.na(i)] <- length(m)
    return(m[i])
  }
  # Rust returns 32-bit positions, so longer tables go to base.
  fast <- max(length(x), length(table)) >= RUST_MIN &&
    length(table) <= .Machine$integer.max
  r <- if (fast) par_match_fast(x, table, nomatch, nthreads)
  if (is.null(r)) match(x, table, nomatch) else r
}

#' @rdname par_match
#' @export
`%par_in%` <- function(x, table) par_match(x, table, nomatch = 0L) > 0L

# Below this many elements, calling into Rust costs more than it saves.
RUST_MIN <- 1e4

# NULL where base must take over: other types, or a case Rust declines.
par_match_fast <- function(x, table, nomatch, nthreads) {
  if (identical(class(x), "Date") && identical(class(table), "Date")) {
    return(par_match_dbl(
      as_dbl(x),
      as_dbl(table),
      nomatch,
      nthreads,
      TRUE
    ))
  }
  x <- plain(x)
  table <- plain(table)
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
plain <- function(x) {
  if (!is.object(x) || identical(class(x), c("POSIXct", "POSIXt"))) {
    x
  } else if (is.factor(x)) {
    as.character(x)
  }
}

is_num <- function(x) is.integer(x) || is.double(x)

# Doubles go to Rust as they are, attributes and all, without a copy.
as_dbl <- function(x) if (is.double(x)) x else as.double(x)

check_nthreads <- function(nthreads) {
  if (
    !is.numeric(nthreads) ||
      length(nthreads) != 1 ||
      is.na(nthreads) ||
      nthreads < 0 ||
      nthreads != trunc(nthreads)
  ) {
    stop("`nthreads` must be a single non-negative whole number.")
  }
  as.integer(nthreads)
}
