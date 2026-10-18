#' Multithreaded `duplicated()`
#'
#' @description Same results as base [duplicated()], but much faster for
#'   large inputs. For a data frame, rows are compared, as in base.
#'
#'   `par_unique()` is base [unique()] on the same machinery, and
#'   `sum(!par_duplicated(x))` counts the distinct elements.
#'
#' @details Computed in Rust when `x` has at least 1e4 elements (or rows)
#'   and is an integer, double, or character vector, a factor, a Date, or a
#'   POSIXct, or a data frame of such columns and logical ones. Anything else
#'   goes to base [duplicated()] unchanged.
#'
#'   Like `par_match()`, this hashes `x` in parallel, but it marks the
#'   duplicates while it builds the hash table, so it never looks the
#'   elements up a second time, as `par_match(x, x) != seq_along(x)` would.
#'
#' @param x A vector or data frame.
#' @param incomparables Values that are never marked as duplicates, as in
#'   base [duplicated()]. `FALSE` means none; anything else goes to base.
#' @param fromLast If `TRUE`, an element is a duplicate if an equal one comes
#'   later, rather than earlier.
#' @inheritParams par_match
#'
#' @returns
#'   - `par_duplicated()`: a logical vector as long as `x` (or with one
#'   element per row of a data frame): `TRUE` where an equal element (or row)
#'   comes earlier, or later if `fromLast = TRUE`.
#'   - `par_unique()`: `x` without those elements (or rows), as [unique()]
#'   returns it.
#'
#' @export
#'
#' @examples
#' x <- c(3, 1, 3, NA, 1, NA)
#' par_duplicated(x)
#' par_duplicated(x, fromLast = TRUE)
#' par_unique(x)
#' sum(!par_duplicated(x)) # number of distinct values
#'
#' df <- data.frame(id = c(1, 2, 1), day = c("mon", "mon", "mon"))
#' par_duplicated(df)
par_duplicated <- function(
  x,
  incomparables = FALSE,
  fromLast = FALSE,
  nthreads = getOption("llutils.threads", 0L)
) {
  nthreads <- check_nthreads(nthreads)

  # Rust marks duplicates by 32-bit positions, so longer inputs go to base.
  out <- if (
    isFALSE(incomparables) &&
      is_bool(fromLast) &&
      NROW(x) <= .Machine$integer.max
  ) {
    if (is.data.frame(x) && length(x) == 1L) {
      # Base compares a single column as a vector.
      return(par_duplicated(x[[1L]], fromLast = fromLast, nthreads = nthreads))
    } else if (is.data.frame(x) && length(x) > 1L && nrow(x) >= RUST_MIN) {
      cols <- lapply(x, dup_column, nthreads)
      if (!any(vapply(cols, is.null, NA))) {
        par_duplicated_rows_rs(unname(cols), fromLast, nthreads)
      }
    } else if (length(x) >= RUST_MIN && is.null(dim(x)) && is_dup_class(x)) {
      if (is.integer(x) || is.double(x) || is.character(x)) {
        par_duplicated_rs(x, fromLast, nthreads)
      }
    }
  }

  if (is.null(out)) {
    duplicated(x, incomparables, fromLast = fromLast)
  } else {
    out
  }
}

#' @rdname par_duplicated
#' @export
par_unique <- function(
  x,
  incomparables = FALSE,
  fromLast = FALSE,
  nthreads = getOption("llutils.threads", 0L)
) {
  if (is.data.frame(x)) {
    # As unique.data.frame() does.
    return(x[
      !par_duplicated(x, incomparables, fromLast, nthreads),
      ,
      drop = FALSE
    ])
  }
  # A subclass of factor may have its own unique() method.
  plain_factor <- identical(class(x), "factor") ||
    identical(class(x), c("ordered", "factor"))
  if (!is.null(dim(x)) || !is_dup_class(x) || (is.factor(x) && !plain_factor)) {
    return(unique(x, incomparables, fromLast = fromLast))
  }
  # unique() keeps the class and time zone of Dates and POSIXct, which `[`
  # keeps too, but drops names, and a factor keeps only its levels and class.
  out <- x[!par_duplicated(x, incomparables, fromLast, nthreads)]
  if (is.factor(x)) {
    attributes(out) <- list(levels = levels(x), class = class(x))
  } else {
    names(out) <- NULL
  }
  out
}

# Classes whose duplicated() method compares the underlying values, as Rust
# does: factors by their codes, Dates and POSIXct by their numbers.
is_dup_class <- function(x) {
  !is.object(x) ||
    is.factor(x) ||
    identical(class(x), "Date") ||
    identical(class(x), c("POSIXct", "POSIXt"))
}

# A data frame column as Rust compares it for duplicated rows, or NULL for base
# to take over.
dup_column <- function(a, nthreads) {
  if (!is_dup_class(a) || !is.null(dim(a))) {
    NULL
  } else if (is.logical(a)) {
    as.integer(a)
  } else if (is.integer(a) || is.double(a)) {
    a
  } else if (is.character(a)) {
    if (par_pointers_suffice(a, a, nthreads)) {
      a
    } else {
      # Equal positions of first occurrences mean equal strings.
      par_match(a, a, nthreads = nthreads)
    }
  }
}
