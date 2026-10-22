#' Multithreaded `duplicated()`
#'
#' @description Same results as base [duplicated()], but much faster for
#'   large inputs. For a data frame, rows are compared, as in base.
#'
#'   `par_unique()` is base [unique()] on the same machinery, and
#'   `length(par_unique(x))` counts the distinct elements.
#'   `par_any_duplicated()` is base [anyDuplicated()]: the position of the
#'   first duplicate, or `0` if there is none. Base stops at the first, so
#'   it is quick where one comes early; this checks the first 65536
#'   elements (the last, with `fromLast`) that way before marking all.
#'
#' @details Computed in Rust when `x` has at least 1e4 elements (or rows)
#'   and is an integer, double, or string vector, a factor, a Date, or a
#'   POSIXct, or a data frame of such columns and logical ones. Anything else
#'   goes to base [duplicated()] unchanged.
#'
#'   Like `par_match()`, this hashes `x` in parallel, but it marks the
#'   duplicates while it builds the hash table, so it never looks the
#'   elements up a second time, as `par_match(x, x) != seq_along(x)` would.
#'   `par_unique()` collects the first occurrences instead of marking the
#'   duplicates, so it never subsets `x` by the marks either.
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
#'   - `par_any_duplicated()`: an integer, the position of the first `TRUE`
#'   of `par_duplicated()`, the last if `fromLast = TRUE`, or `0L`.
#'
#' @export
#'
#' @examples
#' x <- c(3, 1, 3, NA, 1, NA)
#' par_duplicated(x)
#' par_duplicated(x, fromLast = TRUE)
#' par_unique(x)
#' length(par_unique(x)) # number of distinct values
#' par_any_duplicated(x)
#'
#' df <- data.frame(id = c(1, 2, 1), day = c("mon", "mon", "mon"))
#' par_duplicated(df)
par_duplicated <- function(
  x,
  incomparables = FALSE,
  fromLast = FALSE,
  nthreads = getOption("parvenu.threads", 0L)
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
      if (is.integer(x) || is.double(x) || is.character(x) || plain_factor(x)) {
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
  nthreads = getOption("parvenu.threads", 0L)
) {
  if (is.data.frame(x)) {
    keep <- !par_duplicated(x, incomparables, fromLast, nthreads)
    if (!identical(class(x), "data.frame")) {
      # As unique.data.frame() does. Subclasses may have their own method.
      return(x[keep, , drop = FALSE])
    }
    # `[.data.frame` gives the same, but spends most of its time checking
    # the new row names for duplicates, which kept rows can't have.
    keep <- which(keep)
    out <- lapply(x, function(col) {
      if (is.null(dim(col))) col[keep] else col[keep, , drop = FALSE]
    })
    attributes(out) <- attributes(x)
    attr(out, "row.names") <- if (.row_names_info(x) < 0L) {
      keep
    } else {
      attr(x, "row.names")[keep]
    }
    return(out)
  }
  nthreads <- check_nthreads(nthreads)
  out <- if (
    isFALSE(incomparables) &&
      is_bool(fromLast) &&
      length(x) >= RUST_MIN &&
      length(x) <= .Machine$integer.max &&
      is.null(dim(x)) &&
      is_dup_class(x) &&
      (is.integer(x) || is.double(x) || is.character(x) || plain_factor(x))
  ) {
    par_unique_rs(x, fromLast, nthreads)
  }
  if (is.null(out)) {
    return(unique(x, incomparables, fromLast = fromLast))
  }
  # unique() keeps the class and time zone of Dates and POSIXct, and a factor
  # keeps its levels and class; everything else is dropped.
  if (is.factor(x)) {
    attributes(out) <- list(levels = levels(x), class = class(x))
  } else if (is.object(x)) {
    class(out) <- class(x)
    attr(out, "tzone") <- attr(x, "tzone")
  }
  out
}

#' @rdname par_duplicated
#' @export
par_any_duplicated <- function(
  x,
  incomparables = FALSE,
  fromLast = FALSE,
  nthreads = getOption("parvenu.threads", 0L)
) {
  # A duplicate within the first elements (or the last, with `fromLast`) is
  # the first overall, so base's early stop is kept where it is quick.
  slice <- 65536L
  n <- length(x)
  if (is.atomic(x) && is.null(dim(x)) && n > 2L * slice) {
    i <- if (isTRUE(fromLast)) seq.int(n - slice + 1L, n) else seq_len(slice)
    w <- anyDuplicated(x[i], incomparables, fromLast = fromLast)
    if (w) {
      return(i[w])
    }
  }
  w <- which(par_duplicated(x, incomparables, fromLast, nthreads))
  # Base scans from the end with `fromLast`, so it finds the last one.
  if (!length(w)) {
    0L
  } else if (fromLast) {
    w[length(w)]
  } else {
    w[1L]
  }
}

# A factor that isn't a subclass, which may have its own duplicated() or
# unique() method.
plain_factor <- function(x) {
  identical(class(x), "factor") || identical(class(x), c("ordered", "factor"))
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
  } else if (is.integer(a) || is.double(a) || is.factor(a)) {
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
