#' Multithreaded `match()` on rows
#'
#' @description For each row of `x`, the position of the first row of
#'   `table` that is equal to it in every column, as in a join on several
#'   key columns. Columns are paired by position, not by name, and each pair
#'   is compared as `par_match()` compares vectors: `1L` matches `1`, a
#'   factor matches by its levels, and `NA` matches `NA`.
#'
#' @details Whole rows are hashed in parallel, so this costs about as much
#'   as one `par_match()` call, however many columns there are. Integer,
#'   double, character, Date, and POSIXct columns, and factors, are compared
#'   in Rust as they are; for other pairs of columns, `par_match()` first
#'   turns each into the positions of first occurrences in `table`.
#'
#' @param x Rows to look up: a data frame, or a list of vectors as long as
#'   each other.
#' @param table Rows to look them up in, in the same form, with as many
#'   columns as `x`.
#' @inheritParams par_match
#'
#' @returns An integer vector with one element per row of `x`: the first
#'   row of `table` equal to it, or `nomatch` if there is none.
#'   `par_match_rows(x, table, nomatch = 0L) > 0L` tells which rows of `x`
#'   are in `table`.
#'
#' @export
#'
#' @examples
#' orders <- data.frame(customer = c(1L, 2L, 1L), year = c(2025, 2025, 2026))
#' targets <- data.frame(
#'   customer = c(1L, 1L, 2L),
#'   year = c(2026, 2025, 2026),
#'   goal = c(500, 400, 300)
#' )
#' i <- par_match_rows(orders, targets[c("customer", "year")])
#' targets$goal[i]
par_match_rows <- function(
  x,
  table,
  nomatch = NA_integer_,
  nthreads = getOption("llutils.threads", 0L)
) {
  nthreads <- check_nthreads(nthreads)

  nomatch <- as.integer(nomatch)[1L]
  x <- as_columns(x, "x")
  table <- as_columns(table, "table")

  if (length(x) != length(table)) {
    stop("`x` and `table` must have the same number of columns.")
  }

  day <- logical(length(x))

  for (j in seq_along(x)) {
    a <- x[[j]]
    b <- table[[j]]

    if (identical(class(a), "Date") && identical(class(b), "Date")) {
      # Base matches Dates by their printed day; Rust floors them.
      day[j] <- TRUE
      a <- as_dbl(a)
      b <- as_dbl(b)
    } else {
      a <- as_plain(a)
      b <- as_plain(b)
      if (is_num(a) && is_num(b) && is.integer(a) != is.integer(b)) {
        a <- as.double(a)
        b <- as.double(b)
      } else if (
        !(is_num(a) && is_num(b)) &&
          !(is.character(a) &&
            is.character(b) &&
            par_pointers_suffice(a, b, nthreads))
      ) {
        # Positions of first occurrences in `table` stand in for the values.
        a <- par_match(x[[j]], table[[j]], nthreads = nthreads)
        b <- par_match(table[[j]], table[[j]], nthreads = nthreads)
      }
    }

    x[[j]] <- a
    table[[j]] <- b
  }

  par_match_rows_rs(x, table, day, nomatch, nthreads)
}
