#' All matches, not just the first
#'
#' @description `par_match()` and `par_match_rows()` give the first match of
#'   each element or row of `x` in `table`. These give all of them, as pairs
#'   of positions: a many-to-many join, such as [merge()] does on key columns.
#'
#'   - `par_match_all()` for vectors, compared as `par_match()` compares them.
#'   - `par_match_rows_all()` for rows, compared as `par_match_rows()`
#'   compares them.
#'
#'   With `x` and `table` the same, the pairs are all groups of equal
#'   elements, including each element paired with itself.
#'
#' @details Both sides are hashed once, by `par_match(x, table)` and
#'   `par_match(table, table)` (or the row versions), which number the
#'   groups of equal elements by their first position in `table`. The
#'   positions in `table` are then sorted by group, and each element of `x`
#'   is paired with the run of its group, so the work beyond the two lookups
#'   is linear in the number of pairs.
#'
#'   The number of pairs can be much larger than either input: 1e5 equal
#'   elements on each side make 1e10. It is counted before any pair is
#'   built, and more than 2^31 - 1 pairs are an error.
#'
#' @inheritParams par_match_rows
#'
#' @returns A data frame with one row per pair, sorted by `x` and then by
#'   `table`: `x`, a position in `x` (or row of `x`), and `table`, a
#'   position in `table` equal to it. Elements of `x` with no match in
#'   `table` have no rows.
#'
#' @export
#'
#' @examples
#' par_match_all(c("b", "a", "z"), c("a", "b", "a"))
#'
#' # Groups of equal rows, each pair once
#' keys <- data.frame(x = c(1, 2, 1, 1), y = c("a", "b", "a", "a"))
#' pairs <- par_match_rows_all(keys, keys)
#' pairs[pairs$x < pairs$table, ]
par_match_all <- function(
  x,
  table,
  nthreads = getOption("parvenu.threads", 0L)
) {
  match_all(
    par_match(x, table, nthreads = nthreads),
    par_match(table, table, nthreads = nthreads)
  )
}

#' @rdname par_match_all
#' @export
par_match_rows_all <- function(
  x,
  table,
  nthreads = getOption("parvenu.threads", 0L)
) {
  match_all(
    par_match_rows(x, table, nthreads = nthreads),
    par_match_rows(table, table, nthreads = nthreads)
  )
}

# The pairs from `x_group`, the group of each element of `x`, and
# `table_group`, the group of each element of `table`, both numbered by
# first positions in `table`.
match_all <- function(x_group, table_group) {
  # The positions in `table` sorted by group (a stable sort, so ascending
  # within each group), and where each group's run starts.
  by_group <- order(table_group, method = "radix")
  sizes <- tabulate(table_group, length(table_group))
  run_starts <- cumsum(c(1L, sizes))

  matched <- which(!is.na(x_group))
  counts <- sizes[x_group[matched]]

  if (sum(as.double(counts)) > .Machine$integer.max) {
    stop("More than 2^31 - 1 pairs match.")
  }

  list2DF(list(
    x = rep(matched, counts),
    table = by_group[sequence(counts, from = run_starts[x_group[matched]])]
  ))
}
