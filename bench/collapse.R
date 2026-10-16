# Does collapse already cover the planned Rust functions?
# Worst case for predicates: no hit, so nothing can short-circuit.
library(collapse)
set.seed(1)
n <- 1e7
xi <- sample.int(1e6, n, TRUE)
xd <- runif(n)
keys <- sample.int(2e6, 1e4) + 1e6L # no overlap with xi

b <- function(...) {
  r <- bench::mark(..., check = FALSE, min_iterations = 5, filter_gc = FALSE)
  print(r[, c("expression", "median", "mem_alloc")])
}

cat("\n## Phase 2: predicates, worst case (no hit), n = 1e7\n")
b(base = any(xi == -1L), collapse = anyv(xi, -1L))
b(base = which(xi == -1L)[1], collapse = whichv(xi, -1L)[1])
b(base = sum(xi == 5L), collapse = length(whichv(xi, 5L)))
b(base = all(xi == 5L), collapse = allv(xi, 5L))
b(
  base = any(xi %in% keys),
  collapse = length(xi %iin% keys) > 0,
  vctrs = any(vctrs::vec_in(xi, keys))
)
b(
  base = all(xd >= 0 & xd <= 1),
  dt = all(data.table::between(xd, 0, 1)),
  collapse = {
    r <- frange(xd)
    r[1] >= 0 && r[2] <= 1
  }
)

cat("\n## Phase 3: match, 1e7 lookups against 1e4 keys\n")
b(
  base = match(xi, keys),
  vctrs = vctrs::vec_match(xi, keys),
  collapse = fmatch(xi, keys)
)
b(
  base = xi %in% keys,
  vctrs = vctrs::vec_in(xi, keys),
  collapse = xi %iin% keys # returns indices, not logicals
)

cat("\n## Phase 4: single-pass summaries, n = 1e7\n")
b(base = range(xd), collapse = frange(xd))
b(
  base = length(unique(xi)),
  dt = data.table::uniqueN(xi),
  collapse = fnunique(xi)
)
