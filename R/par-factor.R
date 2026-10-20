#' Make a factor, or count values, in parallel
#'
#' @description `par_factor()` is base [base::factor()] with the same results, and
#'   `par_table()` is base [base::table()]. Both spend most of their time finding
#'   the distinct values and the position of each element among them, which
#'   here [par_unique()] and [par_match()] do on several threads; sorting
#'   the levels stays with base R.
#'
#' @details `factor()` of 1e7 strings in 1e5 levels takes about 0.8s in
#'   base R; this takes about 0.5s, most of it sorting the levels. For 1e7
#'   integers in 1e6 levels, 0.85s becomes 0.3s, most of it making the
#'   levels' strings. `table()` of 1e7 integers takes about 1s in base,
#'   here 0.45s; the rest is R's own counting. A double vector is
#'   tabulated by its printed values, as in base, which takes seconds for
#'   1e7 of them either way.
#'
#' @param x A vector, as in [base::factor()].
#' @inheritParams base::factor
#' @inheritParams base::table
#' @inheritParams par_match
#'
#' @returns `par_factor()`: a factor, as [base::factor()] returns it, with the
#'   names of `x`. `par_table()`: a `table`, as [base::table()] returns it.
#'
#' @export
#'
#' @examples
#' par_factor(c("b", "a", "b"))
#' par_factor(c(3, 1, 3), labels = c("low", "high"))
#' par_table(c("b", "a", "b", NA), useNA = "ifany")
#' par_table(c(1, 2, 1), c("x", "x", "y"))
par_factor <- function(
  x = character(),
  levels,
  labels = levels,
  exclude = NA,
  ordered = is.ordered(x),
  nthreads = getOption("parvenu.threads", 0L)
) {
  nthreads <- check_nthreads(nthreads)
  # As base's factor(), with par_unique() and par_match().
  if (is.null(x)) {
    x <- character()
  }
  nx <- names(x)
  matchAsChar <- is.object(x) ||
    !(is.character(x) || is.integer(x) || is.logical(x))
  if (missing(levels)) {
    y <- par_unique(x, nthreads = nthreads)
    ind <- order(y)
    if (matchAsChar) {
      y <- as.character(y)
    }
    levels <- unique(y[ind])
  }
  force(ordered)
  if (matchAsChar) {
    x <- as.character(x)
  }
  levels <- levels[is.na(match(levels, exclude))]
  f <- par_match(x, levels, nthreads = nthreads)
  if (!is.null(nx)) {
    names(f) <- nx
  }
  if (missing(labels)) {
    levels(f) <- as.character(levels)
  } else {
    nlab <- length(labels)
    if (nlab == length(levels)) {
      nlevs <- unique(xlevs <- as.character(labels))
      at <- attributes(f)
      at$levels <- nlevs
      f <- match(xlevs, nlevs)[f]
      attributes(f) <- at
    } else if (nlab == 1L) {
      levels(f) <- paste0(labels, seq_along(levels))
    } else {
      stop(
        gettextf(
          "invalid 'labels'; length %d should be 1 or %d",
          nlab,
          length(levels)
        ),
        domain = NA
      )
    }
  }
  class(f) <- c(if (ordered) "ordered", "factor")
  f
}

#' @rdname par_factor
#' @export
par_table <- function(
  ...,
  exclude = if (useNA == "no") c(NA, NaN),
  useNA = c("no", "ifany", "always"),
  dnn = list.names(...),
  deparse.level = 1,
  nthreads = getOption("parvenu.threads", 0L)
) {
  nthreads <- check_nthreads(nthreads)
  # As base's table(), with par_factor() and par_match().
  list.names <- function(...) {
    l <- as.list(substitute(list(...)))[-1L]
    if (length(l) == 1L && is.list(..1) && !is.null(nm <- names(..1))) {
      return(nm)
    }
    nm <- names(l)
    fixup <- if (is.null(nm)) seq_along(l) else nm == ""
    dep <- vapply(
      l[fixup],
      function(x) {
        switch(
          deparse.level + 1,
          "",
          if (is.symbol(x)) as.character(x) else "",
          deparse(x, nlines = 1)[1L]
        )
      },
      ""
    )
    if (is.null(nm)) {
      dep
    } else {
      nm[fixup] <- dep
      nm
    }
  }
  miss.use <- missing(useNA)
  miss.exc <- missing(exclude)
  useNA <- if (miss.use && !miss.exc && !match(NA, exclude, nomatch = 0L)) {
    "ifany"
  } else {
    match.arg(useNA)
  }
  doNA <- useNA != "no"
  if (!miss.use && !miss.exc && doNA && match(NA, exclude, nomatch = 0L)) {
    warning(
      "'exclude' containing NA and 'useNA' != \"no\"' are a bit contradicting"
    )
  }
  args <- list(...)
  if (length(args) == 1L && is.list(args[[1L]])) {
    args <- args[[1L]]
    if (length(dnn) != length(args)) {
      dnn <- paste(dnn[1L], seq_along(args), sep = ".")
    }
  }
  if (!length(args)) {
    stop("nothing to tabulate")
  }
  bin <- 0L
  lens <- NULL
  dims <- integer()
  pd <- 1L
  dn <- NULL
  for (a in args) {
    if (is.null(lens)) {
      lens <- length(a)
    } else if (length(a) != lens) {
      stop("all arguments must have the same length")
    }
    fact.a <- is.factor(a)
    if (doNA) {
      aNA <- anyNA(a)
    }
    if (!fact.a) {
      a0 <- a
      op <- options(warn = 2)
      on.exit(options(op))
      a <- par_factor(a, exclude = exclude, nthreads = nthreads)
      options(op)
    }
    add.na <- doNA
    if (add.na) {
      ifany <- (useNA == "ifany")
      anNAc <- anyNA(a)
      add.na <- if (!ifany || anNAc) {
        ll <- levels(a)
        if (add.ll <- !anyNA(ll)) {
          ll <- c(ll, NA)
          TRUE
        } else if (!ifany && !anNAc) {
          FALSE
        } else {
          TRUE
        }
      } else {
        FALSE
      }
    }
    if (add.na) {
      a <- par_factor(a, levels = ll, exclude = NULL, nthreads = nthreads)
    } else {
      ll <- levels(a)
    }
    a <- as.integer(a)
    if (fact.a && !miss.exc) {
      ll <- ll[keep <- which(match(ll, exclude, nomatch = 0L) == 0L)]
      a <- par_match(a, keep, nthreads = nthreads)
    } else if (!fact.a && add.na) {
      if (ifany && !aNA && add.ll) {
        ll <- ll[!is.na(ll)]
        is.na(a) <- par_match(a0, c(exclude, NA), 0L, nthreads = nthreads) > 0L
      } else {
        is.na(a) <- par_match(a0, exclude, 0L, nthreads = nthreads) > 0L
      }
    }
    nl <- length(ll)
    dims <- c(dims, nl)
    if (prod(dims) > .Machine$integer.max) {
      stop("attempt to make a table with >= 2^31 elements")
    }
    dn <- c(dn, list(ll))
    bin <- bin + pd * (a - 1L)
    pd <- pd * nl
  }
  names(dn) <- dnn
  bin <- bin[!is.na(bin)]
  if (length(bin)) {
    bin <- bin + 1L
  }
  y <- array(tabulate(bin, pd), dims, dimnames = dn)
  class(y) <- "table"
  y
}
