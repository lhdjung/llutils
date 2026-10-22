#' Parse dates and times, in parallel
#'
#' @description `par_as_date()` is [as.Date()] for strings, and
#'   `par_as_posixct()` is [as.POSIXct()] for strings in UTC, with the same
#'   results. Base R parses about a million dates in seconds, on one thread;
#'   these read the strings on several, and give the same results.
#'
#' @details Formats may use `%Y`, `%m`, `%d`, `%H`, `%M`, `%S`, and `%OS`,
#'   with a year, month, and day, as the default `tryFormats` do. They are
#'   read as [strptime()] reads them, quirks included: text after the format
#'   is ignored, `"24:00:00"` is midnight of the next day, and `"2020-1-5"`
#'   is a date.
#'
#'   Everything else goes to base R: other formats, time zones other than
#'   `"UTC"` and `"GMT"` (or `""` in a session whose time zone is UTC),
#'   inputs that aren't strings or factors, fewer than 1e4 strings, and
#'   seconds written in hexadecimal, with more than 15 digits, or as
#'   `nan(...)`. So does an input that no format fits, which base R reports.
#'   Factors are turned into strings first.
#'
#' @param x A string vector, or a factor.
#' @param ... Passed on to base R where it takes over.
#' @inheritParams base::as.Date
#' @inheritParams base::as.POSIXlt
#' @inheritParams par_match
#'
#' @returns As [as.Date()]: a Date vector as long as `x`, `NA` where a string
#'   doesn't parse. `par_as_posixct()` returns a POSIXct vector instead, as
#'   [as.POSIXct()] does.
#'
#' @export
#'
#' @examples
#' par_as_date(c("2024-02-29", "2024-02-30", NA))
#' par_as_date("05.03.2024", format = "%d.%m.%Y")
#' par_as_posixct(c("2024-03-05 14:30:00.25", "2024-03-05 24:00:00"), tz = "UTC")
par_as_date <- function(
  x,
  format,
  tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
  optional = FALSE,
  ...,
  nthreads = getOption("parvenu.threads", 0L)
) {
  nthreads <- check_nthreads(nthreads)
  out <- parse_time(x, format, tryFormats, TRUE, nthreads)

  if (is.null(out)) {
    return(
      if (missing(format)) {
        as.Date(x, tryFormats = tryFormats, optional = optional, ...)
      } else {
        as.Date(x, format, tryFormats = tryFormats, optional = optional, ...)
      }
    )
  }

  names(out) <- if (!is.factor(x)) names(x) # as.character() drops a factor's
  class(out) <- "Date"

  out
}

#' @rdname par_as_date
#' @export
par_as_posixct <- function(
  x,
  tz = "",
  format,
  tryFormats = c(
    "%Y-%m-%d %H:%M:%OS",
    "%Y/%m/%d %H:%M:%OS",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M",
    "%Y-%m-%d",
    "%Y/%m/%d"
  ),
  optional = FALSE,
  ...,
  nthreads = getOption("parvenu.threads", 0L)
) {
  nthreads <- check_nthreads(nthreads)

  utc <- identical(tz, "UTC") ||
    identical(tz, "GMT") ||
    (identical(tz, "") && session_utc())
  out <- if (utc) {
    parse_time(x, format, tryFormats, FALSE, nthreads)
  }

  if (is.null(out)) {
    return(
      if (missing(format)) {
        as.POSIXct(x, tz, tryFormats = tryFormats, optional = optional, ...)
      } else {
        as.POSIXct(x, tz, format = format, optional = optional, ...)
      }
    )
  }

  names(out) <- names(x)
  class(out) <- c("POSIXct", "POSIXt")
  attr(out, "tzone") <- tz

  out
}

# Whether the session's time zone, which `tz = ""` means, is UTC, as on most
# servers and CI machines. `Sys.timezone()` caches its answer, so TZ is read
# first in case it changed.
session_utc <- function() {
  tz <- Sys.getenv("TZ")
  if (!nzchar(tz)) {
    tz <- Sys.timezone()
  }
  isTRUE(tz %in% c("UTC", "GMT", "Etc/UTC", "Etc/GMT"))
}

# Days or seconds, or NULL where base must take over.
parse_time <- function(x, format, tryFormats, date, nthreads) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  try_formats <- missing(format)
  formats <- if (try_formats) tryFormats else format

  if (
    is.character(x) &&
      length(x) >= RUST_MIN &&
      is.character(formats) &&
      # strptime() recycles several formats along `x`.
      (try_formats || length(formats) == 1L) &&
      length(formats) >= 1L &&
      !anyNA(formats)
  ) {
    par_parse_time_rs(as_utf8(x), formats, try_formats, date, nthreads)
  }
}
