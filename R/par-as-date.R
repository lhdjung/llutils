#' Parse dates and times, in parallel
#'
#' @description `par_as_date()` is [as.Date()] for strings, and
#'   `par_as_posixct()` is [as.POSIXct()] for strings, with the same
#'   results. Base R parses about a million dates in seconds, on one thread;
#'   these read the strings on several, and give the same results.
#'
#' @details Formats may use `%Y`, `%y`, `%m`, `%b` (also `%B` and `%h`:
#'   English month names, full or their first three letters, in any case,
#'   where the `LC_TIME` locale is C or English), `%d`, `%e`, `%j`, `%H`,
#'   `%I` with `%p`, `%M`, `%S`, `%OS`, `%z` (an offset such as `+0530`),
#'   `%F`, `%T`, and `%R`, with a year, and a month and day or a day of the
#'   year, as the default `tryFormats` do. They are read as [strptime()]
#'   reads them, quirks included: text after the format is ignored,
#'   `"24:00:00"` is midnight of the next day, and `"2020-1-5"` is a date.
#'
#'   In UTC (`"UTC"`, `"GMT"`, or `""` in a session whose time zone is UTC)
#'   the parse gives the times. In another time zone, base R's own C code
#'   places each local time, so a time that doesn't exist there, or occurs
#'   twice, comes out as in base. That code runs on only two times per
#'   calendar day for the days that have no change of clock, and on every
#'   time of the days that do.
#'
#'   Everything else goes to base R: other formats, several formats in
#'   `format`, month names in other locales, inputs that aren't strings or
#'   factors, fewer than 1e4 strings, `tryFormats` with `%z` in a time zone
#'   other than UTC, spaces other than ASCII ones, and seconds written in
#'   hexadecimal, with more than 15 digits, or as `nan(...)`. So does an
#'   input that no format fits, which base R reports.
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
#' par_as_posixct("5 Mar 2024 2:30 PM", format = "%d %b %Y %I:%M %p", tz = "UTC")
#' par_as_posixct("2024-03-05T14:30:00+0530", format = "%FT%T%z", tz = "UTC")
#' par_as_posixct("2024-03-05 14:30:00", tz = "Europe/Berlin")
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

  formats <- if (missing(format)) tryFormats else format
  utc <- identical(tz, "UTC") ||
    identical(tz, "GMT") ||
    (identical(tz, "") && session_utc())
  out <- if (utc) {
    parse_time(x, format, tryFormats, FALSE, nthreads)
  } else if (
    is_string(tz) &&
      is.character(formats) &&
      # strptime() recycles several formats along `x`.
      (missing(format) || length(format) == 1L)
  ) {
    if (!any(grepl("%z", formats, fixed = TRUE))) {
      local_time(x, formats, missing(format), tz, nthreads)
    } else if (!missing(format)) {
      # The offset fixes the instant, so the zone only labels it.
      parse_time(x, format, tryFormats, FALSE, nthreads)
    }
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

# Days or seconds, or NULL where base must take over. With `split`, a list
# of the whole seconds and the fractions instead.
parse_time <- function(x, format, tryFormats, date, nthreads, split = FALSE) {
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
      !anyNA(formats) &&
      (english_time() || !any(grepl("%[bBhp]", formats)))
  ) {
    par_parse_time_rs(as_utf8(x), formats, try_formats, date, split, nthreads)
  }
}

# Whether strptime() reads the English month names and AM/PM that Rust does.
# It takes them from the C library, as format() does, and some English
# locales differ: glibc's en_GB abbreviates September as "Sept".
english_time <- function() {
  up <- function(x, f) toupper(format(x, f))
  months <- ISOdate(2000, 1:12, 1)
  identical(up(months, "%b"), toupper(month.abb)) &&
    identical(up(months, "%B"), toupper(month.name)) &&
    identical(
      up(ISOdatetime(2000, 1, 1, c(1, 13), 0, 0, "UTC"), "%p"),
      c("AM", "PM")
    )
}

# Seconds of the times of `x` read with `formats` and placed in `tz`, or
# NULL for base. With `try_formats`, the first format that reads every
# string that isn't NA, as base checks after placing the times: one that
# doesn't exist in `tz` is NA, and fails its format.
local_time <- function(x, formats, try_formats, tz, nthreads) {
  for (f in formats) {
    secs <- parse_time(x, f, NULL, FALSE, nthreads, split = TRUE)
    if (is.null(secs)) {
      return(NULL)
    }
    out <- local_secs(secs$whole, secs$frac, tz, nthreads)
    if (!try_formats || !anyNA(out[!is.na(x)])) {
      return(out)
    }
  }
  NULL
}

# Seconds since 1970 of local times in `tz`, given as the whole seconds they
# would be in UTC and the fractions, added last as base adds them. R's C
# code places a local time, as as.POSIXct() does for a
# POSIXlt, in about 4us, so it places only the ends of the calendar days
# used and of their neighbours: a day whose offset from UTC is the same at
# both its ends, and at the ends of the days next to it, has no change of
# clock in it or at its edges, so its times are placed by subtracting the
# offset. The times of the other days go through the C code one by one,
# which decides the times that don't exist or occur twice as base does.
# ponytail: a day whose clock changes twice, back to the same offset, would
# be placed by that offset; no zone has had one.
local_secs <- function(secs, frac, tz, nthreads) {
  place <- function(s) {
    if (!length(s)) {
      return(s)
    }
    lt <- as.POSIXlt(.POSIXct(s, "UTC"))
    attr(lt, "tzone") <- tz
    lt$isdst <- -1L
    lt$gmtoff <- NA_integer_
    # Before R 4.4.0, strptime() kept the isdst that a first mktime()
    # decided, and as.POSIXct() passed it to a second, which the C
    # library's mktime() may place differently from -1 where the time
    # doesn't exist. Since then it keeps -1 there (PR#18581), so one
    # mktime() gives base's result.
    if (getRversion() < "4.4.0") {
      isdst <- as.POSIXlt(as.POSIXct(lt, tz), tz)$isdst
      isdst[is.na(isdst)] <- -1L
      lt$isdst <- isdst
    }
    as.double(as.POSIXct(lt, tz))
  }
  days <- floor(secs / 86400)
  used <- par_unique(days[!is.na(days)], nthreads = nthreads)
  probe <- sort(par_unique(c(used - 1, used, used + 1), nthreads = nthreads))
  ends <- c(probe * 86400, probe * 86400 + 86399)
  offset <- place(ends) - ends
  start <- offset[seq_along(probe)]
  end <- offset[length(probe) + seq_along(probe)]
  i <- match(used, probe)
  same <- function(a, b) !is.na(a) & !is.na(b) & a == b
  plain <- same(end[i - 1L], start[i]) &
    same(start[i], end[i]) &
    same(end[i], start[i + 1L]) &
    same(start[i + 1L], end[i + 1L])
  k <- par_match(days, used, nthreads = nthreads)
  out <- secs + start[i][k]
  hard <- which(!plain[k])
  out[hard] <- place(secs[hard])
  out + frac
}
