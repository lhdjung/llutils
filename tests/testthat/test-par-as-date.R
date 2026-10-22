test_that("par_as_date() and par_as_posixct() agree with base on messy strings", {
  set.seed(42)
  n <- 2e4
  pick <- function(choices) sample(choices, n, TRUE)
  pad <- function(v, w) formatC(v, width = w, flag = "0")
  x <- paste0(
    pick(c(pad(sample(0:9999, 50), 4), "20", "", "02020", " 2020")),
    pick(c("-", "-", "/", ".")),
    pick(c(pad(1:12, 2), 1:12, "13", "00", " 7")),
    "-",
    pick(c(pad(1:31, 2), "32", "84", " 5")),
    pick(c(" ", "  ", "\t", "T", "")),
    pick(c(pad(0:24, 2), "25")),
    ":",
    pick(c(pad(0:59, 2), "60")),
    ":",
    pick(c(
      pad(0:61, 2),
      "62",
      "-1",
      "+5",
      ".5",
      "60.5",
      "1e1",
      "5e",
      "5e+",
      "nan",
      "inf",
      "Infinity",
      "-inf",
      "75",
      "",
      "12.3456789"
    )),
    pick(c("", "Z", "xyz"))
  )
  x[sample(n, 100)] <- NA
  for (format in c(
    "%Y-%m-%d %H:%M:%OS",
    # strptime() moves past seconds it rejects, to the "Z"
    "%Y-%m-%d %H:%M:%OSZ",
    "%Y-%m-%d %H:%M:%S",
    "%Y/%m/%d %H:%M",
    " %Y-%m-%d%H"
  )) {
    expect_identical(
      par_as_posixct(x, tz = "UTC", format = format),
      as.POSIXct(x, tz = "UTC", format = format)
    )
    expect_identical(
      par_as_date(x, format = format),
      as.Date(x, format = format)
    )
  }
})

test_that("tryFormats pick the format as base does", {
  set.seed(1)
  t <- as.POSIXct("1600-01-01", tz = "UTC") + runif(2e4, 0, 2e10)
  x <- c(NA, format(t, "%Y-%m-%d %H:%M:%OS3"))
  expect_identical(par_as_posixct(x, tz = "UTC"), as.POSIXct(x, tz = "UTC"))
  expect_identical(par_as_posixct(x, tz = "GMT"), as.POSIXct(x, tz = "GMT"))
  minutes <- substr(x, 1, 16)
  expect_identical(
    par_as_posixct(minutes, tz = "UTC"),
    as.POSIXct(minutes, tz = "UTC")
  )
  days <- c("", chartr("-", "/", substr(x, 1, 10)))
  expect_identical(par_as_date(days), as.Date(days))
  expect_identical(par_as_date(factor(days)), as.Date(factor(days)))
  named <- setNames(days, seq_along(days))
  expect_identical(par_as_date(named), as.Date(named))
  expect_identical(par_as_date(factor(named)), as.Date(factor(named)))
  expect_error(par_as_date(c("x", days)), "unambiguous")
  expect_identical(
    par_as_date(c("x", days), optional = TRUE),
    as.Date(c("x", days), optional = TRUE)
  )
  # Other time zones and hexadecimal seconds go to base.
  expect_identical(
    par_as_posixct(minutes, tz = "Europe/Berlin"),
    as.POSIXct(minutes, tz = "Europe/Berlin")
  )
  hex <- c("2020-01-01 12:34:0x1A", x)
  expect_identical(
    par_as_posixct(hex, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS"),
    as.POSIXct(hex, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")
  )
  nan <- c("2020-01-01 12:34:nan(1)Z", x)
  expect_identical(
    par_as_posixct(nan, tz = "UTC", format = "%Y-%m-%d %H:%M:%OSZ"),
    as.POSIXct(nan, tz = "UTC", format = "%Y-%m-%d %H:%M:%OSZ")
  )
})

test_that("tz = \"\" is parsed in Rust where the session is in UTC", {
  x <- rep(c("2024-03-05 14:30:00", "2024-03-05 24:00:00", NA), 1e4)
  withr::local_timezone("UTC")
  expect_identical(par_as_posixct(x), as.POSIXct(x))
  expect_identical(par_as_posixct(x, tz = ""), as.POSIXct(x, tz = ""))
  withr::local_timezone("Europe/Berlin")
  expect_identical(par_as_posixct(x), as.POSIXct(x))
})

test_that("the other specifiers read as strptime() does", {
  n <- 1e4
  sp <- function(s, f) {
    s <- rep(s, length.out = n)
    expect_identical(
      par_as_posixct(s, format = f, tz = "UTC"),
      suppressWarnings(as.POSIXct(s, format = f, tz = "UTC")),
      info = f
    )
    expect_identical(
      par_as_date(s, format = f),
      suppressWarnings(as.Date(s, format = f)),
      info = f
    )
  }
  sp(c("68-01-01", "69-01-01", "5-01-01", "123-01-01", "00-02-29"), "%y-%m-%d")
  sp(c("2020-01- 5", "2020-01-5", "2020-01-32"), "%Y-%m-%e")
  sp(c("2024-001", "2024-366", "2023-366", "2024- 60", "2024-0601"), "%Y-%j")
  sp(c("2024-03-060", "2024-03-100", "2024-02-090", "2024-12-366"), "%Y-%m-%j")
  sp(c("2024-15-060", "2024-31-100", "2024-02-15-300"), "%Y-%d-%j")
  sp(c("24-060", "99-365", "00-366"), "%y-%j")
  sp(
    c("12 AM", "12 PM", "1 pm", "01 Pm", "00 AM", "13 PM", "11 A.M.", "11 a"),
    "%I %p"
  )
  sp(c("2020-01-01 12", "2020-01-01 1", "2020-01-01 0"), "%Y-%m-%d %I")
  sp(c("2020-01-01 10 PM", "2020-01-01 24 PM"), "%Y-%m-%d %H %p")
  sp(c("2020-01-01 PM 10", "2020-01-01 am 12"), "%Y-%m-%d %p %I")
  sp(c("2020-01-01 1PM", "2020-01-01 1 PM", "2020-01-01 12am"), "%Y-%m-%d %I%p")
  sp(
    c(
      "05 Mar 2024",
      "05 March 2024",
      "05 MARCH 2024",
      "05 Sept 2024",
      "5 Jun 2024",
      "05 Marc 2024",
      "05 Ma 2024",
      "05 Juni 2024",
      "05 mAy 2024",
      "05 Decem 2024",
      "31 Apr 2024"
    ),
    "%d %b %Y"
  )
  sp(c("05 March 2024", "05 MAR 2024", "05 Marchx 2024"), "%d %B %Y")
  sp(c("05Mar2024", "05 Mar 2024", "5march2024"), "%d%h%Y")
  sp(
    paste(
      "2020-01-01 00:00:00",
      c(
        "+0530",
        "+05:30",
        "+05",
        "-0800",
        "+1400",
        "+1401",
        "+0560",
        "Z",
        "0530",
        " +0530",
        "+053",
        "+05301",
        "-0000",
        "+0501", # R: 5h 0m 36s
        "+0059",
        "-1400",
        "+"
      )
    ),
    "%Y-%m-%d %H:%M:%S %z"
  )
  sp(
    c(
      "2024-02-30 10:00:00 +0000",
      "2024-02-29 24:00:00 +0100",
      "2024-02-29 24:30:00 +0000",
      "2024-02-29 23:59:60 +0000",
      "2024-02-29 23:59:61 +0000"
    ),
    "%Y-%m-%d %H:%M:%S %z"
  )
  sp(c("+0100 2020-01-01 10:00:00", "-0100 2020-01-01 10:00:00"), "%z %F %T")
  sp(c("2020-01-01 00:00:00+0530", "2020-01-01 00:00:00\t+0530"), "%F %T%z")
  sp(
    c("2020-01-01 00:00:00.75 +0100", "2020-01-01 00:00:00.5+0000"),
    "%F %H:%M:%OS %z"
  )
  sp(c("2020-01-01 10:20", "2020-01-01 10:20:30", "2020/01/01 10:20"), "%F %R")
  sp(c("2024-03-05T14:30:00+0530", "2024-03-05T14:30:00"), "%FT%T%z")
  sp(c("2020-01-01%10", "2020-01-01 10"), "%Y-%m-%d%%%H")
  sp(c("1900-5", "1900-70"), "%Y-%y")
  sp(c("5 Mar 24 11:30 PM", "05 mar 99 12:00 am"), "%d %b %y %I:%M %p")
  s <- rep(c("05 Mar 2024", "6 Jun 2024", NA), length.out = n)
  formats <- c("%Y-%m-%d", "%d %b %Y")
  expect_identical(
    par_as_date(s, tryFormats = formats),
    as.Date(s, tryFormats = formats)
  )
  expect_identical(
    par_as_posixct(s, tz = "UTC", tryFormats = formats),
    as.POSIXct(s, tz = "UTC", tryFormats = formats)
  )
})

test_that("month names in other locales go to base", {
  ok <- tryCatch(
    {
      withr::local_locale(c(LC_TIME = "de_DE.UTF-8"))
      TRUE
    },
    warning = function(w) FALSE,
    error = function(e) FALSE
  )
  skip_if_not(ok, "no de_DE.UTF-8 locale")
  s <- rep(c("05 Mar 2024", "05 März 2024", "05 Mai 2024", NA), 2500)
  expect_identical(par_as_date(s, "%d %b %Y"), as.Date(s, "%d %b %Y"))
  s <- rep("2020-01-01 1 PM", 1e4)
  expect_identical(
    par_as_posixct(s, "%Y-%m-%d %I %p", tz = "UTC"),
    as.POSIXct(s, "%Y-%m-%d %I %p", tz = "UTC")
  )
})

test_that("other time zones give base's instants, also at changes of clock", {
  minutes <- function(from) {
    format(as.POSIXct(from, tz = "UTC") + seq(-7200, 7200, 60), "%F %T")
  }
  edge <- c(
    minutes("2024-03-31 01:00:00"), # Berlin
    minutes("2024-10-27 01:00:00"),
    minutes("2024-03-10 02:00:00"), # New York
    minutes("2024-11-03 01:00:00"),
    minutes("2024-04-07 02:00:00"), # Lord Howe, by half an hour
    minutes("2024-10-06 02:00:00"),
    minutes("2011-12-30 00:00:00"), # Apia skipped the day
    "2024-02-29 24:00:00",
    "1969-12-31 23:59:59",
    NA
  )
  set.seed(5)
  t <- as.POSIXct("1950-01-01", tz = "UTC") + runif(1e4, 0, 4e9)
  x <- c(edge, format(t, "%Y-%m-%d %H:%M:%OS3"))
  f <- "%Y-%m-%d %H:%M:%OS"
  zones <- c(
    "Europe/Berlin",
    "America/New_York",
    "Australia/Lord_Howe",
    "Pacific/Apia",
    "Asia/Kolkata"
  )
  for (tz in zones) {
    expect_identical(
      par_as_posixct(x, tz = tz, format = f),
      as.POSIXct(x, tz = tz, format = f),
      info = tz
    )
    # A time that doesn't exist in `tz` fails its format, so base goes on to
    # one that reads the date alone, or errors.
    expect_identical(
      tryCatch(par_as_posixct(x, tz = tz), error = conditionMessage),
      tryCatch(as.POSIXct(x, tz = tz), error = conditionMessage),
      info = tz
    )
    expect_identical(
      par_as_posixct(x, tz = tz, optional = TRUE),
      as.POSIXct(x, tz = tz, optional = TRUE),
      info = tz
    )
  }
  y <- x[-seq_along(edge)]
  tz <- "Europe/Berlin"
  expect_identical(par_as_posixct(y, tz = tz), as.POSIXct(y, tz = tz))
  named <- setNames(y, seq_along(y))
  expect_identical(par_as_posixct(named, tz = tz), as.POSIXct(named, tz = tz))
  expect_identical(
    par_as_posixct(factor(y), tz = tz),
    as.POSIXct(factor(y), tz = tz)
  )
  z <- rep(
    c("2024-03-05T14:30:00+0530", "2024-10-27T02:30:00+0100", NA),
    length.out = 1e4
  )
  expect_identical(
    par_as_posixct(z, tz = tz, format = "%FT%T%z"),
    as.POSIXct(z, tz = tz, format = "%FT%T%z")
  )
  expect_identical(
    par_as_posixct(z, tz = tz, tryFormats = "%FT%T%z"),
    as.POSIXct(z, tz = tz, tryFormats = "%FT%T%z")
  )
})
