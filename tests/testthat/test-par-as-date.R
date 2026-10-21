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
