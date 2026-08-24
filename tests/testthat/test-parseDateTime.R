utc <- function(x) as.POSIXct(x, tz = "UTC")

# ── Regression tests for the bugs found in the pre-rewrite parser ─────────────

test_that("day-first dates are not truncated by year-first formats", {
  # Previously "15-01-2024" matched the prefix "15-01-20" under "%Y-%m-%d"
  # and returned year 15.
  expect_equal(parse_to_datetime("15-01-2024"), utc("2024-01-15"))
  expect_equal(parse_to_datetime("25/12/2024"), utc("2024-12-25"))
  expect_equal(parse_to_datetime("25.12.2024"), utc("2024-12-25"))
  expect_equal(parse_to_datetime("15-01-2024 08:45:00"), utc("2024-01-15 08:45:00"))
})

test_that("time of day is preserved for every separator", {
  expect_equal(parse_to_datetime("2024-01-15 10:30:00"), utc("2024-01-15 10:30:00"))
  expect_equal(parse_to_datetime("2024-01-15 10:30"),    utc("2024-01-15 10:30:00"))
  expect_equal(parse_to_datetime("2024-01-15T10:30:00"), utc("2024-01-15 10:30:00"))
  expect_equal(parse_to_datetime("15/01/2024 10:30:00"), utc("2024-01-15 10:30:00"))
})

test_that("compact yyyymmdd is a date, not a Unix timestamp", {
  # Previously 20240115 > 100000 routed it to the Unix branch (1970-08-23).
  expect_equal(parse_to_datetime("20240115"), utc("2024-01-15"))
  expect_equal(parse_to_datetime("19991231"), utc("1999-12-31"))
})

test_that("malformed numeric-looking values return NA instead of crashing", {
  # Previously "1.2.3" coerced to NA, producing an NA subscript and the error
  # "NAs are not allowed in subscripted assignments".
  expect_warning(r <- parse_to_datetime(c("1.2.3", "1704067200")))
  expect_true(is.na(r[1]))
  expect_equal(unname(r[2]), utc("2024-01-01 00:00:00"))
})

test_that("numeric input is not mangled by scientific notation", {
  # as.character(1e5) is "1e+05", which failed the old numeric pattern and
  # returned NA. Any non-NA result proves the round-trip is gone.
  expect_false(is.na(parse_to_datetime(1e5)))
  expect_equal(parse_to_datetime(1704067200), utc("2024-01-01 00:00:00"))
  expect_equal(parse_to_datetime(45000), utc("2023-03-15"))
})

test_that("the Unix/Excel split sits just above 100000", {
  # Documented heuristic: strictly greater than 100000 is Unix seconds,
  # anything at or below it is an Excel serial.
  expect_equal(parse_to_datetime(100000), utc("2173-10-14"))
  expect_equal(parse_to_datetime(100001), utc("1970-01-02 03:46:41"))
})

test_that("names are preserved", {
  r <- parse_to_datetime(c(a = "2024-01-15", b = "2024-02-20"))
  expect_equal(names(r), c("a", "b"))
})

test_that("unparsed values raise a warning naming them", {
  expect_warning(parse_to_datetime(c("2024-01-15", "not a date")), "not a date")
  expect_warning(parse_to_datetime("nope"), "1 of 1")
  expect_silent(parse_to_datetime("nope", quiet = TRUE))
})

test_that("trailing junk is rejected rather than silently truncated", {
  # Previously "2024-01-15xyz" parsed to 2024-01-15.
  expect_warning(r <- parse_to_datetime("2024-01-15xyz"))
  expect_true(is.na(r))
})

# ── Ambiguity: explicit ───────────────────────────────────────────────────────

test_that("dayfirst controls ambiguous all-numeric dates", {
  expect_equal(parse_to_datetime("01/02/2024", dayfirst = TRUE),  utc("2024-02-01"))
  expect_equal(parse_to_datetime("01/02/2024", dayfirst = FALSE), utc("2024-01-02"))
  expect_equal(parse_to_datetime("01-02-2024", dayfirst = TRUE),  utc("2024-02-01"))
  expect_equal(parse_to_datetime("01-02-2024", dayfirst = FALSE), utc("2024-01-02"))
})

# ── Ambiguity: automatic detection ────────────────────────────────────────────

test_that("self-evident values are parsed per element, whatever the vector holds", {
  # 25 > 12 can only be a day; 12/25 can only be month-day. Neither needs a
  # column convention, so both are correct even in the same vector.
  expect_equal(parse_to_datetime(c("25/12/2024", "12/25/2024")),
               rep(utc("2024-12-25"), 2))
  expect_equal(parse_to_datetime(c("31-01-2024", "01-31-2024")),
               rep(utc("2024-01-31"), 2))
})

test_that("ambiguous values inherit the order implied by their neighbours", {
  expect_equal(unname(parse_to_datetime(c("25/12/2024", "01/02/2024"))[2]),
               utc("2024-02-01"))
  expect_equal(unname(parse_to_datetime(c("12/25/2024", "01/02/2024"))[2]),
               utc("2024-01-02"))
})

test_that("inference works across separators and with times attached", {
  expect_equal(unname(parse_to_datetime(c("25-12-2024", "01-02-2024 08:30"))[2]),
               utc("2024-02-01 08:30:00"))
  expect_equal(unname(parse_to_datetime(c("12.25.2024", "01.02.2024"))[2]),
               utc("2024-01-02"))
})

test_that("the vote is weighted by row count, not by distinct value", {
  # One month-first outlier must not overturn many day-first rows.
  x <- c(rep("25/12/2024", 50), "12/25/2024", "01/02/2024")
  expect_warning(r <- parse_to_datetime(x), "mixes day-first and month-first")
  expect_equal(unname(r[52]), utc("2024-02-01"))

  y <- c(rep("12/25/2024", 50), "25/12/2024", "01/02/2024")
  expect_warning(r2 <- parse_to_datetime(y), "mixes day-first and month-first")
  expect_equal(unname(r2[52]), utc("2024-01-02"))
})

test_that("evidence from an unambiguous value beats the day-first fallback", {
  # Alone, "03/04/2024" would fall back to day-first (3 April). The
  # month-first neighbour flips it to 4 March.
  expect_equal(unname(parse_to_datetime("03/04/2024", dayfirst = TRUE)),
               utc("2024-04-03"))
  expect_equal(unname(parse_to_datetime(c("12/25/2024", "03/04/2024"))[2]),
               utc("2024-03-04"))
})

test_that("unresolvable ambiguity warns and falls back to day-first", {
  expect_warning(r <- parse_to_datetime("01/02/2024"), "ambiguous")
  expect_equal(unname(r), utc("2024-02-01"))
  expect_silent(parse_to_datetime("01/02/2024", quiet = TRUE))
  # Naming the convention explicitly is never a guess, so never a warning.
  expect_silent(parse_to_datetime("01/02/2024", dayfirst = TRUE))
  expect_silent(parse_to_datetime("01/02/2024", dayfirst = FALSE))
})

test_that("a consistent vector with no ambiguous values infers silently", {
  expect_silent(parse_to_datetime(c("25/12/2024", "31/01/2024")))
  expect_silent(parse_to_datetime(c("12/25/2024", "01/31/2024")))
})

test_that("explicit dayfirst still overrides inference for ambiguous values", {
  x <- c("25/12/2024", "01/02/2024")
  # Evidence says day-first; the caller says otherwise for the ambiguous one.
  expect_equal(unname(parse_to_datetime(x, dayfirst = FALSE)[2]), utc("2024-01-02"))
  # The self-evident value is unaffected either way.
  expect_equal(unname(parse_to_datetime(x, dayfirst = FALSE)[1]), utc("2024-12-25"))
})

test_that("inference ignores non-triple formats", {
  # ISO, month-name and serial values carry no day/month evidence, so the
  # ambiguous value still falls back and warns.
  expect_warning(
    r <- parse_to_datetime(c("2024-12-25", "25 Dec 2024", "1704067200", "01/02/2024")),
    "ambiguous"
  )
  expect_equal(unname(r[4]), utc("2024-02-01"))
})

test_that("inference is unaffected by unparseable noise", {
  expect_warning(r <- parse_to_datetime(c("12/25/2024", "01/02/2024", "rubbish")))
  expect_equal(unname(r[2]), utc("2024-01-02"))
  expect_true(is.na(r[3]))
})

test_that("dayfirst applies consistently when a time is attached", {
  expect_equal(parse_to_datetime("01/02/2024 06:00", dayfirst = TRUE),
               utc("2024-02-01 06:00:00"))
  expect_equal(parse_to_datetime("01/02/2024 06:00", dayfirst = FALSE),
               utc("2024-01-02 06:00:00"))
})

test_that("unambiguous dates ignore dayfirst", {
  for (df in c(TRUE, FALSE)) {
    expect_equal(parse_to_datetime("2024-01-15", dayfirst = df), utc("2024-01-15"))
    expect_equal(parse_to_datetime("25/12/2024", dayfirst = df), utc("2024-12-25"))
    expect_equal(parse_to_datetime("12/25/2024", dayfirst = df), utc("2024-12-25"))
  }
})

# ── Format coverage ───────────────────────────────────────────────────────────

test_that("year-first layouts parse", {
  expect_equal(parse_to_datetime("2024-01-15"), utc("2024-01-15"))
  expect_equal(parse_to_datetime("2024/01/15"), utc("2024-01-15"))
  expect_equal(parse_to_datetime("2024.01.15"), utc("2024-01-15"))
})

test_that("month-name layouts parse", {
  expect_equal(parse_to_datetime("15-Jan-2024"),   utc("2024-01-15"))
  expect_equal(parse_to_datetime("15 Jan 2024"),   utc("2024-01-15"))
  expect_equal(parse_to_datetime("Jan 15, 2024"),  utc("2024-01-15"))
  expect_equal(parse_to_datetime("January 15, 2024"), utc("2024-01-15"))
  expect_equal(parse_to_datetime("15 January 2024"),  utc("2024-01-15"))
})

test_that("unpadded components parse", {
  expect_equal(parse_to_datetime("2024-1-5"), utc("2024-01-05"))
  expect_equal(parse_to_datetime("5/1/2024", dayfirst = TRUE), utc("2024-01-05"))
})

test_that("12-hour clock with AM/PM parses", {
  expect_equal(parse_to_datetime("2024-01-15 01:30:00 PM"), utc("2024-01-15 13:30:00"))
  expect_equal(parse_to_datetime("15/01/2024 09:05 AM"),    utc("2024-01-15 09:05:00"))
})

test_that("ISO 8601 offsets are honoured", {
  expect_equal(parse_to_datetime("2024-01-15T10:30:00Z"),      utc("2024-01-15 10:30:00"))
  expect_equal(parse_to_datetime("2024-01-15T10:30:00+04:30"), utc("2024-01-15 06:00:00"))
  expect_equal(parse_to_datetime("2024-01-15T10:30:00+0430"),  utc("2024-01-15 06:00:00"))
  expect_equal(parse_to_datetime("2024-01-15T10:30:00.123Z"),  utc("2024-01-15 10:30:00"))
})

test_that("JavaScript Date.toString() output parses", {
  expect_equal(
    parse_to_datetime("Mon Jan 15 2024 10:30:00 GMT+0100 (Central European Time)"),
    utc("2024-01-15 09:30:00")
  )
  expect_equal(
    parse_to_datetime("Mon Jan 15 2024 10:30:00 GMT+0000"),
    utc("2024-01-15 10:30:00")
  )
})

test_that("Unix seconds and Excel serials are distinguished", {
  expect_equal(parse_to_datetime("1704067200"), utc("2024-01-01 00:00:00"))
  expect_equal(parse_to_datetime("45000"),      utc("2023-03-15"))
  # Fractional Excel serials keep their time of day; the old parser went
  # through as.Date() and dropped it.
  expect_equal(parse_to_datetime("45000.5"),    utc("2023-03-15 12:00:00"))
})

test_that("pre-1970 Unix timestamps are read as negative seconds", {
  expect_equal(parse_to_datetime("-86400"), utc("1969-12-31"))
})

# ── Typed input ───────────────────────────────────────────────────────────────

test_that("POSIXct input round-trips without losing time", {
  x <- utc("2024-01-15 10:30:00")
  expect_equal(parse_to_datetime(x), x)
})

test_that("Date input parses to midnight UTC", {
  expect_equal(parse_to_datetime(as.Date("2024-01-15")), utc("2024-01-15"))
})

test_that("POSIXlt input round-trips", {
  x <- as.POSIXlt("2024-01-15 10:30:00", tz = "UTC")
  expect_equal(parse_to_datetime(x), utc("2024-01-15 10:30:00"))
})

test_that("factor input parses", {
  expect_equal(parse_to_datetime(factor(c("2024-01-15", "2024-02-20"))),
               utc(c("2024-01-15", "2024-02-20")))
})

# ── Validity and edge cases ───────────────────────────────────────────────────

test_that("impossible dates are rejected", {
  expect_warning(expect_true(is.na(parse_to_datetime("2024-02-30"))))
  expect_warning(expect_true(is.na(parse_to_datetime("2023-02-29"))))
  expect_warning(expect_true(is.na(parse_to_datetime("2024-13-01"))))
  expect_equal(parse_to_datetime("2024-02-29"), utc("2024-02-29"))  # real leap day
})

test_that("blank, whitespace and NA inputs give NA without warning", {
  expect_silent(r <- parse_to_datetime(c(NA, "", "   ")))
  expect_true(all(is.na(r)))
})

test_that("zero-length input returns zero-length POSIXct", {
  r <- parse_to_datetime(character(0))
  expect_s3_class(r, "POSIXct")
  expect_length(r, 0)
})

test_that("surrounding whitespace is tolerated", {
  expect_equal(parse_to_datetime("  2024-01-15  "), utc("2024-01-15"))
})

test_that("mixed formats resolve element-wise in one call", {
  expect_warning(
    r <- parse_to_datetime(c("2024-01-15", "15/01/2024", "15-Jan-2024", "20240115",
                             "1704067200", NA, "rubbish"))
  )
  expect_equal(unname(r[1:4]), rep(utc("2024-01-15"), 4))
  expect_equal(unname(r[5]), utc("2024-01-01"))
  expect_true(all(is.na(r[6:7])))
})

# ── Contract ──────────────────────────────────────────────────────────────────

test_that("timezone relabels without shifting the instant", {
  r <- parse_to_datetime("2024-01-15T10:30:00Z", timezone = "Asia/Kabul")
  expect_equal(attr(r, "tzone"), "Asia/Kabul")
  expect_equal(as.numeric(r), as.numeric(utc("2024-01-15 10:30:00")))
  expect_equal(format(r, "%H:%M"), "15:00")
})

test_that("invalid arguments are rejected", {
  expect_error(parse_to_datetime("2024-01-15", timezone = "Not/AZone"), "Unknown")
  expect_error(parse_to_datetime("2024-01-15", timezone = c("UTC", "GMT")), "single")
  expect_error(parse_to_datetime("2024-01-15", dayfirst = NA), "auto")
  expect_error(parse_to_datetime("2024-01-15", dayfirst = "yes"), "auto")
  expect_error(parse_to_datetime("2024-01-15", quiet = "yes"), "TRUE or FALSE")
})

test_that("result length always matches input length", {
  x <- c("2024-01-15", NA, "20240115", "rubbish", "45000")
  expect_warning(r <- parse_to_datetime(x))
  expect_length(r, length(x))
})

test_that("repeated values parse identically under the unique() fast path", {
  x <- rep(c("15/01/2024", "2024-01-15"), 50)
  r <- parse_to_datetime(x)
  expect_true(all(r == utc("2024-01-15")))
})
