# ── Internal: strptime format → anchored regex ────────────────────────────────
#
# The core correctness problem this solves: strptime() does not require a
# format to consume the whole string, so "%Y-%m-%d" happily matches the prefix
# "15-01-20" of "15-01-2024" and silently yields year 15. Every candidate
# format is therefore paired with a fully anchored regex, and a format is only
# attempted on strings that match its regex end to end.

.STRPTIME_RX <- list(
  "%Y" = "[0-9]{4}",
  "%m" = "[0-9]{1,2}",
  "%d" = "[0-9]{1,2}",
  "%H" = "[0-9]{1,2}",
  "%I" = "[0-9]{1,2}",
  "%M" = "[0-9]{2}",
  "%S" = "[0-9]{2}([.][0-9]+)?",
  "%b" = "[A-Za-z]{3,9}",
  "%B" = "[A-Za-z]{3,9}",
  "%p" = "[AaPp][Mm]"
)

.rx_escape <- function(ch) {
  if (grepl("^[[:space:]]$", ch)) return("[[:space:]]+")
  if (grepl("^[][{}().*+?^$|\\\\]$", ch)) return(paste0("\\", ch))
  ch
}

.fmt_to_regex <- function(fmt) {
  chars <- strsplit(fmt, "", fixed = TRUE)[[1]]
  out   <- character(length(chars))
  k     <- 0L
  i     <- 1L

  while (i <= length(chars)) {
    if (chars[i] == "%" && i < length(chars)) {
      tok <- paste0("%", chars[i + 1L])
      rx  <- .STRPTIME_RX[[tok]]
      if (is.null(rx)) {
        stop("Unsupported strptime token in format: ", tok, call. = FALSE)
      }
      k <- k + 1L; out[k] <- rx
      i <- i + 2L
    } else {
      k <- k + 1L; out[k] <- .rx_escape(chars[i])
      i <- i + 1L
    }
  }

  paste0("^", paste(out[seq_len(k)], collapse = ""), "$")
}

# Format tables are pure functions of `dayfirst`, so build once and memoise.
.fmt_cache <- new.env(parent = emptyenv())

.format_table <- function(dayfirst) {
  key <- if (isTRUE(dayfirst)) "dayfirst" else "monthfirst"
  hit <- .fmt_cache[[key]]
  if (!is.null(hit)) return(hit)

  # Year-first patterns are unambiguous (anchored %Y needs exactly 4 digits),
  # so they always go first. The d-m / m-d pair is genuinely ambiguous and its
  # order is what `dayfirst` controls.
  ymd   <- c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d")
  dmy   <- c("%d-%m-%Y", "%d/%m/%Y", "%d.%m.%Y")
  mdy   <- c("%m-%d-%Y", "%m/%d/%Y", "%m.%d.%Y")
  named <- c("%d-%b-%Y", "%d %b %Y", "%d %b, %Y",
             "%b-%d-%Y", "%b %d %Y", "%b %d, %Y",
             "%d-%B-%Y", "%d %B %Y", "%B %d %Y", "%B %d, %Y",
             "%Y %b %d", "%Y-%b-%d")

  dates <- c(ymd, if (isTRUE(dayfirst)) c(dmy, mdy) else c(mdy, dmy), named)
  times <- c("", " %H:%M:%S", "T%H:%M:%S", " %H:%M", "T%H:%M",
             " %I:%M:%S %p", " %I:%M %p")

  # Date format varies slowest: every time variant of a day-first pattern is
  # tried before any month-first pattern, so `dayfirst` wins consistently
  # whether or not a time component is present.
  fmts <- as.vector(t(outer(dates, times, paste0)))

  tbl <- list(fmt = fmts, rx = vapply(fmts, .fmt_to_regex, character(1), USE.NAMES = FALSE))
  .fmt_cache[[key]] <- tbl
  tbl
}

# ── Internal: small helpers ───────────────────────────────────────────────────

.strptime_utc <- function(x, fmt) {
  suppressWarnings(as.POSIXct(strptime(x, format = fmt, tz = "UTC")))
}

# Month abbreviations and AM/PM are locale-dependent; force the C locale so
# English month names parse regardless of the machine's LC_TIME.
.with_c_time_locale <- function(expr) {
  old <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old), silent = TRUE), add = TRUE)
  ok <- suppressWarnings(try(Sys.setlocale("LC_TIME", "C"), silent = TRUE))
  force(expr)
}

# Unix epoch seconds vs. Excel serial days. Excel serials for plausible dates
# run to roughly 55000 (year 2050); Unix seconds below 100000 would be the
# first day of 1970. Negative values are treated as pre-1970 Unix seconds.
# ── Internal: day-first / month-first resolution ──────────────────────────────
#
# An all-numeric triple like "01/02/2024" is ambiguous, but most real values
# are not: "25/12/2024" can only be day-first and "12/25/2024" can only be
# month-first. Those resolve from their own digits, per element, with no need
# for a column-wide convention — which is why a vector may legitimately mix
# both and still parse correctly.
#
# Only values whose first two components are both <= 12 need a convention.
# Those are settled by a vote over the self-disambiguating values elsewhere in
# the same vector, weighted by how often each distinct string occurs.

.TRIPLE_RX <- "^([0-9]{1,2})([-/.])([0-9]{1,2})\\2([0-9]{4})([T[:space:]].*)?$"

.resolve_dayfirst <- function(s, pending, weights, dayfirst, quiet) {
  n      <- length(s)
  forced <- rep(NA, n)   # TRUE = must be day-first, FALSE = must be month-first

  m <- pending & grepl(.TRIPLE_RX, s, perl = TRUE)
  if (any(m)) {
    c1 <- suppressWarnings(as.integer(sub(.TRIPLE_RX, "\\1", s[m], perl = TRUE)))
    c2 <- suppressWarnings(as.integer(sub(.TRIPLE_RX, "\\3", s[m], perl = TRUE)))
    forced[m] <- ifelse(c1 > 12 & c2 <= 12, TRUE,
                        ifelse(c2 > 12 & c1 <= 12, FALSE, NA))
  }

  if (is.logical(dayfirst)) {
    fallback <- dayfirst
  } else {
    w        <- if (is.null(weights)) rep(1L, n) else weights
    n_day    <- sum(w[which(forced)])
    n_month  <- sum(w[which(!forced)])
    fallback <- if (n_month > n_day) FALSE else TRUE

    if (!quiet && any(m & is.na(forced))) {
      order_txt <- if (fallback) "day-month-year" else "month-day-year"
      if (n_day > 0 && n_month > 0) {
        warning(sprintf(
          paste0("parse_to_datetime(): this vector mixes day-first and month-first dates ",
                 "(%d vs %d self-evident value(s)). Self-evident values were parsed on their ",
                 "own terms; ambiguous ones were read as %s. Pass dayfirst = TRUE/FALSE to override."),
          n_day, n_month, order_txt), call. = FALSE)
      } else if (n_day == 0 && n_month == 0) {
        warning(sprintf(
          paste0("parse_to_datetime(): all-numeric dates here are ambiguous and nothing in the ",
                 "vector settles the order; read as %s. Pass dayfirst = TRUE/FALSE to be explicit."),
          order_txt), call. = FALSE)
      }
    }
  }

  forced[is.na(forced)] <- fallback
  forced
}

.numeric_to_posixct <- function(v) {
  out  <- .POSIXct(rep(NA_real_, length(v)), tz = "UTC")
  unix <- !is.na(v) & (v > 100000 | v < 0)
  xl   <- !is.na(v) & !unix
  out[unix] <- .POSIXct(v[unix], tz = "UTC")
  # 25569 = days between the Excel origin (1899-12-30) and 1970-01-01.
  # Multiplying rather than going via as.Date() keeps fractional days, so an
  # Excel serial carrying a time of day no longer loses it.
  out[xl]   <- .POSIXct(round((v[xl] - 25569) * 86400), tz = "UTC")
  out
}

# ── Internal: the staged string parser ────────────────────────────────────────

.parse_strings <- function(s, dayfirst, weights = NULL, quiet = FALSE) {
  res     <- .POSIXct(rep(NA_real_, length(s)), tz = "UTC")
  pending <- !is.na(s)
  if (!any(pending)) return(res)

  claim <- function(mask, parsed) {
    hit <- !is.na(parsed)
    if (any(hit)) {
      pos <- which(mask)[hit]
      res[pos]     <<- parsed[hit]
      pending[pos] <<- FALSE
    }
  }

  .with_c_time_locale({
    # 1. Compact yyyymmdd. This must precede the numeric stage, which would
    #    otherwise read "20240115" as Unix seconds (1970-08-23). The regex is
    #    tight — 8 digits, year 1900-2099, valid month and day range — so a
    #    genuine 8-digit Unix timestamp only collides inside August 1970.
    m <- pending & grepl("^(19|20)[0-9]{2}(0[1-9]|1[0-2])(0[1-9]|[12][0-9]|3[01])$", s)
    if (any(m)) claim(m, .strptime_utc(s[m], "%Y%m%d"))

    # 2. Numeric (Unix seconds / Excel serials). The regex rejects things like
    #    "1.2.3", which previously coerced to NA and crashed the index maths.
    m <- pending & grepl("^-?[0-9]+([.][0-9]+)?$", s)
    if (any(m)) claim(m, .numeric_to_posixct(as.numeric(s[m])))

    # 3. ISO 8601, with either "T" or a space as the separator, optional
    #    sub-seconds and optional Z / ±HH:MM offset. Unlike the previous
    #    version, values that match the shape but fail to parse stay pending
    #    and fall through to the format loop rather than being written off.
    m <- pending & grepl(paste0(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}[T ][0-9]{2}:[0-9]{2}",
      "(:[0-9]{2}([.][0-9]+)?)?[[:space:]]*(Z|[+-][0-9]{2}:?[0-9]{2})?$"
    ), s)
    if (any(m)) claim(m, suppressWarnings(lubridate::as_datetime(s[m])))

    # 4. JavaScript Date.toString(): "Mon Jan 15 2024 10:30:00 GMT+0100 (CET)"
    m <- pending & grepl(
      "^[A-Za-z]{3}[[:space:]]+[A-Za-z]{3}[[:space:]]+[0-9]{1,2}[[:space:]]+[0-9]{4}[[:space:]]+[0-9]{2}:[0-9]{2}:[0-9]{2}[[:space:]]+GMT",
      s
    )
    if (any(m)) {
      clean <- sub("[[:space:]]*\\(.*\\)[[:space:]]*$", "", s[m])  # drop "(Timezone Name)"
      clean <- sub("GMT([+-][0-9]{4})", "\\1", clean)              # GMT+0100 → +0100
      clean <- sub("GMT$", "+0000", clean)
      claim(m, .strptime_utc(clean, "%a %b %d %Y %H:%M:%S %z"))
    }

    # 5. Anchored format trial loop. Each format is regex-filtered first, then
    #    strptime confirms the date is real (rejecting e.g. 2024-02-30).
    #
    #    Day/month order is decided per element, so one vector can hold both
    #    conventions. The loop therefore runs once per convention over disjoint
    #    subsets; formats that carry no ambiguity (year-first, month-name)
    #    behave identically in either pass.
    want_day <- .resolve_dayfirst(s, pending, weights, dayfirst, quiet)

    for (conv in c(TRUE, FALSE)) {
      grp <- pending & want_day == conv
      if (!any(grp)) next
      tbl <- .format_table(conv)
      for (i in seq_along(tbl$fmt)) {
        if (!any(grp)) break
        m <- grp & grepl(tbl$rx[i], s)
        if (any(m)) {
          claim(m, .strptime_utc(s[m], tbl$fmt[i]))
          grp <- grp & pending
        }
      }
    }
  })

  res
}

# ── Internal: unparsed-value reporting ────────────────────────────────────────
#
# Failures are reported as data, not just as prose: one row per distinct
# unrecognised string, with how often it occurred and where. The warning text
# is a rendering of that table, capped so a wholly unparseable column does not
# print thousands of lines.

.unparsed_table <- function(original, failed) {
  idx <- which(failed)
  val <- trimws(original[idx])
  lvl <- unique(val)
  by  <- split(idx, factor(val, levels = lvl))

  out <- data.frame(value = lvl, count = as.integer(lengths(by)),
                    stringsAsFactors = FALSE)
  out$index <- unname(by)   # list column: row positions in the input
  out
}

.empty_unparsed <- function() {
  out <- data.frame(value = character(0), count = integer(0),
                    stringsAsFactors = FALSE)
  out$index <- list()
  out
}

.format_unparsed <- function(tbl, report_max) {
  show <- if (is.finite(report_max)) min(report_max, nrow(tbl)) else nrow(tbl)
  show <- max(as.integer(show), 1L)

  lines <- vapply(seq_len(show), function(i) {
    rows <- tbl$index[[i]]
    shown_rows <- rows[seq_len(min(3L, length(rows)))]
    rows_txt <- paste0(
      if (length(rows) == 1L) "row " else "rows ",
      paste(shown_rows, collapse = ", "),
      if (length(rows) > length(shown_rows)) ", ..." else ""
    )
    sprintf('  "%s" (%d value(s); %s)', tbl$value[i], tbl$count[i], rows_txt)
  }, character(1))

  if (nrow(tbl) > show) {
    lines <- c(lines, sprintf(
      "  ... and %d more distinct value(s); see attr(x, \"unparsed\") or unparsed_values(x)",
      nrow(tbl) - show))
  }
  paste(lines, collapse = "\n")
}

# ── Exported ──────────────────────────────────────────────────────────────────

#' Parse heterogeneous date and date-time values to `POSIXct`
#'
#' Coerces a vector of mixed-format dates — the sort that accumulates in survey
#' exports and hand-maintained spreadsheets — to a single `POSIXct` vector.
#' Recognises Unix epoch seconds, Excel serial numbers, ISO 8601, JavaScript
#' `Date.toString()` output, compact `yyyymmdd`, and a table of common
#' numeric and month-name layouts with optional time of day.
#'
#' Values are matched against fully anchored patterns, so a format is only
#' applied when it accounts for the entire string. Anything unrecognised
#' returns `NA` and is reported: the offending values are listed in a warning
#' (unless `quiet = TRUE`) and, either way, attached to the result as an
#' `"unparsed"` attribute readable with [unparsed_values()].
#'
#' Resolution order is: compact `yyyymmdd`, then numeric, then ISO 8601, then
#' JavaScript, then the format table. Within the format table, year-first
#' layouts are unambiguous and always tried first.
#'
#' @section Day-first vs. month-first:
#'
#' All-numeric dates like `"01/02/2024"` are ambiguous, but most real values
#' are not: `"25/12/2024"` can only be day-month-year and `"12/25/2024"` can
#' only be month-day-year. Such values are resolved element by element from
#' their own digits, so a single vector may mix both conventions and still
#' parse correctly.
#'
#' Only values whose first two components are both `<= 12` need a convention
#' imposed. Under the default `dayfirst = "auto"` these are settled by a vote
#' over the self-evident values elsewhere in the same vector, weighted by how
#' often each distinct string occurs. If the vote is tied or there is nothing
#' to vote on, day-month-year is assumed and a warning says so. Passing
#' `dayfirst = TRUE` or `FALSE` skips inference for the ambiguous values;
#' self-evident values are still read on their own terms either way.
#'
#' @param date_vector Vector to parse. Character, factor, numeric, `Date`,
#'   `POSIXct` and `POSIXlt` inputs are accepted. Temporal inputs are passed
#'   through without a string round-trip, so their time of day is preserved.
#' @param dayfirst How to read ambiguous all-numeric dates such as
#'   `"01/02/2024"`. `"auto"` (the default) infers the order from the rest of
#'   the vector; `TRUE` forces day-month-year and `FALSE` month-day-year. See
#'   the section below.
#' @param timezone Time zone to label the result with, as a name from
#'   [OlsonNames()]. Parsing always happens in UTC; this sets the `tzone`
#'   attribute, which changes how the instants display but not which instants
#'   they are.
#' @param quiet Logical. When `FALSE` (the default), emit a warning listing the
#'   values that could not be parsed, how often each occurred and where.
#' @param report_max Maximum number of *distinct* unparsed values to list in
#'   that warning; the remainder are counted in a trailing line. Use `Inf` to
#'   list every one. The `"unparsed"` attribute on the result is never
#'   truncated.
#'
#' @return A `POSIXct` vector the same length as `date_vector`, carrying its
#'   names, with `NA` wherever parsing failed. When some values failed, the
#'   result also carries an `"unparsed"` attribute: a data frame with one row
#'   per distinct unrecognised value and columns `value`, `count` and `index`
#'   (a list column of positions in the input). See [unparsed_values()].
#'
#' @examples
#' parse_to_datetime(c("2024-01-15", "15/01/2024", "15-Jan-2024", "20240115"))
#'
#' # Time of day survives, whatever the separator
#' parse_to_datetime(c("2024-01-15 10:30:00", "2024-01-15T10:30:00Z"))
#'
#' # Self-evident values decide the order for the ambiguous one:
#' # "25/12/2024" can only be day-first, so "01/02/2024" is read as 1 February
#' parse_to_datetime(c("25/12/2024", "01/02/2024"))
#'
#' # ... and the same ambiguous value flips when the evidence does
#' parse_to_datetime(c("12/25/2024", "01/02/2024"))
#'
#' # A vector may mix both conventions where each value is self-evident
#' parse_to_datetime(c("25/12/2024", "12/25/2024"))
#'
#' # Or state it outright
#' parse_to_datetime("01/02/2024", dayfirst = FALSE)  # 2 January
#'
#' # Excel serials and Unix epoch seconds
#' parse_to_datetime(c("45000", "1704067200"))
#'
#' # Unparsed values are named in the warning and kept on the result
#' x <- suppressWarnings(parse_to_datetime(c("2024-01-15", "n/a", "later", "n/a")))
#' unparsed_values(x)
#'
#' @export
parse_to_datetime <- function(date_vector,
                              dayfirst   = "auto",
                              timezone   = "UTC",
                              quiet      = FALSE,
                              report_max = 10) {

  if (!is.character(timezone) || length(timezone) != 1L || is.na(timezone)) {
    stop("`timezone` must be a single non-NA character string.", call. = FALSE)
  }
  if (!nzchar(timezone) || !timezone %in% OlsonNames()) {
    stop("Unknown `timezone`: \"", timezone, "\". See OlsonNames().", call. = FALSE)
  }
  if (!(identical(dayfirst, "auto") ||
        (is.logical(dayfirst) && length(dayfirst) == 1L && !is.na(dayfirst)))) {
    stop("`dayfirst` must be \"auto\", TRUE, or FALSE.", call. = FALSE)
  }
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("`quiet` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(report_max) || length(report_max) != 1L ||
      is.na(report_max) || report_max < 1) {
    stop("`report_max` must be a single number >= 1 (or Inf).", call. = FALSE)
  }

  nm <- names(date_vector)
  n  <- length(date_vector)

  finish <- function(x) {
    attr(x, "tzone") <- timezone
    names(x) <- nm
    x
  }

  # Already temporal: convert directly rather than via as.character(), which
  # would drop the time component on the way through.
  if (inherits(date_vector, "POSIXct")) {
    return(finish(.POSIXct(unname(as.numeric(date_vector)), tz = "UTC")))
  }
  if (inherits(date_vector, "POSIXlt")) {
    return(finish(.POSIXct(unname(as.numeric(as.POSIXct(date_vector))), tz = "UTC")))
  }
  if (inherits(date_vector, "Date")) {
    return(finish(.POSIXct(unname(as.numeric(date_vector)) * 86400, tz = "UTC")))
  }

  if (n == 0L) return(finish(.POSIXct(numeric(0), tz = "UTC")))

  if (is.numeric(date_vector)) {
    # Use the numbers as given. Routing through as.character() would turn
    # 1e5 into "1e+05" and fail the numeric pattern.
    result   <- .numeric_to_posixct(unname(as.numeric(date_vector)))
    original <- as.character(date_vector)
  } else {
    original <- as.character(date_vector)
    date_str <- trimws(original)
    date_str[!nzchar(date_str)] <- NA_character_

    # Real columns repeat the same handful of values; parse each distinct
    # string once and expand. Occurrence counts go with them so that
    # day/month inference votes on rows rather than on distinct strings —
    # one stray value should not outweigh a thousand consistent ones.
    u      <- unique(date_str)
    idx    <- match(date_str, u)
    result <- .parse_strings(u, dayfirst,
                             weights = tabulate(idx, nbins = length(u)),
                             quiet   = quiet)[idx]
  }

  failed <- is.na(result) & !is.na(original) & nzchar(trimws(original))
  result <- finish(result)

  if (any(failed)) {
    tbl <- .unparsed_table(original, failed)
    # Carried on the result even when quiet, so callers that suppress the
    # warning can still see exactly what was dropped.
    attr(result, "unparsed") <- tbl

    if (!quiet) {
      warning(sprintf(
        "parse_to_datetime(): %d of %d value(s) could not be parsed and became NA.\n%d distinct unparsed value(s):\n%s",
        sum(failed), n, nrow(tbl), .format_unparsed(tbl, report_max)
      ), call. = FALSE)
    }
  }

  result
}

#' Inspect the values a parse could not read
#'
#' Returns the record of unrecognised values that [parse_to_datetime()] leaves
#' on its result, so failures can be inspected or joined back to the source
#' data rather than only read off a warning. The record is attached whether or
#' not the warning was emitted, so it survives `quiet = TRUE`.
#'
#' @param x A vector returned by [parse_to_datetime()].
#'
#' @return A data frame with one row per distinct unparsed value and columns
#'   `value` (the trimmed input string), `count` (how many times it occurred)
#'   and `index` (a list column of its positions in the input). Zero rows if
#'   everything parsed.
#'
#' @examples
#' x <- suppressWarnings(parse_to_datetime(c("2024-01-15", "n/a", "later", "n/a")))
#' unparsed_values(x)
#'
#' # Which input rows failed
#' unlist(unparsed_values(x)$index)
#'
#' @export
unparsed_values <- function(x) {
  tbl <- attr(x, "unparsed")
  if (is.null(tbl)) .empty_unparsed() else tbl
}
