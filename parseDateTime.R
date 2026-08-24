library(lubridate)


parseDateTime <- function (x){
  # Identify numeric Unix timestamps
  is_unix <- grepl("^\\d+(\\.\\d+)?$", x)
  
  # Initialize an empty vector
  parsed <- as.POSIXct(rep(NA, length(x)), origin = "1970-01-01", tz = "UTC")
  
  # Parse Unix timestamps
  parsed[is_unix] <- as.POSIXct(as.numeric(x[is_unix]), origin = "1970-01-01", tz = "UTC")
  
  # Parse regular date-time strings
  parsed[!is_unix] <- parse_date_time(
    x[!is_unix],
    orders = c("Ymd HMS", "Ymd HM", "Yb d HM", "Yb d HMS"),
    exact = FALSE
  )
  
  return(parsed)
}


parse_to_date_utc <- function(date_vector, output_format = "%Y-%m-%d %H:%M:%S", timezone = "UTC") {
  
  n       <- length(date_vector)
  result  <- rep(as.POSIXct(NA), n)
  
  # ── Pre-flight ──────────────────────────────────────────────────────────────
  # Convert everything to character once (avoids repeated coercion in the loop)
  date_str <- as.character(date_vector)
  
  # Boolean mask of elements that still need a result
  pending <- !is.na(date_vector)
  
  # Check for lubridate once, outside any loop
  has_lubridate <- requireNamespace("lubridate", quietly = TRUE)
  
  formats <- c(
    "%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y", "%d/%m/%Y",
    "%m-%d-%Y", "%m/%d/%Y", "%d-%b-%Y", "%d %b %Y",
    "%b-%d-%Y", "%Y%m%d",   "%d.%m.%Y", "%Y.%m.%d"
  )
  
  # ── 1. Numeric (Unix / Excel serials) ───────────────────────────────────────
  # grepl() works on the whole vector at once
  num_mask <- pending & grepl("^[0-9.]+$", date_str)
  
  if (any(num_mask)) {
    num_vals  <- as.numeric(date_str[num_mask])
    unix_mask <- num_vals > 100000          # TRUE  → Unix timestamp
    # FALSE → Excel serial
    
    unix_pos  <- which(num_mask)[ unix_mask]
    excel_pos <- which(num_mask)[!unix_mask]
    
    if (length(unix_pos))
      result[unix_pos]  <- as.POSIXct(num_vals[ unix_mask],
                                      origin = "1970-01-01", tz = "UTC")
    if (length(excel_pos))
      result[excel_pos] <- as.POSIXct(as.Date(num_vals[!unix_mask],
                                              origin = "1899-12-30"), tz = "UTC")
    
    pending[num_mask] <- FALSE
  }
  
  # ── 2. ISO 8601  (2024-01-15T10:30:00+04:30, …) ────────────────────────────
  iso_mask <- pending & grepl("^\\d{4}-\\d{2}-\\d{2}T", date_str)
  
  if (any(iso_mask)) {
    iso_strs <- date_str[iso_mask]
    
    if (has_lubridate) {
      parsed <- suppressWarnings(lubridate::as_datetime(iso_strs))
    } else {
      clean <- sub("\\.\\d+", "",          iso_strs)   # strip sub-seconds
      clean <- sub("(:)(\\d{2})$", "\\2", clean)       # strip colon in offset
      parsed <- suppressWarnings(
        as.POSIXct(strptime(clean, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"))
      )
    }
    
    hit <- !is.na(parsed)
    result[which(iso_mask)[hit]] <- parsed[hit]
    pending[iso_mask] <- FALSE   # mark entire group as attempted
  }
  
  # ── 3. JS / toString()  (Mon Jan 15 2024 10:30:00 GMT+0100 (…)) ────────────
  js_pat  <- "^[A-Za-z]{3}\\s+[A-Za-z]{3}\\s+\\d{1,2}\\s+\\d{4}\\s+\\d{2}:\\d{2}:\\d{2}\\s+GMT"
  js_mask <- pending & grepl(js_pat, date_str)
  
  if (any(js_mask)) {
    clean <- sub("\\s*\\(.*\\)\\s*$", "",      date_str[js_mask])  # strip "(Timezone Name)"
    clean <- sub("GMT([+-]\\d{4})", "\\1", clean)                  # GMT+0100 → +0100
    parsed <- suppressWarnings(
      as.POSIXct(strptime(clean, format = "%a %b %d %Y %H:%M:%S %z", tz = "UTC"))
    )
    
    hit <- !is.na(parsed)
    result[which(js_mask)[hit]] <- parsed[hit]
    pending[js_mask] <- FALSE
  }
  
  # ── 4. Standard format trial loop ──────────────────────────────────────────
  # Key optimisation: each format is tried on ALL remaining candidates at once.
  # Elements that get a hit are removed from the candidate pool immediately,
  # so later formats only process what is genuinely unresolved.
  if (any(pending)) {
    cand_pos   <- which(pending)            # original indices of candidates
    cand_strs  <- date_str[cand_pos]       # their string values
    n_cand     <- length(cand_pos)
    resolved   <- rep(FALSE, n_cand)       # local resolved flag
    
    for (fmt in formats) {
      if (all(resolved)) break
      
      todo <- !resolved
      parsed <- suppressWarnings(
        as.POSIXct(strptime(cand_strs[todo], format = fmt, tz = "UTC"))
      )
      
      hit <- !is.na(parsed)
      if (any(hit)) {
        todo_pos          <- which(todo)
        result[cand_pos[todo_pos[hit]]] <- parsed[hit]
        resolved[todo_pos[hit]]         <- TRUE
      }
    }
  }
  
  # ── Return ──────────────────────────────────────────────────────────────────
  attr(result, "tzone") <- timezone
  result
}