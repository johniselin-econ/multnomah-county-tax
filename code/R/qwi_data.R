# =============================================================================
# Author: John Iselin
# Purpose: Download county-level Quarterly Workforce Indicators (QWI) from the
#          LEHD bulk CSV files. Downloads one pre-built gzipped CSV per state
#          (51 downloads total), filters to county-level all-industry aggregates
#          by sex x education.
#
# Called by: 00_multnomah.do via rcall (or standalone)
#
# Data source: https://lehd.ces.census.gov/data/qwi/
# Inputs:   LEHD bulk CSV files (one gzipped CSV per state)
# Outputs:  data/qwi/qwi_YYYY_QN.csv  (one file per quarter, all states)
#
# Note: LEHD "op" files contain private-sector data only (ownercode A05).
#       This excludes federal/state/local government employment — a minor
#       difference for most county-level analyses.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# ---- Configuration --------------------------------------------------------

# URL template for LEHD bulk CSV download
# se=sex*education, f=no firm detail, gc=county, ns=NAICS sector,
# op=private ownership, u=unadjusted
LEHD_URL_TEMPLATE <- paste0(
  "https://lehd.ces.census.gov/data/qwi/latest_release/",
  "%s/qwi_%s_se_f_gc_ns_op_u.csv.gz"
)

# Variables to keep in output
QWI_VARS <- c(
  "Emp",        # Beginning-of-quarter employment
  "EmpEnd",     # End-of-quarter employment
  "EmpS",       # Full-quarter (stable) employment
  "EarnS",      # Full-quarter avg monthly earnings
  "EarnBeg",    # End-of-quarter avg monthly earnings
  "HirA",       # Hires all (accessions)
  "HirN",       # Hires new
  "Sep",        # Separations
  "FrmJbGn",    # Firm job gains (job creation)
  "FrmJbLs",    # Firm job loss (job destruction)
  "FrmJbC"      # Firm job change (net)
)

# State FIPS codes (50 states + DC)
STATE_FIPS <- c(
  "01", "02", "04", "05", "06", "08", "09", "10", "11", "12",
  "13", "15", "16", "17", "18", "19", "20", "21", "22", "23",
  "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
  "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
  "45", "46", "47", "48", "49", "50", "51", "53", "54", "55",
  "56"
)

# FIPS code -> lowercase state abbreviation (50 states + DC)
FIPS_TO_ABBR <- c(
  "01" = "al", "02" = "ak", "04" = "az", "05" = "ar", "06" = "ca",
  "08" = "co", "09" = "ct", "10" = "de", "11" = "dc", "12" = "fl",
  "13" = "ga", "15" = "hi", "16" = "id", "17" = "il", "18" = "in",
  "19" = "ia", "20" = "ks", "21" = "ky", "22" = "la", "23" = "me",
  "24" = "md", "25" = "ma", "26" = "mi", "27" = "mn", "28" = "ms",
  "29" = "mo", "30" = "mt", "31" = "ne", "32" = "nv", "33" = "nh",
  "34" = "nj", "35" = "nm", "36" = "ny", "37" = "nc", "38" = "nd",
  "39" = "oh", "40" = "ok", "41" = "or", "42" = "pa", "44" = "ri",
  "45" = "sc", "46" = "sd", "47" = "tn", "48" = "tx", "49" = "ut",
  "50" = "vt", "51" = "va", "53" = "wa", "54" = "wv", "55" = "wi",
  "56" = "wy"
)

# Education codes (SE endpoint)
# E0 = All, E1 = < HS, E2 = HS, E3 = Some college/AA, E4 = BA+
EDUCATION_CODES <- c("E0", "E1", "E2", "E3", "E4")

# ---- Download helper -------------------------------------------------------

download_lehd_state <- function(state_fips, raw_dir, max_retries = 3) {
  abbr <- FIPS_TO_ABBR[state_fips]
  if (is.na(abbr)) {
    warning("Unknown state FIPS: ", state_fips)
    return(NULL)
  }

  url  <- sprintf(LEHD_URL_TEMPLATE, abbr, abbr)
  dest <- file.path(raw_dir, sprintf("qwi_%s_se_f_gc_ns_op_u.csv.gz", abbr))

  # Skip if already cached
  if (file.exists(dest) && file.size(dest) > 0) {
    return(dest)
  }

  for (attempt in seq_len(max_retries)) {
    dl_result <- tryCatch(
      {
        download.file(url, dest, mode = "wb", quiet = TRUE, method = "curl")
        0L
      },
      error = function(e) {
        message("  Download error (attempt ", attempt, "): ", conditionMessage(e))
        -1L
      },
      warning = function(w) {
        message("  Download warning (attempt ", attempt, "): ", conditionMessage(w))
        -1L
      }
    )

    if (dl_result == 0 && file.exists(dest) && file.size(dest) > 0) {
      return(dest)
    }

    unlink(dest)
    message("  Retry ", attempt, "/", max_retries, " (state=", abbr, ")")
    Sys.sleep(2 * attempt)
  }

  warning("Failed after ", max_retries, " attempts: state=", state_fips)
  NULL
}

# ---- Process one state file ------------------------------------------------

process_state_file <- function(gz_path, start_year, end_year) {
  if (is.null(gz_path) || !file.exists(gz_path)) return(NULL)

  # Read the gzipped CSV (readr handles .gz transparently)
  df <- tryCatch(
    read_csv(gz_path, col_types = cols(.default = "c"), progress = FALSE),
    error = function(e) {
      warning("Error reading ", basename(gz_path), ": ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(df) || nrow(df) == 0) return(NULL)

  # Filter: county-level, all-industry aggregate, both sexes, education, year range
  # geo_level "C" = county (excludes state-level "S" rows also present in gc files)
  df <- df %>%
    filter(
      geo_level == "C",
      industry == "00",
      sex == "0",
      education %in% EDUCATION_CODES,
      as.integer(year) >= start_year,
      as.integer(year) <= end_year
    )

  if (nrow(df) == 0) return(NULL)

  # Build output columns to match existing CSV format
  df <- df %>%
    mutate(
      time      = paste0(year, "-Q", quarter),
      state_out = substr(geography, 1, 2),
      county    = substr(geography, 3, 5),
      fips      = as.numeric(geography)
    )

  # Convert indicator columns to numeric
  for (v in QWI_VARS) {
    if (v %in% names(df)) {
      df[[v]] <- as.numeric(df[[v]])
    }
  }

  # Select and rename to match expected output columns
  df %>%
    select(
      all_of(QWI_VARS),
      time, sex, education, industry, ownercode, seasonadj,
      state = state_out, county, fips
    )
}

# ---- Main download function -----------------------------------------------

download_qwi <- function(project_root,
                         api_codes_path = NULL,
                         states         = STATE_FIPS,
                         start_year     = 2012,
                         end_year       = 2024,
                         quarters       = 1:4,
                         education      = EDUCATION_CODES,
                         overwrite_csv  = FALSE) {

  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  dir_qwi <- file.path(project_root, "data", "qwi")
  dir_raw <- file.path(dir_qwi, "raw")

  if (!dir.exists(dir_raw)) {
    dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
  }

  years <- seq.int(start_year, end_year)
  if (length(years) == 0) stop("start_year must be <= end_year", call. = FALSE)

  # Check which output files already exist
  needed <- expand.grid(y = years, q = quarters, stringsAsFactors = FALSE)
  needed$path <- file.path(dir_qwi, sprintf("qwi_%d_Q%d.csv", needed$y, needed$q))
  needed$exists <- file.exists(needed$path)

  if (all(needed$exists) && !isTRUE(overwrite_csv)) {
    message("All QWI output files exist. Use overwrite_csv=TRUE to re-download.")
    return(invisible(TRUE))
  }

  # --- Step 1: Download raw bulk files for each state ---
  message("Downloading LEHD bulk QWI files (", length(states), " states) ...")

  raw_paths <- setNames(character(length(states)), states)
  for (st in states) {
    abbr <- FIPS_TO_ABBR[st]
    message("  State ", st, " (", abbr, ") ...")

    # If overwrite requested, remove cached raw file to force re-download
    if (isTRUE(overwrite_csv)) {
      cached <- file.path(dir_raw, sprintf("qwi_%s_se_f_gc_ns_op_u.csv.gz", abbr))
      unlink(cached)
    }

    path <- download_lehd_state(st, dir_raw)
    raw_paths[st] <- if (is.null(path)) "" else path
  }

  n_ok <- sum(raw_paths != "")
  message("  Downloaded/cached: ", n_ok, "/", length(states), " states")

  if (n_ok == 0) {
    warning("No LEHD files were downloaded. Check internet connection.")
    return(invisible(FALSE))
  }

  # --- Step 2: Determine which year-quarter files need writing ---
  files_to_write <- list()
  for (y in years) {
    for (q in quarters) {
      file_qwi <- file.path(dir_qwi, sprintf("qwi_%d_Q%d.csv", y, q))
      if (isTRUE(overwrite_csv) || !file.exists(file_qwi)) {
        key <- paste0(y, "-Q", q)
        files_to_write[[key]] <- file_qwi
      } else {
        message("  Skipping ", y, "-Q", q, " (file exists)")
      }
    }
  }

  if (length(files_to_write) == 0) {
    message("All target year-quarter files exist. Nothing to write.")
    return(invisible(TRUE))
  }

  # If overwriting, remove existing files so we start fresh
  if (isTRUE(overwrite_csv)) {
    for (fp in files_to_write) {
      unlink(fp)
    }
  }

  # --- Step 3: Process each state and append to output CSVs incrementally ---
  message("Processing bulk files ...")

  total_rows   <- 0L
  states_ok    <- 0L
  oc_vals_seen <- character(0)
  header_written <- character(0)  # track which files have headers

  for (st in states) {
    if (raw_paths[st] == "") next

    abbr <- FIPS_TO_ABBR[st]
    message("  Processing ", st, " (", abbr, ") ...")

    df <- process_state_file(raw_paths[st], start_year, end_year)
    if (is.null(df) || nrow(df) == 0) next

    states_ok <- states_ok + 1L

    # Track ownercode values for log message
    if ("ownercode" %in% names(df)) {
      oc_vals_seen <- unique(c(oc_vals_seen, unique(df$ownercode)))
    }

    # Parse year/quarter and split this state's data by year-quarter
    df <- df %>%
      mutate(
        .yr = as.integer(sub("-Q\\d$", "", time)),
        .qt = as.integer(sub("^\\d{4}-Q", "", time))
      )

    for (key in names(files_to_write)) {
      y <- as.integer(sub("-Q\\d$", "", key))
      q <- as.integer(sub("^\\d{4}-Q", "", key))

      chunk <- df %>%
        filter(.yr == y, .qt == q) %>%
        select(-starts_with("."))

      if (nrow(chunk) == 0) next

      fp <- files_to_write[[key]]
      needs_header <- !(key %in% header_written)

      # Write header row on first state, append-only for subsequent states
      if (needs_header) {
        write_csv(chunk, fp, append = FALSE)
        header_written <- c(header_written, key)
      } else {
        write_csv(chunk, fp, append = TRUE, col_names = FALSE)
      }

      total_rows <- total_rows + nrow(chunk)
    }
  }

  if (states_ok == 0L) {
    warning("No data after processing. Check year range and file contents.")
    return(invisible(FALSE))
  }

  # Log ownership code difference
  if (length(oc_vals_seen) > 0 && !"A00" %in% oc_vals_seen) {
    message("NOTE: LEHD bulk files use ownercode ", paste(oc_vals_seen, collapse = "/"),
            " (private-sector only, excludes government).")
  }

  message("  Total rows written: ", format(total_rows, big.mark = ","))

  # Log per-file row counts
  message("Output CSVs:")
  for (key in names(files_to_write)) {
    fp <- files_to_write[[key]]
    if (file.exists(fp)) {
      # count lines minus header
      n_lines <- length(readLines(fp, warn = FALSE)) - 1L
      message("  ", basename(fp), ": ", format(n_lines, big.mark = ","), " rows")
    } else {
      message("  ", key, ": no data")
    }
  }

  invisible(TRUE)
}

# ---- Entry point (when called standalone via Rscript) -----------------------

if (exists("project_root") && !exists(".sourced_by_main")) {
  sy <- if (exists("start_year"))    start_year    else 2012
  ey <- if (exists("end_year"))      end_year      else 2024
  ow <- if (exists("overwrite_csv")) overwrite_csv else FALSE
  st <- if (exists("qwi_states"))    qwi_states    else STATE_FIPS

  download_qwi(
    project_root   = project_root,
    states         = st,
    start_year     = sy,
    end_year       = ey,
    overwrite_csv  = ow
  )
}
