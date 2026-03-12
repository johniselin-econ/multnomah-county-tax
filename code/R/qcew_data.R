# =============================================================================
# Author: John Iselin
# Purpose: Download county-level Quarterly Census of Employment and Wages (QCEW)
#          data from BLS. Downloads one zipped CSV per year, filters to
#          county-level totals, and outputs quarterly files with employment,
#          wages, and establishment counts.
#
# Called by: 00_multnomah.do via rcall (or standalone)
#
# Data source: https://www.bls.gov/cew/
# Inputs:   BLS QCEW quarterly CSV single files (one zip per year)
# Outputs:  data/qcew/qcew_YYYY_QN.csv  (one file per quarter, all counties)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# ---- Configuration --------------------------------------------------------

# URL template for BLS QCEW annual single-file download
QCEW_URL_TEMPLATE <- "https://data.bls.gov/cew/data/files/%d/csv/%d_qtrly_singlefile.zip"

# agglvl_code 70 = County, Total Covered (all ownership, all industries)
QCEW_AGGLVL_COUNTY_TOTAL <- "70"

# Columns to keep in output
QCEW_OUTPUT_COLS <- c(
  "area_fips",       # 5-digit county FIPS
  "year",
  "qtr",
  "qtrly_estabs",    # Establishment count
  "month1_emplvl",   # Employment, 1st month of quarter
  "month2_emplvl",   # Employment, 2nd month of quarter
  "month3_emplvl",   # Employment, 3rd month of quarter
  "total_qtrly_wages",
  "avg_wkly_wage"
)

QCEW_NUMERIC_COLS <- c(
  "year", "qtr", "qtrly_estabs",
  "month1_emplvl", "month2_emplvl", "month3_emplvl",
  "total_qtrly_wages", "avg_wkly_wage"
)

# ---- Download helper -------------------------------------------------------

download_qcew_year <- function(yr, raw_dir, max_retries = 3) {
  url  <- sprintf(QCEW_URL_TEMPLATE, yr, yr)
  dest <- file.path(raw_dir, sprintf("%d_qtrly_singlefile.zip", yr))

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
    message("  Retry ", attempt, "/", max_retries, " (year=", yr, ")")
    Sys.sleep(2 * attempt)
  }

  warning("Failed after ", max_retries, " attempts: year=", yr)
  NULL
}

# ---- Process one year's zip file -------------------------------------------

process_qcew_chunk <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  df <- df %>%
    filter(agglvl_code == QCEW_AGGLVL_COUNTY_TOTAL)

  if (nrow(df) == 0) return(NULL)

  missing_cols <- setdiff(QCEW_OUTPUT_COLS, names(df))
  if (length(missing_cols) > 0) {
    keep_cols <- intersect(QCEW_OUTPUT_COLS, names(df))
  } else {
    keep_cols <- QCEW_OUTPUT_COLS
  }

  df <- df %>% select(all_of(keep_cols))

  for (col in intersect(QCEW_NUMERIC_COLS, names(df))) {
    df[[col]] <- as.numeric(df[[col]])
  }

  df
}

process_qcew_year <- function(zip_path, yr) {
  if (is.null(zip_path) || !file.exists(zip_path)) return(NULL)

  # List files inside the zip
  zip_contents <- tryCatch(
    unzip(zip_path, list = TRUE),
    error = function(e) {
      warning("Error listing zip contents for ", yr, ": ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(zip_contents) || nrow(zip_contents) == 0) return(NULL)

  # Find the CSV file inside the zip
  csv_name <- grep("\\.csv$", zip_contents$Name, value = TRUE, ignore.case = TRUE)
  if (length(csv_name) == 0) {
    warning("No CSV found in zip for year ", yr)
    return(NULL)
  }
  csv_name <- csv_name[1]

  # Extract to a temporary directory
  tmp_dir <- tempdir()
  tryCatch(
    unzip(zip_path, files = csv_name, exdir = tmp_dir, overwrite = TRUE),
    error = function(e) {
      warning("Error extracting zip for ", yr, ": ", conditionMessage(e))
      return(NULL)
    }
  )

  csv_path <- file.path(tmp_dir, csv_name)
  if (!file.exists(csv_path)) return(NULL)

  # Read CSV (all columns as character for safe parsing)
  df <- tryCatch(
    read_csv(csv_path, col_types = cols(.default = "c"), progress = FALSE),
    error = function(e) {
      warning("Error reading CSV for ", yr, ": ", conditionMessage(e))
      NULL
    }
  )

  # Clean up extracted file
  unlink(csv_path)

  if (is.null(df) || nrow(df) == 0) return(NULL)

  df <- process_qcew_chunk(df)
  if (is.null(df) || nrow(df) == 0) {
    message("  No county-level rows (agglvl_code=70) for year ", yr)
    return(NULL)
  }

  df
}

write_qcew_year_chunks <- function(zip_path, yr, dir_qcew, overwrite_csv = FALSE,
                                   chunk_size = 100000) {
  if (is.null(zip_path) || !file.exists(zip_path)) return(FALSE)

  zip_contents <- tryCatch(
    unzip(zip_path, list = TRUE),
    error = function(e) {
      warning("Error listing zip contents for ", yr, ": ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(zip_contents) || nrow(zip_contents) == 0) return(FALSE)

  csv_name <- grep("\\.csv$", zip_contents$Name, value = TRUE, ignore.case = TRUE)
  if (length(csv_name) == 0) {
    warning("No CSV found in zip for year ", yr)
    return(FALSE)
  }
  csv_name <- csv_name[1]

  tmp_dir <- tempdir()
  ok <- tryCatch(
    {
      unzip(zip_path, files = csv_name, exdir = tmp_dir, overwrite = TRUE)
      TRUE
    },
    error = function(e) {
      warning("Error extracting zip for ", yr, ": ", conditionMessage(e))
      FALSE
    }
  )
  if (!ok) return(FALSE)

  csv_path <- file.path(tmp_dir, csv_name)
  if (!file.exists(csv_path)) return(FALSE)

  quarter_files <- setNames(
    file.path(dir_qcew, sprintf("qcew_%d_Q%d.csv", yr, 1:4)),
    as.character(1:4)
  )
  should_write <- setNames(
    isTRUE(overwrite_csv) | !file.exists(unname(quarter_files)),
    names(quarter_files)
  )

  if (!any(should_write)) {
    for (q in names(quarter_files)) {
      message("  Skipping ", yr, "-Q", q, " (file exists)")
    }
    return(TRUE)
  }

  if (isTRUE(overwrite_csv)) {
    unlink(unname(quarter_files))
  }

  rows_written <- setNames(integer(4), names(quarter_files))
  header_written <- setNames(rep(FALSE, 4), names(quarter_files))

  cb <- SideEffectChunkCallback$new(function(x, pos) {
    chunk <- process_qcew_chunk(x)
    if (is.null(chunk) || nrow(chunk) == 0) return()

    for (q in names(quarter_files)) {
      if (!should_write[[q]]) next

      out_path <- quarter_files[[q]]
      q_chunk <- chunk %>% filter(qtr == as.numeric(q))
      if (nrow(q_chunk) == 0) next

      if (!header_written[[q]]) {
        write_csv(q_chunk, out_path, append = FALSE)
        header_written[[q]] <<- TRUE
      } else {
        write_csv(q_chunk, out_path, append = TRUE, col_names = FALSE)
      }
      rows_written[[q]] <<- rows_written[[q]] + nrow(q_chunk)
    }
  })

  on.exit(unlink(csv_path), add = TRUE)
  read_csv_chunked(
    csv_path,
    callback = cb,
    chunk_size = chunk_size,
    col_types = cols(.default = "c"),
    progress = FALSE
  )

  for (q in names(quarter_files)) {
    if (!should_write[[q]]) {
      message("  Skipping ", yr, "-Q", q, " (file exists)")
    } else if (rows_written[[q]] > 0) {
      message("  Saved: ", basename(quarter_files[[q]]),
              " (", format(rows_written[[q]], big.mark = ","), " rows)")
    } else {
      message("  No data for ", yr, "-Q", q)
    }
  }

  any(rows_written > 0)
}

# ---- Main download function -----------------------------------------------

download_qcew <- function(project_root,
                          start_year    = 2012,
                          end_year      = 2024,
                          overwrite_csv = FALSE) {

  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  dir_qcew <- file.path(project_root, "data", "qcew")
  dir_raw  <- file.path(dir_qcew, "raw")

  if (!dir.exists(dir_raw)) {
    dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
  }

  years <- seq.int(start_year, end_year)
  if (length(years) == 0) stop("start_year must be <= end_year", call. = FALSE)

  # Check which output files already exist
  needed <- expand.grid(y = years, q = 1:4, stringsAsFactors = FALSE)
  needed$path <- file.path(dir_qcew, sprintf("qcew_%d_Q%d.csv", needed$y, needed$q))
  needed$exists <- file.exists(needed$path)

  if (all(needed$exists) && !isTRUE(overwrite_csv)) {
    message("All QCEW output files exist. Use overwrite_csv=TRUE to re-download.")
    return(invisible(TRUE))
  }

  # --- Step 1: Download and process each year ---
  for (yr in years) {

    # Check if all quarters for this year already exist
    yr_files <- file.path(dir_qcew, sprintf("qcew_%d_Q%d.csv", yr, 1:4))
    if (all(file.exists(yr_files)) && !isTRUE(overwrite_csv)) {
      message("Skipping year ", yr, " (all quarter files exist)")
      next
    }

    message("Downloading QCEW for year ", yr, " ...")

    # If overwrite requested, remove cached zip to force re-download
    if (isTRUE(overwrite_csv)) {
      cached <- file.path(dir_raw, sprintf("%d_qtrly_singlefile.zip", yr))
      unlink(cached)
    }

    zip_path <- download_qcew_year(yr, dir_raw)
    if (is.null(zip_path)) {
      message("  Skipping year ", yr, " (download failed)")
      next
    }

    message("  Processing ...")
    wrote_any <- write_qcew_year_chunks(
      zip_path = zip_path,
      yr = yr,
      dir_qcew = dir_qcew,
      overwrite_csv = overwrite_csv
    )

    if (!wrote_any) {
      message("  No data for year ", yr)
    }
  }

  invisible(TRUE)
}

# ---- Entry point (when called standalone via Rscript) -----------------------

if (exists("project_root") && !exists(".sourced_by_main")) {
  sy <- if (exists("start_year"))    start_year    else 2012
  ey <- if (exists("end_year"))      end_year      else 2024
  ow <- if (exists("overwrite_csv")) overwrite_csv else FALSE

  download_qcew(
    project_root  = project_root,
    start_year    = sy,
    end_year      = ey,
    overwrite_csv = ow
  )
}
