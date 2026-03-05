# =============================================================================
# Author: John Iselin
# Purpose: Download NHGIS time-series demographics via IPUMS API, replicating
#          the manual extract #0031.
#
# Called by: 00_multnomah.R
#
# Requires: ipumsr (>= 0.7.0) with NHGIS API support
#           IPUMS API key in api_codes.txt
#
# Output:   data/demographic/nhgis0031_csv/nhgis0031_ts_nominal_county.csv
#           (same columns as the manual extract)
#
# Extract spec:
#   Time series tables: AV0 (total pop), D15 (urban/rural), B79 (median HH income)
#   Geographic level: county
#   Integration: nominal
#   All available years
# =============================================================================

suppressPackageStartupMessages({
  library(ipumsr)
  library(readr)
})

# ---- Shared API key utility ----
.this_dir_nhgis <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "code/R")
source(file.path(.this_dir_nhgis, "utils.R"))


#' Download NHGIS demographics (replicates extract #0031)
#'
#' Uses the IPUMS NHGIS API to define, submit, and download a time-series
#' extract matching the manually created extract #0031.
#'
#' @param dir_data Path to the project's data/ directory
#' @param api_codes_path Path to api_codes.txt (contains IPUMS API key)
#' @param overwrite If FALSE, skip download when output file already exists
download_nhgis_demographics <- function(dir_data, api_codes_path, overwrite = FALSE) {

  dir_nhgis <- file.path(dir_data, "demographic", "nhgis0031_csv")
  dest      <- file.path(dir_nhgis, "nhgis0031_ts_nominal_county.csv")

  if (file.exists(dest) && !isTRUE(overwrite)) {
    message("   Skipping NHGIS demographics (file exists). Set overwrite=TRUE to re-download.")
    return(invisible(TRUE))
  }

  if (!dir.exists(dir_nhgis)) {
    dir.create(dir_nhgis, recursive = TRUE, showWarnings = FALSE)
  }

  # Set IPUMS API key
  ipums_key <- read_api_key(api_codes_path, "ipums")
  set_ipums_api_key(ipums_key, save = TRUE, overwrite = TRUE)

  message("   Defining NHGIS extract ...")

  # Define extract matching #0031
  extract_def <- define_extract_nhgis(
    description = "County demographics (AV0, D15, B79) - nominal integration",
    time_series_tables = list(
      tst_spec("AV0", geog_levels = "county"),
      tst_spec("D15", geog_levels = "county"),
      tst_spec("B79", geog_levels = "county")
    ),
    tst_layout = "time_by_row",
    geographic_extensions = NULL
  )

  message("   Submitting NHGIS extract ...")
  submitted <- submit_extract(extract_def)

  message("   Waiting for extract (this may take several minutes) ...")
  ready <- wait_for_extract(submitted)

  message("   Downloading extract ...")
  tmp_dir <- tempdir()
  dl_path <- download_extract(ready, download_dir = tmp_dir)

  # Read the downloaded data
  nhgis_data <- read_nhgis(dl_path)

  # Expected columns (from codebook)
  expected_cols <- c("GISJOIN", "YEAR", "STATE", "STATEFP", "STATENH",
                     "COUNTY", "COUNTYFP", "COUNTYNH", "NAME",
                     "AV0AA", "D15AA", "D15AB", "B79AA",
                     "AV0AAM", "B79AAM")

  # Ensure column names match (ipumsr should produce these by default)
  actual_cols <- names(nhgis_data)
  missing <- setdiff(expected_cols, actual_cols)
  if (length(missing) > 0) {
    warning("NHGIS extract missing expected columns: ", paste(missing, collapse = ", "),
            "\n  Available columns: ", paste(actual_cols, collapse = ", "))
  }

  # Keep only expected columns (in order), adding NAs for any missing
  for (col in expected_cols) {
    if (!col %in% names(nhgis_data)) {
      nhgis_data[[col]] <- NA
    }
  }
  nhgis_data <- nhgis_data[, expected_cols]

  # Write CSV
  write_csv(nhgis_data, dest)
  message("   Saved: ", dest,
          " (", format(nrow(nhgis_data), big.mark = ","), " rows)")

  invisible(TRUE)
}
