# =============================================================================
# Author: John Iselin
# Purpose: Download publicly available data files (BLS LAUS, DOL childcare,
#          county centroids) that were previously manual downloads.
#
# Called by: 00_download_data.R
#
# Functions:
#   download_bls_laus(dir_data, overwrite = FALSE)
#   download_dol_childcare(dir_data, overwrite = FALSE)
#   download_county_centroids(dir_data, overwrite = FALSE)
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
})

# ---- BLS LAUS ----------------------------------------------------------------

#' Download BLS Local Area Unemployment Statistics (county-level)
#'
#' Downloads la.data.64.County from BLS bulk data server.
#'
#' @param dir_data Path to the project's data/ directory
#' @param overwrite If FALSE, skip download when file already exists
download_bls_laus <- function(dir_data, overwrite = FALSE) {

  dir_bls  <- file.path(dir_data, "demographic", "bls")
  dest     <- file.path(dir_bls, "la.data.64.County")

  if (file.exists(dest) && !isTRUE(overwrite)) {
    message("   Skipping BLS LAUS (file exists). Set overwrite=TRUE to re-download.")
    return(invisible(TRUE))
  }

  if (!dir.exists(dir_bls)) {
    dir.create(dir_bls, recursive = TRUE, showWarnings = FALSE)
  }

  url <- "https://download.bls.gov/pub/time.series/la/la.data.64.County"
  message("   Downloading BLS LAUS data ...")

  dl <- tryCatch(
    {
      download.file(url, dest, mode = "wb", quiet = TRUE,
                    headers = c("User-Agent" = "Mozilla/5.0 (research)"))
      0L
    },
    error = function(e) {
      message("   Download failed: ", conditionMessage(e))
      -1L
    }
  )

  if (dl != 0 || !file.exists(dest) || file.size(dest) < 1000) {
    unlink(dest)
    stop("Failed to download BLS LAUS data from:\n  ", url,
         "\n  Download manually and place at: ", dest, call. = FALSE)
  }

  message("   Saved: ", dest)
  invisible(TRUE)
}


# ---- DOL Childcare -----------------------------------------------------------

#' Download DOL National Database of Childcare Prices
#'
#' Downloads NDCP2022.xlsx from the DOL website.
#'
#' @param dir_data Path to the project's data/ directory
#' @param overwrite If FALSE, skip download when file already exists
download_dol_childcare <- function(dir_data, overwrite = FALSE) {

  dir_dol  <- file.path(dir_data, "demographic", "dol")
  dest     <- file.path(dir_dol, "NDCP2022.xlsx")

  if (file.exists(dest) && !isTRUE(overwrite)) {
    message("   Skipping DOL childcare (file exists). Set overwrite=TRUE to re-download.")
    return(invisible(TRUE))
  }

  if (!dir.exists(dir_dol)) {
    dir.create(dir_dol, recursive = TRUE, showWarnings = FALSE)
  }

  url <- "https://www.dol.gov/sites/dolgov/files/WB/NDCP2022.xlsx"
  message("   Downloading DOL childcare data ...")

  dl <- tryCatch(
    {
      download.file(url, dest, mode = "wb", quiet = TRUE)
      0L
    },
    error = function(e) {
      message("   Download failed: ", conditionMessage(e))
      -1L
    }
  )

  if (dl != 0 || !file.exists(dest) || file.size(dest) < 1000) {
    unlink(dest)
    stop("Failed to download DOL childcare data from:\n  ", url,
         "\n  Download manually and place at: ", dest, call. = FALSE)
  }

  message("   Saved: ", dest)
  invisible(TRUE)
}


# ---- County Centroids --------------------------------------------------------

#' Download Census Bureau county population centers
#'
#' Downloads CenPop2010_Mean_CO.txt from Census Bureau and reshapes to match
#' the existing PopCenterCounty_US.csv format expected by Stata.
#'
#' @param dir_data Path to the project's data/ directory
#' @param overwrite If FALSE, skip download when file already exists
download_county_centroids <- function(dir_data, overwrite = FALSE) {

  dest    <- file.path(dir_data, "demographic", "PopCenterCounty_US.csv")
  dir_dem <- dirname(dest)

  if (file.exists(dest) && !isTRUE(overwrite)) {
    message("   Skipping county centroids (file exists). Set overwrite=TRUE to re-download.")
    return(invisible(TRUE))
  }

  if (!dir.exists(dir_dem)) {
    dir.create(dir_dem, recursive = TRUE, showWarnings = FALSE)
  }

  url <- "https://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO.txt"
  message("   Downloading Census county centroids ...")

  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)

  dl <- tryCatch(
    {
      download.file(url, tmp, mode = "wb", quiet = TRUE)
      0L
    },
    error = function(e) {
      message("   Download failed: ", conditionMessage(e))
      -1L
    }
  )

  if (dl != 0 || !file.exists(tmp) || file.size(tmp) < 1000) {
    stop("Failed to download Census centroids from:\n  ", url,
         "\n  Download manually and place at: ", dest, call. = FALSE)
  }

  # Read Census file (columns: STATEFP, COUNTYFP, COUNAME, STNAME, POPULATION, LATITUDE, LONGITUDE)
  census <- read_csv(tmp, col_types = cols(.default = "c"), trim_ws = TRUE)

  # Build the 5-digit FIPS (Geographic Indentifier — note the typo matches the original)
  census$fips <- paste0(census$STATEFP, census$COUNTYFP)

  # Reshape to match existing PopCenterCounty_US.csv format
  # Stata only uses: year, geographicindentifier (as 'fips'), latitude, longitude
  # We write all columns the original ArcGIS Hub file had so the import works
  out <- data.frame(
    ObjectID                  = seq_len(nrow(census)),
    Year                      = 2010L,
    `Geographic Indentifier`  = as.integer(census$fips),
    `State FIPS Code`         = census$STATEFP,
    `County FIPS Code`        = census$COUNTYFP,
    `State Name`              = census$STNAME,
    `County Name`             = census$COUNAME,
    `Postal Code`             = "",
    Population                = as.integer(census$POPULATION),
    Latitude                  = as.numeric(census$LATITUDE),
    Longitude                 = as.numeric(census$LONGITUDE),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  write_csv(out, dest)
  message("   Saved: ", dest, " (", nrow(out), " counties)")
  invisible(TRUE)
}
