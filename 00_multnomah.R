# =============================================================================
# 00_multnomah.R
# Main R orchestrator for Multnomah County tax-migration project
#
# Author: John Iselin
# Date:   March 5, 2026
#
# Purpose: Runs all R scripts for this project. Execute this BEFORE running
#          00_multnomah.do (Stata). The pipeline is:
#
#   1. Data pulls    — ACS microdata, QWI, QCEW, Census age shares
#   2. Stata         — Run 00_multnomah.do separately after this script
#   3. R figures     — Maps and diagrams (run after Stata creates working data)
#
# Usage:
#   From the project root directory:
#     source("00_multnomah.R")
#   Or from the command line:
#     Rscript 00_multnomah.R
# =============================================================================

# ---- Setup ------------------------------------------------------------------

cat("=== 00_multnomah.R ===\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Set project root to the directory containing this script
project_root <- tryCatch(
  normalizePath(dirname(sys.frame(1)$ofile), winslash = "/"),
  error = function(e) normalizePath(".", winslash = "/")
)
setwd(project_root)

# Paths
dir_code_r     <- file.path(project_root, "code", "R")
dir_data       <- file.path(project_root, "data")
dir_data_acs   <- file.path(dir_data, "acs")
api_codes_path <- file.path(project_root, "api_codes.txt")

# Parameters (match 00_multnomah.do)
start_year     <- 2012L
end_year       <- 2024L
overwrite_csv  <- FALSE

# Flag to prevent auto-execution when sourcing sub-scripts
.sourced_by_main <- TRUE

# Source shared utilities
source(file.path(dir_code_r, "utils.R"))


# =============================================================================
# SECTION 1: DATA PULLS
# =============================================================================

cat("── Section 1: Data Pulls ──────────────────────────────────────────────\n\n")

# ---- 1a. ACS microdata via IPUMS --------------------------------------------

cat(">> 1a. ACS microdata (IPUMS)\n")
source(file.path(dir_code_r, "api_code.R"))
download_ipums_acs(
  project_root  = project_root,
  dir_data_acs  = dir_data_acs,
  api_codes_path = api_codes_path,
  start_year    = start_year,
  end_year      = end_year,
  overwrite_csv = overwrite_csv
)
cat("   Done.\n\n")


# ---- 1b. QWI data via LEHD bulk download ------------------------------------

cat(">> 1b. QWI data (LEHD)\n")
source(file.path(dir_code_r, "qwi_data.R"))
download_qwi(
  project_root  = project_root,
  start_year    = start_year,
  end_year      = end_year,
  overwrite_csv = overwrite_csv
)
cat("   Done.\n\n")


# ---- 1c. QCEW data via BLS -------------------------------------------------

cat(">> 1c. QCEW data (BLS)\n")
source(file.path(dir_code_r, "qcew_data.R"))
download_qcew(
  project_root  = project_root,
  start_year    = start_year,
  end_year      = end_year,
  overwrite_csv = overwrite_csv
)
cat("   Done.\n\n")


# ---- 1d. Census age shares via tidycensus -----------------------------------

cat(">> 1d. Census age shares (B01001)\n")
age_shares_path <- file.path(dir_data, "working", "age_shares_county.csv")

if (!file.exists(age_shares_path) || isTRUE(overwrite_csv)) {
  # Set variables expected by census_age_shares.R (script-based, not function-based)
  # project_root and api_codes_path are already set above
  source(file.path(dir_code_r, "census_age_shares.R"))
} else {
  cat("   Skipping (file exists). Set overwrite_csv=TRUE to re-download.\n")
}
cat("   Done.\n\n")


# =============================================================================
# SECTION 2: FIGURES (R-generated)
# These may depend on Stata outputs. Scripts with missing inputs skip gracefully.
# =============================================================================

cat("── Section 2: R Figures ────────────────────────────────────────────────\n\n")

# ---- 2a. Conceptual diagrams (no data dependencies) -------------------------

cat(">> 2a. Conceptual diagrams\n")
source(file.path(dir_code_r, "fig_diagrams.R"))
cat("   Done.\n\n")


# ---- 2b. Maps (requires Stata working data) ---------------------------------

cat(">> 2b. Maps\n")
county_sample_path <- file.path(dir_data, "working", "acs_county_sample.xlsx")

if (file.exists(county_sample_path)) {
  source(file.path(dir_code_r, "map_code.R"))
  cat("   Done.\n\n")
} else {
  cat("   SKIPPED: acs_county_sample.xlsx not found.\n")
  cat("   Run 00_multnomah.do first, then re-run this script for maps.\n\n")
}


# =============================================================================
# DONE
# =============================================================================

cat("── Complete ────────────────────────────────────────────────────────────\n")
cat("End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("\nNext step: run 00_multnomah.do in Stata.\n")
cat("Then re-run this script to generate maps (Section 2b) if skipped.\n")
