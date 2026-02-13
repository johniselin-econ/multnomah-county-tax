# =============================================================================
# 00_master.R
# Master orchestration script for Multnomah County Tax analysis (R version)
#
# Converted from: code/00_multnomah.do
# Author: John Iselin
#
# Purpose: Runs the full analysis pipeline on the effect of tax changes
#          on migration in Multnomah County, Oregon.
#
# Usage:
#   source("R/00_multnomah.R")
#
# Required R packages:
#   dplyr, tidyr, readr, stringr, purrr, ggplot2, patchwork,
#   fixest, synthdid, openxlsx, marginaleffects, modelsummary, scales
#
# Pipeline:
#   Step 0: Load globals and helpers
#   Step 1: IPUMS API data download (optional)
#   Step 2: Data cleaning (01_clean_data.R)
#   Step 3: Descriptive analysis (02_descriptives.R)
#   Step 4: Flow-based analysis (02_flow_analysis.R)
#   Step 5: SDID analysis (02_sdid_analysis.R)
#   Step 6: Individual-level analysis (02_indiv_analysis.R)
#   Step 7: DiD analysis (02_did_analysis.R)
#   Step 8: Map generation (02_maps.R)
#   Step 9: Paper output (03_paper_output.R)
#   Step 10: Revenue analysis (02_revenue.R)
# =============================================================================

cat("==============================================================\n")
cat("  Multnomah County Tax Migration Analysis\n")
cat("  R Pipeline\n")
cat("  Run date:", format(Sys.Date(), "%Y-%m-%d"), "\n")
cat("==============================================================\n\n")

# =============================================================================
# STEP 0: LOAD GLOBALS AND HELPERS
# =============================================================================

source(here::here("R", "utils", "globals.R"))
source(here::here("R", "utils", "helpers.R"))

# Debug mode: set TRUE to run all analysis on reduced samples
debug <- TRUE

# Parallel processing: set TRUE to use future/future.apply for SDID and flows
use_parallel <- FALSE

message("Globals and helpers loaded.")
message("  Project dir:  ", dir)
message("  Data dir:     ", data_dir)
message("  Results dir:  ", results_dir)

# =============================================================================
# STEP 1: IPUMS API DATA DOWNLOAD (optional)
#
# This calls the existing R/api_code.R script to download ACS microdata
# from IPUMS USA. Set run_api <- TRUE to execute.
# Requires: api_codes.txt in the project root with your IPUMS API key.
# =============================================================================

run_api <- FALSE

if (run_api) {
  message("\n--- Step 1: IPUMS API data download ---")
  api_script <- file.path(dir, "code", "R", "api_code.R")
  if (file.exists(api_script)) {
    # Set variables expected by api_code.R
    project_root   <- dir
    dir_data_acs   <- data_acs
    api_codes_path <- file.path(dir, "api_codes.txt")
    start_year     <- start_year_acs
    end_year       <- end_year_acs
    overwrite_csv  <- FALSE

    source(api_script)
    message("  IPUMS API download complete.")
  } else {
    warning("  api_code.R not found at: ", api_script)
  }
} else {
  message("\nStep 1: IPUMS API download skipped (run_api = FALSE)")
}

# =============================================================================
# STEP 2: DATA CLEANING
# =============================================================================

run_clean <- TRUE

if (run_clean) {
  message("\n--- Step 2: Data cleaning ---")
  source(here::here("R", "01_clean_data.R"))
} else {
  message("\nStep 2: Data cleaning skipped (run_clean = FALSE)")
}

# =============================================================================
# STEP 3: DESCRIPTIVE ANALYSIS
# =============================================================================

run_descriptives <- TRUE

if (run_descriptives) {
  message("\n--- Step 3: Descriptive analysis ---")
  source(here::here("R", "02_descriptives.R"))
} else {
  message("\nStep 3: Descriptives skipped (run_descriptives = FALSE)")
}

# =============================================================================
# STEP 4: FLOW-BASED ANALYSIS (IRS)
# =============================================================================

run_flows <- TRUE

if (run_flows) {
  message("\n--- Step 4: Flow-based analysis ---")
  source(here::here("R", "02_flow_analysis.R"))
} else {
  message("\nStep 4: Flow analysis skipped (run_flows = FALSE)")
}

# =============================================================================
# STEP 5: SDID ANALYSIS
# =============================================================================

run_sdid <- TRUE

if (run_sdid) {
  message("\n--- Step 5: Synthetic Difference-in-Differences ---")
  source(here::here("R", "02_sdid_analysis.R"))
} else {
  message("\nStep 5: SDID analysis skipped (run_sdid = FALSE)")
}

# =============================================================================
# STEP 6: INDIVIDUAL-LEVEL ANALYSIS (ACS)
# =============================================================================

run_indiv <- TRUE

if (run_indiv) {
  message("\n--- Step 6: Individual-level analysis ---")
  source(here::here("R", "02_indiv_analysis.R"))
} else {
  message("\nStep 6: Individual analysis skipped (run_indiv = FALSE)")
}

# =============================================================================
# STEP 7: DIFFERENCE-IN-DIFFERENCES (ACS)
# =============================================================================

run_did <- TRUE

if (run_did) {
  message("\n--- Step 7: Difference-in-Differences analysis ---")
  source(here::here("R", "02_did_analysis.R"))
} else {
  message("\nStep 7: DiD analysis skipped (run_did = FALSE)")
}

# =============================================================================
# STEP 8: MAP GENERATION
# =============================================================================

run_maps <- TRUE

if (run_maps) {
  message("\n--- Step 8: Map generation ---")
  source(here::here("R", "02_maps.R"))
} else {
  message("\nStep 8: Map generation skipped (run_maps = FALSE)")
}

# =============================================================================
# STEP 9: PAPER OUTPUT
# =============================================================================

run_paper <- TRUE

if (run_paper) {
  message("\n--- Step 9: Paper output ---")
  source(here::here("R", "03_paper_output.R"))
} else {
  message("\nStep 9: Paper output skipped (run_paper = FALSE)")
}

# =============================================================================
# STEP 10: REVENUE ANALYSIS
# =============================================================================

run_revenue <- TRUE

if (run_revenue) {
  message("\n--- Step 10: Revenue analysis ---")
  source(here::here("R", "02_revenue.R"))
} else {
  message("\nStep 10: Revenue analysis skipped (run_revenue = FALSE)")
}

# =============================================================================
# DONE
# =============================================================================

cat("\n==============================================================\n")
cat("  Pipeline complete.\n")
cat("  Results saved to: ", results_dir, "\n")
cat("==============================================================\n")
