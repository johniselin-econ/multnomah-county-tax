# =============================================================================
# Shared R orchestration helpers for the Multnomah County tax-migration project
# =============================================================================

multnomah_r_init <- function(script_label = "R pipeline") {
  cat("=== ", script_label, " ===\n", sep = "")
  cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

  # Parameters (match 00_multnomah.do)
  start_year        <- 2012L
  end_year          <- 2024L
  overwrite_csv     <- FALSE
  auto_install_pkgs <- TRUE

  required_packages <- c(
    "here",
    "dplyr",
    "readr",
    "tidyr",
    "stringr",
    "ipumsr",
    "tidycensus",
    "sf",
    "tigris",
    "readxl",
    "patchwork",
    "cowplot",
    "ggplot2"
  )

  missing_packages <- required_packages[!vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )]

  if (length(missing_packages) > 0) {
    if (isTRUE(auto_install_pkgs)) {
      cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
      install.packages(missing_packages, repos = "https://cloud.r-project.org")
    } else {
      stop(
        "Missing required packages: ", paste(missing_packages, collapse = ", "), "\n",
        "  Install them manually or set auto_install_pkgs <- TRUE.",
        call. = FALSE
      )
    }
  }

  project_root <- here::here()
  setwd(project_root)
  cat("Project root:", project_root, "\n\n")

  dir_code_r     <- file.path(project_root, "code", "R")
  dir_data       <- file.path(project_root, "data")
  dir_data_acs   <- file.path(dir_data, "acs")
  api_codes_path <- file.path(project_root, "api_codes.txt")

  if (!file.exists(api_codes_path)) {
    stop(
      "api_codes.txt not found at: ", api_codes_path, "\n",
      "  Create this file with your IPUMS and Census API keys.\n",
      "  See README.md Section 'Setup > API Keys' for instructions.",
      call. = FALSE
    )
  }

  source(file.path(dir_code_r, "utils.R"))

  # Overleaf sync (optional) — mirrors profile.do convention
  overleaf   <- FALSE
  dir_ol_fig <- ""
  dir_ol_tab <- ""
  profile_r  <- file.path(project_root, "profile.R")
  if (file.exists(profile_r)) {
    source(profile_r, local = TRUE)
  }
  if (nzchar(Sys.getenv("OVERLEAF_PATH", ""))) {
    oth_path <- Sys.getenv("OVERLEAF_PATH")
  }
  if (exists("oth_path") && nzchar(oth_path)) {
    dir_ol_fig <- file.path(oth_path, "figures")
    dir_ol_tab <- file.path(oth_path, "tables")
    overleaf   <- TRUE
    cat("Overleaf sync ON:", oth_path, "\n\n")
  }

  list(
    project_root = project_root,
    dir_code_r = dir_code_r,
    dir_data = dir_data,
    dir_data_acs = dir_data_acs,
    api_codes_path = api_codes_path,
    start_year = start_year,
    end_year = end_year,
    overwrite_csv = overwrite_csv,
    overleaf = overleaf,
    dir_ol_fig = dir_ol_fig,
    dir_ol_tab = dir_ol_tab
  )
}

run_multnomah_data_pulls <- function(cfg) {
  cat("---- Data Pulls ----------------------------------------------------------\n\n")

  cat(">> ACS microdata (IPUMS)\n")
  source(file.path(cfg$dir_code_r, "api_code.R"))
  download_ipums_acs(
    project_root = cfg$project_root,
    dir_data_acs = cfg$dir_data_acs,
    api_codes_path = cfg$api_codes_path,
    start_year = cfg$start_year,
    end_year = cfg$end_year,
    overwrite_csv = cfg$overwrite_csv
  )
  cat("   Done.\n\n")

  cat(">> QWI data (LEHD)\n")
  source(file.path(cfg$dir_code_r, "qwi_data.R"))
  download_qwi(
    project_root = cfg$project_root,
    start_year = cfg$start_year,
    end_year = cfg$end_year,
    overwrite_csv = cfg$overwrite_csv
  )
  cat("   Done.\n\n")

  cat(">> QCEW data (BLS)\n")
  source(file.path(cfg$dir_code_r, "qcew_data.R"))
  download_qcew(
    project_root = cfg$project_root,
    start_year = cfg$start_year,
    end_year = cfg$end_year,
    overwrite_csv = cfg$overwrite_csv
  )
  cat("   Done.\n\n")

  cat(">> Census age shares (B01001)\n")
  age_shares_path <- file.path(cfg$dir_data, "working", "age_shares_county.csv")
  if (!file.exists(age_shares_path) || isTRUE(cfg$overwrite_csv)) {
    source(file.path(cfg$dir_code_r, "census_age_shares.R"))
  } else {
    cat("   Skipping (file exists). Set overwrite_csv=TRUE to re-download.\n")
  }
  cat("   Done.\n\n")

  cat(">> Public data (BLS LAUS, DOL childcare, county centroids)\n")
  source(file.path(cfg$dir_code_r, "download_public_data.R"))
  download_bls_laus(cfg$dir_data, overwrite = cfg$overwrite_csv)
  download_dol_childcare(cfg$dir_data, overwrite = cfg$overwrite_csv)
  download_county_centroids(cfg$dir_data, overwrite = cfg$overwrite_csv)
  cat("   Done.\n\n")

  cat(">> NHGIS demographics (IPUMS API)\n")
  nhgis_path <- file.path(
    cfg$dir_data,
    "demographic",
    "nhgis0031_csv",
    "nhgis0031_ts_nominal_county.csv"
  )

  if (!file.exists(nhgis_path) || isTRUE(cfg$overwrite_csv)) {
    source(file.path(cfg$dir_code_r, "download_nhgis.R"))
    download_nhgis_demographics(
      dir_data = cfg$dir_data,
      api_codes_path = cfg$api_codes_path,
      overwrite = cfg$overwrite_csv
    )
  } else {
    cat("   Skipping (file exists). Set overwrite_csv=TRUE to re-download.\n")
  }
  cat("   Done.\n\n")
}

run_multnomah_post_stata <- function(cfg) {
  cat("---- Post-Stata Figures --------------------------------------------------\n\n")

  cat(">> Conceptual diagrams\n")
  source(file.path(cfg$dir_code_r, "fig_diagrams.R"))
  cat("   Done.\n\n")

  cat(">> Maps\n")
  county_sample_path <- file.path(cfg$dir_data, "working", "acs_county_sample.xlsx")

  if (file.exists(county_sample_path)) {
    source(file.path(cfg$dir_code_r, "map_code.R"))
    cat("   Done.\n\n")
  } else {
    cat("   SKIPPED: acs_county_sample.xlsx not found.\n")
    cat("   Run 00_multnomah.do first, then re-run this script for maps.\n\n")
  }
}

multnomah_r_finish <- function(next_step = NULL) {
  cat("---- Complete ------------------------------------------------------------\n")
  cat("End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  if (!is.null(next_step) && nzchar(next_step)) {
    cat("\n", next_step, "\n", sep = "")
  }
}
