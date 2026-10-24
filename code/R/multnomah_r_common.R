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
  cat("Project root:", project_root, "\n\n")

  dir_code_r     <- file.path(project_root, "code", "R")
  dir_data       <- file.path(project_root, "data")
  dir_data_acs   <- file.path(dir_data, "acs")
  api_codes_path <- file.path(project_root, "api_codes.txt")
  # api_codes.txt is checked in run_multnomah_data_pulls(), the only stage
  # that uses it; the post-Stata stage (diagrams, maps) needs no API keys.

  source(file.path(dir_code_r, "utils.R"))

  # user_settings.R (gitignored; template user_settings.example.R) holds the
  # machine-specific settings: acs_source / acs_shared_root and, optionally,
  # the Overleaf folder. One loader (utils.R) serves this and the audit script.
  settings <- load_user_settings(project_root)

  # Overleaf sync (optional) — mirrors the Stata user_settings.do convention
  overleaf   <- FALSE
  dir_ol_fig <- ""
  dir_ol_tab <- ""
  oth_path   <- resolve_oth_path(settings)
  if (nzchar(oth_path)) {
    dir_ol_fig <- file.path(oth_path, "figures")
    dir_ol_tab <- file.path(oth_path, "tables")
    overleaf   <- TRUE
    cat("Overleaf sync ON:", oth_path, "\n\n")
  }

  # ---- ACS data source -------------------------------------------------------
  # "shared" (default): read the Budget Lab common IPUMS extract from the shared
  # drive. "local": download a fresh per-year extract via the IPUMS API (needs
  # api_codes.txt). Override either in user_settings.R (gitignored):
  #   acs_source      <- "local"
  #   acs_shared_root <- "/path/to/shared/raw_data/ACS/acs_common_v2"
  # The absolute shared path is kept out of the repo (set in user_settings.R).
  acs_source <- if (exists("acs_source", envir = settings, inherits = FALSE) &&
                    nzchar(settings$acs_source)) tolower(settings$acs_source) else "shared"
  acs_shared_root <- if (exists("acs_shared_root", envir = settings, inherits = FALSE)) {
    settings$acs_shared_root
  } else NULL
  cat("ACS source:", acs_source, "\n\n")

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
    dir_ol_tab = dir_ol_tab,
    acs_source = acs_source,
    acs_shared_root = acs_shared_root
  )
}

run_multnomah_data_pulls <- function(cfg) {
  cat("---- Data Pulls ----------------------------------------------------------\n\n")

  # Needed in both ACS modes: NHGIS (IPUMS key) and the Census age shares
  # (Census key) always download.
  if (!file.exists(cfg$api_codes_path)) {
    stop(
      "api_codes.txt not found at: ", cfg$api_codes_path, "\n",
      "  Create this file with your IPUMS and Census API keys.\n",
      "  See README.md Section 'Setup > API Keys' for instructions.",
      call. = FALSE
    )
  }

  cat(">> ACS microdata (IPUMS)\n")
  source(file.path(cfg$dir_code_r, "api_code.R"))
  download_ipums_acs(
    project_root = cfg$project_root,
    dir_data_acs = cfg$dir_data_acs,
    api_codes_path = cfg$api_codes_path,
    start_year = cfg$start_year,
    end_year = cfg$end_year,
    overwrite_csv = cfg$overwrite_csv,
    acs_source = cfg$acs_source,
    shared_acs_dir = cfg$acs_shared_root
  )
  cat("   Done.\n\n")

  cat(">> Census age shares (B01001)\n")
  age_shares_path <- file.path(cfg$dir_data, "working", "age_shares_county.csv")
  if (!file.exists(age_shares_path) || isTRUE(cfg$overwrite_csv)) {
    source(file.path(cfg$dir_code_r, "census_age_shares.R"))
    download_census_age_shares(
      project_root = cfg$project_root,
      api_codes_path = cfg$api_codes_path
    )
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
  # Inputs written by 02_descriptives.do (Stage 2). Figure 1 and the
  # directional flow maps (Figs 6, A13, A14) come from map_code.R, so a
  # missing input is a hard stop, not a skip -- the old skip exited 0 and
  # quietly left the paper's maps unrefreshed.
  map_inputs <- c(
    file.path(cfg$dir_data, "working", "acs_county_sample.xlsx"),
    file.path(cfg$dir_data, "working",
              paste0("multnomah_partner_flows_", c("n1", "n2", "agi"), ".csv"))
  )
  missing_inputs <- map_inputs[!file.exists(map_inputs)]
  if (length(missing_inputs) > 0) {
    stop("map_code.R inputs missing (run 00_multnomah.do / 02_descriptives.do first):\n  ",
         paste(missing_inputs, collapse = "\n  "), call. = FALSE)
  }
  source(file.path(cfg$dir_code_r, "map_code.R"))
  cat("   Done.\n\n")
}

multnomah_r_finish <- function(next_step = NULL) {
  cat("---- Complete ------------------------------------------------------------\n")
  cat("End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  if (!is.null(next_step) && nzchar(next_step)) {
    cat("\n", next_step, "\n", sep = "")
  }
}
