# =============================================================================
# helpers.R
# Common utility functions for the Multnomah County Tax analysis
#
# Converted from: code/01_clean_data.do (programs section)
# Author: John Iselin
# =============================================================================

#' Make FIPS code from state and county FIPS codes
#'
#' Stata equivalent: make_fips state_fips county_fips, gen(fips)
#' Formula: fips = state_fips * 1000 + county_fips
#'
#' @param state_fips Numeric state FIPS code
#' @param county_fips Numeric county FIPS code
#' @return Numeric 5-digit FIPS code
make_fips <- function(state_fips, county_fips) {
  as.numeric(sprintf("%02d", state_fips)) * 1000 +
    as.numeric(sprintf("%03d", county_fips))
}

#' Extract state FIPS from combined FIPS code
#'
#' @param fips Numeric 5-digit FIPS code
#' @return Numeric state FIPS code
state_from_fips <- function(fips) {
  floor(fips / 1000)
}

#' Extract county FIPS from combined FIPS code
#'
#' @param fips Numeric 5-digit FIPS code
#' @return Numeric county FIPS code
county_from_fips <- function(fips) {
  fips %% 1000
}

#' Unsuppress: replace -1 values with 0
#'
#' Stata equivalent: unsuppress n1 n2 agi
#' IRS SOI uses -1 for suppressed values; treat as 0.
#'
#' @param x Numeric vector
#' @return Numeric vector with -1 replaced by 0
unsuppress <- function(x) {
  ifelse(x == -1, 0, x)
}

#' Download a file if it doesn't already exist locally
#'
#' @param url Remote URL to download from
#' @param destfile Local destination path
#' @param quiet Logical; suppress download messages
download_if_missing <- function(url, destfile, quiet = FALSE) {
  if (!file.exists(destfile)) {
    if (!quiet) message("Downloading: ", basename(destfile), " ...")
    tryCatch(
      download.file(url, destfile, mode = "wb", quiet = quiet),
      error = function(e) {
        warning("Failed to download ", url, ": ", conditionMessage(e))
      }
    )
  } else {
    if (!quiet) message("File exists, skipping: ", basename(destfile))
  }
}

#' Check that a required file exists; stop with informative error if missing
#'
#' @param filepath Path to required file
#' @param description Human-readable name for error messages
require_file <- function(filepath, description = NULL) {
  if (!file.exists(filepath)) {
    desc <- if (!is.null(description)) description else basename(filepath)
    stop("Required file not found: ", desc, "\n  Path: ", filepath, call. = FALSE)
  }
  invisible(TRUE)
}

#' Save data as both .rds and .csv
#'
#' Provides R-native format (.rds) for fast reloading and CSV for portability.
#'
#' @param df Data frame to save
#' @param path_stem File path without extension (e.g., "data/working/ids")
#' @param csv Logical; also write CSV (default TRUE)
save_data <- function(df, path_stem, csv = TRUE) {
  saveRDS(df, paste0(path_stem, ".rds"))
  if (csv) {
    readr::write_csv(df, paste0(path_stem, ".csv"))
  }
  invisible(df)
}

#' Load data from .rds (preferred) or .csv fallback
#'
#' @param path_stem File path without extension
#' @return Data frame
load_data <- function(path_stem) {
  rds_path <- paste0(path_stem, ".rds")
  csv_path <- paste0(path_stem, ".csv")

  if (file.exists(rds_path)) {
    return(readRDS(rds_path))
  } else if (file.exists(csv_path)) {
    return(readr::read_csv(csv_path, show_col_types = FALSE))
  } else {
    stop("Data file not found: ", path_stem, " (.rds or .csv)", call. = FALSE)
  }
}

#' Build ACS gross migration file from microdata
#'
#' Converts the Stata program `acs_make_gross_migration` to R.
#' Computes county-level in/out/net migration by mover type from
#' ACS person-level microdata with origin and destination counties.
#'
#' Move-type indices (mirrors IRS convention):
#'   1 = Non-movers (same county)
#'   2 = All movers (different county) = 4 + 5
#'   3 = Domestic movers (same as 2; foreign already dropped)
#'   4 = Within-state movers (different county, same state)
#'   5 = Inter-state movers (different state)
#'
#' @param micro Data frame of ACS microdata (from acs_migration_file)
#' @param ids Data frame with state_fips, county_fips, state_name, county_name
#' @param sample_expr Optional filter expression (e.g., hh_any_college == 1)
#' @return Data frame with county-year gross migration totals
acs_make_gross_migration <- function(micro, ids, sample_expr = NULL) {

  df <- micro

  # Apply optional sample filter
  if (!is.null(sample_expr)) {
    df <- dplyr::filter(df, !!rlang::parse_expr(sample_expr))
  }

  # Drop rows with missing key identifiers
  df <- df |>
    dplyr::filter(!is.na(year), !is.na(fips_o), !is.na(fips_d))

  # Treat missing income as 0
  df <- dplyr::mutate(df, inctot = ifelse(is.na(inctot), 0, inctot))

  # Weighted components at person level
  df <- df |>
    dplyr::mutate(
      persons_wt    = ifelse(hh_head == 1, hh_perwt, NA_real_),
      dollars_wt    = inctot * perwt,
      households_wt = ifelse(hh_head == 1, hhwt, 0)
    )

  # Collapse to origin-destination-year
  flows <- df |>
    dplyr::group_by(year, fips_o, fips_d) |>
    dplyr::summarize(
      persons    = sum(persons_wt, na.rm = TRUE),
      dollars    = sum(dollars_wt, na.rm = TRUE),
      households = sum(households_wt, na.rm = TRUE),
      .groups = "drop"
    )

  # Derive mover-type indicators
  flows <- flows |>
    dplyr::mutate(
      state_o = floor(fips_o / 1000),
      state_d = floor(fips_d / 1000),
      same_county        = (fips_o == fips_d),
      same_state         = (state_o == state_d),
      within_state_mover = same_state & !same_county,
      inter_state_mover  = !same_state
    )

  # ---- IN-MIGRATION (by destination county) ----
  in_mig <- flows |>
    dplyr::mutate(fips = fips_d) |>
    dplyr::mutate(
      dplyr::across(
        c(persons, households, dollars),
        list(
          `1` = ~ ifelse(same_county, ., 0),
          `4` = ~ ifelse(within_state_mover, ., 0),
          `5` = ~ ifelse(inter_state_mover, ., 0)
        ),
        .names = "{.col}_in_{.fn}"
      )
    ) |>
    dplyr::group_by(year, fips) |>
    dplyr::summarize(
      dplyr::across(dplyr::matches("_in_[145]$"), sum, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      persons_in_2    = persons_in_4 + persons_in_5,
      persons_in_3    = persons_in_2,
      households_in_2 = households_in_4 + households_in_5,
      households_in_3 = households_in_2,
      dollars_in_2    = dollars_in_4 + dollars_in_5,
      dollars_in_3    = dollars_in_2
    )

  # ---- OUT-MIGRATION (by origin county) ----
  out_mig <- flows |>
    dplyr::mutate(fips = fips_o) |>
    dplyr::mutate(
      dplyr::across(
        c(persons, households, dollars),
        list(
          `1` = ~ ifelse(same_county, ., 0),
          `4` = ~ ifelse(within_state_mover, ., 0),
          `5` = ~ ifelse(inter_state_mover, ., 0)
        ),
        .names = "{.col}_out_{.fn}"
      )
    ) |>
    dplyr::group_by(year, fips) |>
    dplyr::summarize(
      dplyr::across(dplyr::matches("_out_[145]$"), sum, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      persons_out_2    = persons_out_4 + persons_out_5,
      persons_out_3    = persons_out_2,
      households_out_2 = households_out_4 + households_out_5,
      households_out_3 = households_out_2,
      dollars_out_2    = dollars_out_4 + dollars_out_5,
      dollars_out_3    = dollars_out_2
    )

  # ---- MERGE in/out and compute net ----
  result <- dplyr::full_join(in_mig, out_mig, by = c("year", "fips"))

  # Replace NAs with 0
  num_cols <- names(result)[sapply(result, is.numeric) & names(result) != "year"]
  result <- result |>
    dplyr::mutate(dplyr::across(dplyr::all_of(num_cols), ~ tidyr::replace_na(., 0)))

  # Net migration (types 2-5)
  for (m in c("persons", "households", "dollars")) {
    for (t in c(2, 3, 4, 5)) {
      in_col  <- paste0(m, "_in_", t)
      out_col <- paste0(m, "_out_", t)
      net_col <- paste0(m, "_net_", t)
      result[[net_col]] <- result[[in_col]] - result[[out_col]]
    }
  }

  # Add state/county FIPS components
  result <- result |>
    dplyr::mutate(
      state_fips  = floor(fips / 1000),
      county_fips = fips %% 1000
    )

  # Merge county names
  result <- result |>
    dplyr::left_join(
      ids |> dplyr::select(state_fips, county_fips, state_name, county_name),
      by = c("state_fips", "county_fips")
    )

  # Organize
  result <- result |>
    dplyr::select(year, fips, state_fips, county_fips, state_name, county_name,
                  dplyr::everything()) |>
    dplyr::arrange(year, state_fips, county_fips)

  result
}
