# =============================================================================
# Author: John Iselin (refactor)
# Purpose: Download ACS microdata via IPUMS (year-by-year) and write per-year CSVs
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ipumsr)
  library(stringr)
})

# ---- Shared API key utility ----
.this_dir_api <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "code/R")
source(file.path(.this_dir_api, "utils.R"))

# ---- Main function ----
download_ipums_acs <- function(project_root,
                               dir_data_acs,
                               api_codes_path,
                               start_year = 2015,
                               end_year   = 2023,
                               overwrite_csv = FALSE,
                               overwrite_extract_files = TRUE,
                               extract_desc_prefix = "ACS microdata extract") {
  
  # Normalize paths (Windows-safe)
  project_root  <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  dir_data_acs  <- normalizePath(dir_data_acs, winslash = "/", mustWork = FALSE)
  api_codes_path <- normalizePath(api_codes_path, winslash = "/", mustWork = TRUE)
  
  if (!dir.exists(dir_data_acs)) {
    dir.create(dir_data_acs, recursive = TRUE, showWarnings = FALSE)
  }
  
  setwd(project_root)
  
  # IPUMS key setup
  ipums_key <- read_api_key(api_codes_path, "ipums")
  ipumsr::set_ipums_api_key(ipums_key, save = TRUE, overwrite = TRUE)
  
  # Years
  years <- seq.int(start_year, end_year)
  if (length(years) == 0) stop("start_year must be <= end_year", call. = FALSE)
  
  for (y in years) {
    
    file_acs <- file.path(dir_data_acs, paste0("acs_", y, ".csv"))
    
    if (!file.exists(file_acs) || isTRUE(overwrite_csv)) {
      
      message("Downloading ACS data for ", y, " via IPUMS...")
      
      extract_name <- paste0(extract_desc_prefix, ", Year: ", y)
      
      # Variable specs (as in your script)
      gq        <- var_spec("GQ", case_selections = c("1", "2"))
      workedyr  <- var_spec("WORKEDYR", data_quality_flags = TRUE)
      empstat   <- var_spec("EMPSTAT",  data_quality_flags = TRUE)
      empstatd  <- var_spec("EMPSTATD", data_quality_flags = TRUE)
      inctot <- var_spec("INCTOT", data_quality_flags = TRUE) 
      incwage <- var_spec("INCWAGE", data_quality_flags = TRUE) 
      incbus00 <- var_spec("INCBUS00", data_quality_flags = TRUE) 
      incearn <- var_spec("INCEARN", data_quality_flags = TRUE) 
      incinvst <- var_spec("INCINVST", data_quality_flags = TRUE) 
      incwelfr <- var_spec("INCWELFR", data_quality_flags = TRUE) 
      incsupp <- var_spec("INCSUPP", data_quality_flags = TRUE) 
      incother <-  var_spec("INCOTHER", data_quality_flags = TRUE)
      ftotinc   <- var_spec("FTOTINC",  data_quality_flags = TRUE)

      migrate1   <- var_spec("MIGRATE1",   data_quality_flags = FALSE)
      migrate1d  <- var_spec("MIGRATE1D",  data_quality_flags = TRUE)
      migplac1   <- var_spec("MIGPLAC1",   data_quality_flags = TRUE)
      migcounty1 <- var_spec("MIGCOUNTY1", data_quality_flags = FALSE)
      pwstate2   <- var_spec("PWSTATE2",   data_quality_flags = FALSE)
      pwcounty   <- var_spec("PWCOUNTY",   data_quality_flags = FALSE)
      
      rent       <- var_spec("RENT", data_quality_flags = TRUE)
      valueh     <- var_spec("VALUEH", data_quality_flags = TRUE)
      proptx99   <- var_spec("PROPTX99", data_quality_flags = TRUE)
      
      
      acs_data <- define_extract_micro(
        collection  = "usa",
        description = extract_name,
        samples     = paste0("us", y, "a"),
        variables   = list(
          "YEAR", "SAMPLE", "SERIAL", "HHWT", "PERWT", "CLUSTER", "CPI99",
          "STATEFIP", "COUNTYFIP",
          "PERNUM", "RELATED",
          "NCHILD", "YNGCH", "Nchlt5",  "SPLOC", 
          "AGE", "SEX", "RACE", "HISPAN", "MARST", "CITIZEN", "SCHOOL", "EDUCD",
          inctot, ftotinc, incwage, incearn, incbus00, incinvst, incwelfr, incsupp, incother,
          gq, workedyr, empstat, empstatd,
          migrate1, migrate1d, migplac1, migcounty1, pwcounty, pwstate2, 
          rent, valueh, proptx99
        )
      ) |>
        submit_extract() |>
        wait_for_extract() |>
        download_extract(download_dir = dir_data_acs, overwrite = overwrite_extract_files) |>
        read_ipums_micro() |>
        rename_with(tolower)
      
      # Write CSV
      utils::write.csv(acs_data, file_acs, row.names = FALSE)
      
      rm(acs_data)
      gc()
      
    } else {
      message("Skipping ", y, " (CSV exists and overwrite_csv=FALSE): ", file_acs)
    }
  }
  
  invisible(TRUE)
}

if (exists("project_root") && exists("dir_data_acs") && exists("api_codes_path") &&
    !exists(".sourced_by_main")) {
   download_ipums_acs(project_root, dir_data_acs, api_codes_path,
                      start_year = start_year, end_year = end_year,
                      overwrite_csv = overwrite_csv)
  }