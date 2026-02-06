# =============================================================================
# globals.R
# Global parameters and paths for the Multnomah County Tax analysis
#
# Converted from: code/00_multnomah.do
# Author: John Iselin
# =============================================================================

# ---- Project paths ----------------------------------------------------------
dir <- "C:/Users/ji252/Documents/GitHub/multnomah-county-tax/"
code_dir    <- file.path(dir, "code")
data_dir    <- file.path(dir, "data")
results_dir <- file.path(dir, "results")
logs_dir    <- file.path(code_dir, "logs")
r_dir       <- file.path(dir, "R")

# Subdirectories
data_working     <- file.path(data_dir, "working")
data_acs         <- file.path(data_dir, "acs")
data_irs         <- file.path(data_dir, "irs")
data_demographic <- file.path(data_dir, "demographic")
data_covid       <- file.path(data_dir, "covid")

results_sdid     <- file.path(results_dir, "sdid")
results_did      <- file.path(results_dir, "did")
results_flows    <- file.path(results_dir, "flows")
results_indiv    <- file.path(results_dir, "individual")
results_tables   <- file.path(results_dir, "tables")
results_maps     <- file.path(results_dir, "maps")

# ---- Project parameters -----------------------------------------------------
pr_name <- "multnomah"
run_date <- format(Sys.Date(), "%Y-%m-%d")

# Random seed (matches Stata: set seed 56403)
set.seed(56403)

# ACS year range
start_year_acs <- 2015
end_year_acs   <- 2024

# SDID bootstrap replications
sdid_reps <- 100

# Parallel processing flag (1 = parallel, 0 = sequential)
use_parallel <- TRUE

# Debug mode: set TRUE to run all analysis on reduced samples for faster iteration
debug <- FALSE

# Multnomah County identifiers
multnomah_state_fips  <- 41
multnomah_county_fips <- 51
multnomah_fips        <- 41051

# Treatment year (tax effective 2021)
treatment_year <- 2020

# Neighboring counties for descriptive analysis
# Oregon: Multnomah (41051), Washington (41067), Clackamas (41005),
#          Marion (41047), Yamhill (41071), Columbia (41009)
# Washington: Clark (53011), Skamania (53059)
or_neighbor_fips <- c(41051, 41067, 41005, 41047, 41071, 41009)
wa_neighbor_fips <- c(53011, 53059)

# ---- Ensure output directories exist ----------------------------------------
dirs_to_create <- c(
  data_working, data_acs, data_irs, data_demographic, data_covid,
  file.path(data_demographic, "CAINC1"),
  file.path(data_demographic, "nhgis0031_csv"),
  file.path(data_demographic, "dol"),
  file.path(data_demographic, "bls"),
  results_sdid, results_did, results_flows, results_indiv,
  results_tables, results_maps, logs_dir
)

for (d in dirs_to_create) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ---- Label definitions (Stata label equivalents) ----------------------------
lb_move_type <- c(
  "0" = "ERROR",
  "1" = "Non-movers",
  "2" = "All movers",
  "3" = "Domestic movers",
  "4" = "Within-state movers",
  "5" = "Inter-state movers",
  "6" = "Foreign movers"
)

lb_agi <- c(
  "1" = "Under $1",
  "2" = "$1 under $10K",
  "3" = "$10K under $25K",
  "4" = "$25K under $50K",
  "5" = "$50K under $75K",
  "6" = "$75K under $100K",
  "7" = "$100K under $200K",
  "8" = "$200K or more"
)

# ---- ggplot2 theme defaults -------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::theme_set(ggplot2::theme_minimal(base_size = 12))
}
