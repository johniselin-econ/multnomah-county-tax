# =============================================================================
# globals.R
# Global parameters and paths for the Multnomah County Tax analysis
#
# Converted from: code/00_multnomah.do
# Author: John Iselin
# =============================================================================

# ---- Timezone fix (for WSL / misconfigured systems) -------------------------
if (Sys.getenv("TZ") == "") Sys.setenv(TZ = "America/New_York")

# ---- Project paths ----------------------------------------------------------
dir <- here::here()
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

# ---- Paper output flag -------------------------------------------------------
# If TRUE, copy paper figures/tables to Overleaf Dropbox directory
# If FALSE, store in results/paper/ within the project
overleaf_dir <- "C:/Users/ji252/Dropbox/Apps/Overleaf/Multnomah County"
overleaf <- dir.exists(overleaf_dir)

# Paper output directory (figures + tables for the paper)
results_paper <- if (overleaf) overleaf_dir else file.path(results_dir, "paper")
paper_figures <- file.path(results_paper, "figures")
paper_tables  <- file.path(results_paper, "tables")

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
  results_tables, results_maps, logs_dir,
  paper_figures, paper_tables
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

  # Stata-matched color palettes
  stata_navy         <- "#000080"
  stata_maroon       <- "#800000"
  stata_forest_green <- "#228B22"
  stata_dkorange     <- "#FF8C00"
  stata_purple       <- "#800080"
  stata_gs10         <- "#A1A1A1"

  # Age group colors and shapes
  age_colors <- c("25-44" = stata_maroon, "45-64" = stata_forest_green, "65+" = stata_dkorange)
  age_shapes <- c("25-44" = 16, "45-64" = 17, "65+" = 15)

  # Migration type colors and shapes
  migration_colors <- c(
    "Out-migration from Multnomah"   = stata_navy,
    "Out-of-state from Multnomah"    = stata_purple,
    "In-migration (West Coast)"      = stata_maroon,
    "In-migration (Lower 48 + DC)"   = stata_forest_green
  )
  migration_shapes <- c(
    "Out-migration from Multnomah"   = 16,
    "Out-of-state from Multnomah"    = 18,
    "In-migration (West Coast)"      = 17,
    "In-migration (Lower 48 + DC)"   = 15
  )

  # Flow analysis colors and shapes (with/without covariates)
  flow_colors <- c("With Covariates" = stata_navy, "Without Covariates" = stata_maroon)
  flow_shapes <- c("With Covariates" = 16, "Without Covariates" = 17)

  # Stata-style theme

  theme_stata <- function(base_size = 12) {
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        panel.background  = ggplot2::element_rect(fill = "white", color = NA),
        plot.background   = ggplot2::element_rect(fill = "white", color = NA),
        legend.position   = "bottom",
        legend.title      = ggplot2::element_blank()
      )
  }

  # Figure export dimensions
  fig_width  <- 10
  fig_height <- 6
  fig_dpi    <- 300

  ggplot2::theme_set(theme_stata())
}
