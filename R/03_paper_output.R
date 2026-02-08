# =============================================================================
# 03_paper_output.R
# Copy and organize analysis outputs for the paper
#
# Author: John Iselin
#
# Purpose: Collects figures and tables from various results/ subdirectories
#          and copies them to the paper output directory. If overleaf == TRUE,
#          files go to the Overleaf Dropbox directory; otherwise to results/paper/.
#
# Depends on: R/utils/globals.R (for paths and overleaf flag)
# =============================================================================

source(here::here("R", "utils", "globals.R"))

library(ggplot2)
library(grid)

message("=== Paper Output ===")
message("  overleaf = ", overleaf)
message("  Output dir: ", results_paper)
message("  Figures: ", paper_figures)
message("  Tables:  ", paper_tables)

# =============================================================================
# HELPER: safe file copy with message
# =============================================================================

copy_file <- function(from, to, label = NULL) {
  if (!file.exists(from)) {
    warning("  MISSING: ", from, if (!is.null(label)) paste0(" (", label, ")"))
    return(invisible(FALSE))
  }
  ok <- file.copy(from, to, overwrite = TRUE)
  if (ok) {
    message("  Copied: ", if (!is.null(label)) label else basename(from))
  } else {
    warning("  FAILED to copy: ", from)
  }
  invisible(ok)
}

# =============================================================================
# 1. SDID SPECIFICATION CURVES (Figure 4)
# =============================================================================

message("\n--- Copying SDID specification curves ---")

for (unit in c("agi", "n1", "n2")) {
  for (rate in c("in", "out", "net")) {
    fname <- paste0("fig_speccurve_", unit, "_", rate, ".pdf")
    copy_file(
      file.path(results_sdid, fname),
      file.path(paper_figures, fname),
      label = fname
    )
  }
}

# =============================================================================
# 1b. SDID EVENT STUDIES (Figure 5) -- 4 baseline specs x 3 rates
# =============================================================================

message("\n--- Copying SDID event studies ---")

# IRS full, with covariates (_1_), excl 2020, all donors and urban-covid donors
for (rate in c("in", "out", "net")) {
  for (sample in c("all", "urban95_covid")) {
    fname <- paste0("fig_irs_full_16_22_agi_", rate, "_rate_irs_1_sample_",
                    sample, "_excl2020_eventstudy.jpg")
    copy_file(
      file.path(results_sdid, "irs_full_16_22", fname),
      file.path(paper_figures, fname),
      label = fname
    )
  }
}

# ACS college 16-24, with covariates (_1_), excl 2020, all and urban-covid
for (rate in c("in", "out", "net")) {
  for (sample in c("all", "urban95_covid")) {
    fname <- paste0("fig_acs_16_24_col_agi_", rate, "_rate_acs2_1_sample_",
                    sample, "_excl2020_eventstudy.jpg")
    copy_file(
      file.path(results_sdid, "acs_16_24_col", fname),
      file.path(paper_figures, fname),
      label = fname
    )
  }
}

# =============================================================================
# 2. MAPS (Figures 1, 6a, 6b, Appendix)
# =============================================================================

message("\n--- Copying maps ---")

# Figure 1: Study area map
copy_file(
  file.path(results_maps, "map_combined.png"),
  file.path(paper_figures, "map_combined.png"),
  label = "Figure 1: Study area map"
)

# Figure 6a/6b: Directional AGI maps (west coast)
for (dir_type in c("out", "in")) {
  fname <- paste0("map_directional_agi_", dir_type, "_westcoast.png")
  copy_file(
    file.path(results_maps, fname),
    file.path(paper_figures, fname),
    label = fname
  )
}

# Appendix: Directional AGI maps (full US)
for (dir_type in c("out", "in")) {
  fname <- paste0("map_directional_agi_", dir_type, "_us.png")
  copy_file(
    file.path(results_maps, fname),
    file.path(paper_figures, fname),
    label = fname
  )
}

# Appendix: Directional maps for returns (n1) and exemptions (n2)
for (unit in c("n1", "n2")) {
  for (dir_type in c("out", "in")) {
    for (geo in c("us", "westcoast")) {
      fname <- paste0("map_directional_", unit, "_", dir_type, "_", geo, ".png")
      copy_file(
        file.path(results_maps, fname),
        file.path(paper_figures, fname),
        label = fname
      )
    }
  }
}

# =============================================================================
# 3. PPML FLOW EVENT STUDIES (Figure 7)
# =============================================================================

message("\n--- Copying PPML flow event studies ---")

for (dir_type in c("out", "in")) {
  for (suffix in c("", "_acs")) {
    fname <- paste0("fig_multnomah_", dir_type, "_agi", suffix, ".png")
    copy_file(
      file.path(results_flows, fname),
      file.path(paper_figures, fname),
      label = fname
    )
  }
}

# =============================================================================
# 4. PLACEBO DISTRIBUTIONS (Figure 8)
# =============================================================================

message("\n--- Copying placebo distributions ---")

for (dir_type in c("out", "in")) {
  for (stat in c("b", "t")) {
    fname <- paste0("fig_hist_", dir_type, "_", stat, ".png")
    copy_file(
      file.path(results_flows, fname),
      file.path(paper_figures, fname),
      label = fname
    )
  }
}

# =============================================================================
# 5. CONDITIONAL MEAN PLOTS (Figure 9, Appendix)
# =============================================================================

message("\n--- Copying conditional mean plots ---")

# All individual-level conditional mean figures
indiv_cats <- c("ftotinc", "educ", "yngch", "age", "married", "sex", "child")
for (cat in indiv_cats) {
  for (panel in c("1", "2")) {
    fname <- paste0("fig_cat_", cat, "_", panel, ".pdf")
    copy_file(
      file.path(results_indiv, fname),
      file.path(paper_figures, fname),
      label = fname
    )
  }
}

# =============================================================================
# 6. DID EVENT STUDIES (Figures 10, 11)
# =============================================================================

message("\n--- Copying DiD event studies ---")

did_figs <- c(
  # Out-migration (Figure 10)
  "fig_es_out_migration.png",
  "fig_es_out_state_migration.png",
  "fig_es_age_out_migration.png",
  "fig_es_age_out_state_migration.png",
  # In-migration (Figure 11)
  "fig_es_in_migration_48.png",
  "fig_es_in_migration_west.png",
  "fig_es_age_in_migration_48.png",
  "fig_es_age_in_migration_west.png",
  # Age x post versions
  "fig_es_agepost_out_migration.png",
  "fig_es_agepost_out_state_migration.png",
  "fig_es_agepost_in_migration_48.png",
  "fig_es_agepost_in_migration_west.png",
  # Combined
  "fig_es_combined.png"
)

for (fname in did_figs) {
  copy_file(
    file.path(results_did, fname),
    file.path(paper_figures, fname),
    label = fname
  )
}

# =============================================================================
# 7. DID TABLES
# =============================================================================

message("\n--- Copying DiD tables ---")

did_tabs <- c(
  "tab_did_overall.tex",
  "tab_did_by_age.tex",
  "tab_did_by_age_post.tex"
)

for (fname in did_tabs) {
  copy_file(
    file.path(results_did, fname),
    file.path(paper_tables, fname),
    label = fname
  )
}

# =============================================================================
# 8. SDID K-MEANS FIGURE
# =============================================================================

message("\n--- Copying SDID k-means figure ---")
copy_file(
  file.path(results_sdid, "fig_kmeans.jpg"),
  file.path(paper_figures, "fig_kmeans.jpg"),
  label = "K-means clustering"
)

# =============================================================================
# 9. GENERATE CONCEPTUAL DIAGRAMS (Figures 2 & 3)
# =============================================================================

message("\n--- Generating conceptual diagrams ---")
source(here::here("R", "utils", "fig_diagrams.R"))

# =============================================================================
# SUMMARY
# =============================================================================

n_figures <- length(list.files(paper_figures, pattern = "\\.(png|pdf|jpg)$"))
n_tables  <- length(list.files(paper_tables, pattern = "\\.(tex|xlsx)$"))

message("\n=== Paper Output Complete ===")
message("  Figures: ", n_figures, " files in ", paper_figures)
message("  Tables:  ", n_tables, " files in ", paper_tables)
