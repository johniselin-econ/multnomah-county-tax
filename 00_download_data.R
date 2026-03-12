# =============================================================================
# 00_download_data.R
# Download all external R-managed data inputs for the project.
#
# Run this before 00_multnomah.do.
# =============================================================================

.sourced_by_main <- TRUE
source(file.path("code", "R", "multnomah_r_common.R"))

cfg <- multnomah_r_init("00_download_data.R")
run_multnomah_data_pulls(cfg)
multnomah_r_finish("Next step: run 00_multnomah.do in Stata.")
