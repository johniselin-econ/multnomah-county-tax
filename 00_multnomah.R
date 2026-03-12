# =============================================================================
# 00_multnomah.R
# Backward-compatible wrapper for the split R pipeline
#
# Preferred usage:
#   source("00_download_data.R")
#   ... run 00_multnomah.do in Stata ...
#   source("00_post_stata.R")
# =============================================================================

.sourced_by_main <- TRUE
source(file.path("code", "R", "multnomah_r_common.R"))

cfg <- multnomah_r_init("00_multnomah.R")

cat("This wrapper runs both R stages in sequence.\n")
cat("For the split workflow, use 00_download_data.R before Stata and 00_post_stata.R after Stata.\n\n")

run_multnomah_data_pulls(cfg)
run_multnomah_post_stata(cfg)

multnomah_r_finish(
  "If maps were skipped, run 00_multnomah.do in Stata and then source(\"00_post_stata.R\")."
)
