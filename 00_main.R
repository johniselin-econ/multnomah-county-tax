# =============================================================================
# 00_main.R — R entry point for the pipeline's two R stages.
#
#   Rscript 00_main.R download   Stage 1: external data pulls (IPUMS ACS, NHGIS,
#                                BLS, DOL, Census). Run before 00_multnomah.do.
#   Rscript 00_main.R post       Stage 3: diagrams + maps that depend on Stata
#                                outputs. Run after 00_multnomah.do.
#
# The stage functions live in code/R/multnomah_r_common.R; run_stage1.sh and
# run_stage3.sh wrap the two calls with environment checks.
# =============================================================================

args  <- commandArgs(trailingOnly = TRUE)
stage <- if (length(args) >= 1) args[1] else ""
if (!stage %in% c("download", "post")) {
  stop("usage: Rscript 00_main.R download|post", call. = FALSE)
}

.sourced_by_main <- TRUE
source(file.path("code", "R", "multnomah_r_common.R"))

cfg <- multnomah_r_init(paste("00_main.R", stage))
if (stage == "download") {
  run_multnomah_data_pulls(cfg)
  multnomah_r_finish("Next step: run 00_multnomah.do in Stata.")
} else {
  run_multnomah_post_stata(cfg)
  multnomah_r_finish("Post-Stata R outputs complete.")
}
