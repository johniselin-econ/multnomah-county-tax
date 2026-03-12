# =============================================================================
# 00_post_stata.R
# Run R outputs that depend on cleaned Stata data or are naturally post-analysis.
#
# Run this after 00_multnomah.do.
# =============================================================================

.sourced_by_main <- TRUE
source(file.path("code", "R", "multnomah_r_common.R"))

cfg <- multnomah_r_init("00_post_stata.R")
run_multnomah_post_stata(cfg)
multnomah_r_finish("Post-Stata R outputs complete.")
