# user_settings.example.R — template for local, machine-specific settings.
#
# Copy this file to user_settings.R (gitignored) and edit. It is sourced by
# code/R/multnomah_r_common.R if present, so its objects are available to the
# R pipeline (the download and post-Stata stages).
#
# Overleaf sync: set oth_path to your local Overleaf folder to mirror figures
# and tables there. Leave user_settings.R absent (or oth_path unset) to disable.
# (You can alternatively set the OVERLEAF_PATH environment variable.)

# oth_path <- "C:/Users/<you>/Dropbox/Apps/Overleaf/Multnomah County/"

# ACS data source (see code/R/multnomah_r_common.R). Default is "shared".
#   acs_source      : "shared" reads the Budget Lab common IPUMS extract from the
#                     shared drive; "local" downloads via the IPUMS API.
#   acs_shared_root : path to the shared per-year extract root (contains
#                     us<YYYY>a/ folders). Required when acs_source is "shared".
# acs_source      <- "shared"
# acs_shared_root <- "/path/to/shared/raw_data/ACS/acs_common"
