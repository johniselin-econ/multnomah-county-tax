# user_settings.example.R — template for local, machine-specific settings.
#
# Copy this file to user_settings.R (gitignored) and edit. REQUIRED for the
# Stage-1 R downloads: acs_source defaults to "shared" and the pipeline stops
# unless acs_shared_root is set (or acs_source is "local"). It is loaded by
# code/R/utils.R::load_user_settings() for multnomah_r_common.R (download and
# post-Stata stages) and for code/R/audit_paper_artifacts.R.
#
# Overleaf sync: set oth_path to your local Overleaf folder to mirror figures
# and tables there; leave it unset to disable. The OVERLEAF_PATH environment
# variable, when set, takes precedence over oth_path.

# oth_path <- "C:/Users/<you>/Dropbox/Apps/Overleaf/Multnomah County/"

# ACS data source (see code/R/multnomah_r_common.R). Default is "shared".
#   acs_source      : "shared" reads the Budget Lab common IPUMS extract from the
#                     shared drive; "local" downloads via the IPUMS API.
#   acs_shared_root : path to the shared per-year extract root (contains
#                     us<YYYY>a/ folders). Required when acs_source is "shared".
# acs_source      <- "shared"
# acs_shared_root <- "/path/to/shared/raw_data/ACS/acs_common_v2"
