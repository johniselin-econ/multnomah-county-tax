# =============================================================================
# Author: John Iselin
# Purpose: Download Census B01001 (Sex by Age) from ACS 5-year estimates and
#          compute county-level age shares (under 24, over 65) for k-means
#          clustering in SDID analysis.
#
# Called by: 01_clean_data.do via rcall
#
# Inputs:  Census API (tidycensus)
# Outputs: data/working/age_shares_county.csv
# =============================================================================

suppressPackageStartupMessages({
  library(tidycensus)
  library(dplyr)
  library(tidyr)
  library(readr)
})

# ---- Paths (set by 00_multnomah.R before sourcing) ----
if (!exists("project_root")) {
  stop("project_root must be defined (set by 00_multnomah.R)")
}
if (!exists("api_codes_path")) {
  stop("api_codes_path must be defined (set by 00_multnomah.R)")
}

output_path <- file.path(project_root, "data", "working", "age_shares_county.csv")

# ---- Read Census API key from api_codes.txt ----
.this_dir_census <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "code/R")
source(file.path(.this_dir_census, "utils.R"))

census_key <- read_api_key(api_codes_path, "census")
census_api_key(census_key)
cat("Census API key loaded from api_codes.txt\n")

# ---- Download B01001 table ----
# B01001: Sex by Age, ACS 2015-2019 5-year estimates
# Total = B01001_001
# Male 0-4 through Male 20-24:  B01001_003 to B01001_010
# Male 65-66 through Male 85+:  B01001_020 to B01001_025
# Female 0-4 through Female 20-24: B01001_027 to B01001_034
# Female 65-66 through Female 85+: B01001_044 to B01001_049

cat("Downloading Census B01001 (Sex by Age) from ACS 2015-2019 5-year...\n")

age_raw <- get_acs(
  geography = "county",
  table     = "B01001",
  year      = 2019,
  survey    = "acs5",
  output    = "wide"
)

# ---- Compute age shares ----
age_shares <- age_raw %>%
  transmute(
    # FIPS as numeric (drop leading zeros)
    fips = as.numeric(GEOID),
    county_name = NAME,
    # Total population
    total_pop = B01001_001E,
    # Under 24: Male (003-010) + Female (027-034)
    # 003: Under 5, 004: 5-9, 005: 10-14, 006: 15-17, 007: 18-19, 008: 20, 009: 21, 010: 22-24
    pop_under_24 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E +
                      B01001_007E + B01001_008E + B01001_009E + B01001_010E +
                      B01001_027E + B01001_028E + B01001_029E + B01001_030E +
                      B01001_031E + B01001_032E + B01001_033E + B01001_034E),
    # Over 65: Male (020-025) + Female (044-049)
    # 020: 65-66, 021: 67-69, 022: 70-74, 023: 75-79, 024: 80-84, 025: 85+
    pop_over_65 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E +
                     B01001_024E + B01001_025E +
                     B01001_044E + B01001_045E + B01001_046E + B01001_047E +
                     B01001_048E + B01001_049E)
  ) %>%
  filter(total_pop > 0) %>%
  mutate(
    share_under_24 = pop_under_24 / total_pop,
    share_over_65  = pop_over_65 / total_pop
  ) %>%
  # Keep only 50 states + DC (FIPS < 57000, excludes PR/territories)
  filter(fips < 57000) %>%
  select(fips, share_under_24, share_over_65)

cat(sprintf("Computed age shares for %d counties.\n", nrow(age_shares)))

# ---- Save output ----
if (!dir.exists(dirname(output_path))) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
}
write_csv(age_shares, output_path)
cat(sprintf("Saved to: %s\n", output_path))
