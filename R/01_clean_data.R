# =============================================================================
# 01_clean_data.R
# Data cleaning and preparation for Multnomah County Tax analysis
#
# Converted from: code/01_clean_data.do (~1,740 lines)
# Author: John Iselin
#
# Data sources:
#   (a) Demographic data via IPUMS NHGIS
#   (b) Economic data via BEA Regional Economic Accounts (CAINC1)
#   (c) BLS Unemployment data
#   (d) NYTimes COVID-19 data
#   (e) ACS individual data via IPUMS USA
#   (f) IRS SOI County-Level Migration Files
#   (g) IRS SOI County-Level Income Data
#   (h) DOL Childcare Cost Data
#   (i) Property Tax Rates from ACS data
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(readxl)
})

# Source configuration
source(file.path(r_dir, "utils", "globals.R"))
source(file.path(r_dir, "utils", "helpers.R"))

message("=== 01_clean_data.R: Starting data cleaning ===")
message("  Run date: ", run_date)

# =============================================================================
# STEP 1: Download non-IPUMS data (if missing)
# =============================================================================
message("STEP 1: Checking and downloading source data...")

# IRS SOI: migration files
irs_base_url <- "https://www.irs.gov/pub/irs-soi"

for (yy in 15:21) {
  zz <- yy + 1
  fn_out <- sprintf("countyoutflow%02d%02d.csv", yy, zz)
  fn_in  <- sprintf("countyinflow%02d%02d.csv", yy, zz)
  download_if_missing(file.path(irs_base_url, fn_out),
                      file.path(data_irs, fn_out))
  download_if_missing(file.path(irs_base_url, fn_in),
                      file.path(data_irs, fn_in))
}

# IRS SOI: county income (AGI) files
for (yy in 15:22) {
  fn_inc <- paste0(yy, "incyallagi.csv")
  download_if_missing(file.path(irs_base_url, fn_inc),
                      file.path(data_irs, fn_inc))
}

# BEA Regional: CAINC1
bea_dir <- file.path(data_demographic, "CAINC1")
bea_files <- list.files(bea_dir, pattern = "CAINC1__ALL_AREAS.*\\.csv$")
if (length(bea_files) == 0) {
  bea_files <- list.files(bea_dir, pattern = "CAINC1__ALL_STATES.*\\.csv$")
}
if (length(bea_files) == 0) {
  bea_zip <- file.path(bea_dir, "CAINC1.zip")
  download_if_missing("https://apps.bea.gov/regional/zip/CAINC1.zip", bea_zip)
  if (file.exists(bea_zip)) {
    utils::unzip(bea_zip, exdir = bea_dir)
    file.remove(bea_zip)
  }
}

# NYTimes COVID Data
covid_file <- file.path(data_covid, "covid_nyt.csv")
download_if_missing(
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
  covid_file
)

# Check required manual downloads
require_file(file.path(data_demographic, "dol", "NDCP2022.xlsx"),
             "DOL childcare data (NDCP2022.xlsx)")
require_file(file.path(data_demographic, "bls", "la.data.64.County"),
             "BLS unemployment data (la.data.64.County)")

# =============================================================================
# STEP 2: Clean demographic + economic data (NHGIS, BEA, BLS)
# =============================================================================
message("STEP 2: Cleaning demographic and economic data...")

# ---- 2a: NHGIS demographic data ----
demo <- read_csv(
  file.path(data_demographic, "nhgis0031_csv", "nhgis0031_ts_nominal_county.csv"),
  show_col_types = FALSE
) |>
  select(-GISJOIN, -STATENH, -COUNTYNH, -NAME) |>
  rename(
    state_name       = STATE,
    state_fips       = STATEFP,
    county_name      = COUNTY,
    county_fips      = COUNTYFP,
    year             = YEAR,
    population       = AV0AA,
    pop_urban        = D15AA,
    pop_rural        = D15AB,
    median_income    = B79AA,
    population_margin = AV0AAM,
    median_income_margin = B79AAM
  ) |>
  mutate(percent_urban = pop_urban / population)

# (1) County and state IDs (from 2020 snapshot)
ids <- demo |>
  filter(year == "2020") |>
  select(state_fips, county_fips, state_name, county_name) |>
  mutate(fips = make_fips(state_fips, county_fips))

save_data(ids, file.path(data_working, "ids"))

state_ids <- ids |>
  select(state_fips, state_name) |>
  distinct()

save_data(state_ids, file.path(data_working, "state_ids"))

# (2) Population data (2020, urban/rural breakdown)
population_2020 <- demo |>
  filter(!is.na(pop_urban), year == "2020") |>
  select(-year, -median_income, -median_income_margin, -population_margin) |>
  mutate(fips = make_fips(state_fips, county_fips))

save_data(population_2020, file.path(data_working, "population_2020"))

# (3) ACS 2015-2019 data
acs_2015_2019 <- demo |>
  filter(!is.na(median_income), year == "2015-2019") |>
  select(-year, -pop_rural, -pop_urban, -percent_urban) |>
  mutate(fips = make_fips(state_fips, county_fips))

save_data(acs_2015_2019, file.path(data_working, "acs_2015_2019_data"))

# (4) Combined demographics 2020
demographics_2020 <- acs_2015_2019 |>
  rename(population_acs = population) |>
  inner_join(
    population_2020 |> select(state_fips, county_fips, fips, population,
                               pop_urban, pop_rural, percent_urban),
    by = c("state_fips", "county_fips", "fips")
  )

save_data(demographics_2020, file.path(data_working, "demographics_2020"))

# ---- 2b: BEA economic data (CAINC1) ----
bea_file <- list.files(bea_dir, pattern = "CAINC1__ALL_AREAS.*\\.csv$",
                       full.names = TRUE)
if (length(bea_file) == 0) {
  bea_file <- list.files(bea_dir, pattern = "CAINC1__ALL_STATES.*\\.csv$",
                         full.names = TRUE)
}

bea_raw <- read_csv(bea_file[1], show_col_types = FALSE) |>
  select(-Region, -TableName, -IndustryClassification, -Unit, -GeoName) |>
  filter(!is.na(LineCode))

# Clean GeoFips: remove quotes, convert to numeric
bea_raw <- bea_raw |>
  mutate(fips = as.numeric(str_remove_all(GeoFips, '"'))) |>
  select(-GeoFips)

# Keep population (LineCode 2) and per-capita income (LineCode 3)
bea_raw <- bea_raw |> filter(LineCode != 1)

# Reshape from wide to long: columns like 1969, 1970, ... are year values
# V9=1969 ... V63=2023 in Stata; in R the CSV has named year columns
year_cols <- names(bea_raw)[str_detect(names(bea_raw), "^\\d{4}$")]
if (length(year_cols) == 0) {
  # Fallback: columns may be named like V9, V10, etc.
  v_cols <- names(bea_raw)[str_detect(names(bea_raw), "^V\\d+$")]
  if (length(v_cols) > 0) {
    col_mapping <- setNames(v_cols, as.character(seq(1960 + as.numeric(str_extract(v_cols[1], "\\d+")),
                                                      length.out = length(v_cols))))
    bea_raw <- bea_raw |> rename(!!!col_mapping)
    year_cols <- names(col_mapping)
  }
}

bea_long <- bea_raw |>
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "value"
  ) |>
  mutate(year = as.integer(year))

# Keep relevant years
bea_long <- bea_long |>
  filter(year >= 2015, year <= end_year_acs)

# Pivot LineCode to columns
bea_wide <- bea_long |>
  pivot_wider(names_from = LineCode, values_from = value, names_prefix = "value") |>
  rename(population = value2, per_capita_income = value3)

# Drop if missing values
bea_wide <- bea_wide |>
  filter(population != "(NA)", !is.na(population)) |>
  mutate(
    population = as.numeric(population),
    per_capita_income = as.numeric(per_capita_income)
  )

# Keep only counties with all observations
bea_wide <- bea_wide |>
  group_by(fips) |>
  filter(n() == n_distinct(bea_wide$year[bea_wide$year <= 2023])) |>
  ungroup()

# Extrapolate 2024 using linear trend
if (max(bea_wide$year) < end_year_acs) {
  bea_extrap <- bea_wide |>
    group_by(fips) |>
    group_modify(~ {
      max_yr <- max(.x$year)
      if (max_yr >= end_year_acs) return(.x)

      new_years <- (max_yr + 1):end_year_acs
      for (v in c("population", "per_capita_income")) {
        mod <- lm(reformulate("year", v), data = .x)
        new_vals <- predict(mod, newdata = data.frame(year = new_years))
        new_rows <- tibble(year = new_years, !!v := new_vals)
        if (v == "population") {
          result <- new_rows
        } else {
          result[[v]] <- new_vals
        }
      }
      bind_rows(.x, result)
    }) |>
    ungroup()
  bea_wide <- bea_extrap
}

save_data(bea_wide |> select(fips, year, population, per_capita_income),
          file.path(data_working, "bea_economics"))

# ---- 2c: BLS Unemployment data ----
bls_raw <- read_delim(
  file.path(data_demographic, "bls", "la.data.64.County"),
  delim = "\t",
  show_col_types = FALSE,
  trim_ws = TRUE
)

bls <- bls_raw |>
  filter(str_trim(period) == "M13") |>   # Annual average
  mutate(
    measure = substr(str_trim(series_id), 20, 20),
    fips = as.numeric(substr(str_trim(series_id), 6, 10))
  ) |>
  filter(measure == "3") |>               # Unemployment rate
  filter(year >= 2015, year <= end_year_acs) |>
  filter(fips <= 60000) |>                 # Drop PR and territories
  select(year, fips, unemp = value) |>
  mutate(unemp = as.numeric(unemp))

save_data(bls, file.path(data_working, "bls_unemployment"))

# ---- 2d: County centroids ----
centroids_file <- file.path(data_demographic, "PopCenterCounty_US.csv")
if (file.exists(centroids_file)) {
  pop_centers <- read_csv(centroids_file, show_col_types = FALSE) |>
    filter(YEAR == 2010) |>
    select(fips = GEOGRAPHICINDENTIFIER, lat = LATITUDE, lon = LONGITUDE) |>
    filter(fips <= 60000)

  # Handle alternate column names
  if (!"fips" %in% names(pop_centers)) {
    alt_names <- names(pop_centers)
    geo_col <- alt_names[str_detect(tolower(alt_names), "geographic|geoid|fips")]
    if (length(geo_col) > 0) {
      pop_centers <- pop_centers |> rename(fips = !!geo_col[1])
    }
  }

  save_data(pop_centers, file.path(data_working, "pop_centers"))
}

# =============================================================================
# STEP 3: Clean NYTimes COVID-19 data
# =============================================================================
message("STEP 3: Cleaning COVID-19 data...")

covid_raw <- read_csv(covid_file, show_col_types = FALSE)

covid <- covid_raw |>
  rename(state_name = state, county_name = county) |>
  filter(!is.na(fips)) |>
  filter(!state_name %in% c("Puerto Rico", "Virgin Islands",
                              "Northern Mariana Islands")) |>
  mutate(date = as.Date(date))

# Fill panel (complete all fips x date combinations)
covid_panel <- covid |>
  tidyr::complete(fips, date, fill = list(cases = 0, deaths = 0)) |>
  arrange(fips, date)

# Fill names for newly created rows
covid_names <- covid |>
  filter(!is.na(state_name), !is.na(county_name)) |>
  distinct(fips, state_name, county_name)

covid_panel <- covid_panel |>
  select(-state_name, -county_name) |>
  left_join(covid_names, by = "fips")

# Add time components
covid_panel <- covid_panel |>
  mutate(
    year  = lubridate::year(date),
    month = lubridate::month(date),
    day   = lubridate::day(date)
  )

# Cumulative cases and deaths
covid_panel <- covid_panel |>
  group_by(fips) |>
  arrange(date) |>
  mutate(
    cases_cum  = cumsum(replace_na(cases, 0)),
    deaths_cum = cumsum(replace_na(deaths, 0))
  ) |>
  ungroup()

# Merge population
covid_panel <- covid_panel |>
  inner_join(population_2020 |> select(fips, population), by = "fips")

# Save daily file
save_data(covid_panel |> select(date, year, month, day, fips, state_name,
                                 county_name, cases, deaths, cases_cum,
                                 deaths_cum, population),
          file.path(data_working, "covid_cleaned"))

# Monthly collapsed version (per capita, wide)
covid_monthly <- covid_panel |>
  group_by(year, month, fips, state_name, county_name) |>
  summarize(
    cases  = sum(cases, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE),
    population = mean(population, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year, month, fips) |>
  group_by(fips) |>
  mutate(date_num = row_number()) |>
  ungroup()

covid_monthly <- covid_monthly |>
  group_by(fips) |>
  arrange(date_num) |>
  mutate(
    cases_cum  = cumsum(cases),
    deaths_cum = cumsum(deaths)
  ) |>
  ungroup() |>
  mutate(
    cases_cum  = 1000 * cases_cum / population,
    deaths_cum = 1000 * deaths_cum / population
  )

# Reshape wide (one row per county)
covid_wide <- covid_monthly |>
  select(fips, state_name, county_name, date_num, cases_cum, deaths_cum) |>
  pivot_wider(
    id_cols = c(fips, state_name, county_name),
    names_from = date_num,
    values_from = c(cases_cum, deaths_cum)
  )

save_data(covid_wide, file.path(data_working, "covid_cleaned_wide"))

# =============================================================================
# STEP 4: Clean ACS microdata
# =============================================================================
message("STEP 4: Cleaning ACS microdata...")

# Load and append all ACS year files
acs_list <- list()
for (y in start_year_acs:end_year_acs) {
  acs_file <- file.path(data_acs, paste0("acs_", y, ".csv"))
  if (file.exists(acs_file)) {
    acs_list[[as.character(y)]] <- read_csv(acs_file, show_col_types = FALSE)
  } else {
    warning("ACS file not found for year ", y, ": ", acs_file)
  }
}

acs_raw <- bind_rows(acs_list)

# Harmonize column names to lowercase
names(acs_raw) <- tolower(names(acs_raw))

# Define household-level variables
acs <- acs_raw |>
  mutate(
    adult = as.integer(age >= 18),
    child = as.integer(age < 18)
  ) |>
  group_by(year, serial) |>
  mutate(
    hh_size     = n(),
    hh_adult_ct = sum(adult),
    hh_child_ct = sum(child),
    college = as.integer(educd >= 0),
    hh_any_child   = as.integer(hh_child_ct > 0),
    hh_any_college = max(college),
    hh_perwt = sum(perwt)
  ) |>
  ungroup()

# Keep adults only (18+)
acs <- acs |> filter(adult == 1)

# Drop those living abroad last year, and foreign movers
acs <- acs |>
  filter(migplac1 <= 56, migrate1 != 4)

# Rename destination variables
acs <- acs |>
  rename(
    state_fips_d  = statefip,
    county_fips_d = countyfip
  )

# Set up origin data
acs <- acs |>
  rename(
    state_fips_o  = migplac1,
    county_fips_o = migcounty1
  )

# Use migrate1 to update origin for non-movers and within-state movers
acs <- acs |>
  mutate(
    # Non-movers: same house -> origin = destination
    state_fips_o  = ifelse(migrate1 == 1, state_fips_d, state_fips_o),
    county_fips_o = ifelse(migrate1 == 1, county_fips_d, county_fips_o),
    # Within-state: state of origin = destination state
    state_fips_o  = ifelse(migrate1 == 2, state_fips_d, state_fips_o)
  )

# Generate FIPS codes
acs <- acs |>
  mutate(
    fips_o = make_fips(state_fips_o, county_fips_o),
    fips_d = make_fips(state_fips_d, county_fips_d),
    same_county = as.integer(fips_o == fips_d),
    hh_head = as.integer(related == 1)
  )

# Save full ACS migration file
save_data(acs, file.path(data_working, "acs_migration_file"))

# ---- Build ACS county flow file ----
acs_flow_data <- acs |>
  filter(!is.na(year), !is.na(fips_o), !is.na(fips_d)) |>
  mutate(inctot = ifelse(is.na(inctot), 0, inctot))

# Persons + income by origin/destination/year
acs_pi <- acs_flow_data |>
  mutate(income_wt = inctot * perwt) |>
  group_by(year, fips_o, fips_d) |>
  summarize(
    persons      = sum(perwt, na.rm = TRUE),
    income_total = sum(income_wt, na.rm = TRUE),
    .groups = "drop"
  )

# Households by origin/destination/year (HH heads only)
acs_hh <- acs_flow_data |>
  filter(hh_head == 1) |>
  group_by(year, fips_o, fips_d) |>
  summarize(households = sum(hhwt, na.rm = TRUE), .groups = "drop")

# Merge persons/income with households
acs_county_flow <- acs_pi |>
  left_join(acs_hh, by = c("year", "fips_o", "fips_d")) |>
  mutate(
    state_fips_o  = floor(fips_o / 1000),
    county_fips_o = fips_o %% 1000,
    state_fips_d  = floor(fips_d / 1000),
    county_fips_d = fips_d %% 1000
  )

# Merge names for origin and destination
for (x in c("o", "d")) {
  sfx <- paste0("_", x)
  acs_county_flow <- acs_county_flow |>
    left_join(
      ids |> select(state_fips, county_fips, state_name, county_name) |>
        rename_with(~ paste0(., sfx)),
      by = setNames(c(paste0("state_fips", sfx), paste0("county_fips", sfx)),
                     c(paste0("state_fips", sfx), paste0("county_fips", sfx)))
    )
}

# Identify suppressed counties
acs_county_flow <- acs_county_flow |>
  mutate(
    county_name_o = ifelse(county_fips_o == 0, "Other", county_name_o),
    county_name_d = ifelse(county_fips_d == 0, "Other", county_name_d)
  )

save_data(acs_county_flow, file.path(data_working, "acs_county_flow"))

# ---- Build ACS gross migration files ----
message("  Building ACS gross migration files...")

acs_gross_25plus <- acs_make_gross_migration(acs, ids,
                                               sample_expr = "age >= 25")
save_data(acs_gross_25plus, file.path(data_working, "acs_county_gross_25plus"))

acs_gross_college <- acs_make_gross_migration(acs, ids,
                                               sample_expr = "hh_any_college == 1 & age >= 25")
save_data(acs_gross_college, file.path(data_working, "acs_county_gross_college"))

acs_gross_nokids <- acs_make_gross_migration(acs, ids,
                                              sample_expr = "hh_any_child == 0 & age >= 25")
save_data(acs_gross_nokids, file.path(data_working, "acs_county_gross_nokids"))

# =============================================================================
# STEP 5: Import and clean IRS migration data
# =============================================================================
message("STEP 5: Cleaning IRS migration data...")

irs_gross_in_list  <- list()
irs_gross_out_list <- list()
irs_flow_list      <- list()

for (y in 16:22) {
  start_yr <- y - 1
  end_yr   <- y
  tax_year <- 2000 + y

  # ---- OUTFLOW ----
  out_raw <- read_csv(
    file.path(data_irs, sprintf("countyoutflow%02d%02d.csv", start_yr, end_yr)),
    show_col_types = FALSE
  )
  names(out_raw) <- tolower(names(out_raw))

  out_raw <- out_raw |>
    mutate(year = tax_year) |>
    filter(y2_state != "DS", y2_state != "FR") |>
    filter(y1_countyfips != 0) |>
    mutate(across(any_of(c("n1", "n2", "agi")), unsuppress))

  # Gross categories (stayers + aggregates)
  out_gross <- out_raw |>
    filter(
      (y1_statefips == y2_statefips & y1_countyfips == y2_countyfips) |
        y2_statefips %in% c(96, 97, 98)
    ) |>
    mutate(
      move_type = case_when(
        y1_statefips == y2_statefips & y1_countyfips == y2_countyfips ~ 1L,
        y2_statefips == 96 ~ 2L,
        y2_statefips == 97 & y2_countyfips == 0 ~ 3L,
        y2_statefips == 97 & y2_countyfips == 1 ~ 4L,
        y2_statefips == 97 & y2_countyfips == 3 ~ 5L,
        y2_statefips == 98 ~ 6L,
        TRUE ~ 0L
      )
    )

  # Generate county totals (stayers + all movers)
  out_gross <- out_gross |>
    group_by(y1_statefips, y1_countyfips) |>
    mutate(
      n1_total = sum(n1[move_type %in% c(1, 2)]),
      n2_total = sum(n2[move_type %in% c(1, 2)]),
      agi_total = sum(agi[move_type %in% c(1, 2)])
    ) |>
    ungroup() |>
    rename(state_fips = y1_statefips, county_fips = y1_countyfips) |>
    select(year, state_fips, county_fips, move_type, n1, n2, agi,
           n1_total, n2_total, agi_total)

  irs_gross_out_list[[as.character(y)]] <- out_gross

  # Domestic movers for merge with flow file
  out_merge <- out_gross |>
    filter(move_type == 3) |>
    rename(n1_mover = n1, n2_mover = n2, agi_mover = agi) |>
    select(year, state_fips, county_fips, n1_mover, n2_mover, agi_mover,
           n1_total, n2_total, agi_total)

  # Flow file (county-to-county)
  out_flow <- out_raw |>
    filter(
      !y2_statefips %in% c(96, 97, 98),
      !(y1_statefips == y2_statefips & y1_countyfips == y2_countyfips)
    ) |>
    rename(
      state_fips_o  = y1_statefips,
      county_fips_o = y1_countyfips,
      state_fips_d  = y2_statefips,
      county_fips_d = y2_countyfips
    ) |>
    select(year, state_fips_o, county_fips_o, state_fips_d, county_fips_d,
           n1, n2, agi) |>
    left_join(
      out_merge |> rename(state_fips_o = state_fips, county_fips_o = county_fips),
      by = c("year", "state_fips_o", "county_fips_o")
    )

  irs_flow_list[[as.character(y)]] <- out_flow

  # ---- INFLOW ----
  in_raw <- read_csv(
    file.path(data_irs, sprintf("countyinflow%02d%02d.csv", start_yr, end_yr)),
    show_col_types = FALSE
  )
  names(in_raw) <- tolower(names(in_raw))

  in_raw <- in_raw |>
    mutate(year = tax_year) |>
    filter(y1_state != "DS", y1_state != "FR") |>
    filter(y2_countyfips != 0) |>
    mutate(across(any_of(c("n1", "n2", "agi")), unsuppress))

  in_gross <- in_raw |>
    filter(
      (y1_statefips == y2_statefips & y1_countyfips == y2_countyfips) |
        y1_statefips %in% c(96, 97, 98)
    ) |>
    mutate(
      move_type = case_when(
        y1_statefips == y2_statefips & y1_countyfips == y2_countyfips ~ 1L,
        y1_statefips == 96 ~ 2L,
        y1_statefips == 97 & y1_countyfips == 0 ~ 3L,
        y1_statefips == 97 & y1_countyfips == 1 ~ 4L,
        y1_statefips == 97 & y1_countyfips == 3 ~ 5L,
        y1_statefips == 98 ~ 6L,
        TRUE ~ 0L
      )
    )

  in_gross <- in_gross |>
    group_by(y2_statefips, y2_countyfips) |>
    mutate(
      n1_total = sum(n1[move_type %in% c(1, 2)]),
      n2_total = sum(n2[move_type %in% c(1, 2)]),
      agi_total = sum(agi[move_type %in% c(1, 2)])
    ) |>
    ungroup() |>
    rename(state_fips = y2_statefips, county_fips = y2_countyfips) |>
    select(year, state_fips, county_fips, move_type, n1, n2, agi,
           n1_total, n2_total, agi_total)

  irs_gross_in_list[[as.character(y)]] <- in_gross
}

# Append all years
irs_gross_in  <- bind_rows(irs_gross_in_list)
irs_gross_out <- bind_rows(irs_gross_out_list)
irs_flow      <- bind_rows(irs_flow_list)

# Merge names for gross files
irs_gross_in <- irs_gross_in |>
  left_join(ids |> select(state_fips, county_fips, state_name, county_name, fips),
            by = c("state_fips", "county_fips"))

irs_gross_out <- irs_gross_out |>
  left_join(ids |> select(state_fips, county_fips, state_name, county_name, fips),
            by = c("state_fips", "county_fips"))

# Add FIPS and names to flow file
irs_flow <- irs_flow |>
  mutate(
    fips_o = make_fips(state_fips_o, county_fips_o),
    fips_d = make_fips(state_fips_d, county_fips_d)
  )

for (x in c("o", "d")) {
  sfx <- paste0("_", x)
  irs_flow <- irs_flow |>
    left_join(
      ids |> select(state_fips, county_fips, state_name, county_name) |>
        rename_with(~ paste0(., sfx)),
      by = setNames(c(paste0("state_fips", sfx), paste0("county_fips", sfx)),
                     c(paste0("state_fips", sfx), paste0("county_fips", sfx)))
    )
}

save_data(irs_gross_in, file.path(data_working, "irs_county_gross_in"))
save_data(irs_gross_out, file.path(data_working, "irs_county_gross_out"))
save_data(irs_flow, file.path(data_working, "irs_county_flow"))

# ---- Build combined gross file (in + out + net) ----
message("  Building IRS combined gross migration file...")

# Reshape inflow wide by move_type
gross_in_wide <- irs_gross_in |>
  select(year, state_fips, county_fips, fips, state_name, county_name,
         move_type, n1, n2, agi, n1_total, n2_total, agi_total) |>
  rename(n1_total_in = n1_total, n2_total_in = n2_total,
         agi_total_in = agi_total) |>
  pivot_wider(
    id_cols = c(year, state_fips, county_fips, fips, state_name, county_name,
                n1_total_in, n2_total_in, agi_total_in),
    names_from = move_type,
    values_from = c(n1, n2, agi),
    names_glue = "{.value}_in_{move_type}"
  )

# Reshape outflow wide by move_type
gross_out_wide <- irs_gross_out |>
  select(year, state_fips, county_fips, fips, state_name, county_name,
         move_type, n1, n2, agi, n1_total, n2_total, agi_total) |>
  rename(n1_total_out = n1_total, n2_total_out = n2_total,
         agi_total_out = agi_total) |>
  pivot_wider(
    id_cols = c(year, state_fips, county_fips, fips, state_name, county_name,
                n1_total_out, n2_total_out, agi_total_out),
    names_from = move_type,
    values_from = c(n1, n2, agi),
    names_glue = "{.value}_out_{move_type}"
  )

# Merge
irs_gross <- inner_join(gross_in_wide, gross_out_wide,
                         by = c("year", "state_fips", "county_fips", "fips",
                                "state_name", "county_name"))

# Replace NAs with 0 and compute net migration
in_out_cols <- names(irs_gross)[str_detect(names(irs_gross), "_(in|out)_\\d+$")]
irs_gross <- irs_gross |>
  mutate(across(all_of(in_out_cols), ~ replace_na(., 0)))

# Net migration (types 2-6)
for (a in c("n1", "n2", "agi")) {
  for (n in 2:6) {
    in_col  <- paste0(a, "_in_", n)
    out_col <- paste0(a, "_out_", n)
    net_col <- paste0(a, "_net_", n)
    if (in_col %in% names(irs_gross) && out_col %in% names(irs_gross)) {
      irs_gross[[net_col]] <- irs_gross[[in_col]] - irs_gross[[out_col]]
    }
  }
}

save_data(irs_gross, file.path(data_working, "irs_county_gross"))

# =============================================================================
# STEP 6: Import and clean IRS county-level aggregate data
# =============================================================================
message("STEP 6: Cleaning IRS county-level aggregate data...")

irs_county_all_list <- list()

for (y in 15:22) {
  irs_inc_file <- file.path(data_irs, paste0(y, "incyallagi.csv"))
  if (!file.exists(irs_inc_file)) next

  irs_inc <- read_csv(irs_inc_file, show_col_types = FALSE)
  names(irs_inc) <- tolower(names(irs_inc))

  irs_inc <- irs_inc |>
    mutate(year = 2000 + y) |>
    select(any_of(c("statefips", "state", "countyfips", "countyname",
                     "agi_stub", "year", "n1", "mars1", "mars2", "mars4",
                     "n2", "elderly", "a00100", "n02650", "a02650",
                     "n00200", "a00200"))) |>
    rename(
      state_fips  = statefips,
      state_abb   = state,
      county_fips = countyfips,
      county_name = countyname,
      agi         = a00100,
      n_total_inc = n02650,
      a_total_inc = a02650,
      n_wage      = n00200,
      a_wage      = a00200
    ) |>
    mutate(
      agi         = agi * 1000,
      a_total_inc = a_total_inc * 1000,
      a_wage      = a_wage * 1000,
      fips        = make_fips(state_fips, county_fips)
    )

  irs_county_all_list[[as.character(y)]] <- irs_inc
}

irs_county_all <- bind_rows(irs_county_all_list)
save_data(irs_county_all, file.path(data_working, "irs_county_all"))

# =============================================================================
# STEP 7: Import and clean DOL childcare cost data
# =============================================================================
message("STEP 7: Cleaning DOL childcare data...")

dol_raw <- read_excel(
  file.path(data_demographic, "dol", "NDCP2022.xlsx")
)
names(dol_raw) <- tolower(names(dol_raw))

dol <- dol_raw |>
  filter(state_fips != 72) |>
  filter(between(studyyear, 2015, 2022)) |>
  select(fips = county_fips_code, year = studyyear, me,
         mcinfant, mctoddler, mcpreschool,
         mfccinfant, mfcctoddler, mfccpreschool)

# Ensure column names match (DOL sometimes uses different casing)
if (!"fips" %in% names(dol) && "county_fips" %in% names(dol)) {
  dol <- dol |> rename(fips = county_fips)
}

# Balance panel and interpolate
dol <- dol |>
  complete(fips, year = 2015:2022) |>
  group_by(fips) |>
  mutate(across(c(me, mcinfant, mctoddler, mcpreschool,
                   mfccinfant, mfcctoddler, mfccpreschool),
                ~ approx(year[!is.na(.)], .[!is.na(.)], xout = year)$y)) |>
  ungroup()

# Generate cost as percent of median earnings
dol <- dol |>
  mutate(
    mc_infant_med    = mcinfant / me,
    mc_toddler_med   = mctoddler / me,
    mc_preschool_med = mcpreschool / me,
    mf_infant_med    = mfccinfant / me,
    mf_toddler_med   = mfcctoddler / me,
    mf_preschool_med = mfccpreschool / me
  ) |>
  select(fips, year, starts_with("mc_"), starts_with("mf_"))

# Extrapolate to end_year_acs using linear trend
if (end_year_acs > 2022) {
  new_years <- (2023):end_year_acs

  dol_extrap <- dol |>
    group_by(fips) |>
    group_modify(~ {
      cost_vars <- names(.x)[str_detect(names(.x), "^(mc_|mf_)")]
      new_rows <- tibble(year = new_years)
      for (v in cost_vars) {
        non_missing <- sum(!is.na(.x[[v]]))
        if (non_missing > 3) {
          mod <- lm(reformulate("year", v), data = .x)
          new_rows[[v]] <- predict(mod, newdata = data.frame(year = new_years))
        } else {
          new_rows[[v]] <- NA_real_
        }
      }
      bind_rows(.x, new_rows)
    }) |>
    ungroup()

  # Remove counties with too many missing values
  dol <- dol_extrap |>
    group_by(fips) |>
    filter(sum(!is.na(mc_infant_med)) > 3) |>
    ungroup()
}

save_data(dol, file.path(data_working, "dol_childcare"))

# =============================================================================
# STEP 8: Calculate county-level property tax rates from ACS data
# =============================================================================
message("STEP 8: Calculating property tax rates...")

# Load ACS migration file
acs_proptx <- load_data(file.path(data_working, "acs_migration_file"))

# Keep household heads only
acs_proptx <- acs_proptx |>
  filter(related == 1) |>
  mutate(fips = fips_d) |>
  filter(!is.na(fips)) |>
  filter(proptx99 != 0)  # Drop N/A (not applicable)

# Convert PROPTX99 codes to dollar midpoint values
# Based on IPUMS coding scheme
acs_proptx <- acs_proptx |>
  mutate(
    proptx_dollars = case_when(
      proptx99 == 1 ~ 0,
      proptx99 == 2 ~ 25,
      proptx99 >= 3  & proptx99 <= 12  ~ (proptx99 - 3) * 50 + 50 + 24.5,
      proptx99 == 13 ~ 575,
      proptx99 == 14 ~ 650,
      proptx99 == 15 ~ 750,
      proptx99 == 16 ~ 850,
      proptx99 == 17 ~ 950,
      proptx99 == 18 ~ 1050,
      proptx99 == 19 ~ 1150,
      proptx99 == 20 ~ 1250,
      proptx99 == 21 ~ 1350,
      proptx99 == 22 ~ 1450,
      proptx99 >= 23 & proptx99 <= 62 ~ (proptx99 - 23) * 100 + 1500 + 49.5,
      proptx99 == 63 ~ 5750,
      proptx99 == 64 ~ 6250,
      proptx99 == 65 ~ 6750,
      proptx99 == 66 ~ 7250,
      proptx99 == 67 ~ 7750,
      proptx99 == 68 ~ 8500,
      proptx99 == 69 ~ 9500,
      proptx99 >= 70 & proptx99 < 140 ~ 10000 + (proptx99 - 70) * 1000,
      proptx99 >= 140 & proptx99 < 159 ~ 75000,
      proptx99 == 159 ~ 100000,
      TRUE ~ NA_real_
    )
  )

# Drop if property tax could not be assigned or home value missing
acs_proptx <- acs_proptx |>
  filter(!is.na(proptx_dollars)) |>
  filter(!is.na(valueh), valueh > 0, valueh != 9999999)

# Calculate property tax rate
acs_proptx <- acs_proptx |>
  mutate(prop_rate = 100 * proptx_dollars / valueh)

# ---- VERSION 1: Overall (all observations) ----
prop_tax_overall <- acs_proptx |>
  group_by(year, fips) |>
  summarize(
    prop_rate_mean = weighted.mean(prop_rate, hhwt, na.rm = TRUE),
    prop_rate_se   = sqrt(Hmisc::wtd.var(prop_rate, hhwt, na.rm = TRUE) /
                            sum(!is.na(prop_rate))),
    prop_rate_n    = sum(!is.na(prop_rate)),
    .groups = "drop"
  ) |>
  mutate(
    state_fips  = floor(fips / 1000),
    county_fips = fips %% 1000
  ) |>
  left_join(ids |> select(state_fips, county_fips, state_name, county_name),
            by = c("state_fips", "county_fips")) |>
  left_join(state_ids, by = "state_fips", suffix = c("", "_state")) |>
  mutate(
    state_name  = coalesce(state_name, state_name_state),
    county_name = ifelse(county_fips == 0, "Other", county_name)
  ) |>
  select(-any_of("state_name_state"))

save_data(prop_tax_overall, file.path(data_working, "property_tax_rates_overall"))

# ---- VERSION 2: Excluding allocated values ----
prop_tax_excl <- acs_proptx |>
  filter(qproptx99 != 4, qvalueh != 4) |>
  group_by(year, fips) |>
  summarize(
    prop_rate_mean = weighted.mean(prop_rate, hhwt, na.rm = TRUE),
    prop_rate_se   = sqrt(Hmisc::wtd.var(prop_rate, hhwt, na.rm = TRUE) /
                            sum(!is.na(prop_rate))),
    prop_rate_n    = sum(!is.na(prop_rate)),
    .groups = "drop"
  ) |>
  mutate(
    state_fips  = floor(fips / 1000),
    county_fips = fips %% 1000
  ) |>
  left_join(ids |> select(state_fips, county_fips, state_name, county_name),
            by = c("state_fips", "county_fips")) |>
  left_join(state_ids, by = "state_fips", suffix = c("", "_state")) |>
  mutate(
    state_name  = coalesce(state_name, state_name_state),
    county_name = ifelse(county_fips == 0, "Other", county_name)
  ) |>
  select(-any_of("state_name_state"))

save_data(prop_tax_excl,
          file.path(data_working, "property_tax_rates_excl_allocated"))

message("=== 01_clean_data.R: Complete ===")
message("  Property tax rate calculation complete.")
message("  Overall version saved to: ", file.path(data_working, "property_tax_rates_overall"))
message("  Excluding allocated saved to: ", file.path(data_working, "property_tax_rates_excl_allocated"))
