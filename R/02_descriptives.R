# =============================================================================
# 02_descriptives.R
# Descriptive statistics for Multnomah County Tax analysis
#
# Converted from: code/02_descriptives.do
# Author: John Iselin
#
# Outputs:
# - Table2.xlsx: Migration rates for Multnomah and neighboring counties
#   - Sheet "raw_irs": IRS-based rates (2018-19 vs 2021-22)
#   - Sheet "raw_acs": ACS-based rates (2018-19 vs 2021-22 vs 2021-24)
# - multnomah_partner_flows_{n1|n2|agi}.csv: Partner-normalized flow maps
# - multnomah.xlsx: Raw flow data for Multnomah (IRS and ACS)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(openxlsx)
})

source(here::here("R", "utils", "globals.R"))
source(here::here("R", "utils", "helpers.R"))

message("=== 02_descriptives.R: Starting descriptive analysis ===")

# =============================================================================
# SECTION 1: RAW FLOW DATA EXPORT (IRS + ACS)
# =============================================================================

# ---- IRS flows ----
irs_flow <- load_data(file.path(data_working, "irs_county_flow"))

irs_flow <- irs_flow |>
  mutate(
    multnomah_o = as.integer(state_fips_o == 41 & county_fips_o == 51),
    multnomah_d = as.integer(state_fips_d == 41 & county_fips_d == 51)
  )

wb <- createWorkbook()
for (x in c("o", "d")) {
  sheet_name <- paste0("irs_", x)
  sheet_data <- irs_flow |> filter(get(paste0("multnomah_", x)) == 1)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, sheet_data)
}

# ---- ACS flows ----
acs_flow <- load_data(file.path(data_working, "acs_county_flow"))

acs_flow <- acs_flow |>
  mutate(
    multnomah_o = as.integer(state_fips_o == 41 & county_fips_o == 51),
    multnomah_d = as.integer(state_fips_d == 41 & county_fips_d == 51)
  )

for (x in c("o", "d")) {
  sheet_name <- paste0("acs_", x)
  sheet_data <- acs_flow |> filter(get(paste0("multnomah_", x)) == 1)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, sheet_data)
}

saveWorkbook(wb, file.path(data_dir, "multnomah.xlsx"), overwrite = TRUE)

# =============================================================================
# SECTION 2: PRE-POST FLOW COMPARISON (Table 2)
# =============================================================================
message("Building Table 2: Pre-post flow comparison...")

wb2 <- createWorkbook()

# ---- IRS DATA ----
irs_gross <- load_data(file.path(data_working, "irs_county_gross"))

irs_table <- irs_gross |>
  filter(state_name %in% c("Oregon", "Washington"),
         year %in% c(2018, 2019, 2021, 2022)) |>
  mutate(
    period = case_when(
      year %in% c(2018, 2019) ~ "pre",
      year %in% c(2021, 2022) ~ "post_21_22",
      TRUE ~ NA_character_
    ),
    n1_base = n1_out_1 + n1_out_2,
    n2_base = n2_out_1 + n2_out_2,
    agi_base = agi_out_1 + agi_out_2
  ) |>
  filter(!is.na(period))

# Create "Other" categories
irs_table <- irs_table |>
  mutate(
    county_name = case_when(
      state_name == "Oregon" & !fips %in% or_neighbor_fips ~ "Other",
      state_name == "Washington" & !fips %in% wa_neighbor_fips ~ "Other",
      TRUE ~ county_name
    )
  )

irs_rates <- irs_table |>
  group_by(state_name, county_name, period) |>
  summarize(
    across(c(n1_base, n2_base, agi_base, n1_out_3, n2_out_3, agi_out_3,
             n1_in_3, n2_in_3, agi_in_3), sum, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    n1_out_rate = n1_out_3 / n1_base,
    n2_out_rate = n2_out_3 / n2_base,
    agi_out_rate = agi_out_3 / agi_base,
    n1_in_rate = n1_in_3 / n1_base,
    n2_in_rate = n2_in_3 / n2_base,
    agi_in_rate = agi_in_3 / agi_base
  ) |>
  select(state_name, county_name, period, ends_with("_rate"))

addWorksheet(wb2, "raw_irs")
writeData(wb2, "raw_irs", irs_rates)

# ---- ACS DATA ----
acs_gross <- load_data(file.path(data_working, "acs_county_gross_25plus"))

acs_table <- acs_gross |>
  filter(state_name %in% c("Oregon", "Washington"),
         year %in% c(2018, 2019, 2021, 2022, 2023, 2024)) |>
  mutate(
    period = case_when(
      year %in% c(2018, 2019) ~ "pre",
      year %in% c(2021, 2022) ~ "post_21_22",
      year %in% c(2023, 2024) ~ "post_23_24",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(period))

# Rename for consistency
acs_table <- acs_table |>
  rename_with(~ gsub("^households_", "hh_", .x)) |>
  rename_with(~ gsub("^persons_", "per_", .x)) |>
  rename_with(~ gsub("^dollars_", "dol_", .x))

# Base population
acs_table <- acs_table |>
  mutate(
    hh_base  = hh_out_1 + hh_out_2,
    per_base = per_out_1 + per_out_2,
    dol_base = dol_out_1 + dol_out_2
  )

# Create "Other" categories
acs_table <- acs_table |>
  mutate(
    county_name = case_when(
      state_name == "Oregon" & !fips %in% or_neighbor_fips ~ "Other",
      state_name == "Washington" & !fips %in% wa_neighbor_fips ~ "Other",
      TRUE ~ county_name
    )
  )

# Collapse
acs_rates <- acs_table |>
  group_by(state_name, county_name, period) |>
  summarize(
    across(c(hh_base, per_base, dol_base, hh_out_3, per_out_3, dol_out_3,
             hh_in_3, per_in_3, dol_in_3), sum, na.rm = TRUE),
    .groups = "drop"
  )

# Combine post_21_22 + post_23_24 into post_21_24
post_combined <- acs_rates |>
  filter(period %in% c("post_21_22", "post_23_24")) |>
  group_by(state_name, county_name) |>
  summarize(
    across(c(hh_base, per_base, dol_base, hh_out_3, per_out_3, dol_out_3,
             hh_in_3, per_in_3, dol_in_3), sum, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(period = "post_21_24")

acs_rates <- bind_rows(
  acs_rates |> filter(period != "post_23_24"),
  post_combined
)

# Calculate rates
acs_rates <- acs_rates |>
  mutate(
    hh_out_rate  = hh_out_3 / hh_base,
    per_out_rate = per_out_3 / per_base,
    dol_out_rate = dol_out_3 / dol_base,
    hh_in_rate   = hh_in_3 / hh_base,
    per_in_rate  = per_in_3 / per_base,
    dol_in_rate  = dol_in_3 / dol_base
  ) |>
  select(state_name, county_name, period, ends_with("_rate"))

addWorksheet(wb2, "raw_acs")
writeData(wb2, "raw_acs", acs_rates)

saveWorkbook(wb2, file.path(results_dir, "Table2.xlsx"), overwrite = TRUE)

# =============================================================================
# SECTION 3: PARTNER-COUNTY NORMALIZED FLOW MAPS
# =============================================================================
message("Building partner-county normalized flow datasets...")

# Get county populations for rate calculations
county_pops <- irs_gross |>
  mutate(
    period = case_when(
      year %in% c(2018, 2019) ~ "pre",
      year %in% c(2021, 2022) ~ "post",
      TRUE ~ NA_character_
    ),
    n1_pop  = n1_out_1 + n1_out_2,
    n2_pop  = n2_out_1 + n2_out_2,
    agi_pop = agi_out_1 + agi_out_2
  ) |>
  filter(!is.na(period)) |>
  # Keep balanced counties
  group_by(fips) |>
  filter(n() == 4) |>
  ungroup() |>
  group_by(fips, period) |>
  summarize(across(c(n1_pop, n2_pop, agi_pop), sum, na.rm = TRUE),
            .groups = "drop") |>
  pivot_wider(names_from = period, values_from = c(n1_pop, n2_pop, agi_pop)) |>
  mutate(
    n1_pop_avg  = (n1_pop_pre + n1_pop_post) / 2,
    n2_pop_avg  = (n2_pop_pre + n2_pop_post) / 2,
    agi_pop_avg = (agi_pop_pre + agi_pop_post) / 2
  )

# Load IRS flow data
irs_flow_data <- load_data(file.path(data_working, "irs_county_flow"))

irs_flow_data <- irs_flow_data |>
  mutate(
    period = case_when(
      year %in% c(2018, 2019) ~ "pre",
      year %in% c(2021, 2022) ~ "post",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(period))

# Loop over measures
for (measure in c("n1", "n2", "agi")) {

  message("  Processing partner flows for ", measure, "...")

  # ---- OUT-MIGRATION: FROM Multnomah TO other counties ----
  out_flows <- irs_flow_data |>
    filter(fips_o == multnomah_fips) |>
    group_by(fips_d, period) |>
    summarize(flow = sum(.data[[measure]], na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = period, values_from = flow,
                names_prefix = "out_") |>
    rename(fips = fips_d) |>
    mutate(across(starts_with("out_"), ~ replace_na(., 0)))

  # Merge destination populations
  pop_cols <- paste0(measure, c("_pop_pre", "_pop_post", "_pop_avg"))
  out_flows <- out_flows |>
    inner_join(county_pops |> select(fips, all_of(pop_cols)),
               by = "fips") |>
    rename(dest_pop_pre = !!pop_cols[1],
           dest_pop_post = !!pop_cols[2],
           dest_pop_avg = !!pop_cols[3]) |>
    mutate(
      out_rate_pre    = 100000 * out_pre / dest_pop_pre,
      out_rate_post   = 100000 * out_post / dest_pop_post,
      out_rate_change = out_rate_post - out_rate_pre
    )

  # ---- IN-MIGRATION: TO Multnomah FROM other counties ----
  in_flows <- irs_flow_data |>
    filter(fips_d == multnomah_fips) |>
    group_by(fips_o, period) |>
    summarize(flow = sum(.data[[measure]], na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = period, values_from = flow,
                names_prefix = "in_") |>
    rename(fips = fips_o) |>
    mutate(across(starts_with("in_"), ~ replace_na(., 0)))

  in_flows <- in_flows |>
    inner_join(county_pops |> select(fips, all_of(pop_cols)),
               by = "fips") |>
    rename(orig_pop_pre = !!pop_cols[1],
           orig_pop_post = !!pop_cols[2],
           orig_pop_avg = !!pop_cols[3]) |>
    mutate(
      in_rate_pre    = 100000 * in_pre / orig_pop_pre,
      in_rate_post   = 100000 * in_post / orig_pop_post,
      in_rate_change = in_rate_post - in_rate_pre
    )

  # ---- MERGE in and out flows ----
  partner_flows <- full_join(out_flows, in_flows, by = "fips") |>
    mutate(
      across(c(out_pre, out_post, in_pre, in_post), ~ replace_na(., 0)),
      net_rate_change = in_rate_change - out_rate_change
    ) |>
    arrange(fips)

  # Save
  save_data(partner_flows,
            file.path(data_working, paste0("multnomah_partner_flows_", measure)))

  message("    Top 5 out-migration destinations (post rate): ",
          paste(head(partner_flows |> arrange(desc(out_rate_post)) |>
                       pull(fips), 5), collapse = ", "))
}

message("=== 02_descriptives.R: Complete ===")
