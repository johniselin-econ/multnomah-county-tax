# =============================================================================
# 02_revenue.R
# Revenue effects of tax-induced migration from Multnomah County's
# Preschool for All (PFA) income tax.
#
# Converted from: code/02_revenue.do
# Author: John Iselin
#
# Purpose: Builds a microsimulation tax model using 2019 ACS microdata,
#          calibrates to IRS administrative totals, computes baseline PFA and
#          Oregon income tax revenue, and simulates revenue loss from
#          tax-induced out-migration via Monte Carlo.
#
# Key changes from Stata version:
#   - Income adjustment: subtract incwel_nom from inctot before AGI proxy
#   - Proper raking: survey::postStratify() instead of manual ratio adjustment
#   - SDID runs: estimate effect_agi and effect_agi_oregon from panel data
#
# Outputs:
#   - results/revenue/tbl_revenue_summary.xlsx
#   - results/revenue/tbl_pfa_by_bracket.xlsx
#   - results/revenue/fig_pfa_revenue_distribution.pdf
#   - results/revenue/fig_pfa_revenue_distribution.png
#   - data/working/revenue_microsim.rds
#   - data/working/revenue_simulations.rds
#
# Requirements:
#   - survey, synthdid, dplyr, readr, ggplot2, openxlsx
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(survey)
  library(openxlsx)
})

# Source configuration
source(here::here("R", "utils", "globals.R"))
source(here::here("R", "utils", "helpers.R"))

message("=== 02_revenue.R: Starting revenue analysis ===")
message("  Run date: ", run_date)

# =============================================================================
# SECTION 0: Setup & Parameters
# =============================================================================

## Default migration effects (will be overwritten by SDID if data available)
effect_agi         <- 0.02  # 2% net out-migration effect on AGI
effect_agi_oregon  <- 0.02  # Oregon-level effect
n_sims             <- 1000  # Monte Carlo iterations
cpi_2019_to_2022   <- 1.136 # CPI-U inflation factor 2019 -> 2022

## PFA tax brackets (2022)
pfa_thresh1_single <- 125000
pfa_thresh2_single <- 250000
pfa_thresh1_joint  <- 200000
pfa_thresh2_joint  <- 400000
pfa_rate           <- 0.015

## Create output directory
results_revenue <- file.path(results_dir, "revenue")
dir.create(results_revenue, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# SECTION 0B: SDID Estimation of Migration Effects
# =============================================================================

message("")
message("==============================================")
message("Section 0B: SDID estimation of migration effects")
message("==============================================")

sdid_data_path <- file.path(data_working, "sdid_analysis_data")
sdid_data_exists <- file.exists(paste0(sdid_data_path, ".rds")) ||
                    file.exists(paste0(sdid_data_path, ".csv"))

if (sdid_data_exists) {
  message("  Loading SDID panel data...")
  sdid_df <- load_data(sdid_data_path)

  ## ---- Run 1: effect_agi (county-level, domestic type 3) ----
  ## Sample: irs_sample_1, sample_all, exclude 2020
  message("  Running SDID for effect_agi (agi_net_rate_irs)...")

  sdid_run <- function(sdid_df, outcome_var, sample_var, exclude_2020 = TRUE) {
    suppressPackageStartupMessages(library(synthdid))

    sub <- sdid_df |>
      filter(.data[[sample_var]] == 1)
    if (exclude_2020) sub <- sub |> filter(year != 2020)

    ## Drop missing/infinite outcome
    sub <- sub |>
      filter(!is.na(.data[[outcome_var]]), !is.infinite(.data[[outcome_var]]))

    ## Ensure balanced panel
    max_t <- max(table(sub$fips))
    sub <- sub |>
      group_by(fips) |>
      filter(n() == max_t) |>
      ungroup()

    ## Residualize outcome on covariates (synthdid doesn't support covariates)
    covars <- c("population", "per_capita_income")
    covars_present <- covars[covars %in% names(sub)]
    if (length(covars_present) > 0) {
      fml <- as.formula(paste0(outcome_var, " ~ ", paste(covars_present, collapse = " + ")))
      fit <- lm(fml, data = sub, na.action = na.exclude)
      sub[[outcome_var]] <- residuals(fit) + mean(sub[[outcome_var]], na.rm = TRUE)
    }

    ## Build panel matrices
    panel <- tryCatch(
      synthdid::panel_matrices(
        panel = as.data.frame(sub),
        unit = "fips", time = "year",
        outcome = outcome_var, treatment = "Treated"
      ),
      error = function(e) NULL
    )
    if (is.null(panel)) return(NA_real_)

    ## SDID estimate
    est <- tryCatch(
      synthdid::synthdid_estimate(panel$Y, panel$N0, panel$T0),
      error = function(e) NULL
    )
    if (is.null(est)) return(NA_real_)
    as.numeric(est)
  }

  tau_agi <- sdid_run(sdid_df, "agi_net_rate_irs", "irs_sample_1")
  if (!is.na(tau_agi)) {
    ## tau is in percentage points (100 * ratio), convert to proportion
    effect_agi <- abs(tau_agi) / 100
    message("  SDID effect_agi: tau = ", round(tau_agi, 4),
            " pp -> effect = ", round(effect_agi, 4))
  } else {
    message("  SDID for effect_agi failed, using default: ", effect_agi)
  }

  ## ---- Run 2: effect_agi_oregon (state-level, interstate type 5) ----
  message("  Running SDID for effect_agi_oregon (agi_net_rate_irs5)...")

  tau_agi_oregon <- sdid_run(sdid_df, "agi_net_rate_irs5", "irs_sample_1")
  if (!is.na(tau_agi_oregon)) {
    effect_agi_oregon <- abs(tau_agi_oregon) / 100
    message("  SDID effect_agi_oregon: tau = ", round(tau_agi_oregon, 4),
            " pp -> effect = ", round(effect_agi_oregon, 4))
  } else {
    message("  SDID for effect_agi_oregon failed, using default: ", effect_agi_oregon)
  }

  rm(sdid_df)
} else {
  message("  SDID panel data not found. Using default effects:")
  message("    effect_agi = ", effect_agi)
  message("    effect_agi_oregon = ", effect_agi_oregon)
}

message("  Final parameters:")
message("    effect_agi         = ", round(effect_agi, 4))
message("    effect_agi_oregon  = ", round(effect_agi_oregon, 4))

# =============================================================================
# SECTION 1: Load 2019 ACS Microdata for Multnomah County
# =============================================================================

message("")
message("==============================================")
message("Section 1: Load ACS 2019 microdata")
message("==============================================")

acs_path <- file.path(data_acs, "acs_2019.csv")
require_file(acs_path, "ACS 2019 microdata")
acs <- read_csv(acs_path, show_col_types = FALSE)

## Lowercase column names
acs <- acs |> rename_with(tolower)

## Filter to Multnomah County, Oregon
acs <- acs |> filter(statefip == 41, countyfip == 51)

## Drop group quarters
acs <- acs |> filter(gq <= 2)

## Keep ages 18+
acs <- acs |> filter(age >= 18)

## Handle top-coded / missing income
topcode_vars <- c("inctot", "incwage", "incbus00", "incinvst", "incearn")
for (v in topcode_vars) {
  if (v %in% names(acs)) {
    acs[[v]] <- ifelse(acs[[v]] == 9999999, NA_real_, acs[[v]])
  }
}

## Create household ID
acs <- acs |> mutate(hh_id = serial)

message("ACS 2019 Multnomah County observations (18+, non-GQ): ", nrow(acs))

# =============================================================================
# SECTION 2: Create Tax Units
# =============================================================================

message("")
message("==============================================")
message("Section 2: Create tax units")
message("==============================================")

## ---- (a) Link married couples via SPLOC ----
acs <- acs |>
  mutate(
    unit_id = ifelse(marst == 1 & sploc != 0 & pernum > sploc, sploc, pernum)
  )

## Count individuals per tax unit
acs <- acs |>
  group_by(hh_id, unit_id) |>
  mutate(unit_ct = n()) |>
  ungroup()

## ---- (b) Filing status ----
acs <- acs |>
  mutate(
    married = as.integer(marst %in% c(1, 2)),
    mfs     = as.integer(marst == 3 & sploc == 0),
    filing_status = case_when(
      married == 1 ~ 2L,  # MFJ
      mfs == 1     ~ 6L,  # MFS
      TRUE         ~ 1L   # Single
    )
  )

## ---- (c) Dependents ----
acs <- acs |> mutate(depx = pmin(nchild, 3L))

## ---- (d) Income variable construction ----
acs <- acs |>
  mutate(
    incwage_nom  = pmax(incwage, 0, na.rm = TRUE),
    incse_nom    = ifelse(is.na(incearn) | is.na(incwage), 0,
                          incearn - incwage),
    incinvst_nom = ifelse(is.na(incinvst), 0, incinvst),
    inctot_raw   = ifelse(is.na(inctot), 0, inctot),
    incwel_nom   = ifelse(!is.na(incwelfr) & incwelfr != 999999, incwelfr, 0),
    ## Subtract untaxed welfare income, floor at 0
    inctot_nom   = pmax(inctot_raw - incwel_nom, 0)
  )

## Tax-unit level aggregation
acs <- acs |>
  group_by(hh_id, unit_id) |>
  mutate(
    inctot_tax   = sum(inctot_nom, na.rm = TRUE),
    incwage_tax  = sum(incwage_nom, na.rm = TRUE),
    incse_tax    = sum(incse_nom, na.rm = TRUE),
    incinvst_tax = sum(incinvst_nom, na.rm = TRUE)
  ) |>
  ungroup()

## ---- (e) Primary filer flag ----
acs <- acs |> mutate(primary_filer = as.integer(unit_id == pernum))

## ---- (f) AGI proxy ----
acs <- acs |> mutate(agi_proxy = inctot_tax)

n_tax_units <- sum(acs$primary_filer == 1)
message("Number of tax units: ", n_tax_units)

# =============================================================================
# SECTION 3: Load IRS County Data for Raking Targets
# =============================================================================

message("")
message("==============================================")
message("Section 3: Load IRS county data for raking")
message("==============================================")

irs_path <- file.path(data_irs, "19incyallagi.csv")
require_file(irs_path, "IRS county income data")
irs <- read_csv(irs_path, show_col_types = FALSE)

## Lowercase column names
irs <- irs |> rename_with(tolower)

## Ensure numeric FIPS
irs <- irs |>
  mutate(across(any_of(c("statefips", "countyfips", "agi_stub",
                         "n1", "mars2", "a00100", "a00200",
                         "n04470", "a04470")), as.numeric))

## Keep Multnomah County
irs <- irs |> filter(statefips == 41, countyfips == 51)

## Rename variables
irs <- irs |>
  rename(
    irs_n1          = n1,
    irs_mars2       = mars2,
    irs_agi         = a00100,
    irs_wages       = a00200,
    irs_n_itemizers = n04470,
    irs_itemded     = a04470
  )

## Rescale (IRS reports in thousands)
irs <- irs |>
  mutate(
    irs_agi     = irs_agi * 1000,
    irs_wages   = irs_wages * 1000,
    irs_itemded = irs_itemded * 1000
  )

## Keep relevant variables and drop "all" stub
irs_targets <- irs |>
  select(agi_stub, irs_n1, irs_mars2, irs_agi, irs_wages,
         irs_n_itemizers, irs_itemded) |>
  filter(agi_stub != 0)

message("IRS targets loaded:")
print(irs_targets |> select(agi_stub, irs_n1, irs_agi, irs_wages))

# =============================================================================
# SECTION 4: Raking / Calibration
# =============================================================================

message("")
message("==============================================")
message("Section 4: Raking / calibration")
message("==============================================")

## ---- (a) Create AGI brackets matching IRS stubs ----
filers <- acs |>
  filter(primary_filer == 1) |>
  mutate(
    agi_stub = case_when(
      agi_proxy < 1      ~ 1L,
      agi_proxy < 10000  ~ 2L,
      agi_proxy < 25000  ~ 3L,
      agi_proxy < 50000  ~ 4L,
      agi_proxy < 75000  ~ 5L,
      agi_proxy < 100000 ~ 6L,
      agi_proxy < 200000 ~ 7L,
      TRUE               ~ 8L
    )
  )

## ---- (b) Proper raking via survey::postStratify() ----

## Merge IRS targets
filers <- filers |>
  left_join(irs_targets, by = "agi_stub")

## Build survey design
des <- svydesign(id = ~1, weights = ~perwt, data = filers)

## Population totals for postStratification
pop_agi <- irs_targets |>
  select(agi_stub, Freq = irs_n1) |>
  mutate(agi_stub = factor(agi_stub))

## Ensure agi_stub is a factor in the design data
des$variables$agi_stub <- factor(des$variables$agi_stub)

## PostStratify
des_ps <- postStratify(des, ~agi_stub, pop_agi)

## Extract calibrated weights
filers$cal_wt <- weights(des_ps)

message("  Calibrated weight sum: ", round(sum(filers$cal_wt), 0))

## ---- Scale incomes within bracket to match IRS totals ----

filers <- filers |>
  group_by(agi_stub) |>
  mutate(
    ## AGI scaling (guard division by zero for stub 1 where ACS AGI = 0)
    acs_agi_sum  = sum(agi_proxy * cal_wt),
    agi_scale    = ifelse(acs_agi_sum == 0, 1, irs_agi / acs_agi_sum),
    agi_proxy    = agi_proxy * agi_scale,

    ## Wage scaling
    acs_wages_sum = sum(incwage_tax * cal_wt),
    wage_scale    = ifelse(acs_wages_sum == 0, 1, irs_wages / acs_wages_sum),
    incwage_nom   = incwage_nom * wage_scale,
    incwage_tax   = incwage_tax * wage_scale,

    ## Scale other income components proportionally to AGI scaling
    inctot_tax   = inctot_tax * agi_scale,
    incse_tax    = incse_tax * agi_scale,
    incinvst_tax = incinvst_tax * agi_scale,
    incse_nom    = incse_nom * agi_scale,
    inctot_nom   = inctot_nom * agi_scale
  ) |>
  ungroup()

## Drop temporary columns
filers <- filers |>
  select(-acs_agi_sum, -agi_scale, -acs_wages_sum, -wage_scale)

## ---- (c) Itemizer assignment ----
set.seed(56403)  # reproducibility
filers <- filers |>
  mutate(u_item = runif(n())) |>
  group_by(agi_stub) |>
  arrange(u_item, .by_group = TRUE) |>
  mutate(
    cumshare   = row_number() / n(),
    item_share = pmin(irs_n_itemizers / irs_n1, 1),
    itemizer   = as.integer(cumshare > (1 - item_share)),
    itemded_amt = ifelse(itemizer == 1 & irs_n_itemizers > 0,
                         irs_itemded / irs_n_itemizers, 0)
  ) |>
  ungroup() |>
  select(-u_item, -cumshare)

## ---- (d) Verification ----
message("")
message("Raking verification: ACS calibrated vs IRS targets")
message("---------------------------------------------------")

for (s in 1:8) {
  sub <- filers |> filter(agi_stub == s)
  acs_n <- sum(sub$cal_wt)
  irs_n <- sub$irs_n1[1]
  acs_agi_sum <- sum(sub$agi_proxy * sub$cal_wt)
  irs_agi_val <- sub$irs_agi[1]
  message(sprintf("Stub %d: ACS N=%10.0f vs IRS N=%10.0f  |  ACS AGI=%14.0f vs IRS AGI=%14.0f",
                  s, acs_n, irs_n, acs_agi_sum, irs_agi_val))
}

# =============================================================================
# SECTION 5: Inflate to 2022
# =============================================================================

message("")
message("==============================================")
message("Section 5: Inflate incomes to 2022 dollars")
message("==============================================")

inflate_vars <- c("incwage_nom", "incwage_tax", "incse_nom", "incse_tax",
                   "incinvst_nom", "incinvst_tax", "inctot_nom", "inctot_tax",
                   "agi_proxy", "itemded_amt", "incwel_nom")
for (v in inflate_vars) {
  if (v %in% names(filers)) {
    filers[[v]] <- filers[[v]] * cpi_2019_to_2022
  }
}

filers <- filers |> mutate(year = 2022L)

message("Income inflated from 2019 to 2022 using CPI factor: ", cpi_2019_to_2022)

# =============================================================================
# SECTION 6: Simplified Oregon Income Tax Calculator
# =============================================================================

message("")
message("==============================================")
message("Section 6: Tax calculation (simplified)")
message("==============================================")

## Filing status for tax calc
filers <- filers |>
  mutate(mstat = filing_status)

## Taxable income: AGI - standard deduction (or itemized deduction)
filers <- filers |>
  mutate(
    std_ded = ifelse(mstat == 2, 25900, 12950),
    deduction = ifelse(itemizer == 1, pmax(itemded_amt, std_ded), std_ded),
    taxable_income = pmax(agi_proxy - deduction, 0)
  )

## Simplified Oregon state income tax (2022 brackets, single schedule)
## 5% up to $3,750; 7% $3,750-$9,450; 9% $9,450-$125k; 9.9% above $125k
filers <- filers |>
  mutate(
    siitax = 0.05 * pmin(taxable_income, 3750) +
             0.07 * pmax(pmin(taxable_income, 9450) - 3750, 0) +
             0.09 * pmax(pmin(taxable_income, 125000) - 9450, 0) +
             0.099 * pmax(taxable_income - 125000, 0),
    fiitax = 0  # placeholder for federal tax
  )

message("Tax calculation complete (simplified Oregon tax schedule)")
message("  Mean Oregon state tax: $", round(weighted.mean(filers$siitax, filers$cal_wt), 0))

# =============================================================================
# SECTION 7: Multnomah PFA Tax Calculator
# =============================================================================

message("")
message("==============================================")
message("Section 7: PFA tax calculation")
message("==============================================")

filers <- filers |>
  mutate(
    pfa_thresh1 = ifelse(mstat == 2, pfa_thresh1_joint, pfa_thresh1_single),
    pfa_thresh2 = ifelse(mstat == 2, pfa_thresh2_joint, pfa_thresh2_single),
    pfa_tax = pfa_rate * pmax(taxable_income - pfa_thresh1, 0) +
              pfa_rate * pmax(taxable_income - pfa_thresh2, 0)
  )

message("PFA tax distribution:")
message("  Mean (all filers): $", round(weighted.mean(filers$pfa_tax, filers$cal_wt), 0))
pfa_payers <- filers |> filter(pfa_tax > 0)
if (nrow(pfa_payers) > 0) {
  message("  Mean (payers only): $",
          round(weighted.mean(pfa_payers$pfa_tax, pfa_payers$cal_wt), 0))
}

# =============================================================================
# SECTION 8: Baseline Revenue
# =============================================================================

message("")
message("==============================================")
message("Section 8: Baseline revenue")
message("==============================================")

baseline_pfa_revenue   <- sum(filers$pfa_tax * filers$cal_wt)
baseline_state_revenue <- sum(filers$siitax * filers$cal_wt)

message(sprintf("Baseline PFA revenue: $%s", formatC(baseline_pfa_revenue, format = "f",
                                                       big.mark = ",", digits = 0)))
message(sprintf("Baseline Oregon state income tax revenue: $%s",
                formatC(baseline_state_revenue, format = "f", big.mark = ",", digits = 0)))

## Flag impacted (subject to PFA tax)
filers <- filers |>
  mutate(impacted = as.integer(taxable_income > pfa_thresh1))

n_impacted <- sum(filers$impacted)
wt_impacted <- sum(filers$cal_wt[filers$impacted == 1])
message("Number of impacted tax units (unweighted): ", n_impacted)
message(sprintf("Weighted number of impacted filers: %s",
                formatC(wt_impacted, format = "f", big.mark = ",", digits = 0)))

## Save working data
save_data(filers, file.path(data_working, "revenue_microsim"))

# =============================================================================
# SECTION 9: Migration Revenue Effect -- Monte Carlo
# =============================================================================

message("")
message("==============================================")
message("Section 9: Monte Carlo revenue simulation")
message("==============================================")

## ---- (a) Compute X (AGI loss from migration effect) ----

message("  Loading IRS gross migration flows...")

flow_years <- c("1516", "1617", "1718", "1819", "1920")
flow_results <- list()

for (yr in flow_years) {
  ## Outflow: Multnomah = origin
  out_path <- file.path(data_irs, paste0("countyoutflow", yr, ".csv"))
  out_df <- read_csv(out_path, show_col_types = FALSE) |> rename_with(tolower)
  out_df <- out_df |>
    mutate(across(any_of(c("y1_statefips", "y1_countyfips",
                           "y2_statefips", "y2_countyfips",
                           "n1", "n2", "agi")), as.numeric))
  out_row <- out_df |>
    filter(y1_statefips == 41, y1_countyfips == 51,
           y2_statefips == 97, y2_countyfips == 0)
  out_agi <- out_row$agi[1] * 1000

  ## Inflow: Multnomah = destination
  in_path <- file.path(data_irs, paste0("countyinflow", yr, ".csv"))
  in_df <- read_csv(in_path, show_col_types = FALSE) |> rename_with(tolower)
  in_df <- in_df |>
    mutate(across(any_of(c("y1_statefips", "y1_countyfips",
                           "y2_statefips", "y2_countyfips",
                           "n1", "n2", "agi")), as.numeric))
  in_row <- in_df |>
    filter(y2_statefips == 41, y2_countyfips == 51,
           y1_statefips == 97, y1_countyfips == 0)
  in_agi <- in_row$agi[1] * 1000

  flow_results[[yr]] <- tibble(
    flow_year       = yr,
    out_agi         = out_agi,
    in_agi          = in_agi,
    net_outmig_agi  = out_agi - in_agi
  )
}

flows <- bind_rows(flow_results)

message("IRS gross migration flows for Multnomah County (pre-treatment):")
print(flows)

avg_net_outmig_agi <- mean(flows$net_outmig_agi)
message(sprintf("Average net out-migration AGI (nominal): $%s",
                formatC(avg_net_outmig_agi, format = "f", big.mark = ",", digits = 0)))

## Inflate to 2022
net_outmig_agi_2022 <- avg_net_outmig_agi * cpi_2019_to_2022
message(sprintf("Net out-migration AGI (2022 $): $%s",
                formatC(net_outmig_agi_2022, format = "f", big.mark = ",", digits = 0)))

## X = AGI loss from migration effect
X <- effect_agi * net_outmig_agi_2022
message(sprintf("AGI loss from %.1f%% migration effect (X): $%s",
                effect_agi * 100,
                formatC(X, format = "f", big.mark = ",", digits = 0)))

## ---- (b) Compute out-migration probability ----

agi_impacted <- sum(filers$agi_proxy[filers$impacted == 1] *
                    filers$cal_wt[filers$impacted == 1])
message(sprintf("Total AGI of impacted filers: $%s",
                formatC(agi_impacted, format = "f", big.mark = ",", digits = 0)))

p_migrate <- X / agi_impacted
message(sprintf("Migration probability (p): %.6f", p_migrate))

## ---- (c) Monte Carlo simulation ----

message("")
message(sprintf("Running %d Monte Carlo simulations...", n_sims))

set.seed(56403)  # reproducibility

sim_results <- tibble(
  sim_id        = integer(n_sims),
  pfa_revenue   = double(n_sims),
  state_revenue = double(n_sims)
)

for (s in seq_len(n_sims)) {
  if (s %% 100 == 0) message("  Simulation ", s, " of ", n_sims)

  ## Random draw: each impacted unit stays with prob (1-p)
  u_sim <- runif(nrow(filers))
  stays <- ifelse(filers$impacted == 1, as.integer(u_sim > p_migrate), 1L)

  ## Recompute revenues
  sim_pfa   <- sum(filers$pfa_tax[stays == 1] * filers$cal_wt[stays == 1])
  sim_state <- sum(filers$siitax[stays == 1] * filers$cal_wt[stays == 1])

  sim_results$sim_id[s]        <- s
  sim_results$pfa_revenue[s]   <- sim_pfa
  sim_results$state_revenue[s] <- sim_state
}

message("Monte Carlo simulation complete.")

## Save simulation results
save_data(sim_results, file.path(data_working, "revenue_simulations"))

# =============================================================================
# SECTION 10: Oregon State Revenue Effect
# =============================================================================

message("")
message("==============================================")
message("Section 10: Oregon state revenue effect")
message("==============================================")

total_state_tax_impacted <- sum(filers$siitax[filers$impacted == 1] *
                                filers$cal_wt[filers$impacted == 1])
total_agi_impacted <- agi_impacted
avg_state_rate <- total_state_tax_impacted / total_agi_impacted

message(sprintf("Average effective state tax rate on impacted: %.4f", avg_state_rate))

oregon_revenue_loss <- avg_state_rate * X
message(sprintf("Oregon revenue loss from migration effect: $%s",
                formatC(oregon_revenue_loss, format = "f", big.mark = ",", digits = 0)))

# =============================================================================
# SECTION 11: Output -- Tables & Figures
# =============================================================================

message("")
message("==============================================")
message("Section 11: Output tables and figures")
message("==============================================")

## ---- (a) Summary statistics ----

sim_results <- sim_results |>
  mutate(
    pfa_loss   = baseline_pfa_revenue - pfa_revenue,
    state_loss = baseline_state_revenue - state_revenue
  )

mean_pfa_loss     <- mean(sim_results$pfa_loss, na.rm = TRUE)
med_pfa_loss      <- median(sim_results$pfa_loss, na.rm = TRUE)
p5_pfa_loss       <- quantile(sim_results$pfa_loss, 0.05, na.rm = TRUE)
p95_pfa_loss      <- quantile(sim_results$pfa_loss, 0.95, na.rm = TRUE)
sd_pfa_loss       <- sd(sim_results$pfa_loss, na.rm = TRUE)
mean_state_loss   <- mean(sim_results$state_loss, na.rm = TRUE)
mean_pfa_rev_post <- mean(sim_results$pfa_revenue, na.rm = TRUE)
mean_state_rev_post <- mean(sim_results$state_revenue, na.rm = TRUE)

message("")
message("==================================================================")
message("REVENUE IMPACT SUMMARY")
message("==================================================================")
message("")
message(sprintf("Baseline PFA revenue:                    $%15s", formatC(baseline_pfa_revenue, format = "f", big.mark = ",", digits = 0)))
message(sprintf("Mean simulated PFA revenue (post-mig):   $%15s", formatC(mean_pfa_rev_post, format = "f", big.mark = ",", digits = 0)))
message(sprintf("Mean PFA revenue loss:                   $%15s", formatC(mean_pfa_loss, format = "f", big.mark = ",", digits = 0)))
message(sprintf("Median PFA revenue loss:                 $%15s", formatC(med_pfa_loss, format = "f", big.mark = ",", digits = 0)))
message(sprintf("5th percentile PFA loss:                 $%15s", formatC(p5_pfa_loss, format = "f", big.mark = ",", digits = 0)))
message(sprintf("95th percentile PFA loss:                $%15s", formatC(p95_pfa_loss, format = "f", big.mark = ",", digits = 0)))
message(sprintf("Std. dev. of PFA loss:                   $%15s", formatC(sd_pfa_loss, format = "f", big.mark = ",", digits = 0)))
message("")
message(sprintf("Baseline Oregon state revenue:           $%15s", formatC(baseline_state_revenue, format = "f", big.mark = ",", digits = 0)))
message(sprintf("Mean simulated state revenue (post-mig): $%15s", formatC(mean_state_rev_post, format = "f", big.mark = ",", digits = 0)))
message(sprintf("Mean Oregon state revenue loss:          $%15s", formatC(mean_state_loss, format = "f", big.mark = ",", digits = 0)))
message(sprintf("Oregon revenue loss (analytical):        $%15s", formatC(oregon_revenue_loss, format = "f", big.mark = ",", digits = 0)))
message("==================================================================")

## Export summary table to Excel
summary_tbl <- tibble(
  metric = c(
    "Baseline PFA revenue",
    "Mean simulated PFA revenue (post-mig)",
    "Mean PFA revenue loss",
    "Median PFA revenue loss",
    "5th percentile PFA loss",
    "95th percentile PFA loss",
    "Oregon state revenue loss (analytical)",
    "Mean Oregon state revenue loss (MC)"
  ),
  value = c(
    baseline_pfa_revenue,
    mean_pfa_rev_post,
    mean_pfa_loss,
    med_pfa_loss,
    p5_pfa_loss,
    p95_pfa_loss,
    oregon_revenue_loss,
    mean_state_loss
  )
)

tryCatch({
  write.xlsx(summary_tbl, file.path(results_revenue, "tbl_revenue_summary.xlsx"))
  message("  Summary table saved: tbl_revenue_summary.xlsx")
}, error = function(e) {
  warning("Failed to write summary table: ", conditionMessage(e))
})

## ---- (b) Histogram of simulated PFA revenue losses ----

p_hist <- ggplot(sim_results, aes(x = pfa_loss)) +
  geom_histogram(fill = alpha("navy", 0.7), color = "navy", bins = 30) +
  labs(
    title = "Distribution of Simulated PFA Revenue Losses",
    subtitle = sprintf("%s Monte Carlo draws, %.1f%% migration effect",
                       formatC(n_sims, big.mark = ","), effect_agi * 100),
    x = "PFA Revenue Loss ($)",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave(file.path(results_revenue, "fig_pfa_revenue_distribution.pdf"),
       p_hist, width = 8, height = 5)
ggsave(file.path(results_revenue, "fig_pfa_revenue_distribution.png"),
       p_hist, width = 8, height = 5, dpi = 300)

message("  Histogram saved: fig_pfa_revenue_distribution.pdf/png")

## ---- (c) Table by AGI bracket ----

bracket_tbl <- filers |>
  group_by(agi_stub) |>
  summarize(
    n_filers    = sum(cal_wt),
    n_impacted  = sum(cal_wt[impacted == 1]),
    total_pfa   = sum(pfa_tax * cal_wt),
    total_agi   = sum(agi_proxy * cal_wt),
    .groups = "drop"
  ) |>
  mutate(
    avg_pfa   = ifelse(n_impacted > 0, total_pfa / n_impacted, 0),
    pfa_share = total_pfa / sum(total_pfa) * 100
  )

message("PFA tax by AGI bracket:")
print(bracket_tbl |> select(agi_stub, n_filers, n_impacted, avg_pfa, total_pfa, pfa_share))

tryCatch({
  write.xlsx(as.data.frame(bracket_tbl),
             file.path(results_revenue, "tbl_pfa_by_bracket.xlsx"))
  message("  Bracket table saved: tbl_pfa_by_bracket.xlsx")
}, error = function(e) {
  warning("Failed to write bracket table: ", conditionMessage(e))
})

## ---- (d) Summary by filing status ----

filing_tbl <- filers |>
  group_by(mstat) |>
  summarize(
    n_filers   = sum(cal_wt),
    n_impacted = sum(cal_wt[impacted == 1]),
    total_pfa  = sum(pfa_tax * cal_wt),
    .groups = "drop"
  ) |>
  mutate(
    mstat_label = case_when(
      mstat == 1 ~ "Single",
      mstat == 2 ~ "MFJ",
      mstat == 6 ~ "MFS",
      TRUE       ~ as.character(mstat)
    )
  )

message("PFA tax by filing status:")
print(filing_tbl)

# =============================================================================
# FINISH
# =============================================================================

message("")
message("==============================================")
message("02_revenue.R complete.")
message("Output files:")
message("  ", file.path(results_revenue, "tbl_revenue_summary.xlsx"))
message("  ", file.path(results_revenue, "fig_pfa_revenue_distribution.pdf"))
message("  ", file.path(results_revenue, "tbl_pfa_by_bracket.xlsx"))
message("  ", file.path(data_working, "revenue_microsim.rds"))
message("  ", file.path(data_working, "revenue_simulations.rds"))
message("==============================================")
