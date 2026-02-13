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
#          Oregon income tax revenue, and estimates revenue loss from
#          tax-induced out-migration.
#
# Key changes from Stata version:
#   - Income adjustment: subtract incwel_nom from inctot before AGI proxy
#   - GREG calibration: survey::calibrate(calfun="linear") to match N, MFJ, AGI, wages
#   - SDID runs: estimate effect_agi and effect_agi_oregon from panel data
#
# Outputs:
#   - results/revenue/tbl_revenue_summary.xlsx
#   - results/revenue/tbl_pfa_by_bracket.xlsx
#   - data/working/revenue_microsim.rds
#
# Requirements:
#   - survey, synthdid, dplyr, readr, openxlsx
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
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

## ---- (b) GREG calibration (weights match IRS counts + AGI + wages by bracket) ----

## Merge IRS targets
filers <- filers |>
  left_join(irs_targets, by = "agi_stub")

## Create auxiliary variables for GREG calibration (32 constraints)
filers <- filers |>
  mutate(
    is_mfj   = as.integer(filing_status == 2),
    agi_stub = as.integer(agi_stub)
  )

for (s in 1:8) {
  filers[[paste0("d_stub_", s)]]    <- as.integer(filers$agi_stub == s)
  filers[[paste0("d_mfj_", s)]]     <- as.integer(filers$agi_stub == s) * filers$is_mfj
  filers[[paste0("agi_stub_", s)]]  <- filers$agi_proxy * as.integer(filers$agi_stub == s)
  filers[[paste0("wages_stub_", s)]] <- filers$incwage_tax * as.integer(filers$agi_stub == s)
}

## Build survey design
des <- svydesign(id = ~1, weights = ~perwt, data = filers)

## Population totals (length 32)
pop_totals <- c(
  setNames(irs_targets$irs_n1,    paste0("d_stub_", 1:8)),
  setNames(irs_targets$irs_mars2, paste0("d_mfj_", 1:8)),
  setNames(irs_targets$irs_agi,   paste0("agi_stub_", 1:8)),
  setNames(irs_targets$irs_wages, paste0("wages_stub_", 1:8))
)

## GREG calibration (linear distance function)
cal_formula <- as.formula(
  paste0("~ ", paste(names(pop_totals), collapse = " + "), " - 1")
)
des_cal <- calibrate(des, cal_formula, population = pop_totals,
                     calfun = "linear", bounds = c(0.3, 5),
                     maxit = 100, epsilon = 1e-6)

## Extract calibrated weights
filers$cal_wt <- weights(des_cal)

## Clean up auxiliary columns
aux_cols <- c(paste0("d_stub_", 1:8), paste0("d_mfj_", 1:8),
              paste0("agi_stub_", 1:8), paste0("wages_stub_", 1:8))
filers <- filers |> select(-all_of(aux_cols))

message("  Calibrated weight sum: ", round(sum(filers$cal_wt), 0))
message("  Weight ratio summary (cal_wt / perwt):")
message("    ", paste(capture.output(summary(filers$cal_wt / filers$perwt)), collapse = "\n    "))

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
message("GREG calibration verification: ACS calibrated vs IRS targets")
message("-------------------------------------------------------------")

for (s in 1:8) {
  sub <- filers |> filter(as.integer(agi_stub) == s)
  message(sprintf("Stub %d: N=%10.0f vs %10.0f | MFJ=%8.0f vs %8.0f | AGI=%14.0f (IRS %14.0f) | Wages=%14.0f (IRS %14.0f)",
    s,
    sum(sub$cal_wt), sub$irs_n1[1],
    sum(sub$cal_wt * sub$is_mfj), sub$irs_mars2[1],
    sum(sub$cal_wt * sub$agi_proxy), sub$irs_agi[1],
    sum(sub$cal_wt * sub$incwage_tax), sub$irs_wages[1]
  ))
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
# SECTION 9: Migration Revenue Effect
# =============================================================================

message("")
message("==============================================")
message("Section 9: Migration Revenue Effects")
message("==============================================")

## ---- (a) Total AGI stock for Multnomah County ----

## The SDID coefficient represents the treatment effect on
## net AGI migration as a share of total AGI:
##   outcome = (AGI_out - AGI_in) / AGI_total
## So: additional_AGI_loss = coefficient * total_AGI_stock
## We use IRS-reported total AGI (2019) inflated to 2022.

total_agi_multnomah <- sum(irs_targets$irs_agi)
total_agi_multnomah_2022 <- total_agi_multnomah * cpi_2019_to_2022

message(sprintf("Total Multnomah County AGI (2019 IRS): $%s",
                formatC(total_agi_multnomah, format = "f", big.mark = ",", digits = 0)))
message(sprintf("Total Multnomah County AGI (2022 $):   $%s",
                formatC(total_agi_multnomah_2022, format = "f", big.mark = ",", digits = 0)))

## ---- (b) AGI loss from migration effect ----

## X_1: overall domestic migration (IRS type 3, effect_agi)
## X_2: out-of-state migration (IRS type 5, effect_agi_oregon)
X_1 <- effect_agi * total_agi_multnomah_2022
X_2 <- effect_agi_oregon * total_agi_multnomah_2022

message(sprintf("AGI loss from %.2f pp overall migration effect (X_1): $%s",
                effect_agi * 100,
                formatC(X_1, format = "f", big.mark = ",", digits = 0)))
message(sprintf("AGI loss from %.2f pp out-of-state effect (X_2): $%s",
                effect_agi_oregon * 100,
                formatC(X_2, format = "f", big.mark = ",", digits = 0)))

## ---- (c) Out-migration probability ----

agi_impacted <- sum(filers$agi_proxy[filers$impacted == 1] *
                    filers$cal_wt[filers$impacted == 1])
message(sprintf("Total AGI of impacted filers: $%s",
                formatC(agi_impacted, format = "f", big.mark = ",", digits = 0)))

p_migrate <- X_1 / agi_impacted
p_migrate_state <- X_2 / agi_impacted
message(sprintf("Migration probability (p): %.6f", p_migrate))
message(sprintf("Out-of-state migration probability (p_state): %.6f", p_migrate_state))

# =============================================================================
# SECTION 10: Oregon and Multnomah Revenue Effects
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

## Oregon loses revenue only from interstate departures (X_2)
oregon_revenue_loss <- avg_state_rate * X_2
message(sprintf("Oregon revenue loss from migration effect: $%s",
                formatC(oregon_revenue_loss, format = "f", big.mark = ",", digits = 0)))

message("")
message("==============================================")
message("Section 10: Multnomah county revenue effect")
message("==============================================")

## Multnomah loses PFA revenue from all domestic departures (X_1)
total_mt_tax_impacted <- sum(filers$pfa_tax[filers$impacted == 1] *
                             filers$cal_wt[filers$impacted == 1])
avg_mt_rate <- total_mt_tax_impacted / total_agi_impacted

message(sprintf("Average effective PFA tax rate on impacted: %.4f", avg_mt_rate))

mt_revenue_loss <- avg_mt_rate * X_1
message(sprintf("Multnomah county revenue loss from migration effect: $%s",
                formatC(mt_revenue_loss, format = "f", big.mark = ",", digits = 0)))

# =============================================================================
# SECTION 11: Output -- Tables
# =============================================================================

message("")
message("==============================================")
message("Section 11: Output tables")
message("==============================================")

## ---- (a) Summary ----

message("")
message("==================================================================")
message("REVENUE IMPACT SUMMARY")
message("==================================================================")
message("")
message(sprintf("Baseline PFA revenue:                    $%15s", formatC(baseline_pfa_revenue, format = "f", big.mark = ",", digits = 0)))
message(sprintf("Baseline Oregon state revenue:           $%15s", formatC(baseline_state_revenue, format = "f", big.mark = ",", digits = 0)))
message("")
message(sprintf("Multnomah PFA revenue loss (analytical): $%15s", formatC(mt_revenue_loss, format = "f", big.mark = ",", digits = 0)))
message(sprintf("Oregon state revenue loss (analytical):  $%15s", formatC(oregon_revenue_loss, format = "f", big.mark = ",", digits = 0)))
message("==================================================================")

## Export summary table to Excel
summary_tbl <- tibble(
  metric = c(
    "Baseline PFA revenue",
    "Baseline Oregon state revenue",
    "Multnomah PFA revenue loss (analytical)",
    "Oregon state revenue loss (analytical)"
  ),
  value = c(
    baseline_pfa_revenue,
    baseline_state_revenue,
    mt_revenue_loss,
    oregon_revenue_loss
  )
)

tryCatch({
  write.xlsx(summary_tbl, file.path(results_revenue, "tbl_revenue_summary.xlsx"))
  message("  Summary table saved: tbl_revenue_summary.xlsx")
}, error = function(e) {
  warning("Failed to write summary table: ", conditionMessage(e))
})

## ---- (b) Table by AGI bracket ----

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

## ---- (c) Summary by filing status ----

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
message("  ", file.path(results_revenue, "tbl_pfa_by_bracket.xlsx"))
message("  ", file.path(data_working, "revenue_microsim.rds"))
message("==============================================")
