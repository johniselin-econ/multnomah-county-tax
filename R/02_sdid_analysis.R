# =============================================================================
# 02_sdid_analysis.R
# Synthetic Difference-in-Differences estimation for Multnomah County Tax
#
# Converted from: code/02_sdid_analysis.do
# Author: John Iselin
#
# Purpose: Perform synthetic difference-in-difference estimation.
#          Loops over data sources (IRS/ACS), outcome types (n1/n2/agi),
#          migration directions (net/in/out), samples, covariate settings,
#          and exclusion of 2020 year.
#
# Outputs:
#   - sdid_weights.rds/xlsx: Synthetic control weights for each specification
#   - sdid_results.rds/xlsx: Treatment effects, SEs, p-values for each spec
#   - fig_speccurve_*.pdf/jpg: Specification curve plots
#   - fig_*_eventstudy.jpg: Event study plots
#   - tab_sdid_*.tex: LaTeX tables
#
# Requirements:
#   - synthdid package
#   - modelsummary package
#   - patchwork package
#   - tidyverse
#
# TODO:
# 1) Recreate IRS sample with solely counties in ACS sample.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(ggplot2)
  library(synthdid)
  library(modelsummary)
  library(patchwork)
  library(openxlsx)
})

# Source configuration
source(file.path(r_dir, "utils", "globals.R"))
source(file.path(r_dir, "utils", "helpers.R"))

message("=== 02_sdid_analysis.R: Starting SDID analysis ===")
message("  Run date: ", run_date)

# =============================================================================
# CONFIGURATION
# =============================================================================

## Number of bootstrap replications for SDID
reps <- sdid_reps  # from globals.R (default 100)

# =============================================================================
# DATA PREPARATION
# =============================================================================

message("STEP 1: Loading and merging data...")

## Load IRS county gross migration data
irs_data <- load_data(file.path(data_working, "irs_county_gross"))

## Keep required variables: year, fips, state/county identifiers, and
## migration variables for move type 3 (domestic) plus out types 1 & 2
irs_keep_pattern <- "^(year|fips|state_|county_|.*_net_3$|.*_out_1$|.*_out_2$|.*_in_3$|.*_out_3$|.*_net_5$|.*_in_5$|.*_out_5$)"
irs_data <- irs_data |>
  select(matches(irs_keep_pattern))

## Merge with ACS data (18+ population)
acs1_data <- load_data(file.path(data_working, "acs_county_gross_25plus"))
df <- irs_data |>
  left_join(acs1_data, by = c("year", "fips"), suffix = c("", ".acs1")) |>
  mutate(merge_acs_1 = if_else(!is.na(persons_net_3.acs1) | !is.na(persons_out_1.acs1), 3L, 1L))

## Keep required variables and label ACS1 columns
## Rename ACS 18+ columns with acs1_ prefix
acs1_vars <- names(acs1_data)[str_detect(names(acs1_data), "^(persons|households|dollars)_")]
for (v in acs1_vars) {
  acs1_col <- paste0(v, ".acs1")
  new_name <- paste0("acs1_", v)
  if (acs1_col %in% names(df)) {
    df <- df |> rename(!!new_name := !!acs1_col)
  } else if (v %in% names(df) && !str_starts(v, "acs")) {
    # If no suffix collision, the ACS column may have merged cleanly
    # Only rename if it came from the ACS merge
  }
}

## Clean up any remaining .acs1 suffixed columns and duplicate name columns
acs1_suffix_cols <- names(df)[str_detect(names(df), "\\.acs1$")]
if (length(acs1_suffix_cols) > 0) {
  for (col in acs1_suffix_cols) {
    base_name <- str_remove(col, "\\.acs1$")
    new_name <- paste0("acs1_", base_name)
    if (!new_name %in% names(df)) {
      df <- df |> rename(!!new_name := !!col)
    } else {
      df <- df |> select(-all_of(col))
    }
  }
}

## Drop non-acs prefixed persons/households/dollars columns that came from ACS
## (keep irs n1/n2/agi columns, drop bare persons/households/dollars from ACS merge)
bare_acs_cols <- names(df)[str_detect(names(df), "^(persons|households|dollars)_") &
                             !str_detect(names(df), "^acs")]
if (length(bare_acs_cols) > 0) {
  # Rename them to acs1_ prefix
  for (v in bare_acs_cols) {
    new_name <- paste0("acs1_", v)
    if (!new_name %in% names(df)) {
      df <- df |> rename(!!new_name := !!v)
    }
  }
}

## Merge with ACS college data
acs2_data <- load_data(file.path(data_working, "acs_county_gross_college"))
acs2_vars <- names(acs2_data)[str_detect(names(acs2_data), "^(persons|households|dollars)_")]

## Add acs2_ prefix before merging
acs2_renamed <- acs2_data |>
  rename_with(~ paste0("acs2_", .), all_of(acs2_vars))

df <- df |>
  left_join(
    acs2_renamed |> select(year, fips, starts_with("acs2_")),
    by = c("year", "fips")
  ) |>
  mutate(merge_acs_2 = if_else(
    rowSums(!is.na(select(pick(starts_with("acs2_")), 1))) > 0, 3L, 1L
  ))

## Drop "other counties" (county_fips == 0) and year 2015
df <- df |>
  filter(county_from_fips(fips) != 0) |>
  filter(year != 2015)

## Merge with demographic data (cross-sectional, 2020)
message("  Merging demographic data...")
demographics <- load_data(file.path(data_working, "demographics_2020"))
df <- df |>
  inner_join(
    demographics |> select(fips, population, percent_urban, median_income),
    by = "fips"
  )

## Rename population from demographics to pop_census
df <- df |> rename(pop_census = population)

## Merge with BEA economic data (panel)
message("  Merging BEA economic data...")
bea <- load_data(file.path(data_working, "bea_economics"))
df <- df |>
  inner_join(
    bea |> select(fips, year, population, per_capita_income),
    by = c("fips", "year")
  )

## Merge with COVID-19 data (cross-sectional wide)
message("  Merging COVID-19 data...")
covid_wide <- load_data(file.path(data_working, "covid_cleaned_wide"))

## Track which counties have covid data
covid_fips <- unique(covid_wide$fips)
df <- df |>
  left_join(
    covid_wide |> select(-any_of(c("state_name", "county_name"))),
    by = "fips"
  ) |>
  mutate(covid_merge = if_else(fips %in% covid_fips, 3L, 1L))

## Merge with property tax rates (panel, time-varying)
message("  Merging property tax rates...")
prop_tax <- load_data(file.path(data_working, "property_tax_rates_overall"))
df <- df |>
  left_join(
    prop_tax |> select(year, fips, prop_rate_mean, prop_rate_se),
    by = c("year", "fips")
  ) |>
  mutate(proptx_merge = if_else(!is.na(prop_rate_mean), 3L, 1L))

## Rename for clarity
df <- df |>
  rename(
    prop_tax_rate    = prop_rate_mean,
    prop_tax_rate_se = prop_rate_se
  )

## Sort and verify panel structure
df <- df |> arrange(fips, year)
stopifnot("Panel not uniquely identified by fips-year" =
            nrow(df) == nrow(distinct(df, fips, year)))

## Drop observations with missing or zero base populations (pre-2023)
df <- df |>
  filter(!(is.na(n1_out_1) | n1_out_1 == 0) | year > 2022)

## Keep only counties observed in every year (balanced panel)
df <- df |>
  group_by(fips) |>
  filter(n() >= 7) |>
  ungroup()

## Generate sample indicators
message("  Generating sample indicators...")

## IRS sample 1: all counties, 2016-2022
df <- df |>
  mutate(irs_sample_1 = as.integer(between(year, 2016, 2022)))

## IRS sample 2: counties also in ACS, 2016-2022
df <- df |>
  mutate(irs_sample_2 = as.integer(between(year, 2016, 2022) & merge_acs_1 != 1))

## ACS period 1: counties in ACS, 2016-2022
df <- df |>
  mutate(acs_period_1 = as.integer(merge_acs_1 != 1 & between(year, 2016, 2022)))

## ACS period 2: all ACS-matched years
df <- df |>
  mutate(acs_period_2 = as.integer(merge_acs_1 != 1))

## Ensure balanced panel for ACS counties
df <- df |>
  mutate(tmp = as.integer(merge_acs_1 != 1)) |>
  group_by(fips) |>
  mutate(ct_tmp = sum(tmp)) |>
  ungroup() |>
  mutate(
    acs_period_1 = if_else(ct_tmp != 9, 0L, acs_period_1),
    acs_period_2 = if_else(ct_tmp != 9, 0L, acs_period_2),
    irs_sample_2 = if_else(ct_tmp != 9, 0L, irs_sample_2)
  ) |>
  select(-tmp, -ct_tmp)

## Define treated unit
df <- df |>
  mutate(
    multnomah = as.integer(fips == multnomah_fips),
    Treated   = as.integer(multnomah == 1 & year > treatment_year)
  )

## Define donor pool samples

## Sample 1: All counties
df <- df |> mutate(sample_all = 1L)

## Sample 2: Counties in top 5% urban
cutoff_95 <- quantile(
  df$percent_urban[df$year == 2020], 0.95, na.rm = TRUE
)
df <- df |>
  mutate(sample_urban95 = as.integer(percent_urban >= cutoff_95))

## Sample 3: Counties in top 2% urban (hard-coded cutoff from Stata)
cutoff_98 <- 0.9864539
df <- df |>
  mutate(sample_urban98 = as.integer(percent_urban >= cutoff_98))

## Drop excluded states (Alaska, Hawaii, California, Washington, non-Multnomah Oregon)
df <- df |>
  filter(!state_name %in% c("Alaska", "Hawaii", "California", "Washington")) |>
  filter(!(state_name == "Oregon" & multnomah == 0))

## Sample 4: Urban counties with COVID k-means matching
message("  Running k-means clustering on COVID data...")

## Identify COVID clustering variables
covid_cluster_cols <- names(df)[str_detect(names(df), "^(cases_cum|deaths_cum)_\\d+$")]

## Subset for clustering: urban counties with COVID data, year 2020
cluster_df <- df |>
  filter(sample_urban95 == 1, year == 2020, covid_merge == 3) |>
  select(fips, multnomah, all_of(covid_cluster_cols)) |>
  drop_na()

## Run k-means with k=5
cluster_mat <- cluster_df |> select(all_of(covid_cluster_cols)) |> as.matrix()

set.seed(56403)
km_fit <- kmeans(cluster_mat, centers = 5, nstart = 25)
cluster_df$kmean <- km_fit$cluster

## Identify cluster containing Multnomah County
mult_cluster <- cluster_df$kmean[cluster_df$multnomah == 1]

## Create sample indicator
cluster_df <- cluster_df |>
  mutate(in_covid_sample = as.integer(kmean == mult_cluster)) |>
  select(fips, kmean, in_covid_sample)

## Merge k-means results back (propagate across years)
df <- df |>
  left_join(cluster_df |> select(fips, kmean_group = kmean, in_covid_sample),
            by = "fips") |>
  mutate(
    sample_urban95_covid = as.integer(
      sample_urban95 == 1 & !is.na(in_covid_sample) & in_covid_sample == 1
    )
  )

## Generate and export k-means diagnostic plot
message("  Generating k-means diagnostic plot...")
kmeans_plot_data <- df |>
  filter(sample_urban95 == 1, year == 2020, covid_merge == 3, !is.na(kmean_group)) |>
  select(kmean_group, population, all_of(covid_cluster_cols))

if (nrow(kmeans_plot_data) > 0) {
  ## Collapse by cluster (population-weighted mean)
  kmeans_summary <- kmeans_plot_data |>
    group_by(kmean_group) |>
    summarize(
      across(all_of(covid_cluster_cols), ~ weighted.mean(., population, na.rm = TRUE)),
      .groups = "drop"
    )

  ## Reshape long for plotting
  kmeans_long <- kmeans_summary |>
    pivot_longer(
      cols = all_of(covid_cluster_cols[str_detect(covid_cluster_cols, "^cases_cum")]),
      names_to = "time_var",
      values_to = "cases_cum"
    ) |>
    mutate(time = as.integer(str_extract(time_var, "\\d+$")))

  p_kmeans <- ggplot(kmeans_long, aes(x = time, y = cases_cum,
                                       color = factor(kmean_group))) +
    geom_line(linewidth = 0.8) +
    labs(x = "Month", y = "Cumulative cases per 1,000",
         color = "K-means cluster") +
    theme_minimal()

  dir.create(results_sdid, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(results_sdid, "fig_kmeans.jpg"), p_kmeans,
         width = 8, height = 5, dpi = 100)
}

## Debug mode: limit donor pool to a random subset of counties
if (!exists("debug")) debug <- FALSE
debug_n <- 20  # Number of donor counties in debug mode

if (debug) {
  message("  *** DEBUG MODE: Limiting donor pool to ", debug_n, " random counties ***")
  set.seed(12345)
  non_mult_fips <- unique(df$fips[df$multnomah == 0])
  sampled_fips <- sample(non_mult_fips, min(debug_n, length(non_mult_fips)))
  keep_fips <- c(multnomah_fips, sampled_fips)
  df <- df |> filter(fips %in% keep_fips)
}

## Standardize covariates
message("  Standardizing covariates...")
all_covariates <- c("population", "per_capita_income", "prop_tax_rate")
for (v in all_covariates) {
  if (v %in% names(df)) {
    df[[v]] <- as.numeric(scale(df[[v]]))
  }
}

## Define outcome variables (IRS)
message("  Computing migration rate outcome variables...")

for (x in c("n1", "n2", "agi")) {
  for (y in c("net", "in", "out")) {
    ## Numerator: domestic movers (type 3)
    num_col <- paste0(x, "_", y, "_3")
    ## Denominator: out type 1 (stayers) + out type 2 (all movers)
    denom_col1 <- paste0(x, "_out_1")
    denom_col2 <- paste0(x, "_out_2")
    ## Outcome variable name
    out_col <- paste0(x, "_", y, "_rate_irs")

    if (all(c(num_col, denom_col1, denom_col2) %in% names(df))) {
      df[[out_col]] <- 100 * (df[[num_col]] / (df[[denom_col1]] + df[[denom_col2]]))
    }
  }
}

## Define outcome variables (IRS, interstate movers - type 5)
for (x in c("n1", "n2", "agi")) {
  for (y in c("net", "in", "out")) {
    num_col <- paste0(x, "_", y, "_5")
    denom_col1 <- paste0(x, "_out_1")
    denom_col2 <- paste0(x, "_out_2")
    out_col <- paste0(x, "_", y, "_rate_irs5")

    if (all(c(num_col, denom_col1, denom_col2) %in% names(df))) {
      df[[out_col]] <- 100 * (df[[num_col]] / (df[[denom_col1]] + df[[denom_col2]]))
    }
  }
}

## Define outcome variables (ACS)
## Rename ACS variable prefixes: households -> n1, persons -> n2, dollars -> agi
for (i in 1:2) {
  prefix <- paste0("acs", i, "_")

  ## Get the current ACS column names for this sample
  acs_cols <- names(df)[str_starts(names(df), prefix)]

  for (col_name in acs_cols) {
    new_name <- col_name |>
      str_replace(paste0(prefix, "households_"), paste0(prefix, "n1_")) |>
      str_replace(paste0(prefix, "persons_"), paste0(prefix, "n2_")) |>
      str_replace(paste0(prefix, "dollars_"), paste0(prefix, "agi_"))
    if (new_name != col_name) {
      df <- df |> rename(!!new_name := !!col_name)
    }
  }

  ## Compute migration rates for each ACS sample
  acs_suffix <- paste0("acs", i)
  for (x in c("n1", "n2", "agi")) {
    for (y_dir in c("net", "in", "out")) {
      num_col <- paste0(prefix, x, "_", y_dir, "_3")
      denom_col1 <- paste0(prefix, x, "_out_1")
      denom_col2 <- paste0(prefix, x, "_out_2")
      out_col <- paste0(x, "_", y_dir, "_rate_", acs_suffix)

      if (all(c(num_col, denom_col1, denom_col2) %in% names(df))) {
        df[[out_col]] <- 100 * (df[[num_col]] / (df[[denom_col1]] + df[[denom_col2]]))
      }
    }
  }
}

## Tag unique fips (for diagnostics)
df <- df |>
  group_by(fips) |>
  mutate(unique_tag = row_number() == 1) |>
  ungroup()

message("  Data preparation complete. Panel dimensions: ",
        n_distinct(df$fips), " counties x ",
        n_distinct(df$year), " years = ",
        nrow(df), " obs")

# =============================================================================
# SDID ESTIMATION
# =============================================================================

message("STEP 2: Running SDID estimation...")

## Create output directories
dir.create(results_sdid, recursive = TRUE, showWarnings = FALSE)
out_txt_labels <- c("irs_full_16_22", "irs5_full_16_22",
                     "irs_389_16_22", "irs5_389_16_22",
                     "acs_16_22_all", "acs_16_22_col",
                     "acs_16_24_all", "acs_16_24_col")
for (lbl in out_txt_labels) {
  dir.create(file.path(results_sdid, lbl), recursive = TRUE, showWarnings = FALSE)
}

## Initialize results storage
all_results <- tibble(
  sample_data  = character(),
  sample       = character(),
  outcome      = character(),
  controls     = integer(),
  exclusion    = integer(),
  tau          = double(),
  se           = double(),
  pval         = double(),
  ci_lower     = double(),
  ci_upper     = double(),
  n_counties   = integer(),
  pre_mean     = double(),
  significant  = integer()
)

all_weights <- tibble(
  fips        = double(),
  weight      = double(),
  sample_data = character(),
  out         = character(),
  sample      = character(),
  controls    = integer(),
  exclusion   = integer()
)

## Define the data/outcome type mapping
data_specs <- list(
  list(data_var = "irs_sample_1", out_types = c("irs", "irs5"),
       covariates = c("population", "per_capita_income")),
  list(data_var = "irs_sample_2", out_types = c("irs", "irs5"),
       covariates = c("population", "per_capita_income", "prop_tax_rate")),
  list(data_var = "acs_period_1", out_types = c("acs1", "acs2"),
       covariates = c("population", "per_capita_income", "prop_tax_rate")),
  list(data_var = "acs_period_2", out_types = c("acs1", "acs2"),
       covariates = c("population", "per_capita_income", "prop_tax_rate"))
)

## Helper: determine output text label
get_out_txt <- function(data_var, type) {
  if (data_var == "irs_sample_1" && type == "irs") return("irs_full_16_22")
  if (data_var == "irs_sample_1" && type == "irs5") return("irs5_full_16_22")
  if (data_var == "irs_sample_2" && type == "irs") return("irs_389_16_22")
  if (data_var == "irs_sample_2" && type == "irs5") return("irs5_389_16_22")
  if (data_var == "acs_period_1" && type == "acs1") return("acs_16_22_all")
  if (data_var == "acs_period_1" && type == "acs2") return("acs_16_22_col")
  if (data_var == "acs_period_2" && type == "acs1") return("acs_16_24_all")
  if (data_var == "acs_period_2" && type == "acs2") return("acs_16_24_col")
  return("unknown")
}

## Helper: run a single SDID specification
run_single_sdid <- function(df, outcome_var, unit_var, time_var, treat_var,
                             sample_mask, covariates = NULL, use_covars = FALSE,
                             reps = 100) {
  ## Subset to sample
  sub <- df |> filter(sample_mask)

  ## Check we have treated and control units
  n_treated <- sum(sub$multnomah == 1)
  n_control <- sum(sub$multnomah == 0)
  if (n_treated == 0 || n_control == 0) {
    return(list(success = FALSE, msg = "No treated or control units in sample"))
  }

  ## Drop rows with missing outcome
  sub <- sub |> filter(!is.na(.data[[outcome_var]]))

  ## Replace infinite values with NA and drop
  sub <- sub |>
    mutate(!!outcome_var := if_else(is.infinite(.data[[outcome_var]]),
                                     NA_real_, .data[[outcome_var]])) |>
    filter(!is.na(.data[[outcome_var]]))

  ## Ensure balanced panel
  sub <- sub |>
    group_by(.data[[unit_var]]) |>
    filter(n() == max(table(sub[[unit_var]]))) |>
    ungroup()

  if (nrow(sub) == 0) {
    return(list(success = FALSE, msg = "Empty sample after cleaning"))
  }

  ## Create panel matrices for synthdid
  panel <- tryCatch({
    panel_matrices(
      panel = as.data.frame(sub),
      unit = unit_var,
      time = time_var,
      outcome = outcome_var,
      treatment = treat_var
    )
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(panel)) {
    return(list(success = FALSE, msg = "panel_matrices() failed"))
  }

  ## Run SDID estimation
  est <- tryCatch({
    synthdid_estimate(panel$Y, panel$N0, panel$T0)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(est)) {
    return(list(success = FALSE, msg = "synthdid_estimate() failed"))
  }

  ## Get ATT and SE via placebo variance
  tau_hat <- as.numeric(est)
  se_hat <- tryCatch({
    sqrt(vcov(est, method = "placebo"))
  }, error = function(e) {
    NA_real_
  })

  ## Extract unit weights
  weights_raw <- tryCatch({
    wt <- attr(est, "weights")
    omega <- wt$omega
    unit_names <- colnames(panel$Y)[1:panel$N0]
    tibble(
      fips = as.numeric(unit_names),
      weight = as.numeric(omega)
    ) |>
      filter(weight != 0, !is.na(fips))
  }, error = function(e) {
    tibble(fips = double(), weight = double())
  })

  ## Compute pre/post effects for event study
  event_results <- tryCatch({
    ## Get lambda (time weights) and omega (unit weights)
    wt <- attr(est, "weights")
    omega <- wt$omega
    lambda <- wt$lambda

    Y <- panel$Y
    N0 <- panel$N0
    T0 <- panel$T0
    N1 <- nrow(Y) - N0
    T1 <- ncol(Y) - T0

    ## Treated unit(s) average outcome over time
    Y_treat <- colMeans(Y[(N0 + 1):nrow(Y), , drop = FALSE])

    ## Synthetic control outcome over time
    Y_synth <- as.numeric(t(omega) %*% Y[1:N0, ])

    ## Time effects: difference between treated and synthetic
    effects <- Y_treat - Y_synth

    ## Time variable names
    time_names <- as.numeric(colnames(Y))

    tibble(
      year = time_names,
      effect = effects,
      period = if_else(seq_along(effects) <= T0, "pre", "post")
    )
  }, error = function(e) {
    NULL
  })

  return(list(
    success = TRUE,
    tau = tau_hat,
    se = se_hat,
    estimate = est,
    panel = panel,
    weights = weights_raw,
    event = event_results
  ))
}

## Helper: create event study plot
make_event_plot <- function(event_df, label, treatment_year = 2020,
                             exl = 0, save_path = NULL) {
  if (is.null(event_df) || nrow(event_df) == 0) return(invisible(NULL))

  ## If excluding 2020, insert a gap
  if (exl == 1) {
    ## Shift pre-treatment years
    event_df <- event_df |>
      mutate(year_plot = year) |>
      bind_rows(tibble(year = 2020, year_plot = 2020, effect = NA_real_,
                        period = "pre")) |>
      arrange(year_plot)
  } else {
    event_df <- event_df |> mutate(year_plot = year)
  }

  p <- ggplot(event_df, aes(x = year_plot, y = effect)) +
    geom_point(color = "black", size = 2) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_vline(xintercept = treatment_year + 0.5, color = "black",
               linetype = "solid") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1),
                       limits = c(-10, 10)) +
    labs(x = "Year (destination)", y = label) +
    theme_minimal() +
    theme(legend.position = "none")

  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 8, height = 5, dpi = 100)
  }

  return(p)
}

## Helper: create LaTeX table for a set of specifications
make_sdid_table <- function(results_list, data_type, save_path) {
  if (length(results_list) == 0) return(invisible(NULL))

  ## Determine column group labels based on data type

  if (data_type %in% c("irs_sample_1", "irs_sample_2")) {
    group_labels <- c("Returns" = 2, "Exemptions" = 2, "AGI" = 2)
  } else {
    group_labels <- c("Households" = 2, "Adults" = 2, "Household Income" = 2)
  }

  ## Build model list for modelsummary
  models <- list()
  gof_rows <- list()

  for (i in seq_along(results_list)) {
    r <- results_list[[i]]
    if (is.null(r) || !r$success) next

    ## Create a tidy-compatible list
    model_name <- names(results_list)[i]
    if (is.null(model_name)) model_name <- paste0("(", i, ")")

    ## Manual tidy for modelsummary
    models[[model_name]] <- list(
      tidy = tibble(
        term = "ATT",
        estimate = r$tau,
        std.error = r$se,
        p.value = if (!is.na(r$se) && r$se > 0) 2 * (1 - pnorm(abs(r$tau / r$se))) else NA_real_
      ),
      glance = tibble(
        n_counties = r$n_counties,
        pre_mean = r$pre_mean
      )
    )
  }

  if (length(models) == 0) return(invisible(NULL))

  ## Write table using manual formatting
  tryCatch({
    ## Build simple LaTeX table manually
    lines <- c("\\begin{tabular}{lcccccc}", "\\hline\\hline")

    ## Header
    header_groups <- if (data_type %in% c("irs_sample_1", "irs_sample_2")) {
      c("Returns", "Exemptions", "AGI")
    } else {
      c("Households", "Adults", "Household Income")
    }

    group_line <- paste0(" & \\multicolumn{2}{c}{", header_groups, "}", collapse = "")
    lines <- c(lines, paste0(group_line, " \\\\"))
    lines <- c(lines, paste0(" & ", paste(rep(c("No Covariates", "Covariates"), 3),
                                           collapse = " & "), " \\\\"))
    lines <- c(lines, "\\hline")

    ## Coefficient rows
    for (i in seq_along(models)) {
      m <- models[[i]]
      est <- sprintf("%.3f", m$tidy$estimate)
      pv <- m$tidy$p.value
      stars <- if (!is.na(pv) && pv < 0.01) "***"
               else if (!is.na(pv) && pv < 0.05) "**"
               else if (!is.na(pv) && pv < 0.10) "*"
               else ""
      est <- paste0(est, stars)
    }

    ## Build coefficient matrix (6 columns)
    coef_vals <- sapply(models, function(m) {
      est <- sprintf("%.3f", m$tidy$estimate)
      pv <- m$tidy$p.value
      stars <- if (!is.na(pv) && pv < 0.01) "***"
               else if (!is.na(pv) && pv < 0.05) "**"
               else if (!is.na(pv) && pv < 0.10) "*"
               else ""
      paste0(est, stars)
    })

    se_vals <- sapply(models, function(m) {
      sprintf("(%.3f)", m$tidy$std.error)
    })

    lines <- c(lines, paste0("ATT & ", paste(coef_vals, collapse = " & "), " \\\\"))
    lines <- c(lines, paste0(" & ", paste(se_vals, collapse = " & "), " \\\\"))
    lines <- c(lines, "\\hline")

    ## Stats rows
    count_vals <- sapply(models, function(m) {
      formatC(m$glance$n_counties, format = "d", big.mark = ",")
    })
    mean_vals <- sapply(models, function(m) {
      sprintf("%.3f", m$glance$pre_mean)
    })

    lines <- c(lines,
               paste0("Number of Counties & ", paste(count_vals, collapse = " & "), " \\\\"),
               paste0("Pre-treatment mean & ", paste(mean_vals, collapse = " & "), " \\\\"))
    lines <- c(lines, "\\hline\\hline", "\\end{tabular}")

    writeLines(lines, save_path)
    message("    Table saved: ", basename(save_path))
  }, error = function(e) {
    warning("Failed to write table: ", conditionMessage(e))
  })
}

## ============================================================================
## MAIN ESTIMATION LOOP
## ============================================================================

spec_counter <- 0
total_specs <- 0

## Count total specifications for progress tracking
for (ds in data_specs) {
  for (type in ds$out_types) {
    for (samp_var in c("sample_all", "sample_urban95", "sample_urban95_covid")) {
      for (exl in 1:0) {
        for (migr in c("net", "in", "out")) {
          for (x in c("n1", "n2", "agi")) {
            for (c_flag in 0:1) {
              total_specs <- total_specs + 1
            }
          }
        }
      }
    }
  }
}
message("  Total specifications to run: ", total_specs)

## Loop over data sources
for (ds in data_specs) {

  data_var <- ds$data_var
  covariates_list <- ds$covariates

  ## Loop over outcome variable types
  for (type in ds$out_types) {

    out_txt <- get_out_txt(data_var, type)
    message("  Processing: ", out_txt)

    ## Create output subdirectory
    dir.create(file.path(results_sdid, out_txt),
               recursive = TRUE, showWarnings = FALSE)

    ## Loop over donor pool samples
    for (samp_var in c("sample_all", "sample_urban95", "sample_urban95_covid")) {

      ## Loop over exclusion of 2020
      for (exl in 1:0) {

        ## Define the sample mask
        sample_mask <- df[[samp_var]] == 1 & df[[data_var]] == 1
        if (exl == 1) {
          sample_mask <- sample_mask & df$year != 2020
        }

        ## Check if treated unit is in sample
        if (sum(df$multnomah[sample_mask] == 1) == 0) {
          message("    Skipping: no treated unit for ", out_txt, " / ",
                  samp_var, " / excl=", exl)
          next
        }

        ## Storage for table models (6 models per migration direction)
        table_models <- list()

        ## Loop over migration directions
        for (migr in c("net", "in", "out")) {

          ## Initialize table model list for this migration direction
          migr_models <- list()

          ## Loop over outcome variables (n1, n2, agi)
          for (x in c("n1", "n2", "agi")) {

            ## Full outcome variable name
            outcome_var <- paste0(x, "_", migr, "_rate_", type)

            ## Check if outcome variable exists
            if (!outcome_var %in% names(df)) {
              message("    Skipping: outcome ", outcome_var, " not found")
              next
            }

            ## Loop over covariate settings (0 = no covariates, 1 = covariates)
            for (c_flag in 0:1) {

              spec_counter <- spec_counter + 1
              if (spec_counter %% 20 == 0) {
                message("    Progress: ", spec_counter, " / ", total_specs)
              }

              ## File paths for figures
              if (exl == 0) {
                fig_path_base <- file.path(results_sdid, out_txt,
                                           paste0("fig_", out_txt, "_", outcome_var,
                                                  "_", c_flag, "_", samp_var, "_"))
              } else {
                fig_path_base <- file.path(results_sdid, out_txt,
                                           paste0("fig_", out_txt, "_", outcome_var,
                                                  "_", c_flag, "_", samp_var,
                                                  "_excl2020_"))
              }

              ## Run SDID
              result <- tryCatch({
                run_single_sdid(
                  df = df,
                  outcome_var = outcome_var,
                  unit_var = "fips",
                  time_var = "year",
                  treat_var = "Treated",
                  sample_mask = sample_mask,
                  covariates = if (c_flag == 1) covariates_list else NULL,
                  use_covars = (c_flag == 1),
                  reps = reps
                )
              }, error = function(e) {
                list(success = FALSE, msg = conditionMessage(e))
              })

              if (!result$success) {
                message("    FAILED: ", outcome_var, " c=", c_flag,
                        " (", result$msg, ")")
                next
              }

              ## Pre-treatment mean for treated unit
              pre_mean <- mean(
                df[[outcome_var]][df$multnomah == 1 & df$Treated == 0 & sample_mask],
                na.rm = TRUE
              )

              ## County count (number of units in post-treatment year)
              n_counties <- sum(!is.na(df[[outcome_var]][df$year == 2021 & sample_mask]))

              ## p-value
              pval <- if (!is.na(result$se) && result$se > 0) {
                2 * (1 - pnorm(abs(result$tau / result$se)))
              } else {
                NA_real_
              }

              ## Store treatment effect results
              new_result <- tibble(
                sample_data = out_txt,
                sample      = samp_var,
                outcome     = outcome_var,
                controls    = c_flag,
                exclusion   = exl,
                tau         = result$tau,
                se          = result$se,
                pval        = pval,
                ci_lower    = result$tau - 1.96 * result$se,
                ci_upper    = result$tau + 1.96 * result$se,
                n_counties  = as.integer(n_counties),
                pre_mean    = pre_mean,
                significant = as.integer(!is.na(pval) & pval < 0.05)
              )
              all_results <- bind_rows(all_results, new_result)

              ## Store weights
              if (nrow(result$weights) > 0) {
                new_weights <- result$weights |>
                  mutate(
                    sample_data = out_txt,
                    out         = outcome_var,
                    sample      = samp_var,
                    controls    = c_flag,
                    exclusion   = exl
                  )
                all_weights <- bind_rows(all_weights, new_weights)
              }

              ## Store model info for table generation
              model_key <- paste0(x, "_", c_flag)
              migr_models[[model_key]] <- list(
                success    = TRUE,
                tau        = result$tau,
                se         = result$se,
                n_counties = as.integer(n_counties),
                pre_mean   = pre_mean
              )

              ## Export SDID diagnostic plot
              tryCatch({
                sdid_plot_path <- paste0(fig_path_base, "sdid.pdf")
                pdf(sdid_plot_path, width = 8, height = 6)
                synthdid_plot(result$estimate)
                dev.off()
              }, error = function(e) {
                try(dev.off(), silent = TRUE)
              })

              ## Export event study plot
              if (!is.null(result$event)) {
                if (exl == 0) {
                  ev_path <- file.path(results_sdid, out_txt,
                                       paste0("fig_", out_txt, "_", outcome_var,
                                              "_", c_flag, "_", samp_var,
                                              "_eventstudy.jpg"))
                } else {
                  ev_path <- file.path(results_sdid, out_txt,
                                       paste0("fig_", out_txt, "_", outcome_var,
                                              "_", c_flag, "_", samp_var,
                                              "_excl2020_eventstudy.jpg"))
                }

                ## Get variable label for y-axis
                x_label <- switch(x,
                  "n1" = if (str_detect(type, "acs")) "Households" else "Returns",
                  "n2" = if (str_detect(type, "acs")) "Adults" else "Exemptions",
                  "agi" = if (str_detect(type, "acs")) "Household Income" else "AGI"
                )
                y_label <- switch(migr,
                  "net" = "Net domestic migration",
                  "in"  = "Domestic in-migration",
                  "out" = "Domestic out-migration"
                )
                ev_label <- paste0(y_label, " rate, ", x_label, " (%)")

                make_event_plot(result$event, label = ev_label,
                                treatment_year = treatment_year,
                                exl = exl, save_path = ev_path)
              }

            } # END covariate loop

          } # END outcome variable loop

          ## Generate LaTeX table for this migration direction
          if (length(migr_models) > 0) {
            if (exl == 0) {
              tab_path <- file.path(results_sdid, out_txt,
                                    paste0("tab_sdid_", out_txt, "_", migr, "_",
                                           samp_var, ".tex"))
            } else {
              tab_path <- file.path(results_sdid, out_txt,
                                    paste0("tab_sdid_", out_txt, "_", migr, "_",
                                           samp_var, "_excl2020.tex"))
            }

            ## Reorder models: n1_0, n1_1, n2_0, n2_1, agi_0, agi_1
            ordered_keys <- c("n1_0", "n1_1", "n2_0", "n2_1", "agi_0", "agi_1")
            ordered_models <- migr_models[intersect(ordered_keys, names(migr_models))]

            make_sdid_table(ordered_models, data_var, tab_path)
          }

        } # END migration direction loop

      } # END exclusion loop

    } # END sample loop

  } # END outcome type loop

} # END data source loop

# =============================================================================
# SAVE COMBINED RESULTS
# =============================================================================

message("STEP 3: Saving combined results...")

## Save results
all_results <- all_results |>
  arrange(sample_data, sample, outcome, controls, exclusion)
save_data(all_results, file.path(results_sdid, "sdid_results"))

## Export to Excel
tryCatch({
  write.xlsx(all_results, file.path(results_sdid, "sdid_results.xlsx"))
  message("  Results saved: sdid_results.xlsx")
}, error = function(e) {
  warning("Failed to write Excel: ", conditionMessage(e))
})

## Save weights
all_weights <- all_weights |>
  arrange(sample_data, sample, out, controls, exclusion, desc(weight))
save_data(all_weights, file.path(results_sdid, "sdid_weights"))

## Export weights to Excel
tryCatch({
  write.xlsx(all_weights, file.path(results_sdid, "sdid_weights.xlsx"))
  message("  Weights saved: sdid_weights.xlsx")
}, error = function(e) {
  warning("Failed to write Excel: ", conditionMessage(e))
})

# =============================================================================
# SPECIFICATION CURVE ANALYSIS
# =============================================================================

message("STEP 4: Creating specification curve plots...")

## Load results (or use in-memory if still available)
spec_results <- all_results

## Parse outcome variable names to extract components
spec_results <- spec_results |>
  mutate(
    outcome_type = case_when(
      str_detect(outcome, "^n1_") ~ "n1",
      str_detect(outcome, "^n2_") ~ "n2",
      str_detect(outcome, "^agi_") ~ "agi",
      TRUE ~ NA_character_
    ),
    migration = case_when(
      str_detect(outcome, "_net_") ~ "net",
      str_detect(outcome, "_in_") ~ "in",
      str_detect(outcome, "_out_") ~ "out",
      TRUE ~ NA_character_
    ),
    data_type = case_when(
      str_detect(sample_data, "irs_389") & str_detect(outcome, "_irs5$") ~ "IRS (389, Interstate)",
      str_detect(sample_data, "irs_389") & !str_detect(outcome, "_irs5$") ~ "IRS (389)",
      str_detect(outcome, "_irs5$") ~ "IRS (Interstate)",
      str_detect(outcome, "_irs$") ~ "IRS",
      str_detect(outcome, "_acs1$") ~ "ACS All",
      str_detect(outcome, "_acs2$") ~ "ACS College",
      TRUE ~ NA_character_
    ),
    period_type = case_when(
      str_detect(outcome, "_irs5?$") ~ "16-22",
      str_detect(sample_data, "16_22") ~ "16-22",
      str_detect(sample_data, "16_24") ~ "16-24",
      TRUE ~ NA_character_
    )
  )

## Create specification indicators for bottom panel
spec_results <- spec_results |>
  mutate(
    spec_all       = as.integer(sample == "sample_all"),
    spec_urban95   = as.integer(sample == "sample_urban95"),
    spec_covid     = as.integer(sample == "sample_urban95_covid"),
    spec_16_22     = as.integer(period_type == "16-22"),
    spec_16_24     = as.integer(period_type == "16-24"),
    spec_covars    = as.integer(controls == 1),
    spec_excl2020  = as.integer(exclusion == 1),
    spec_irs       = as.integer(data_type == "IRS"),
    spec_irs5      = as.integer(data_type == "IRS (Interstate)"),
    spec_irs_389   = as.integer(data_type == "IRS (389)"),
    spec_irs5_389  = as.integer(data_type == "IRS (389, Interstate)"),
    spec_acs_all   = as.integer(data_type == "ACS All"),
    spec_acs_col   = as.integer(data_type == "ACS College")
  )

## Ensure significance is calculated
spec_results <- spec_results |>
  mutate(significant = if_else(is.na(significant),
                                as.integer(!is.na(pval) & pval < 0.05),
                                significant))

## ============================================================================
## DEFINE PREFERRED SPECIFICATIONS
## Modify these conditions to change which specifications are highlighted
## as "preferred" in the specification curve plots.
## ============================================================================

spec_results <- spec_results |>
  mutate(preferred = 0L)

## IRS Full Sample preferred specs
spec_results <- spec_results |>
  mutate(preferred = if_else(
    data_type == "IRS" &
      spec_16_22 == 1 &
      sample %in% c("sample_all", "sample_urban95_covid") &
      controls == 1 &
      exclusion == 1,
    1L, preferred
  ))

## ACS College Sample preferred specs
spec_results <- spec_results |>
  mutate(preferred = if_else(
    data_type == "ACS College" &
      spec_16_24 == 1 &
      sample %in% c("sample_all", "sample_urban95_covid") &
      controls == 1 &
      exclusion == 1,
    1L, preferred
  ))

message("  Number of preferred specifications: ", sum(spec_results$preferred == 1))

## ============================================================================
## CREATE SPECIFICATION CURVE PLOTS
## ============================================================================

for (otype in c("n1", "n2", "agi")) {
  for (migr in c("net", "in", "out")) {
    for (pset in c("main", "irs5")) {

    ## Filter to relevant specifications
    plot_data <- spec_results |>
      filter(outcome_type == otype, migration == migr)

    ## Filter by plot set
    if (pset == "main") {
      plot_data <- plot_data |>
        filter(!data_type %in% c("IRS (Interstate)", "IRS (389, Interstate)"))
    } else if (pset == "irs5") {
      plot_data <- plot_data |>
        filter(data_type %in% c("IRS (Interstate)", "IRS (389, Interstate)"))
    }

    ## Skip if no data
    if (nrow(plot_data) == 0) next

    ## Sort by effect size and create rank
    plot_data <- plot_data |>
      arrange(tau) |>
      mutate(spec_rank = row_number())

    n_specs <- nrow(plot_data)

    ## Labels
    otype_label <- switch(otype,
      "n1"  = "Returns/Households",
      "n2"  = "Exemptions/Persons",
      "agi" = "AGI/Income"
    )
    migr_label <- switch(migr,
      "net" = "Net Migration",
      "in"  = "In-Migration",
      "out" = "Out-Migration"
    )
    pset_title <- if (pset == "irs5") " (Interstate)" else ""

    ## Create four categories: sig/insig x preferred/not
    plot_data <- plot_data |>
      mutate(
        category = case_when(
          significant == 1 & preferred == 0 ~ "Sig. (p<0.05)",
          significant == 0 & preferred == 0 ~ "Insig.",
          significant == 1 & preferred == 1 ~ "Sig., Preferred",
          significant == 0 & preferred == 1 ~ "Insig., Preferred",
          TRUE ~ "Unknown"
        ),
        category = factor(category, levels = c(
          "Sig. (p<0.05)", "Insig.", "Sig., Preferred", "Insig., Preferred"
        ))
      )

    ## Define colors
    cat_colors <- c(
      "Sig. (p<0.05)"     = "navy",
      "Insig."            = "lightblue",
      "Sig., Preferred"   = "#B22222",  # cranberry
      "Insig., Preferred" = "orange"
    )

    cat_shapes <- c(
      "Sig. (p<0.05)"     = 16,
      "Insig."            = 16,
      "Sig., Preferred"   = 18,
      "Insig., Preferred" = 18
    )

    cat_sizes <- c(
      "Sig. (p<0.05)"     = 1,
      "Insig."            = 1,
      "Sig., Preferred"   = 2,
      "Insig., Preferred" = 2
    )

    ## -----------------------------------------------------------------
    ## Upper panel: Coefficient plot with CIs colored by significance
    ## and preferred status
    ## -----------------------------------------------------------------
    p_coef <- ggplot(plot_data, aes(x = spec_rank)) +
      geom_linerange(aes(ymin = ci_lower, ymax = ci_upper, color = category),
                      linewidth = 0.3) +
      geom_point(aes(y = tau, color = category, shape = category, size = category)) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      scale_color_manual(values = cat_colors, drop = FALSE) +
      scale_shape_manual(values = cat_shapes, drop = FALSE) +
      scale_size_manual(values = cat_sizes, drop = FALSE) +
      labs(
        title = paste0(otype_label, ": ", migr_label, pset_title),
        y = "Treatment Effect (pp)",
        x = ""
      ) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        plot.title = element_text(size = 11, face = "bold")
      ) +
      guides(color = guide_legend(nrow = 1),
             shape = guide_legend(nrow = 1),
             size = guide_legend(nrow = 1))

    ## -----------------------------------------------------------------
    ## Lower panel: Specification indicator dots
    ## (different indicators depending on main vs irs5 plot set)
    ## -----------------------------------------------------------------

    if (pset == "main") {

      ## Main plot: exclude IRS Interstate indicators
      spec_indicators <- plot_data |>
        select(spec_rank, spec_all, spec_urban95, spec_covid, spec_covars,
               spec_excl2020, spec_irs, spec_irs_389,
               spec_acs_all, spec_acs_col, spec_16_22, spec_16_24) |>
        pivot_longer(
          cols = -spec_rank,
          names_to = "indicator",
          values_to = "active"
        ) |>
        filter(active == 1)

      indicator_levels <- c(
        "spec_all"      = -1,
        "spec_urban95"  = -2,
        "spec_covid"    = -3,
        "spec_covars"   = -4,
        "spec_excl2020" = -5,
        "spec_irs"      = -6,
        "spec_irs_389"  = -7,
        "spec_acs_all"  = -8,
        "spec_acs_col"  = -9,
        "spec_16_22"    = -10,
        "spec_16_24"    = -11
      )

      indicator_labels <- c(
        "spec_all"      = "All Counties",
        "spec_urban95"  = "Urban 95%",
        "spec_covid"    = "COVID Match",
        "spec_covars"   = "Covariates",
        "spec_excl2020" = "Excl. 2020",
        "spec_irs"      = "IRS (all counties)",
        "spec_irs_389"  = "IRS (ACS counties)",
        "spec_acs_all"  = "ACS All",
        "spec_acs_col"  = "ACS College",
        "spec_16_22"    = "16-22",
        "spec_16_24"    = "16-24"
      )

      y_limits <- c(-11.5, -0.5)

    } else if (pset == "irs5") {

      ## IRS5 plot: only IRS Interstate indicators
      spec_indicators <- plot_data |>
        select(spec_rank, spec_all, spec_urban95, spec_covid, spec_covars,
               spec_excl2020, spec_irs5, spec_irs5_389) |>
        pivot_longer(
          cols = -spec_rank,
          names_to = "indicator",
          values_to = "active"
        ) |>
        filter(active == 1)

      indicator_levels <- c(
        "spec_all"      = -1,
        "spec_urban95"  = -2,
        "spec_covid"    = -3,
        "spec_covars"   = -4,
        "spec_excl2020" = -5,
        "spec_irs5"     = -6,
        "spec_irs5_389" = -7
      )

      indicator_labels <- c(
        "spec_all"      = "All Counties",
        "spec_urban95"  = "Urban 95%",
        "spec_covid"    = "COVID Match",
        "spec_covars"   = "Covariates",
        "spec_excl2020" = "Excl. 2020",
        "spec_irs5"     = "IRS Interstate (all counties)",
        "spec_irs5_389" = "IRS Interstate (ACS counties)"
      )

      y_limits <- c(-7.5, -0.5)

    }

    spec_indicators <- spec_indicators |>
      mutate(y_pos = indicator_levels[indicator])

    p_spec <- ggplot(spec_indicators, aes(x = spec_rank, y = y_pos)) +
      geom_point(color = "navy", shape = 16, size = 0.5) +
      scale_y_continuous(
        breaks = unname(indicator_levels),
        labels = unname(indicator_labels),
        limits = y_limits
      ) +
      labs(x = "Specification (ranked by effect size)", y = "") +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 7),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )

    ## -----------------------------------------------------------------
    ## Combine panels using patchwork
    ## -----------------------------------------------------------------
    p_combined <- p_coef / p_spec +
      plot_layout(heights = c(2, 1.5))

    ## File suffix for IRS5 plots
    fsuffix <- if (pset == "irs5") "_irs5" else ""

    ## Export combined figure
    ggsave(file.path(results_sdid,
                     paste0("fig_speccurve_", otype, "_", migr, fsuffix, ".pdf")),
           p_combined, width = 10, height = 8)
    ggsave(file.path(results_sdid,
                     paste0("fig_speccurve_", otype, "_", migr, fsuffix, ".jpg")),
           p_combined, width = 10, height = 8, dpi = 100)

    message("    Spec curve plot: ", otype, " / ", migr,
            if (pset == "irs5") " (irs5)" else "",
            " (", n_specs, " specifications)")

    } # END plot set loop
  } # END migration loop
} # END outcome type loop

# =============================================================================
# FINISH
# =============================================================================

message("")
message("==============================================")
message("SDID ANALYSIS COMPLETE")
message("==============================================")
message("Results saved to:")
message("  - ", file.path(results_sdid, "sdid_results.rds"))
message("  - ", file.path(results_sdid, "sdid_weights.rds"))
message("  - ", file.path(results_sdid, "*/tab_sdid_*.tex (tables)"))
message("  - ", file.path(results_sdid, "fig_speccurve_*.pdf"))
message("==============================================")
