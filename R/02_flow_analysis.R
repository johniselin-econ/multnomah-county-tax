# =============================================================================
# 02_flow_analysis.R
# Flow-based analysis on IRS migration data
#
# Converted from: code/02_flow_analysis.do
# Author: John Iselin
# Date: January 31, 2026
#
# Called by: 00_multnomah.R (or run standalone)
#
# Purpose: Perform flow-based analysis on IRS data.
#
# Outputs:
# All outputs are produced twice: once for all counties and once for ACS
# counties only (with _acs suffix). ACS sample includes property tax rate
# controls.
#
# - county_flow_coefficients[_acs].rds/csv: flow coefficients for all counties
#   (b_out, b_in, se_out, se_in, t-stats, p-values, percentile ranks)
# - fig_multnomah_post_*[_acs].png: flow coefficient plots for Multnomah
# - fig_hist_out_*[_acs].png: Histogram of out-migration coefficients
# - fig_hist_in_*[_acs].png: Histogram of in-migration coefficients
# - fig_scatter_out_in_*[_acs].png: Scatter plot of out vs in coefficients
# - fig_multnomah_out_*[_acs].png: Event study plots for out-migration
# - fig_multnomah_in_*[_acs].png: Event study plots for in-migration
# - fig_multnomah_both_*[_acs].png: Combined event study plots
#
# Excel output (flow_analysis_stats.xlsx):
# - all/acs: Summary statistics for Multnomah and distribution
# - larger_out_all/acs: Counties with larger out-migration coefficients
# - smaller_in_all/acs: Counties with smaller in-migration coefficients
# - worse_both_all/acs: Counties worse on both dimensions
# - larger_t_out_all/acs: Counties with larger out-migration t-statistics
# - smaller_t_in_all/acs: Counties with smaller in-migration t-statistics
#
# Debug mode:
# - Set debug <- TRUE to run on a random subset of counties for faster testing
# - Set debug_n to control the number of random counties (default: 20)
# - Multnomah County is always included in the debug sample
# - Set debug <- FALSE for full production runs
#
# For more information, contact john.iselin@yale.edu
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(fixest)
  library(openxlsx)
  library(tibble)
})

# Source configuration
source(file.path(r_dir, "utils", "globals.R"))
source(file.path(r_dir, "utils", "helpers.R"))

message("=== 02_flow_analysis.R: Starting flow analysis ===")
message("  Run date: ", run_date)

# ==============================================================================
# Parameters
# ==============================================================================

reps <- 100

# Debug mode: set to TRUE to run on a random subset of counties for faster
# testing. Set to FALSE for full production run.
if (!exists("debug")) debug <- FALSE
debug_n <- 20 # Number of random counties to sample (plus Multnomah)

# ==============================================================================
# DATA PREPARATION
# ==============================================================================

message("Loading data...")

# --- Load ACS data to get the set of always-observed counties ----------------
acs_gross <- load_data(file.path(data_working, "acs_county_gross_25plus"))

acs_fips <- acs_gross |>
  select(year, fips) |>
  group_by(fips) |>
  mutate(ct = n()) |>
  ungroup() |>
  filter(ct == 10) |>
  select(fips) |>
  distinct()

# --- Load IRS flow data -----------------------------------------------------
irs_flow <- load_data(file.path(data_working, "irs_county_flow"))

# Sample restrictions: drop Alaska and Hawaii
irs_flow <- irs_flow |>
  filter(!state_fips_o %in% c(2, 15),
         !state_fips_d %in% c(2, 15))

# Drop fips if it exists as a column (use fips_o and fips_d instead)
if ("fips" %in% names(irs_flow)) {
  irs_flow <- irs_flow |> select(-fips)
}

# Mover indicator
irs_flow <- irs_flow |>
  mutate(mover = as.integer(fips_o != fips_d))

# Generate flow_id (unique origin-destination pair identifier)
irs_flow <- irs_flow |>
  mutate(flow_id = as.integer(as.factor(paste(fips_d, fips_o, sep = "_"))))

# Balance the panel: fill in all flow_id x year combinations
all_years <- sort(unique(irs_flow$year))
all_flows <- irs_flow |>
  select(flow_id, fips_d, fips_o) |>
  distinct()

irs_flow <- irs_flow |>
  select(flow_id, fips_d, fips_o, year, mover, n1, n2, agi) |>
  complete(
    nesting(flow_id, fips_d, fips_o),
    year = all_years
  )

# For rows created by complete(), fill fips from their flow_id group
# (fips_d and fips_o are already part of the nesting, so they are filled)

# Replace missing outcomes with 0
irs_flow <- irs_flow |>
  mutate(
    n1  = replace_na(n1, 0),
    n2  = replace_na(n2, 0),
    agi = replace_na(agi, 0),
    agi = ifelse(agi < 0, 0, agi)
  )

# --- Merge with time-varying controls for origin and destination -------------
message("Merging with control variables...")

ids    <- load_data(file.path(data_working, "ids"))
bls    <- load_data(file.path(data_working, "bls_unemployment"))
bea    <- load_data(file.path(data_working, "bea_economics"))
proptx <- load_data(file.path(data_working, "property_tax_rates_overall"))

for (x in c("o", "d")) {

  sfx <- paste0("_", x)
  fips_col <- paste0("fips_", x)

  # Merge IDs
  irs_flow <- irs_flow |>
    inner_join(
      ids |>
        select(fips, state_name, state_fips, county_name, county_fips) |>
        rename_with(~ paste0(., sfx), .cols = c(state_name, state_fips,
                                                 county_name, county_fips)),
      by = setNames("fips", fips_col)
    )

  # Merge BLS unemployment
  irs_flow <- irs_flow |>
    inner_join(
      bls |>
        select(year, fips, unemp) |>
        rename(!!paste0("unemp", sfx) := unemp),
      by = setNames(c("year", "fips"), c("year", fips_col))
    )

  # Merge BEA economics
  irs_flow <- irs_flow |>
    inner_join(
      bea |>
        select(year, fips, population, per_capita_income) |>
        rename(
          !!paste0("pop", sfx) := population,
          !!paste0("per_capita_income", sfx) := per_capita_income
        ),
      by = setNames(c("year", "fips"), c("year", fips_col))
    )

  # Merge ACS fips (flag which counties are in the ACS sample)
  irs_flow <- irs_flow |>
    left_join(
      acs_fips |> mutate(!!paste0("in_acs", sfx) := 1L),
      by = setNames("fips", fips_col)
    ) |>
    mutate(!!paste0("in_acs", sfx) := replace_na(!!sym(paste0("in_acs", sfx)), 0L))

  # Merge property tax rates
  irs_flow <- irs_flow |>
    left_join(
      proptx |>
        select(year, fips, prop_rate_mean) |>
        rename(!!paste0("prop_rate_mean", sfx) := prop_rate_mean),
      by = setNames(c("year", "fips"), c("year", fips_col))
    )
}

# Tag flows to-and-from ACS counties
irs_flow <- irs_flow |>
  mutate(acs_flow = as.integer(in_acs_d == 1L & in_acs_o == 1L))

# Multnomah indicators
irs_flow <- irs_flow |>
  mutate(
    out_multnomah = as.integer(state_fips_o == multnomah_state_fips &
                                 county_fips_o == multnomah_county_fips),
    in_multnomah  = as.integer(state_fips_d == multnomah_state_fips &
                                 county_fips_d == multnomah_county_fips)
  )

# Post indicator (treatment year is 2020; post = year > 2020)
irs_flow <- irs_flow |>
  mutate(post = as.integer(year > treatment_year))

# Interactions: Multnomah x Post
irs_flow <- irs_flow |>
  mutate(
    in_multnomah_post  = in_multnomah * post,
    out_multnomah_post = out_multnomah * post
  )

# Year-by-year interactions for event study (base year = 2020)
event_years <- setdiff(2016:2022, 2020)
for (y in event_years) {
  irs_flow[[paste0("x_out_", y)]] <- irs_flow$out_multnomah * as.integer(irs_flow$year == y)
  irs_flow[[paste0("x_in_", y)]]  <- irs_flow$in_multnomah  * as.integer(irs_flow$year == y)
}

# Store the main analysis dataset
main_data <- irs_flow

message("Data preparation complete. Observations: ", nrow(main_data))

# ==============================================================================
# MAIN ANALYSIS LOOP: Run for (1) "acs" counties, (2) "all" counties
# ==============================================================================

# Initialize Excel workbook for results
debug_txt <- if (debug) "_debug" else ""
wb <- createWorkbook()

for (sample_type in c("acs", "all")) {

  # --- Set sample-specific parameters ----------------------------------------
  if (sample_type == "all") {
    covar_names  <- c("unemp_o", "unemp_d", "pop_o", "pop_d",
                      "per_capita_income_o", "per_capita_income_d")
    file_suffix  <- ""
    title_suffix <- ""
  } else {
    covar_names  <- c("unemp_o", "unemp_d", "pop_o", "pop_d",
                      "per_capita_income_o", "per_capita_income_d",
                      "prop_rate_mean_o", "prop_rate_mean_d")
    file_suffix  <- "_acs"
    title_suffix <- " (ACS Counties)"
  }

  message("")
  message("========================================================================")
  message("RUNNING ANALYSIS FOR SAMPLE: ", sample_type)
  message("========================================================================")
  message("")

  # --- Filter data for this sample -------------------------------------------
  if (sample_type == "acs") {
    df <- main_data |> filter(acs_flow == 1)
  } else {
    df <- main_data
  }

  # ============================================================================
  # REGRESSION 1: Multnomah x Post (flow DiD)
  # ============================================================================
  message("  REGRESSION 1: Multnomah x Post...")

  outcomes      <- c("n1", "n2", "agi")
  outcome_labels <- c(n1 = "Number of Returns",
                      n2 = "Number of Exemptions",
                      agi = "Adjusted Gross Income")

  for (outcome in outcomes) {

    outcome_label <- outcome_labels[outcome]
    message("    Outcome: ", outcome_label)

    # Build covariate formula string
    covar_fml <- paste(covar_names, collapse = " + ")

    # Regression 1a: With covariates
    fml_wc <- as.formula(paste0(
      outcome,
      " ~ i(out_multnomah_post) + i(in_multnomah_post) + ",
      covar_fml,
      " | year + flow_id"
    ))

    fit_wc <- tryCatch(
      fepois(fml_wc,
             data = df |> filter(mover == 1),
             cluster = ~flow_id),
      error = function(e) {
        warning("Regression 1a (with covariates) failed for ", outcome,
                " (", sample_type, "): ", conditionMessage(e))
        NULL
      }
    )

    # Regression 1b: Without covariates
    fml_nc <- as.formula(paste0(
      outcome,
      " ~ i(out_multnomah_post) + i(in_multnomah_post) | year + flow_id"
    ))

    fit_nc <- tryCatch(
      fepois(fml_nc,
             data = df |> filter(mover == 1),
             cluster = ~flow_id),
      error = function(e) {
        warning("Regression 1b (no covariates) failed for ", outcome,
                " (", sample_type, "): ", conditionMessage(e))
        NULL
      }
    )

    # --- Coefficient plot: Multnomah x Post ---
    if (!is.null(fit_wc) && !is.null(fit_nc)) {

      # Extract coefficients for the two treatment indicators
      coef_names <- c("out_multnomah_post::1", "in_multnomah_post::1")
      coef_labels <- c("Out-migration", "In-migration")

      # With covariates
      ct_wc <- coeftable(fit_wc)
      # Without covariates
      ct_nc <- coeftable(fit_nc)

      plot_data <- bind_rows(
        tibble(
          term   = coef_labels,
          est    = ct_wc[coef_names, "Estimate"],
          ci_lo  = ct_wc[coef_names, "Estimate"] - 1.96 * ct_wc[coef_names, "Std. Error"],
          ci_hi  = ct_wc[coef_names, "Estimate"] + 1.96 * ct_wc[coef_names, "Std. Error"],
          model  = "With Covariates"
        ),
        tibble(
          term   = coef_labels,
          est    = ct_nc[coef_names, "Estimate"],
          ci_lo  = ct_nc[coef_names, "Estimate"] - 1.96 * ct_nc[coef_names, "Std. Error"],
          ci_hi  = ct_nc[coef_names, "Estimate"] + 1.96 * ct_nc[coef_names, "Std. Error"],
          model  = "Without Covariates"
        )
      ) |>
        mutate(term = factor(term, levels = coef_labels))

      p <- ggplot(plot_data, aes(x = term, y = est, color = model, shape = model)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
        geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi),
                        position = position_dodge(width = 0.4), size = 0.7) +
        scale_color_manual(values = c("With Covariates" = "navy",
                                      "Without Covariates" = "maroon")) +
        scale_shape_manual(values = c("With Covariates" = 16,
                                      "Without Covariates" = 17)) +
        labs(
          title    = paste0("Migration Flows: Multnomah County x Post", title_suffix),
          subtitle = outcome_label,
          y        = "Coefficient",
          x        = "",
          color    = NULL,
          shape    = NULL
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "bottom",
          panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA)
        )

      ggsave(file.path(results_flows,
                        paste0("fig_multnomah_post_", outcome, file_suffix, ".png")),
             p, width = 8, height = 6, dpi = 300)
    }

  } # END OUTCOME LOOP (Regression 1)

  # ============================================================================
  # REGRESSION 1b: Permutation-style analysis - loop over ALL counties
  # ============================================================================
  message("  REGRESSION 1b: Permutation-style county loop...")

  # Get list of unique origin fips codes
  if (sample_type == "all") {
    fips_list <- sort(unique(df$fips_o))
  } else {
    fips_list <- sort(unique(df$fips_o[df$acs_flow == 1]))
  }
  n_fips_full <- length(fips_list)

  # Debug mode: randomly sample a subset of counties (always include Multnomah)
  if (debug) {
    message("  *** DEBUG MODE: Sampling ", debug_n,
            " random counties (plus Multnomah) ***")
    set.seed(12345)
    non_multnomah <- setdiff(fips_list, multnomah_fips)
    sampled <- sample(non_multnomah, min(debug_n, length(non_multnomah)))
    fips_list <- sort(unique(c(multnomah_fips, sampled)))
  }

  n_fips <- length(fips_list)
  if (debug) {
    message("  DEBUG: Running regressions for ", n_fips,
            " counties (sampled from ", n_fips_full, ") (", sample_type, " sample)...")
  } else {
    message("  Running regressions for ", n_fips,
            " counties (", sample_type, " sample)...")
  }

  # Prepare filtered data for the county loop (movers only, sample condition)
  df_movers <- df |> filter(mover == 1)

  # Accumulate results in a list (much faster than growing a data frame)
  results_list <- vector("list", n_fips)

  for (i in seq_along(fips_list)) {
    f <- fips_list[i]

    if (i %% 100 == 0) {
      message("    Processing county ", i, " of ", n_fips, " (fips = ", f, ")...")
    }

    result <- tryCatch({
      # Create treatment variables for this county
      df_tmp <- df_movers |>
        mutate(
          out_post_tmp = as.integer(fips_o == f) * post,
          in_post_tmp  = as.integer(fips_d == f) * post
        )

      # Run PPML regression with covariates
      fml_county <- as.formula(paste0(
        "agi ~ i(out_post_tmp) + i(in_post_tmp) + ",
        paste(covar_names, collapse = " + "),
        " | year + flow_id"
      ))

      fit_county <- fepois(fml_county, data = df_tmp, cluster = ~flow_id)

      ct <- coeftable(fit_county)

      tibble(
        fips   = f,
        b_out  = ct["out_post_tmp::1", "Estimate"],
        se_out = ct["out_post_tmp::1", "Std. Error"],
        b_in   = ct["in_post_tmp::1", "Estimate"],
        se_in  = ct["in_post_tmp::1", "Std. Error"]
      )
    },
    error = function(e) {
      message("    Warning: Regression failed for fips ", f, ": ", conditionMessage(e))
      tibble(fips = f, b_out = NA_real_, se_out = NA_real_,
             b_in = NA_real_, se_in = NA_real_)
    })

    results_list[[i]] <- result
  }

  # Combine results
  county_coefs <- bind_rows(results_list)

  # Add identifiers and derived statistics
  county_coefs <- county_coefs |>
    mutate(
      multnomah = as.integer(fips == multnomah_fips),
      t_out     = b_out / se_out,
      t_in      = b_in / se_in,
      p_out     = 2 * (1 - pnorm(abs(t_out))),
      p_in      = 2 * (1 - pnorm(abs(t_in)))
    )

  # Calculate percentile ranks
  for (x in c("b", "t")) {
    out_col <- paste0(x, "_out")
    in_col  <- paste0(x, "_in")

    county_coefs <- county_coefs |>
      mutate(
        !!paste0("rank_", x, "_out")   := rank(!!sym(out_col), na.last = "keep"),
        !!paste0("rank_", x, "_in")    := rank(!!sym(in_col), na.last = "keep"),
        !!paste0("pctile_", x, "_out") := 100 * rank(!!sym(out_col), na.last = "keep") /
          sum(!is.na(!!sym(out_col))),
        !!paste0("pctile_", x, "_in")  := 100 * rank(!!sym(in_col), na.last = "keep") /
          sum(!is.na(!!sym(in_col)))
      )
  }

  # Display Multnomah's position
  message("")
  message("  ==========================================")
  message("  Multnomah County Results (", sample_type, "):")
  message("  ==========================================")
  multnomah_row <- county_coefs |> filter(multnomah == 1)
  if (nrow(multnomah_row) > 0) {
    message("    b_out: ", round(multnomah_row$b_out, 6),
            "  t_out: ", round(multnomah_row$t_out, 3),
            "  pctile_b_out: ", round(multnomah_row$pctile_b_out, 1))
    message("    b_in:  ", round(multnomah_row$b_in, 6),
            "  t_in:  ", round(multnomah_row$t_in, 3),
            "  pctile_b_in:  ", round(multnomah_row$pctile_b_in, 1))
  }

  # Display distribution summary
  message("")
  message("  Distribution of coefficients (all counties):")
  message("    b_out - mean: ", round(mean(county_coefs$b_out, na.rm = TRUE), 6),
          "  sd: ", round(sd(county_coefs$b_out, na.rm = TRUE), 6))
  message("    b_in  - mean: ", round(mean(county_coefs$b_in, na.rm = TRUE), 6),
          "  sd: ", round(sd(county_coefs$b_in, na.rm = TRUE), 6))

  # Save county coefficients
  save_data(county_coefs,
            file.path(data_working,
                      paste0("county_flow_coefficients", file_suffix, debug_txt)))

  message("  County coefficients saved.")

  # ============================================================================
  # PLOTS: Multnomah vs Distribution (Histograms and Scatter)
  # ============================================================================
  message("  Creating distribution plots...")

  for (x in c("b", "t")) {

    txt <- if (x == "b") "Coefficients" else "T-Statistics"

    out_var    <- paste0(x, "_out")
    in_var     <- paste0(x, "_in")
    pctile_out <- paste0("pctile_", x, "_out")
    pctile_in  <- paste0("pctile_", x, "_in")

    # Multnomah's values
    m_val_out    <- multnomah_row[[out_var]]
    m_val_in     <- multnomah_row[[in_var]]
    m_pctile_out <- multnomah_row[[pctile_out]]
    m_pctile_in  <- multnomah_row[[pctile_in]]

    # --- Plot 1: Out-migration histogram ---
    p_hist_out <- ggplot(
      county_coefs |> filter(!is.na(!!sym(out_var))),
      aes(x = !!sym(out_var))
    ) +
      geom_histogram(bins = 50, fill = alpha("navy", 0.5), color = "navy") +
      geom_vline(xintercept = m_val_out, color = "red", linewidth = 1.2) +
      labs(
        title    = paste0("Distribution of Out-Migration ", txt, title_suffix),
        subtitle = paste0("Red line = Multnomah County (percentile: ",
                          sprintf("%.1f", m_pctile_out), ")"),
        x        = paste0("Out-migration ", txt, " (County x Post)"),
        y        = "Frequency"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
      )

    ggsave(file.path(results_flows,
                      paste0("fig_hist_out_", x, file_suffix, debug_txt, ".png")),
           p_hist_out, width = 8, height = 6, dpi = 300)

    # --- Plot 2: In-migration histogram ---
    p_hist_in <- ggplot(
      county_coefs |> filter(!is.na(!!sym(in_var))),
      aes(x = !!sym(in_var))
    ) +
      geom_histogram(bins = 50, fill = alpha("maroon", 0.5), color = "maroon") +
      geom_vline(xintercept = m_val_in, color = "red", linewidth = 1.2) +
      labs(
        title    = paste0("Distribution of In-Migration ", txt, title_suffix),
        subtitle = paste0("Red line = Multnomah County (percentile: ",
                          sprintf("%.1f", m_pctile_in), ")"),
        x        = paste0("In-migration ", txt, " (County x Post)"),
        y        = "Frequency"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
      )

    ggsave(file.path(results_flows,
                      paste0("fig_hist_in_", x, file_suffix, debug_txt, ".png")),
           p_hist_in, width = 8, height = 6, dpi = 300)

    # --- Plot 3: Scatter plot of out vs in ---
    p_scatter <- ggplot() +
      geom_point(
        data = county_coefs |> filter(multnomah == 0,
                                       !is.na(!!sym(out_var)),
                                       !is.na(!!sym(in_var))),
        aes(x = !!sym(out_var), y = !!sym(in_var)),
        color = alpha("navy", 0.3), shape = 16, size = 1.5
      ) +
      geom_point(
        data = county_coefs |> filter(multnomah == 1),
        aes(x = !!sym(out_var), y = !!sym(in_var)),
        color = "red", shape = 18, size = 4
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
      labs(
        title = paste0("Out- vs In-Migration Effects by County", title_suffix),
        x     = paste0("Out-migration ", txt),
        y     = paste0("In-migration ", txt)
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
      )

    # Add a manual legend via annotate
    p_scatter <- p_scatter +
      annotate("point", x = Inf, y = Inf, color = alpha("navy", 0.3),
               shape = 16, size = 2) +
      annotate("point", x = Inf, y = Inf, color = "red",
               shape = 18, size = 4)

    # Use a cleaner approach for the legend
    scatter_data <- bind_rows(
      county_coefs |>
        filter(multnomah == 0, !is.na(!!sym(out_var)), !is.na(!!sym(in_var))) |>
        select(!!sym(out_var), !!sym(in_var)) |>
        mutate(county_type = "Other Counties"),
      county_coefs |>
        filter(multnomah == 1) |>
        select(!!sym(out_var), !!sym(in_var)) |>
        mutate(county_type = "Multnomah County")
    ) |>
      mutate(county_type = factor(county_type,
                                   levels = c("Other Counties", "Multnomah County")))

    p_scatter <- ggplot(scatter_data,
                        aes(x = !!sym(out_var), y = !!sym(in_var),
                            color = county_type, shape = county_type,
                            size = county_type, alpha = county_type)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
      geom_point() +
      scale_color_manual(values = c("Other Counties" = "navy",
                                    "Multnomah County" = "red")) +
      scale_shape_manual(values = c("Other Counties" = 16,
                                    "Multnomah County" = 18)) +
      scale_size_manual(values = c("Other Counties" = 1.5,
                                   "Multnomah County" = 4)) +
      scale_alpha_manual(values = c("Other Counties" = 0.3,
                                    "Multnomah County" = 1)) +
      labs(
        title = paste0("Out- vs In-Migration Effects by County", title_suffix),
        x     = paste0("Out-migration ", txt),
        y     = paste0("In-migration ", txt),
        color = NULL, shape = NULL, size = NULL, alpha = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position  = "bottom",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
      ) +
      guides(
        color = guide_legend(override.aes = list(size = c(2, 4), alpha = c(0.5, 1)))
      )

    ggsave(file.path(results_flows,
                      paste0("fig_scatter_out_in_", x, file_suffix, debug_txt, ".png")),
           p_scatter, width = 8, height = 6, dpi = 300)

  } # END BETA / T-STAT LOOP

  # ============================================================================
  # EXPORT: Descriptive Statistics to Excel
  # ============================================================================
  message("  Exporting descriptive statistics to Excel...")

  # --- Multnomah's statistics ---
  m <- multnomah_row

  m_b_out       <- m$b_out
  m_se_out      <- m$se_out
  m_t_out       <- m$t_out
  m_p_out       <- m$p_out
  m_pctile_b_out <- m$pctile_b_out
  m_pctile_t_out <- m$pctile_t_out

  m_b_in        <- m$b_in
  m_se_in       <- m$se_in
  m_t_in        <- m$t_in
  m_p_in        <- m$p_in
  m_pctile_b_in  <- m$pctile_b_in
  m_pctile_t_in  <- m$pctile_t_in

  # Count statistics
  n_counties_out  <- sum(!is.na(county_coefs$b_out))
  n_counties_in   <- sum(!is.na(county_coefs$b_in))
  n_counties_both <- sum(!is.na(county_coefs$b_out) & !is.na(county_coefs$b_in))

  n_larger_out     <- sum(county_coefs$b_out > m_b_out, na.rm = TRUE)
  n_larger_out_sig <- sum(county_coefs$b_out > m_b_out & county_coefs$p_out < 0.05,
                          na.rm = TRUE)

  n_smaller_in     <- sum(county_coefs$b_in < m_b_in, na.rm = TRUE)
  n_smaller_in_sig <- sum(county_coefs$b_in < m_b_in & county_coefs$p_in < 0.05,
                          na.rm = TRUE)

  n_worse_both     <- sum(county_coefs$b_out > m_b_out & county_coefs$b_in < m_b_in &
                            !is.na(county_coefs$b_out) & !is.na(county_coefs$b_in))
  n_worse_both_sig <- sum(county_coefs$b_out > m_b_out & county_coefs$b_in < m_b_in &
                            !is.na(county_coefs$b_out) & !is.na(county_coefs$b_in) &
                            county_coefs$p_out < 0.05 & county_coefs$p_in < 0.05)

  # Distribution statistics
  dist_out <- summary(county_coefs$b_out[!is.na(county_coefs$b_out)])
  dist_in  <- summary(county_coefs$b_in[!is.na(county_coefs$b_in)])

  dist_out_q <- quantile(county_coefs$b_out, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  dist_in_q  <- quantile(county_coefs$b_in, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

  dist_t_out_vals <- county_coefs$t_out[!is.na(county_coefs$t_out)]
  dist_t_in_vals  <- county_coefs$t_in[!is.na(county_coefs$t_in)]

  # Build summary statistics table
  stats_df <- tribble(
    ~stat_name,           ~stat_value,                                    ~stat_description,
    # Multnomah statistics
    "m_b_out",            m_b_out,                                        "Multnomah out-migration coefficient",
    "m_se_out",           m_se_out,                                       "Multnomah out-migration standard error",
    "m_t_out",            m_t_out,                                        "Multnomah out-migration t-statistic",
    "m_p_out",            m_p_out,                                        "Multnomah out-migration p-value",
    "m_pctile_b_out",     m_pctile_b_out,                                 "Multnomah out-migration percentile rank",
    "m_b_in",             m_b_in,                                         "Multnomah in-migration coefficient",
    "m_se_in",            m_se_in,                                        "Multnomah in-migration standard error",
    "m_t_in",             m_t_in,                                         "Multnomah in-migration t-statistic",
    "m_p_in",             m_p_in,                                         "Multnomah in-migration p-value",
    "m_pctile_b_in",      m_pctile_b_in,                                  "Multnomah in-migration percentile rank",
    # Count statistics
    "n_counties_out",     n_counties_out,                                 "Number of counties with out-migration coef",
    "n_counties_in",      n_counties_in,                                  "Number of counties with in-migration coef",
    "n_counties_both",    n_counties_both,                                "Number of counties with both coefs",
    "n_larger_out",       n_larger_out,                                   "Counties with larger out-migration coef than Multnomah",
    "n_larger_out_sig",   n_larger_out_sig,                               "Counties with larger out-migration coef (p<0.05)",
    "n_smaller_in",       n_smaller_in,                                   "Counties with smaller in-migration coef than Multnomah",
    "n_smaller_in_sig",   n_smaller_in_sig,                               "Counties with smaller in-migration coef (p<0.05)",
    "n_worse_both",       n_worse_both,                                   "Counties worse on both out AND in migration",
    "n_worse_both_sig",   n_worse_both_sig,                               "Counties worse on both (p<0.05 for both)",
    # Distribution - out-migration
    "dist_out_mean",      mean(county_coefs$b_out, na.rm = TRUE),         "Distribution mean: out-migration coef",
    "dist_out_sd",        sd(county_coefs$b_out, na.rm = TRUE),           "Distribution SD: out-migration coef",
    "dist_out_min",       min(county_coefs$b_out, na.rm = TRUE),          "Distribution min: out-migration coef",
    "dist_out_max",       max(county_coefs$b_out, na.rm = TRUE),          "Distribution max: out-migration coef",
    "dist_out_p25",       as.numeric(dist_out_q[1]),                      "Distribution 25th pctile: out-migration coef",
    "dist_out_p50",       as.numeric(dist_out_q[2]),                      "Distribution median: out-migration coef",
    "dist_out_p75",       as.numeric(dist_out_q[3]),                      "Distribution 75th pctile: out-migration coef",
    # Distribution - in-migration
    "dist_in_mean",       mean(county_coefs$b_in, na.rm = TRUE),          "Distribution mean: in-migration coef",
    "dist_in_sd",         sd(county_coefs$b_in, na.rm = TRUE),            "Distribution SD: in-migration coef",
    "dist_in_min",        min(county_coefs$b_in, na.rm = TRUE),           "Distribution min: in-migration coef",
    "dist_in_max",        max(county_coefs$b_in, na.rm = TRUE),           "Distribution max: in-migration coef",
    "dist_in_p25",        as.numeric(dist_in_q[1]),                       "Distribution 25th pctile: in-migration coef",
    "dist_in_p50",        as.numeric(dist_in_q[2]),                       "Distribution median: in-migration coef",
    "dist_in_p75",        as.numeric(dist_in_q[3]),                       "Distribution 75th pctile: in-migration coef",
    # Distribution - t-statistics
    "dist_t_out_mean",    mean(dist_t_out_vals),                          "Distribution mean: out-migration t-stat",
    "dist_t_out_sd",      sd(dist_t_out_vals),                            "Distribution SD: out-migration t-stat",
    "dist_t_out_p50",     median(dist_t_out_vals),                        "Distribution median: out-migration t-stat",
    "dist_t_in_mean",     mean(dist_t_in_vals),                           "Distribution mean: in-migration t-stat",
    "dist_t_in_sd",       sd(dist_t_in_vals),                             "Distribution SD: in-migration t-stat",
    "dist_t_in_p50",      median(dist_t_in_vals),                         "Distribution median: in-migration t-stat"
  ) |>
    mutate(sample = sample_type)

  # Write summary sheet
  addWorksheet(wb, sample_type)
  writeData(wb, sample_type, stats_df)

  # --- Merge county names for export sheets ---
  county_coefs_merged <- county_coefs |>
    left_join(
      ids |> select(fips, state_name, county_name, state_fips, county_fips),
      by = "fips"
    ) |>
    select(fips, state_name, county_name, state_fips, county_fips, multnomah,
           b_out, se_out, t_out, p_out, pctile_b_out, pctile_t_out,
           b_in, se_in, t_in, p_in, pctile_b_in, pctile_t_in,
           everything())

  # Sheet 1: Counties with larger out-migration coefficients than Multnomah
  sheet_larger_out <- county_coefs_merged |>
    filter(b_out > m_b_out, !is.na(b_out)) |>
    arrange(desc(b_out)) |>
    select(fips, state_name, county_name, b_out, se_out, t_out, p_out,
           pctile_b_out, pctile_t_out)

  sheet_name <- paste0("larger_out_", sample_type)
  if (nrow(sheet_larger_out) > 0) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, sheet_larger_out)
    message("    Exported ", nrow(sheet_larger_out),
            " counties with larger out-migration coef to sheet: ", sheet_name)
  } else {
    message("    No counties with larger out-migration coef - skipping: ", sheet_name)
  }

  # Sheet 2: Counties with smaller in-migration coefficients than Multnomah
  sheet_smaller_in <- county_coefs_merged |>
    filter(b_in < m_b_in, !is.na(b_in)) |>
    arrange(b_in) |>
    select(fips, state_name, county_name, b_in, se_in, t_in, p_in,
           pctile_b_in, pctile_t_in)

  sheet_name <- paste0("smaller_in_", sample_type)
  if (nrow(sheet_smaller_in) > 0) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, sheet_smaller_in)
    message("    Exported ", nrow(sheet_smaller_in),
            " counties with smaller in-migration coef to sheet: ", sheet_name)
  } else {
    message("    No counties with smaller in-migration coef - skipping: ", sheet_name)
  }

  # Sheet 3: Counties worse on BOTH dimensions
  sheet_worse_both <- county_coefs_merged |>
    filter(b_out > m_b_out, b_in < m_b_in,
           !is.na(b_out), !is.na(b_in)) |>
    arrange(desc(b_out)) |>
    select(fips, state_name, county_name,
           b_out, se_out, t_out, p_out, pctile_b_out, pctile_t_out,
           b_in, se_in, t_in, p_in, pctile_b_in, pctile_t_in)

  sheet_name <- paste0("worse_both_", sample_type)
  if (nrow(sheet_worse_both) > 0) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, sheet_worse_both)
    message("    Exported ", nrow(sheet_worse_both),
            " counties worse on both dimensions to sheet: ", sheet_name)
  } else {
    message("    No counties worse on both dimensions - skipping: ", sheet_name)
  }

  # Sheet 4: Counties with larger out-migration t-statistics
  sheet_larger_t_out <- county_coefs_merged |>
    filter(t_out > m_t_out, !is.na(t_out)) |>
    arrange(desc(t_out)) |>
    select(fips, state_name, county_name, b_out, se_out, t_out, p_out,
           pctile_b_out, pctile_t_out)

  sheet_name <- paste0("larger_t_out_", sample_type)
  if (nrow(sheet_larger_t_out) > 0) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, sheet_larger_t_out)
    message("    Exported ", nrow(sheet_larger_t_out),
            " counties with larger out-migration t-stat to sheet: ", sheet_name)
  } else {
    message("    No counties with larger out-migration t-stat - skipping: ", sheet_name)
  }

  # Sheet 5: Counties with smaller in-migration t-statistics
  sheet_smaller_t_in <- county_coefs_merged |>
    filter(t_in < m_t_in, !is.na(t_in)) |>
    arrange(t_in) |>
    select(fips, state_name, county_name, b_in, se_in, t_in, p_in,
           pctile_b_in, pctile_t_in)

  sheet_name <- paste0("smaller_t_in_", sample_type)
  if (nrow(sheet_smaller_t_in) > 0) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, sheet_smaller_t_in)
    message("    Exported ", nrow(sheet_smaller_t_in),
            " counties with smaller in-migration t-stat to sheet: ", sheet_name)
  } else {
    message("    No counties with smaller in-migration t-stat - skipping: ", sheet_name)
  }

  # ============================================================================
  # REGRESSION 2: Event Study (Multnomah x Year)
  # ============================================================================
  message("  REGRESSION 2: Event study...")

  for (outcome in outcomes) {

    outcome_label <- outcome_labels[outcome]
    message("    Event study for: ", outcome_label)

    # Build formula with year-by-year interactions
    x_out_vars <- paste0("x_out_", event_years)
    x_in_vars  <- paste0("x_in_", event_years)
    covar_fml  <- paste(covar_names, collapse = " + ")

    # Regression 2a: With covariates
    fml_es_wc <- as.formula(paste0(
      outcome, " ~ ",
      paste(c(x_out_vars, x_in_vars), collapse = " + "),
      " + ", covar_fml,
      " | year + flow_id"
    ))

    fit_es_wc <- tryCatch(
      fepois(fml_es_wc,
             data = df |> filter(mover == 1),
             cluster = ~flow_id),
      error = function(e) {
        warning("Event study (with covariates) failed for ", outcome,
                " (", sample_type, "): ", conditionMessage(e))
        NULL
      }
    )

    # Regression 2b: Without covariates
    fml_es_nc <- as.formula(paste0(
      outcome, " ~ ",
      paste(c(x_out_vars, x_in_vars), collapse = " + "),
      " | year + flow_id"
    ))

    fit_es_nc <- tryCatch(
      fepois(fml_es_nc,
             data = df |> filter(mover == 1),
             cluster = ~flow_id),
      error = function(e) {
        warning("Event study (no covariates) failed for ", outcome,
                " (", sample_type, "): ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(fit_es_wc) || is.null(fit_es_nc)) next

    # --- Build coefficient data for plotting ---
    ct_wc <- coeftable(fit_es_wc)
    ct_nc <- coeftable(fit_es_nc)

    es_data <- tibble(year = 2016:2022)

    # Initialize columns
    for (prefix in c("out_coef_wc", "out_ci_lo_wc", "out_ci_hi_wc",
                     "in_coef_wc",  "in_ci_lo_wc",  "in_ci_hi_wc",
                     "out_coef_nc", "out_ci_lo_nc", "out_ci_hi_nc",
                     "in_coef_nc",  "in_ci_lo_nc",  "in_ci_hi_nc")) {
      es_data[[prefix]] <- NA_real_
    }

    for (y in event_years) {
      out_var_name <- paste0("x_out_", y)
      in_var_name  <- paste0("x_in_", y)
      row_idx <- which(es_data$year == y)

      # With covariates
      if (out_var_name %in% rownames(ct_wc)) {
        es_data$out_coef_wc[row_idx]  <- ct_wc[out_var_name, "Estimate"]
        es_data$out_ci_lo_wc[row_idx] <- ct_wc[out_var_name, "Estimate"] -
          1.96 * ct_wc[out_var_name, "Std. Error"]
        es_data$out_ci_hi_wc[row_idx] <- ct_wc[out_var_name, "Estimate"] +
          1.96 * ct_wc[out_var_name, "Std. Error"]
      }
      if (in_var_name %in% rownames(ct_wc)) {
        es_data$in_coef_wc[row_idx]  <- ct_wc[in_var_name, "Estimate"]
        es_data$in_ci_lo_wc[row_idx] <- ct_wc[in_var_name, "Estimate"] -
          1.96 * ct_wc[in_var_name, "Std. Error"]
        es_data$in_ci_hi_wc[row_idx] <- ct_wc[in_var_name, "Estimate"] +
          1.96 * ct_wc[in_var_name, "Std. Error"]
      }

      # Without covariates
      if (out_var_name %in% rownames(ct_nc)) {
        es_data$out_coef_nc[row_idx]  <- ct_nc[out_var_name, "Estimate"]
        es_data$out_ci_lo_nc[row_idx] <- ct_nc[out_var_name, "Estimate"] -
          1.96 * ct_nc[out_var_name, "Std. Error"]
        es_data$out_ci_hi_nc[row_idx] <- ct_nc[out_var_name, "Estimate"] +
          1.96 * ct_nc[out_var_name, "Std. Error"]
      }
      if (in_var_name %in% rownames(ct_nc)) {
        es_data$in_coef_nc[row_idx]  <- ct_nc[in_var_name, "Estimate"]
        es_data$in_ci_lo_nc[row_idx] <- ct_nc[in_var_name, "Estimate"] -
          1.96 * ct_nc[in_var_name, "Std. Error"]
        es_data$in_ci_hi_nc[row_idx] <- ct_nc[in_var_name, "Estimate"] +
          1.96 * ct_nc[in_var_name, "Std. Error"]
      }
    }

    # Base year (2020) = 0
    base_row <- which(es_data$year == 2020)
    coef_cols <- setdiff(names(es_data), "year")
    es_data[base_row, coef_cols] <- 0

    # Offset years slightly for visibility
    es_data <- es_data |>
      mutate(
        year_out = year - 0.1,
        year_in  = year + 0.1
      )

    # --- Event Study Plot 1: Out-migration (with and without covariates) ---
    p_es_out <- ggplot(es_data) +
      # With covariates
      geom_errorbar(aes(x = year_out, ymin = out_ci_lo_wc, ymax = out_ci_hi_wc),
                    width = 0.1, color = "navy", na.rm = TRUE) +
      geom_point(aes(x = year_out, y = out_coef_wc), color = "navy",
                 shape = 16, size = 2.5, na.rm = TRUE) +
      geom_line(aes(x = year_out, y = out_coef_wc), color = "navy",
                na.rm = TRUE) +
      # Without covariates
      geom_errorbar(aes(x = year_in, ymin = out_ci_lo_nc, ymax = out_ci_hi_nc),
                    width = 0.1, color = "maroon", na.rm = TRUE) +
      geom_point(aes(x = year_in, y = out_coef_nc), color = "maroon",
                 shape = 17, size = 2.5, na.rm = TRUE) +
      geom_line(aes(x = year_in, y = out_coef_nc), color = "maroon",
                na.rm = TRUE) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      geom_vline(xintercept = 2020.5, linetype = "solid", color = "black") +
      scale_x_continuous(breaks = 2016:2022) +
      labs(
        title    = paste0("Out-Migration from Multnomah County", title_suffix),
        subtitle = outcome_label,
        y        = "Coefficient (relative to 2020)",
        x        = "Year"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position  = "bottom",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
      )

    # Add manual legend
    legend_data_out <- tibble(
      x = c(NA, NA), y = c(NA, NA),
      model = c("With Covariates", "Without Covariates")
    )
    p_es_out <- p_es_out +
      geom_point(data = legend_data_out, aes(x = x, y = y, color = model, shape = model),
                 na.rm = TRUE) +
      scale_color_manual(values = c("With Covariates" = "navy",
                                    "Without Covariates" = "maroon")) +
      scale_shape_manual(values = c("With Covariates" = 16,
                                    "Without Covariates" = 17)) +
      labs(color = NULL, shape = NULL)

    ggsave(file.path(results_flows,
                      paste0("fig_multnomah_out_", outcome, file_suffix, ".png")),
           p_es_out, width = 8, height = 6, dpi = 300)

    # --- Event Study Plot 2: In-migration (with and without covariates) ---
    p_es_in <- ggplot(es_data) +
      geom_errorbar(aes(x = year_out, ymin = in_ci_lo_wc, ymax = in_ci_hi_wc),
                    width = 0.1, color = "navy", na.rm = TRUE) +
      geom_point(aes(x = year_out, y = in_coef_wc), color = "navy",
                 shape = 16, size = 2.5, na.rm = TRUE) +
      geom_line(aes(x = year_out, y = in_coef_wc), color = "navy",
                na.rm = TRUE) +
      geom_errorbar(aes(x = year_in, ymin = in_ci_lo_nc, ymax = in_ci_hi_nc),
                    width = 0.1, color = "maroon", na.rm = TRUE) +
      geom_point(aes(x = year_in, y = in_coef_nc), color = "maroon",
                 shape = 17, size = 2.5, na.rm = TRUE) +
      geom_line(aes(x = year_in, y = in_coef_nc), color = "maroon",
                na.rm = TRUE) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      geom_vline(xintercept = 2020.5, linetype = "solid", color = "black") +
      scale_x_continuous(breaks = 2016:2022) +
      labs(
        title    = paste0("In-Migration to Multnomah County", title_suffix),
        subtitle = outcome_label,
        y        = "Coefficient (relative to 2020)",
        x        = "Year"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position  = "bottom",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
      ) +
      geom_point(data = legend_data_out, aes(x = x, y = y, color = model, shape = model),
                 na.rm = TRUE) +
      scale_color_manual(values = c("With Covariates" = "navy",
                                    "Without Covariates" = "maroon")) +
      scale_shape_manual(values = c("With Covariates" = 16,
                                    "Without Covariates" = 17)) +
      labs(color = NULL, shape = NULL)

    ggsave(file.path(results_flows,
                      paste0("fig_multnomah_in_", outcome, file_suffix, ".png")),
           p_es_in, width = 8, height = 6, dpi = 300)

    # --- Event Study Plot 3: Both flows (with covariates only) ---
    p_es_both <- ggplot(es_data) +
      # Out-migration (navy)
      geom_errorbar(aes(x = year_out, ymin = out_ci_lo_wc, ymax = out_ci_hi_wc),
                    width = 0.1, color = "navy", na.rm = TRUE) +
      geom_point(aes(x = year_out, y = out_coef_wc), color = "navy",
                 shape = 16, size = 2.5, na.rm = TRUE) +
      geom_line(aes(x = year_out, y = out_coef_wc), color = "navy",
                na.rm = TRUE) +
      # In-migration (maroon)
      geom_errorbar(aes(x = year_in, ymin = in_ci_lo_wc, ymax = in_ci_hi_wc),
                    width = 0.1, color = "maroon", na.rm = TRUE) +
      geom_point(aes(x = year_in, y = in_coef_wc), color = "maroon",
                 shape = 17, size = 2.5, na.rm = TRUE) +
      geom_line(aes(x = year_in, y = in_coef_wc), color = "maroon",
                na.rm = TRUE) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      geom_vline(xintercept = 2020.5, linetype = "solid", color = "black") +
      scale_x_continuous(breaks = 2016:2022) +
      labs(
        title    = paste0("Migration Flows: Multnomah County", title_suffix),
        subtitle = outcome_label,
        y        = "Coefficient (relative to 2020)",
        x        = "Year"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position  = "bottom",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
      )

    # Legend for both flows
    legend_data_both <- tibble(
      x = c(NA, NA), y = c(NA, NA),
      flow = c("Out-migration", "In-migration")
    )
    p_es_both <- p_es_both +
      geom_point(data = legend_data_both, aes(x = x, y = y, color = flow, shape = flow),
                 na.rm = TRUE) +
      scale_color_manual(values = c("Out-migration" = "navy",
                                    "In-migration" = "maroon")) +
      scale_shape_manual(values = c("Out-migration" = 16,
                                    "In-migration" = 17)) +
      labs(color = NULL, shape = NULL)

    ggsave(file.path(results_flows,
                      paste0("fig_multnomah_both_", outcome, file_suffix, ".png")),
           p_es_both, width = 8, height = 6, dpi = 300)

  } # END OUTCOME LOOP (Event Study)

} # END SAMPLE LOOP (all vs acs)

# Save Excel workbook
excel_path <- file.path(results_flows,
                        paste0("flow_analysis_stats", debug_txt, ".xlsx"))
saveWorkbook(wb, excel_path, overwrite = TRUE)
message("Excel output saved to: ", excel_path)

message("")
message("=== 02_flow_analysis.R: Complete ===")
