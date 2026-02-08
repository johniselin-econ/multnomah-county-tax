# =============================================================================
# 02_indiv_analysis.R
# Individual-level migration analysis by demographic subgroups
#
# Converted from: code/02_indiv_analysis.do
# Author: John Iselin
#
# Purpose: Examine migration patterns by demographic subgroups using
#          HDFE regressions with category x year interactions and
#          adjusted conditional mean plots (equivalent to Stata margins).
#
# Outputs:
# - fig_{cat}_{1|2}.png: Adjusted mean plots for each category x sample
#   where 1 = out-migration, 2 = in-migration
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(ggplot2)
  library(marginaleffects)
})

source(here::here("R", "utils", "globals.R"))
source(here::here("R", "utils", "helpers.R"))

message("=== 02_indiv_analysis.R: Starting individual-level analysis ===")

if (!dir.exists(results_indiv)) dir.create(results_indiv, recursive = TRUE)

# =============================================================================
# HELPER: hdfe_catyear_plot
#
# R equivalent of the Stata program hdfe_catyear_plot.
# Runs feols with cat x year interactions and absorbed FEs,
# computes adjusted conditional means via marginaleffects::avg_predictions,
# and plots one series per category level.
#
# @param data       Data frame
# @param outcome    Character: name of outcome variable
# @param cat_var    Character: name of categorical variable
# @param year_var   Character: name of year variable
# @param absorb_vars Character vector: variables to absorb as fixed effects
# @param weight_var Character: name of weight variable (default "perwt")
# @param vce        Character: variance-covariance type (default "hetero")
# @param title      Character: plot title (optional)
# @param xtitle     Character: x-axis label
# @param ytitle     Character: y-axis label (auto-generated if NULL)
# @param show_ci    Logical: show confidence intervals (default TRUE)
# @param baseyear   Integer: normalize to this year (0 = plot levels)
# @param save_path  Character: file path for export (optional)
# =============================================================================

hdfe_catyear_plot <- function(data, outcome, cat_var, year_var, absorb_vars,
                               weight_var = "perwt", vce = "hetero",
                               title = NULL,
                               xtitle = "ACS Survey Year (t=2)",
                               ytitle = NULL,
                               show_ci = TRUE,
                               baseyear = 0,
                               save_path = NULL) {

  # Convert year to factor for the regression
  data$year_f <- factor(data[[year_var]])

  # Build formula: outcome ~ cat * year_f | absorbed_FEs
  fe_str <- paste(absorb_vars, collapse = " + ")
  fml <- as.formula(paste0(outcome, " ~ ", cat_var, " * year_f | ", fe_str))

  # Run HDFE regression (weights treated as analytic/WLS)
  wt_fml <- as.formula(paste0("~", weight_var))
  model <- feols(fml, data = data, weights = wt_fml, vcov = vce)

  # Compute adjusted means by cat x year (Stata margins equivalent)
  preds <- avg_predictions(model, by = c(cat_var, "year_f"))

  # Build plot data frame
  plot_df <- as.data.frame(preds)
  plot_df$year <- as.integer(as.character(plot_df$year_f))
  plot_df$cat_level <- factor(plot_df[[cat_var]])

  # Optional normalization to base year (within-category)
  if (baseyear != 0) {
    base_vals <- plot_df |>
      filter(year == baseyear) |>
      select(cat_level, base_b = estimate)

    plot_df <- plot_df |>
      left_join(base_vals, by = "cat_level") |>
      mutate(
        estimate = estimate - base_b,
        conf.low = conf.low - base_b,
        conf.high = conf.high - base_b
      ) |>
      select(-base_b)

    if (is.null(ytitle)) ytitle <- paste0(outcome, " (relative to ", baseyear, ")")
  } else {
    if (is.null(ytitle)) ytitle <- paste0(outcome, " (adjusted mean)")
  }

  # Build ggplot
  p <- ggplot(plot_df, aes(x = year, y = estimate,
                            color = cat_level, shape = cat_level)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = stata_gs10)

  if (show_ci) {
    p <- p + geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                            width = 0.1, alpha = 0.5)
  }

  p <- p +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2)

  # Apply standard age palettes when cat_var is cat_age
  if (cat_var == "cat_age") {
    p <- p +
      scale_color_manual(values = age_colors) +
      scale_shape_manual(values = age_shapes)
  }

  p <- p +
    scale_x_continuous(breaks = sort(unique(plot_df$year))) +
    labs(
      title = title,
      x = xtitle,
      y = ytitle,
      color = NULL, shape = NULL
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  # Export
  if (!is.null(save_path)) {
    if (!grepl("\\.(png|pdf)$", save_path)) save_path <- paste0(save_path, ".png")
    ggsave(save_path, p, width = 10, height = 6, dpi = 300)
    message("    Saved: ", basename(save_path))
  }

  invisible(list(plot = p, data = plot_df, model = model))
}

# =============================================================================
# LOAD AND PREPARE DATA
# =============================================================================
message("Loading ACS microdata...")

acs <- load_data(file.path(data_working, "acs_migration_file"))

# Sample restrictions (matches Stata)
acs <- acs |>
  filter(
    year > 2015,                        # 2016-2024
    qmigplc1 != 4,                      # No errors in migration place
    !state_fips_o %in% c(2, 15),        # Drop Alaska and Hawaii (origin)
    !state_fips_d %in% c(2, 15),        # Drop Alaska and Hawaii (dest)
    ftotinc >= 0,                        # Drop negative family income
    age >= 25                            # Dropping under 25
  )

# Debug mode: sample random individuals for faster iteration
if (!exists("debug")) debug <- FALSE
debug_n <- 50000  # Number of random ACS individuals in debug mode

if (debug) {
  message("  *** DEBUG MODE: Sampling ", debug_n, " random individuals ***")
  set.seed(12345)
  acs <- acs |> slice_sample(n = min(debug_n, nrow(acs)))
}

# =============================================================================
# MULTNOMAH INDICATORS AND SAMPLES
# =============================================================================

acs <- acs |>
  mutate(
    multnomah_o = as.integer(state_fips_o == 41 & county_fips_o == 51),
    multnomah_d = as.integer(state_fips_d == 41 & county_fips_d == 51),

    # Sample 1: Out-migration (Multnomah residents)
    sample_1 = as.integer(multnomah_o == 1),
    # Sample 2: In-migration (all non-Multnomah origins)
    sample_2 = as.integer(multnomah_o != 1),

    # Outcomes (percentage scale)
    out_1 = as.numeric(same_county == 0) * 100,   # Moved out of Multnomah
    out_2 = as.numeric(multnomah_d == 1) * 100     # Moved to Multnomah
  )

# =============================================================================
# COVARIATE CATEGORIES
# =============================================================================

acs <- acs |>
  mutate(
    cat_age = cut(age, breaks = c(24, 44, 64, Inf),
                  labels = c("25-44", "45-64", "65+"), right = TRUE),

    cat_sex = factor(as.integer(sex == 2), levels = 0:1,
                     labels = c("Male", "Female")),

    cat_married = case_when(
      marst == 1     ~ "Married",
      marst %in% 2:3 ~ "Separated",
      marst %in% 4:5 ~ "Divorced / Widowed",
      marst == 6     ~ "Single",
      TRUE           ~ NA_character_
    ) |> factor(),

    cat_child = case_when(
      nchild == 0  ~ "0 Children",
      nchild == 1  ~ "1 Child",
      nchild == 2  ~ "2 Children",
      nchild >= 3  ~ "3+ Children"
    ) |> factor(),

    cat_yngch = case_when(
      yngch == 99  ~ "No Children",
      yngch <= 4   ~ "0-4",
      yngch <= 12  ~ "5-12",
      yngch <= 17  ~ "13-17",
      yngch >= 18  ~ "18+"
    ) |> factor(),

    cat_educ = case_when(
      educd <= 61   ~ "Less than HS",
      educd <= 71   ~ "HS Diploma",
      educd <= 100  ~ "Some College",
      educd >= 101  ~ "College Degree"
    ) |> factor()
  )

# =============================================================================
# REAL INCOME AND INCOME CATEGORIES
# =============================================================================
message("Computing real income categories...")

# CPI adjustment to end_year_acs (2024) dollars
cpi_base <- mean(acs$cpi99[acs$year == end_year_acs], na.rm = TRUE)
acs <- acs |>
  mutate(cpi_adj = cpi99 / cpi_base)

make_income_cat <- function(x) {
  case_when(
    x < 0        ~ "Negative income",
    x == 0       ~ "$0",
    x < 25000    ~ "$1-$25K",
    x < 50000    ~ "$25K-$50K",
    x < 100000   ~ "$50K-$100K",
    x < 200000   ~ "$100K-$200K",
    x >= 200000  ~ "$200K+"
  ) |> factor(levels = c("Negative income", "$0", "$1-$25K", "$25K-$50K",
                           "$50K-$100K", "$100K-$200K", "$200K+"))
}

for (v in c("inctot", "incwage", "incearn", "ftotinc")) {
  real_col <- paste0("real_", v)
  cat_col  <- paste0("cat_", v)
  acs[[real_col]] <- round(acs[[v]] * acs$cpi_adj)
  acs[[cat_col]]  <- make_income_cat(acs[[real_col]])
}

# =============================================================================
# ANALYSIS LOOPS
# =============================================================================
message("Running HDFE regressions by category...")

catvars <- c("cat_yngch", "cat_sex", "cat_married", "cat_age",
             "cat_child", "cat_educ", "cat_ftotinc")

for (i in 1:2) {

  ytitle_txt <- if (i == 1) "Out-migration rate (%)" else "In-migration rate (%)"
  samp <- acs |> filter(.data[[paste0("sample_", i)]] == 1)

  message("  Sample ", i, " (", ifelse(i == 1, "out-migration", "in-migration"),
          "): ", format(nrow(samp), big.mark = ","), " observations")

  for (cat in catvars) {

    message("    Category: ", cat)

    # Absorb all OTHER categories plus origin county FEs
    othercats <- setdiff(catvars, cat)
    absorb <- c("state_fips_o", "county_fips_o", othercats)

    save_file <- file.path(results_indiv, paste0("fig_", cat, "_", i, ".png"))

    tryCatch({
      hdfe_catyear_plot(
        data        = samp,
        outcome     = paste0("out_", i),
        cat_var     = cat,
        year_var    = "year",
        absorb_vars = absorb,
        weight_var  = "perwt",
        vce         = "hetero",
        xtitle      = "ACS Survey Year (t=2)",
        ytitle      = ytitle_txt,
        save_path   = save_file
      )
    }, error = function(e) {
      warning("  Failed for ", cat, " sample_", i, ": ", conditionMessage(e))
    })
  }
}

message("=== 02_indiv_analysis.R: Complete ===")
