# =============================================================================
# 02_did_analysis.R
# Difference-in-Differences analysis on ACS data
#
# Converted from: code/02_did_analysis.do
# Author: John Iselin
#
# Purpose: Examine the effect of the Multnomah County tax on migration
#          patterns by education level using DiD and event study designs.
#
# Outputs:
# - fig_did_coefplot.png: DiD coefficient plot
# - fig_es_out_migration.png: Out-migration event study
# - fig_es_in_migration_west.png: In-migration event study (West Coast)
# - fig_es_in_migration_48.png: In-migration event study (Lower 48)
# - fig_es_combined.png: Combined event study
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(fixest)
  library(ggplot2)
  library(patchwork)
})

source(file.path(r_dir, "utils", "globals.R"))
source(file.path(r_dir, "utils", "helpers.R"))

message("=== 02_did_analysis.R: Starting DiD analysis ===")

# Ensure output directory exists
if (!dir.exists(results_did)) dir.create(results_did, recursive = TRUE)

# =============================================================================
# LOAD AND PREPARE DATA
# =============================================================================

acs <- load_data(file.path(data_working, "acs_migration_file"))

# Sample restrictions
acs <- acs |>
  filter(
    year > 2015,                              # 2016-2024
    qmigplc1 != 4,                            # No errors in migration place
    !state_fips_o %in% c(2, 15),              # Drop Alaska and Hawaii (origin)
    !state_fips_d %in% c(2, 15),              # Drop Alaska and Hawaii (dest)
    age >= 25,                                # Dropping under 25
    ftotinc >= 0                              # Negative Family Income
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
# DEFINE SAMPLES AND TREATMENT
# =============================================================================

acs <- acs |>
  mutate(
    # Multnomah county indicators
    multnomah_o = as.integer(state_fips_o == 41 & county_fips_o == 51),
    multnomah_d = as.integer(state_fips_d == 41 & county_fips_d == 51),

    # Analysis samples
    sample_1 = as.integer(multnomah_o == 1),         # Out-migration
    sample_2 = as.integer(multnomah_o != 1 &          # In-migration (West Coast)
                            state_fips_o %in% c(6, 41, 53)),
    sample_3 = as.integer(multnomah_o != 1),          # In-migration (Lower 48)

    # Outcome variables (scaled to percentages)
    out_1 = as.numeric(same_county == 0) * 100,       # Moved out of Multnomah
    out_2 = as.numeric(multnomah_d == 1) * 100,       # Moved to Multnomah
    out_3 = as.numeric(state_fips_o != state_fips_d) * 100,  # Moved out of Oregon

    # Treatment variables
    post    = as.integer(year > 2020),
    high_ed = as.integer(educd >= 101),
    treated = high_ed * post
  )

# =============================================================================
# COVARIATE CATEGORIES
# =============================================================================

acs <- acs |>
  mutate(
    # Age categories
    cat_age = cut(age,
                  breaks = c(24, 44, 64, Inf),
                  labels = c("25-44", "45-64", "65+"),
                  right = TRUE),

    # Sex
    cat_sex = factor(as.integer(sex == 2), levels = 0:1,
                     labels = c("Male", "Female")),

    # Marital status
    cat_married = case_when(
      marst == 1         ~ "Married",
      marst %in% 2:3     ~ "Separated",
      marst %in% 4:5     ~ "Divorced / Widowed",
      marst == 6         ~ "Single",
      TRUE               ~ NA_character_
    ) |> factor(),

    # Number of children
    cat_child = case_when(
      nchild == 0         ~ "0 Children",
      nchild == 1         ~ "1 Child",
      nchild == 2         ~ "2 Children",
      nchild >= 3         ~ "3+ Children"
    ) |> factor(),

    # Age of youngest child
    cat_yngch = case_when(
      yngch == 99         ~ "No Children",
      yngch <= 4          ~ "0-4",
      yngch <= 12         ~ "5-12",
      yngch <= 17         ~ "13-17",
      yngch >= 18         ~ "18+"
    ) |> factor(),

    # Education categories
    cat_educ = case_when(
      educd <= 61         ~ "Less than HS",
      educd <= 71         ~ "HS Diploma",
      educd <= 100        ~ "Some College",
      educd >= 101        ~ "College Degree"
    ) |> factor()
  )

# =============================================================================
# REGRESSION 1: DIFFERENCE-IN-DIFFERENCES
# =============================================================================
message("Running DiD regressions...")

# Out-migration (Sample 1: Multnomah residents)
did_out <- feols(
  out_1 ~ treated | year + cat_age + cat_sex + cat_married + cat_child +
    cat_yngch + cat_educ,
  data = filter(acs, sample_1 == 1),
  weights = ~ perwt,
  vcov = "hetero"
)

# In-migration (Sample 2: CA/OR/WA residents)
did_in_west <- feols(
  out_2 ~ treated | year + fips_o + cat_age + cat_sex + cat_married +
    cat_child + cat_yngch + cat_educ,
  data = filter(acs, sample_2 == 1),
  weights = ~ perwt,
  cluster = ~ fips_o
)

# In-migration (Sample 3: Lower 48 + DC residents)
did_in_48 <- feols(
  out_2 ~ treated | year + fips_o + cat_age + cat_sex + cat_married +
    cat_child + cat_yngch + cat_educ,
  data = filter(acs, sample_3 == 1),
  weights = ~ perwt,
  cluster = ~ fips_o
)

# Out-of-state migration (Sample 1: Multnomah residents)
did_out_state <- feols(
  out_3 ~ treated | year + cat_age + cat_sex + cat_married + cat_child +
    cat_yngch + cat_educ,
  data = filter(acs, sample_1 == 1),
  weights = ~ perwt,
  vcov = "hetero"
)

# =============================================================================
# TABLE: DiD RESULTS
# =============================================================================
message("Creating DiD results table...")

# Keep model_colors for event study combined plot
model_colors <- c(
  "Out-migration from Multnomah"    = "#1f77b4",
  "Out-of-state from Multnomah"     = "#9467bd",
  "In-migration (West Coast)"       = "#d62728",
  "In-migration (Lower 48 + DC)"    = "#2ca02c"
)

etable(did_out, did_out_state, did_in_west, did_in_48,
       keep = "treated",
       dict = c(treated = "College Degree x Post"),
       headers = c("Out-migration", "Out-of-state",
                    "In-migration (West Coast)",
                    "In-migration (Lower 48 + DC)"),
       tex = TRUE,
       file = file.path(results_did, "tab_did_overall.tex"),
       replace = TRUE,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
       fitstat = ~ n)

# =============================================================================
# REGRESSION 2: EVENT STUDY
# =============================================================================
message("Running event study regressions...")

# Create year x education interactions (base year = 2019, 2020 excluded)
event_years <- setdiff(2016:2024, c(2019, 2020))
for (y in event_years) {
  acs[[paste0("x_high_ed_", y)]] <- acs$high_ed * as.integer(acs$year == y)
}

x_vars <- paste0("x_high_ed_", event_years)
es_formula_out <- as.formula(
  paste("out_1 ~", paste(x_vars, collapse = " + "),
        "| year + cat_age + cat_sex + cat_married + cat_child + cat_yngch + cat_educ")
)
es_formula_in_fe <- as.formula(
  paste("out_2 ~", paste(x_vars, collapse = " + "),
        "| year + fips_o + cat_age + cat_sex + cat_married + cat_child + cat_yngch + cat_educ")
)

# Out-migration event study
es_out <- feols(es_formula_out,
                data = filter(acs, sample_1 == 1),
                weights = ~ perwt, vcov = "hetero")

# In-migration event study (West Coast)
es_in_west <- feols(es_formula_in_fe,
                    data = filter(acs, sample_2 == 1),
                    weights = ~ perwt, cluster = ~ fips_o)

# In-migration event study (Lower 48)
es_in_48 <- feols(es_formula_in_fe,
                  data = filter(acs, sample_3 == 1),
                  weights = ~ perwt, cluster = ~ fips_o)

# Out-of-state migration event study
es_formula_out_state <- as.formula(
  paste("out_3 ~", paste(x_vars, collapse = " + "),
        "| year + cat_age + cat_sex + cat_married + cat_child + cat_yngch + cat_educ")
)
es_out_state <- feols(es_formula_out_state,
                      data = filter(acs, sample_1 == 1),
                      weights = ~ perwt, vcov = "hetero")

# =============================================================================
# EVENT STUDY PLOTS
# =============================================================================
message("Creating event study plots...")

# Helper: extract event study coefficients
extract_es_coefs <- function(model, label) {
  cf <- broom::tidy(model, conf.int = TRUE) |>
    filter(grepl("x_high_ed_", term)) |>
    mutate(year = as.integer(gsub("x_high_ed_", "", term)))

  # Add base year (2019 = 0) and 2020 (excluded, NA)
  base_rows <- tibble(
    term = c("x_high_ed_2019", "x_high_ed_2020"),
    estimate = c(0, NA_real_),
    std.error = c(0, NA_real_),
    conf.low = c(0, NA_real_),
    conf.high = c(0, NA_real_),
    year = c(2019L, 2020L)
  )

  bind_rows(cf, base_rows) |>
    arrange(year) |>
    mutate(model = label)
}

es_data <- bind_rows(
  extract_es_coefs(es_out, "Out-migration from Multnomah"),
  extract_es_coefs(es_out_state, "Out-of-state from Multnomah"),
  extract_es_coefs(es_in_west, "In-migration (West Coast)"),
  extract_es_coefs(es_in_48, "In-migration (Lower 48 + DC)")
)

# Individual event study plots
make_es_plot <- function(data, title_text, color) {
  ggplot(data, aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 2020.5, linetype = "solid", color = "black") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = color) +
    geom_line(color = color, linewidth = 1) +
    geom_point(color = color, size = 2.5) +
    scale_x_continuous(breaks = 2016:2024) +
    labs(
      title = title_text,
      subtitle = "College Degree vs. No College Degree",
      x = "Year", y = "Effect on Migration Rate (pp)",
      caption = "Base year: 2019. 2020 excluded. Vertical line indicates tax implementation."
    )
}

p_es_out <- make_es_plot(
  filter(es_data, model == "Out-migration from Multnomah"),
  "Out-Migration from Multnomah County", "#1f77b4"
)

p_es_in_west <- make_es_plot(
  filter(es_data, model == "In-migration (West Coast)"),
  "In-Migration to Multnomah County (West Coast)", "#8b0000"
)

p_es_in_48 <- make_es_plot(
  filter(es_data, model == "In-migration (Lower 48 + DC)"),
  "In-Migration to Multnomah County (Lower 48 + DC)", "#228b22"
)

p_es_out_state <- make_es_plot(
  filter(es_data, model == "Out-of-state from Multnomah"),
  "Out-of-State Migration from Multnomah County", "#9467bd"
)

ggsave(file.path(results_did, "fig_es_out_migration.png"), p_es_out,
       width = 10, height = 6, dpi = 300)
ggsave(file.path(results_did, "fig_es_in_migration_west.png"), p_es_in_west,
       width = 10, height = 6, dpi = 300)
ggsave(file.path(results_did, "fig_es_in_migration_48.png"), p_es_in_48,
       width = 10, height = 6, dpi = 300)
ggsave(file.path(results_did, "fig_es_out_state_migration.png"), p_es_out_state,
       width = 10, height = 6, dpi = 300)

# Combined event study plot
es_combined <- es_data |>
  mutate(year_offset = case_when(
    model == "Out-migration from Multnomah"  ~ year - 0.2,
    model == "Out-of-state from Multnomah"   ~ year - 0.07,
    model == "In-migration (West Coast)"     ~ year + 0.07,
    model == "In-migration (Lower 48 + DC)"  ~ year + 0.2
  ))

p_es_combined <- ggplot(es_combined, aes(x = year_offset, y = estimate,
                                          color = model, shape = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 2020.5, linetype = "solid", color = "black") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.5) +
  scale_color_manual(values = model_colors) +
  scale_x_continuous(breaks = 2016:2024) +
  labs(
    title = "Migration Effects: Multnomah County Tax",
    subtitle = "College Degree vs. No College Degree",
    x = "Year", y = "Effect on Migration Rate (pp)",
    color = NULL, shape = NULL,
    caption = "Base year: 2019. 2020 excluded. Vertical line indicates tax implementation."
  ) +
  theme(legend.position = "bottom") +
  guides(shape = "none")

ggsave(file.path(results_did, "fig_es_combined.png"), p_es_combined,
       width = 10, height = 6, dpi = 300)

# =============================================================================
# REGRESSION 3: DIFFERENCE-IN-DIFFERENCES BY AGE
# =============================================================================
message("Running DiD regressions by age...")

# Create age x treatment interactions
acs <- acs |>
  mutate(
    treated_age_1 = treated * as.integer(cat_age == "25-44"),
    treated_age_2 = treated * as.integer(cat_age == "45-64"),
    treated_age_3 = treated * as.integer(cat_age == "65+")
  )

# Out-migration by age (Sample 1)
did_age_out <- feols(
  out_1 ~ treated_age_1 + treated_age_2 + treated_age_3 |
    year + cat_age + cat_sex + cat_married + cat_child + cat_yngch + cat_educ,
  data = filter(acs, sample_1 == 1),
  weights = ~ perwt,
  vcov = "hetero"
)

# In-migration by age (Sample 2: West Coast)
did_age_in_west <- feols(
  out_2 ~ treated_age_1 + treated_age_2 + treated_age_3 |
    year + fips_o + cat_age + cat_sex + cat_married + cat_child + cat_yngch + cat_educ,
  data = filter(acs, sample_2 == 1),
  weights = ~ perwt,
  cluster = ~ fips_o
)

# In-migration by age (Sample 3: Lower 48 + DC)
did_age_in_48 <- feols(
  out_2 ~ treated_age_1 + treated_age_2 + treated_age_3 |
    year + fips_o + cat_age + cat_sex + cat_married + cat_child + cat_yngch + cat_educ,
  data = filter(acs, sample_3 == 1),
  weights = ~ perwt,
  cluster = ~ fips_o
)

# Out-of-state migration by age (Sample 1)
did_age_out_state <- feols(
  out_3 ~ treated_age_1 + treated_age_2 + treated_age_3 |
    year + cat_age + cat_sex + cat_married + cat_child + cat_yngch + cat_educ,
  data = filter(acs, sample_1 == 1),
  weights = ~ perwt,
  vcov = "hetero"
)

# =============================================================================
# TABLE: DiD BY AGE
# =============================================================================
message("Creating DiD by age results table...")

etable(did_age_out, did_age_out_state, did_age_in_west, did_age_in_48,
       keep = "treated_age_",
       order = c("treated_age_1", "treated_age_2", "treated_age_3"),
       dict = c(treated_age_1 = "College x Post x Age 25-44",
                treated_age_2 = "College x Post x Age 45-64",
                treated_age_3 = "College x Post x Age 65+"),
       headers = c("Out-migration", "Out-of-state",
                    "In-migration (West Coast)",
                    "In-migration (Lower 48 + DC)"),
       tex = TRUE,
       file = file.path(results_did, "tab_did_by_age.tex"),
       replace = TRUE,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
       fitstat = ~ n)

# =============================================================================
# REGRESSION 4: EVENT STUDY BY AGE
# =============================================================================
message("Running event study by age regressions...")

# Create year x education x age interactions (base year = 2019, 2020 excluded)
for (y in event_years) {
  for (a in 1:3) {
    age_level <- c("25-44", "45-64", "65+")[a]
    var_name <- paste0("x_he_", y, "_age", a)
    acs[[var_name]] <- acs$high_ed * as.integer(acs$year == y) *
      as.integer(acs$cat_age == age_level)
  }
}

age_x_vars <- paste0("x_he_", rep(event_years, each = 3), "_age", 1:3)
es_age_formula_out <- as.formula(
  paste("out_1 ~", paste(age_x_vars, collapse = " + "),
        "| year + cat_age + cat_sex + cat_married + cat_child + cat_yngch + cat_educ")
)
es_age_formula_in_fe <- as.formula(
  paste("out_2 ~", paste(age_x_vars, collapse = " + "),
        "| year + fips_o + cat_age + cat_sex + cat_married + cat_child + cat_yngch + cat_educ")
)

es_age_out <- feols(es_age_formula_out,
                     data = filter(acs, sample_1 == 1),
                     weights = ~ perwt, vcov = "hetero")

es_age_in_west <- feols(es_age_formula_in_fe,
                         data = filter(acs, sample_2 == 1),
                         weights = ~ perwt, cluster = ~ fips_o)

es_age_in_48 <- feols(es_age_formula_in_fe,
                       data = filter(acs, sample_3 == 1),
                       weights = ~ perwt, cluster = ~ fips_o)

# Out-of-state migration event study by age
es_age_formula_out_state <- as.formula(
  paste("out_3 ~", paste(age_x_vars, collapse = " + "),
        "| year + cat_age + cat_sex + cat_married + cat_child + cat_yngch + cat_educ")
)
es_age_out_state <- feols(es_age_formula_out_state,
                           data = filter(acs, sample_1 == 1),
                           weights = ~ perwt, vcov = "hetero")

# =============================================================================
# EVENT STUDY BY AGE PLOTS
# =============================================================================
message("Creating event study by age plots...")

extract_age_es_coefs <- function(model, model_label) {
  cf <- broom::tidy(model, conf.int = TRUE) |>
    filter(grepl("x_he_", term)) |>
    mutate(
      year = as.integer(str_extract(term, "\\d{4}")),
      age_num = as.integer(str_extract(term, "\\d$")),
      age_group = c("25-44", "45-64", "65+")[age_num]
    )

  # Add base year (2019) for each age group, and 2020 as NA
  base_rows <- tibble(
    term = paste0("x_he_", rep(c(2019, 2020), each = 3), "_age", 1:3),
    estimate = rep(c(0, NA_real_), each = 3),
    std.error = rep(c(0, NA_real_), each = 3),
    conf.low = rep(c(0, NA_real_), each = 3),
    conf.high = rep(c(0, NA_real_), each = 3),
    year = rep(c(2019L, 2020L), each = 3),
    age_num = rep(1:3, 2),
    age_group = rep(c("25-44", "45-64", "65+"), 2)
  )

  bind_rows(cf, base_rows) |>
    arrange(age_group, year) |>
    mutate(model = model_label)
}

age_es_data <- bind_rows(
  extract_age_es_coefs(es_age_out, "Out-migration"),
  extract_age_es_coefs(es_age_out_state, "Out-of-state"),
  extract_age_es_coefs(es_age_in_west, "In-migration (West Coast)"),
  extract_age_es_coefs(es_age_in_48, "In-migration (Lower 48 + DC)")
)

age_colors <- c("25-44" = "#8b0000", "45-64" = "#228b22", "65+" = "#ff8c00")

# Individual event study by age plots
plot_configs <- list(
  list(mdl = "Out-migration", title = "Out-Migration from Multnomah County by Age",
       fname = "fig_es_age_out_migration.png"),
  list(mdl = "Out-of-state", title = "Out-of-State Migration from Multnomah County by Age",
       fname = "fig_es_age_out_state_migration.png"),
  list(mdl = "In-migration (West Coast)", title = "In-Migration to Multnomah (West Coast) by Age",
       fname = "fig_es_age_in_migration_west.png"),
  list(mdl = "In-migration (Lower 48 + DC)", title = "In-Migration to Multnomah (Lower 48 + DC) by Age",
       fname = "fig_es_age_in_migration_48.png")
)

for (cfg in plot_configs) {
  pdata <- filter(age_es_data, model == cfg$mdl)

  # Offset years for visibility
  pdata <- pdata |>
    mutate(year_offset = year + case_when(
      age_group == "25-44" ~ -0.15,
      age_group == "45-64" ~  0,
      age_group == "65+"   ~  0.15
    ))

  p <- ggplot(pdata, aes(x = year_offset, y = estimate,
                           color = age_group, shape = age_group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 2020.5, linetype = "solid", color = "black") +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.4) +
    geom_line(linewidth = 0.5) +
    scale_color_manual(values = age_colors) +
    scale_x_continuous(breaks = 2016:2024) +
    labs(
      title = cfg$title,
      subtitle = "College Degree vs. No College Degree",
      x = "Year", y = "Effect on Migration Rate (pp)",
      color = "Age Group", shape = "Age Group",
      caption = "Base year: 2019. 2020 excluded. Vertical line indicates tax implementation."
    ) +
    theme(legend.position = "bottom")

  ggsave(file.path(results_did, cfg$fname), p, width = 10, height = 6, dpi = 300)
}

message("=== 02_did_analysis.R: Complete ===")
