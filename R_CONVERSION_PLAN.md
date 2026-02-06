# Multnomah County Tax: Stata to R Conversion Plan

## Overview

Convert the multnomah-county-tax repository (~3,000+ lines of Stata code across 8 analysis files) to R, preserving all functionality including Synthetic Difference-in-Differences (SDID) estimation, migration flow analysis, and visualization outputs.

---

## Phase 1: Project Infrastructure Setup

### 1.1 Create R Project Structure
```
multnomah-county-tax/
├── R/
│   ├── 00_master.R              # Main orchestration script
│   ├── 01_clean_data.R          # Data cleaning/preparation
│   ├── 02_descriptives.R        # Descriptive statistics
│   ├── 02_sdid_analysis.R       # Primary SDID estimation
│   ├── 02_did_analysis.R        # Difference-in-differences
│   ├── 02_flow_analysis.R       # County-level flow regression
│   ├── 02_indiv_analysis.R      # Individual categorical analysis
│   ├── api_code.R               # IPUMS data download (existing)
│   ├── map_code.R               # Geographic mapping (existing)
│   └── utils/
│       ├── globals.R            # Global parameters and paths
│       ├── helpers.R            # Common utility functions
│       └── sdid_functions.R     # SDID estimation wrappers
├── data/
│   ├── raw/                     # Original data sources
│   ├── interim/                 # Intermediate datasets
│   └── final/                   # Analysis-ready datasets
├── results/
│   ├── figures/
│   └── tables/
└── renv.lock                    # Package dependencies
```

### 1.2 Package Dependencies
```r
# Core data manipulation
library(tidyverse)      # dplyr, tidyr, ggplot2, readr, etc.
library(data.table)     # Fast data operations (for large datasets)
library(haven)          # Read/write Stata .dta files
library(labelled)       # Handle Stata value labels

# Econometric analysis
library(fixest)         # High-dimensional fixed effects (replaces reghdfe)
library(synthdid)       # Synthetic DiD estimation (Arkhangelsky et al.)
library(did)            # Callaway-Sant'Anna DiD
library(modelsummary)   # Regression tables (replaces esttab)
library(sandwich)       # Robust standard errors
library(lmtest)         # Coefficient testing

# Visualization
library(ggplot2)        # Primary plotting
library(patchwork)      # Combine plots
library(scales)         # Axis formatting
library(sf)             # Spatial data for maps
library(tigris)         # US Census geography

# Tables and output
library(gt)             # Publication-quality tables
library(kableExtra)     # Alternative table formatting
library(openxlsx)       # Excel export

# Data access
library(ipumsr)         # IPUMS data (already used)
library(tidycensus)     # Census/ACS API access
library(fredr)          # FRED economic data
```

### 1.3 Install Required Packages
```r
# Run this once to install all dependencies
install.packages(c(
  "tidyverse", "data.table", "haven", "labelled",
  "fixest", "synthdid", "did", "modelsummary", "sandwich", "lmtest",
  "ggplot2", "patchwork", "scales", "sf", "tigris",
  "gt", "kableExtra", "openxlsx",
  "ipumsr", "tidycensus", "fredr"
))

# Initialize renv for reproducibility (recommended)
# renv::init()
```

---

## Phase 2: Data Cleaning Conversion (01_clean_data.R)

### 2.1 Data Sources to Convert
| Source | Stata Approach | R Approach |
|--------|---------------|------------|
| IRS SOI Migration | `import delimited` | `read_csv()` with `col_types` |
| ACS/IPUMS | CSV via API | `ipumsr::read_ipums_micro()` |
| NHGIS | CSV import | `ipumsr::read_nhgis()` |
| BEA Personal Income | `import delimited` | `read_csv()` or `bea.R` API |
| BLS Unemployment | `import delimited` | `blsAPI` package |
| DOL UI Claims | `import delimited` | `read_csv()` |
| COVID Data | `import delimited` | `read_csv()` |
| Tax Parameters | Manual entry | YAML/JSON config file |
| Minimum Wage | `import delimited` | `read_csv()` |

### 2.2 Key Transformations

**IRS Migration Data Processing:**
```r
# Stata: reshape wide, egen, merge
# R equivalent:
irs_flows <- irs_raw %>%
  pivot_wider(
    names_from = year,
    values_from = c(n1, n2, agi)
  ) %>%
  group_by(state_fips, county_fips) %>%
  mutate(
    net_migration = inflow - outflow,
    migration_rate = net_migration / population
  )
```

**Panel Data Construction:**
```r
# Create balanced panel with county-year observations
panel <- expand_grid(
  county_fips = unique(counties$fips),
  year = 2011:2022
) %>%
  left_join(demographics, by = c("county_fips", "year")) %>%
  left_join(tax_data, by = c("county_fips", "year")) %>%
  mutate(
    treated = county_fips == "41051",  # Multnomah
    post = year >= 2021,
    treat_post = treated * post
  )
```

### 2.3 Sample Restrictions
Convert Stata `if` conditions to dplyr `filter()`:
```r
# Stata: keep if inrange(year, 2011, 2022) & state_fips == 41
# R:
analysis_sample <- panel %>%
  filter(
    between(year, 2011, 2022),
    state_fips == 41  # Oregon
  )
```

---

## Phase 3: SDID Analysis Conversion (02_sdid_analysis.R)

### 3.1 SDID Implementation Strategy

The `synthdid` package (Arkhangelsky et al. 2021) provides native R implementation:

```r
library(synthdid)

# Prepare data in required format (balanced panel matrix)
setup <- panel_matrices(

panel_data,
  unit = "county_fips",
  time = "year",
  outcome = "net_migration_rate",
  treatment = "treated"
)

# Estimate SDID
sdid_est <- synthdid_estimate(setup$Y, setup$N0, setup$T0)

# Get standard errors via placebo/bootstrap
se <- sqrt(vcov(sdid_est, method = "placebo"))

# Extract weights for transparency
unit_weights <- attr(sdid_est, "weights")$omega
time_weights <- attr(sdid_est, "weights")$lambda
```

### 3.2 Multiple Outcome Variables
```r
outcomes <- c("net_migration_rate", "inflow_rate", "outflow_rate",
              "agi_per_return", "young_migration", "high_income_migration")

sdid_results <- map_dfr(outcomes, function(y) {
  setup <- panel_matrices(panel_data, outcome = y, ...)
  est <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
  tibble(
    outcome = y,
    estimate = as.numeric(est),
    se = sqrt(vcov(est, method = "jackknife")),
    ci_lower = estimate - 1.96 * se,
    ci_upper = estimate + 1.96 * se
  )
})
```

### 3.3 Bootstrap Confidence Intervals
```r
# Replicate Stata's bootstrap approach
bootstrap_sdid <- function(data, outcome, n_boot = 500) {
  boot_estimates <- map_dbl(1:n_boot, function(b) {
    # Block bootstrap by unit
    units <- unique(data$county_fips)
    boot_units <- sample(units, replace = TRUE)
    boot_data <- map_dfr(boot_units, ~filter(data, county_fips == .x))

    setup <- panel_matrices(boot_data, outcome = outcome, ...)
    as.numeric(synthdid_estimate(setup$Y, setup$N0, setup$T0))
  })

  list(
    estimate = mean(boot_estimates),
    se = sd(boot_estimates),
    ci = quantile(boot_estimates, c(0.025, 0.975))
  )
}
```

---

## Phase 4: Traditional DiD Conversion (02_did_analysis.R)

### 4.1 Two-Way Fixed Effects with fixest
```r
library(fixest)

# Stata: reghdfe outcome treat_post, absorb(county_fips year) cluster(county_fips)
# R equivalent:
did_model <- feols(
  net_migration_rate ~ treat_post | county_fips + year,
  data = panel_data,
  cluster = ~county_fips
)

# Event study specification
# Stata: reghdfe outcome i.year#treat, absorb(...)
event_study <- feols(
  net_migration_rate ~ i(year, treated, ref = 2020) | county_fips + year,
  data = panel_data,
  cluster = ~county_fips
)

# Plot event study coefficients
iplot(event_study, main = "Event Study: Migration Response to Tax")
```

### 4.2 Triple Difference
```r
# Stata: reghdfe outcome treat_post##high_income, absorb(county year county#high_income year#high_income)
triple_did <- feols(
  net_migration ~ treat_post * high_income |
    county_fips + year + county_fips^high_income + year^high_income,
  data = panel_data,
  cluster = ~county_fips
)
```

---

## Phase 5: Flow Analysis Conversion (02_flow_analysis.R)

### 5.1 County-Pair Flow Regressions
```r
# Origin-destination flow model
flow_model <- feols(
  log(migration_flow + 1) ~
    treat_dest * post +           # Destination treatment effect
    treat_origin * post +         # Origin treatment effect
    log(distance) +               # Gravity model control
    log(origin_pop) + log(dest_pop) |
    origin_fips + dest_fips + year,
  data = flow_data,
  cluster = ~origin_fips + dest_fips
)
```

### 5.2 Destination-Specific Analysis
```r
# Where do Multnomah emigrants go?
destination_analysis <- flow_data %>%
  filter(origin_fips == "41051") %>%  # From Multnomah
  group_by(dest_fips, dest_name, post) %>%
  summarize(
    total_migrants = sum(n1),
    avg_agi = weighted.mean(agi / n1, n1)
  ) %>%
  pivot_wider(
    names_from = post,
    values_from = c(total_migrants, avg_agi),
    names_prefix = c("pre_", "post_")
  ) %>%
  mutate(
    change = total_migrants_post - total_migrants_pre,
    pct_change = change / total_migrants_pre * 100
  )
```

---

## Phase 6: Visualization Conversion

### 6.1 SDID Diagnostic Plots
```r
# synthdid provides built-in plotting
plot(sdid_est, se.method = "placebo")

# Custom ggplot version for publication
sdid_plot <- ggplot() +
  # Treated unit
  geom_line(data = filter(plot_data, treated == 1),
            aes(x = year, y = outcome), color = "steelblue", size = 1.2) +
  # Synthetic control
  geom_line(data = synthetic_control,
            aes(x = year, y = fitted), color = "firebrick",
            linetype = "dashed", size = 1.2) +
  # Treatment timing
  geom_vline(xintercept = 2020.5, linetype = "dotted") +
  labs(
    title = "Synthetic Difference-in-Differences",
    subtitle = "Multnomah County vs. Synthetic Control",
    x = "Year", y = "Net Migration Rate"
  ) +
  theme_minimal()
```

### 6.2 Event Study Plots
```r
# Using fixest's iplot or custom ggplot
coef_data <- broom::tidy(event_study, conf.int = TRUE) %>%
  filter(str_detect(term, "year::")) %>%
  mutate(year = parse_number(term))

event_plot <- ggplot(coef_data, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 2020.5, linetype = "dotted", color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  labs(x = "Year", y = "Coefficient (pp)") +
  theme_minimal()
```

### 6.3 Maps
```r
library(sf)
library(tigris)

# Get Oregon county boundaries
or_counties <- counties(state = "OR", cb = TRUE, year = 2020) %>%
  left_join(migration_summary, by = c("GEOID" = "county_fips"))

migration_map <- ggplot(or_counties) +
  geom_sf(aes(fill = net_migration_rate), color = "white", size = 0.2) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue",
    midpoint = 0, name = "Net Migration\nRate (%)"
  ) +
  theme_void()
```

---

## Phase 7: Table Output Conversion

### 7.1 Regression Tables with modelsummary
```r
library(modelsummary)

# Stata esttab equivalent
models <- list(
  "SDID" = sdid_model,
  "TWFE" = did_model,
  "Event Study" = event_study
)

modelsummary(
  models,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  coef_map = c(
    "treat_post" = "Treatment Effect",
    "year::2021:treated" = "2021",
    "year::2022:treated" = "2022"
  ),
  gof_map = c("nobs", "r.squared", "FE: county_fips", "FE: year"),
  output = "results/tables/main_results.tex"
)
```

### 7.2 Summary Statistics Tables
```r
library(gt)

summary_table <- panel_data %>%
  group_by(treated) %>%
  summarize(
    across(
      c(net_migration_rate, population, median_income, unemployment),
      list(mean = mean, sd = sd),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  gt() %>%
  tab_header(title = "Summary Statistics by Treatment Status") %>%
  fmt_number(columns = everything(), decimals = 2)
```

---

## Phase 8: Conversion Order and Dependencies

### Recommended Conversion Sequence:

| Order | File | Dependencies | Complexity | Est. Lines |
|-------|------|--------------|------------|------------|
| 1 | `utils/globals.R` | None | Low | 50 |
| 2 | `utils/helpers.R` | globals.R | Low | 100 |
| 3 | `01_clean_data.R` | helpers | High | 800 |
| 4 | `02_descriptives.R` | clean_data | Medium | 200 |
| 5 | `02_sdid_analysis.R` | clean_data | High | 400 |
| 6 | `02_did_analysis.R` | clean_data | Medium | 300 |
| 7 | `02_flow_analysis.R` | clean_data | Medium | 350 |
| 8 | `02_indiv_analysis.R` | clean_data | Medium | 250 |
| 9 | `00_master.R` | All above | Low | 100 |

---

## Phase 9: Testing and Validation

### 9.1 Numerical Validation
```r
# Compare key results between Stata and R
validation <- tibble(
  statistic = c("SDID estimate", "SDID SE", "N observations",
                "Pre-treatment mean", "Control mean"),
  stata_value = c(...),  # From original output
  r_value = c(...),      # From R replication
  difference = abs(stata_value - r_value),
  tolerance = c(0.001, 0.001, 0, 0.001, 0.001)
) %>%
  mutate(passed = difference <= tolerance)
```

### 9.2 Visual Comparison
- Generate same figures in both Stata and R
- Overlay or side-by-side comparison
- Document any intentional differences

---

## Phase 10: Documentation and Reproducibility

### 10.1 Package Management with renv
```r
# Initialize renv for reproducibility
renv::init()
renv::snapshot()

# Document R version requirements
# R >= 4.1.0 required for native pipe |>
```

### 10.2 README Updates
- Installation instructions
- Data source documentation
- Execution order
- Expected outputs
- Troubleshooting guide

---

## Key Stata-to-R Translations Reference

| Stata Command | R Equivalent |
|---------------|--------------|
| `use "file.dta"` | `haven::read_dta("file.dta")` |
| `save "file.dta"` | `haven::write_dta(df, "file.dta")` |
| `import delimited` | `readr::read_csv()` |
| `merge 1:1` | `dplyr::left_join()` |
| `reshape wide/long` | `tidyr::pivot_wider/longer()` |
| `collapse (mean)` | `dplyr::group_by() %>% summarize()` |
| `egen` | `dplyr::mutate()` with grouped operations |
| `reghdfe` | `fixest::feols()` |
| `bootstrap` | `boot::boot()` or manual |
| `esttab` | `modelsummary::modelsummary()` |
| `graph twoway` | `ggplot2::ggplot()` |
| `local/global` | R variables or `options()` |
| `forvalues/foreach` | `purrr::map()` or `for` loops |
| `preserve/restore` | Work with copies or use functions |

---

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| SDID numerical differences | Use same algorithm (Arkhangelsky), validate against Stata |
| SE calculation differences | Document methodology, compare multiple SE estimators |
| Data type handling | Explicit type conversion, preserve Stata labels where needed |
| Missing value behavior | R uses `NA`, Stata uses `.` - explicit handling required |
| Large dataset performance | Use `data.table` for 1M+ row operations |

---

## Deliverables

1. **Complete R codebase** mirroring all Stata functionality
2. **Validation report** comparing Stata vs R results
3. **Updated README** with R-specific instructions
4. **renv.lock** for reproducible package versions
5. **All original outputs** (figures, tables) reproduced in R

---

## Implementation Notes

### To Start Conversion:
1. Create the `R/utils/` directory structure
2. Begin with `globals.R` to establish paths and parameters
3. Convert `01_clean_data.R` first - all analysis depends on it
4. Test each file independently before proceeding
5. Run Stata and R in parallel during validation phase

### Key Decisions to Make:
- Whether to use `data.table` vs pure tidyverse for large operations
- SDID SE method: jackknife vs bootstrap vs placebo
- Output format preferences: .tex, .html, .docx for tables
- Whether to maintain .dta intermediate files for Stata compatibility
