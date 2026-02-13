# Multnomah County Tax Migration Analysis

Studying the effect of the Multnomah County income tax (enacted 2020, effective 2021) on domestic migration patterns by education and income level.

## Overview

This project analyzes whether the Multnomah County tax policy change affected migration of high-income and college-educated individuals. The analysis combines:

- **Synthetic Difference-in-Differences (SDID)** as the primary methodology
- **Difference-in-Differences (DiD)** with event studies on ACS microdata
- **Flow-based analysis** using IRS county-to-county migration data
- **Specification curve analysis** for robustness across data sources, samples, and covariates

The codebase has parallel **Stata** and **R** implementations that produce equivalent results.

## Project Structure

```
multnomah-county-tax/
├── code/                          # Stata do-files
│   ├── 00_multnomah.do            # Orchestrator (runs full pipeline)
│   ├── 00_ipums_api.R             # IPUMS data download via API
│   ├── 01_clean_data.do           # Data cleaning and preparation
│   ├── 02_sdid_analysis.do        # Synthetic DiD estimation
│   ├── 02_did_analysis.do         # DiD + event study (ACS microdata)
│   ├── 02_flow_analysis.do        # Flow-based analysis (IRS county flows)
│   ├── 02_indiv_analysis.do       # Individual-level categorical analysis
│   ├── 02_multi_analysis.do       # Multi-specification analysis
│   ├── 02_descriptives.do         # Descriptive statistics
│   ├── 02_revenue.do              # Revenue analysis
│   ├── R/                         # Legacy R helpers (API, mapping)
│   ├── old/                       # Archived do-files
│   └── logs/                      # Stata log files (by run date)
│
├── R/                             # R scripts (mirrors Stata pipeline)
│   ├── 00_multnomah.R             # Orchestrator (runs full pipeline)
│   ├── 01_clean_data.R            # Data cleaning
│   ├── 02_sdid_analysis.R         # SDID estimation (county-level)
│   ├── 02_sdid_analysis_state.R   # SDID estimation (state-level)
│   ├── 02_did_analysis.R          # DiD + event study
│   ├── 02_flow_analysis.R         # Flow-based analysis
│   ├── 02_indiv_analysis.R        # Individual-level analysis
│   ├── 02_descriptives.R          # Descriptive statistics
│   ├── 02_maps.R                  # Choropleth maps
│   ├── 02_revenue.R               # Revenue analysis
│   ├── 03_paper_output.R          # Paper-ready tables and figures
│   └── utils/                     # Shared utilities
│       ├── globals.R              # Constants, paths, color palettes
│       ├── helpers.R              # Helper functions
│       └── fig_diagrams.R         # Diagram/figure utilities
│
├── data/
│   ├── acs/                       # ACS microdata (IPUMS extracts, gitignored)
│   ├── irs/                       # IRS SOI migration data (county flows + AGI)
│   ├── covid/                     # NYTimes COVID-19 county data
│   ├── demographic/               # Census/BEA/BLS demographics
│   │   ├── CAINC1/                # BEA county personal income
│   │   ├── bls/                   # BLS employment data
│   │   ├── dol/                   # Dept of Labor data
│   │   └── nhgis0031_csv/         # 2020 Census (NHGIS)
│   ├── mapping/                   # Shapefiles (Metro District Boundary)
│   ├── working/                   # Processed datasets (gitignored)
│   ├── multnomah.xlsx             # Multnomah County reference data
│   └── taxsim_state_codes.csv     # TAXSIM state code crosswalk
│
├── results/
│   ├── sdid/                      # SDID outputs
│   │   ├── sdid_results.*         # Treatment effects (csv/dta/rds/xlsx)
│   │   ├── sdid_weights.*         # Synthetic control weights
│   │   ├── sdid_*_state.*         # State-level results and weights
│   │   ├── fig_speccurve_*.pdf    # Specification curve plots
│   │   ├── fig_kmeans.jpg         # K-means clustering figure
│   │   ├── irs_full_16_22/        # IRS full-sample SDID (gitignored)
│   │   ├── irs_389_16_22/         # IRS top-389 counties (gitignored)
│   │   ├── irs5_*/                # IRS type 5 interstate (gitignored)
│   │   ├── acs_*/                 # ACS-based SDID (gitignored)
│   │   └── *_outstate_*/          # Out-of-state samples (gitignored)
│   ├── did/                       # DiD regression outputs
│   │   ├── fig_es_*.png           # Event study coefficient plots
│   │   └── tab_did_*.tex          # LaTeX regression tables
│   ├── flows/                     # Flow analysis outputs
│   │   ├── fig_hist_*.png         # Flow histograms
│   │   └── fig_multnomah_*.png    # Multnomah-specific flow plots
│   ├── individual/                # Individual-level analysis
│   │   └── fig_cat_*.pdf          # Categorical breakdowns
│   ├── maps/                      # Choropleth maps (US and West Coast)
│   │   └── map_*.png              # Migration rate maps by outcome
│   ├── revenue/                   # Revenue analysis
│   │   ├── fig_pfa_*.png          # PFA revenue figures
│   │   └── tbl_*.xlsx             # Revenue tables
│   └── tables/                    # Summary tables
│       └── table*.tex             # LaTeX tables for paper
│
├── drafts/                        # Paper drafts (Word, PDF)
├── .gitignore
├── STATA_REQUIREMENTS.txt         # Package installation guide
└── README.md
```

## Data Sources

| Source | Description | Period |
|--------|-------------|--------|
| **IRS SOI** | County-level migration flows (returns, exemptions, AGI) | 2015-2022 |
| **ACS (IPUMS)** | Individual-level migration microdata | 2015-2024 |
| **NHGIS** | 2020 Census county demographics | 2020 |
| **BEA CAINC1** | County personal income | varies |
| **BLS** | County employment data | varies |
| **NYTimes** | COVID-19 cases/deaths by county | 2020-2022 |

## Methodology

### Synthetic Difference-in-Differences (SDID)

The primary analysis uses SDID to estimate the causal effect on migration:

- **Treatment unit**: Multnomah County, Oregon
- **Treatment period**: Post-2020
- **Outcomes**: Migration rates (in, out, net) for returns (n1), exemptions (n2), and AGI
- **Donor pools**: All counties, urban top 389, COVID-matched, out-of-state
- **Data sources**: IRS county flows (2016-2022), ACS microdata (2016-2024)

### Difference-in-Differences (DiD)

Individual-level DiD using ACS microdata:

- **Treatment**: College-educated x post-2020
- **Samples**: Multnomah residents (out-migration), West Coast (in-migration), Lower 48 (in-migration)
- **Age groups**: 25-44, 45-64, 65+
- **Fixed effects**: Year, origin county, demographic categories
- **Year 2020 excluded** from all regressions (COVID robustness)

### Specification Curve Analysis

Robustness is assessed across all combinations of data source, sample, outcome, and covariate specification, visualized in specification curve plots.

## Usage

### Stata

```stata
* Set working directory and run full pipeline
do "code/00_multnomah.do"
```

### R

```r
# Run full pipeline from project root
source("R/00_multnomah.R")
```

## Requirements

### Stata Packages

Install all required packages:

```stata
ssc install sdid, replace
ssc install sdid_event, replace
ssc install reghdfe, replace
ssc install ftools, replace
ssc install ppmlhdfe, replace
ssc install estout, replace
ssc install coefplot, replace
ssc install blindschemes, replace
net install parallel, from(https://raw.github.com/gvegayon/parallel/stable/) replace
```

See `STATA_REQUIREMENTS.txt` for verification instructions.

### R Packages

```r
install.packages(c(
  "dplyr", "tidyr", "readr", "stringr", "purrr", "ggplot2", "patchwork",
  "fixest", "synthdid", "openxlsx", "marginaleffects", "modelsummary",
  "scales", "ipumsr", "sf", "tigris"
))
```

## Author

John Iselin
john.iselin@yale.edu
