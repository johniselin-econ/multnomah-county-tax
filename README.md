# Multnomah County Tax Migration Analysis

Studying the effect of the Multnomah County income tax (enacted 2020, effective 2021) on domestic migration patterns by education and income level.

## Overview

This project analyzes whether the Multnomah County tax policy change affected migration of high-income and college-educated individuals. The analysis combines:

- **Synthetic Difference-in-Differences (SDID)** as the primary methodology
- **Difference-in-Differences (DiD)** with event studies on ACS microdata
- **Flow-based analysis** using IRS county-to-county migration data
- **Specification curve analysis** for robustness across data sources, samples, and covariates

The primary codebase is **Stata**, with R helper scripts for the IPUMS API data download and choropleth mapping.

## Project Structure

```
multnomah-county-tax/
├── code/                          # Stata do-files
│   ├── 00_multnomah.do            # Orchestrator (runs full pipeline)
│   ├── 01_clean_data.do           # Data cleaning and preparation
│   ├── 02_sdid_analysis.do        # Synthetic DiD estimation
│   ├── 02_did_analysis.do         # DiD + event study (ACS microdata)
│   ├── 02_flow_analysis.do        # Flow-based analysis (IRS county flows)
│   ├── 02_indiv_analysis.do       # Individual-level categorical analysis
│   ├── 02_multi_analysis.do       # Multi-specification analysis
│   ├── 02_descriptives.do         # Descriptive statistics
│   ├── 02_revenue.do              # Revenue analysis
│   ├── R/                         # R helpers called from Stata via rcall
│   │   ├── api_code.R             # IPUMS API data download
│   │   └── map_code.R             # Choropleth maps
│   ├── old/                       # Archived do-files
│   └── logs/                      # Stata log files (by run date)
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
│   ├── did/                       # DiD regression outputs
│   ├── flows/                     # Flow analysis outputs
│   ├── individual/                # Individual-level analysis
│   ├── maps/                      # Choropleth maps
│   ├── revenue/                   # Revenue analysis
│   └── tables/                    # Summary tables (LaTeX)
│
├── drafts/                        # Paper drafts (Word, PDF)
├── api_codes.txt                  # API keys (gitignored)
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

## API Setup

The Stata pipeline calls R via [`rcall`](https://github.com/haghish/rcall) to download ACS microdata from [IPUMS USA](https://usa.ipums.org/usa/). This requires an IPUMS API key.

1. **Register for an IPUMS account** at <https://uma.pop.umn.edu/usa/user/new> and request an API key from your account settings.

2. **Create `api_codes.txt`** in the project root (this file is gitignored). The file should be a comma-delimited table with the following format:

   ```
   name, code
   "ipums", "YOUR_IPUMS_API_KEY_HERE"
   "bls", ""
   "dol", ""
   ```

   Only the IPUMS key is required. The BLS and DOL rows can be left blank.

3. **Install the `rcall` Stata package** so that Stata can invoke R scripts:

   ```stata
   net install github, from("https://haghish.github.io/github/")
   github install haghish/rcall, stable
   ```

4. **Install the `ipumsr` R package**, which `rcall` will use:

   ```r
   install.packages("ipumsr")
   ```

The orchestrator (`code/00_multnomah.do`) reads `api_codes.txt`, passes the key to R, and downloads year-by-year ACS extracts into `data/acs/`. Existing extracts are skipped unless `overwrite_csv` is set to 1.

## Usage

```stata
* Set working directory and run full pipeline
do "code/00_multnomah.do"
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
ssc install geodist, replace
ssc install ipfraking, replace
net install parallel, from(https://raw.github.com/gvegayon/parallel/stable/) replace
```

See `STATA_REQUIREMENTS.txt` for verification instructions.

### R Packages (used by Stata via rcall)

```r
install.packages(c("ipumsr", "dplyr", "stringr", "sf", "tigris", "ggplot2"))
```

## Author

John Iselin
john.iselin@yale.edu
