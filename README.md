# Multnomah County Tax Migration Analysis

Studying the effect of high-income tax changes on migration in Multnomah County, Oregon.

## Overview

This project analyzes whether the 2020 Multnomah County tax policy change affected domestic migration patterns of high-income individuals. The analysis uses Synthetic Difference-in-Differences (SDID) as the primary methodology, with supplementary DiD and flow-based analyses.

## Project Structure

```
multnomah-county-tax/
├── code/
│   ├── 00_multnomah.do         # Master script (runs all analyses)
│   ├── 01_clean_data.do        # Data cleaning and preparation
│   ├── 02_sdid_analysis.do     # Synthetic DiD estimation
│   ├── 02_did_analysis.do      # DiD analysis (ACS individual-level)
│   ├── 02_flow_analysis.do     # Flow-based analysis (IRS county flows)
│   ├── 02_indiv_analysis.do    # Individual-level categorical analysis
│   ├── 02_descriptives.do      # Descriptive statistics
│   └── R/                      # R scripts for API calls and mapping
├── data/
│   ├── acs/                    # American Community Survey (IPUMS)
│   ├── irs/                    # IRS SOI migration data
│   ├── demographic/            # Census demographics (NHGIS)
│   ├── covid/                  # COVID-19 data
│   └── working/                # Processed data
└── results/
    ├── sdid/                   # SDID analysis outputs
    ├── did/                    # DiD analysis outputs
    ├── flows/                  # Flow analysis outputs
    └── individual/             # Individual-level outputs
```

## Data Sources

- **IRS SOI Migration Data**: County-level migration flows (2016-2022)
- **American Community Survey (ACS)**: Individual-level migration via IPUMS (2016-2024)
- **Census Demographics**: 2020 NHGIS data
- **BEA Regional Accounts**: County-level economic data
- **COVID-19 Data**: NYTimes cases/deaths by county

## Methodology

### Primary Analysis: Synthetic Difference-in-Differences (SDID)

The main analysis uses SDID to estimate the causal effect of Multnomah County's 2020 tax policy on migration. Key features:

- **Treatment unit**: Multnomah County, Oregon
- **Treatment period**: Post-2020
- **Outcomes**: Migration rates (in, out, net) for returns, exemptions, and AGI
- **Sample restrictions**: All counties, urban top 5%, COVID-matched

### Specification Curve Analysis

Treatment effects are preserved across all specifications and visualized using specification curve plots, showing robustness across:
- Data sources (IRS, ACS by education level)
- Sample definitions
- Covariate specifications
- Exclusion of 2020 (COVID robustness)

## Key Outputs

### SDID Results (`results/sdid/`)

- `sdid_results.dta/xlsx`: Treatment effects, SEs, p-values for all specifications
- `sdid_weights.dta/xlsx`: Synthetic control weights for all specifications
- `fig_speccurve_*.pdf`: Specification curve plots
- `fig_*_eventstudy.jpg`: Event study coefficient plots
- `tab_sdid_*.tex`: LaTeX tables for publication

See `results/README.txt` for detailed naming conventions.

## Usage

```stata
* Open Stata and run master script
do "code/00_multnomah.do"
```

## Requirements

### Stata Packages
- `sdid` - Synthetic Difference-in-Differences
- `sdid_event` - Event study post-estimation
- `reghdfe`, `ftools` - Fixed effects estimation
- `coefplot` - Coefficient plotting
- `estout` - Table export

### R Packages
- `ipumsr` - IPUMS data integration
- `sf`, `tigris` - Geographic data

## Author

John Iselin
john.iselin@yale.edu

## Changelog

### January 2025
- Added treatment effects preservation (`sdid_results.dta/xlsx`)
- Added specification curve plotting
- Updated documentation
