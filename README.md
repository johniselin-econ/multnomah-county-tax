# Mult-No-More? The Migration Effects of Multnomah County's Preschool For All Income Tax

**Karen Conway, John Iselin, and Jon Rork**

Replication code for Conway, Iselin, and Rork (2026). Studies the effect of the Multnomah County income tax (enacted 2020, effective 2021) on domestic migration patterns by education and income level.

## Overview

This project analyzes whether the Multnomah County tax policy change affected migration of high-income and college-educated individuals. The analysis combines:

- **Synthetic Difference-in-Differences (SDID)** as the primary methodology
- **PPML flow models** on IRS county-to-county migration data with placebo tests
- **Difference-in-Differences (DiD)** with event studies on ACS microdata
- **Specification curve analysis** for robustness across data sources, samples, and covariates
- **Revenue microsimulation** using TAXSIM to estimate fiscal impacts of tax-induced migration

The primary codebase is **Stata**, with R helper scripts for the IPUMS API data download and choropleth mapping.

## Project Structure

```
multnomah-county-tax/
├── 00_multnomah.do                # Orchestrator (runs full pipeline)
├── code/                          # Stata do-files
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
│   └── old/                       # Archived do-files
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
├── api_codes.txt                  # API keys (gitignored — see setup below)
├── profile.do                     # Local user overrides (gitignored)
├── LICENSE
├── CITATION.cff
├── STATA_REQUIREMENTS.txt         # Package installation guide
├── .gitignore
└── README.md
```

## Data Sources and Licensing

| Source | Description | Period | Access |
|--------|-------------|--------|--------|
| **ACS (IPUMS)** | Individual-level migration microdata | 2015-2024 | Registration required |
| **NHGIS (IPUMS)** | 2020 Census county demographics | 2020 | Registration required |
| **IRS SOI** | County-level migration flows (returns, exemptions, AGI) | 2015-2022 | Public domain |
| **BEA CAINC1** | County personal income | varies | Public domain |
| **BLS LAUS** | County employment data | varies | Public domain |
| **DOL** | County-level childcare costs | 2022 | Public domain |
| **NYTimes** | COVID-19 cases/deaths by county | 2020-2022 | [License](https://github.com/nytimes/covid-19-data/blob/master/LICENSE) |

**IPUMS citation requirement:** Use of IPUMS data (ACS and NHGIS) requires [registration](https://uma.pop.umn.edu/usa/user/new) and proper citation. See [IPUMS citation guidelines](https://www.ipums.org/about/citation). ACS microdata are downloaded via the IPUMS API and are not redistributed in this repository.

**IRS SOI data:** County-to-county migration files are available from the [IRS Statistics of Income](https://www.irs.gov/statistics/soi-tax-stats-migration-data) program. These are U.S. government public domain data.

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

### PPML Flow Models

Poisson pseudo-maximum likelihood (PPML) gravity models on IRS county-to-county AGI flows, with event study coefficients and placebo distributions from randomized treatment assignment.

### Specification Curve Analysis

Robustness is assessed across all combinations of data source, sample, outcome, and covariate specification, visualized in specification curve plots.

### Revenue Microsimulation

A TAXSIM-based microsimulation calibrated to 2019 ACS microdata and IRS administrative totals estimates PFA revenue under counterfactual no-migration scenarios. Quantifies fiscal cost of tax-induced out-migration for both the county (PFA) and the state (Oregon income tax).

## Setup

### 1. Stata Packages

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

### 2. R Packages (used by Stata via rcall)

Install [`rcall`](https://github.com/haghish/rcall) so Stata can invoke R scripts:

```stata
net install github, from("https://haghish.github.io/github/")
github install haghish/rcall, stable
```

Then install the required R packages:

```r
install.packages(c("ipumsr", "dplyr", "stringr", "sf", "tigris", "ggplot2"))
```

### 3. IPUMS API Key

The pipeline downloads ACS microdata from [IPUMS USA](https://usa.ipums.org/usa/) via the API.

1. **Register** at <https://uma.pop.umn.edu/usa/user/new> and request an API key from your account settings.

2. **Create `api_codes.txt`** in the project root (this file is gitignored):

   ```
   name, code
   "ipums", "YOUR_IPUMS_API_KEY_HERE"
   "bls", ""
   "dol", ""
   ```

   Only the IPUMS key is required.

### 4. Local Configuration (optional)

To sync outputs to an Overleaf folder or override other defaults, create `profile.do` in the project root (this file is gitignored):

```stata
** profile.do — Local user overrides
global oth_path "C:/Users/yourname/Dropbox/Apps/Overleaf/Your Project/"
```

## Usage

```stata
* Set working directory to project root, then run the full pipeline
cd "path/to/multnomah-county-tax"
do "00_multnomah.do"
```

The orchestrator downloads ACS data via the IPUMS API (skipping years already on disk), cleans all data sources, and runs the full analysis pipeline.

## Citation

If you use this code, please cite the associated working paper:

> Conway, K., Iselin, J., & Rork, J. (2026). Mult-No-More? The Migration Effects of Multnomah County's Preschool For All Income Tax. Working Paper.

See `CITATION.cff` for machine-readable citation metadata.

## License

This project is licensed under the MIT License. See [LICENSE](LICENSE) for details.

## Author

John Iselin
john.iselin@yale.edu
