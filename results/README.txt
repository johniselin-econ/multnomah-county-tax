================================================================================
RESULTS FOLDER STRUCTURE AND NAMING CONVENTIONS
Multnomah County Tax Migration Analysis
Last Updated: January 2025
================================================================================

This folder contains all outputs from the analysis scripts. Results are
organized into subfolders by analysis type.

================================================================================
FOLDER STRUCTURE
================================================================================

results/
    sdid/                       Synthetic Difference-in-Differences analysis
        irs_full_16_22/         IRS data, full sample, 2016-2022
        irs_389_16_22/          IRS data, ACS-matched counties, 2016-2022
        acs_16_22_all/          ACS data, all adults 18+, 2016-2022
        acs_16_22_col/          ACS data, college-educated, 2016-2022
        acs_16_24_all/          ACS data, all adults 18+, 2016-2024
        acs_16_24_col/          ACS data, college-educated, 2016-2024
        (aggregate files)       sdid_weights.*, sdid_results.*, fig_kmeans.jpg
    did/                        Difference-in-Differences analysis (ACS individual-level)
    flows/                      Flow-based analysis (IRS county-level)
    individual/                 Individual-level categorical analysis
    old/                        Archived/deprecated results

================================================================================
SUBFOLDER CONTENTS AND NAMING CONVENTIONS
================================================================================

------------------------------------------------------------------------------
sdid/ - Synthetic Difference-in-Differences (02_sdid_analysis.do)
------------------------------------------------------------------------------

Results are organized into subfolders by data source/period:
    irs_full_16_22/     IRS full sample (2016-2022)
    irs_389_16_22/      IRS ACS-matched counties (2016-2022)
    acs_16_22_all/      ACS all adults 18+ (2016-2022)
    acs_16_22_col/      ACS college-educated (2016-2022)
    acs_16_24_all/      ACS all adults 18+ (2016-2024)
    acs_16_24_col/      ACS college-educated (2016-2024)

Aggregate files remain in the main sdid/ folder:
    sdid_weights.dta/xlsx, sdid_results.dta/xlsx, fig_kmeans.jpg

Figures follow this naming pattern:
    fig_{data}_{outcome}_{covars}_{sample}_{suffix}.{ext}

Components:
    {data}:     Data source and period
                - irs_full_16_22    IRS data, full sample, 2016-2022
                - irs_389_16_22     IRS data, ACS-matched counties, 2016-2022
                - acs_16_22_all     ACS data, all adults 18+, 2016-2022
                - acs_16_22_col     ACS data, college-educated, 2016-2022
                - acs_16_24_all     ACS data, all adults 18+, 2016-2024

    {outcome}:  Outcome variable
                - n1_net_rate       Net migration rate (returns)
                - n1_in_rate        In-migration rate (returns)
                - n1_out_rate       Out-migration rate (returns)
                - n2_net_rate       Net migration rate (exemptions)
                - n2_in_rate        In-migration rate (exemptions)
                - n2_out_rate       Out-migration rate (exemptions)
                - agi_net_rate      Net migration rate (AGI)
                - agi_in_rate       In-migration rate (AGI)
                - agi_out_rate      Out-migration rate (AGI)

    {covars}:   Covariate specification
                - 0                 No covariates
                - 1                 With covariates (population, per capita income)

    {sample}:   Sample restriction
                - sample_all            All counties (excl. AK, CA, HI, OR, WA)
                - sample_urban95        Top 5% urban counties
                - sample_urban95_covid  Top 5% urban + COVID k-means match

    {suffix}:   Output type
                - trends{year}      SDID trends plot (treatment year)
                - weights{year}     SDID weights plot (treatment year)
                - eventstudy        Event study coefficients
                - excl2020_*        Excludes 2020 from estimation

File types:
    .pdf        Vector graphics (trends, weights)
    .jpg        Raster graphics (event studies)

Tables:
    tab_sdid_{data}_{migration}_{sample}.tex

    {migration}: net, in, out

Data files:
    sdid_weights.dta    Synthetic control weights (Stata format)
    sdid_weights.xlsx   Synthetic control weights (Excel format)
    sdid_results.dta    Treatment effects, SEs, p-values (Stata format)
    sdid_results.xlsx   Treatment effects, SEs, p-values (Excel format)

    sdid_results contains the following variables:
        sample_data     Data source and period (e.g., "irs_full_16_22")
        sample          Sample restriction (e.g., "sample_all")
        outcome         Outcome variable (e.g., "n1_net_rate_irs")
        controls        Covariate specification (0=none, 1=with covariates)
        exclusion       Exclusion of 2020 (0=included, 1=excluded)
        tau             SDID treatment effect estimate (ATT)
        se              Standard error (placebo VCE)
        pval            Two-sided p-value
        ci_lower        95% confidence interval lower bound
        ci_upper        95% confidence interval upper bound
        n_counties      Number of control counties in estimation
        pre_mean        Pre-treatment mean for Multnomah County

Other:
    fig_kmeans.jpg      K-means clustering results for COVID matching

Specification Curve Figures:
    fig_speccurve_{outcome_type}_{migration}.pdf

    Specification curve plots showing treatment effects across all specifications
    for a given outcome type (n1, n2, agi) and migration direction (net, in, out).
    These figures display:
        - Upper panel: Point estimates with 95% CIs, sorted by effect size
        - Lower panel: Specification indicators (data, sample, controls, exclusion)

------------------------------------------------------------------------------
did/ - Difference-in-Differences (02_did_analysis.do)
------------------------------------------------------------------------------

Figures:
    fig_did_coefplot.png        DiD coefficient plot (out vs in migration)
    fig_es_out_migration.png    Event study: out-migration from Multnomah
    fig_es_in_migration.png     Event study: in-migration to Multnomah
    fig_es_combined.png         Combined event study (both directions)

Analysis compares college-educated vs. non-college-educated individuals,
using 2020 as the treatment year. Based on ACS individual-level data.

------------------------------------------------------------------------------
flows/ - Flow-Based Analysis (02_flow_analysis.do)
------------------------------------------------------------------------------

Figures follow this pattern:
    fig_multnomah_{type}_{outcome}.png

    {type}:     Analysis type
                - post          DiD coefficient (Multnomah x Post-2020)
                - post_any      DiD coefficient (any interaction)
                - out           Event study: out-migration coefficients
                - in            Event study: in-migration coefficients
                - both          Combined event study (both directions)
                - flows         Raw flow visualization

    {outcome}:  Outcome variable
                - n1            Number of returns
                - n2            Number of exemptions
                - agi           Adjusted gross income

Based on IRS county-to-county migration flow data (2016-2022).

------------------------------------------------------------------------------
individual/ - Individual-Level Categorical Analysis (02_indiv_analysis.do)
------------------------------------------------------------------------------

Figures follow this pattern:
    fig_{category}_{sample}.pdf

    {category}: Demographic/economic category
                - cat_age           Age categories (18-24, 25-44, 45-64, 65+)
                - cat_sex           Sex (Male, Female)
                - cat_married       Marital status
                - cat_child         Number of children
                - cat_yngch         Age of youngest child
                - cat_educ          Education level
                - cat_ftotinc       Family income categories

    {sample}:   Migration direction
                - 1                 Out-migration sample (Multnomah residents)
                - 2                 In-migration sample (CA/OR/WA residents)

Shows adjusted migration rates by category over time (2016-2024).

------------------------------------------------------------------------------
old/ - Archived Results
------------------------------------------------------------------------------

Contains deprecated results from earlier analysis iterations.
Not actively maintained; retained for reference only.

================================================================================
GENERATING SCRIPTS
================================================================================

Script                      Output Folder
-------------------------   -------------
02_sdid_analysis.do         sdid/
02_did_analysis.do          did/
02_flow_analysis.do         flows/
02_indiv_analysis.do        individual/
02_descriptives.do          (exports to data/multnomah.xlsx)

All scripts are called by the master script: 00_multnomah.do

================================================================================
NOTES
================================================================================

1. Vector formats (.pdf) are preferred for publication-quality figures
2. Raster formats (.jpg, .png) are used for quick viewing/iteration
3. LaTeX tables (.tex) can be included directly in papers
4. The "excl2020" suffix indicates 2020 was excluded from estimation
   (useful for robustness checks given COVID-19 confounding)

================================================================================
