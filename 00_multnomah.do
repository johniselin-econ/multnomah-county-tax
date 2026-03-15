/*******************************************************************************
	File Name:    00_multnomah.do
	Creator:      John Iselin
	Date Updated: March 12, 2026

	Purpose:      Orchestrator for the Stata analysis pipeline examining the
	              effect of Multnomah County's Preschool for All tax on
	              migration. Calls data-cleaning scripts (01_*) and analysis
	              scripts (02_*) in dependency order.

	Prerequisite: Run 00_ipums_api.R first to download ACS, QWI, QCEW, and
	              Census age-share data and generate R-based figures.

	Author:       John Iselin (john.iselin@yale.edu)
*******************************************************************************/


** ============================================================================
** REQUIRED PACKAGES
** ============================================================================
** Uncomment and run once to install:
*   ssc install reghdfe, replace
*   ssc install ftools, replace
*   ssc install ppmlhdfe, replace
*   ssc install sdid, replace
*   ssc install sdid_event, replace
*   ssc install estout, replace
*   ssc install coefplot, replace
*   ssc install fre, replace
*   ssc install distinct, replace
*   ssc install blindschemes, replace
*   net install taxsimlocal35, from("https://taxsim.nber.org/stata") replace
*   net install parallel, from(https://raw.github.com/gvegayon/parallel/stable/) replace


** ============================================================================
** PRELIMINARIES
** ============================================================================
capture log close
clear matrix
clear all
set more off

** Verify all required packages are installed
local pkg_missing = 0
foreach pkg in reghdfe ftools ppmlhdfe sdid sdid_event estout coefplot fre distinct taxsimlocal35 {
    capture which `pkg'
    if _rc {
        di as error "  Package not found: `pkg'"
        local pkg_missing = 1
    }
}
capture findfile scheme-plotplainblind.scheme
if _rc {
    di as error "  Package not found: blindschemes (scheme plotplainblind)"
    local pkg_missing = 1
}
** parallel is optional (controlled by use_parallel flag below)
capture which parallel
if _rc {
    di as txt "  Note: parallel not installed. Setting use_parallel = 0."
    global use_parallel = 0
}
if `pkg_missing' {
    di as error _n "ERROR: Required Stata packages are missing."
    di as error "See STATA_REQUIREMENTS.txt for install instructions."
    error 199
}


** ============================================================================
** PROJECT GLOBALS
** ============================================================================
global pr_name "multnomah"
global date "`: di %tdCY-N-D daily("$S_DATE", "DMY")'"

** Directories — set working directory to project root before running
global dir      = subinstr("`c(pwd)'", "\", "/", .)
global code     "${dir}/code/stata/"
global rcode    "${dir}/code/R/"
global data     "${dir}/data/"
global results  "${dir}/results/"
global logs     "${code}logs/"
cd "${dir}"

** Overleaf sync (optional) — set oth_path in profile.do (gitignored)
global overleaf = 0
global oth_path ""
global ol_fig   ""
global ol_tab   ""
capture do "${dir}/profile.do"
if "${oth_path}" != "" {
    global ol_fig "${oth_path}figures/"
    global ol_tab "${oth_path}tables/"
    global overleaf = 1
}

** Create output directories
foreach d in "" "tables" "figures" "sdid" "flows" "did" "individual" {
    capture mkdir "${results}`d'"
}
capture mkdir "${logs}"

** Start log
log using "${logs}00_log_${pr_name}_${date}", replace text

** Seed and scheme
set seed 56403
set scheme plotplainblind


** ============================================================================
** PARAMETERS
** ============================================================================

** Parallel processing (set to 0 for sequential)
if "${use_parallel}" == "" global use_parallel = 1
global n_clusters = 6

** Resume mode: skip specs with existing temp_results files (set to 0 for fresh run)
if "${resume}" == "" global resume = 1

** Event study mode: "all" = every spec, "preferred" = only preferred specs
** Set to "all" for full replication, "preferred" for fast runs (~99% fewer event studies)
global event_study_mode "preferred"

** Year ranges
global start_year_irs_data     = 2012   // Extended back for appendix (2011-12 flows)
global start_year_irs_analysis = 2016   // Main analysis start
global start_year_acs          = 2012   // Extended back for appendix comparison
global end_year_acs            = 2024

** IRS file year ranges (2-digit)
global start_yy_irs_download   = 11     // IRS file download start (2011-12 flows)
global end_yy_irs_migration    = 21     // IRS migration file end (2021-22 flows)
global end_yy_irs_agi          = 22     // IRS AGI file end (2022 data)
global start_yy_irs_county     = 12     // County data processing start
global end_yy_irs_county       = 22     // County data processing end


** ============================================================================
** STAGE 1: DATA CLEANING
** ============================================================================
** Calls 01a_programs through 01h_auxiliary; see 01_clean_data.do for details.
*do "${code}01_clean_data.do"


** ============================================================================
** STAGE 2: DESCRIPTIVE ANALYSIS
** ============================================================================
*do "${code}02_descriptives.do"


** ============================================================================
** STAGE 3: CAUSAL ANALYSIS
** ============================================================================

** IRS county-level flow regressions
*do "${code}02_flow_analysis.do"

** ACS individual-level difference-in-differences
*do "${code}02_did_analysis.do"

** Synthetic difference-in-differences (main specification)
** Produces sdid_results.dta used by downstream scripts
do "${code}02_sdid_analysis.do"


** ============================================================================
** STAGE 4: ROBUSTNESS
** ============================================================================

** SDID with narrow control pool (21 similar cities)
do "${code}02_narrow_sdid.do"

** Individual-level person-year event study
do "${code}02_indiv_analysis.do"


** ============================================================================
** STAGE 5: DERIVED ESTIMATES (depend on SDID results)
** ============================================================================

** Revenue effects of tax-induced migration
do "${code}02_revenue.do"

** Flow and stock elasticities (depends on 02_revenue + 02_sdid_analysis)
do "${code}02_elasticities.do"

** Observation count table
do "${code}02_diagnostics.do"


** ============================================================================
** STAGE 6: SUPPLEMENTAL ANALYSES & APPENDIX
** ============================================================================

** SDID on non-migration IRS outcomes (returns, AGI, wages, income)
do "${code}02_otherout_sdid.do"

** SDID on quarterly outcomes (QWI employment/earnings, QCEW estabs/wages)
do "${code}02_quarterly_sdid.do"

** Supplemental obs counts for otherout + quarterly (optional — uncomment to run)
* do "${code}02_diagnostics_supp.do"

** Appendix B: IRS data quality (extended 2012-2022 window)
do "${code}02_appendix_data_quality.do"


** ============================================================================
** CLOSE
** ============================================================================
capture log close

