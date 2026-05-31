/*******************************************************************************
	File Name:    00_multnomah.do
	Creator:      John Iselin
	Date Updated: May 10, 2026

	Purpose:      Orchestrator for the Stata analysis pipeline examining the
	              effect of Multnomah County's Preschool for All tax on
	              migration. Calls data-cleaning scripts (01_*) and analysis
	              scripts (02_*) in dependency order.

	Prerequisite: Run 00_download_data.R (or 00_multnomah.R) first to
	              download the R-managed inputs used below.

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
set linesize 120

** Load shared project defaults and helper programs
if "${dir}" == "" {
    local _cwd = subinstr("`c(pwd)'", "\", "/", .)
    if regexm("`_cwd'", "(.*)/code/(stata|utils)$") global dir = regexs(1)
    else global dir "`_cwd'"
}
do "${dir}/code/utils/globals.do"

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
** parallel is optional — handled by the use_parallel auto-downgrade block
** below the PROJECT GLOBALS panel.
if `pkg_missing' {
    di as error _n "ERROR: Required Stata packages are missing."
    di as error "See STATA_REQUIREMENTS.txt for install instructions."
    error 199
}


** ============================================================================
** PROJECT GLOBALS — RUN-CONTROL FLAGS
** ============================================================================

** ----------------------------------------------------------------
** Bootstrap
** ----------------------------------------------------------------
global run_bootstrap         = 1 		// 1 to (re)run bootstrap; 0 to skip the stage
global bootstrap_reps        = 500		// 20=smoke, 100=stress, 500=publication
global show_bootstrap_cis    = 1		// 1 to render rcap whiskers on spec curves (requires bootstrap_cis.dta)
global ci_level              = 95		// 90, 95, or 99 — percentile CI level for bootstrap_cis.dta

** ----------------------------------------------------------------
** Parallel execution
** ----------------------------------------------------------------
global use_parallel          = 1		// 1 to use Vega `parallel` ado; auto-downgrades to 0 if package missing
global n_clusters            = 4		// worker count; setup_parallel caps to floor(physical_cores / processors_max)
global resume                = 0		// 1 to skip bootstrap reps whose draw .dta already exists

** ----------------------------------------------------------------
** Output mode
** ----------------------------------------------------------------
global event_study_mode      "all"     // "all" | "main" | "none" — which event studies the SDID stage runs
global overleaf              = 1       // 1 to copy figures/tables to ${oth_path}; auto-set to 1 below if profile.do defines oth_path

** Auto-downgrade use_parallel if the `parallel` ado isn't installed.
** User intent of 1 means "use parallel if available", not "fail if missing".
capture which parallel
if _rc & ${use_parallel} == 1 {
    di as txt "  Note: `parallel` package not installed. Downgrading use_parallel 1 -> 0."
    global use_parallel = 0
}


** ============================================================================
** PROJECT PATHS & LOGGING
** ============================================================================
** Working directory, Overleaf sync, output directories, log file. Path
** defaults live in 00_stata_config.do; this section only overrides as needed.

** Set working directory to project root before running
cd "${dir}"

** Overleaf sync (optional) — set oth_path and overleaf=1 in profile.do (gitignored)
global ol_fig   ""
global ol_tab   ""
capture do "${dir}/profile.do"
if "${oth_path}" != "" {
    global ol_fig "${oth_path}figures/"
    global ol_tab "${oth_path}tables/"
    global overleaf = 1
}
else if ${overleaf} == 1 {
    ** Guard: overleaf=1 from the RUN-CONTROL FLAGS panel but profile.do
    ** didn't define ${oth_path}, so ${ol_fig} / ${ol_tab} are empty. Every
    ** downstream `graph export "${ol_fig}foo.png"` would silently land in
    ** the current working directory. Downgrade to 0 with a visible warning.
    dis as error "WARNING: overleaf=1 but oth_path unset (profile.do missing or " ///
        "doesn't define oth_path). Disabling overleaf sync for this run."
    global overleaf = 0
}

** Create output directories
foreach d in "" "tables" "figures" "sdid" "flows" "did" "individual" {
    capture mkdir "${results}`d'"
}
capture mkdir "${logs}"

** Start log
log using "${logs}00_log_${pr_name}_${date}", replace text

** Seed and scheme
project_set_seed, context("00_multnomah.do") offset(0)


** ============================================================================
** PARAMETERS
** ============================================================================

** Shared defaults live in 00_stata_config.do.
** Override specific globals here only for intentionally custom runs.
project_export_run_manifest


** ============================================================================
** STAGE 1: DATA CLEANING
** ============================================================================
** Calls 01a_programs through 01h_auxiliary; see 01_clean_data.do for details.
do "${code}01_clean_data.do"


** ============================================================================
** STAGE 2: CAUSAL ANALYSIS
** ============================================================================
** SDID stage produces sdid_analysis_data.dta and sdid_results.dta consumed
** by Stage 3 (descriptives Table 1) and Stage 4 (derived estimates).

** IRS county-level flow regressions
do "${code}02_flow_analysis.do"

** ACS individual-level difference-in-differences
do "${code}02_did_analysis.do"

** Synthetic difference-in-differences (main specification)
do "${code}02_sdid_analysis.do"


** ============================================================================
** STAGE 3: DESCRIPTIVE TABLES & CONDITIONAL MEANS
** ============================================================================
** Runs after SDID prep so Table 1's Multnomah-vs-donor-pool comparison can
** read sdid_analysis_data.dta. The earlier sections of 02_descriptives.do
** read raw cleaned inputs and could run in Stage 1, but keeping descriptives
** together avoids splitting the file.

** Combined descriptives: flow comparisons (consumed by R/map_code.R),
** Table 2 (Multnomah + neighbors), stringency KDPs, and Table 1 (combined).
do "${code}02_descriptives.do"

** ACS individual-level conditional-mean migration regressions
do "${code}02_indiv_analysis.do"


** ============================================================================
** STAGE 4: DERIVED ESTIMATES (depend on SDID results)
** ============================================================================

** Revenue microsim: produces revenue_parameters.dta (rates + shares)
do "${code}02_revenue_microsim.do"

** Per-spec elasticity + revenue-loss combiner. Reads sdid_results.dta and
** revenue_parameters.dta; writes spec_results.dta via the spec engine.
do "${code}02_post_spec.do"

** Donor-cluster bootstrap for highlighted-spec CIs.
** Gated by ${run_bootstrap} from the PROJECT GLOBALS panel above.
**
** Two-stage pipeline:
**   1. 02_bootstrap.do        — runs all reps and writes the canonical
**                               bootstrap_draws.dta.
**   2. 02_bootstrap_tables.do — collapses bootstrap_draws.dta to
**                               percentile CIs in bootstrap_cis.dta,
**                               keyed by spec_id (uses ${ci_level}).
if ${run_bootstrap} == 1 {
	do "${code}02_bootstrap.do"
	do "${code}02_bootstrap_tables.do"
}

** Elasticity and revenue tables + figures
do "${code}02_tables_figures.do"


** ============================================================================
** STAGE 5: DIAGNOSTICS & SAMPLE COUNTS
** ============================================================================
** Observation count table for the four primary methods (SDID, narrow SDID,
** flows, DiD). 
do "${code}02_diagnostics.do"


** ============================================================================
** STAGE 6: APPENDIX & ROBUSTNESS
** ============================================================================

** Method-specific descriptive tables (Appendix A1: SDID / IRS-Flow / ACS).
** Reads sdid_analysis_data.dta + irs_county_flow.dta + acs_county_gross_25plus.dta.
do "${code}02_appendix_descriptives.do"

** SDID on non-migration IRS outcomes (returns, AGI, wages, income).
do "${code}02_otherout_sdid.do"

** Supplemental obs counts for otherout SDID (must run after 02_otherout_sdid).
do "${code}02_diagnostics_supp.do"

** Appendix B: IRS data quality (extended 2012-2022 window).
do "${code}02_appendix_data_quality.do"


** ============================================================================
** CLOSE
** ============================================================================
** Consolidate SDID per-worker failure logs into a single summary file
capture sdid_consolidate_failures

capture log close

