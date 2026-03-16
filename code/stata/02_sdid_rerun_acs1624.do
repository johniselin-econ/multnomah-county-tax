/*******************************************************************************
File Name:      02_sdid_rerun_acs1624.do
Purpose:        Re-run only the ACS 2016-2024 SDID specifications after a
                sample-definition change, then rebuild SDID-derived outputs.

Usage:
- First update the acs_period_2 definition in 02_sdid_analysis.do.
- Then run this file from the project root after globals are available, or let
  it infer the standard project globals from the working directory.

What it does:
1. Backs up results/sdid/sdid_results.dta
2. Removes the stale acs_16_24* rows from sdid_results.dta
3. Runs 02_sdid_analysis.do with sdid_data_filter = "acs_period_2",
   sequentially, in resume mode
4. Rebuilds specification curves and influence figures automatically as part of
   02_sdid_analysis.do

Notes:
- This does not rerun revenue/elasticties; rerun those separately if needed.
- Existing acs_16_24 tables/figures in results/sdid/ will be overwritten.
*******************************************************************************/

capture log close log_02_sdid_rerun_acs1624

** Load shared project defaults and helper programs
local cwd = subinstr("`c(pwd)'", "\", "/", .)
local suffix "/code/stata"
if "${dir}" == "" {
    if length("`cwd'") >= length("`suffix'") & ///
        substr("`cwd'", length("`cwd'") - length("`suffix'") + 1, .) == "`suffix'" {
        global dir = substr("`cwd'", 1, length("`cwd'") - length("`suffix'"))
    }
    else {
        global dir "`cwd'"
    }
}
if "${code}" == "" global code "${dir}/code/stata/"
do "${code}00_stata_config.do"
project_set_seed, context("02_sdid_rerun_acs1624.do") offset(140)

global date "`: di %tdCY-N-D daily("$S_DATE", "DMY")'"
log using "${logs}02_log_rerun_acs1624_${date}", replace text name(log_02_sdid_rerun_acs1624)

capture confirm file "${results}sdid/sdid_results.dta"
if _rc {
    di as error "ERROR: ${results}sdid/sdid_results.dta not found."
    error 601
}

** Backup current results before pruning/replacing rows
copy "${results}sdid/sdid_results.dta" ///
     "${results}sdid/sdid_results_backup_before_acs1624_rerun_${date}.dta", replace

use "${results}sdid/sdid_results.dta", clear
drop if inlist(sample_data, ///
    "acs_16_24_all", "acs_16_24_col", ///
    "acs_outstate_16_24_all", "acs_outstate_16_24_col")
compress
save "${results}sdid/sdid_results.dta", replace
export excel using "${results}sdid/sdid_results.xlsx", firstrow(variables) replace

** Force a targeted sequential rerun
global use_parallel = 0
global resume = 1
global sdid_data_filter "acs_period_2"

do "${code}02_sdid_analysis.do"

macro drop sdid_data_filter

di as txt "Targeted ACS 2016-2024 rerun complete."
di as txt "If needed, rerun 02_revenue.do and 02_elasticities.do to refresh downstream outputs."
