/*****************************************************************************
* Program: 			01_clean_data.do
* Author(s): 		John Iselin
* Date Updated:		February 27, 2026
*
* Purpose:  Master data-cleaning caller. Runs 7 sub-files in order:
*
*   01b_download.do     — Auto-download IRS/BEA/COVID, verify BLS/DOL
*   01c_demographics.do — NHGIS, BEA economics, BLS unemployment, centroids
*   01d_covid.do        — NYTimes COVID panel
*   01e_acs.do          — ACS microdata, flows, gross migration
*   01f_irs_migration.do— IRS county + state migration
*   01g_irs_agi.do      — IRS county AGI by bracket
*   01h_auxiliary.do    — DOL childcare + property tax rates
*
* NOTE: programs.do (reusable helpers) is now sourced by 00_multnomah.do
*       before this file runs, so its programs are available throughout the
*       pipeline (not just the cleaning stage).
*
* Data sources documented in data/README.md
******************************************************************************/

** Load shared project defaults and helper programs
if "${dir}" == "" {
    local _cwd = subinstr("`c(pwd)'", "\", "/", .)
    if regexm("`_cwd'", "(.*)/code/(stata|utils)$") global dir = regexs(1)
    else global dir "`_cwd'"
}
do "${dir}/code/utils/globals.do"

** Start log file
capture log close log_01
log using "${logs}01_log_data_clean_${pr_name}_${date}", replace text name(log_01)
project_set_seed, context("01_clean_data.do") offset(5)

//--------------------------------------------------
// STEP 1: Download and verify source data
//--------------------------------------------------
do "${code}01b_download.do"

//--------------------------------------------------
// STEP 2: Demographics, economics, unemployment, centroids
//--------------------------------------------------
do "${code}01c_demographics.do"

//--------------------------------------------------
// STEP 3: COVID-19 data
//--------------------------------------------------
do "${code}01d_covid.do"

//--------------------------------------------------
// STEP 4: ACS microdata and migration flows
//--------------------------------------------------
do "${code}01e_acs.do"

//--------------------------------------------------
// STEP 5: IRS county and state migration
//--------------------------------------------------
do "${code}01f_irs_migration.do"

//--------------------------------------------------
// STEP 6: IRS county AGI by bracket
//--------------------------------------------------
do "${code}01g_irs_agi.do"

//--------------------------------------------------
// STEP 7-8: DOL childcare + property tax rates
//--------------------------------------------------
do "${code}01h_auxiliary.do"

** Close log
log close log_01
