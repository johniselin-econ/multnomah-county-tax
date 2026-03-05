/*****************************************************************************
* Program: 			01_clean_data.do
* Author(s): 		John Iselin
* Date Updated:		February 27, 2026
*
* Purpose:  Master data-cleaning caller. Runs 8 sub-files in order:
*
*   01a_programs.do     — Labels & reusable programs (make_fips, unsuppress, etc.)
*   01b_download.do     — Auto-download IRS/BEA/COVID, verify BLS/DOL
*   01c_demographics.do — NHGIS, BEA economics, BLS unemployment, centroids
*   01d_covid.do        — NYTimes COVID panel
*   01e_acs.do          — ACS microdata, flows, gross migration
*   01f_irs_migration.do— IRS county + state migration
*   01g_irs_agi.do      — IRS county AGI by bracket
*   01h_auxiliary.do    — DOL childcare + property tax rates
*
* Data sources documented in data/README.md
******************************************************************************/

** Start log file
capture log close log_01
log using "${logs}01_log_data_clean_${pr_name}_${date}", replace text name(log_01)

//--------------------------------------------------
// STEP 0: Labels and programs
//--------------------------------------------------
do "${code}01a_programs.do"

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
