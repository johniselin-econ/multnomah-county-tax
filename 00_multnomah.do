/*******************************************************************************
File Name: 		00_multnomah.do
Creator: 		John Iselin
Date Update:	March 5th, 2026

Purpose: 	Runs the Stata analysis on the effect of tax changes on migration in
			Multnomah County, Oregon.

			NOTE: Run 00_multnomah.R first to download all data (ACS, QWI, QCEW,
			Census age shares) and generate R-based figures (maps, diagrams).

Authors: John Iselin

For more information, contact john.iselin@yale.edu

*******************************************************************************/

** INSTALLATION
* ssc install reghdfe, replace
* ssc install ftools, replace
* ssc install ppmlhdfe, replace
* ssc install sdid, replace
* ssc install sdid_event, replace
* ssc install estout, replace
* ssc install coefplot, replace
* ssc install fre, replace
* ssc install distinct, replace
* ssc install blindschemes, replace
* net install parallel, from(https://raw.github.com/gvegayon/parallel/stable/) replace

** Preliminaries
capture log close
clear matrix
clear all
set more off

** CHECK REQUIRED PACKAGES
** Verify all user-written packages are installed before proceeding.
local pkg_missing = 0
foreach pkg in reghdfe ftools ppmlhdfe sdid sdid_event estout coefplot fre distinct {
    capture which `pkg'
    if _rc {
        di as error "  Package not found: `pkg'"
        local pkg_missing = 1
    }
}
** blindschemes check (look for the scheme file, not an ado)
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

** Name of project
global pr_name "multnomah"

** Date of run
global date "`: di %tdCY-N-D daily("$S_DATE", "DMY")'"

** Set Directories
** NOTE: Set your working directory to the project root before running this file.
** Example: cd "C:/Users/yourname/Documents/GitHub/multnomah-county-tax/"
global dir 		`c(pwd)'
** Convert backslashes to forward slashes for compatibility (Windows)
global dir = subinstr("${dir}", "\", "/", .)
global code 	"${dir}/code/stata/"			// STATA CODE FILEPATH
global rcode 	"${dir}/code/R/"				// R CODE FILEPATH
global data 	"${dir}/data/"					// DATA FILEPATH
global results 	"${dir}/results/"				// RESULTS FILEPATH
global logs 	"${code}logs/"					// LOG FILE SUB-FILEPATH
cd $dir

** OVERLEAF FILE PATH (optional — for syncing outputs to Overleaf)
** To enable, create profile.do (gitignored) in the project root with:
**   global oth_path "C:/Users/yourname/Dropbox/Apps/Overleaf/Your Project/"
global overleaf = 0
global oth_path ""
global ol_fig   ""
global ol_tab   ""

** Load user-specific overrides from profile.do (gitignored)
capture do "${dir}/profile.do"

** If oth_path was set in profile.do, derive Overleaf subdirectories
if "${oth_path}" != "" {
    global ol_fig "${oth_path}figures/"
    global ol_tab "${oth_path}tables/"
    global overleaf = 1
}

** Ensure output directories exist
capture mkdir "${results}"
capture mkdir "${results}tables"
capture mkdir "${results}figures"
capture mkdir "${results}sdid"
capture mkdir "${results}flows"
capture mkdir "${results}did"
capture mkdir "${results}individual"
capture mkdir "${code}logs"

** Start log file
log using "${logs}00_log_${pr_name}_${date}", replace text

** Set Seed
set seed 56403

** Set scheme
set scheme plotplainblind

** PARALLEL PROCESSING FLAG
** Set to 1 to use parallel processing, 0 for sequential processing
global use_parallel = 1
global n_clusters = 6

** Set parameters
global start_year_irs_data = 2012		// Extended back for appendix (2011-12 flows)
global start_year_irs_analysis = 2016	// Main analysis start (unchanged)
global start_year_acs = 2012			// Extended back for appendix comparison
global end_year_acs = 2024

** IRS file year ranges (2-digit)
global start_yy_irs_download = 11		// IRS file download start (2011-12 flows)
global end_yy_irs_migration  = 21		// IRS migration file end (2021-22 flows)
global end_yy_irs_agi        = 22		// IRS AGI file end (2022 data)
global start_yy_irs_county   = 12		// County data processing start
global end_yy_irs_county     = 22		// County data processing end


** CLEAN DATA (01)
** (a) 	Demographic data via IPUMS NHGIS
** 		- https://www.nhgis.org/
** (b) 	Individual-level ACS data via IPUMS USA
** 		- https://usa.ipums.org/usa/index.shtml
** (c) 	County-level IRS migration via IRS SOI
** 		- https://www.irs.gov/statistics/soi-tax-stats-migration-data
** (d) County-level IRS data via IRS SOI
** 		- https://www.irs.gov/statistics/soi-tax-stats-county-data
** (e) NYTimes Covid-19 Cases and Deaths by county
** 		- https://github.com/nytimes/covid-19-data
** (f) County-level childcare cost data via DOL
** 		- www.dol.gov/sites/dolgov/files/WB/NDCP2022.xlsx
** (g) County-level Unemployment data via BLS
** 		- https://www.bls.gov/lau/
** (h) County-level QWI data via LEHD bulk download
** 		- https://lehd.ces.census.gov/data/qwi/
** (i) County-level QCEW data via BLS
** 		- https://www.bls.gov/cew/
do ${code}01_clean_data.do

** ANALYSIS (02)

** Descriptives
do ${code}02_descriptives.do

** Flow Analysis (IRS county-level flow regressions)
do ${code}02_flow_analysis.do

** Difference-in-Differences (ACS individual-level DiD)
do ${code}02_did_analysis.do

** Synthetic Difference-in-Difference Analysis
do ${code}02_sdid_analysis.do

** Narrow SDID (similar-cities control pool)
do ${code}02_narrow_sdid.do

** Other Outcomes SDID (non-migration IRS outcomes)
do ${code}02_otherout_sdid.do

** Quarterly SDID (QWI employment/earnings + QCEW establishments/wages)
do ${code}02_quarterly_sdid.do

** Individual-level Model
do ${code}02_indiv_analysis.do

** Revenue Effects of Tax-Induced Migration
do "${code}02_revenue.do"

** Elasticities of Migration with Respect to PFA Tax
do "${code}02_elasticities.do"

** Appendix B: IRS Data Quality
do "${code}02_appendix_data_quality.do"

** Diagnostics: observation count table (optional — uncomment to run)
* do "${code}02_diagnostics.do"

** End log file
capture log close

