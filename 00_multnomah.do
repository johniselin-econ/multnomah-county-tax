/*******************************************************************************
File Name: 		00_multnomah.do
Creator: 		John Iselin
Date Update:	March 3rd, 2026

Purpose: 	Runs the analysis on the effect of tax changes on migration in 
			Multnomah County, Oregon

Authors: John Iselin

For more information, contact john.iselin@yale.edu

*******************************************************************************/

** INSTALLATION 
* net install github, from("https://haghish.github.io/github/")
* github install haghish/rcall, stable
* ssc install ftools
* ssc install reghdfe
* ssc install fre 
* ssc install coefplot
* ssc install sdid 
* ssc install estout 
* ssc install sdid_event
* ssc install geodist
* ssc install ipfraking
* ssc install distinct
** net install parallel, from(https://raw.github.com/gvegayon/parallel/stable/) replace
** mata mata mlib index

** Preliminaries 
capture log close 
clear matrix
clear all 
set more off 

** Name of project 
global pr_name "multnomah"

** Date of run 
global date "`: di %tdCY-N-D daily("$S_DATE", "DMY")'"

** Set Directories
** NOTE: Set your working directory to the project root before running this file.
** Example: cd "C:/Users/yourname/Documents/GitHub/multnomah-county-tax/"
global dir 		`c(pwd)'
** Convert backslashes to forward slashes for R/rcall compatibility (Windows)
global dir = subinstr("${dir}", "\", "/", .)
global code 	"${dir}/code/"				// CODE FILEPATH
global data 	"${dir}/data/"				// DATA FILEPATH
global results 	"${dir}/results/"			// RESULTS FILEPATH
global logs 	"${code}/logs/"				// LOG FILE SUB-FILEPATH
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
local overwrite_csv = 0
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


** CALL R CODE TO IMPORT IPUMS DATA
rcall script "${code}R/api_code.R", ///
    args( project_root  <- "${dir}"; ///
          dir_data_acs  <- "${data}acs"; ///
          api_codes_path<- "${dir}/api_codes.txt"; ///
          start_year    <- ${start_year_acs}; ///
          end_year      <- ${end_year_acs}; ///
          overwrite_csv <- as.logical(`overwrite_csv'); ///
    ) vanilla

** CALL R CODE TO DOWNLOAD QWI DATA (LEHD bulk download)
rcall script "${code}R/qwi_data.R", ///
    args( project_root   <- "${dir}"; ///
          api_codes_path <- "${dir}/api_codes.txt"; ///
          start_year     <- ${start_year_acs}; ///
          end_year       <- ${end_year_acs}; ///
          overwrite_csv  <- as.logical(`overwrite_csv'); ///
    ) vanilla

** CALL R CODE TO DOWNLOAD QCEW DATA (BLS)
rcall script "${code}R/qcew_data.R", ///
    args( project_root  <- "${dir}"; ///
          start_year    <- ${start_year_acs}; ///
          end_year      <- ${end_year_acs}; ///
          overwrite_csv <- as.logical(`overwrite_csv'); ///
    ) vanilla

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

** Create maps 
rcall script "${code}R/map_code.R", vanilla

** Create diagrams 
rcall script "${code}R/fig_diagrams.R", vanilla

** Difference-in-Difference (ACS)
*do ${code}02_did_analysis.do

** Flow-based models (IRS)
*do ${code}02_flow_analysis.do

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

 		
