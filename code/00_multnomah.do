/*******************************************************************************
File Name: 		00_multnomah.do
Creator: 		John Iselin
Date Update:	December 12, 2025

Purpose: 	Runs the analysis on the effect of tax changes on migration in 
			Multnomah County, Oregon

Authors: John Iselin

For more information, contact joiselin@gmail.com

*******************************************************************************/

** INSTALLATION 
* net install github, from("https://haghish.github.io/github/")
* github install haghish/rcall, stable
* ssc install ftools
* ssc install reghdfe
* ssc install fre 
* ssc install coefplot

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
global dir "C:/Users/ji252/Documents/GitHub/multnomah-county-tax/" 	
global code 	"${dir}code/"				// CODE FILEPATH
global data 	"${dir}data/"				// DATA FILEPATH
global results 	"${dir}results/"			// RESULTS FILEPATH
global logs 	"${code}logs/"				// LOG FILE SUB-FILEPATH

** Set WD 
cd ${dir}

** OVERLEAF FILE PATH 
/*
global oth_path		///
	"/Users/johniselin/Library/CloudStorage/Dropbox/Apps/Overleaf/Multnomah County/"	
*/
	
** Start log file 
log using "${logs}00_log_${pr_name}_${date}", replace text 

** Set Seed 
set seed 56403

** Set scheme
set scheme plotplainblind

** Set parameters 
local overwrite_csv = 0
global start_year_acs = 2015
global end_year_acs = 2023

** CALL R CODE TO IMPORT IPUMS DATA 
rcall script "${code}R/api_code.R", ///
    args( project_root  <- "${dir}"; ///
          dir_data_acs  <- "${data}acs"; ///
          api_codes_path<- "${dir}api_codes.txt"; ///
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
do ${code}01_clean_data.do

** ANALYSIS (02)

** Descriptives 
do ${code}02_descriptives.do 

** Synthetic Difference-in-Difference Analysis
do ${code}02_sdid_analysis.do 

** Individual-level Model
do ${code}02_indiv_analysis_clean.do 


** End log file
capture log close

 		

		
	


