/*******************************************************************************
File Name: 		02_descriptives.do
Creator: 		John Iselin
Date Update:	December 17, 2025

Called by: 00_multnomah.do

Purpose: Preform descriptive analysis

Authors: John Iselin 

For more information, contact john.iselin@yale.edu

*******************************************************************************/

** Start log file 
capture log close log_02
log using "${logs}02_log_descriptives_${date}", replace text name(log_02)

** Determine set of common in- and out-migration counties for Multnomah
use ${data}working/irs_county_flow.dta, clear 

** Tag Multnomah
gen multnomah_o = (state_fips_o == 41 & county_fips_o == 51)
gen multnomah_d = (state_fips_d == 41 & county_fips_d == 51)

** Loop over samples 
foreach x in "o" "d" {
	
	** Preserve 
	preserve 
	
	** Keep Multnomah
	keep if multnomah_`x' == 1 
	
	** Export data 
	export excel using "${data}multnomah.xlsx", 	///
		sheet(irs_`x', replace ) firstrow(variables) 
		
	** Clear and restore 
	clear 
	restore
		
} // END ORIGIN-DESTINATION LOOP 

** Determine set of common in- and out-migration counties for Multnomah
use ${data}working/acs_county_flow.dta, clear 

** Tag Multnomah
gen multnomah_o = (state_fips_o == 41 & county_fips_o == 51)
gen multnomah_d = (state_fips_d == 41 & county_fips_d == 51)

** Loop over samples 
foreach x in "o" "d" {
	
	** Preserve 
	preserve 
	
	** Keep Multnomah
	keep if multnomah_`x' == 1 
	
	** Export data 
	export excel using "${data}multnomah.xlsx", 	///
		sheet(acs_`x', replace ) firstrow(variables) 
		
	** Clear and restore 
	clear 
	restore
		
} // END ORIGIN-DESTINATION LOOP 

** Close log
clear 
log close log_02
