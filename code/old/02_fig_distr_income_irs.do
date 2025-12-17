/*******************************************************************************
File Name: 		02_fig_distr_income_irs.do
Creator: 		John Iselin
Date Update:	November 14, 2025

Called by: 00_multnomah.do 

Purpose: Creates Figure XXX

Authors: John Iselin 

For more information, contact XXXX

*******************************************************************************/

** Start log file 
capture log close log_02
log using "${logs}02_log_fig_distr_income_irs_${date}", replace text name(log_02)

** Load data for figure 
use "${data}working/irs_county_all.dta", clear 

** Set up

** Sample 1: Multnomah County Only 
gen sample_multnomah = state_fips == 41 & county_fips == 51

** Sample 2: Rest of Oregon 
gen sample_oregon = state_fips == 41 & county_fips != 51

** Sample 3: US overall 
gen sample_us = 1 

** Keep required variables 
keep state_fips county_fips year agi_stub n1 sample_* 

** Loop over samples 
foreach var of varlist sample_* {
	
	** Preserve 
	preserve 
	
	** Keep sample 
	keep if `var' == 1 
	
	** Collapse 
	collapse (sum) n1, by(year agi_stub)
	
	** Save 
	tempfile dta_`var'
	save `dta_`var''
	clear 
	
	** Restore 
	restore 
	
} // END VAR LOOP 

** Clear and import temp files 
clear 
use `dta_sample_multnomah'
gen sample = 1
append using `dta_sample_oregon'
replace sample = 2 if missing(sample)
append using `dta_sample_us'
replace sample = 3 if missing(sample)

** Label variables 
label var sample "Geographic sample"
label define lb_sample 1 "Multnomah County" 2 "Rest of Oregon" 3 "U.S.", modify
label values sample lb_sample

** Set up shares 
sort year agi_stub
bysort year sample: egen tmp1 = total(n1)
gen share = 100 * n1 / tmp1

** Graph bar 
graph bar (asis) share, 				///
		over(agi_stub) 					///
		over(year, label(angle(45))) 	///
		over(sample) stack asyvars 

graph export "${results}fig_dist_income_irs.jpg", 	///
			as(jpg) name("Graph") quality(100) replace 	
		

** Close 
clear 
log close log_02 
