/*******************************************************************************
File Name: 		02_fig_distr_income_acs.do
Creator: 		John Iselin
Date Update:	November 14, 2025

Called by: 00_multnomah.do 

Purpose: Creates Figure XXX

Authors: John Iselin 

For more information, contact XXXX

*******************************************************************************/

** Start log file 
capture log close log_02
log using "${logs}02_log_fig_distr_income_acs_${date}", replace text name(log_02)

** Load data for figure 
use "${data}working/acs_migration_file", replace 

** Set up

** Drop origin state info 
drop state_fips_o county_fips_o 
rename state_fips_d state_fips 
rename county_fips_d county_fips


** Sample 1: Multnomah County Only 
gen sample_multnomah = state_fips == 41 & county_fips == 51

** Sample 2: Rest of Oregon 
gen sample_oregon = state_fips == 41 & county_fips != 51

** Sample 3: US overall 
gen sample_us = 1 

** TODO create "rough" tax units by taking married couples

** TODO create income def similar to AGI 
gen agi = inctot - ... (transfer income)

** Collapse by TU X year 
collapse (sum) perwt agi, by(year sample_* tu_id)

** CREATE SUPPLEMENTAL FIGURES WITH FULL INCOME DISTRIBUTION 




** Generate bins matching IRS 
gen agi_stub = 1 if agi < 1 
replace agi_stub = 2 if inrange(agi, 1, 10000)
replace agi_stub = 3 if inrange(agi, 10000, 25000)
replace agi_stub = 4 if inrange(agi, 25000, 50000)
replace agi_stub = 5 if inrange(agi, 50000, 75000)
replace agi_stub = 6 if inrange(agi, 75000, 100000)
replace agi_stub = 7 if inrange(agi, 100000, 200000)
replace agi_stub = 8 if agi >= 200000

label define lb_agi 1 "Under $1"			///
						2 "$1 under $10K"		///
						3 "$10K under $25K"		///
						4 "$25K under $50K"		///
						5 "$50K under $75K"		///
						6 "$75K under $100K"	///
						7 "$100K under $200K"	///
						8 "$200K or more", modify 
label var agi_stub "AGI Brackets"
label values agi_stub lb_agi 

rename perwt n1 

** Keep required variables 
keep year agi_stub n1 sample_* 

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

graph export "${results}fig_dist_income_acs.jpg", 	///
			as(jpg) name("Graph") quality(100) replace 	
		

** Close 
clear 
log close log_02 
