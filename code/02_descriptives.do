/*******************************************************************************
File Name: 		02_descriptives.do
Creator: 		John Iselin
Date Update:	January 15, 2026

Called by: 00_multnomah.do

Purpose: Perform descriptive analysis

Outputs:
- multnomah_flow_comparison_[n1|n2|agi].dta/csv: Pre-post flow comparison with:
  - Raw flows (out_pre, out_post, in_pre, in_post)
  - Multnomah baseline population (pop_pre, pop_post)
  - Migration rates as % of Multnomah population (out_rate_*, in_rate_*)
  - Rate changes in percentage points (out_rate_change, in_rate_change, net_rate_change)

- table2.xlsx: Migration rates for Multnomah and neighboring counties
  - Sheet "IRS": IRS-based rates (2016-19 vs 2021-22)
  - Sheet "ACS": ACS-based rates (2016-19 vs 2021-22 vs 2021-24)
  - Rows: Multnomah, neighboring OR/WA counties, all other OR/WA combined
  - Rates: in-migration and out-migration as % of partner county population
  - Change in net in-migration rate (in_rate - out_rate) from pre to post period

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

/*******************************************************************************
SECTION 2: PRE-POST FLOW COMPARISON (2018-2019 vs 2021-2022 / 21-24)
*******************************************************************************/

** IRS DATA 

** Load gross data to get Multnomah's total population by period
use "${data}working/irs_county_gross", clear

** Keep only in sample 
keep if inlist(state_name, "Oregon", "Washington")
keep if inlist(year, 2018, 2019, 2021, 2022)

** Keep required variables 
keep year fips state_* county_* 	///
	*_out_1 *_out_2 *_out_3			///
	*_in_1 *_in_2 *_in_3 			//
	
** Keep only pre and post periods
gen period = ""
replace period = "pre" if inlist(year, 2018, 2019)
replace period = "post_21_22" if inlist(year, 2021, 2022)
keep if period != ""

** Calculate base population (non-movers + all movers = total filers)
gen n1_base = n1_out_1 + n1_out_2
gen n2_base = n2_out_1 + n2_out_2
gen agi_base = agi_out_1 + agi_out_2

** Create Other Oregon and Other Washington 
drop county_fips state_fips 

replace county_name = "Other" if state_name == "Oregon" & !inlist(fips, 41051, 41067, 41005, 41047, 41071, 41009)
replace county_name = "Other" if state_name == "Washington" & !inlist(fips, 53011, 53059)

** Collapse by time 
collapse (sum) *_base *_out_3 *_in_3, by(state_name county_name period)

** Calculate rates 
foreach x in "n1" "n2" "agi" {
	gen `x'_out_rate = `x'_out_3 / `x'_base
	gen `x'_in_rate = `x'_in_3 / `x'_base
} // END RATE LOOP 

** Keep final variables 
keep state_name county_name period *_rate

** Export 
export excel using "${results}Table2.xlsx", 	///
	sheet("raw_irs") sheetreplace firstrow(variables)

clear

** ACS DATA

** Load gross data to get Multnomah's total population by period
use "${data}working/acs_county_gross_18plus.dta", clear

** Keep only in sample 
keep if inlist(state_name, "Oregon", "Washington")
keep if inlist(year, 2018, 2019, 2021, 2022, 2023, 2024)

** Keep required variables 
keep year fips state_* county_* 	///
	*_out_1 *_out_2 *_out_3			///
	*_in_1 *_in_2 *_in_3 			//
	
** Keep only pre and post periods
gen period = ""
replace period = "pre" if inlist(year, 2018, 2019)
replace period = "post_21_22" if inlist(year, 2021, 2022)
replace period = "post_23_24" if inlist(year, 2023, 2024)
keep if period != ""

** Calculate base population (non-movers + all movers = total filers)
gen hh_base = households_out_1 + households_out_2
gen per_base = persons_out_1 + persons_out_2
gen dol_base = dollars_out_1 + dollars_out_2

** Rename 
rename households_* hh_* 
rename persons_* per_* 
rename dollars_* dol_*

** Create Other Oregon and Other Washington 
drop county_fips state_fips 

replace county_name = "Other" if state_name == "Oregon" & !inlist(fips, 41051, 41067, 41005, 41047, 41071, 41009)
replace county_name = "Other" if state_name == "Washington" & !inlist(fips, 53011, 53059)

** Collapse by time 
collapse (sum) *_base *_out_3 *_in_3, by(state_name county_name period)

** Update period
gen tmp = inlist(period, "post_21_22", "post_23_24")

** Loop over variables 
foreach var of varlist hh_* per_* dol_* {
	
	bysort state_name county_name tmp: egen total = total(`var')
	replace `var' = total if period == "post_23_24"
	drop total 

}

drop tmp 
replace period = "post_21_24" if period == "post_23_24"


** Calculate rates 
foreach x in "hh" "per" "dol" {
	gen `x'_out_rate = `x'_out_3 / `x'_base
	gen `x'_in_rate = `x'_in_3 / `x'_base
} // END RATE LOOP 

** Keep final variables 
keep state_name county_name period *_rate

** Export 
export excel using "${results}Table2.xlsx", 	///
	sheet("raw_acs") sheetreplace firstrow(variables)

clear

** Close log
clear
log close log_02
