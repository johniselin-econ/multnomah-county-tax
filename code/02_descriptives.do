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
SECTION 2: PRE-POST FLOW COMPARISON (2018-2019 vs 2021-2022)
Purpose: Compare IRS migration flows to/from Multnomah County before and after
         the 2020 tax change. Creates datasets and exports CSVs for mapping.
         Includes both raw flows and rates (flow / Multnomah population).
*******************************************************************************/

** ----------------------------------------------------------------
** GET MULTNOMAH BASELINE POPULATION FOR RATE CALCULATIONS
** ----------------------------------------------------------------

** Load gross data to get Multnomah's total population by period
use "${data}working/irs_county_gross", clear

** Keep only Multnomah County
keep if state_fips == 41 & county_fips == 51

** Keep only pre and post periods
gen period = ""
replace period = "pre" if inlist(year, 2018, 2019)
replace period = "post" if inlist(year, 2021, 2022)
keep if period != ""

** Calculate base population (non-movers + all movers = total filers)
gen n1_base = n1_out_1 + n1_out_2
gen n2_base = n2_out_1 + n2_out_2
gen agi_base = agi_out_1 + agi_out_2

** Collapse to get average by period
collapse (mean) n1_base n2_base agi_base, by(period)

** Store population values in locals for later use
summ n1_base if period == "pre", meanonly
local n1_pop_pre = r(mean)
summ n1_base if period == "post", meanonly
local n1_pop_post = r(mean)

summ n2_base if period == "pre", meanonly
local n2_pop_pre = r(mean)
summ n2_base if period == "post", meanonly
local n2_pop_post = r(mean)

summ agi_base if period == "pre", meanonly
local agi_pop_pre = r(mean)
summ agi_base if period == "post", meanonly
local agi_pop_post = r(mean)

** Display population values for log
dis "Multnomah County baseline populations:"
dis "  n1 (returns): pre = `n1_pop_pre', post = `n1_pop_post'"
dis "  n2 (exemptions): pre = `n2_pop_pre', post = `n2_pop_post'"
dis "  agi: pre = `agi_pop_pre', post = `agi_pop_post'"

clear

** ----------------------------------------------------------------
** LOAD FLOW DATA AND CREATE COMPARISON DATASETS
** ----------------------------------------------------------------

** Load IRS flow data
use ${data}working/irs_county_flow.dta, clear

** Define Multnomah County FIPS
local multnomah_fips = 41051

** Define pre and post periods
local pre_years "2018 2019"
local post_years "2021 2022"

** Create period indicator
gen period = ""
replace period = "pre" if inlist(year, 2018, 2019)
replace period = "post" if inlist(year, 2021, 2022)

** Keep only pre and post periods
keep if period != ""

** Loop over each measure to create separate datasets
foreach measure in "n1" "n2" "agi" {

	** Display status
	dis "Creating flow comparison dataset for `measure'..."

	** Preserve original data
	preserve

	** ----------------------------------------------------------------
	** OUT-MIGRATION: Flows FROM Multnomah TO other counties
	** ----------------------------------------------------------------

	** Keep flows where Multnomah is the origin
	keep if fips_o == `multnomah_fips'

	** Collapse to sum flows by destination county and period
	collapse (sum) `measure', by(fips_d period)

	** Reshape to wide format (pre and post as columns)
	reshape wide `measure', i(fips_d) j(period) string

	** Rename for clarity
	rename fips_d fips
	rename `measure'pre out_pre
	rename `measure'post out_post

	** Save temp file for out-migration
	tempfile out_flows
	save `out_flows'

	** ----------------------------------------------------------------
	** IN-MIGRATION: Flows TO Multnomah FROM other counties
	** ----------------------------------------------------------------

	** Restore and start fresh
	restore
	preserve

	** Keep flows where Multnomah is the destination
	keep if fips_d == `multnomah_fips'

	** Collapse to sum flows by origin county and period
	collapse (sum) `measure', by(fips_o period)

	** Reshape to wide format
	reshape wide `measure', i(fips_o) j(period) string

	** Rename for clarity
	rename fips_o fips
	rename `measure'pre in_pre
	rename `measure'post in_post

	** ----------------------------------------------------------------
	** MERGE: Combine in and out flows
	** ----------------------------------------------------------------

	** Merge with out-migration flows
	merge 1:1 fips using `out_flows', nogen

	** Replace missing with 0 (counties with flows in only one direction)
	foreach var of varlist out_pre out_post in_pre in_post {
		replace `var' = 0 if missing(`var')
	}

	** ----------------------------------------------------------------
	** ADD MULTNOMAH BASELINE POPULATION
	** ----------------------------------------------------------------

	** Add Multnomah population for pre and post periods
	gen pop_pre = ``measure'_pop_pre'
	gen pop_post = ``measure'_pop_post'

	** ----------------------------------------------------------------
	** CALCULATE RATES (flow / population * 100)
	** ----------------------------------------------------------------

	** Out-migration rates (% of Multnomah population moving to each county)
	gen out_rate_pre = 100 * out_pre / pop_pre
	gen out_rate_post = 100 * out_post / pop_post

	** In-migration rates (% of Multnomah population represented by in-migrants)
	gen in_rate_pre = 100 * in_pre / pop_pre
	gen in_rate_post = 100 * in_post / pop_post

	** Calculate rate changes
	gen out_rate_change = out_rate_post - out_rate_pre
	gen in_rate_change = in_rate_post - in_rate_pre
	gen net_rate_change = in_rate_change - out_rate_change

	** Order columns
	order fips pop_pre pop_post ///
		out_pre out_post out_rate_pre out_rate_post out_rate_change ///
		in_pre in_post in_rate_pre in_rate_post in_rate_change net_rate_change

	** Sort by fips
	sort fips

	** Label variables
	label var fips "County FIPS code"
	label var pop_pre "Multnomah baseline population (2018-2019 avg)"
	label var pop_post "Multnomah baseline population (2021-2022 avg)"
	label var out_pre "Out-migration from Multnomah (2018-2019)"
	label var out_post "Out-migration from Multnomah (2021-2022)"
	label var out_rate_pre "Out-migration rate (%, 2018-2019)"
	label var out_rate_post "Out-migration rate (%, 2021-2022)"
	label var out_rate_change "Change in out-migration rate (pp)"
	label var in_pre "In-migration to Multnomah (2018-2019)"
	label var in_post "In-migration to Multnomah (2021-2022)"
	label var in_rate_pre "In-migration rate (%, 2018-2019)"
	label var in_rate_post "In-migration rate (%, 2021-2022)"
	label var in_rate_change "Change in in-migration rate (pp)"
	label var net_rate_change "Change in net migration rate (pp)"

	** Save Stata dataset to working directory (for R mapping)
	save "${data}working/multnomah_flow_comparison_`measure'.dta", replace

	** Export to CSV for R mapping
	export delimited using "${data}working/multnomah_flow_comparison_`measure'.csv", replace

	** Display summary statistics
	dis "Summary for `measure' - Raw flows:"
	summ out_pre out_post in_pre in_post

	dis "Summary for `measure' - Rates (%):"
	summ out_rate_pre out_rate_post in_rate_pre in_rate_post

	dis "Summary for `measure' - Rate changes (pp):"
	summ out_rate_change in_rate_change net_rate_change

	** Restore original data for next measure
	restore

} // END MEASURE LOOP

dis "Flow comparison datasets created and exported to ${results}flows/"

** Close log
clear
log close log_02
