/*******************************************************************************
File Name: 		02_descriptives.do
Creator: 		John Iselin
Date Update:	January 30, 2026

Called by: 00_multnomah.do

Purpose: Perform descriptive analysis

Outputs:
- multnomah_flow_comparison_[n1|n2|agi].dta/csv: Pre-post flow comparison with:
  - Raw flows (out_pre, out_post, in_pre, in_post)
  - Multnomah baseline population (pop_pre, pop_post)
  - Migration rates as % of Multnomah population (out_rate_*, in_rate_*)
  - Rate changes in percentage points (out_rate_change, in_rate_change, net_rate_change)

- multnomah_partner_flows_[n1|n2|agi].dta/csv: Partner-normalized flows for maps:
  - Out-migration rates per 100K of DESTINATION county population
  - In-migration rates per 100K of ORIGIN county population
  - Used by R/map_code.R to create directional flow maps

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

** ---- Export LaTeX version of Table 2 (IRS) ----
preserve

** Focus on n1 (returns) measure
keep state_name county_name period n1_out_rate n1_in_rate

** Convert to percentages
replace n1_out_rate = n1_out_rate * 100
replace n1_in_rate  = n1_in_rate * 100

** Reshape wide by period
reshape wide n1_out_rate n1_in_rate, i(state_name county_name) j(period) string

** Compute change in net migration rate (percentage points)
gen change_net = (n1_in_ratepost_21_22 - n1_out_ratepost_21_22) - ///
				 (n1_in_ratepre - n1_out_ratepre)

** Create row ordering
gen order = .
replace order = 1  if county_name == "Multnomah County"
replace order = 3  if state_name == "Oregon"     & county_name == "Washington County"
replace order = 4  if state_name == "Oregon"     & county_name == "Clackamas County"
replace order = 5  if state_name == "Oregon"     & county_name == "Marion County"
replace order = 6  if state_name == "Oregon"     & county_name == "Yamhill County"
replace order = 7  if state_name == "Oregon"     & county_name == "Columbia County"
replace order = 9  if state_name == "Washington" & county_name == "Clark County"
replace order = 10 if state_name == "Washington" & county_name == "Skamania County"
replace order = 11 if state_name == "Oregon"     & county_name == "Other"
replace order = 12 if state_name == "Washington" & county_name == "Other"
sort order

** Create display labels (strip " County" suffix)
gen label = subinstr(county_name, " County", "", 1)
replace label = "All other OR counties" if state_name == "Oregon"     & county_name == "Other"
replace label = "All other WA counties" if state_name == "Washington" & county_name == "Other"

** Write LaTeX file (requires \usepackage{booktabs, threeparttable})
local nrows = _N
tempname fh
file open `fh' using "${results}tables/table2.tex", write replace

file write `fh' "% Table 2: Migration Rates for Multnomah and Neighboring Counties" _n
file write `fh' "% Generated by 02_descriptives.do" _n
file write `fh' "% Requires: \usepackage{booktabs, threeparttable}" _n
file write `fh' `"\begin{table}[htbp]"' _n
file write `fh' `"\centering"' _n
file write `fh' `"\begin{threeparttable}"' _n
file write `fh' `"\caption{Migration Rates for Multnomah County and Neighboring Counties}"' _n
file write `fh' `"\label{tab:migration_rates}"' _n
file write `fh' `"\small"' _n
file write `fh' `"\resizebox{\textwidth}{!}{%"' _n
file write `fh' `"\begin{tabular}{lccccc}"' _n
file write `fh' `"\toprule"' _n
file write `fh' `" & \multicolumn{2}{c}{In-Migration Rate} & \multicolumn{2}{c}{Out-Migration Rate} & Change in \\"' _n
file write `fh' `"\cmidrule(lr){2-3} \cmidrule(lr){4-5}"' _n
file write `fh' `"County & 2018--19 & 2021--22 & 2018--19 & 2021--22 & Net Rate \\"' _n
file write `fh' `"\midrule"' _n

forvalues i = 1/`nrows' {

	local lbl = label[`i']
	local ord = order[`i']

	** Insert section headers
	if `ord' == 3 {
		file write `fh' `"\addlinespace"' _n
		file write `fh' `"\textit{Neighboring OR counties} & & & & & \\"' _n
	}
	if `ord' == 9 {
		file write `fh' `"\addlinespace"' _n
		file write `fh' `"\textit{Neighboring WA counties} & & & & & \\"' _n
	}
	if `ord' == 11 {
		file write `fh' `"\addlinespace"' _n
	}

	** Format numbers to 2 decimal places
	local in_pre  = trim(string(n1_in_ratepre[`i'],        "%9.2f"))
	local in_post = trim(string(n1_in_ratepost_21_22[`i'], "%9.2f"))
	local out_pre = trim(string(n1_out_ratepre[`i'],        "%9.2f"))
	local out_post = trim(string(n1_out_ratepost_21_22[`i'], "%9.2f"))
	local chg      = trim(string(change_net[`i'],            "%9.2f"))

	** Indent neighboring county sub-rows
	if inlist(`ord', 3, 4, 5, 6, 7, 9, 10) {
		file write `fh' `"\quad `lbl' & `in_pre' & `in_post' & `out_pre' & `out_post' & `chg' \\"' _n
	}
	else {
		file write `fh' `"`lbl' & `in_pre' & `in_post' & `out_pre' & `out_post' & `chg' \\"' _n
	}
}

file write `fh' `"\bottomrule"' _n
file write `fh' `"\end{tabular}%"' _n
file write `fh' `"}"' _n
file write `fh' `"\begin{tablenotes}[flushleft]"' _n
file write `fh' `"\small"' _n
file write `fh' `"\item \textit{Notes:} Rates are domestic migration flows as a percentage of the base filing population (non-movers plus all movers) for each county-period. Pre-period averages tax years 2018--2019; post-period averages 2021--2022. Change in net rate is the shift in net in-migration rate (in-rate minus out-rate) between periods, in percentage points. Source: IRS Statistics of Income."' _n
file write `fh' `"\end{tablenotes}"' _n
file write `fh' `"\end{threeparttable}"' _n
file write `fh' `"\end{table}"' _n

file close `fh'
dis "LaTeX table exported to: ${results}tables/table2.tex"
if ${overleaf} == 1 {
	copy "${results}tables/table2.tex" "${ol_tab}table2.tex", replace
}

restore

clear

** ACS DATA

** Load gross data to get Multnomah's total population by period
use "${data}working/acs_county_gross_25plus.dta", clear

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

/*******************************************************************************
SECTION 3: PARTNER-COUNTY NORMALIZED FLOW MAPS
Purpose: Create flow data normalized by partner county population (per 100K)
         for directional flow maps showing Multnomah's migration patterns.

Out-flow map: Rate of migration FROM Multnomah TO each destination county
              normalized by DESTINATION county average population
In-flow map:  Rate of migration TO Multnomah FROM each origin county
              normalized by ORIGIN county average population

Outputs:
- multnomah_partner_flows_[n1|n2|agi].csv: Flow data with partner normalization
*******************************************************************************/

** ------------------------------------------------------------
** GET COUNTY POPULATIONS FOR RATE CALCULATIONS
** ------------------------------------------------------------

** Load gross data to get each county's population by period
use "${data}working/irs_county_gross", clear

** Keep only pre and post periods
gen period = ""
replace period = "pre" if inlist(year, 2018, 2019)
replace period = "post" if inlist(year, 2021, 2022)
keep if period != ""

** Calculate base population (non-movers + all movers = total filers)
gen n1_pop = n1_out_1 + n1_out_2
gen n2_pop = n2_out_1 + n2_out_2
gen agi_pop = agi_out_1 + agi_out_2

** Drop non-balanced counties 
bysort fips: gen ct = _N
keep if ct == 4
drop ct 

** Keep relevant variables
keep fips period n1_pop n2_pop agi_pop 

** Collapse to get average by period
collapse (sum) n1_pop n2_pop agi_pop, by(fips period)

** Reshape to wide format
reshape wide n1_pop n2_pop agi_pop, i(fips) j(period) string

** Calculate average population across periods
gen n1_pop_avg = (n1_poppre + n1_poppost) / 2
gen n2_pop_avg = (n2_poppre + n2_poppost) / 2
gen agi_pop_avg = (agi_poppre + agi_poppost) / 2

** Keep needed variables
keep fips *_pop_avg *_poppre *_poppost
rename *_poppre *_pop_pre
rename *_poppost *_pop_post

** Save county populations
tempfile county_pops
save `county_pops'

clear

** ------------------------------------------------------------
** LOAD FLOW DATA AND CREATE PARTNER-NORMALIZED DATASETS
** ------------------------------------------------------------

** Load IRS flow data
use ${data}working/irs_county_flow.dta, clear

** Define Multnomah County FIPS
local multnomah_fips = 41051

** Define pre and post periods
gen period = ""
replace period = "pre" if inlist(year, 2018, 2019)
replace period = "post" if inlist(year, 2021, 2022)

** Keep only pre and post periods
keep if period != ""

** Loop over each measure to create separate datasets
foreach measure in "n1" "n2" "agi" {

	** Display status
	dis "Creating partner-normalized flow dataset for `measure'..."

	** Preserve original data
	preserve

	** ------------------------------------------------------------
	** OUT-MIGRATION: Flows FROM Multnomah TO other counties
	** ------------------------------------------------------------

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
	
	** Replace missing values with 0s
	replace out_pre = 0 if missing(out_pre)
	replace out_post = 0 if missing(out_post)

	** Merge with destination county populations
	merge 1:1 fips using `county_pops', keep(match) nogen ///
		keepusing(`measure'_pop_pre `measure'_pop_post `measure'_pop_avg)

	** Rename population variables for out-migration (destination pop)
	rename `measure'_pop_pre dest_pop_pre
	rename `measure'_pop_post dest_pop_post
	rename `measure'_pop_avg dest_pop_avg

	** Calculate out-migration rate per 100K of destination population
	gen out_rate_pre = 100000 * out_pre / dest_pop_pre
	gen out_rate_post = 100000 * out_post / dest_pop_post
	gen out_rate_change = out_rate_post - out_rate_pre

	** Save temp file for out-migration
	tempfile out_flows
	save `out_flows'

	** ------------------------------------------------------------
	** IN-MIGRATION: Flows TO Multnomah FROM other counties
	** ------------------------------------------------------------

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
	
	** Replace missing values with 0s
	replace in_pre = 0 if missing(in_pre)
	replace in_post = 0 if missing(in_post)

	** Merge with origin county populations
	merge 1:1 fips using `county_pops', keep(match) nogen ///
		keepusing(`measure'_pop_pre `measure'_pop_post `measure'_pop_avg)

	** Rename population variables for in-migration (origin pop)
	rename `measure'_pop_pre orig_pop_pre
	rename `measure'_pop_post orig_pop_post
	rename `measure'_pop_avg orig_pop_avg

	** Calculate in-migration rate per 100K of origin population
	gen in_rate_pre = 100000 * in_pre / orig_pop_pre
	gen in_rate_post = 100000 * in_post / orig_pop_post
	gen in_rate_change = in_rate_post - in_rate_pre

	** ------------------------------------------------------------
	** MERGE: Combine in and out flows
	** ------------------------------------------------------------

	** Merge with out-migration flows
	merge 1:1 fips using `out_flows', nogen

	** Replace missing with 0 (counties with flows in only one direction)
	foreach var of varlist out_pre out_post in_pre in_post {
		replace `var' = 0 if missing(`var')
	}

	** Calculate net rate change (negative = net outflow from Multnomah)
	gen net_rate_change = in_rate_change - out_rate_change

	** Order columns
	order fips ///
		dest_pop_avg out_pre out_post out_rate_pre out_rate_post out_rate_change ///
		orig_pop_avg in_pre in_post in_rate_pre in_rate_post in_rate_change ///
		net_rate_change

	** Sort by fips
	sort fips

	** Label variables
	label var fips "Partner county FIPS code"
	label var dest_pop_avg "Destination county avg population (for out-migration rate)"
	label var out_pre "Out-migration from Multnomah (2018-2019)"
	label var out_post "Out-migration from Multnomah (2021-2022)"
	label var out_rate_pre "Out-migration rate per 100K dest pop (pre)"
	label var out_rate_post "Out-migration rate per 100K dest pop (post)"
	label var out_rate_change "Change in out-migration rate (per 100K)"
	label var orig_pop_avg "Origin county avg population (for in-migration rate)"
	label var in_pre "In-migration to Multnomah (2018-2019)"
	label var in_post "In-migration to Multnomah (2021-2022)"
	label var in_rate_pre "In-migration rate per 100K origin pop (pre)"
	label var in_rate_post "In-migration rate per 100K origin pop (post)"
	label var in_rate_change "Change in in-migration rate (per 100K)"
	label var net_rate_change "Net rate change: in_change - out_change"

	** Save Stata dataset
	save "${data}working/multnomah_partner_flows_`measure'.dta", replace

	** Export to CSV for R mapping
	export delimited using "${data}working/multnomah_partner_flows_`measure'.csv", replace

	** Display summary statistics
	dis ""
	dis "Summary for `measure' - Partner-normalized rates (per 100K):"
	summ out_rate_pre out_rate_post out_rate_change
	summ in_rate_pre in_rate_post in_rate_change

	** Top 10 destinations for out-migration from Multnomah
	dis ""
	dis "Top 10 destinations for out-migration (by post rate):"
	gsort -out_rate_post
	list fips out_rate_post out_rate_change in 1/10

	** Top 10 origins for in-migration to Multnomah
	dis ""
	dis "Top 10 origins for in-migration (by post rate):"
	gsort -in_rate_post
	list fips in_rate_post in_rate_change in 1/10

	** Restore original data for next measure
	restore

} // END MEASURE LOOP

dis ""
dis "Partner-normalized flow datasets created successfully!"
dis "Files saved to: ${data}working/multnomah_partner_flows_*.csv"


** =============================================================================
** TABLE 1: County Characteristics and Tax Rates in the Portland MSA
** =============================================================================

dis ""
dis "=============================================="
dis "Creating Table 1: County Characteristics"
dis "=============================================="

** Prepare BEA per capita income for 2020
use ${data}working/bea_economics.dta, clear
keep if year == 2020
keep fips per_capita_income
tempfile bea_2020
save `bea_2020'

** Load demographics (2020 Census + ACS 2015-19 median income)
use ${data}working/demographics_2020.dta, clear

** Keep study counties
** Group 1: Multnomah (41051)
** Group 2: Washington (41067), Clackamas (41005)
** Group 3: Marion (41047), Yamhill (41071), Columbia (41009)
** Group 4: Clark (53011), Skamania (53059)
keep if inlist(fips, 41051, 41067, 41005, 41047, 41071, 41009, 53011, 53059)

** Merge BEA per capita income (2020)
merge 1:1 fips using `bea_2020', keep(master match) nogen

** Create group variable
gen group = .
replace group = 1 if fips == 41051
replace group = 2 if inlist(fips, 41067, 41005)
replace group = 3 if inlist(fips, 41047, 41071, 41009)
replace group = 4 if inlist(fips, 53011, 53059)

** Create sort order within groups
gen sort_order = .
replace sort_order = 1 if fips == 41051
replace sort_order = 2 if fips == 41067
replace sort_order = 3 if fips == 41005
replace sort_order = 4 if fips == 41047
replace sort_order = 5 if fips == 41071
replace sort_order = 6 if fips == 41009
replace sort_order = 7 if fips == 53011
replace sort_order = 8 if fips == 53059

sort sort_order

** Create short county names (strip " County")
gen county_short = subinstr(county_name, " County", "", .)

** Compute combined marginal tax rates (single filer, 2021+)
** Oregon state income tax: 9.9% on income > $125K (single)
** Metro SHS: 1.0% on income > $125K (single) — Multnomah, Washington, Clackamas
** PFA bracket 1: 1.5% on income > $125K (single) — Multnomah only
** PFA bracket 2: 3.0% on income > $200K (single) — Multnomah only

** Tax rate at $150K (single): above all $125K thresholds, below $200K PFA bracket 2
gen tax_150k = 0
replace tax_150k = 9.9 						if group <= 3  		// OR state
replace tax_150k = tax_150k + 1.0 				if group <= 2  		// Metro SHS
replace tax_150k = tax_150k + 1.5 				if group == 1  		// PFA bracket 1

** Tax rate at $300K (single): above all thresholds including PFA bracket 2
gen tax_300k = 0
replace tax_300k = 9.9 						if group <= 3		// OR state
replace tax_300k = tax_300k + 1.0 				if group <= 2		// Metro SHS
replace tax_300k = tax_300k + 3.0 				if group == 1		// PFA bracket 2

** Display for verification
list county_short population median_income per_capita_income ///
	tax_150k tax_300k, sep(0)

** ---- Export LaTeX Table 1 ----
tempname fh
file open `fh' using "${results}tables/table1.tex", write replace

file write `fh' "% Table 1: County Characteristics and Tax Rates" _n
file write `fh' "% Generated by 02_descriptives.do" _n
file write `fh' "% Requires: \usepackage{booktabs, threeparttable}" _n
file write `fh' `"\begin{table}[htbp]"' _n
file write `fh' `"\centering"' _n
file write `fh' `"\begin{threeparttable}"' _n
file write `fh' `"\caption{County Characteristics and Tax Rates in the Portland MSA}"' _n
file write `fh' `"\label{tab:county_chars}"' _n
file write `fh' `"\small"' _n
file write `fh' `"\resizebox{\textwidth}{!}{%"' _n
file write `fh' `"\begin{tabular}{lrrccc}"' _n
file write `fh' `"\toprule"' _n
file write `fh' `" & & Median HH & Per Capita & \multicolumn{2}{c}{Marginal Tax Rate} \\"' _n
file write `fh' `"\cmidrule(lr){5-6}"' _n
file write `fh' `"County & Population & Income & Income & \$150K & \$300K \\"' _n
file write `fh' `"\midrule"' _n

** Loop over counties in sort order
forvalues i = 1/8 {

	** Get values for this row
	local cname = county_short[`i']
	local pop_raw = population[`i']
	local medinc_raw = median_income[`i']
	local pci_raw = per_capita_income[`i']
	local t150 = tax_150k[`i']
	local t300 = tax_300k[`i']
	local g = group[`i']

	** Format population with commas
	local pop : di %12.0fc `pop_raw'
	local pop = strtrim("`pop'")

	** Format median income with commas and dollar sign
	local medinc : di %12.0fc `medinc_raw'
	local medinc = "\$" + strtrim("`medinc'")

	** Format per capita income with commas and dollar sign
	local pci : di %12.0fc `pci_raw'
	local pci = "\$" + strtrim("`pci'")

	** Format tax rates
	local t150_fmt : di %4.1f `t150'
	local t300_fmt : di %4.1f `t300'
	local t150_str = strtrim("`t150_fmt'") + "\%"
	local t300_str = strtrim("`t300_fmt'") + "\%"

	** Handle zero tax (WA counties)
	if `t150' == 0 {
		local t150_str "---"
		local t300_str "---"
	}

	** Write group headers before first county in each group
	if `i' == 1 {
		file write `fh' `"\textit{Multnomah County (State + Metro + PFA)} & & & & & \\"' _n
	}
	if `i' == 2 {
		file write `fh' `"\addlinespace"' _n
		file write `fh' `"\textit{Metro counties (State + Metro)} & & & & & \\"' _n
	}
	if `i' == 4 {
		file write `fh' `"\addlinespace"' _n
		file write `fh' `"\textit{Other Oregon counties (State only)} & & & & & \\"' _n
	}
	if `i' == 7 {
		file write `fh' `"\addlinespace"' _n
		file write `fh' `"\textit{Washington State counties (no income tax)} & & & & & \\"' _n
	}

	** Write data row
	file write `fh' `"\quad `cname' & `pop' & `medinc' & `pci' & `t150_str' & `t300_str' \\"' _n
}

file write `fh' `"\bottomrule"' _n
file write `fh' `"\end{tabular}%"' _n
file write `fh' `"}"' _n
file write `fh' `"\begin{tablenotes}[flushleft]"' _n
file write `fh' `"\small"' _n
file write `fh' `"\item \textit{Notes:} Population from 2020 Decennial Census. Median household income from ACS 2015--2019 5-year estimates. Per capita income from BEA CAINC1 (2020). Marginal tax rates shown for a single filer at the indicated income level (2021+). Oregon state income tax: 9.9\% on income above \$125K. Metro Supportive Housing Services tax: 1\% on income above \$125K (Multnomah, Washington, Clackamas counties). Preschool for All (PFA) tax: 1.5\% on income above \$125K, rising to 3\% above \$200K (Multnomah County only). Married filing jointly thresholds are \$200K (Metro, PFA bracket 1) and \$400K (PFA bracket 2). Washington State has no personal income tax."' _n
file write `fh' `"\end{tablenotes}"' _n
file write `fh' `"\end{threeparttable}"' _n
file write `fh' `"\end{table}"' _n

file close `fh'

dis "LaTeX table exported to: ${results}tables/table1.tex"
if ${overleaf} == 1 {
	copy "${results}tables/table1.tex" "${ol_tab}table1.tex", replace
}

** Close log
clear
log close log_02
