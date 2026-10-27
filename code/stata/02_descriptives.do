/*******************************************************************************
File Name:      02_descriptives.do
Creator:        John Iselin
Last Modified:  2026-10-26 (dropped the legacy Table 1 "Portland MSA" and
                Table 2 "Multnomah + neighbors" writers, superseded by
                table1_combined.tex and the elasticity tables)

Called by:      00_multnomah.do

Purpose:        Perform descriptive analysis. Two phases:
                  (i)  Pre-SDID descriptives reading raw cleaned inputs
                       (irs_county_gross, acs_county_gross_25plus, etc.) --
                       produces flow comparisons consumed by R/map_code.R,
                       the county/state migration-comparison tables, and
                       stringency KDPs.
                  (ii) Table 1 (combined): Multnomah vs. 5 SDID donor pools,
                       reading sdid_analysis_data.dta. Skipped cleanly if
                       that file is missing.

Outputs:
- acs_county_sample.xlsx: county flags for R/map_code.R's US pool map
  (fips, sample_migrants_in, sample_migrants_out): counties in the top 80% of
  the cumulative ACS migrant mass to / from Multnomah -- the rule the retired
  code/old/02_multi_analysis.do used. Nothing wrote this file after that code
  was archived, so the maps stage was silently skipped.

- multnomah_partner_flows_[n1|n2|agi].dta/csv: Partner-normalized flows for maps:
  - Raw flows (out_pre, out_post, in_pre, in_post)
  - Out-migration rates per 100K of DESTINATION county population
  - In-migration rates per 100K of ORIGIN county population
  - Rate changes (out_rate_change, in_rate_change, net_rate_change)
  - Used by R/map_code.R for the flow-delta, rate-change and directional maps
    (this replaced the former multnomah_flow_comparison_* output)

Authors: John Iselin

For more information, contact john.iselin@yale.edu

*******************************************************************************/

** Load shared project defaults and helper programs
if "${dir}" == "" {
    local _cwd = subinstr("`c(pwd)'", "\", "/", .)
    if regexm("`_cwd'", "(.*)/code/(stata|utils)$") global dir = regexs(1)
    else global dir "`_cwd'"
}
do "${dir}/code/utils/globals.do"

** Overleaf sync (ol_fig / ol_tab / overleaf) is resolved in globals.do from
** user_settings.do, so it is already set whether run standalone or orchestrated.

** Start log file
capture log close log_02
log using "${logs}02_log_descriptives_${date}", replace text name(log_02)
project_set_seed, context("02_descriptives.do") offset(20)

** Ensure output dir exists (supports standalone runs outside the orchestrator)
capture mkdir "${results}tables"

** plotplainblind palette (RGB) — consistent across all figures
local col_out  "0 114 178"    // sea (p7) — out-migration
local col_in   "213 94 0"     // vermillion (p6) — in-migration
local col_mult "230 159 0"    // orangebrown (p8) — Multnomah/Oregon highlight
local col_ref  "153 153 153"  // gs10 (p2) — reference lines

** ACS county flows tagged for Multnomah: build_acs_county_sample (below)
** reads them from memory.
use ${data}working/acs_county_flow.dta, clear

** Tag Multnomah
gen multnomah_o = (state_fips_o == 41 & county_fips_o == 51)
gen multnomah_d = (state_fips_d == 41 & county_fips_d == 51)

** County flags for R/map_code.R's US pool map (build_acs_county_sample in
** programs.do). The data in memory is still acs_county_flow with the tags.
build_acs_county_sample, saving("${data}working/acs_county_sample.xlsx")


/*******************************************************************************
SECTION 2: PARTNER-COUNTY NORMALIZED FLOW MAPS
Purpose: Create flow data normalized by partner county population (per 100K)
         for directional flow maps showing Multnomah's migration patterns.

Out-flow map: Rate of migration FROM Multnomah TO each destination county
              normalized by DESTINATION county average population
In-flow map:  Rate of migration TO Multnomah FROM each origin county
              normalized by ORIGIN county average population

Outputs:
- multnomah_partner_flows_[n1|n2|agi].csv: Flow data with partner normalization
*******************************************************************************/

********************************************************************************
** GET COUNTY POPULATIONS FOR RATE CALCULATIONS
********************************************************************************

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

********************************************************************************
** LOAD FLOW DATA AND CREATE PARTNER-NORMALIZED DATASETS
********************************************************************************

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

	********************************************************************************
	** OUT-MIGRATION: Flows FROM Multnomah TO other counties
	********************************************************************************

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

	********************************************************************************
	** IN-MIGRATION: Flows TO Multnomah FROM other counties
	********************************************************************************

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

	********************************************************************************
	** MERGE: Combine in and out flows
	********************************************************************************

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


********************************************************************************
** TABLE: County-Level Migration Comparison (Multnomah vs Distribution)
********************************************************************************

dis ""
dis "=============================================="
dis "Creating County-Level Migration Comparison Table"
dis "=============================================="

** Load county gross migration data
use "${data}working/irs_county_gross", clear

** Keep only relevant years (pre: 2018-2019, post: 2021-2022)
keep if inlist(year, 2018, 2019, 2021, 2022)

** Assign period
gen period = ""
replace period = "pre" if inlist(year, 2018, 2019)
replace period = "post" if inlist(year, 2021, 2022)

** Drop suppressed counties (county_fips == 0)
drop if county_fips == 0

** Calculate base population (non-movers + all movers)
gen n1_base = n1_out_1 + n1_out_2
gen n2_base = n2_out_1 + n2_out_2
gen agi_base = agi_out_1 + agi_out_2

** Calculate rates as percentage of base
foreach x in "n1" "n2" "agi" {
	gen `x'_out_rate = 100 * `x'_out_3 / `x'_base
	gen `x'_in_rate  = 100 * `x'_in_3  / `x'_base
	gen `x'_net_rate = 100 * `x'_net_3 / `x'_base
}

** Collapse to county-period averages
collapse (mean) n1_out_rate n1_in_rate n1_net_rate ///
				n2_out_rate n2_in_rate n2_net_rate ///
				agi_out_rate agi_in_rate agi_net_rate, ///
	by(fips period)

** Identify Multnomah
gen multnomah = fips == 41051

********************************************************************************
** BOX PLOTS: County Migration Rate Distributions (twoway)
********************************************************************************

dis ""
dis "=============================================="
dis "Creating Box Plots: Migration Rate Distributions"
dis "=============================================="

preserve

** Rename rate variables for reshape (direction: 1=out, 2=in, 3=net)
foreach x in "n1" "n2" "agi" {
	rename `x'_out_rate `x'_rate1
	rename `x'_in_rate  `x'_rate2
	rename `x'_net_rate `x'_rate3
}

** Reshape to long: one row per county-period-direction
reshape long n1_rate n2_rate agi_rate, i(fips period) j(direction)

** Create group variable: direction x period (sequential 1-6)
gen group = .
replace group = 1 if direction == 1 & period == "pre"
replace group = 2 if direction == 1 & period == "post"
replace group = 3 if direction == 2 & period == "pre"
replace group = 4 if direction == 2 & period == "post"
replace group = 5 if direction == 3 & period == "pre"
replace group = 6 if direction == 3 & period == "post"

** Jittered x-position for scatter dots (Version B)
project_set_seed, context("02_descriptives.do panel scatter") offset(20)
gen group_jit = group + (uniform() - 0.5) * 0.4

** Endpoints for median line segments (match barwidth of 0.5)
gen group_lo = group - 0.25
gen group_hi = group + 0.25

** Loop over measures
foreach x in "n1" "n2" "agi" {

	if "`x'" == "n1" local mtitle "Tax Returns"
	else if "`x'" == "n2" local mtitle "Exemptions"
	else if "`x'" == "agi" local mtitle "Adjusted Gross Income"

	** ---- Compute box plot statistics by group ----
	capture drop p25 p50 p75 lo_adj hi_adj in_fence tag

	gen p25 = .
	gen p50 = .
	gen p75 = .
	gen lo_adj = .
	gen hi_adj = .

	forvalues g = 1/6 {
		qui _pctile `x'_rate if group == `g', p(25 50 75)
		local q25 = r(r1)
		local q50 = r(r2)
		local q75 = r(r3)
		local iq  = `q75' - `q25'

		replace p25 = `q25' if group == `g'
		replace p50 = `q50' if group == `g'
		replace p75 = `q75' if group == `g'

		** Adjacent values: most extreme data within 1.5 * IQR of box
		qui summ `x'_rate if group == `g' & `x'_rate >= (`q25' - 1.5 * `iq')
		replace lo_adj = r(min) if group == `g'

		qui summ `x'_rate if group == `g' & `x'_rate <= (`q75' + 1.5 * `iq')
		replace hi_adj = r(max) if group == `g'
	}

	** Flag non-outlier observations (within 1.5 * IQR fences)
	gen in_fence = (`x'_rate >= (p25 - 1.5 * (p75 - p25)) & ///
					`x'_rate <= (p75 + 1.5 * (p75 - p25)))

	** Tag one obs per group for box/whisker elements
	egen tag = tag(group)

	** ---- Version A: Box + Multnomah only ----
	twoway ///
		(rcap lo_adj hi_adj group if tag, lcolor(gs8)) ///
		(rbar p25 p75 group if tag, ///
			bfcolor(gs14) blcolor(gs8) barwidth(0.5)) ///
		(pcspike p50 group_lo p50 group_hi if tag, ///
			lcolor(gs2) lwidth(medthick)) ///
		(scatter `x'_rate group if multnomah == 1, ///
			mcolor("`col_mult'") msize(med) msymbol(D)) ///
		, title("County Rates: `mtitle'") ///
		subtitle("Multnomah vs. U.S. counties") ///
		  ytitle("Migration rate (% of base population)") ///
		  xtitle("") ///
		  xlabel(1 `""Out" "2018-19""' 2 `""Out" "2021-22""' ///
				 3 `""In" "2018-19""' 4 `""In" "2021-22""' ///
				 5 `""Net" "2018-19""' 6 `""Net" "2021-22""', ///
				 labsize(small)) ///
		  xscale(range(0.5 6.5)) ///
		  legend(off) ///
		  graphregion(color(white))

	graph export "${results}flows/fig_strip_`x'_mult.png", replace

	project_save_overleaf "${results}flows/fig_strip_`x'_mult.png"

	** ---- Version B: All counties (grey dots) + Multnomah overlay ----
	twoway ///
		(scatter `x'_rate group_jit if multnomah == 0 , ///
			mcolor(gs12) msize(vsmall) msymbol(oh)) ///
		(rcap lo_adj hi_adj group if tag, lcolor(gs8)) ///
		(rbar p25 p75 group if tag, ///
			bfcolor(gs14%50) blcolor(gs8) barwidth(0.5)) ///
		(pcspike p50 group_lo p50 group_hi if tag, ///
			lcolor(gs2) lwidth(medthick)) ///
		(scatter `x'_rate group if multnomah == 1, ///
			mcolor("`col_mult'") msize(med) msymbol(D)) ///
		, title("County Rates: `mtitle'") ///
		subtitle("Multnomah vs. all U.S. counties") ///
		  ytitle("Migration rate (% of base population)") ///
		  xtitle("") ///
		  xlabel(1 `""Out" "2018-19""' 2 `""Out" "2021-22""' ///
				 3 `""In" "2018-19""' 4 `""In" "2021-22""' ///
				 5 `""Net" "2018-19""' 6 `""Net" "2021-22""', ///
				 labsize(small)) ///
		  xscale(range(0.5 6.5)) ///
		  legend(off) ///
		  graphregion(color(white))

	graph export "${results}flows/fig_strip_`x'_all.png", replace

	project_save_overleaf "${results}flows/fig_strip_`x'_all.png"

	** Clean up for next measure
	drop p25 p50 p75 lo_adj hi_adj in_fence tag

} // END MEASURE LOOP

restore

** Compute percentiles of the distribution (excluding Multnomah) by period
foreach x in "n1" "n2" "agi" {
	foreach y in "out" "in" "net" {
		foreach p in "pre" "post" {
			** Percentiles from non-Multnomah counties
			qui summ `x'_`y'_rate if multnomah == 0 & period == "`p'", de
			local `x'_`y'_p25_`p' = r(p25)
			local `x'_`y'_p50_`p' = r(p50)
			local `x'_`y'_p75_`p' = r(p75)

			** Multnomah value
			qui summ `x'_`y'_rate if multnomah == 1 & period == "`p'"
			local `x'_`y'_mult_`p' = r(mean)
		}
	}
}

** Save table locally and to Overleaf
local _dests `""${results}tables/table_migration_county.tex""'

foreach _outfile of local _dests {

tempname fh
file open `fh' using "`_outfile'", write replace

file write `fh' "% Table: County-Level Migration Rate Comparison" _n
file write `fh' "% Generated by 02_descriptives.do" _n
file write `fh' "% Requires: \usepackage{booktabs, threeparttable}" _n
file write `fh' `"\begin{table}[htbp]"' _n
file write `fh' `"\centering"' _n
file write `fh' `"\caption{County-Level Migration Rates: Multnomah vs. National Distribution}"' _n
file write `fh' `"\label{tab:migration_county}"' _n
file write `fh' `"\begin{threeparttable}"' _n
file write `fh' `"\small"' _n
file write `fh' `"\begin{tabular}{lcccccccc}"' _n
file write `fh' `"\toprule"' _n
file write `fh' `" & \multicolumn{4}{c}{2018--2019} & \multicolumn{4}{c}{2021--2022} \\"' _n
file write `fh' `"\cmidrule(lr){2-5} \cmidrule(lr){6-9}"' _n
file write `fh' `" & Mult. & Median & 25th & 75th & Mult. & Median & 25th & 75th \\"' _n
file write `fh' `"\midrule"' _n

** Write rows for each measure x direction
foreach x in "n1" "n2" "agi" {

	if "`x'" == "n1" local xlabel "Returns"
	else if "`x'" == "n2" local xlabel "Exemptions"
	else if "`x'" == "agi" local xlabel "AGI"

	file write `fh' `"\textit{`xlabel'} & & & & & & & & \\"' _n

	foreach y in "out" "in" "net" {

		if "`y'" == "out" local ylabel "Out-migration"
		else if "`y'" == "in" local ylabel "In-migration"
		else if "`y'" == "net" local ylabel "Net migration"

		local m_pre  : di %5.2f ``x'_`y'_mult_pre'
		local p50_pre : di %5.2f ``x'_`y'_p50_pre'
		local p25_pre : di %5.2f ``x'_`y'_p25_pre'
		local p75_pre : di %5.2f ``x'_`y'_p75_pre'
		local m_post  : di %5.2f ``x'_`y'_mult_post'
		local p50_post : di %5.2f ``x'_`y'_p50_post'
		local p25_post : di %5.2f ``x'_`y'_p25_post'
		local p75_post : di %5.2f ``x'_`y'_p75_post'

		file write `fh' `"\quad `ylabel' & `m_pre' & `p50_pre' & `p25_pre' & `p75_pre' & `m_post' & `p50_post' & `p25_post' & `p75_post' \\"' _n
	}

	file write `fh' `"\addlinespace"' _n
}

file write `fh' `"\bottomrule"' _n
file write `fh' `"\end{tabular}"' _n
file write `fh' `"\begin{tablenotes}[flushleft]"' _n
file write `fh' `"\small"' _n
file write `fh' `"\item \textit{Notes:} Migration rates as a percentage of the base filing population (non-movers plus all movers). Pre-period averages 2018--2019; post-period averages 2021--2022. Distribution statistics computed across all U.S.\ counties excluding Multnomah. Source: IRS Statistics of Income."' _n
file write `fh' `"\end{tablenotes}"' _n
file write `fh' `"\end{threeparttable}"' _n
file write `fh' `"\end{table}"' _n

file close `fh'

} // end foreach _outfile (table_migration_county)
project_save_overleaf "${results}tables/table_migration_county.tex"
dis "Migration county table exported to: ${results}tables/table_migration_county.tex"

clear

********************************************************************************
** TABLE: State-Level Migration Comparison (Oregon vs Distribution)
********************************************************************************

dis ""
dis "=============================================="
dis "Creating State-Level Migration Comparison Table"
dis "=============================================="

** Load state gross migration data
use "${data}working/irs_state_gross", clear

** Keep only relevant years
keep if inlist(year, 2018, 2019, 2021, 2022)

** Assign period
gen period = ""
replace period = "pre" if inlist(year, 2018, 2019)
replace period = "post" if inlist(year, 2021, 2022)

** Drop territories (state_fips > 56)
drop if state_fips > 56

** Calculate base population (non-movers + all movers)
gen n1_base = n1_out_1 + n1_out_2
gen n2_base = n2_out_1 + n2_out_2
gen agi_base = agi_out_1 + agi_out_2

** Calculate rates
foreach x in "n1" "n2" "agi" {
	gen `x'_out_rate = 100 * `x'_out_3 / `x'_base
	gen `x'_in_rate  = 100 * `x'_in_3  / `x'_base
	gen `x'_net_rate = 100 * `x'_net_3 / `x'_base
}

** Collapse to state-period averages
collapse (mean) n1_out_rate n1_in_rate n1_net_rate ///
				n2_out_rate n2_in_rate n2_net_rate ///
				agi_out_rate agi_in_rate agi_net_rate, ///
	by(state_fips period)

** Identify Oregon
gen oregon = state_fips == 41

********************************************************************************
** BOX PLOTS: State Migration Rate Distributions (twoway)
********************************************************************************

dis ""
dis "=============================================="
dis "Creating Box Plots: State Migration Rate Distributions"
dis "=============================================="

preserve

** Rename rate variables for reshape (direction: 1=out, 2=in, 3=net)
foreach x in "n1" "n2" "agi" {
	rename `x'_out_rate `x'_rate1
	rename `x'_in_rate  `x'_rate2
	rename `x'_net_rate `x'_rate3
}

** Reshape to long: one row per state-period-direction
reshape long n1_rate n2_rate agi_rate, i(state_fips period) j(direction)

** Create group variable: direction x period (sequential 1-6)
gen group = .
replace group = 1 if direction == 1 & period == "pre"
replace group = 2 if direction == 1 & period == "post"
replace group = 3 if direction == 2 & period == "pre"
replace group = 4 if direction == 2 & period == "post"
replace group = 5 if direction == 3 & period == "pre"
replace group = 6 if direction == 3 & period == "post"

** Jittered x-position for scatter dots (Version B)
project_set_seed, context("02_descriptives.do appendix scatter") offset(21)
gen group_jit = group + (uniform() - 0.5) * 0.4

** Endpoints for median line segments (match barwidth of 0.5)
gen group_lo = group - 0.25
gen group_hi = group + 0.25

** Loop over measures
foreach x in "n1" "n2" "agi" {

	if "`x'" == "n1" local mtitle "Tax Returns"
	else if "`x'" == "n2" local mtitle "Exemptions"
	else if "`x'" == "agi" local mtitle "Adjusted Gross Income"

	** ---- Compute box plot statistics by group ----
	capture drop p25 p50 p75 lo_adj hi_adj in_fence tag

	gen p25 = .
	gen p50 = .
	gen p75 = .
	gen lo_adj = .
	gen hi_adj = .

	forvalues g = 1/6 {
		qui _pctile `x'_rate if group == `g', p(25 50 75)
		local q25 = r(r1)
		local q50 = r(r2)
		local q75 = r(r3)
		local iq  = `q75' - `q25'

		replace p25 = `q25' if group == `g'
		replace p50 = `q50' if group == `g'
		replace p75 = `q75' if group == `g'

		** Adjacent values: most extreme data within 1.5 * IQR of box
		qui summ `x'_rate if group == `g' & `x'_rate >= (`q25' - 1.5 * `iq')
		replace lo_adj = r(min) if group == `g'

		qui summ `x'_rate if group == `g' & `x'_rate <= (`q75' + 1.5 * `iq')
		replace hi_adj = r(max) if group == `g'
	}

	** Flag non-outlier observations (within 1.5 * IQR fences)
	gen in_fence = (`x'_rate >= (p25 - 1.5 * (p75 - p25)) & ///
					`x'_rate <= (p75 + 1.5 * (p75 - p25)))

	** Tag one obs per group for box/whisker elements
	egen tag = tag(group)

	** ---- Version A: Box + Oregon only ----
	twoway ///
		(rcap lo_adj hi_adj group if tag, lcolor(gs8)) ///
		(rbar p25 p75 group if tag, ///
			bfcolor(gs14) blcolor(gs8) barwidth(0.5)) ///
		(pcspike p50 group_lo p50 group_hi if tag, ///
			lcolor(gs2) lwidth(medthick)) ///
		(scatter `x'_rate group if oregon == 1, ///
			mcolor("`col_mult'") msize(med) msymbol(D)) ///
		, title("State Rates: `mtitle'") ///
		subtitle("Oregon vs. U.S. states") ///
		  ytitle("Migration rate (% of base population)") ///
		  xtitle("") ///
		  xlabel(1 `""Out" "2018-19""' 2 `""Out" "2021-22""' ///
				 3 `""In" "2018-19""' 4 `""In" "2021-22""' ///
				 5 `""Net" "2018-19""' 6 `""Net" "2021-22""', ///
				 labsize(small)) ///
		  xscale(range(0.5 6.5)) ///
		  legend(off) ///
		  graphregion(color(white))

	graph export "${results}flows/fig_strip_`x'_state_mult.png", replace

	project_save_overleaf "${results}flows/fig_strip_`x'_state_mult.png"

	** ---- Version B: All states (grey dots) + Oregon overlay ----
	twoway ///
		(scatter `x'_rate group_jit if oregon == 0 , ///
			mcolor(gs12) msize(vsmall) msymbol(oh)) ///
		(rcap lo_adj hi_adj group if tag, lcolor(gs8)) ///
		(rbar p25 p75 group if tag, ///
			bfcolor(gs14%50) blcolor(gs8) barwidth(0.5)) ///
		(pcspike p50 group_lo p50 group_hi if tag, ///
			lcolor(gs2) lwidth(medthick)) ///
		(scatter `x'_rate group if oregon == 1, ///
			mcolor("`col_mult'") msize(med) msymbol(D)) ///
		, title("State Rates: `mtitle'") ///
		subtitle("Oregon vs. all U.S. states") ///
		  ytitle("Migration rate (% of base population)") ///
		  xtitle("") ///
		  xlabel(1 `""Out" "2018-19""' 2 `""Out" "2021-22""' ///
				 3 `""In" "2018-19""' 4 `""In" "2021-22""' ///
				 5 `""Net" "2018-19""' 6 `""Net" "2021-22""', ///
				 labsize(small)) ///
		  xscale(range(0.5 6.5)) ///
		  legend(off) ///
		  graphregion(color(white))

	graph export "${results}flows/fig_strip_`x'_state_all.png", replace

	project_save_overleaf "${results}flows/fig_strip_`x'_state_all.png"

	** Clean up for next measure
	drop p25 p50 p75 lo_adj hi_adj in_fence tag

} // END MEASURE LOOP

restore

** Compute percentiles of distribution (excluding Oregon) by period
foreach x in "n1" "n2" "agi" {
	foreach y in "out" "in" "net" {
		foreach p in "pre" "post" {
			qui summ `x'_`y'_rate if oregon == 0 & period == "`p'", de
			local `x'_`y'_p25_`p' = r(p25)
			local `x'_`y'_p50_`p' = r(p50)
			local `x'_`y'_p75_`p' = r(p75)

			qui summ `x'_`y'_rate if oregon == 1 & period == "`p'"
			local `x'_`y'_or_`p' = r(mean)
		}
	}
}

** Save table locally and to Overleaf
local _dests `""${results}tables/table_migration_state.tex""'

foreach _outfile of local _dests {

tempname fh
file open `fh' using "`_outfile'", write replace

file write `fh' "% Table: State-Level Migration Rate Comparison" _n
file write `fh' "% Generated by 02_descriptives.do" _n
file write `fh' "% Requires: \usepackage{booktabs, threeparttable}" _n
file write `fh' `"\begin{table}[htbp]"' _n
file write `fh' `"\centering"' _n
file write `fh' `"\caption{State-Level Migration Rates: Oregon vs. National Distribution}"' _n
file write `fh' `"\label{tab:migration_state}"' _n
file write `fh' `"\begin{threeparttable}"' _n
file write `fh' `"\small"' _n
file write `fh' `"\begin{tabular}{lcccccccc}"' _n
file write `fh' `"\toprule"' _n
file write `fh' `" & \multicolumn{4}{c}{2018--2019} & \multicolumn{4}{c}{2021--2022} \\"' _n
file write `fh' `"\cmidrule(lr){2-5} \cmidrule(lr){6-9}"' _n
file write `fh' `" & Oregon & Median & 25th & 75th & Oregon & Median & 25th & 75th \\"' _n
file write `fh' `"\midrule"' _n

foreach x in "n1" "n2" "agi" {

	if "`x'" == "n1" local xlabel "Returns"
	else if "`x'" == "n2" local xlabel "Exemptions"
	else if "`x'" == "agi" local xlabel "AGI"

	file write `fh' `"\textit{`xlabel'} & & & & & & & & \\"' _n

	foreach y in "out" "in" "net" {

		if "`y'" == "out" local ylabel "Out-migration"
		else if "`y'" == "in" local ylabel "In-migration"
		else if "`y'" == "net" local ylabel "Net migration"

		local m_pre  : di %5.2f ``x'_`y'_or_pre'
		local p50_pre : di %5.2f ``x'_`y'_p50_pre'
		local p25_pre : di %5.2f ``x'_`y'_p25_pre'
		local p75_pre : di %5.2f ``x'_`y'_p75_pre'
		local m_post  : di %5.2f ``x'_`y'_or_post'
		local p50_post : di %5.2f ``x'_`y'_p50_post'
		local p25_post : di %5.2f ``x'_`y'_p25_post'
		local p75_post : di %5.2f ``x'_`y'_p75_post'

		file write `fh' `"\quad `ylabel' & `m_pre' & `p50_pre' & `p25_pre' & `p75_pre' & `m_post' & `p50_post' & `p25_post' & `p75_post' \\"' _n
	}

	file write `fh' `"\addlinespace"' _n
}

file write `fh' `"\bottomrule"' _n
file write `fh' `"\end{tabular}"' _n
file write `fh' `"\begin{tablenotes}[flushleft]"' _n
file write `fh' `"\small"' _n
file write `fh' `"\item \textit{Notes:} Migration rates as a percentage of the base filing population (non-movers plus all movers). Pre-period averages 2018--2019; post-period averages 2021--2022. Distribution statistics computed across all U.S.\ states excluding Oregon. Source: IRS Statistics of Income."' _n
file write `fh' `"\end{tablenotes}"' _n
file write `fh' `"\end{threeparttable}"' _n
file write `fh' `"\end{table}"' _n

file close `fh'

} // end foreach _outfile (table_migration_state)
project_save_overleaf "${results}tables/table_migration_state.tex"
dis "Migration state table exported to: ${results}tables/table_migration_state.tex"


********************************************************************************
** COVID STRINGENCY KERNEL DENSITY PLOTS
** Shows Multnomah County's position in the national distribution of
** JII restriction-duration measures (days).
********************************************************************************

** Create output directory
capture mkdir "${results}stringency"

** Load JII stringency data
use "${data}working/jii_stringency.dta", clear

** Tag Multnomah
gen multnomah = fips == 41051

** Store Multnomah values
foreach v in msahodays restclosedays gatherbandays strictgatherbandays maskpubdays {
	qui summ `v' if multnomah == 1
	local mult_`v' = r(mean)

	** Compute percentile rank
	qui count if `v' <= `mult_`v'' & !missing(`v')
	local n_below = r(N)
	qui count if !missing(`v')
	local n_total = r(N)
	local pctile_`v' = round(`n_below' / `n_total' * 100, 0.1)
}

** Individual kernel density plots
local var1 "msahodays"
local var2 "restclosedays"
local var3 "gatherbandays"
local var4 "strictgatherbandays"
local var5 "maskpubdays"

local lbl1 "Stay-at-Home Order (days)"
local lbl2 "Restaurant Closure (days)"
local lbl3 "Gathering Ban (days)"
local lbl4 "Strict Gathering Ban (days)"
local lbl5 "Public Mask Mandate (days)"

forvalues i = 1/5 {
	local v "`var`i''"
	local l "`lbl`i''"

	twoway 	(kdensity `v', lc("`col_out'") lw(medthick)),						///
		xline(`mult_`v'', lc("`col_mult'") lp(dash) lw(medthick))				///
		xtitle("`l'")															///
		ytitle("Density")														///
		subtitle("Multnomah: `mult_`v'' days | `pctile_`v''th pctile")		///
		graphregion(color(white))												///
		name(kd_`v', replace)

	graph export "${results}stringency/fig_stringency_`v'.png",					///
		as(png) width(1200) replace
}

** Combined 5-panel figure
graph combine kd_msahodays kd_restclosedays kd_gatherbandays					///
	kd_strictgatherbandays kd_maskpubdays,										///
	cols(3)																		///
	title("COVID Policy Stringency")											///
	graphregion(color(white))													///
	name(kd_combined, replace)

graph export "${results}stringency/fig_stringency_panel.png",					///
	as(png) width(2400) replace
graph export "${results}stringency/fig_stringency_panel.pdf", replace

** Clean up
graph drop kd_msahodays kd_restclosedays kd_gatherbandays						///
	kd_strictgatherbandays kd_maskpubdays kd_combined

dis "Stringency kernel density plots saved to: ${results}stringency/"


********************************************************************************
** TABLE 1 (COMBINED): Multnomah vs. SDID donor-pool comparison groups
********************************************************************************
**
** Two-panel structure -- Panel A = IRS, Panel B = ACS College -- each with
** 6 rows (Multnomah + the 5 donor pools defined in 02_sdid_analysis.do)
** and 8 numeric columns (N counties, out pre/post, in pre/post, net pre/post,
** net change). Folded in from the former 02_descriptives_supp.do (item 8 of
** May 2026 paper revision).
**
** Pre/post windows:
**   IRS  : pre = 2018-2019, post = 2021-2022 (drop 2020).
**   ACS  : pre = 2018-2019, post = 2021-2024 (drop 2020) -- ACS has
**          an extra two years post-treatment.
**
** Inputs:  ${data}working/sdid_analysis_data.dta (built by 02_sdid_analysis.do)
** Outputs: ${results}tables/table1_combined.tex
**          ${ol_tab}table1_combined.tex          (if ${overleaf}==1)

dis ""
dis "=============================================="
dis "Building Table 1: Multnomah vs. comparison groups"
dis "=============================================="

** Skip cleanly if SDID prep hasn't run yet -- enables 02_descriptives.do to
** still execute its earlier sections in the rare case the orchestrator runs
** Stage 2 alone.
capture confirm file "${data}working/sdid_analysis_data.dta"
if _rc {
    dis as error "  Skipping Table 1 build: sdid_analysis_data.dta not found."
    dis as error "  Run 02_sdid_analysis.do first; then re-run 02_descriptives.do."
}
else {

use "${data}working/sdid_analysis_data.dta", clear

** Multnomah identifier (defensive)
capture confirm variable multnomah
if _rc {
    gen byte multnomah = (state_fips == 41 & county_fips == 51)
}

** ---- Define pool list (consistent across panels) ----
** Order: Multnomah first, then 6 donor pools.
local pool_list "mult sample_all sample_urban95 sample_urban75_covid sample_demog sample_stringency sample_narrow"
local pool_label_mult                  "Multnomah"
local pool_label_sample_all            "All donor counties (mean)"
local pool_label_sample_urban95        "Urban top-5\% (mean)"
local pool_label_sample_urban75_covid  "Urban top-25\%, Covid match (mean)"
local pool_label_sample_demog          "Demographic match (mean)"
local pool_label_sample_stringency     "Stringency match (mean)"
local pool_label_sample_narrow         "Narrow similar-cities pool (mean)"

** Donor-pool conditions exclude Multnomah so the per-pool N counts and
** means describe donors only (Multnomah is reported separately on its
** own row).
local cond_mult                  "multnomah == 1"
local cond_sample_all            "sample_all == 1            & multnomah == 0"
local cond_sample_urban95        "sample_urban95 == 1        & multnomah == 0"
local cond_sample_urban75_covid  "sample_urban75_covid == 1  & multnomah == 0"
local cond_sample_demog          "sample_demog == 1          & multnomah == 0"
local cond_sample_stringency     "sample_stringency == 1     & multnomah == 0"
local cond_sample_narrow         "sample_narrow == 1         & multnomah == 0"

** Build one matrix per data source (7 rows x 8 cols):
**   Cols: 1=N, 2=out_pre, 3=out_post, 4=in_pre, 5=in_post, 6=net_pre, 7=net_post, 8=net_chg
**
** Four panels: IRS (irs), IRS restricted to ACS-identified counties
** (irs_389; IRS data filtered to the balanced ACS panel via
** _balanced_acs), ACS all-25+ (acs1), ACS College (acs2). Each uses
** the same five donor-pool definitions plus narrow. IRS sees ~3,140
** counties; the ACS public-use file identifies ~389 counties, of
** which ~337 (336 donors + Multnomah) satisfy our balanced-panel and
** state-drop restrictions and appear in Panels B-D. This is the same
** county universe used by the IRS-389, ACS-all, and ACS-college SDID
** specifications.
tempname M_IRS M_IRS_389 M_ACS1 M_ACS2
foreach m in `M_IRS' `M_IRS_389' `M_ACS1' `M_ACS2' {
    matrix `m' = J(7, 8, .)
    matrix colnames `m' = N out_pre out_post in_pre in_post net_pre net_post net_chg
    matrix rownames `m' = mult all urban95 urban_covid demog stringency narrow
}

** Build per-panel observability flags from a single year-snapshot.
** has_<src> = 1 iff the county has a non-missing AGI rate in that source.
**
** _balanced_acs = county-level flag: 1 iff the county is in the balanced
** ACS panel (same county set used by the IRS-389, ACS-all, and ACS-college
** SDID specifications via irs_sample_2 / acs_period_1 / acs_period_2,
** which share an identical ct_tmp == max balance + state-drop zero-out
** in 02_sdid_analysis.do). Panels B-D share this universe so their N
** counts agree and the displayed rates describe the same sample used in
** estimation.
bysort fips: egen byte _balanced_acs = max(irs_sample_2 == 1)

preserve
keep if year == 2019
gen byte has_irs  = !missing(agi_out_rate_irs)
gen byte has_acs1 = !missing(agi_out_rate_acs1)
gen byte has_acs2 = !missing(agi_out_rate_acs2)

local row = 1
foreach pool of local pool_list {
    qui count if `cond_`pool'' & has_irs == 1
    matrix `M_IRS'[`row', 1] = r(N)
    qui count if `cond_`pool'' & has_irs == 1 & _balanced_acs == 1
    matrix `M_IRS_389'[`row', 1] = r(N)
    qui count if `cond_`pool'' & has_acs1 == 1 & _balanced_acs == 1
    matrix `M_ACS1'[`row', 1] = r(N)
    qui count if `cond_`pool'' & has_acs2 == 1 & _balanced_acs == 1
    matrix `M_ACS2'[`row', 1] = r(N)
    local ++row
}
restore

** ---- IRS panel: pre = 2018-2019, post = 2021-2022 ----
** Fills two matrices in one pass: M_IRS (all IRS counties) and M_IRS_389
** (IRS counties also in the balanced ACS-identified universe via
** irs_sample_2). Same IRS migration data; different county filter.
preserve
keep if inrange(year, 2018, 2022) & year != 2020
gen byte period_post = inlist(year, 2021, 2022)

local row = 1
foreach pool of local pool_list {
    foreach dir in "out" "in" "net" {
        local col_off = cond("`dir'" == "out", 1, cond("`dir'" == "in", 3, 5))
        foreach per in 0 1 {
            local col = `col_off' + 1 + `per'

            qui summ agi_`dir'_rate_irs if `cond_`pool'' & period_post == `per'
            matrix `M_IRS'[`row', `col'] = r(mean)

            qui summ agi_`dir'_rate_irs if `cond_`pool'' & period_post == `per' & _balanced_acs == 1
            matrix `M_IRS_389'[`row', `col'] = r(mean)
        }
    }
    matrix `M_IRS'[`row', 8]     = `M_IRS'[`row', 7]     - `M_IRS'[`row', 6]
    matrix `M_IRS_389'[`row', 8] = `M_IRS_389'[`row', 7] - `M_IRS_389'[`row', 6]
    local ++row
}
restore

** ---- ACS panels: pre = 2018-2019, post = 2021-2024.
**      Loop over (matrix, suffix) pairs to fill all-25+ and college panels.
foreach acs_pair in "M_ACS1 acs1" "M_ACS2 acs2" {
    local matname  : word 1 of `acs_pair'
    local src      : word 2 of `acs_pair'

    preserve
    keep if inrange(year, 2018, 2024) & year != 2020
    gen byte period_post = inrange(year, 2021, 2024)

    local row = 1
    foreach pool of local pool_list {
        foreach dir in "out" "in" "net" {
            local col_off = cond("`dir'" == "out", 1, cond("`dir'" == "in", 3, 5))
            foreach per in 0 1 {
                qui summ agi_`dir'_rate_`src' if `cond_`pool'' & period_post == `per' & _balanced_acs == 1
                local col = `col_off' + 1 + `per'
                matrix ``matname''[`row', `col'] = r(mean)
            }
        }
        matrix ``matname''[`row', 8] = ``matname''[`row', 7] - ``matname''[`row', 6]
        local ++row
    }
    restore
}

mat list `M_IRS'
mat list `M_IRS_389'
mat list `M_ACS1'
mat list `M_ACS2'

** ---- Write LaTeX table ----
local _dests `""${results}tables/table1_combined.tex""'

foreach _outfile of local _dests {

tempname fh
file open `fh' using "`_outfile'", write replace

file write `fh' "% Table 1 (combined): Migration rates by donor pool" _n
file write `fh' "% Generated by 02_descriptives.do" _n
file write `fh' "% Requires: \usepackage{booktabs, threeparttable}" _n
file write `fh' `"\begin{table}[!ht]"' _n
file write `fh' `"\setstretch{1}"' _n
file write `fh' `"\centering"' _n
file write `fh' `"\caption{AGI Migration Rates by Comparison Group: Multnomah vs.\ SDID Donor Pools}"' _n
file write `fh' `"\label{tab:multnomah_vs_groups}"' _n
file write `fh' `"\begin{threeparttable}"' _n
file write `fh' `"\footnotesize"' _n
file write `fh' `"\setlength{\tabcolsep}{3pt}"' _n
file write `fh' `"\begin{tabular}{l r c c c c c c c}"' _n
file write `fh' `"\toprule"' _n
file write `fh' `" & N & \multicolumn{2}{c}{Out-migration} & \multicolumn{2}{c}{In-migration} & \multicolumn{2}{c}{Net in-migration} & Net \\"' _n
file write `fh' `"\cmidrule(lr){3-4} \cmidrule(lr){5-6} \cmidrule(lr){7-8}"' _n
file write `fh' `" & counties & Pre & Post & Pre & Post & Pre & Post & change (pp) \\"' _n
file write `fh' `"\midrule"' _n

** Per-panel header: letter -> (tempname-LOCAL-name, header text). Storing
** the local-name string (e.g., "M_IRS") rather than the resolved tempname
** lets us double-dereference at use time: ``matname'' resolves first to
** "M_IRS", then to the actual tempname.
local matname_A "M_IRS"
local matname_B "M_IRS_389"
local matname_C "M_ACS1"
local matname_D "M_ACS2"
local hdr_A     "Panel A: IRS, all counties (Pre = 2018--2019; Post = 2021--2022)"
local hdr_B     "Panel B: IRS, restricted to ACS-identified counties (Pre = 2018--2019; Post = 2021--2022)"
local hdr_C     "Panel C: ACS, all 25+ (Pre = 2018--2019; Post = 2021--2024)"
local hdr_D     "Panel D: ACS, college-educated (Pre = 2018--2019; Post = 2021--2024)"

foreach letter in A B C D {
    local matname  "`matname_`letter''"
    local panel_hdr "`hdr_`letter''"

    if "`letter'" != "A" {
        file write `fh' `"\midrule"' _n
        file write `fh' `"\addlinespace[0.4em]"' _n
    }
    file write `fh' `"\multicolumn{9}{l}{\textit{`panel_hdr'}} \\"' _n
    file write `fh' `"\addlinespace"' _n

    forvalues r = 1/7 {
        local pool : word `r' of `pool_list'
        local lab  "`pool_label_`pool''"

        local nC : di %12.0fc ``matname''[`r', 1]
        local nC = strtrim("`nC'")
        local cells ""
        forvalues c = 2/8 {
            local v : di %5.2f ``matname''[`r', `c']
            local v = strtrim("`v'")
            local cells "`cells' & `v'"
        }
        file write `fh' `"`lab' & `nC'`cells' \\"' _n

        if `r' == 1 file write `fh' `"\addlinespace"' _n
    }
}

file write `fh' `"\bottomrule"' _n
file write `fh' `"\end{tabular}"' _n
file write `fh' `"\begin{tablenotes}[flushleft]"' _n
file write `fh' `"\setstretch{1}\footnotesize"' _n
file write `fh' `"\setlength{\itemsep}{1pt}"' _n
file write `fh' `"\item \textit{Notes:} AGI in-, out-, and net-migration rates as a percentage of each county's base filing population, averaged over the indicated pre and post periods (2020 dropped). N counts donor counties per pool. The all-donor pool comprises all U.S.\ counties in each dataset except Alaska, Hawaii, California, Washington, and non-Multnomah Oregon; see Appendix~\ref{sec:appendixb} for the other pools. Panel~A uses all IRS counties (2016--2022); Panels~B--D restrict to the balanced ACS panel (IRS-389, ACS-25+, and ACS-college specifications), for which 337 of the 389 ACS-identified counties (336 donors $+$ Multnomah) survive our balanced-panel and state-drop restrictions. Panel~D restricts to households with at least one college-educated member."' _n
file write `fh' `"\item Source: IRS SOI county-to-county migration flows (Panels~A and~B); ACS microdata, all 25+ subsample (Panel~C); ACS microdata, college-educated subsample (Panel~D)."' _n
file write `fh' `"\end{tablenotes}"' _n
file write `fh' `"\end{threeparttable}"' _n
file write `fh' `"\end{table}"' _n

file close `fh'
dis "Wrote `_outfile'"

}
project_save_overleaf "${results}tables/table1_combined.tex"

dis "Table 1 build complete."

}  // end if sdid_analysis_data.dta exists


** Close log
clear
log close log_02
