/*******************************************************************************
File Name: 		02_appendix_data_quality.do
Creator: 		John Iselin
Date Update:	February 22, 2026

Called by: 00_multnomah.do

Purpose: Generate appendix figures investigating whether the well-documented
         IRS migration data quality issues (DeWaard et al. 2022) differentially
         affect Multnomah County. Extended IRS data (2012-2022) is used to show
         that the anomaly affected Multnomah similarly to other counties.

Outputs (IRS):
- fig_dq_timeseries_out.png:  Figure B1(a) - Out-migration rates over time
- fig_dq_timeseries_in.png:   Figure B1(b) - In-migration rates over time
- fig_dq_dist_n1.png:         Figure B2 - YoY change in out-migration (returns)
- fig_dq_dist_n1_in.png:      Figure B2 - YoY change in in-migration (returns)
- fig_dq_dist_agi.png:        Figure B3 - YoY change in out-migration (AGI)
- fig_dq_dist_agi_in.png:     Figure B3 - YoY change in in-migration (AGI)

Outputs (ACS):
- fig_dq_acs_timeseries_out.png:  Figure B4(a) - ACS out-migration rates over time
- fig_dq_acs_timeseries_in.png:   Figure B4(b) - ACS in-migration rates over time
- fig_dq_acs_dist_hh.png:         Figure B5 - ACS YoY change in out-migration (households)
- fig_dq_acs_dist_hh_in.png:      Figure B5 - ACS YoY change in in-migration (households)
- fig_dq_acs_dist_dollars.png:    Figure B6 - ACS YoY change in out-migration (dollars)
- fig_dq_acs_dist_dollars_in.png: Figure B6 - ACS YoY change in in-migration (dollars)

Authors: John Iselin

For more information, contact john.iselin@yale.edu

*******************************************************************************/

** Load shared project defaults and helper programs
local cwd = subinstr("`c(pwd)'", "\", "/", .)
local suffix "/code/stata"
if "${dir}" == "" {
	if length("`cwd'") >= length("`suffix'") & ///
		substr("`cwd'", length("`cwd'") - length("`suffix'") + 1, .) == "`suffix'" {
		global dir = substr("`cwd'", 1, length("`cwd'") - length("`suffix'"))
	}
	else {
		global dir "`cwd'"
	}
}
if "${code}" == "" global code "${dir}/code/stata/"
do "${code}00_stata_config.do"

** Start log file
capture log close log_dq
log using "${logs}02_log_appendix_data_quality_${date}", replace text name(log_dq)
project_set_seed, context("02_appendix_data_quality.do") offset(130)

** plotplainblind palette (RGB) — consistent across all figures
local col_out  "0 114 178"    // sea (p7) — out-migration
local col_in   "213 94 0"     // vermillion (p6) — in-migration
local col_mult "230 159 0"    // orangebrown (p8) — Multnomah highlight
local col_oregon "86 180 233" // sky (p3) — Oregon (secondary)
local col_ref  "153 153 153"  // gs10 (p2) — reference lines

** Create output directory
capture mkdir "${results}appx_irs_data"

********************************************************************************
** STEP 1: Prepare County-Level Migration Rates (2012-2022)
********************************************************************************

** Load the extended county gross migration file
use "${data}working/irs_county_gross", clear

** Verify data extends back to 2012
summ year
assert r(min) <= 2012

** Compute out-migration rate: domestic movers / (stayers + all movers)
** move_type 1 = stayers (non-movers)
** move_type 3 = domestic movers (total)
** n1_out_1 = non-movers (returns), n1_out_3 = domestic movers (returns)

** Returns-based rates
gen out_rate_n1 = n1_out_3 / (n1_out_1 + n1_out_2) if n1_out_1 > 0
gen in_rate_n1  = n1_in_3  / (n1_in_1  + n1_in_2)  if n1_in_1  > 0

** AGI-based rates
gen out_rate_agi = agi_out_3 / (agi_out_1 + agi_out_2) if agi_out_1 > 0
gen in_rate_agi  = agi_in_3  / (agi_in_1  + agi_in_2)  if agi_in_1  > 0

** Multnomah indicator
gen multnomah = (state_fips == 41 & county_fips == 51)

** Save county-level rates for later use
tempfile county_rates
save `county_rates'

********************************************************************************
** STEP 2: Compute National Average Migration Rates
********************************************************************************

** Collapse to national means by year (weighted by number of returns)
collapse (mean) out_rate_n1 in_rate_n1 out_rate_agi in_rate_agi ///
	[aw = n1_out_1], by(year)

rename out_rate_n1  natl_out_rate_n1
rename in_rate_n1   natl_in_rate_n1
rename out_rate_agi  natl_out_rate_agi
rename in_rate_agi   natl_in_rate_agi

tempfile national
save `national'

********************************************************************************
** STEP 3: Get Oregon State-Level Migration Rates
********************************************************************************

** Load state-level gross migration data
use "${data}working/irs_state_gross", clear

** Keep Oregon (state_fips == 41)
keep if state_fips == 41

** Compute state-level rates
gen or_out_rate_n1 = n1_out_3 / (n1_out_1 + n1_out_2) if n1_out_1 > 0
gen or_in_rate_n1  = n1_in_3  / (n1_in_1  + n1_in_2)  if n1_in_1  > 0

keep year or_out_rate_n1 or_in_rate_n1

tempfile oregon
save `oregon'

********************************************************************************
** STEP 4: Get Multnomah County Rates
********************************************************************************

use `county_rates', clear
keep if multnomah == 1

rename out_rate_n1  mult_out_rate_n1
rename in_rate_n1   mult_in_rate_n1
rename out_rate_agi  mult_out_rate_agi
rename in_rate_agi   mult_in_rate_agi

keep year mult_*

tempfile multnomah_rates
save `multnomah_rates'

********************************************************************************
** STEP 5: Figure B1 -- Time Series of Migration Rates
********************************************************************************

** Merge all three series
use `national', clear
merge 1:1 year using `oregon', nogen
merge 1:1 year using `multnomah_rates', nogen

** Scale to percentage
foreach v of varlist natl_* or_* mult_* {
	replace `v' = `v' * 100
}

** --- Panel (a): Out-migration ---
twoway ///
	(line natl_out_rate_n1 year, lcolor("`col_ref'") lpattern(dash) lwidth(medthick)) ///
	(line or_out_rate_n1 year, lcolor("`col_oregon'") lpattern(shortdash) lwidth(medthick)) ///
	(line mult_out_rate_n1 year, lcolor("`col_mult'") lpattern(solid) lwidth(thick)) ///
	, ///
	xline(2016, lcolor(gs10) lpattern(dash)) ///
	xlabel(2012(1)2022, angle(45) labsize(small)) ///
	ylabel(, labsize(small) format(%9.1f)) ///
	ytitle("Out-migration rate (%)", size(small)) ///
	xtitle("Year", size(small)) ///
	legend(order(1 "National average" 2 "Oregon" 3 "Multnomah County") ///
		rows(1) size(vsmall) position(6)) ///
	graphregion(color(white)) ///
	note("Vertical line marks start of main analysis window (2016)." ///
		"Source: IRS SOI county-to-county migration data.", size(vsmall))

graph export "${results}appx_irs_data/fig_dq_timeseries_out.png", replace width(2400)
if ${overleaf} == 1 {
	graph export "${ol_fig}fig_dq_timeseries_out.png", replace width(2400)
}

** --- Panel (b): In-migration ---
twoway ///
	(line natl_in_rate_n1 year, lcolor("`col_ref'") lpattern(dash) lwidth(medthick)) ///
	(line or_in_rate_n1 year, lcolor("`col_oregon'") lpattern(shortdash) lwidth(medthick)) ///
	(line mult_in_rate_n1 year, lcolor("`col_mult'") lpattern(solid) lwidth(thick)) ///
	, ///
	xline(2016, lcolor(gs10) lpattern(dash)) ///
	xlabel(2012(1)2022, angle(45) labsize(small)) ///
	ylabel(, labsize(small) format(%9.1f)) ///
	ytitle("In-migration rate (%)", size(small)) ///
	xtitle("Year", size(small)) ///
	legend(order(1 "National average" 2 "Oregon" 3 "Multnomah County") ///
		rows(1) size(vsmall) position(6)) ///
	graphregion(color(white)) ///
	note("Vertical line marks start of main analysis window (2016)." ///
		"Source: IRS SOI county-to-county migration data.", size(vsmall))

graph export "${results}appx_irs_data/fig_dq_timeseries_in.png", replace width(2400)
if ${overleaf} == 1 {
	graph export "${ol_fig}fig_dq_timeseries_in.png", replace width(2400)
}

********************************************************************************
** STEP 6: Compute Year-over-Year Changes in County-Level Rates
********************************************************************************

use `county_rates', clear

** Keep necessary variables
keep year state_fips county_fips out_rate_n1 in_rate_n1 out_rate_agi in_rate_agi multnomah

** Generate FIPS identifier for panel
gen long fips = state_fips * 1000 + county_fips

** Set panel
xtset fips year

** Compute year-over-year percent changes (relative to base year)
foreach var in out_rate_n1 in_rate_n1 out_rate_agi in_rate_agi {
	gen d_`var' = (`var' - L.`var') / L.`var' * 100 if L.`var' > 0
}

** Save
tempfile yoy_changes
save `yoy_changes'

********************************************************************************
** STEP 7: Figures B2 & B3 -- Distribution of YoY Changes with Multnomah Marked
********************************************************************************

** Key anomaly transition years to examine:
** 2013: 2012-13 vs 2011-12 (start of anomaly decline)
** 2014: 2013-14 vs 2012-13 (continued decline)
** 2015: 2014-15 vs 2013-14 (trough)
** 2016: 2015-16 vs 2014-15 (recovery begins)
** 2017: 2016-17 vs 2015-16 (peak / start of analysis window)

** ---- Figure B2: Returns (n1) ----
** We show out-migration rate changes in a 2x2 layout for 4 transition years

foreach measure in "n1" "agi" {

	if "`measure'" == "n1" {
		local fig_title "Returns"
		local fig_num "B2"
		local fig_file "fig_dq_dist_n1"
	}
	else {
		local fig_title "AGI"
		local fig_num "B3"
		local fig_file "fig_dq_dist_agi"
	}

	local glist ""
	local panel_idx = 0
	local panel_labels `" "a" "b" "c" "d" "'

	foreach yr in 2014 2015 2016 2017 {

		local panel_idx = `panel_idx' + 1
		local panel_ltr : word `panel_idx' of `panel_labels'
		local prev = `yr' - 1

		** Get Multnomah's change value for this year (out-migration)
		use `yoy_changes', clear
		keep if year == `yr'

		** Get Multnomah's value
		summ d_out_rate_`measure' if multnomah == 1
		local mult_val = r(mean)

		** Compute Multnomah's percentile rank
		count if !missing(d_out_rate_`measure')
		local N_total = r(N)
		count if d_out_rate_`measure' <= `mult_val' & !missing(d_out_rate_`measure')
		local N_below = r(N)
		local pctile = round(100 * `N_below' / `N_total', 1)

		** Count outliers excluded from density (|change| > 200%)
		count if abs(d_out_rate_`measure') > 200 & !missing(d_out_rate_`measure') & multnomah == 0
		local N_excl = r(N)

		** Kernel density with Multnomah marked (exclude |change| > 200%)
		twoway ///
			(kdensity d_out_rate_`measure' ///
				if multnomah == 0 & inrange(d_out_rate_`measure', -200, 200), ///
				lcolor("`col_ref'") lwidth(medthick) lpattern(solid)) ///
			, ///
			xline(`mult_val', lcolor("`col_mult'") lwidth(thick) lpattern(solid)) ///
			ylabel(, labsize(vsmall)) ///
			xlabel(, labsize(vsmall)) ///
			ytitle("Density", size(vsmall)) ///
			xtitle("Change in out-migration rate (%)", size(vsmall)) ///
			graphregion(color(white)) ///
			note("Multnomah at `pctile'th percentile (vertical line)." ///
				"`N_excl' counties with |change| > 200% excluded.", size(vsmall)) ///
			legend(off) ///
			name(g_`measure'_`yr', replace)

		local glist "`glist' g_`measure'_`yr'"

	} // END YEAR LOOP

	** Combine panels
	graph combine `glist', ///
		rows(2) cols(2) ///
		graphregion(color(white)) ///
		note("Kernel density of county-level year-over-year changes in the out-migration rate." ///
			"Vertical line marks Multnomah County. Counties with |change| > 200% excluded. Source: IRS SOI.", size(vsmall))

	graph export "${results}appx_irs_data/`fig_file'.png", replace width(3000)
	if ${overleaf} == 1 {
		graph export "${ol_fig}`fig_file'.png", replace width(3000)
	}

	** Repeat for in-migration
	local glist ""
	local panel_idx = 0

	foreach yr in 2014 2015 2016 2017 {

		local panel_idx = `panel_idx' + 1
		local panel_ltr : word `panel_idx' of `panel_labels'
		local prev = `yr' - 1

		use `yoy_changes', clear
		keep if year == `yr'

		** Get Multnomah's value
		summ d_in_rate_`measure' if multnomah == 1
		local mult_val = r(mean)

		** Compute Multnomah's percentile rank
		count if !missing(d_in_rate_`measure')
		local N_total = r(N)
		count if d_in_rate_`measure' <= `mult_val' & !missing(d_in_rate_`measure')
		local N_below = r(N)
		local pctile = round(100 * `N_below' / `N_total', 1)

		** Count outliers excluded from density (|change| > 200%)
		count if abs(d_in_rate_`measure') > 200 & !missing(d_in_rate_`measure') & multnomah == 0
		local N_excl = r(N)

		** Kernel density with Multnomah marked (exclude |change| > 200%)
		twoway ///
			(kdensity d_in_rate_`measure' ///
				if multnomah == 0 & inrange(d_in_rate_`measure', -200, 200), ///
				lcolor("`col_ref'") lwidth(medthick) lpattern(solid)) ///
			, ///
			xline(`mult_val', lcolor("`col_mult'") lwidth(thick) lpattern(solid)) ///
			ylabel(, labsize(vsmall)) ///
			xlabel(, labsize(vsmall)) ///
			ytitle("Density", size(vsmall)) ///
			xtitle("Change in in-migration rate (%)", size(vsmall)) ///
			graphregion(color(white)) ///
			note("Multnomah at `pctile'th percentile (vertical line)." ///
				"`N_excl' counties with |change| > 200% excluded.", size(vsmall)) ///
			legend(off) ///
			name(g_`measure'_in_`yr', replace)

		local glist "`glist' g_`measure'_in_`yr'"

	} // END YEAR LOOP

	** Combine in-migration panels
	graph combine `glist', ///
		rows(2) cols(2) ///
		graphregion(color(white)) ///
		note("Kernel density of county-level year-over-year changes in the in-migration rate." ///
			"Vertical line marks Multnomah County. Counties with |change| > 200% excluded. Source: IRS SOI.", size(vsmall))

	graph export "${results}appx_irs_data/`fig_file'_in.png", replace width(3000)
	if ${overleaf} == 1 {
		graph export "${ol_fig}`fig_file'_in.png", replace width(3000)
	}

} // END MEASURE LOOP

********************************************************************************
** STEP 8: Display Summary Statistics
********************************************************************************

** Report Multnomah's percentile for each transition year and measure
di _n "==============================================="
di "Multnomah County Percentile Ranks"
di "(Out-Migration Rate, Year-over-Year Change)"
di "==============================================="

foreach measure in "n1" "agi" {
	di _n "--- Measure: `measure' ---"
	foreach yr in 2014 2015 2016 2017 {
		use `yoy_changes', clear
		keep if year == `yr'

		summ d_out_rate_`measure' if multnomah == 1
		local mult_val = r(mean)

		count if !missing(d_out_rate_`measure')
		local N_total = r(N)
		count if d_out_rate_`measure' <= `mult_val' & !missing(d_out_rate_`measure')
		local N_below = r(N)
		local pctile = round(100 * `N_below' / `N_total', 0.1)

		di "  Year `yr': `pctile'th percentile (change = " %7.2f `mult_val' "%)"
	}
}

di _n "==============================================="
di "Multnomah County Percentile Ranks"
di "(In-Migration Rate, Year-over-Year Change)"
di "==============================================="

foreach measure in "n1" "agi" {
	di _n "--- Measure: `measure' ---"
	foreach yr in 2014 2015 2016 2017 {
		use `yoy_changes', clear
		keep if year == `yr'

		summ d_in_rate_`measure' if multnomah == 1
		local mult_val = r(mean)

		count if !missing(d_in_rate_`measure')
		local N_total = r(N)
		count if d_in_rate_`measure' <= `mult_val' & !missing(d_in_rate_`measure')
		local N_below = r(N)
		local pctile = round(100 * `N_below' / `N_total', 0.1)

		di "  Year `yr': `pctile'th percentile (change = " %7.2f `mult_val' "%)"
	}
}

********************************************************************************
********************************************************************************
** ACS-BASED ANALYSIS
** Parallel exercises using ACS microdata (households, dollars)
** Note: ACS covers ~389 identified counties vs ~3,100 for IRS
********************************************************************************
********************************************************************************

********************************************************************************
** STEP 9: Prepare ACS County-Level Migration Rates
********************************************************************************

** Load the ACS county gross migration file (25+ sample)
use "${data}working/acs_county_gross_25plus", clear

** Check year range
summ year
local acs_min_year = r(min)
local acs_max_year = r(max)
di "ACS data range: `acs_min_year' - `acs_max_year'"

** Compute out-migration rate: domestic movers / (stayers + all movers)
** households_out_1 = non-movers, households_out_2 = all movers,
** households_out_3 = domestic movers (same as 2 since foreign dropped upstream)

** Household-based rates
gen out_rate_hh = households_out_3 / (households_out_1 + households_out_2) ///
	if households_out_1 > 0
gen in_rate_hh  = households_in_3  / (households_in_1  + households_in_2)  ///
	if households_in_1  > 0

** Dollar-based rates
gen out_rate_dollars = dollars_out_3 / (dollars_out_1 + dollars_out_2) ///
	if dollars_out_1 > 0 & (dollars_out_1 + dollars_out_2) > 0
gen in_rate_dollars  = dollars_in_3  / (dollars_in_1  + dollars_in_2)  ///
	if dollars_in_1  > 0 & (dollars_in_1  + dollars_in_2)  > 0

** Multnomah indicator (fips = 41051)
gen multnomah = (fips == 41051)

** Save county-level rates
tempfile acs_county_rates
save `acs_county_rates'

********************************************************************************
** STEP 10: Compute ACS National Average Migration Rates
********************************************************************************

** Collapse to national means by year (weighted by households)
collapse (mean) out_rate_hh in_rate_hh out_rate_dollars in_rate_dollars ///
	[aw = households_out_1], by(year)

rename out_rate_hh      acs_natl_out_rate_hh
rename in_rate_hh       acs_natl_in_rate_hh
rename out_rate_dollars  acs_natl_out_rate_dollars
rename in_rate_dollars   acs_natl_in_rate_dollars

tempfile acs_national
save `acs_national'

********************************************************************************
** STEP 11: Get ACS Multnomah County Rates
********************************************************************************

use `acs_county_rates', clear
keep if multnomah == 1

rename out_rate_hh      acs_mult_out_rate_hh
rename in_rate_hh       acs_mult_in_rate_hh
rename out_rate_dollars  acs_mult_out_rate_dollars
rename in_rate_dollars   acs_mult_in_rate_dollars

keep year acs_mult_*

tempfile acs_multnomah_rates
save `acs_multnomah_rates'

********************************************************************************
** STEP 12: Figure B4 -- ACS Time Series of Migration Rates
********************************************************************************

** Merge national and Multnomah series
use `acs_national', clear
merge 1:1 year using `acs_multnomah_rates', nogen

** Scale to percentage
foreach v of varlist acs_natl_* acs_mult_* {
	replace `v' = `v' * 100
}

** --- Panel (a): Out-migration ---
twoway ///
	(line acs_natl_out_rate_hh year, lcolor("`col_ref'") lpattern(dash) lwidth(medthick)) ///
	(line acs_mult_out_rate_hh year, lcolor("`col_mult'") lpattern(solid) lwidth(thick)) ///
	, ///
	xline(2016, lcolor(gs10) lpattern(dash)) ///
	xlabel(`acs_min_year'(1)`acs_max_year', angle(45) labsize(small)) ///
	ylabel(, labsize(small) format(%9.1f)) ///
	ytitle("Out-migration rate (%)", size(small)) ///
	xtitle("Year", size(small)) ///
	legend(order(1 "National average" 2 "Multnomah County") ///
		rows(1) size(vsmall) position(6)) ///
	graphregion(color(white)) ///
	note("Vertical line marks start of main analysis window (2016)." ///
		"Source: Authors' calculations using ACS microdata, ages 25+.", size(vsmall))

graph export "${results}appx_irs_data/fig_dq_acs_timeseries_out.png", replace width(2400)
if ${overleaf} == 1 {
	graph export "${ol_fig}fig_dq_acs_timeseries_out.png", replace width(2400)
}

** --- Panel (b): In-migration ---
twoway ///
	(line acs_natl_in_rate_hh year, lcolor("`col_ref'") lpattern(dash) lwidth(medthick)) ///
	(line acs_mult_in_rate_hh year, lcolor("`col_mult'") lpattern(solid) lwidth(thick)) ///
	, ///
	xline(2016, lcolor(gs10) lpattern(dash)) ///
	xlabel(`acs_min_year'(1)`acs_max_year', angle(45) labsize(small)) ///
	ylabel(, labsize(small) format(%9.1f)) ///
	ytitle("In-migration rate (%)", size(small)) ///
	xtitle("Year", size(small)) ///
	legend(order(1 "National average" 2 "Multnomah County") ///
		rows(1) size(vsmall) position(6)) ///
	graphregion(color(white)) ///
	note("Vertical line marks start of main analysis window (2016)." ///
		"Source: Authors' calculations using ACS microdata, ages 25+.", size(vsmall))

graph export "${results}appx_irs_data/fig_dq_acs_timeseries_in.png", replace width(2400)
if ${overleaf} == 1 {
	graph export "${ol_fig}fig_dq_acs_timeseries_in.png", replace width(2400)
}

********************************************************************************
** STEP 13: Compute ACS Year-over-Year Changes
********************************************************************************

use `acs_county_rates', clear

** Keep necessary variables
keep year fips out_rate_hh in_rate_hh out_rate_dollars in_rate_dollars multnomah

** Set panel
xtset fips year

** Compute year-over-year percent changes (relative to base year)
foreach var in out_rate_hh in_rate_hh out_rate_dollars in_rate_dollars {
	gen d_`var' = (`var' - L.`var') / L.`var' * 100 if L.`var' > 0
}

** Save
tempfile acs_yoy_changes
save `acs_yoy_changes'

********************************************************************************
** STEP 14: Figures B5 & B6 -- ACS Distribution of YoY Changes
********************************************************************************

** Use the same transition years as IRS (where ACS data is available)
** ACS years depend on start_year_acs; show 4 transition years within range

foreach measure in "hh" "dollars" {

	if "`measure'" == "hh" {
		local fig_title "Households"
		local fig_num "B5"
		local fig_file "fig_dq_acs_dist_hh"
	}
	else {
		local fig_title "Dollars"
		local fig_num "B6"
		local fig_file "fig_dq_acs_dist_dollars"
	}

	** Out-migration distributions
	local glist ""
	local panel_idx = 0
	local panel_labels `" "a" "b" "c" "d" "'

	foreach yr in 2014 2015 2016 2017 {

		local panel_idx = `panel_idx' + 1
		local panel_ltr : word `panel_idx' of `panel_labels'
		local prev = `yr' - 1

		use `acs_yoy_changes', clear
		keep if year == `yr'

		** Check if Multnomah has data for this year
		count if multnomah == 1 & !missing(d_out_rate_`measure')
		if r(N) == 0 {
			di as txt "  Skipping ACS `measure' out-migration `yr': no Multnomah data"
			continue
		}

		** Get Multnomah's value
		summ d_out_rate_`measure' if multnomah == 1
		local mult_val = r(mean)

		** Compute Multnomah's percentile rank
		count if !missing(d_out_rate_`measure')
		local N_total = r(N)
		count if d_out_rate_`measure' <= `mult_val' & !missing(d_out_rate_`measure')
		local N_below = r(N)
		local pctile = round(100 * `N_below' / `N_total', 1)

		** Count outliers excluded from density (|change| > 200%)
		count if abs(d_out_rate_`measure') > 200 & !missing(d_out_rate_`measure') & multnomah == 0
		local N_excl = r(N)

		** Kernel density with Multnomah marked (exclude |change| > 200%)
		twoway ///
			(kdensity d_out_rate_`measure' ///
				if multnomah == 0 & inrange(d_out_rate_`measure', -200, 200), ///
				lcolor("`col_ref'") lwidth(medthick) lpattern(solid)) ///
			, ///
			xline(`mult_val', lcolor("`col_mult'") lwidth(thick) lpattern(solid)) ///
			ylabel(, labsize(vsmall)) ///
			xlabel(, labsize(vsmall)) ///
			ytitle("Density", size(vsmall)) ///
			xtitle("Change in out-migration rate (%)", size(vsmall)) ///
			graphregion(color(white)) ///
			note("Multnomah at `pctile'th percentile (vertical line)." ///
				"`N_excl' counties with |change| > 200% excluded.", size(vsmall)) ///
			legend(off) ///
			name(g_acs_`measure'_`yr', replace)

		local glist "`glist' g_acs_`measure'_`yr'"

	} // END YEAR LOOP

	** Combine panels (only if we have graphs)
	if "`glist'" != "" {
		graph combine `glist', ///
			rows(2) cols(2) ///
			graphregion(color(white)) ///
			note("Kernel density of county-level year-over-year changes in the out-migration rate." ///
				"Vertical line marks Multnomah County. Counties with |change| > 200% excluded. Source: ACS microdata, ages 25+.", size(vsmall))

		graph export "${results}appx_irs_data/`fig_file'.png", replace width(3000)
		if ${overleaf} == 1 {
			graph export "${ol_fig}`fig_file'.png", replace width(3000)
		}
	}

	** Repeat for in-migration
	local glist ""
	local panel_idx = 0

	foreach yr in 2014 2015 2016 2017 {

		local panel_idx = `panel_idx' + 1
		local panel_ltr : word `panel_idx' of `panel_labels'
		local prev = `yr' - 1

		use `acs_yoy_changes', clear
		keep if year == `yr'

		** Check if Multnomah has data for this year
		count if multnomah == 1 & !missing(d_in_rate_`measure')
		if r(N) == 0 {
			di as txt "  Skipping ACS `measure' in-migration `yr': no Multnomah data"
			continue
		}

		** Get Multnomah's value
		summ d_in_rate_`measure' if multnomah == 1
		local mult_val = r(mean)

		** Compute Multnomah's percentile rank
		count if !missing(d_in_rate_`measure')
		local N_total = r(N)
		count if d_in_rate_`measure' <= `mult_val' & !missing(d_in_rate_`measure')
		local N_below = r(N)
		local pctile = round(100 * `N_below' / `N_total', 1)

		** Count outliers excluded from density (|change| > 200%)
		count if abs(d_in_rate_`measure') > 200 & !missing(d_in_rate_`measure') & multnomah == 0
		local N_excl = r(N)

		** Kernel density with Multnomah marked (exclude |change| > 200%)
		twoway ///
			(kdensity d_in_rate_`measure' ///
				if multnomah == 0 & inrange(d_in_rate_`measure', -200, 200), ///
				lcolor("`col_ref'") lwidth(medthick) lpattern(solid)) ///
			, ///
			xline(`mult_val', lcolor("`col_mult'") lwidth(thick) lpattern(solid)) ///
			ylabel(, labsize(vsmall)) ///
			xlabel(, labsize(vsmall)) ///
			ytitle("Density", size(vsmall)) ///
			xtitle("Change in in-migration rate (%)", size(vsmall)) ///
			graphregion(color(white)) ///
			note("Multnomah at `pctile'th percentile (vertical line)." ///
				"`N_excl' counties with |change| > 200% excluded.", size(vsmall)) ///
			legend(off) ///
			name(g_acs_`measure'_in_`yr', replace)

		local glist "`glist' g_acs_`measure'_in_`yr'"

	} // END YEAR LOOP

	** Combine in-migration panels
	if "`glist'" != "" {
		graph combine `glist', ///
			rows(2) cols(2) ///
			graphregion(color(white)) ///
			note("Kernel density of county-level year-over-year changes in the in-migration rate." ///
				"Vertical line marks Multnomah County. Counties with |change| > 200% excluded. Source: ACS microdata, ages 25+.", size(vsmall))

		graph export "${results}appx_irs_data/`fig_file'_in.png", replace width(3000)
		if ${overleaf} == 1 {
			graph export "${ol_fig}`fig_file'_in.png", replace width(3000)
		}
	}

} // END MEASURE LOOP

********************************************************************************
** STEP 15: ACS Summary Statistics
********************************************************************************

di _n "==============================================="
di "ACS: Multnomah County Percentile Ranks"
di "(Out-Migration Rate, Year-over-Year Change)"
di "==============================================="

foreach measure in "hh" "dollars" {
	di _n "--- Measure: `measure' ---"
	foreach yr in 2014 2015 2016 2017 {
		use `acs_yoy_changes', clear
		keep if year == `yr'

		count if multnomah == 1 & !missing(d_out_rate_`measure')
		if r(N) == 0 {
			di "  Year `yr': no Multnomah data"
			continue
		}

		summ d_out_rate_`measure' if multnomah == 1
		local mult_val = r(mean)

		count if !missing(d_out_rate_`measure')
		local N_total = r(N)
		count if d_out_rate_`measure' <= `mult_val' & !missing(d_out_rate_`measure')
		local N_below = r(N)
		local pctile = round(100 * `N_below' / `N_total', 0.1)

		di "  Year `yr': `pctile'th percentile (change = " %7.2f `mult_val' "%, N = `N_total' counties)"
	}
}

di _n "==============================================="
di "ACS: Multnomah County Percentile Ranks"
di "(In-Migration Rate, Year-over-Year Change)"
di "==============================================="

foreach measure in "hh" "dollars" {
	di _n "--- Measure: `measure' ---"
	foreach yr in 2014 2015 2016 2017 {
		use `acs_yoy_changes', clear
		keep if year == `yr'

		count if multnomah == 1 & !missing(d_in_rate_`measure')
		if r(N) == 0 {
			di "  Year `yr': no Multnomah data"
			continue
		}

		summ d_in_rate_`measure' if multnomah == 1
		local mult_val = r(mean)

		count if !missing(d_in_rate_`measure')
		local N_total = r(N)
		count if d_in_rate_`measure' <= `mult_val' & !missing(d_in_rate_`measure')
		local N_below = r(N)
		local pctile = round(100 * `N_below' / `N_total', 0.1)

		di "  Year `yr': `pctile'th percentile (change = " %7.2f `mult_val' "%, N = `N_total' counties)"
	}
}

** Close log file
capture log close log_dq
