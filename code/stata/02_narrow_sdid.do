/*******************************************************************************
File Name: 		02_narrow_sdid.do
Creator: 		John Iselin
Date Update:	February 2026

Called by: 00_multnomah.do

Purpose: Perform synthetic difference-in-difference estimation using a narrow
         control pool of 20 similar cities identified via the Harvard Growth
         Lab's Metroverse tool.

Note: Unlike the main SDID analysis, this file does NOT drop CA/WA/OR counties
      from the donor pool. Sacramento (CA) and Seattle (WA) are included by
      design as part of the similar-cities comparison group. Vancouver (WA),
      which Metroverse also suggests, is intentionally excluded because the
      Multnomah--Vancouver commuter / migration link creates SUTVA-violating
      spillover into the donor county.

Source: https://metroverse.hks.harvard.edu/city/101/similar-cities

Outputs:
- ${results}sdid/narrow/ — Tables and event study figures
- narrow_sdid_results.dta: Treatment effects with same schema as sdid_results.dta

Authors: John Iselin

For more information, contact john.iselin@yale.edu

*******************************************************************************/


** Load shared project defaults and helper programs
if "${code}" == "" {
	local _cwd = subinstr("`c(pwd)'", "\", "/", .)
	if regexm("`_cwd'", "(.*)/code/stata$") global code "`_cwd'/"
	else global code "`_cwd'/code/stata/"
}
do "${code}00_stata_config.do"
** 01a_programs.do is normally sourced by 00_multnomah.do; source defensively
** so standalone invocation also has project_parse_outcome_components.
do "${code}01a_programs.do"

** Start log file
capture log close log_02_narrow
log using "${logs}02_log_narrow_sdid_${date}", replace text name(log_02_narrow)
project_set_seed, context("02_narrow_sdid.do") offset(70)

** plotplainblind palette (RGB) — consistent across all figures
local col_sig_notpref  "0 114 178"    // sea (p7) — sig, not preferred
local col_insig_notpref "86 180 233"  // sky (p3) — insig, not preferred
local col_sig_pref     "213 94 0"     // vermillion (p6) — sig, preferred
local col_insig_pref   "230 159 0"    // orangebrown (p8) — insig, preferred
local col_zero         "204 121 167"  // reddish (p5) — zero line
local col_ref          "153 153 153"  // gs10 (p2) — reference lines

** Number of bootstrap replications
local reps = 100

********************************************************************************
** DATA PREPARATION
********************************************************************************

** Load data (same sequence as 02_sdid_analysis.do)
use "${data}working/irs_county_gross", replace

** Keep required variables
keep year fips state* county* *_net_3 *_out_1 *_out_2 *_in_3 *_out_3 *_net_5 *_in_5 *_out_5
order year fips state* county*

** Merge with ACS Data (25+)
merge 1:1 year fips using "${data}working/acs_county_gross_25plus", gen(merge_acs_1)

** Keep required variables
keep year fips state* county* *_net_3 *_out_1 *_out_2 *_in_3 *_out_3 *_net_5 *_in_5 *_out_5 merge_acs_*

** Label ACS samples
rename persons_* acs1_persons_*
rename households_* acs1_households_*
rename dollars_* acs1_dollars_*

** Merge with ACS Data (college)
merge 1:1 year fips using "${data}working/acs_county_gross_college", gen(merge_acs_2)

** Keep required variables
keep year fips state* county* *_net_3 *_out_1 *_out_2 *_in_3 *_out_3 *_net_5 *_in_5 *_out_5 merge_acs_*

** Label ACS samples
rename persons_* acs2_persons_*
rename households_* acs2_households_*
rename dollars_* acs2_dollars_*

** Drop "other counties"
drop if county_fips == 0
drop if year < 2016					// Sample: 2016-2024 (IRS/ACS data start 2012)

** Merge with Demographic data
merge m:1 fips using "${data}working/demographics_2020", ///
	gen(demo_merge) keep(master match)
project_report_merge, gen(demo_merge) tag("demographics_2020") keep_merge
keep if demo_merge == 3
drop demo_merge

** Rename
rename population pop_census

** Merge with BEA economics
merge m:1 year fips using "${data}working/bea_economics", ///
	gen(econ_merge) keep(master match)
project_report_merge, gen(econ_merge) tag("bea_economics") keep_merge
keep if econ_merge == 3
drop econ_merge

** Merge with COVID-19 Data
merge m:1 fips using "${data}working/covid_cleaned_wide.dta", ///
	gen(covid_merge) keep(master match)
project_report_merge, gen(covid_merge) tag("covid_wide")

** Merge with Property Tax Rates
merge m:1 year fips using "${data}working/property_tax_rates_overall", ///
	gen(proptx_merge) keep(master match) keepusing(prop_rate_mean)
project_report_merge, gen(proptx_merge) tag("property_tax")

** Rename for clarity
rename prop_rate_mean prop_tax_rate
label var prop_tax_rate "Mean property tax rate (% of home value)"

** Organize data
order year fips state_* county_*
sort fips year
isid fips year

** Keep only sample with non-missing base populations
drop if (missing(n1_out_1) | n1_out_1 == 0) & year <= 2022

** Keep only counties with observations in every IRS year (2016-2022)
** Note: balanced panel is required for SDID estimation. IRS-only
**       counties span 2016-2022; ACS-matched counties extend to 2024.
**       Require IRS-period balance for all counties here; ACS-period
**       balance is enforced separately via acs_period indicators below.
bysort fips: egen ct_irs = total(inrange(year, 2016, 2022))
local n_irs_years = 2022 - 2016 + 1
drop if ct_irs < `n_irs_years'
drop ct_irs

********************************************************************************
** NARROW SAMPLE DEFINITION
********************************************************************************

** Define treated county
gen multnomah = state_fips == 41 & county_fips == 51
label var multnomah "Indicator for Multnomah County, Oregon"

** Define treatment indicator
gen Treated = multnomah == 1 & year > 2020
label var Treated "Treatment indicator for Multnomah County, Oregon"

** Define narrow sample: 20 similar cities + Multnomah
** FIPS codes from Harvard Growth Lab Metroverse similar-cities analysis
gen sample_narrow = 0
replace sample_narrow = 1 if fips == 41051		// Multnomah (Portland, OR)
replace sample_narrow = 1 if fips == 39049		// Franklin (Columbus, OH)
replace sample_narrow = 1 if fips == 27053		// Hennepin (Minneapolis, MN)
replace sample_narrow = 1 if fips == 42101		// Philadelphia (Philadelphia, PA)
replace sample_narrow = 1 if fips == 48453		// Travis (Austin, TX)
replace sample_narrow = 1 if fips == 12095		// Orange (Orlando, FL)
replace sample_narrow = 1 if fips == 12057		// Hillsborough (Tampa, FL)
replace sample_narrow = 1 if fips == 49035		// Salt Lake (Salt Lake City, UT)
replace sample_narrow = 1 if fips == 26163		// Wayne (Detroit, MI)
replace sample_narrow = 1 if fips == 53033		// King (Seattle, WA)
replace sample_narrow = 1 if fips == 24510		// Baltimore City (Baltimore, MD)
replace sample_narrow = 1 if fips == 55079		// Milwaukee (Milwaukee, WI)
replace sample_narrow = 1 if fips == 29510		// St. Louis City (St. Louis, MO)
replace sample_narrow = 1 if fips == 08031		// Denver (Denver, CO)
replace sample_narrow = 1 if fips == 29095		// Jackson (Kansas City, MO)
replace sample_narrow = 1 if fips == 18097		// Marion (Indianapolis, IN)
replace sample_narrow = 1 if fips == 13121		// Fulton (Atlanta, GA)
replace sample_narrow = 1 if fips == 32003		// Clark (Las Vegas, NV)
replace sample_narrow = 1 if fips == 06067		// Sacramento (Sacramento, CA)
replace sample_narrow = 1 if fips == 48029		// Bexar (San Antonio, TX)
replace sample_narrow = 1 if fips == 25025		// Suffolk (Boston, MA)
label var sample_narrow "Narrow sample: 20 similar cities + Multnomah"

** Report narrow sample
tab county_name state_name if sample_narrow == 1 & year == 2020, m

** Keep if in set of counties
keep if sample_narrow == 1

** Note: NO state drops. CA, WA, and OR counties are retained by design.
** Sacramento (CA) and Seattle (WA) are in the donor pool. Vancouver/Clark
** (WA) was deliberately excluded for SUTVA-violating Multnomah spillover.

** Generate IRS sample
gen irs_sample_1 = inrange(year, 2016, 2022)

** Generate ACS Period Indicators
gen acs_period_1 = merge_acs_1 != 1 & inrange(year, 2016, 2022)
gen acs_period_2 = merge_acs_1 != 1 & inrange(year, 2016, 2024)

** Make sure we have a balanced panel of ACS counties
gen tmp = merge_acs_1 != 1
bysort fips: egen ct_tmp = total(tmp)
qui summ ct_tmp
replace acs_period_1 = 0 if ct_tmp < `r(max)'
replace acs_period_2 = 0 if ct_tmp < `r(max)'
drop tmp ct_tmp

** Standardize covariates
local all_covariates "population per_capita_income prop_tax_rate"
foreach v of local all_covariates {
	egen tmp_v = std(`v')
	replace `v' = tmp_v
	drop tmp_v
}

********************************************************************************
** OUTCOME VARIABLES
********************************************************************************

** IRS outcomes (domestic movers, type 3)
foreach x in "n1" "n2" "agi" {

	if "`x'" == "n1" local xtxt "returns"
	else if "`x'" == "n2" local xtxt "exemptions"
	else if "`x'" == "agi" local xtxt "AGI"

	foreach y in "net" "in" "out" {

		if "`y'" == "net" local ytxt "Net domestic migration"
		else if "`y'" == "in" local ytxt "Domestic in-migration"
		else if "`y'" == "out" local ytxt "Domestic out-migration"

		gen `x'_`y'_rate_irs = 100 * (`x'_`y'_3 / (`x'_out_1 + `x'_out_2))
		label var `x'_`y'_rate_irs "`ytxt' rate, `xtxt' (%)"

	}
}

** ACS outcomes
rename acs*_households_* acs*_n1_*
rename acs*_persons_* acs*_n2_*
rename acs*_dollars_* acs*_agi_*

forvalues i = 1/2 {

	if `i' == 1 local itxt ""
	else if `i' == 2 local itxt " (College)"

	foreach x in "n1" "n2" "agi" {

		if "`x'" == "n1" local xtxt "HHs"
		else if "`x'" == "n2" local xtxt "persons"
		else if "`x'" == "agi" local xtxt "total income"

		foreach y in "net" "in" "out" {

			if "`y'" == "net" local ytxt "Net domestic migration"
			else if "`y'" == "in" local ytxt "Domestic in-migration"
			else if "`y'" == "out" local ytxt "Domestic out-migration"

			gen `x'_`y'_rate_acs`i' = 100 * (acs`i'_`x'_`y'_3 / (acs`i'_`x'_out_1 + acs`i'_`x'_out_2))
			label var `x'_`y'_rate_acs`i' "`ytxt' rate, `xtxt'`itxt' (%)"

		}
	}
}

** Declare panel
xtset fips year

********************************************************************************
** OUTPUT SETUP
********************************************************************************

capture mkdir "${results}sdid/narrow"

** Set up results dataset
preserve
clear
set obs 0
gen sample_data = ""
gen sample = ""
gen outcome = ""
gen controls = .
gen exclusion = .
gen tau = .
gen se = .
gen pval = .
gen ci_lower = .
gen ci_upper = .
gen n_counties = .
gen pre_mean = .
gen significant = .
save "${results}sdid/narrow/narrow_sdid_results.dta", replace
clear
restore

********************************************************************************
** ESTIMATION LOOP
********************************************************************************

** Simplified loop: IRS (full) and ACS (16-22 and 16-24)
foreach data of varlist irs_sample_1 acs_period_1 acs_period_2 {

	** Define covariates
	if "`data'" == "irs_sample_1" local covariates "population per_capita_income"
	else local covariates "population per_capita_income prop_tax_rate"

	** Outcome types
	if "`data'" == "irs_sample_1" local out_type "irs"
	else local out_type "acs1 acs2"

	** Loop over outcome variable types
	foreach type of local out_type {

		** Labels
		if "`data'" == "irs_sample_1" & "`type'" == "irs" local out_txt "narrow_irs_full"
		else if "`data'" == "acs_period_1" & "`type'" == "acs1" local out_txt "narrow_acs_16_22_all"
		else if "`data'" == "acs_period_1" & "`type'" == "acs2" local out_txt "narrow_acs_16_22_col"
		else if "`data'" == "acs_period_2" & "`type'" == "acs1" local out_txt "narrow_acs_16_24_all"
		else if "`data'" == "acs_period_2" & "`type'" == "acs2" local out_txt "narrow_acs_16_24_col"

		** Create subfolder
		capture mkdir "${results}sdid/narrow/`out_txt'"

		** Loop over exclusion of 2020
		forvalues exl = 1(-1)0 {

			** Define sample
			gen sample = sample_narrow == 1 & `data' == 1
			if `exl' == 1 replace sample = 0 if year == 2020

			** Clear stored values
			eststo clear

			** Loop over migration type
			foreach migr in "net" "in" "out" {

				** Loop over outcomes
				foreach out of varlist	n1_`migr'_rate_`type'	///
										n2_`migr'_rate_`type'	///
										agi_`migr'_rate_`type' {

					** Store label
					local label : variable label `out'

					** Loop over inclusion of covariates
					forvalues c = 0/1 {

						** Covariates
						if `c' == 0 local covars ""
						else if `c' == 1 local covars "covariates(`covariates')"

						** Covariates for sdid_event
						if `c' == 0 local covars_event ""
						else if `c' == 1 local covars_event "covariates(`covariates')"

						** File Name
						if `exl' == 0 local path "${results}sdid/narrow/`out_txt'/fig_`out_txt'_`out'_`c'_"
						if `exl' == 1 local path "${results}sdid/narrow/`out_txt'/fig_`out_txt'_`out'_`c'_excl2020_"

						** Run SDID
						capture noisily {
							eststo sdid_`out'_`c': sdid `out' fips year Treated	///
								if sample == 1,				///
								vce(placebo) 				///
								`covars'					///
								reps(`reps')				///
								graph graph_export("`path'", .pdf)
						}

						if _rc != 0 {
							local _failed_rc = _rc
							dis "SDID failed for `out' c=`c' exl=`exl'. Skipping."
							sdid_log_failure, rc(`_failed_rc') script("02_narrow_sdid") ///
								tableid("`out_txt'") outcome("`out'") c(`c') exl(`exl') ///
								samp("narrow") context("narrow-control-pool")
							continue
						}

						** Store results
						local tmp_tau = e(ATT)
						local tmp_se = e(se)

						** Pre-treatment mean and county count
						qui summ `out' if multnomah == 1 & Treated == 0 & sample == 1
						local tmp_premean = r(mean)
						estadd scalar mean = r(mean)

						qui summ `out' if year == 2021 & sample == 1
						local tmp_ncounties = r(N)
						estadd scalar count = r(N)

						** Save treatment effects
						preserve
						clear
						qui set obs 1
						gen sample_data = "`out_txt'"
						gen sample = "sample_narrow"
						gen outcome = "`out'"
						gen controls = `c'
						gen exclusion = `exl'
						gen tau = `tmp_tau'
						gen se = `tmp_se'
						gen pval = 2 * (1 - normal(abs(tau/se)))
						gen ci_lower = tau - 1.96 * se
						gen ci_upper = tau + 1.96 * se
						gen n_counties = `tmp_ncounties'
						gen pre_mean = `tmp_premean'
						gen significant = abs(tau/se) > 1.96
						order sample_data sample outcome controls exclusion	///
							tau se pval ci_lower ci_upper n_counties pre_mean significant
						append using "${results}sdid/narrow/narrow_sdid_results.dta"
						compress
						save "${results}sdid/narrow/narrow_sdid_results.dta", replace
						clear
						restore

						** Run event study
						capture noisily {
							sdid_event `out' fips year Treated	///
								if sample == 1,				///
								`covars_event'				///
								vce(placebo) 				///
								brep(`reps') 				///
								placebo(all)
						}

						local event_rc = _rc
						capture drop ever_treated*

						if `event_rc' == 0 {

							** Store max year
							qui summ year if multnomah == 1 & sample == 1
							local max_yr = r(max)

							** Extract matrix
							qui count if multnomah == 1 & sample == 1
							local ct = r(N)
							local ct = `ct' + 1

							capture mat res = e(H)[2..`ct',1..5]
							if _rc != 0 {
								continue
							}

							** Preserve and plot
							preserve
							svmat res
							gen id = `max_yr' - _n + 1 if !missing(res1)

							if `exl' == 1 {
								replace id = id - 1 if id <= 2020
								expand 2 if id == 2019, gen(tag)
								replace id = 2020 if tag == 1
								replace res1 = . if tag == 1
								replace res3 = . if tag == 1
								replace res4 = . if tag == 1
							}
							label var id "Year (destination)"
							sort id

							twoway	(rcap res3 res4 id, lc(gs10) fc(gs11%50))	///
									(scatter res1 id, mc(black)),				///
								legend(off) ytitle("`label'")					///
								yline(0, lc("`col_zero'") lp(-))						///
								xline(2020.5, lc(black) lp(solid))				///
								ylabel(-10(2.5)10, format(%9.1f))

							if `exl' == 1 local evpath "${results}sdid/narrow/`out_txt'/fig_`out_txt'_`out'_`c'_excl2020_eventstudy.jpg"
							else local evpath "${results}sdid/narrow/`out_txt'/fig_`out_txt'_`out'_`c'_eventstudy.jpg"

							graph export "`evpath'", as(jpg) name("Graph") quality(100) replace
							restore
						}

					} // END COVAR LOOP

				} // END OUTCOME LOOP

				** Table
				if `exl' == 0 local tabpath "${results}sdid/narrow/`out_txt'/tab_sdid_`out_txt'_`migr'.tex"
				if `exl' == 1 local tabpath "${results}sdid/narrow/`out_txt'/tab_sdid_`out_txt'_`migr'_excl2020.tex"

				** Save table locally and to Overleaf
				local _dests `""`tabpath'""'
				if ${overleaf} == 1 {
					if `exl' == 0 local ol_fname "tab_sdid_`out_txt'_`migr'.tex"
					if `exl' == 1 local ol_fname "tab_sdid_`out_txt'_`migr'_excl2020.tex"
					local _dests `"`_dests' "${ol_tab}`ol_fname'""'
				}

				foreach _outfile of local _dests {

				if "`data'" == "irs_sample_1" {
					capture noisily {
						esttab	sdid_n1_`migr'_rate_`type'_0 sdid_n1_`migr'_rate_`type'_1	///
								sdid_n2_`migr'_rate_`type'_0 sdid_n2_`migr'_rate_`type'_1	///
								sdid_agi_`migr'_rate_`type'_0 sdid_agi_`migr'_rate_`type'_1 ///
							using "`_outfile'",								///
						starlevel("*" 0.10 "**" 0.05 "***" 0.01)		///
						b(%-9.3f) se(%-9.3f) replace 					///
						mgroups("Returns" "Exemptions" "AGI", 			///
							pattern(1 0 1 0 1 0))						///
						mtitle(	"No Covariates" "Covariates"			///
								"No Covariates" "Covariates"			///
								"No Covariates" "Covariates")			///
						stats(count mean, 								///
							fmt(%9.0fc %9.3fc) 							///
							labels("Number of Counties" "Pre-treatment mean"))
					}
				}
				else {
					capture noisily {
						esttab	sdid_n1_`migr'_rate_`type'_0 sdid_n1_`migr'_rate_`type'_1	///
								sdid_n2_`migr'_rate_`type'_0 sdid_n2_`migr'_rate_`type'_1	///
								sdid_agi_`migr'_rate_`type'_0 sdid_agi_`migr'_rate_`type'_1 ///
							using "`_outfile'",								///
						starlevel("*" 0.10 "**" 0.05 "***" 0.01)		///
						b(%-9.3f) se(%-9.3f) replace 					///
						mgroups("Households" "Adults" "Household Income",	///
							pattern(1 0 1 0 1 0))						///
						mtitle(	"No Covariates" "Covariates"			///
								"No Covariates" "Covariates"			///
								"No Covariates" "Covariates")			///
						stats(count mean, 								///
							fmt(%9.0fc %9.3fc) 							///
							labels("Number of Counties" "Pre-treatment mean"))
					}
				}

				} // end foreach _outfile

			} // END MIGRATION TYPE LOOP

			** Drop sample var
			drop sample

		} // END EXCLUSION LOOP

	} // END OUT TYPE

} // END DATA LOOP


** Export results
use "${results}sdid/narrow/narrow_sdid_results.dta", clear
export excel using "${results}sdid/narrow/narrow_sdid_results.xlsx", firstrow(variables) replace

********************************************************************************
** SPECIFICATION CURVE ANALYSIS
********************************************************************************

/*******************************************************************************
Creates specification curve plots showing treatment effects across all
narrow SDID specifications for each outcome type and migration direction.

Coefficient colors:
- Sea (p7): Statistically significant (p<0.05), not preferred
- Sky (p3): Statistically insignificant, not preferred
- Vermillion (p6): Statistically significant (p<0.05), preferred specification
- Orangebrown (p8): Statistically insignificant, preferred specification

Preferred specifications are defined below.
*******************************************************************************/

** Load treatment effects
use "${results}sdid/narrow/narrow_sdid_results.dta", clear

** Parse outcome / sample_data into spec metadata + spec_* indicator
** family via the shared helper (01a_programs.do). Narrow data has no
** IRS-389 or outstate variants — those branches just stay empty here.
project_parse_outcome_components, indicators

** Calculate statistical significance (p < 0.05)
replace significant = pval < 0.05 if missing(significant)

********************************************************************************
** DEFINE PREFERRED SPECIFICATIONS
** Modify these conditions to change which specifications are highlighted
** as "preferred" in the specification curve plots.
********************************************************************************

gen preferred = 0

** IRS: with covariates, excluding 2020
replace preferred = 1 if 									///
	data_type == "IRS" & 									///
	controls == 1 &											///
	exclusion == 1 											//

** ACS COLLEGE: 16-24, with covariates, excluding 2020
replace preferred = 1 if 									///
	data_type == "ACS College" & 							///
	period_type == "16-24" & 								///
	controls == 1 &											///
	exclusion == 1 											//

** Display count of preferred specifications
dis "Number of preferred specifications: "
count if preferred == 1

********************************************************************************
** CREATE SPECIFICATION CURVE PLOTS
********************************************************************************

foreach otype in "n1" "n2" "agi" {
	foreach migr in "net" "in" "out" {

		** Preserve full data
		preserve

		** Keep only relevant specifications
		keep if outcome_type == "`otype'" & migration == "`migr'"

		** Check if we have data
		qui count
		if r(N) == 0 {
			restore
			continue
		}

		** Sort by effect size and create rank
		sort tau
		gen spec_rank = _n
		local n_specs = _N

		********************************************************************************
		** Create variables for significance and preferred-based coloring
		********************************************************************************

		** Significant, not preferred
		gen tau_sig_notpref = tau if significant == 1 & preferred == 0
		gen ci_lo_sig_notpref = ci_lower if significant == 1 & preferred == 0
		gen ci_hi_sig_notpref = ci_upper if significant == 1 & preferred == 0

		** Insignificant, not preferred
		gen tau_insig_notpref = tau if significant == 0 & preferred == 0
		gen ci_lo_insig_notpref = ci_lower if significant == 0 & preferred == 0
		gen ci_hi_insig_notpref = ci_upper if significant == 0 & preferred == 0

		** Significant, preferred
		gen tau_sig_pref = tau if significant == 1 & preferred == 1
		gen ci_lo_sig_pref = ci_lower if significant == 1 & preferred == 1
		gen ci_hi_sig_pref = ci_upper if significant == 1 & preferred == 1

		** Insignificant, preferred
		gen tau_insig_pref = tau if significant == 0 & preferred == 1
		gen ci_lo_insig_pref = ci_lower if significant == 0 & preferred == 1
		gen ci_hi_insig_pref = ci_upper if significant == 0 & preferred == 1

		********************************************************************************
		** Upper panel: Coefficient plot with CIs
		********************************************************************************
		** Compute dynamic placement for indicator zone
		qui su ci_lower
		local ci_min = r(min)
		qui su ci_upper
		local ci_max = r(max)
		local sep_y = floor(`ci_min') - 1.5
		local ind_top = floor(`ci_min') - 3
		local tick_lo = floor(`ci_min')
		local tick_hi = ceil(`ci_max')

		** 7 indicator rows
		local yp1 = `ind_top'
		local yp2 = `ind_top' - 1
		local yp3 = `ind_top' - 2
		local yp4 = `ind_top' - 3
		local yp5 = `ind_top' - 4
		local yp6 = `ind_top' - 5
		local yp7 = `ind_top' - 6

		gen y_covars  = `yp1' if spec_covars == 1
		gen y_excl    = `yp2' if spec_excl2020 == 1
		gen y_irs     = `yp3' if spec_irs == 1
		gen y_acs_all = `yp4' if spec_acs_all == 1
		gen y_acs_col = `yp5' if spec_acs_col == 1
		gen y_16_22   = `yp6' if spec_16_22 == 1
		gen y_16_24   = `yp7' if spec_16_24 == 1

		** Single unified specification curve
		twoway 	(rcap ci_lo_sig_notpref ci_hi_sig_notpref spec_rank, 		///
					lc("`col_sig_notpref'") lw(vthin)) 						///
				(rcap ci_lo_insig_notpref ci_hi_insig_notpref spec_rank, 	///
					lc("`col_insig_notpref'") lw(vthin)) 					///
				(rcap ci_lo_sig_pref ci_hi_sig_pref spec_rank, 				///
					lc("`col_sig_pref'") lw(thin)) 							///
				(rcap ci_lo_insig_pref ci_hi_insig_pref spec_rank, 			///
					lc("`col_insig_pref'") lw(thin)) 						///
				(scatter tau_sig_notpref spec_rank, 						///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter tau_insig_notpref spec_rank, 						///
					mc("`col_insig_notpref'") ms(O) msize(vsmall)) 		///
				(scatter tau_sig_pref spec_rank, 							///
					mc("`col_sig_pref'") ms(D) msize(small)) 				///
				(scatter tau_insig_pref spec_rank, 							///
					mc("`col_insig_pref'") ms(D) msize(small)) 			///
				(scatter y_covars spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_excl spec_rank, 									///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_irs spec_rank, 									///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_acs_all spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_acs_col spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_16_22 spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_16_24 spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)), 			///
			yline(`sep_y', lc(gs12) lp(solid) lw(vthin)) 					///
			yline(0, lc("`col_zero'") lp(dash)) 							///
			ylabel(`tick_lo'(1)`tick_hi', labsize(vsmall) nogrid) 			///
			ylabel(`yp1' "Covariates" 										///
				   `yp2' "Excl. 2020" 										///
				   `yp3' "IRS" 												///
				   `yp4' "ACS All" 											///
				   `yp5' "ACS College" 										///
				   `yp6' "16-22" 											///
				   `yp7' "16-24", 											///
				labsize(vsmall) angle(0) notick nogrid add) 				///
			legend(order(5 "Sig. (p<0.05)" 6 "Insig." 						///
						 7 "Sig., Preferred" 8 "Insig., Preferred") 		///
				   rows(1) pos(6) size(vsmall)) 							///
			ytitle("Treatment Effect (pp)", size(vsmall)) 					///
			xtitle("Specification (ranked by effect size)", size(vsmall)) 	///
			xlabel(none) 													///
			xscale(range(0.5 `=`n_specs'+0.5')) 							///
			graphregion(color(white)) 										///
			name(speccurve_`otype'_`migr', replace)

		** Export figure
		graph export "${results}sdid/narrow/fig_speccurve_narrow_`otype'_`migr'.pdf", replace
		graph export "${results}sdid/narrow/fig_speccurve_narrow_`otype'_`migr'.jpg", as(jpg) quality(100) replace
		if ${overleaf} == 1 {
			graph export "${ol_fig}fig_speccurve_narrow_`otype'_`migr'.pdf", replace
		}

		** Clean up
		graph drop speccurve_`otype'_`migr'

		restore

	} // END MIGRATION LOOP
} // END OUTCOME TYPE LOOP

********************************************************************************
** FINISH
********************************************************************************

dis ""
dis "=============================================="
dis "NARROW SDID ANALYSIS COMPLETE"
dis "=============================================="
dis "Results saved to:"
dis "  - ${results}sdid/narrow/narrow_sdid_results.dta"
dis "  - ${results}sdid/narrow/*/tab_sdid_*.tex"
dis "  - ${results}sdid/narrow/*/fig_*_eventstudy.jpg"
dis "  - ${results}sdid/narrow/fig_speccurve_narrow_*.pdf"
dis "=============================================="

** Close log
clear
log close log_02_narrow
