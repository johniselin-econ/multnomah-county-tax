/*******************************************************************************
File Name: 		02_sdid_analysis_parallel.do
Creator: 		John Iselin (parallelized version)
Date Update:	January 13, 2025

Called by: 00_multnomah.do

Purpose: Perform synthetic difference-in-difference estimation using parallel
         processing to speed up computation across multiple specifications.

         Each parallel child runs all 6 specifications for one table:
         - 3 outcome variables (n1, n2, agi) for a given migration type
         - Each with 2 covariate settings (with/without controls)
         This allows tables to be generated within each child process.

Requirements:
- parallel package: net install parallel, from(https://raw.github.com/gvegayon/parallel/stable/) replace

Outputs:
- sdid_weights.dta/xlsx: Synthetic control weights for each specification
- sdid_results.dta/xlsx: Treatment effects, SEs, p-values for each specification
- tab_sdid_*.tex: LaTeX tables for each specification group
- fig_speccurve_*.pdf/jpg: Specification curve plots

Authors: John Iselin

For more information, contact john.iselin@yale.edu

*******************************************************************************/

** Start log file
capture log close log_02
log using "${logs}02_log_sdid_parallel_${date}", replace text name(log_02)

/*******************************************************************************
SECTION 1: SETUP AND CONFIGURATION
*******************************************************************************/

** Set number of parallel clusters (adjust based on your machine)
** Use fewer than total cores to leave resources for system
local n_clusters = 6

** Parameters
local reps = 100

** Initialize parallel processing
parallel initialize `n_clusters', force

/*******************************************************************************
SECTION 2: DATA PREPARATION
(Same as original - prepares main analysis dataset)
*******************************************************************************/

** Load data
use "${data}working/irs_county_gross", replace

** Keep required variables
keep year fips state* county* *_net_3 *_out_1 *_out_2 *_in_3 *_out_3
order year fips state* county*

** Merge with ACS Data
merge 1:1 year fips using "${data}working/acs_county_gross_18plus", gen(merge_acs_1)

** Keep require variables
keep year fips state* county* *_net_3 *_out_1 *_out_2 *_in_3 *_out_3 merge_acs_*

** Label acs samples
rename persons_* acs1_persons_*
rename households_* acs1_households_*
rename dollars_* acs1_dollars_*

** Merge with ACS Data
merge 1:1 year fips using "${data}working/acs_county_gross_college", gen(merge_acs_2)

** Keep require variables
keep year fips state* county* *_net_3 *_out_1 *_out_2 *_in_3 *_out_3  merge_acs_*

** Label acs samples
rename persons_* acs2_persons_*
rename households_* acs2_households_*
rename dollars_* acs2_dollars_*

** Drop "other counties"
drop if county_fips == 0
drop if year == 2015

** Merge with Demographic data
merge m:1 fips using "${data}working/demographics_2020", 	///
	gen(demo_merge) keep(master match)

** Show match
tab state_name demo_merge, m
tab year demo_merge, m

** Keep if matched
keep if demo_merge == 3
drop demo_merge

** Rename
rename population pop_census

** Merge with Demographic data
merge m:1 year fips using "${data}working/bea_economics", 	///
	gen(econ_merge) keep(master match)

** Show match
tab state_name econ_merge, m
tab year econ_merge, m

** Keep if matched
keep if econ_merge == 3
drop econ_merge

** Merge with COVID-19 Data
merge m:1 fips using ${data}working/covid_cleaned_wide.dta, 	///
	gen(covid_merge) keep(master match )

** Show match
tab state_name covid_merge, m
tab year covid_merge, m

** Merge with Property Tax Rates (time-varying)
merge m:1 year fips using "${data}working/property_tax_rates_overall", ///
	gen(proptx_merge) keep(master match) keepusing(prop_rate_mean prop_rate_se)

** Show match
tab state_name proptx_merge, m
tab year proptx_merge, m

** Rename for clarity
rename prop_rate_mean prop_tax_rate
rename prop_rate_se prop_tax_rate_se
label var prop_tax_rate "Mean property tax rate (% of home value)"
label var prop_tax_rate_se "SE of property tax rate"

** Organize data
order year fips state_* county_*
sort fips year
isid fips year

** Keep only sample with non-missing base populations
tab county_name year if (missing(n1_out_1 ) | n1_out_1 == 0 ) & year <= 2022
drop if (missing(n1_out_1 ) | n1_out_1 == 0 ) & year <= 2022

** Keep only sample with observations in each year
bysort fips: gen ct = _N
tab county_name state_name if ct < 7
drop if ct < 7
drop ct

** Generate IRS sample
gen irs_sample_1 = inrange(year, 2016, 2022)
gen irs_sample_2 = inrange(year, 2016, 2022) & merge_acs_1 != 1

** Generate ACS Period Indicators
gen acs_period_1 = merge_acs_1 != 1 & inrange(year, 2016, 2022)
gen acs_period_2 = merge_acs_1 != 1

** Make sure we have a balanced panel of ACS counties
gen tmp = merge_acs_1 != 1
bysort fips: egen ct_tmp = total(tmp)
replace acs_period_1 = 0 if ct_tmp != 9
replace acs_period_2 = 0 if ct_tmp != 9
replace irs_sample_2 = 0 if ct_tmp != 9
drop tmp ct_tmp

tab year irs_sample_1
tab year irs_sample_2
tab year acs_period_1
tab year acs_period_2
tab state_name acs_period_1

** Define treated state
gen multnomah = state_fips == 41 & county_fips == 51
label var multnomah "Indicator for Multnomah County, Oregon"

** Define treatment indicator
gen Treated = multnomah == 1 & year > 2020
label var Treated "Treatment indicator for Multnomah County, Oregon"

** Define sample 1: All counties
gen sample_all = 1
label var sample_all "All counties (excluding AK, CA, HI OR, WA)"

** Define sample 2: Counties in top 95 percent
summ percent_urban if year == 2020, de
local cutoff = r(p95)
tab state_name multnomah if percent_urban >= `cutoff' & year == 2020
gen sample_urban95 = percent_urban >= `cutoff' // All counties
label var sample_urban95 "Urban counties (top 5%) (excluding AK, CA, HI OR, WA)"
tab sample_urban95 if year == 2020

** Define sample 3: Counties in top 98 percent
local cutoff = .9864539 //
tab state_name multnomah if percent_urban >= `cutoff' & year == 2020
gen sample_urban98 = percent_urban >= `cutoff' // All counties
label var sample_urban98 "Urban counties (top 1%) (excluding AK, CA, HI OR, WA)"
tab sample_urban98 if year == 2020

** Define Sample of States
drop if state_name == "Alaska"
drop if state_name == "Hawaii"
drop if state_name == "California"
drop if state_name == "Washington"
drop if state_name == "Oregon" & multnomah == 0

** Define sample 4: Counties in top 95 + covid
cluster kmeans cases_cum* deaths_cum* if 	///
	sample_urban95 == 1 & year == 2020 & covid_merge == 3 , k(5) gen(kmean)
bysort fips: egen kmean_group = mean(kmean)

** Pull out kmeans cluster with Multnoma
gen tmp1 = kmean if sample_urban95 == 1 & year == 2020 & covid_merge == 3 & multnomah == 1
egen tmp2 = mean(tmp1)
gen sample_urban95_covid = sample_urban95 == 1 & kmean_group == tmp2
drop tmp1 tmp2
label var sample_urban95_covid "Urban counties (top 5%) w. Kmean Covid Match  (excluding AK, CA, HI OR, WA)"
tab sample_urban95_covid if year == 2020

** Show results across clusters
preserve

** Keep required variables
keep if sample_urban95 == 1 & year == 2020 & covid_merge == 3
keep kmean cases_cum* deaths_cum* population
collapse (mean) cases_cum* deaths_cum* [fw = population], by(kmean)
reshape long cases_cum deaths_cum, i(kmean) j(time)
xtset kmean time
xtline cases_cum
graph export "${results}sdid/fig_kmeans.jpg", as(jpg) name("Graph") quality(100) replace
clear

** Restore
restore

** Define covariates
local covariates "population per_capita_income"

** Standardize covariates
foreach v of local covariates {
	egen tmp_v = std(`v')
	replace `v' = tmp_v
	drop tmp_v
} // END COVAR LOOP

** Define outcome variables (IRS)
foreach x in "n1" "n2" "agi" {

	if "`x'" == "n1" local xtxt "returns"
	else if "`x'" == "n2" local xtxt "exemptions"
	else if "`x'" == "agi" local xtxt "AGI"

	** Loop over migration type
	foreach y in "net" "in" "out" {

			if "`y'" == "net" local ytxt "Net domestic migration"
			else if "`y'" == "in" local ytxt "Domestic in-migration"
			else if "`y'" == "out" local ytxt "Domestic out-migration"

			** Generate
			gen `x'_`y'_rate_irs = 100 * (`x'_`y'_3 / (`x'_out_1 + `x'_out_2))

			** Label var
			label var `x'_`y'_rate_irs	"`ytxt' rate, `xtxt' (%)"

	} // END MIGRATION TYPE LOOP

} // END OUTCOME TYPE LOOP

** Define outcome variables (ACS)

** Rename for loop
rename acs*_households_* acs*_n1_*
rename acs*_persons_* acs*_n2_*
rename acs*_dollars_* acs*_agi_*

** Loop over sample
** (18+ = 1, college degree == 2, no college degree == 3)
forvalues i = 1/2{

	if `i' == 1 local itxt ""
	else if `i' == 2 local itxt " (College)"
	else if `i' == 3 local itxt " (No College)"


	** Define outcome variables (IRS)
	foreach x in "n1" "n2" "agi" {

		if "`x'" == "n1" local xtxt "HHs"
		else if "`x'" == "n2" local xtxt "persons"
		else if "`x'" == "agi" local xtxt "total income"


		** Loop over migration type
		foreach y in "net" "in" "out" {

				if "`y'" == "net" local ytxt "Net domestic migration"
				else if "`y'" == "in" local ytxt "Domestic in-migration"
				else if "`y'" == "out" local ytxt "Domestic out-migration"

				** Generate
				gen `x'_`y'_rate_acs`i' = 100 * (acs`i'_`x'_`y'_3 / (acs`i'_`x'_out_1 + acs`i'_`x'_out_2))

				** Label var
				label var `x'_`y'_rate_acs`i' "`ytxt' rate, `xtxt'`itxt' (%)"

		} // END MIGRATION TYPE LOOP

	} // END OUTCOME TYPE LOOP

} // END SAMPLE LOOP

** Declare panel
xtset fips year

** Tag unique fips
egen unique = tag(fips)
tab year unique

** Label var
label var year "Year (destination)"

/*******************************************************************************
SECTION 3: CREATE TABLE-LEVEL SPECIFICATION GRID
Each row represents one table = 6 SDID specs (3 outcomes × 2 covariate settings)
*******************************************************************************/

** Save main analysis data
save "${data}working/sdid_analysis_data.dta", replace

** Create table-level specification grid
preserve
clear

** Table units are defined by:
** - data_var (irs_sample_1, irs_sample_2, acs_period_1, acs_period_2)
** - out_type (irs, acs1, acs2) - but tied to data_var
** - samp_var (sample_all, sample_urban95, sample_urban95_covid)
** - exclusion (0, 1)
** - migr_type (net, in, out)
** Total: 6 data-type combos × 3 samples × 2 exclusions × 3 migrations = 108 tables

local table_id = 0

** Initialize empty dataset
set obs 0
gen table_id = .
gen data_var = ""
gen out_type = ""
gen out_txt = ""
gen samp_var = ""
gen exclusion = .
gen migr_type = ""

save "${data}working/table_grid.dta", replace

** Build table grid
foreach data in "irs_sample_1" "irs_sample_2" "acs_period_1" "acs_period_2" {

	** Different sets of outcome variable types
	if "`data'" == "irs_sample_1" | "`data'" == "irs_sample_2" {
		local out_types "irs"
	}
	else {
		local out_types "acs1 acs2"
	}

	foreach type of local out_types {

		** Labels
		if "`data'" == "irs_sample_1" local out_txt "irs_full_16_22"
		else if "`data'" == "irs_sample_2" local out_txt "irs_389_16_22"
		else if "`data'" == "acs_period_1" & "`type'" == "acs1" local out_txt "acs_16_22_all"
		else if "`data'" == "acs_period_1" & "`type'" == "acs2" local out_txt "acs_16_22_col"
		else if "`data'" == "acs_period_2" & "`type'" == "acs1" local out_txt "acs_16_24_all"
		else if "`data'" == "acs_period_2" & "`type'" == "acs2" local out_txt "acs_16_24_col"

		foreach samp in "sample_all" "sample_urban95" "sample_urban95_covid" {
			forvalues exl = 0/1 {
				foreach migr in "net" "in" "out" {

					local table_id = `table_id' + 1

					** Add row to grid
					clear
					set obs 1
					gen table_id = `table_id'
					gen data_var = "`data'"
					gen out_type = "`type'"
					gen out_txt = "`out_txt'"
					gen samp_var = "`samp'"
					gen exclusion = `exl'
					gen migr_type = "`migr'"

					append using "${data}working/table_grid.dta"
					save "${data}working/table_grid.dta", replace

				}
			}
		}
	}
}

** Load and verify grid
use "${data}working/table_grid.dta", clear
dis "Total table units: " _N
sort table_id
save "${data}working/table_grid.dta", replace

restore

/*******************************************************************************
SECTION 4: DEFINE PARALLEL PROGRAM FOR TABLE-LEVEL SDID ESTIMATION
Each call runs 6 SDID specs and generates one table
*******************************************************************************/

** Define program to run all SDID specifications for one table
capture program drop run_sdid_table
program define run_sdid_table
	syntax, table_id(integer) data_path(string) results_path(string) reps(integer)

	** Load table specification from grid
	preserve
	use "`data_path'working/table_grid.dta", clear
	keep if table_id == `table_id'

	** Extract specification parameters
	local data_var = data_var[1]
	local out_type = out_type[1]
	local out_txt = out_txt[1]
	local samp_var = samp_var[1]
	local exl = exclusion[1]
	local migr = migr_type[1]

	restore

	** Load main analysis data
	use "`data_path'working/sdid_analysis_data.dta", clear

	** Define sample
	gen sample = `samp_var' == 1 & `data_var' == 1
	if `exl' == 1 replace sample = 0 if year == 2020

	** Skip if no treated unit in sample
	qui count if multnomah == 1 & sample == 1
	if r(N) == 0 {
		dis "Skipping table `table_id': no treated unit in sample"
		exit
	}

	** Create output directory
	capture mkdir "`results_path'sdid/`out_txt'"

	** Define covariates
	if "`data_var'" == "irs_sample_1" local covariates "population per_capita_income"
	else local covariates "population per_capita_income prop_tax_rate"

	** Clear stored estimates
	eststo clear

	** Loop over outcomes (n1, n2, agi) and covariate settings (0, 1)
	foreach outvar in "n1" "n2" "agi" {

		** Full outcome variable name
		local outcome "`outvar'_`migr'_rate_`out_type'"

		** Store label
		local label : variable label `outcome'

		** Loop over covariate settings
		forvalues c = 0/1 {

			** Covariates for sdid (supports 'projected' option)
			if `c' == 0 local covars ""
			else if `c' == 1 local covars "covariates(`covariates', projected)"

			** Covariates for sdid_event (doesn't support 'projected' option)
			if `c' == 0 local covars_event ""
			else if `c' == 1 local covars_event "covariates(`covariates')"

			** File paths for figures
			if `exl' == 0 local path "`results_path'sdid/`out_txt'/fig_`out_txt'_`outcome'_`c'_`samp_var'_"
			if `exl' == 1 local path "`results_path'sdid/`out_txt'/fig_`out_txt'_`outcome'_`c'_`samp_var'_excl2020_"

			** Run SDID
			capture noisily {
				eststo sdid_`outvar'_`c': sdid `outcome' fips year Treated	///
					if sample == 1,			 	///
					vce(placebo) 				///
					`covars'					///
					reps(`reps')				///
					graph graph_export("`path'", .pdf)
			}

			** Store results
			local tmp_tau = e(ATT)
			local tmp_se = e(se)
			matrix omega = e(omega)

			** Pre-treatment mean and county count
			qui summ `outcome' if multnomah == 1 & Treated == 0
			local tmp_premean = r(mean)
			estadd scalar mean = r(mean)

			qui summ `outcome' if year == 2021 & sample == 1
			local tmp_ncounties = r(N)
			estadd scalar count = r(N)

			** Save treatment effect results
			preserve
			clear
			set obs 1
			gen table_id = `table_id'
			gen sample_data = "`out_txt'"
			gen sample = "`samp_var'"
			gen outcome = "`outcome'"
			gen controls = `c'
			gen exclusion = `exl'
			gen tau = `tmp_tau'
			gen se = `tmp_se'
			gen pval = 2 * (1 - normal(abs(tau/se)))
			gen ci_lower = tau - 1.96 * se
			gen ci_upper = tau + 1.96 * se
			gen n_counties = `tmp_ncounties'
			gen pre_mean = `tmp_premean'
			save "`results_path'sdid/temp_results/results_`table_id'_`outvar'_`c'.dta", replace
			restore

			** Save weights
			preserve
			clear
			svmat double omega, names(col)
			rename c2 fips
			rename c1 weight
			gen table_id = `table_id'
			gen sample_data = "`out_txt'"
			gen out = "`outcome'"
			gen sample = "`samp_var'"
			gen controls = `c'
			gen exclusion = `exl'
			drop if weight == 0
			drop if missing(fips)
			save "`results_path'sdid/temp_weights/weights_`table_id'_`outvar'_`c'.dta", replace
			restore


			** Run event study
			capture noisily {
				sdid_event `outcome' fips year Treated	///
					if sample == 1,			 			///
					`covars_event'						///
					vce(placebo) 						///
					brep(`reps') 						///
					placebo(all)
			}

			local event_rc = _rc

			if `event_rc' == 0 {

				** Store max year
				qui summ year if multnomah == 1 & sample == 1
				local max_yr = r(max)

				** Move results from matrix to data
				qui count if multnomah == 1 & sample == 1
				local ct = r(N)
				local ct = `ct' + 1

				** Extract matrix
				capture mat res = e(H)[2..`ct',1..5]
				if _rc != 0 {
					continue
				}

				** Preserve data before plotting (expand creates duplicate obs)
				preserve

				** Move to data
				svmat res

				** Generate ID variable
				gen id = `max_yr' - _n + 1 if !missing(res1)

				** Handle exclusion year labeling
				if `exl' == 1 {
					replace id = id - 1 if id <= 2020
					expand 2 if id == 2019, gen(tag)
					replace id = 2020 if tag == 1
					replace res1 = . if tag == 1
					replace res3 = . if tag == 1
					replace res4 = . if tag == 1
				}
				label var id "Year (destination)"

				** Sort
				sort id

				** Plot
				twoway 	(rcap res3 res4 id, lc(gs10) fc(gs11%50))	///
						(scatter res1 id, mc(black)),				///
					legend(off) ytitle("`label'") 					///
					yline(0, lc(red) lp(-)) 						///
					xline(2020.5, lc(black) lp(solid))				///
					ylabel(-10(2.5)10, format(%9.1f))

				if `exl' == 1 local evpath "`results_path'sdid/`out_txt'/fig_`out_txt'_`outcome'_`c'_`samp_var'_excl2020_eventstudy.jpg"
				else local evpath "`results_path'sdid/`out_txt'/fig_`out_txt'_`outcome'_`c'_`samp_var'_eventstudy.jpg"

				graph export "`evpath'", as(jpg) name("Graph") quality(100) replace

				** Restore data (removes expanded rows and temp variables)
				restore

			}
	
			** Clean up sdid_event internal variables (prevents "already defined" error on next iteration)
			capture drop ever_treated*

		} // END COVAR LOOP

	} // END OUTCOME LOOP

	** Generate table for this migration type (all 6 specs)
	** Determine table path
	if `exl' == 0 local tabpath "`results_path'sdid/`out_txt'/tab_sdid_`out_txt'_`migr'_`samp_var'.tex"
	if `exl' == 1 local tabpath "`results_path'sdid/`out_txt'/tab_sdid_`out_txt'_`migr'_`samp_var'_excl2020.tex"

	** Check if we have all estimates
	capture confirm matrix e(b)

	** Generate table based on data type
	if "`data_var'" == "irs_sample_1" | "`data_var'" == "irs_sample_2" {
		capture noisily {
			esttab 	sdid_n1_0 sdid_n1_1	///
					sdid_n2_0 sdid_n2_1	///
					sdid_agi_0 sdid_agi_1 ///
				using "`tabpath'",								///
				starlevel("*" 0.10 "**" 0.05 "***" 0.01)		///
				b(%-9.3f) se(%-9.3f) replace 					///
				mgroups("Returns" "Exemptions" "AGI", 			///
					pattern(1 0 1 0 1 0) )						///
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
			esttab 	sdid_n1_0 sdid_n1_1	///
					sdid_n2_0 sdid_n2_1	///
					sdid_agi_0 sdid_agi_1 ///
				using "`tabpath'",								///
				starlevel("*" 0.10 "**" 0.05 "***" 0.01)		///
				b(%-9.3f) se(%-9.3f) replace 					///
				mgroups("Households" "Adults" "Household Income",	///
					pattern(1 0 1 0 1 0) )						///
				mtitle(	"No Covariates" "Covariates"			///
						"No Covariates" "Covariates"			///
						"No Covariates" "Covariates")			///
				stats(count mean, 								///
					fmt(%9.0fc %9.3fc) 							///
					labels("Number of Counties" "Pre-treatment mean"))
		}
	}

	dis "Completed table `table_id': `out_txt' / `migr' / `samp_var' / excl=`exl'"

end

/*******************************************************************************
SECTION 5: CREATE TEMPORARY DIRECTORIES AND RUN PARALLEL ESTIMATION
*******************************************************************************/

** Create temp directories for results
capture mkdir "${results}sdid/temp_results"
capture mkdir "${results}sdid/temp_weights"

** Create subfolders for each output type
foreach out_txt in "irs_full_16_22" "irs_389_16_22" "acs_16_22_all" "acs_16_22_col" "acs_16_24_all" "acs_16_24_col" {
	capture mkdir "${results}sdid/`out_txt'"
}

** Load table grid
use "${data}working/table_grid.dta", clear
local n_tables = _N
dis "Running `n_tables' table units in parallel (each with 6 SDID specs)..."

** Save table IDs for parallel processing
** IMPORTANT: Shuffle table order to balance workload across workers
** Without shuffling, child 1 gets all sample_all specs (heaviest computation)
preserve
keep table_id

** Generate random sort order and shuffle
gen rand_order = runiform()
sort rand_order
drop rand_order

save "${data}working/table_ids.dta", replace
restore

** Define wrapper program for parallel execution
capture program drop parallel_sdid_wrapper
program define parallel_sdid_wrapper
	** Store all table_ids upfront (run_sdid_table will overwrite the dataset)
	local n_obs = _N
	forvalues i = 1/`n_obs' {
		local tid_`i' = table_id[`i']
	}

	** Now loop through and process each table
	forvalues i = 1/`n_obs' {
		dis "Worker processing table `tid_`i'' (`i' of `n_obs' in this chunk)"
		run_sdid_table, table_id(`tid_`i'') data_path("${data}") results_path("${results}") reps(100)
	}
end

** Load table IDs and run in parallel
use "${data}working/table_ids.dta", clear

** Run parallel estimation
dis "Starting parallel SDID estimation at $S_TIME..."
dis "Each parallel worker processes one table (6 SDID specifications)"
timer clear 1
timer on 1

parallel, prog(parallel_sdid_wrapper run_sdid_table): parallel_sdid_wrapper

timer off 1
timer list 1
dis "Parallel estimation completed at $S_TIME"

/*******************************************************************************
SECTION 6: COMBINE PARALLEL RESULTS
*******************************************************************************/

dis "Combining results from parallel workers..."

** Combine all treatment effect results
clear
local files : dir "${results}sdid/temp_results" files "results_*.dta"
local first = 1

foreach f of local files {
	if `first' == 1 {
		use "${results}sdid/temp_results/`f'", clear
		local first = 0
	}
	else {
		append using "${results}sdid/temp_results/`f'"
	}
}

** Save combined results
order sample_data sample outcome controls exclusion tau se pval ci_lower ci_upper n_counties pre_mean
compress
save "${results}sdid/sdid_results.dta", replace
export excel using "${results}sdid/sdid_results.xlsx", firstrow(variables) replace

** Combine all weights
clear
local files : dir "${results}sdid/temp_weights" files "weights_*.dta"
local first = 1

foreach f of local files {
	if `first' == 1 {
		use "${results}sdid/temp_weights/`f'", clear
		local first = 0
	}
	else {
		append using "${results}sdid/temp_weights/`f'"
	}
}

** Save combined weights
order sample_data sample out controls exclusion fips weight
sort sample_data sample out controls exclusion weight
compress
save "${results}sdid/sdid_weights.dta", replace
export excel using "${results}sdid/sdid_weights.xlsx", firstrow(variables) replace

** Clean up temp directories
shell rmdir "${results}sdid/temp_results" /s /q
shell rmdir "${results}sdid/temp_weights" /s /q

dis "Results combined and saved."

/*******************************************************************************
SECTION 7: SPECIFICATION CURVE ANALYSIS
*******************************************************************************/
/*
dis "Creating specification curve plots..."

** Load treatment effects
use "${results}sdid/sdid_results.dta", clear

** Parse outcome variable names to extract components
gen outcome_type = ""
replace outcome_type = "n1" if strpos(outcome, "n1_") > 0
replace outcome_type = "n2" if strpos(outcome, "n2_") > 0
replace outcome_type = "agi" if strpos(outcome, "agi_") > 0

gen migration = ""
replace migration = "net" if strpos(outcome, "_net_") > 0
replace migration = "in" if strpos(outcome, "_in_") > 0
replace migration = "out" if strpos(outcome, "_out_") > 0

gen data_type = ""
replace data_type = "IRS" if strpos(outcome, "_irs") > 0
replace data_type = "ACS All" if strpos(outcome, "_acs1") > 0
replace data_type = "ACS College" if strpos(outcome, "_acs2") > 0

** Create specification indicators for bottom panel
gen spec_all = sample == "sample_all"
gen spec_urban95 = sample == "sample_urban95"
gen spec_covid = sample == "sample_urban95_covid"
gen spec_covars = controls == 1
gen spec_excl2020 = exclusion == 1
gen spec_irs = data_type == "IRS"
gen spec_acs_all = data_type == "ACS All"
gen spec_acs_col = data_type == "ACS College"

** Loop over outcome types and migration directions
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

		** Labels for outcome type
		if "`otype'" == "n1" local otype_label "Returns/Households"
		else if "`otype'" == "n2" local otype_label "Exemptions/Persons"
		else if "`otype'" == "agi" local otype_label "AGI/Income"

		** Labels for migration
		if "`migr'" == "net" local migr_label "Net Migration"
		else if "`migr'" == "in" local migr_label "In-Migration"
		else if "`migr'" == "out" local migr_label "Out-Migration"

		** Create upper panel: Coefficient plot with CIs
		twoway 	(rcap ci_lower ci_upper spec_rank, lc(gs10) lw(thin))	///
				(scatter tau spec_rank, mc(navy) ms(O) msize(small)),	///
			legend(off)													///
			ytitle("Treatment Effect (pp)")								///
			xtitle("Specification (ranked by effect size)")				///
			title("`otype_label': `migr_label'", size(medium))			///
			yline(0, lc(red) lp(dash))									///
			xlabel(1(5)`n_specs')										///
			name(coef_`otype'_`migr', replace)

		** Create lower panel: Specification indicators
		gen y_all = -1 if spec_all == 1
		gen y_urban = -2 if spec_urban95 == 1
		gen y_covid = -3 if spec_covid == 1
		gen y_covars = -4 if spec_covars == 1
		gen y_excl = -5 if spec_excl2020 == 1
		gen y_irs = -6 if spec_irs == 1
		gen y_acs_all = -7 if spec_acs_all == 1
		gen y_acs_col = -8 if spec_acs_col == 1

		twoway 	(scatter y_all spec_rank, mc(navy) ms(O) msize(vsmall))		///
				(scatter y_urban spec_rank, mc(navy) ms(O) msize(vsmall))	///
				(scatter y_covid spec_rank, mc(navy) ms(O) msize(vsmall))	///
				(scatter y_covars spec_rank, mc(navy) ms(O) msize(vsmall))	///
				(scatter y_excl spec_rank, mc(navy) ms(O) msize(vsmall))	///
				(scatter y_irs spec_rank, mc(navy) ms(O) msize(vsmall))		///
				(scatter y_acs_all spec_rank, mc(navy) ms(O) msize(vsmall))	///
				(scatter y_acs_col spec_rank, mc(navy) ms(O) msize(vsmall)),///
			legend(off)														///
			ytitle("")														///
			xtitle("Specification")											///
			ylabel(	-1 "All Counties"										///
					-2 "Urban 95%"											///
					-3 "COVID Match"										///
					-4 "Covariates"											///
					-5 "Excl. 2020"											///
					-6 "IRS Data"											///
					-7 "ACS All"											///
					-8 "ACS College",										///
				angle(0) labsize(vsmall))									///
			xlabel(1(5)`n_specs')											///
			name(spec_`otype'_`migr', replace)

		** Combine panels
		graph combine coef_`otype'_`migr' spec_`otype'_`migr',				///
			cols(1)															///
			xcommon															///
			imargin(zero)													///
			title("Specification Curve: `otype_label' - `migr_label'", size(medium))

		** Export combined figure
		graph export "${results}sdid/fig_speccurve_`otype'_`migr'.pdf", replace
		graph export "${results}sdid/fig_speccurve_`otype'_`migr'.jpg", as(jpg) quality(100) replace

		** Clean up
		graph drop coef_`otype'_`migr' spec_`otype'_`migr'

		restore

	} // END MIGRATION LOOP
} // END OUTCOME TYPE LOOP
*/
/*******************************************************************************
SECTION 8: CLEANUP AND FINISH
*******************************************************************************/

** Clean up temporary files
capture erase "${data}working/sdid_analysis_data.dta"
capture erase "${data}working/table_grid.dta"
capture erase "${data}working/table_ids.dta"

** Report completion
dis ""
dis "=============================================="
dis "PARALLEL SDID ANALYSIS COMPLETE"
dis "=============================================="
dis "Results saved to:"
dis "  - ${results}sdid/sdid_results.dta"
dis "  - ${results}sdid/sdid_weights.dta"
dis "  - ${results}sdid/*/tab_sdid_*.tex (tables)"
dis "  - ${results}sdid/fig_speccurve_*.pdf"
dis "=============================================="

** Close log
clear
log close log_02
