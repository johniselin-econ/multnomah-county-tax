/*******************************************************************************
File Name: 		02_otherout_sdid.do
Creator: 		John Iselin
Date Update:	February 2026

Called by: 00_multnomah.do

Purpose: Perform synthetic difference-in-difference estimation on non-migration
         IRS county-level outcomes: returns, AGI, wages, and total income
         (all in logs, with BEA time-varying population as covariate).

         Supports both sequential and parallel processing modes.

         Designed for extensibility — additional datasets (e.g., unemployment,
         birth rates) can be added by following the template block below.

Requirements (for parallel mode):
- parallel package: net install parallel, from(https://raw.github.com/gvegayon/parallel/stable/) replace

Inputs:
- ${data}working/irs_county_all.dta (IRS county-level aggregates)
- ${data}working/demographics_2020.dta (demographics)
- ${data}working/bea_economics.dta (BEA time-varying population + PCI)
- ${data}working/covid_cleaned_wide.dta (COVID data)
- ${data}working/property_tax_rates_overall.dta (property tax rates)
- ${data}working/age_shares_county.dta (age shares for demographic k-means)

Outputs:
- ${results}sdid/otherout/ — Tables and event study figures
- otherout_sdid_results.dta: Treatment effects

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
** 01a_programs.do (project_set_seed, sdid_log_failure) and 02_spec_engine.do
** (fit_spec_sdid, load_spec_panel) are normally sourced earlier by the
** orchestrator; source them defensively so this script also runs standalone.
do "${code}01a_programs.do"
do "${code}02_spec_engine.do"


** Start log file
capture log close log_02_otherout
log using "${logs}02_log_otherout_sdid_${date}", replace text name(log_02_otherout)
project_set_seed, context("02_otherout_sdid.do") offset(100)

** plotplainblind palette (RGB) — consistent across all figures
local col_sig_notpref  "0 114 178"    // sea (p7) — sig, not preferred
local col_insig_notpref "86 180 233"  // sky (p3) — insig, not preferred
local col_sig_pref     "213 94 0"     // vermillion (p6) — sig, preferred
local col_insig_pref   "230 159 0"    // orangebrown (p8) — insig, preferred
local col_zero         "204 121 167"  // reddish (p5) — zero line
local col_ref          "153 153 153"  // gs10 (p2) — reference lines

** Number of bootstrap replications
local reps = 100

** Initialize parallel processing if enabled
if ${use_parallel} == 1 {
	parallel initialize ${n_clusters}, force
}

********************************************************************************
** DATA PREPARATION
********************************************************************************

** Load IRS county-level aggregates
use "${data}working/irs_county_all", clear

** Collapse to county X year
collapse (sum) 	n1 agi a_total_inc a_wage,	///
		by(fips year state_* county_* )
drop state_abb

** Keep years matching main analysis
keep if inrange(year, 2016, 2022)

** Drop counties without a county ID
drop if county_fips == 0

** Merge with Demographics (2020)
merge m:1 fips using "${data}working/demographics_2020", ///
	gen(demo_merge) keep(master match)
project_report_merge, gen(demo_merge) tag("demographics_2020") keep_merge
keep if demo_merge == 3
drop demo_merge

** Rename population from demographics
rename population pop_census

** Merge with BEA Economics (time-varying population)
merge m:1 year fips using "${data}working/bea_economics", ///
	gen(econ_merge) keep(master match)
project_report_merge, gen(econ_merge) tag("bea_economics") keep_merge
keep if econ_merge == 3
drop econ_merge

** Merge with COVID-19 Data
merge m:1 fips using "${data}working/covid_cleaned_wide.dta", ///
	gen(covid_merge) keep(master match)
project_report_merge, gen(covid_merge) tag("covid_wide") keep_merge

** Merge with Property Tax Rates
merge m:1 year fips using "${data}working/property_tax_rates_overall", ///
	gen(proptx_merge) keep(master match) keepusing(prop_rate_mean)
project_report_merge, gen(proptx_merge) tag("property_tax")

rename prop_rate_mean prop_tax_rate
label var prop_tax_rate "Mean property tax rate (% of home value)"

** Merge with Age Shares (for demographic k-means sample)
merge m:1 fips using "${data}working/age_shares_county", ///
	gen(age_merge) keep(master match)
project_report_merge, gen(age_merge) tag("age_shares")

** Merge with JII COVID Stringency Data
merge m:1 fips using "${data}working/jii_stringency.dta", ///
	gen(jii_merge) keep(master match)
project_report_merge, gen(jii_merge) tag("jii_stringency") keep_merge

** Organize
order year fips state_* county_*
sort fips year

** Keep balanced panel
** Note: balanced panel is required for SDID estimation
capture drop ct
bysort fips: gen ct = _N
qui summ ct 
drop if ct < `r(max)'
drop ct

********************************************************************************
** SAMPLE CONSTRUCTION
********************************************************************************

** Define treated county
gen multnomah = state_fips == 41 & county_fips == 51
label var multnomah "Indicator for Multnomah County, Oregon"

** Define treatment indicator
gen Treated = multnomah == 1 & year > 2020
label var Treated "Treatment indicator"

** Define sample 1: All counties
gen sample_all = 1
label var sample_all "All counties (excluding AK, CA, HI, OR, WA)"

** Define sample 2: Urban counties (top 5% urbanization)
qui summ percent_urban if year == 2020, de
local cutoff = r(p95)
gen sample_urban95 = percent_urban >= `cutoff'
label var sample_urban95 "Urban counties (top 5%)"

** Compute top-25% urban threshold for COVID/stringency clustering base
** Note: must be computed before state drops (like sample_urban95) so
**       Multnomah is evaluated against the full county distribution
qui summ percent_urban if year == 2020, de
local p75 = r(p75)
gen urban_top75 = percent_urban >= `p75'

** Drop states (same as main SDID)
drop if state_name == "Alaska"
drop if state_name == "Hawaii"
drop if state_name == "California"
drop if state_name == "Washington"
drop if state_name == "Oregon" & multnomah == 0

** Define sample 3: COVID k-means match
cluster kmeans cases_cum* deaths_cum* if ///
	urban_top75 == 1 & year == 2020 & covid_merge == 3, k(5) gen(kmean)
bysort fips: egen kmean_group = mean(kmean)

gen tmp1 = kmean if urban_top75 == 1 & year == 2020 & covid_merge == 3 & multnomah == 1
egen tmp2 = mean(tmp1)
gen sample_urban75_covid = urban_top75 == 1 & kmean_group == tmp2
drop tmp1 tmp2
label var sample_urban75_covid "Urban counties (top 25%) w. COVID k-means match"

** Define sample 4: Demographic k-means match
** Note: matches 02_sdid_analysis.do — clusters urban95 counties,
** includes percent_urban as a clustering variable
gen pci_pre = per_capita_income if year == 2020
bysort fips: egen pci_pre_fill = mean(pci_pre)
drop pci_pre
rename pci_pre_fill pci_pre

foreach v in pci_pre pop_census share_under_24 share_over_65 percent_urban {
	egen std_`v' = std(`v') if sample_urban95 == 1 & year == 2020
}

cluster kmeans std_pci_pre std_pop_census std_share_under_24 std_share_over_65 std_percent_urban if ///
	year == 2020 & !missing(share_under_24), k(5) gen(kmean_demog)
bysort fips: egen kmean_demog_group = mean(kmean_demog)

gen tmp1 = kmean_demog if year == 2020 & !missing(share_under_24) & multnomah == 1
egen tmp2 = mean(tmp1)
gen sample_demog = kmean_demog_group == tmp2
drop tmp1 tmp2 std_* pci_pre
label var sample_demog "Counties with Demographic Kmean Match (excluding AK, CA, HI OR, WA)"

** Define sample 5: COVID stringency k-means match (JII restriction-duration)
** Standardize 5 stringency vars within urban top-75%
foreach v in msahodays restclosedays gatherbandays strictgatherbandays maskpubdays {
	egen std_`v' = std(`v') if urban_top75 == 1 & year == 2020 & jii_merge == 3
}

** K-means on standardized stringency measures
cluster kmeans std_msahodays std_restclosedays std_gatherbandays 	///
	std_strictgatherbandays std_maskpubdays if 						///
	urban_top75 == 1 & year == 2020 & jii_merge == 3, k(5) gen(kmean_string)
bysort fips: egen kmean_string_group = mean(kmean_string)

** Identify Multnomah's cluster
gen tmp1 = kmean_string if urban_top75 == 1 & year == 2020 & jii_merge == 3 & multnomah == 1
egen tmp2 = mean(tmp1)
gen sample_stringency = urban_top75 == 1 & kmean_string_group == tmp2
drop tmp1 tmp2 std_* urban_top75 kmean_string kmean_string_group
label var sample_stringency "Urban counties (top 25%) w. COVID stringency k-means match"

********************************************************************************
** OUTCOME VARIABLES
********************************************************************************

** Set up outcome variables 
label var n1 "Number of returns"
label var agi "Adjusted Gross Income (AGI) ($1,000s USD)"
label var a_total_inc "Total taxable income ($1,000s USD)"
label var a_wage "Wage and salary income ($1,000s USD)"

** Get log form 
local outcomes "ln_n1 ln_agi ln_total_inc ln_wage"
gen ln_n1 = ln(n1)
gen ln_agi = ln(agi)
gen ln_total_inc = ln(a_total_inc)
gen ln_wage = ln(a_wage)

** Drop counties with negative or 0 values 
foreach x of local outcomes {
	drop if missing(`x')
}

bysort fips: gen ct = _N
qui summ ct 
drop if ct < `r(max)'
drop ct

** Short labels for tables and spec curve titles
local lbl_ln_n1 "Log Returns"
local lbl_ln_agi "Log AGI (\$1,000s)"
local lbl_ln_total_inc "Log Total Inc. (\$1,000s)"
local lbl_ln_wage "Log Wages (\$1,000s)"

** Standardize covariates
local all_covariates "population per_capita_income"
foreach v of local all_covariates {
	egen tmp_v = std(`v')
	replace `v' = tmp_v
	drop tmp_v
}

** Declare panel
xtset fips year

** Save prepared data (for parallel mode)
compress
save "${data}working/otherout_sdid_data.dta", replace

********************************************************************************
** OUTPUT SETUP
********************************************************************************

capture mkdir "${results}sdid/otherout"


********************************************************************************
** PARALLEL MODE: DEFINE PROGRAMS AND SETUP
********************************************************************************

if ${use_parallel} == 1 {

	** Create table-level specification grid
	** Table units are defined by:
	** - samp_var (sample_all, sample_urban95, sample_urban75_covid, sample_demog, sample_stringency)
	** - exclusion (0, 1)
	** Total: 5 x 2 = 10 table units
	** Each table unit runs 4 outcomes x 2 covariate settings = 8 SDID specs

	preserve
	clear

	local table_id = 0

	** Initialize empty dataset
	set obs 0
	gen table_id = .
	gen samp_var = ""
	gen exclusion = .

	save "${data}working/otherout_table_grid.dta", replace

	** Build table grid
	foreach samp in "sample_all" "sample_urban95" "sample_urban75_covid" "sample_demog" "sample_stringency" {
		forvalues exl = 0/1 {

			local table_id = `table_id' + 1

			** Add row to grid
			clear
			set obs 1
			gen table_id = `table_id'
			gen samp_var = "`samp'"
			gen exclusion = `exl'

			append using "${data}working/otherout_table_grid.dta"
			save "${data}working/otherout_table_grid.dta", replace

		}
	}

	** Load and verify grid
	use "${data}working/otherout_table_grid.dta", clear
	dis "Total otherout table units: " _N
	sort table_id
	save "${data}working/otherout_table_grid.dta", replace

	restore

	** Define program to run all SDID specifications for one table unit
	capture program drop run_otherout_table
	program define run_otherout_table
		syntax, table_id(integer) data_path(string) results_path(string) reps(integer)

		** Color palette (must be redefined inside program scope)
		local col_zero "204 121 167"

		** Load table specification from grid
		preserve
		use "`data_path'working/otherout_table_grid.dta", clear
		keep if table_id == `table_id'

		** Extract specification parameters
		local samp_var = samp_var[1]
		local exl = exclusion[1]

		restore

		** Load analysis data
		use "`data_path'working/otherout_sdid_data.dta", clear
		xtset fips year

		** Define sample
		gen sample = `samp_var' == 1
		if `exl' == 1 replace sample = 0 if year == 2020

		** Skip if no treated unit in sample
		qui count if multnomah == 1 & sample == 1
		if r(N) == 0 {
			dis "Skipping table `table_id': no treated unit in sample"
			exit
		}

		** Create output directory
		capture mkdir "`results_path'sdid/otherout"

		** Covariates
		local covariates "population per_capita_income"

		** Define outcomes (must be local to program scope)
		local outcomes "ln_n1 ln_agi ln_total_inc ln_wage"

		** Short labels for tables
		local lbl_ln_n1 "Log Returns"
		local lbl_ln_agi "Log AGI (\$1,000s)"
		local lbl_ln_total_inc "Log Total Inc. (\$1,000s)"
		local lbl_ln_wage "Log Wages (\$1,000s)"

		** Clear stored estimates
		eststo clear

		** Loop over outcome variables
		foreach out of local outcomes {

			** Store label
			local label : variable label `out'

			** Loop over inclusion of covariates
			forvalues c = 0/1 {

				** Covariates for sdid
				if `c' == 0 local covars ""
				else if `c' == 1 local covars "covariates(`covariates')"

				** Covariates for sdid_event
				if `c' == 0 local covars_event ""
				else if `c' == 1 local covars_event "covariates(`covariates')"

				** File paths for figures
				if `exl' == 0 local path "`results_path'sdid/otherout/fig_otherout_`out'_`c'_`samp_var'_"
				if `exl' == 1 local path "`results_path'sdid/otherout/fig_otherout_`out'_`c'_`samp_var'_excl2020_"

				** ─── Skip if result already exists (resume mode) ───
				if ${resume} == 1 {
					capture confirm file "`results_path'sdid/temp_otherout_results/results_`table_id'_`out'_`c'.dta"
					if _rc == 0 {
						dis "RESUME: Skipping table `table_id' `out' c=`c' (result exists)"
						continue
					}
				}

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
					dis "SDID failed for `out' c=`c' exl=`exl' samp=`samp_var'. Skipping."
					sdid_log_failure, rc(`_failed_rc') script("02_otherout_sdid") ///
						tableid("`table_id'") outcome("`out'") c(`c') exl(`exl') ///
						samp("`samp_var'") context("parallel-worker")
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

				** Save treatment effect results to temp file
				preserve
				clear
				qui set obs 1
				gen table_id = `table_id'
				gen sample_data = "otherout"
				gen sample = "`samp_var'"
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
				save "`results_path'sdid/temp_otherout_results/results_`table_id'_`out'_`c'.dta", replace
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
					label var id "Year"
					sort id

					twoway	(rcap res3 res4 id, lc(gs10) fc(gs11%50))	///
							(scatter res1 id, mc(black)),				///
						legend(off) ytitle("`label'")					///
						yline(0, lc("`col_zero'") lp(-))							///
						xline(2020.5, lc(black) lp(solid))

					if `exl' == 1 local evpath "`results_path'sdid/otherout/fig_otherout_`out'_`c'_`samp_var'_excl2020_eventstudy.jpg"
					else local evpath "`results_path'sdid/otherout/fig_otherout_`out'_`c'_`samp_var'_eventstudy.jpg"

					graph export "`evpath'", as(jpg) name("Graph") quality(100) replace
					restore
				}

			} // END COVAR LOOP

		} // END OUTCOME LOOP

		** Table of results (all 4 outcomes, with/without covariates)
		if `exl' == 0 local tabfname "tab_otherout_`samp_var'.tex"
		if `exl' == 1 local tabfname "tab_otherout_`samp_var'_excl2020.tex"

		local _dests `""`results_path'sdid/otherout/`tabfname'""'
		if ${overleaf} == 1 {
			local _dests `"`_dests' "${ol_tab}`tabfname'""'
		}

		** Build dynamic esttab model list
		local est_list ""
		local mtitle_list ""
		local mgroup_labels ""
		local mgroup_pattern ""
		foreach out of local outcomes {
			local est_list "`est_list' sdid_`out'_0 sdid_`out'_1"
			local mtitle_list `"`mtitle_list' "No Cov." "Cov.""'
			local mgroup_labels `"`mgroup_labels' "`lbl_`out''""'
			local mgroup_pattern "`mgroup_pattern' 1 0"
		}

		foreach _outfile of local _dests {
		capture noisily {
			esttab `est_list'	///
				using "`_outfile'",								///
			starlevel("*" 0.10 "**" 0.05 "***" 0.01)		///
			b(%-9.3f) se(%-9.3f) replace 					///
			mgroups(`mgroup_labels',						///
				pattern(`mgroup_pattern'))					///
			mtitle(`mtitle_list')							///
			stats(count mean, 								///
				fmt(%9.0fc %9.3fc) 							///
				labels("Number of Counties" "Pre-treatment mean"))
		}
		} // end foreach _outfile (tab_otherout)

		dis "Completed table `table_id': `samp_var' / excl=`exl'"

	end

	** Define wrapper program for parallel execution
	capture program drop parallel_otherout_wrapper
	program define parallel_otherout_wrapper
		** Increase matsize for sdid_event memory requirements
		set matsize 5000
		** Store all table_ids upfront (run_otherout_table will overwrite the dataset)
		local n_obs = _N
		forvalues i = 1/`n_obs' {
			local tid_`i' = table_id[`i']
		}

		** Now loop through and process each table
		forvalues i = 1/`n_obs' {
			dis "Worker processing otherout table `tid_`i'' (`i' of `n_obs' in this chunk)"
			run_otherout_table, table_id(`tid_`i'') data_path("${data}") results_path("${results}") reps(100)
		}
	end

} // END PARALLEL SETUP


********************************************************************************
** MAIN ESTIMATION
********************************************************************************

if ${use_parallel} == 1 {

	********************************************************************************
	** PARALLEL ESTIMATION
	********************************************************************************

	** Create temp directory for results (clean stale files unless resuming)
	if ${resume} == 0 {
		capture shell rmdir "${results}sdid/temp_otherout_results" /s /q
	}
	capture mkdir "${results}sdid/temp_otherout_results"

	** Load table grid
	use "${data}working/otherout_table_grid.dta", clear
	local n_tables = _N
	dis "Running `n_tables' otherout table units in parallel (each with 8 SDID specs)..."

	** Cost-balanced worker assignment via snake ordering
	** Tables vary in cost (n^2 in counties). Snake ordering balances load.
	preserve

	** Step 1: Count distinct counties per sample
	use "${data}working/otherout_sdid_data.dta", clear

	foreach samp in "sample_all" "sample_urban95" "sample_urban75_covid" "sample_demog" "sample_stringency" {
		qui count if `samp' == 1 & year == 2021
		local nc_`samp' = r(N)
	}

	** Build cost lookup table from stored counts
	clear
	qui set obs 5
	gen samp_var = ""
	gen n_counties = .
	gen cost = .

	local row = 0
	foreach samp in "sample_all" "sample_urban95" "sample_urban75_covid" "sample_demog" "sample_stringency" {
		local row = `row' + 1
		qui replace samp_var = "`samp'" in `row'
		local nc = `nc_`samp''
		qui replace n_counties = `nc' in `row'
		qui replace cost = `nc' * `nc' in `row'
	}

	tempfile cost_lookup
	save `cost_lookup'

	** Step 2: Merge cost weights into the table grid
	use "${data}working/otherout_table_grid.dta", clear
	merge m:1 samp_var using `cost_lookup', keep(master match) nogen

	** Step 3: Snake ordering (LPT heuristic) for balanced worker assignment
	** Sort tables by cost descending so heaviest tables are assigned first
	gsort -cost table_id

	** Snake pattern: 1,2,...,k, k,k-1,...,1, 1,2,...,k, ...
	** This ensures each worker gets roughly equal total cost
	local k = ${n_clusters}
	gen worker = .
	local direction = 1  // 1 = forward, -1 = backward
	local w = 1          // current worker

	forvalues i = 1/`=_N' {
		qui replace worker = `w' in `i'

		** Advance worker in current direction
		if `direction' == 1 {
			if `w' < `k' {
				local w = `w' + 1
			}
			else {
				** Hit top: reverse direction (stay on same worker for next)
				local direction = -1
			}
		}
		else {
			if `w' > 1 {
				local w = `w' - 1
			}
			else {
				** Hit bottom: reverse direction (stay on same worker for next)
				local direction = 1
			}
		}
	}

	** Step 4: Sort by worker so parallel's contiguous row split aligns
	sort worker table_id

	** Diagnostic: show per-worker cost balance
	dis _n "=== Cost-Balanced Worker Assignment (Otherout) ==="
	dis "Tables: `=_N'  Workers: `k'"
	tempvar worker_cost worker_count
	bysort worker: egen `worker_cost' = total(cost)
	bysort worker: egen `worker_count' = count(table_id)

	** Display per-worker summary
	forvalues w = 1/`k' {
		qui summ `worker_cost' if worker == `w'
		if r(N) > 0 {
			local wc = r(mean)
			qui summ `worker_count' if worker == `w'
			local wn = r(mean)
			dis "  Worker `w': `wn' tables, cost = " %12.0fc `wc'
		}
	}

	qui summ `worker_cost'
	local max_cost = r(max)
	local min_cost = r(min)
	local imbalance = (`max_cost' - `min_cost') / `max_cost' * 100
	dis "  Imbalance: " %4.1f `imbalance' "% (max-min)/max"
	dis "==========================================" _n

	** Save table IDs in worker-sorted order for parallel processing
	sort worker table_id
	keep table_id
	save "${data}working/otherout_table_ids.dta", replace

	restore

	** Load table IDs and run in parallel
	use "${data}working/otherout_table_ids.dta", clear

	** Run parallel estimation
	dis "Starting parallel otherout SDID estimation at $S_TIME..."
	timer clear 2
	timer on 2

	parallel, prog(parallel_otherout_wrapper run_otherout_table sdid_log_failure): parallel_otherout_wrapper

	timer off 2
	timer list 2
	dis "Parallel otherout estimation completed at $S_TIME"

	** Combine all treatment effect results
	dis "Combining otherout results from parallel workers..."
	clear
	local files : dir "${results}sdid/temp_otherout_results" files "results_*.dta"
	local first = 1

	foreach f of local files {
		if `first' == 1 {
			use "${results}sdid/temp_otherout_results/`f'", clear
			local first = 0
		}
		else {
			append using "${results}sdid/temp_otherout_results/`f'"
		}
	}

	** Save combined results
	drop table_id
	order sample_data sample outcome controls exclusion	///
		tau se pval ci_lower ci_upper n_counties pre_mean significant
	compress
	save "${results}sdid/otherout/otherout_sdid_results.dta", replace

	** Clean up temp directory
	shell rmdir "${results}sdid/temp_otherout_results" /s /q

	** Clean up temporary files
	capture erase "${data}working/otherout_sdid_data.dta"
	capture erase "${data}working/otherout_table_grid.dta"
	capture erase "${data}working/otherout_table_ids.dta"

	dis "Parallel otherout results combined and saved."

}
else {

	********************************************************************************
	** SEQUENTIAL ESTIMATION
	********************************************************************************

	** Open postfile for results accumulation (O(1) per spec)
	local pf_path "${results}sdid/otherout/otherout_sdid_results.dta"
	if ${resume} == 1 {
		local pf_path "${results}sdid/otherout/otherout_sdid_results_new.dta"
	}
	capture postclose pf_results
	tempname pf_results
	postfile `pf_results' str40(sample_data sample) str60(outcome) ///
		controls exclusion tau se pval ci_lower ci_upper            ///
		n_counties pre_mean significant                             ///
		using "`pf_path'", replace

	** ─── CHECKPOINT: load completed specs for resume mode ───
	local n_done = 0
	if ${resume} == 1 {
		capture confirm file "${results}sdid/otherout/otherout_sdid_results.dta"
		if _rc == 0 {
			preserve
			use "${results}sdid/otherout/otherout_sdid_results.dta", clear
			qui count
			local n_done = r(N)
			if `n_done' > 0 {
				** Drop any stale _done_set from a prior aborted run before
				** (re-)creating so results don't silently inherit old state.
				capture mata: mata drop _done_set
				gen _done_key = sample_data + "|" + sample + "|" + outcome ///
					+ "|" + string(controls, "%1.0f") + "|" + string(exclusion, "%1.0f")
				mata: _done_set = asarray_create()
				mata: for (_i=1; _i<=st_nobs(); _i++) asarray(_done_set, st_sdata(_i, "_done_key"), 1)
				dis "RESUME MODE: `n_done' specs already completed. Skipping those."
			}
			restore
		}
	}

	** Covariates (property tax always available in this sample)
	local covariates "population per_capita_income"

	** Loop over samples
	foreach samp of varlist sample_all sample_urban95 sample_urban75_covid sample_demog sample_stringency {

		** Loop over exclusion of 2020
		forvalues exl = 1(-1)0 {

			** Sample, exclusion, and covariates are all handled inside the
			** engine (fit_spec_sdid -> load_spec_panel), so no in-memory `sample`
			** var or eststo scaffolding is needed here.
			foreach out of local outcomes {

				** Loop over inclusion of covariates
				forvalues c = 0/1 {

					** Figure base path for the engine's point-estimate SDID graph
					if `exl' == 0 local path "${results}sdid/otherout/fig_otherout_`out'_`c'_`samp'_"
					if `exl' == 1 local path "${results}sdid/otherout/fig_otherout_`out'_`c'_`samp'_excl2020_"

					** ─── Skip if already completed (resume mode) ───
					if `n_done' > 0 {
						local _this_key "otherout|`samp'|`out'|`c'|`exl'"
						mata: st_local("_skip", strofreal(asarray_contains(_done_set, st_local("_this_key"))))
						if `_skip' == 1 {
							dis "RESUME: Skipping `out' c=`c' exl=`exl' samp=`samp'"
							continue
						}
					}

					** Estimate one SDID spec via the shared engine (point estimate
					** + event study). Behavior-preserving: identical
					** `sdid ... vce(placebo)` to the former inline call, so
					** tau/se/pre_mean/n_counties match by construction.
					capture noisily fit_spec_sdid, sampledata("otherout") ///
						sample(`samp') outcome(`out') controls(`c') exclusion(`exl') ///
						eventstudy(1) reps(`reps') ///
						datafile("${data}working/otherout_sdid_data.dta") ///
						graphbase("`path'")

					if _rc != 0 {
						local _failed_rc = _rc
						dis "fit_spec_sdid failed for `out' c=`c' exl=`exl' samp=`samp'. Skipping."
						sdid_log_failure, rc(`_failed_rc') script("02_otherout_sdid") ///
							tableid("otherout") outcome("`out'") c(`c') exl(`exl') ///
							samp("`samp'") context("main-serial")
						continue
					}

					** Headline results -> postfile
					local tmp_tau       = r(tau)
					local tmp_se        = r(se)
					local tmp_premean   = r(pre_mean)
					local tmp_ncounties = r(n_counties)
					local tmp_event_ok  = r(event_ok)
					tempname _evres
					if `tmp_event_ok' == 1 matrix `_evres' = r(event_res)

					local tmp_pval  = 2 * (1 - normal(abs(`tmp_tau'/`tmp_se')))
					local tmp_ci_lo = `tmp_tau' - 1.96 * `tmp_se'
					local tmp_ci_hi = `tmp_tau' + 1.96 * `tmp_se'
					local tmp_sig   = abs(`tmp_tau'/`tmp_se') > 1.96
					post `pf_results' ("otherout") ("`samp'") ("`out'") ///
						(`c') (`exl') (`tmp_tau') (`tmp_se') (`tmp_pval') ///
						(`tmp_ci_lo') (`tmp_ci_hi') (`tmp_ncounties')     ///
						(`tmp_premean') (`tmp_sig')

					** Event-study figure from r(event_res): cols are
					** year, tau, ci_lo, ci_hi. The engine already applies the
					** exclusion-year shift, so the matrix years plot as-is.
					if `tmp_event_ok' == 1 {
						preserve
						clear
						svmat `_evres', names(_ev)
						rename _ev1 id
						rename _ev2 _evtau
						rename _ev3 _evlo
						rename _ev4 _evhi
						label var id "Year"
						sort id

						twoway	(rcap _evlo _evhi id, lc(gs10) fc(gs11%50))	///
								(scatter _evtau id, mc(black)),				///
							legend(off) ytitle("`lbl_`out''")				///
							yline(0, lc("`col_zero'") lp(-))				///
							xline(2020.5, lc(black) lp(solid))

						if `exl' == 1 local evpath "${results}sdid/otherout/fig_otherout_`out'_`c'_`samp'_excl2020_eventstudy.jpg"
						else local evpath "${results}sdid/otherout/fig_otherout_`out'_`c'_`samp'_eventstudy.jpg"

						graph export "`evpath'", as(jpg) name("Graph") quality(100) replace
						restore
					}

				} // END COVAR LOOP

			} // END OUTCOME LOOP

			** Result tables for this sample are built after postclose from
			** otherout_sdid_results.dta (see the "Result tables" pass below).
			** The former esttab-on-live-estimates block was removed: fit_spec_sdid
			** leaves no stored estimate to esttab, and building tables from the
			** results file matches the main-SDID / 02_tables_figures.do pattern.

		} // END EXCLUSION LOOP

	} // END SAMPLE LOOP

	** Close postfile and finalize results
	postclose `pf_results'

	** Resume mode: merge new results into existing file
	if ${resume} == 1 {
		use "${results}sdid/otherout/otherout_sdid_results.dta", clear
		append using "${results}sdid/otherout/otherout_sdid_results_new.dta"
		save "${results}sdid/otherout/otherout_sdid_results.dta", replace
		capture erase "${results}sdid/otherout/otherout_sdid_results_new.dta"
	}

	** ---- Result tables: built from otherout_sdid_results.dta (replaces the
	**      former per-(samp,exl) esttab tables). 8 columns = 4 outcomes x
	**      {No Cov., Cov.}; rows = SDID treatment effect (tau, se, stars),
	**      county count, and pre-treatment mean. ----
	preserve
	use "${results}sdid/otherout/otherout_sdid_results.dta", clear
	foreach samp in sample_all sample_urban95 sample_urban75_covid sample_demog sample_stringency {
		forvalues exl = 0/1 {
			if `exl' == 0 local tabfname "tab_otherout_`samp'.tex"
			if `exl' == 1 local tabfname "tab_otherout_`samp'_excl2020.tex"
			local _dests `""${results}sdid/otherout/`tabfname'""'
			if ${overleaf} == 1 local _dests `"`_dests' "${ol_tab}`tabfname'""'

			local taurow  "PFA effect"
			local serow   ""
			local cntrow  "Number of Counties"
			local meanrow "Pre-treatment mean"
			foreach out in ln_n1 ln_agi ln_total_inc ln_wage {
				forvalues c = 0/1 {
					qui count if outcome=="`out'" & sample=="`samp'" & exclusion==`exl' & controls==`c'
					if r(N) >= 1 {
						qui summ tau        if outcome=="`out'" & sample=="`samp'" & exclusion==`exl' & controls==`c', meanonly
						local b = r(mean)
						qui summ se         if outcome=="`out'" & sample=="`samp'" & exclusion==`exl' & controls==`c', meanonly
						local s = r(mean)
						qui summ pval       if outcome=="`out'" & sample=="`samp'" & exclusion==`exl' & controls==`c', meanonly
						local p = r(mean)
						qui summ n_counties if outcome=="`out'" & sample=="`samp'" & exclusion==`exl' & controls==`c', meanonly
						local nc = r(mean)
						qui summ pre_mean   if outcome=="`out'" & sample=="`samp'" & exclusion==`exl' & controls==`c', meanonly
						local pm = r(mean)
						local star ""
						if `p' < 0.10 local star "*"
						if `p' < 0.05 local star "**"
						if `p' < 0.01 local star "***"
						local bf  : di %9.3f `b'
						local bf  = strtrim("`bf'")
						local sf  : di %9.3f `s'
						local sf  = strtrim("`sf'")
						local ncf : di %9.0fc `nc'
						local ncf = strtrim("`ncf'")
						local pmf : di %9.3f `pm'
						local pmf = strtrim("`pmf'")
						local taurow  "`taurow' & `bf'`star'"
						local serow   "`serow' & (`sf')"
						local cntrow  "`cntrow' & `ncf'"
						local meanrow "`meanrow' & `pmf'"
					}
					else {
						local taurow  "`taurow' & "
						local serow   "`serow' & "
						local cntrow  "`cntrow' & "
						local meanrow "`meanrow' & "
					}
				}
			}

			foreach _outfile of local _dests {
				tempname th
				file open `th' using "`_outfile'", write replace
				file write `th' "\begin{tabular}{l*{8}{c}}" _n
				file write `th' "\toprule" _n
				file write `th' `" & \multicolumn{2}{c}{`lbl_ln_n1'} & \multicolumn{2}{c}{`lbl_ln_agi'} & \multicolumn{2}{c}{`lbl_ln_total_inc'} & \multicolumn{2}{c}{`lbl_ln_wage'} \\"' _n
				file write `th' "\cmidrule(lr){2-3}\cmidrule(lr){4-5}\cmidrule(lr){6-7}\cmidrule(lr){8-9}" _n
				file write `th' " & No Cov. & Cov. & No Cov. & Cov. & No Cov. & Cov. & No Cov. & Cov. \\" _n
				file write `th' "\midrule" _n
				file write `th' `"`taurow' \\"' _n
				file write `th' `"`serow' \\"' _n
				file write `th' "\midrule" _n
				file write `th' `"`cntrow' \\"' _n
				file write `th' `"`meanrow' \\"' _n
				file write `th' "\bottomrule" _n
				file write `th' "\end{tabular}" _n
				file close `th'
			}
		}
	}
	restore

	** Clean up mata checkpoint lookup. `capture` prevents an error if an
	** earlier failure in the loop dropped _done_set prematurely.
	capture mata: mata drop _done_set

	** Clean up temporary data file
	capture erase "${data}working/otherout_sdid_data.dta"

} // END SEQUENTIAL/PARALLEL BRANCH


** Export results
use "${results}sdid/otherout/otherout_sdid_results.dta", clear
export excel using "${results}sdid/otherout/otherout_sdid_results.xlsx", firstrow(variables) replace


********************************************************************************
** SPECIFICATION CURVE ANALYSIS
********************************************************************************

/*******************************************************************************
Creates specification curve plots showing treatment effects across all
specifications for each outcome variable.

Coefficient colors:
- Sea (p7): Statistically significant (p<0.05), not preferred
- Sky (p3): Statistically insignificant, not preferred
- Vermillion (p6): Statistically significant (p<0.05), preferred specification
- Orangebrown (p8): Statistically insignificant, preferred specification

Preferred specifications: Stringency match + covariates + excl 2020
*******************************************************************************/

** Load treatment effects
use "${results}sdid/otherout/otherout_sdid_results.dta", clear

** Create specification indicators for bottom panel
gen spec_all = sample == "sample_all"
gen spec_urban95 = sample == "sample_urban95"
gen spec_covid = sample == "sample_urban75_covid"
gen spec_demog = sample == "sample_demog"
gen spec_stringency = sample == "sample_stringency"
gen spec_covars = controls == 1
gen spec_excl2020 = exclusion == 1

** Calculate statistical significance (p < 0.05)
replace significant = pval < 0.05 if missing(significant)

********************************************************************************
** DEFINE PREFERRED SPECIFICATIONS
** Modify these conditions to change which specifications are highlighted
** as "preferred" in the specification curve plots.
********************************************************************************

gen preferred = 0

** Preferred: stringency-matched + covariates + excl 2020
replace preferred = 1 if 									///
	sample == "sample_stringency" &							///
	controls == 1 &											///
	exclusion == 1

** Display count of preferred specifications
dis "Number of preferred specifications: "
count if preferred == 1

********************************************************************************
** CREATE SPECIFICATION CURVE PLOTS
********************************************************************************

foreach out of local outcomes {

	** Preserve full data
	preserve

	** Keep only this outcome
	keep if outcome == "`out'"

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
	** Four categories:
	**   1. Significant + Not Preferred (sea)
	**   2. Insignificant + Not Preferred (sky)
	**   3. Significant + Preferred (vermillion)
	**   4. Insignificant + Preferred (orangebrown)
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
	** Upper panel: Coefficient plot with CIs colored by significance
	** and preferred status
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

	gen y_all        = `yp1' if spec_all == 1
	gen y_urban      = `yp2' if spec_urban95 == 1
	gen y_covid      = `yp3' if spec_covid == 1
	gen y_demog      = `yp4' if spec_demog == 1
	gen y_stringency = `yp5' if spec_stringency == 1
	gen y_covars     = `yp6' if spec_covars == 1
	gen y_excl       = `yp7' if spec_excl2020 == 1

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
			(scatter y_all spec_rank, 									///
				mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
			(scatter y_urban spec_rank, 								///
				mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
			(scatter y_covid spec_rank, 								///
				mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
			(scatter y_demog spec_rank, 								///
				mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
			(scatter y_stringency spec_rank, 							///
				mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
			(scatter y_covars spec_rank, 								///
				mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
			(scatter y_excl spec_rank, 									///
				mc("`col_sig_notpref'") ms(O) msize(vsmall)), 			///
		yline(`sep_y', lc(gs12) lp(solid) lw(vthin)) 					///
		yline(0, lc("`col_zero'") lp(dash)) 							///
		ylabel(`tick_lo'(1)`tick_hi', labsize(vsmall) nogrid) 			///
		ylabel(`yp1' "All Counties" 									///
			   `yp2' "Urban (Top 5%)" 									///
			   `yp3' "COVID Match" 										///
			   `yp4' "Demographic Match" 								///
			   `yp5' "Stringency Match" 								///
			   `yp6' "Covariates" 										///
			   `yp7' "Excl. 2020", 										///
			labsize(vsmall) angle(0) notick nogrid add) 				///
		legend(order(5 "Sig. (p<0.05)" 6 "Insig." 						///
					 7 "Sig., Preferred" 8 "Insig., Preferred") 		///
			   rows(1) pos(6) size(vsmall)) 							///
		ytitle("Treatment Effect", size(vsmall)) 						///
		xtitle("Specification (ranked by effect size)", size(vsmall)) 	///
		xlabel(none) 													///
		xscale(range(0.5 `=`n_specs'+0.5')) 							///
		graphregion(color(white)) 										///
		name(speccurve_`out', replace)

	** Export figure
	graph export "${results}sdid/otherout/fig_speccurve_otherout_`out'.pdf", replace
	graph export "${results}sdid/otherout/fig_speccurve_otherout_`out'.jpg", as(jpg) quality(100) replace
	if ${overleaf} == 1 {
		graph export "${ol_fig}fig_speccurve_otherout_`out'.pdf", replace
	}

	** Clean up
	graph drop speccurve_`out'

	restore

} // END OUTCOME LOOP


********************************************************************************
** FINISH
********************************************************************************

dis ""
dis "=============================================="
dis "OTHER OUTCOMES SDID ANALYSIS COMPLETE"
dis "=============================================="
dis "Results saved to:"
dis "  - ${results}sdid/otherout/otherout_sdid_results.dta"
dis "  - ${results}sdid/otherout/tab_otherout_*.tex"
dis "  - ${results}sdid/otherout/fig_otherout_*_eventstudy.jpg"
dis "  - ${results}sdid/otherout/fig_speccurve_otherout_*.pdf"
dis "=============================================="

** Close log
clear
log close log_02_otherout
