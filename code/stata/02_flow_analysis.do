/*******************************************************************************
File Name: 		02_flow_analysis.do
Creator: 		John Iselin
Date Update:	January 31, 2026

Called by: 00_multnomah.do

Purpose: Perform flow-based analysis on IRS data.

Outputs:
All outputs are produced twice: once for all counties and once for ACS counties
only (with _acs suffix). ACS sample includes property tax rate controls.

- county_flow_coefficients[_acs].dta/csv: flow coefficients for all counties
  (b_out, b_in, se_out, se_in, t-stats, p-values, percentile ranks)
- fig_multnomah_post_*[_acs].png: flow coefficient plots for Multnomah
- fig_hist_out_*[_acs].png: Histogram of out-migration coefficients (with percentile)
- fig_hist_in_*[_acs].png: Histogram of in-migration coefficients (with percentile)
- fig_scatter_out_in_*[_acs].png: Scatter plot of out vs in coefficients
- fig_multnomah_out_*[_acs].png: Event study plots for out-migration
- fig_multnomah_in_*[_acs].png: Event study plots for in-migration
- fig_multnomah_both_*[_acs].png: Combined event study plots

Excel output (flow_analysis_stats.xlsx):
- all/acs: Summary statistics for Multnomah and distribution
- larger_out_all/acs: Counties with larger out-migration coefficients
- smaller_in_all/acs: Counties with smaller in-migration coefficients
- worse_both_all/acs: Counties worse on both dimensions
- larger_t_out_all/acs: Counties with larger out-migration t-statistics
- smaller_t_in_all/acs: Counties with smaller in-migration t-statistics

Debug mode:
- Set local debug = 1 to run on a random subset of counties for faster testing
- Set local debug_n to control the number of random counties (default: 50)
- Multnomah County is always included in the debug sample
- Set debug = 0 for full production runs

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
capture log close log_02
log using "${logs}02_log_flow_${date}", replace text name(log_02)
project_set_seed, context("02_flow_analysis.do") offset(30)

** plotplainblind palette (RGB) — consistent across all figures
local col_out  "0 114 178"    // sea (p7) — out-migration
local col_in   "213 94 0"     // vermillion (p6) — in-migration
local col_mult "230 159 0"    // orangebrown (p8) — Multnomah highlight
local col_ref  "153 153 153"  // gs10 (p2) — reference lines

** Parameters
local reps = 100

** Debug mode: set to 1 to run on a random subset of counties for faster testing
** Set to 0 for full production run
local debug = 0
local debug_n = 20  // Number of random counties to sample in debug mode (plus Multnomah)

** Load ACS data to get the set of counties
use "${data}working/acs_county_gross_25plus", clear

** Keep required variables
keep year fips

** Keep only always-observed fips (13 ACS years: 2012-2024)
bysort fips: gen ct = _N
tab ct
qui summ ct
keep if ct == `r(max)'

** Keep fips
keep fips

** Drop duplicates
duplicates drop

** Save to tempfile
tempfile acs_fips
save `acs_fips'
clear

** Load IRS Data
use "${data}working/irs_county_flow.dta", clear

** Keep analysis window (consistent with other SDID/DiD files)
keep if inrange(year, 2016, 2022)

** Sample restrictions
drop if inlist(state_fips_o, 2, 15)   	// Alaska and Hawaii
drop if inlist(state_fips_d, 2, 15)   	// Alaska and Hawaii

** Clean up variables
drop fips

** Mover indicators
gen mover = fips_o != fips_d

** Generate flow id
egen flow_id = group(fips_d fips_o)

** Declare panel and fill in missing values
xtset flow_id year
tsfill, full

** Keep required variables
keep fips_d fips_o flow_id year mover n1 n2 agi

** Replace with 0s
replace n1 = 0 if missing(n1)
replace n2 = 0 if missing(n2)
replace agi = 0 if missing(agi)
replace agi = 0 if agi < 0

** Merge with time-varying controls

** Keep if in donor pool in both origin and destination
foreach x in "o" "d" {

	** Fill in missing fips
	bysort flow_id: egen tmp = mean(fips_`x')
	replace fips_`x' = tmp if missing(fips_`x')
	drop tmp

	** Merge with relevent variables
	gen fips = fips_`x'
	merge m:1 fips using "${data}working/ids", 						///
		keep(match) gen(merge_ids_`x')
	merge m:1 year fips using "${data}working/bls_unemployment", 	///
		keep(match) gen(merge_bls_`x')
	merge m:1 year fips using "${data}working/bea_economics",		///
		keep(match) gen(merge_bea_`x')
	merge m:1 fips using `acs_fips', keep(master match) gen(merge_acs_`x')
	merge m:1 year fips using "${data}working/property_tax_rates_overall", ///
		gen(proptx_merge_`x') keep(master match) keepusing(prop_rate_mean )

	** Rename
	rename state_name state_name_`x'
	rename state_fips state_fips_`x'
	rename county_name county_name_`x'
	rename county_fips county_fips_`x'
	rename unemp unemp_`x'
	rename population pop_`x'
	rename per_capita_income per_capita_income_`x'
	rename prop_rate_mean prop_rate_mean_`x'
	drop fips

} // END ORIGIN-DESTINATION LOOP

** Tag flows to-and-from acs counties
gen acs_flow = merge_acs_d == 3 & merge_acs_o == 3
label var acs_flow "Flows to and from ACS counties"

** Multnomah indicators
gen out_multnomah = (state_fips_o == 41 & county_fips_o == 51)
gen in_multnomah = (state_fips_d == 41 & county_fips_d == 51)

** Post-indicators
gen post = year > 2020

** Interactions
gen in_multnomah_post = in_multnomah * post
gen out_multnomah_post = out_multnomah * post

** Create interactions manually (base year = 2020)
forvalues y = 2016/2022 {
	gen x_out_`y' = out_multnomah * (year == `y')
	gen x_in_`y' = in_multnomah * (year == `y')
}

** Drop base year (2020)
drop x_out_2020 x_in_2020

** Save main analysis dataset to tempfile
tempfile main_data
save `main_data'

** Save to real file for parallel workers (parallel can't access tempfiles)
compress
save "${data}working/flow_analysis_data.dta", replace

********************************************************************************
** PARALLEL MODE: DEFINE PROGRAMS (must be outside foreach loop)
********************************************************************************

if ${use_parallel} == 1 {

	** Define worker program: process a single county
	capture program drop run_flow_county
	program define run_flow_county
		syntax, fips(integer)

		** Load flow analysis data
		use "${data}working/flow_analysis_data.dta", clear

		** Generate treatment vars
		gen out_tmp = fips_o == `fips'
		gen in_tmp = fips_d == `fips'
		gen out_post_tmp = out_tmp * post
		gen in_post_tmp = in_tmp * post

		** Run regression with capture
		local tmp_b_out = .
		local tmp_se_out = .
		local tmp_b_in = .
		local tmp_se_in = .

		** `noisily` surfaces ppmlhdfe error messages (convergence, collinearity,
		** etc.) in the log while still letting the loop continue on failure.
		capture noisily {
			ppmlhdfe agi i.out_post_tmp i.in_post_tmp 		///
				${flow_covars} 								///
				if mover == 1 ${flow_sample_cond}, 			///
				absorb(year flow_id) vce(cluster flow_id)

			local tmp_b_out = _b[1.out_post_tmp]
			local tmp_se_out = _se[1.out_post_tmp]
			local tmp_b_in = _b[1.in_post_tmp]
			local tmp_se_in = _se[1.in_post_tmp]
		}

		if _rc != 0 {
			dis "Warning: Regression failed for fips `fips'"
		}

		** Save 1-row result
		clear
		set obs 1
		gen fips = `fips'
		gen b_out = `tmp_b_out'
		gen se_out = `tmp_se_out'
		gen b_in = `tmp_b_in'
		gen se_in = `tmp_se_in'
		save "${results}flows/temp_results/result_`fips'.dta", replace

	end

	** Define wrapper program for parallel execution
	capture program drop parallel_flow_wrapper
	program define parallel_flow_wrapper
		** Store all fips values upfront (run_flow_county overwrites dataset)
		local n_obs = _N
		forvalues i = 1/`n_obs' {
			local fid_`i' = fips_target[`i']
		}

		** Loop through and process each county
		forvalues i = 1/`n_obs' {
			if mod(`i', 50) == 0 {
				dis "Worker processing county `i' of `n_obs' (fips = `fid_`i'')"
			}
			run_flow_county, fips(`fid_`i'')
		}
	end

} // END PARALLEL PROGRAM DEFINITIONS

********************************************************************************
** MAIN ANALYSIS LOOP: Run regressions for (1) all counties, (2) ACS counties
** The second iteration restricts to flows where acs_flow == 1 and adds
** property tax rate controls (prop_rate_mean_o and prop_rate_mean_d)
********************************************************************************

foreach sample in "acs" "all" {

	** Set sample-specific parameters
	if "`sample'" == "all" {
		local sample_cond ""
		local covars "unemp_* pop_* per_capita_income_*"
		local file_suffix ""
	}
	else if "`sample'" == "acs" {
		local sample_cond "& acs_flow == 1"
		local covars "unemp_* pop_* per_capita_income_* prop_rate_mean_o prop_rate_mean_d"
		local file_suffix "_acs"
	}
	
	if `debug' == 1 local debug_txt "_debug"
	else local debug_txt ""
	
	dis ""
	dis "========================================================================"
	dis "RUNNING ANALYSIS FOR SAMPLE: `sample'"
	dis "Sample condition: `sample_cond'"
	dis "Covariates: `covars'"
	dis "========================================================================"
	dis ""

	********************************************************************************
	** REGRESSION 1: Multnomah X Post (flow DID)
	********************************************************************************

	** Load main data
	use `main_data', clear

	** Loop over outcome variables
	foreach outcome in n1 n2 agi {

		** Regression 1a: With covariates
		ppmlhdfe `outcome' i.out_multnomah_post i.in_multnomah_post 	///
			`covars' 													///
			if mover == 1 `sample_cond', 								///
			absorb(year flow_id) vce(cluster flow_id)
		estimates store `outcome'_with_covars
		** Sample-tagged copy for the cross-sample appendix table
		** (item 21c of the May 2026 revision TODO).
		estimates store `outcome'_`sample'_with

		** Regression 1b: Without covariates
		ppmlhdfe `outcome' i.out_multnomah_post i.in_multnomah_post 	///
			if mover == 1 `sample_cond', 								///
			absorb(year flow_id) vce(cluster flow_id)
		estimates store `outcome'_no_covars
		estimates store `outcome'_`sample'_no

		** Plot both regressions
		coefplot 	(`outcome'_with_covars, label("With Covariates") 	///
						mc("`col_out'") ciopts(lc("`col_out'"))) 		///
					(`outcome'_no_covars, label("Without Covariates") 	///
						mc("`col_in'") ciopts(lc("`col_in'"))), 		///
			keep(1.out_multnomah_post 1.in_multnomah_post) 				///
			ciopts(recast(rcap)) 										///
			yline(0, lc(gs10) lp(dash)) 								///
			xline(0, lc(gs10) lp(dash)) 								///
			coeflabels(	1.out_multnomah_post = "Out-migration" 			///
						1.in_multnomah_post = "In-migration") 			///
			ytitle("Coefficient") 										///
			xtitle("")													///
			legend(pos(6) rows(1)) 										///
			graphregion(color(white))

		graph export "${results}flows/fig_multnomah_post_`outcome'`file_suffix'.png", replace


	} // END OUTCOME LOOP

	********************************************************************************
	** REGRESSION 1b: Loop over all counties (Permutation-style analysis)
	** Purpose: Run same flow regression for each county to create distribution
	**          of treatment effects for comparison with Multnomah
	********************************************************************************

	** Get list of unique origin fips codes (respecting sample condition)
	if "`sample'" == "all" {
		levelsof fips_o, local(fips_list)
	}
	else {
		levelsof fips_o if acs_flow == 1, local(fips_list)
	}
	local n_fips_full : word count `fips_list'

	** Debug mode: randomly sample a subset of counties (always include Multnomah)
	if `debug' == 1 {
		dis ""
		dis "*** DEBUG MODE: Sampling `debug_n' random counties (plus Multnomah) ***"
		dis ""

		** Create dataset with all fips codes for sampling
		clear
		local n_fips_tmp : word count `fips_list'
		set obs `n_fips_tmp'
		gen fips = .
		local i = 1
		foreach f of local fips_list {
			replace fips = `f' in `i'
			local i = `i' + 1
		}

		** Tag Multnomah (always include)
		gen multnomah = (fips == 41051)

		** Generate random number for sampling
		project_set_seed, context("02_flow_analysis.do debug sample") offset(31)
		gen random = runiform()

		** Sort: Multnomah first, then random order
		gsort -multnomah random

		** Keep Multnomah + debug_n random counties
		keep if _n <= `debug_n' + 1 | multnomah == 1

		** Extract new fips list
		levelsof fips, local(fips_list)

		** Reload main data
		use `main_data', clear
	}

	local n_fips : word count `fips_list'
	if `debug' == 1 {
		dis "DEBUG: Running regressions for `n_fips' counties (sampled from `n_fips_full') (`sample' sample)..."
	}
	else {
		dis "Running regressions for `n_fips' counties (`sample' sample)..."
	}

	if ${use_parallel} == 1 {

		********************************************************************************
		** PARALLEL PATH: Permutation-style county loop
		********************************************************************************

		** Set globals for worker access (locals don't transfer to parallel)
		global flow_sample_cond "`sample_cond'"
		global flow_covars "`covars'"

		** Create FIPS grid dataset
		clear
		local n_fips_tmp : word count `fips_list'
		set obs `n_fips_tmp'
		gen fips_target = .
		local i = 1
		foreach f of local fips_list {
			replace fips_target = `f' in `i'
			local i = `i' + 1
		}

		** Shuffle for load balancing
		gen random = runiform()
		sort random
		drop random

		** Create temp directory (clean first to avoid residual files from failed runs)
		capture mkdir "${results}flows"
		capture shell rmdir "${results}flows/temp_results" /s /q
		capture mkdir "${results}flows/temp_results"

		parallel initialize ${n_clusters}, force

		dis "Starting parallel flow estimation for `n_fips' counties at $S_TIME..."
		timer clear 2
		timer on 2

		parallel, prog(parallel_flow_wrapper run_flow_county): parallel_flow_wrapper

		timer off 2
		timer list 2
		dis "Parallel flow estimation completed at $S_TIME"

		** Combine results from temp files
		clear
		local files : dir "${results}flows/temp_results" files "result_*.dta"
		local first = 1
		foreach f of local files {
			if `first' == 1 {
				use "${results}flows/temp_results/`f'", clear
				local first = 0
			}
			else {
				append using "${results}flows/temp_results/`f'"
			}
		}

		** Verify we got results
		if _N == 0 {
			dis as error "ERROR: No results from parallel flow estimation. Check worker logs."
			error 1
		}

		** Save county flow coefficients
		save "${data}working/county_flow_coefficients`file_suffix'`debug_txt'.dta", replace

		** Clean up temp directory
		shell rmdir "${results}flows/temp_results" /s /q

	}
	else {

		********************************************************************************
		** SEQUENTIAL PATH: Permutation-style county loop
		********************************************************************************

		** Initialize tempfile to store results
		postfile county_coefs fips b_out se_out b_in se_in 		///
			using  "${data}working/county_flow_coefficients`file_suffix'`debug_txt'.dta", replace

		** Counter for display
		local ct = 1

		foreach f of local fips_list {

			** Display status every 100 counties
			if mod(`ct', 100) == 0 {
				dis "Processing county `ct' of `n_fips' (fips = `f')..."
			}

			** Generate treatment vars
			gen out_tmp = fips_o == `f'
			gen in_tmp = fips_d == `f'

			** Interactions
			gen out_post_tmp = out_tmp * post
			gen in_post_tmp = in_tmp * post

			** Run regression; `noisily` keeps error messages visible in the log
			** while still letting the loop continue on failure.
			capture noisily {
				** Regression: With covariates
				 ppmlhdfe agi i.out_post_tmp i.in_post_tmp 	///
					`covars' 										///
					if mover == 1 `sample_cond', 					///
					absorb(year flow_id) vce(cluster flow_id)

				** Store coefficients and SEs
				local tmp_b_out = _b[1.out_post_tmp]
				local tmp_se_out = _se[1.out_post_tmp]
				local tmp_b_in = _b[1.in_post_tmp]
				local tmp_se_in = _se[1.in_post_tmp]

				** Post results
				post county_coefs (`f') (`tmp_b_out') (`tmp_se_out') (`tmp_b_in') (`tmp_se_in')
			}

			** If regression failed, post missing values
			if _rc != 0 {
				post county_coefs (`f') (.) (.) (.) (.)
				dis "Warning: Regression failed for fips `f'"
			}


			** Drop temporary vars
			drop out_tmp in_tmp out_post_tmp in_post_tmp

			** Update count
			local ct = `ct' + 1

		} // END FIPS LOOP

		** Close postfile
		postclose county_coefs

	} // END PARALLEL/SEQUENTIAL BRANCHING

	** Load results
	use "${data}working/county_flow_coefficients`file_suffix'`debug_txt'.dta", clear

	** Add identifiers
	gen multnomah = (fips == 41051)
	label var fips "County FIPS code"
	label var b_out "Out-migration coefficient (County x Post)"
	label var se_out "Out-migration SE"
	label var b_in "In-migration coefficient (County x Post)"
	label var se_in "In-migration SE"
	label var multnomah "Multnomah County indicator"

	** Calculate t-statistics and p-values
	gen t_out = b_out / se_out
	gen t_in = b_in / se_in
	gen p_out = 2 * (1 - normal(abs(t_out)))
	gen p_in = 2 * (1 - normal(abs(t_in)))

	label var t_out "Out-migration t-statistic"
	label var t_in "In-migration t-statistic"
	label var p_out "Out-migration p-value"
	label var p_in "In-migration p-value"

	** Calculate percentile ranks for Multnomah
	foreach x in "b" "t" {
		egen rank_`x'_out = rank(`x'_out)
		egen rank_`x'_in = rank(`x'_in)
		qui count if !missing(`x'_out)
		gen pctile_`x'_out = 100 * rank_`x'_out / r(N)
		qui count if !missing(`x'_in)
		gen pctile_`x'_in = 100 * rank_`x'_in / r(N)

	}

	label var pctile_b_out "Percentile rank of out-migration coef"
	label var pctile_b_in "Percentile rank of in-migration coef"
	label var pctile_t_out "Percentile rank of out-migration t-stat"
	label var pctile_t_in "Percentile rank of in-migration t-stat"

	** Display Multnomah's position
	dis ""
	dis "=========================================="
	dis "Multnomah County Results:"
	dis "=========================================="
	summ b_out t_out b_in t_in pctile_*_out pctile_*_in if multnomah == 1

	** Display distribution summary
	dis ""
	dis "Distribution of coefficients (all counties):"
	summ b_out b_in, detail

	dis ""
	dis "Distribution of t-stats (all counties):"
	summ t_out t_in, detail

	** Save county coefficients dataset
	compress
	save "${data}working/county_flow_coefficients`file_suffix'`debug_txt'.dta", replace
	export delimited using "${data}working/county_flow_coefficients`file_suffix'`debug_txt'.csv", replace

	dis "County coefficients saved to ${data}working/county_flow_coefficients`file_suffix'`debug_txt'.dta"

	********************************************************************************
	** SECTION: Multnomah vs Distribution Plots (Histogram Comparison)
	** Purpose: Plot Multnomah's coefficients against distribution of all counties
	********************************************************************************

	** Load the county coefficients (already in memory, but reload for clarity)
	use "${data}working/county_flow_coefficients`file_suffix'`debug_txt'.dta", clear

	** Loop over beta and t-stats
	foreach x in "b" "t" {

		** Label
		if "`x'" == "b" local txt "Coefficients"
		else if "`x'" == "t" local txt = "T-Statistics"

		** Store Multnomah's coefficients for reference lines
		summ `x'_out if multnomah == 1, meanonly
		local multnomah_`x'_out = r(mean)
		summ `x'_in if multnomah == 1, meanonly
		local multnomah_`x'_in = r(mean)
		summ pctile_`x'_out if multnomah == 1, meanonly
		local multnomah_pctile_out = r(mean)
		summ pctile_`x'_in if multnomah == 1, meanonly
		local multnomah_pctile_in = r(mean)


		** Plot 1: Out-migration coefficient distribution with Multnomah
		histogram `x'_out if !missing(`x'_out), 								///
			bin(50) 														///
			fcolor("`col_out'"%50) lcolor("`col_out'") 						///
			xline(`multnomah_`x'_out', lcolor("`col_mult'") lwidth(thick) lpattern(solid)) ///
			xtitle("Out-migration `txt' (County x Post)") 			///
			ytitle("Frequency") 											///
			graphregion(color(white))

		graph export "${results}flows/fig_hist_out_`x'`file_suffix'`debug_txt'.png", replace

		if ${overleaf} == 1 & "`file_suffix'" == "" & "`debug_txt'" == "" {
			graph export "${ol_fig}fig_hist_out_`x'.png", replace
		}

		** Plot 2: In-migration coefficient distribution with Multnomah
		histogram `x'_in if !missing(`x'_in), 									///
			bin(50) 														///
			fcolor("`col_in'"%50) lcolor("`col_in'") 						///
			xline(`multnomah_`x'_in', lcolor("`col_mult'") lwidth(thick) lpattern(solid)) ///
			xtitle("In-migration `txt' (County x Post)") 				///
			ytitle("Frequency") 											///
			graphregion(color(white))

		graph export "${results}flows/fig_hist_in_`x'`file_suffix'`debug_txt'.png", replace

		if ${overleaf} == 1 & "`file_suffix'" == "" & "`debug_txt'" == "" {
			graph export "${ol_fig}fig_hist_in_`x'.png", replace
		}

		** Plot 3: Combined scatter of out vs in coefficients
		twoway 	(scatter `x'_in `x'_out if multnomah == 0, 						///
					mc("`col_out'"%30) ms(O) msize(small)) 					///
				(scatter `x'_in `x'_out if multnomah == 1, 						///
					mc("`col_mult'") ms(D) msize(large)), 					///
			xline(0, lc(gs10) lp(dash)) 									///
			yline(0, lc(gs10) lp(dash)) 									///
			xtitle("Out-migration `txt'") 							///
			ytitle("In-migration `txt'") 								///
			legend(order(1 "Other Counties" 2 "Multnomah County") pos(6) rows(1)) ///
			graphregion(color(white))

		graph export "${results}flows/fig_scatter_out_in_`x'`file_suffix'`debug_txt'.png", replace


	} // END BETA / T-STAT LOOP

	********************************************************************************
	** SECTION: Export Descriptive Statistics to Excel
	** Purpose: Export Multnomah vs Distribution stats to Excel file
	********************************************************************************

	** Get Multnomah's statistics

	** Loop over migration type
	foreach x in "in" "out" {

		summ se_`x' if multnomah == 1, meanonly
		local m_se_`x' = r(mean)

		summ p_`x' if multnomah == 1, meanonly
		local m_p_`x' = r(mean)

		** Loop over beta / t-stat
		foreach y in "b" "t" {

			summ `y'_`x' if multnomah == 1, meanonly
			local m_`y'_`x' = r(mean)

			summ pctile_`y'_`x' if multnomah == 1, meanonly
			local m_pctile_`y'_`x' = r(mean)

		} // END BETA / T-STAT LOOP


	} // END IN/OUT LOOP

	** Count statistics
	qui count if !missing(b_out)
	local n_counties_out = r(N)
	qui count if !missing(b_in)
	local n_counties_in = r(N)
	qui count if !missing(b_out) & !missing(b_in)
	local n_counties_both = r(N)

	** Count counties with more extreme coefficients than Multnomah
	** For out-migration: larger (more positive) is "worse"
	** For in-migration: smaller (more negative) is "worse"
	qui count if b_out > `m_b_out' & !missing(b_out)
	local n_larger_out = r(N)
	qui count if b_out > `m_b_out' & !missing(b_out) & p_out < 0.05
	local n_larger_out_sig = r(N)

	qui count if b_in < `m_b_in' & !missing(b_in)
	local n_smaller_in = r(N)
	qui count if b_in < `m_b_in' & !missing(b_in) & p_in < 0.05
	local n_smaller_in_sig = r(N)

	** Counties with both more out AND less in (worse on both dimensions)
	qui count if b_out > `m_b_out' & b_in < `m_b_in' & !missing(b_out) & !missing(b_in)
	local n_worse_both = r(N)
	qui count if b_out > `m_b_out' & b_in < `m_b_in' & !missing(b_out) & !missing(b_in) & p_out < 0.05 & p_in < 0.05
	local n_worse_both_sig = r(N)

	** Distribution statistics for out-migration
	summ b_out, detail
	local dist_out_mean = r(mean)
	local dist_out_sd = r(sd)
	local dist_out_min = r(min)
	local dist_out_max = r(max)
	local dist_out_p25 = r(p25)
	local dist_out_p50 = r(p50)
	local dist_out_p75 = r(p75)

	** Distribution statistics for in-migration
	summ b_in, detail
	local dist_in_mean = r(mean)
	local dist_in_sd = r(sd)
	local dist_in_min = r(min)
	local dist_in_max = r(max)
	local dist_in_p25 = r(p25)
	local dist_in_p50 = r(p50)
	local dist_in_p75 = r(p75)

	** Distribution statistics for t-stats
	summ t_out, detail
	local dist_t_out_mean = r(mean)
	local dist_t_out_sd = r(sd)
	local dist_t_out_p50 = r(p50)

	summ t_in, detail
	local dist_t_in_mean = r(mean)
	local dist_t_in_sd = r(sd)
	local dist_t_in_p50 = r(p50)

	** Save county coefficients to tempfile for later use in export sheets
	tempfile county_coefs_for_export
	save `county_coefs_for_export'

	** Create a new dataset with the statistics
	clear
	set obs 40

	** Create variables
	gen stat_name = ""
	gen stat_value = .
	gen stat_description = ""

	** Row counter
	local row = 1

	********************************************************************************
	** MULTNOMAH STATISTICS
	********************************************************************************

	replace stat_name = "m_b_out" in `row'
	replace stat_value = `m_b_out' in `row'
	replace stat_description = "Multnomah out-migration coefficient" in `row'
	local row = `row' + 1

	replace stat_name = "m_se_out" in `row'
	replace stat_value = `m_se_out' in `row'
	replace stat_description = "Multnomah out-migration standard error" in `row'
	local row = `row' + 1

	replace stat_name = "m_t_out" in `row'
	replace stat_value = `m_t_out' in `row'
	replace stat_description = "Multnomah out-migration t-statistic" in `row'
	local row = `row' + 1

	replace stat_name = "m_p_out" in `row'
	replace stat_value = `m_p_out' in `row'
	replace stat_description = "Multnomah out-migration p-value" in `row'
	local row = `row' + 1

	replace stat_name = "m_pctile_b_out" in `row'
	replace stat_value = `m_pctile_b_out' in `row'
	replace stat_description = "Multnomah out-migration percentile rank" in `row'
	local row = `row' + 1

	replace stat_name = "m_b_in" in `row'
	replace stat_value = `m_b_in' in `row'
	replace stat_description = "Multnomah in-migration coefficient" in `row'
	local row = `row' + 1

	replace stat_name = "m_se_in" in `row'
	replace stat_value = `m_se_in' in `row'
	replace stat_description = "Multnomah in-migration standard error" in `row'
	local row = `row' + 1

	replace stat_name = "m_t_in" in `row'
	replace stat_value = `m_t_in' in `row'
	replace stat_description = "Multnomah in-migration t-statistic" in `row'
	local row = `row' + 1

	replace stat_name = "m_p_in" in `row'
	replace stat_value = `m_p_in' in `row'
	replace stat_description = "Multnomah in-migration p-value" in `row'
	local row = `row' + 1

	replace stat_name = "m_pctile_b_in" in `row'
	replace stat_value = `m_pctile_b_in' in `row'
	replace stat_description = "Multnomah in-migration percentile rank" in `row'
	local row = `row' + 1

	********************************************************************************
	** COUNT STATISTICS
	********************************************************************************

	replace stat_name = "n_counties_out" in `row'
	replace stat_value = `n_counties_out' in `row'
	replace stat_description = "Number of counties with out-migration coef" in `row'
	local row = `row' + 1

	replace stat_name = "n_counties_in" in `row'
	replace stat_value = `n_counties_in' in `row'
	replace stat_description = "Number of counties with in-migration coef" in `row'
	local row = `row' + 1

	replace stat_name = "n_counties_both" in `row'
	replace stat_value = `n_counties_both' in `row'
	replace stat_description = "Number of counties with both coefs" in `row'
	local row = `row' + 1

	replace stat_name = "n_larger_out" in `row'
	replace stat_value = `n_larger_out' in `row'
	replace stat_description = "Counties with larger out-migration coef than Multnomah" in `row'
	local row = `row' + 1

	replace stat_name = "n_larger_out_sig" in `row'
	replace stat_value = `n_larger_out_sig' in `row'
	replace stat_description = "Counties with larger out-migration coef (p<0.05)" in `row'
	local row = `row' + 1

	replace stat_name = "n_smaller_in" in `row'
	replace stat_value = `n_smaller_in' in `row'
	replace stat_description = "Counties with smaller in-migration coef than Multnomah" in `row'
	local row = `row' + 1

	replace stat_name = "n_smaller_in_sig" in `row'
	replace stat_value = `n_smaller_in_sig' in `row'
	replace stat_description = "Counties with smaller in-migration coef (p<0.05)" in `row'
	local row = `row' + 1

	replace stat_name = "n_worse_both" in `row'
	replace stat_value = `n_worse_both' in `row'
	replace stat_description = "Counties worse on both out AND in migration" in `row'
	local row = `row' + 1

	replace stat_name = "n_worse_both_sig" in `row'
	replace stat_value = `n_worse_both_sig' in `row'
	replace stat_description = "Counties worse on both (p<0.05 for both)" in `row'
	local row = `row' + 1

	********************************************************************************
	** DISTRIBUTION STATISTICS - OUT-MIGRATION
	********************************************************************************

	replace stat_name = "dist_out_mean" in `row'
	replace stat_value = `dist_out_mean' in `row'
	replace stat_description = "Distribution mean: out-migration coef" in `row'
	local row = `row' + 1

	replace stat_name = "dist_out_sd" in `row'
	replace stat_value = `dist_out_sd' in `row'
	replace stat_description = "Distribution SD: out-migration coef" in `row'
	local row = `row' + 1

	replace stat_name = "dist_out_min" in `row'
	replace stat_value = `dist_out_min' in `row'
	replace stat_description = "Distribution min: out-migration coef" in `row'
	local row = `row' + 1

	replace stat_name = "dist_out_max" in `row'
	replace stat_value = `dist_out_max' in `row'
	replace stat_description = "Distribution max: out-migration coef" in `row'
	local row = `row' + 1

	replace stat_name = "dist_out_p25" in `row'
	replace stat_value = `dist_out_p25' in `row'
	replace stat_description = "Distribution 25th pctile: out-migration coef" in `row'
	local row = `row' + 1

	replace stat_name = "dist_out_p50" in `row'
	replace stat_value = `dist_out_p50' in `row'
	replace stat_description = "Distribution median: out-migration coef" in `row'
	local row = `row' + 1

	replace stat_name = "dist_out_p75" in `row'
	replace stat_value = `dist_out_p75' in `row'
	replace stat_description = "Distribution 75th pctile: out-migration coef" in `row'
	local row = `row' + 1

	********************************************************************************
	** DISTRIBUTION STATISTICS - IN-MIGRATION
	********************************************************************************

	replace stat_name = "dist_in_mean" in `row'
	replace stat_value = `dist_in_mean' in `row'
	replace stat_description = "Distribution mean: in-migration coef" in `row'
	local row = `row' + 1

	replace stat_name = "dist_in_sd" in `row'
	replace stat_value = `dist_in_sd' in `row'
	replace stat_description = "Distribution SD: in-migration coef" in `row'
	local row = `row' + 1

	replace stat_name = "dist_in_min" in `row'
	replace stat_value = `dist_in_min' in `row'
	replace stat_description = "Distribution min: in-migration coef" in `row'
	local row = `row' + 1

	replace stat_name = "dist_in_max" in `row'
	replace stat_value = `dist_in_max' in `row'
	replace stat_description = "Distribution max: in-migration coef" in `row'
	local row = `row' + 1

	replace stat_name = "dist_in_p25" in `row'
	replace stat_value = `dist_in_p25' in `row'
	replace stat_description = "Distribution 25th pctile: in-migration coef" in `row'
	local row = `row' + 1

	replace stat_name = "dist_in_p50" in `row'
	replace stat_value = `dist_in_p50' in `row'
	replace stat_description = "Distribution median: in-migration coef" in `row'
	local row = `row' + 1

	replace stat_name = "dist_in_p75" in `row'
	replace stat_value = `dist_in_p75' in `row'
	replace stat_description = "Distribution 75th pctile: in-migration coef" in `row'
	local row = `row' + 1

	********************************************************************************
	** DISTRIBUTION STATISTICS - T-STATISTICS
	********************************************************************************

	replace stat_name = "dist_t_out_mean" in `row'
	replace stat_value = `dist_t_out_mean' in `row'
	replace stat_description = "Distribution mean: out-migration t-stat" in `row'
	local row = `row' + 1

	replace stat_name = "dist_t_out_sd" in `row'
	replace stat_value = `dist_t_out_sd' in `row'
	replace stat_description = "Distribution SD: out-migration t-stat" in `row'
	local row = `row' + 1

	replace stat_name = "dist_t_out_p50" in `row'
	replace stat_value = `dist_t_out_p50' in `row'
	replace stat_description = "Distribution median: out-migration t-stat" in `row'
	local row = `row' + 1

	replace stat_name = "dist_t_in_mean" in `row'
	replace stat_value = `dist_t_in_mean' in `row'
	replace stat_description = "Distribution mean: in-migration t-stat" in `row'
	local row = `row' + 1

	replace stat_name = "dist_t_in_sd" in `row'
	replace stat_value = `dist_t_in_sd' in `row'
	replace stat_description = "Distribution SD: in-migration t-stat" in `row'
	local row = `row' + 1

	replace stat_name = "dist_t_in_p50" in `row'
	replace stat_value = `dist_t_in_p50' in `row'
	replace stat_description = "Distribution median: in-migration t-stat" in `row'
	local row = `row' + 1

	** Drop empty rows
	drop if stat_name == ""

	** Add sample identifier
	gen sample = "`sample'"

	** Export to Excel
	** First iteration: create new file; subsequent: add sheet
	if "`sample'" == "all" {
		export excel using "${results}flows/flow_analysis_stats`debug_txt'.xlsx", ///
			sheet("`sample'") sheetreplace firstrow(variables)
	}
	else {
		export excel using "${results}flows/flow_analysis_stats`debug_txt'.xlsx", ///
			sheet("`sample'") sheetmodify firstrow(variables)
	}

	dis "Exported descriptive statistics to flow_analysis_stats.xlsx, sheet: `sample'"

	********************************************************************************
	** EXPORT COUNTY LISTS WITH LARGER COEFFICIENTS TO ADDITIONAL SHEETS
	********************************************************************************

	** Load county coefficients and merge with county identifiers
	use `county_coefs_for_export', clear
	merge m:1 fips using "${data}working/ids", keep(master match) nogen ///
		keepusing(state_name county_name state_fips county_fips)

	** Order variables for cleaner output
	order fips state_name county_name state_fips county_fips multnomah ///
		b_out se_out t_out p_out pctile_b_out pctile_t_out ///
		b_in se_in t_in p_in pctile_b_in pctile_t_in

	** Save merged data for subsequent sheets
	tempfile county_coefs_merged
	save `county_coefs_merged'

	** Sheet 1: Counties with larger out-migration coefficients than Multnomah
	use `county_coefs_merged', clear
	keep if b_out > `m_b_out' & !missing(b_out)

	if _N > 0 {
		gsort -b_out

		** Keep relevant variables
		keep fips state_name county_name b_out se_out t_out p_out pctile_b_out pctile_t_out

		** Export
		export excel using "${results}flows/flow_analysis_stats`debug_txt'.xlsx", ///
			sheet("larger_out_`sample'") sheetmodify firstrow(variables)

		dis "Exported `=_N' counties with larger out-migration coef to sheet: larger_out_`sample'"
	}
	else {
		dis "No counties with larger out-migration coef than Multnomah - skipping sheet: larger_out_`sample'"
	}

	** Sheet 2: Counties with smaller in-migration coefficients than Multnomah
	use `county_coefs_merged', clear
	keep if b_in < `m_b_in' & !missing(b_in)

	if _N > 0 {
		gsort b_in

		** Keep relevant variables
		keep fips state_name county_name b_in se_in t_in p_in pctile_b_in pctile_t_in

		** Export
		export excel using "${results}flows/flow_analysis_stats`debug_txt'.xlsx", ///
			sheet("smaller_in_`sample'") sheetmodify firstrow(variables)

		dis "Exported `=_N' counties with smaller in-migration coef to sheet: smaller_in_`sample'"
	}
	else {
		dis "No counties with smaller in-migration coef than Multnomah - skipping sheet: smaller_in_`sample'"
	}

	** Sheet 3: Counties worse on BOTH dimensions (larger out AND smaller in)
	use `county_coefs_merged', clear
	keep if b_out > `m_b_out' & b_in < `m_b_in' & !missing(b_out) & !missing(b_in)

	if _N > 0 {
		gsort -b_out

		** Keep relevant variables
		keep fips state_name county_name ///
			b_out se_out t_out p_out pctile_b_out pctile_t_out ///
			b_in se_in t_in p_in pctile_b_in pctile_t_in

		** Export
		export excel using "${results}flows/flow_analysis_stats`debug_txt'.xlsx", ///
			sheet("worse_both_`sample'") sheetmodify firstrow(variables)

		dis "Exported `=_N' counties worse on both dimensions to sheet: worse_both_`sample'"
	}
	else {
		dis "No counties worse on both dimensions than Multnomah - skipping sheet: worse_both_`sample'"
	}

	** Sheet 4: Counties with larger out-migration t-statistics than Multnomah
	use `county_coefs_merged', clear
	keep if t_out > `m_t_out' & !missing(t_out)

	if _N > 0 {
		gsort -t_out

		** Keep relevant variables
		keep fips state_name county_name b_out se_out t_out p_out pctile_b_out pctile_t_out

		** Export
		export excel using "${results}flows/flow_analysis_stats`debug_txt'.xlsx", ///
			sheet("larger_t_out_`sample'") sheetmodify firstrow(variables)

		dis "Exported `=_N' counties with larger out-migration t-stat to sheet: larger_t_out_`sample'"
	}
	else {
		dis "No counties with larger out-migration t-stat than Multnomah - skipping sheet: larger_t_out_`sample'"
	}

	** Sheet 5: Counties with smaller in-migration t-statistics than Multnomah
	use `county_coefs_merged', clear
	keep if t_in < `m_t_in' & !missing(t_in)

	if _N > 0 {
		gsort t_in

		** Keep relevant variables
		keep fips state_name county_name b_in se_in t_in p_in pctile_b_in pctile_t_in

		** Export
		export excel using "${results}flows/flow_analysis_stats`debug_txt'.xlsx", ///
			sheet("smaller_t_in_`sample'") sheetmodify firstrow(variables)

		dis "Exported `=_N' counties with smaller in-migration t-stat to sheet: smaller_t_in_`sample'"
	}
	else {
		dis "No counties with smaller in-migration t-stat than Multnomah - skipping sheet: smaller_t_in_`sample'"
	}

	********************************************************************************
	** REGRESSION 2: Multnomah X Year (Event Study)
	********************************************************************************

	** Load main data for event study regressions
	use `main_data', clear

	** Loop over outcome variables
	foreach outcome in n1 n2 agi {

		** Regression 2a: With covariates
		ppmlhdfe `outcome' x_out_* x_in_* 								///
			`covars' 													///
			if mover == 1 `sample_cond', 								///
			absorb(year flow_id) vce(cluster flow_id)
		estimates store `outcome'_es_with_covars

		** Regression 2b: Without covariates
		ppmlhdfe `outcome' x_out_* x_in_* 								///
			if mover == 1 `sample_cond', 								///
			absorb(year flow_id) vce(cluster flow_id)
		estimates store `outcome'_es_no_covars

		** Create coefficient dataset for plotting
		clear
		set obs 7
		gen year = 2015 + _n

		** Out-migration coefficients (with covariates)
		gen out_coef_wc = .
		gen out_ci_lo_wc = .
		gen out_ci_hi_wc = .

		** In-migration coefficients (with covariates)
		gen in_coef_wc = .
		gen in_ci_lo_wc = .
		gen in_ci_hi_wc = .

		** Out-migration coefficients (without covariates)
		gen out_coef_nc = .
		gen out_ci_lo_nc = .
		gen out_ci_hi_nc = .

		** In-migration coefficients (without covariates)
		gen in_coef_nc = .
		gen in_ci_lo_nc = .
		gen in_ci_hi_nc = .

		** Fill in coefficients (2020 = base year = 0)
		foreach y in 2016 2017 2018 2019 2021 2022 {

			** With covariates
			estimates restore `outcome'_es_with_covars
			replace out_coef_wc = _b[x_out_`y'] if year == `y'
			replace out_ci_lo_wc = _b[x_out_`y'] - 1.96 * _se[x_out_`y'] if year == `y'
			replace out_ci_hi_wc = _b[x_out_`y'] + 1.96 * _se[x_out_`y'] if year == `y'

			replace in_coef_wc = _b[x_in_`y'] if year == `y'
			replace in_ci_lo_wc = _b[x_in_`y'] - 1.96 * _se[x_in_`y'] if year == `y'
			replace in_ci_hi_wc = _b[x_in_`y'] + 1.96 * _se[x_in_`y'] if year == `y'

			** Without covariates
			estimates restore `outcome'_es_no_covars
			replace out_coef_nc = _b[x_out_`y'] if year == `y'
			replace out_ci_lo_nc = _b[x_out_`y'] - 1.96 * _se[x_out_`y'] if year == `y'
			replace out_ci_hi_nc = _b[x_out_`y'] + 1.96 * _se[x_out_`y'] if year == `y'

			replace in_coef_nc = _b[x_in_`y'] if year == `y'
			replace in_ci_lo_nc = _b[x_in_`y'] - 1.96 * _se[x_in_`y'] if year == `y'
			replace in_ci_hi_nc = _b[x_in_`y'] + 1.96 * _se[x_in_`y'] if year == `y'
		}

		** Base year (2020) = 0
		foreach v in out_coef_wc out_ci_lo_wc out_ci_hi_wc 			///
					 in_coef_wc in_ci_lo_wc in_ci_hi_wc 			///
					 out_coef_nc out_ci_lo_nc out_ci_hi_nc 			///
					 in_coef_nc in_ci_lo_nc in_ci_hi_nc {
			replace `v' = 0 if year == 2020
		}

		** Offset years slightly for visibility
		gen year_out = year - 0.1
		gen year_in = year + 0.1

		** Plot 1: Out-migration (with and without covariates)
		twoway 	(rcap out_ci_lo_wc out_ci_hi_wc year_out, lc("`col_out'")) 		///
				(connected out_coef_wc year_out, mc("`col_out'") lc("`col_out'") ms(O)) ///
				(rcap out_ci_lo_nc out_ci_hi_nc year_in, lc("`col_in'")) 		///
				(connected out_coef_nc year_in, mc("`col_in'") lc("`col_in'") ms(T)), ///
			yline(0, lc(gs10) lp(dash)) 										///
			xline(2020.5, lc(black) lp(solid))									///
			xlabel(2016(1)2022) 												///
			ytitle("Coefficient (relative to 2019)") 							///
			xtitle("Year")														///
			legend(order(2 "With Covariates" 4 "Without Covariates") 			///
				pos(6) rows(1)) 												///
			graphregion(color(white))

		graph export "${results}flows/fig_multnomah_out_`outcome'`file_suffix'.png", replace

		** Plot 2: In-migration (with and without covariates)
		twoway 	(rcap in_ci_lo_wc in_ci_hi_wc year_out, lc("`col_out'")) 		///
				(connected in_coef_wc year_out, mc("`col_out'") lc("`col_out'") ms(O)) ///
				(rcap in_ci_lo_nc in_ci_hi_nc year_in, lc("`col_in'")) 		///
				(connected in_coef_nc year_in, mc("`col_in'") lc("`col_in'") ms(T)), ///
			yline(0, lc(gs10) lp(dash)) 										///
			xline(2020.5, lc(black) lp(solid))									///
			xlabel(2016(1)2022) 												///
			ytitle("Coefficient (relative to 2019)") 							///
			xtitle("Year")														///
			legend(order(2 "With Covariates" 4 "Without Covariates") 			///
				pos(6) rows(1)) 												///
			graphregion(color(white))

		graph export "${results}flows/fig_multnomah_in_`outcome'`file_suffix'.png", replace

		** Plot 3: Both flows (with covariates only, as in original)
		twoway 	(rcap out_ci_lo_wc out_ci_hi_wc year_out, lc("`col_out'")) 		///
				(connected out_coef_wc year_out, mc("`col_out'") lc("`col_out'") ms(O)) ///
				(rcap in_ci_lo_wc in_ci_hi_wc year_in, lc("`col_in'")) 		///
				(connected in_coef_wc year_in, mc("`col_in'") lc("`col_in'") ms(T)), ///
			yline(0, lc(gs10) lp(dash)) 										///
			xline(2020.5, lc(black) lp(solid))									///
			xlabel(2016(1)2022) 												///
			ytitle("Coefficient (relative to 2019)") 							///
			xtitle("Year")														///
			legend(order(2 "Out-migration" 4 "In-migration") 					///
				pos(6) rows(1)) 												///
			graphregion(color(white))

		graph export "${results}flows/fig_multnomah_both_`outcome'`file_suffix'.png", replace

		if ${overleaf} == 1 {
			graph export "${ol_fig}fig_multnomah_both_`outcome'`file_suffix'.png", replace
		}

		** Reload main data for next outcome
		use `main_data', clear

	} // END OUTCOME LOOP

} // END SAMPLE LOOP (all vs acs)


********************************************************************************
** APPENDIX TABLE: PPML flow regression results (item 21c of May 2026 TODO)
**
** Combines the four sample × covars-or-not specifications into one
** appendix table parallel to the DiD table (tab_did_combined.tex).
** Uses the sample-tagged estimates stored inside the loop above.
********************************************************************************

dis ""
dis "=============================================="
dis "Appendix Table: PPML flow regression results"
dis "=============================================="

** Confirm all four AGI estimates exist before exporting (defensive)
local all_present = 1
foreach est in agi_all_with agi_all_no agi_acs_with agi_acs_no {
	capture estimates dir `est'
	if _rc != 0 local all_present = 0
}

if `all_present' == 1 {
	** esttab the four AGI specs into one 4-column table.
	** Coefficients of interest: 1.out_multnomah_post and 1.in_multnomah_post.
	esttab agi_all_no agi_all_with agi_acs_no agi_acs_with ///
		using "${results}flows/tab_flow_regression.tex", replace ///
		keep(1.out_multnomah_post 1.in_multnomah_post) ///
		coeflabels(1.out_multnomah_post "Multnomah origin $\times$ Post" ///
		           1.in_multnomah_post  "Multnomah destination $\times$ Post") ///
		mtitles("All, no cov." "All, cov." "ACS, no cov." "ACS, cov.") ///
		b(%9.3f) se(%9.3f) ///
		star(* 0.10 ** 0.05 *** 0.01) ///
		stats(N, fmt(%9.0fc) labels("Observations")) ///
		nonotes booktabs label fragment ///
		prehead("\begin{tabular}{l*{4}{w{c}{2.5cm}}}" "\toprule") ///
		postfoot("\bottomrule" "\end{tabular}")
	dis "Wrote: ${results}flows/tab_flow_regression.tex"

	if "${overleaf}" == "1" {
		copy "${results}flows/tab_flow_regression.tex" ///
			"${ol_tab}tab_flow_regression.tex", replace
		dis "Synced: ${ol_tab}tab_flow_regression.tex"
	}
}
else {
	dis as error "Skipping flow-regression appendix table: not all 4 AGI estimates are stored."
}


** Clean up parallel data file
capture erase "${data}working/flow_analysis_data.dta"

** Close log
clear
log close log_02
