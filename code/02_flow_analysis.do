/*******************************************************************************
File Name: 		02_flow_analysis.do
Creator: 		John Iselin
Date Update:	January 15, 2026

Called by: 00_multnomah.do

Purpose: Perform flow-based analysis on IRS data.

Outputs:
All outputs are produced twice: once for all counties and once for ACS counties
only (with _acs suffix). ACS sample includes property tax rate controls.

- county_did_coefficients[_acs].dta/csv: DiD coefficients for all counties
  (b_out, b_in, se_out, se_in, t-stats, p-values, percentile ranks)
- fig_multnomah_post_*[_acs].png: DiD coefficient plots for Multnomah
- fig_hist_out_*[_acs].png: Histogram of out-migration coefficients
- fig_hist_in_*[_acs].png: Histogram of in-migration coefficients
- fig_scatter_out_in_*[_acs].png: Scatter plot of out vs in coefficients
- fig_multnomah_out_*[_acs].png: Event study plots for out-migration
- fig_multnomah_in_*[_acs].png: Event study plots for in-migration
- fig_multnomah_both_*[_acs].png: Combined event study plots

Authors: John Iselin

For more information, contact john.iselin@yale.edu

*******************************************************************************/


** Start log file
capture log close log_02
log using "${logs}02_log_flow_${date}", replace text name(log_02)

** Parameters
local reps = 100

** Load ACS data to get the set of counties 
use "${data}working/acs_county_gross_18plus", clear

** Keep required variables 
keep year fips 

** Keep only always-observed fips 
bysort fips: gen ct = _N
tab ct 
keep if ct == 10 

** Keep fips 
keep fips 

** Drop duplicates 
duplicates drop 

** Preserve data 
tempfile acs_fips 
save `acs_fips'
clear 

** Load IRS Data
use "${data}working/irs_county_flow.dta", clear

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

********************************************************************************
** MAIN ANALYSIS LOOP: Run regressions for (1) all counties, (2) ACS counties
** The second iteration restricts to flows where acs_flow == 1 and adds
** property tax rate controls (prop_rate_mean_o and prop_rate_mean_d)
********************************************************************************

foreach sample in "all" "acs" {

	** Set sample-specific parameters
	if "`sample'" == "all" {
		local sample_cond ""
		local covars "unemp_* pop_* per_capita_income_*"
		local file_suffix ""
		local title_suffix ""
	}
	else if "`sample'" == "acs" {
		local sample_cond "& acs_flow == 1"
		local covars "unemp_* pop_* per_capita_income_* prop_rate_mean_o prop_rate_mean_d"
		local file_suffix "_acs"
		local title_suffix " (ACS Counties)"
	}

	dis ""
	dis "========================================================================"
	dis "RUNNING ANALYSIS FOR SAMPLE: `sample'"
	dis "Sample condition: `sample_cond'"
	dis "Covariates: `covars'"
	dis "========================================================================"
	dis ""

********************************************************************************
** REGRESSION 1: Multnomah X Post (DiD)
********************************************************************************

** Loop over outcome variables
foreach outcome in n1 n2 agi {

	** Set outcome label for graphs
	if "`outcome'" == "n1" local outcome_label "Number of Returns"
	if "`outcome'" == "n2" local outcome_label "Number of Exemptions"
	if "`outcome'" == "agi" local outcome_label "Adjusted Gross Income"

	** Regression 1a: With covariates
	ppmlhdfe `outcome' i.out_multnomah_post i.in_multnomah_post 	///
		`covars' 													///
		if mover == 1 `sample_cond', 								///
		absorb(year flow_id) vce(cluster flow_id)
	estimates store `outcome'_with_covars

	** Regression 1b: Without covariates
	ppmlhdfe `outcome' i.out_multnomah_post i.in_multnomah_post 	///
		if mover == 1 `sample_cond', 								///
		absorb(year flow_id) vce(cluster flow_id)
	estimates store `outcome'_no_covars

	** Plot both regressions
	coefplot 	(`outcome'_with_covars, label("With Covariates") 	///
					mc(navy) ciopts(lc(navy))) 						///
				(`outcome'_no_covars, label("Without Covariates") 	///
					mc(maroon) ciopts(lc(maroon))), 				///
		keep(1.out_multnomah_post 1.in_multnomah_post) 				///
		ciopts(recast(rcap)) 										///
		yline(0, lc(gs10) lp(dash)) 								///
		xline(0, lc(gs10) lp(dash)) 								///
		coeflabels(	1.out_multnomah_post = "Out-migration" 			///
					1.in_multnomah_post = "In-migration") 			///
		ytitle("Coefficient") 										///
		xtitle("")													///
		legend(pos(6) rows(1)) 										///
		title("Migration Flows: Multnomah County x Post`title_suffix'")	///
		subtitle("`outcome_label'")									///
		graphregion(color(white))

	graph export "${results}flows/fig_multnomah_post_`outcome'`file_suffix'.png", replace


} // END OUTCOME LOOP

********************************************************************************
** REGRESSION 1b: Loop over all counties (Permutation-style analysis)
** Purpose: Run same DiD regression for each county to create distribution
**          of treatment effects for comparison with Multnomah
********************************************************************************

** Preserve data for later use in REGRESSION 2
preserve

** Get list of unique origin fips codes (respecting sample condition)
if "`sample'" == "all" {
	levelsof fips_o, local(fips_list)
}
else {
	levelsof fips_o if acs_flow == 1, local(fips_list)
}
local n_fips : word count `fips_list'
dis "Running regressions for `n_fips' counties (`sample' sample)..."

** Initialize tempfile to store results
tempfile county_results
postfile county_coefs fips b_out se_out b_in se_in using `county_results', replace

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

	** Run regression with capture to handle potential errors
	capture {
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

** Load results
use `county_results', clear

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
egen rank_b_out = rank(b_out)
egen rank_b_in = rank(b_in)
qui count if !missing(b_out)
gen pctile_out = 100 * rank_b_out / r(N)
qui count if !missing(b_in)
gen pctile_in = 100 * rank_b_in / r(N)

label var pctile_out "Percentile rank of out-migration coef"
label var pctile_in "Percentile rank of in-migration coef"

** Display Multnomah's position
dis ""
dis "=========================================="
dis "Multnomah County Results:"
dis "=========================================="
summ b_out b_in pctile_out pctile_in if multnomah == 1

** Display distribution summary
dis ""
dis "Distribution of coefficients (all counties):"
summ b_out b_in, detail

** Save county coefficients dataset
compress
save "${data}working/county_did_coefficients`file_suffix'.dta", replace
export delimited using "${data}working/county_did_coefficients`file_suffix'.csv", replace

dis "County coefficients saved to ${data}working/county_did_coefficients`file_suffix'.dta"

** Restore original data for REGRESSION 2
restore

********************************************************************************
** SECTION: Multnomah vs Distribution Plots (Histogram Comparison)
** Purpose: Plot Multnomah's coefficients against distribution of all counties
********************************************************************************

** Load the county coefficients
preserve
use "${data}working/county_did_coefficients`file_suffix'.dta", clear

** Get Tags for 
gen tag_in = 0 
gen tag_out = 0 

** Get number of counties with larger coef. 
foreach x in "in" "out" {
	
	** Get Multnomah value 
	qui summ b_`x' if multnomah == 1
	local m_b = r(mean)
	
	** Counts 
	dis "Count of FIPS with non-missing `x'-migration flow coef."
	count if b_`x' != . 
	dis "Count of FIPS with non-missing `x'-migration flow coef. greater than Multnomah"
	if "`x'" == "in" {
		count if b_`x' < `m_b' & !missing( b_`x')
		count if b_`x' < `m_b' & !missing( b_`x') &  p_`x' < 0.05
		replace tag_`x' = 1 if b_`x' < `m_b' & !missing( b_`x')
	}	
	if "`x'" == "out" 	{
		count if b_`x' > `m_b' & !missing( b_`x')
		count if b_`x' > `m_b' & !missing( b_`x') &  p_`x' < 0.05
		replace tag_`x' = 1 if b_`x' > `m_b' & !missing( b_`x')
	}	
} // END IN-OUT LOOP 

** 
tab tag_in tag_out

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
	summ pctile_out if multnomah == 1, meanonly
	local multnomah_pctile_out = r(mean)
	summ pctile_in if multnomah == 1, meanonly
	local multnomah_pctile_in = r(mean)

	
	** Plot 1: Out-migration coefficient distribution with Multnomah
	histogram `x'_out if !missing(`x'_out), 								///
		bin(50) 														///
		fcolor(navy%50) lcolor(navy) 									///
		xline(`multnomah_`x'_out', lcolor(red) lwidth(thick) lpattern(solid)) ///
		xtitle("Out-migration `txt' (County x Post)") 			///
		ytitle("Frequency") 											///
		title("Distribution of Out-Migration `txt'`title_suffix'") 		///
		subtitle("Red line = Multnomah County (percentile: `: display %4.1f `multnomah_pctile_out'')") ///
		graphregion(color(white))

	graph export "${results}flows/fig_hist_out_`x'`file_suffix'.png", replace

	** Plot 2: In-migration coefficient distribution with Multnomah
	histogram `x'_in if !missing(`x'_in), 									///
		bin(50) 														///
		fcolor(maroon%50) lcolor(maroon) 								///
		xline(`multnomah_`x'_in', lcolor(red) lwidth(thick) lpattern(solid)) ///
		xtitle("In-migration `txt' (County x Post)") 				///
		ytitle("Frequency") 											///
		title("Distribution of In-Migration `txt'`title_suffix'") 		///
		subtitle("Red line = Multnomah County (percentile: `: display %4.1f `multnomah_pctile_in'')") ///
		graphregion(color(white))

	graph export "${results}flows/fig_hist_in_`x'`file_suffix'.png", replace

	** Plot 3: Combined scatter of out vs in coefficients
	twoway 	(scatter `x'_in `x'_out if multnomah == 0, 						///
				mc(navy%30) ms(O) msize(small)) 						///
			(scatter `x'_in `x'_out if multnomah == 1, 						///
				mc(red) ms(D) msize(large)), 							///
		xline(0, lc(gs10) lp(dash)) 									///
		yline(0, lc(gs10) lp(dash)) 									///
		xtitle("Out-migration `txt'") 							///
		ytitle("In-migration `txt'") 								///
		title("Out- vs In-Migration Effects by County`title_suffix'") 	///
		legend(order(1 "Other Counties" 2 "Multnomah County") pos(6) rows(1)) ///
		graphregion(color(white))

	graph export "${results}flows/fig_scatter_out_in_`x'`file_suffix'.png", replace

	
} // END BETA / T-STAT LOOP 

** Restore and continue
restore

********************************************************************************
** REGRESSION 2: Multnomah X Year (Event Study)
********************************************************************************

** Loop over outcome variables
foreach outcome in n1 n2 agi {

	** Set outcome label for graphs
	if "`outcome'" == "n1" local outcome_label "Number of Returns"
	if "`outcome'" == "n2" local outcome_label "Number of Exemptions"
	if "`outcome'" == "agi" local outcome_label "Adjusted Gross Income"

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

	** Extract coefficients into a dataset and plot
	preserve
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
		twoway 	(rcap out_ci_lo_wc out_ci_hi_wc year_out, lc(navy)) 			///
				(connected out_coef_wc year_out, mc(navy) lc(navy) ms(O)) 		///
				(rcap out_ci_lo_nc out_ci_hi_nc year_in, lc(maroon)) 			///
				(connected out_coef_nc year_in, mc(maroon) lc(maroon) ms(T)),	///
			yline(0, lc(gs10) lp(dash)) 										///
			xline(2020.5, lc(black) lp(solid))									///
			xlabel(2016(1)2022) 												///
			ytitle("Coefficient (relative to 2020)") 							///
			xtitle("Year")														///
			legend(order(2 "With Covariates" 4 "Without Covariates") 			///
				pos(6) rows(1)) 												///
			title("Out-Migration from Multnomah County`title_suffix'")			///
			subtitle("`outcome_label'")											///
			graphregion(color(white))

		graph export "${results}flows/fig_multnomah_out_`outcome'`file_suffix'.png", replace

		** Plot 2: In-migration (with and without covariates)
		twoway 	(rcap in_ci_lo_wc in_ci_hi_wc year_out, lc(navy)) 				///
				(connected in_coef_wc year_out, mc(navy) lc(navy) ms(O)) 		///
				(rcap in_ci_lo_nc in_ci_hi_nc year_in, lc(maroon)) 				///
				(connected in_coef_nc year_in, mc(maroon) lc(maroon) ms(T)),	///
			yline(0, lc(gs10) lp(dash)) 										///
			xline(2020.5, lc(black) lp(solid))									///
			xlabel(2016(1)2022) 												///
			ytitle("Coefficient (relative to 2020)") 							///
			xtitle("Year")														///
			legend(order(2 "With Covariates" 4 "Without Covariates") 			///
				pos(6) rows(1)) 												///
			title("In-Migration to Multnomah County`title_suffix'")				///
			subtitle("`outcome_label'")											///
			graphregion(color(white))

		graph export "${results}flows/fig_multnomah_in_`outcome'`file_suffix'.png", replace

		** Plot 3: Both flows (with covariates only, as in original)
		twoway 	(rcap out_ci_lo_wc out_ci_hi_wc year_out, lc(navy)) 				///
				(connected out_coef_wc year_out, mc(navy) lc(navy) ms(O)) 		///
				(rcap in_ci_lo_wc in_ci_hi_wc year_in, lc(maroon)) 				///
				(connected in_coef_wc year_in, mc(maroon) lc(maroon) ms(T)),	///
			yline(0, lc(gs10) lp(dash)) 										///
			xline(2020.5, lc(black) lp(solid))									///
			xlabel(2016(1)2022) 												///
			ytitle("Coefficient (relative to 2020)") 							///
			xtitle("Year")														///
			legend(order(2 "Out-migration" 4 "In-migration") 					///
				pos(6) rows(1)) 												///
			title("Migration Flows: Multnomah County`title_suffix'")			///
			subtitle("`outcome_label'")											///
			graphregion(color(white))

		graph export "${results}flows/fig_multnomah_both_`outcome'`file_suffix'.png", replace

	restore

} // END OUTCOME LOOP

} // END SAMPLE LOOP (all vs acs)


** Close log
clear
log close log_02
