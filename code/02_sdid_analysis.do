/*******************************************************************************
File Name: 		02_sdid_analysis.do
Creator: 		John Iselin
Date Update:	January 13, 2025

Called by: 00_multnomah.do

Purpose: Perform synthetic difference-in-difference estimation.

Outputs:
- sdid_weights.dta/xlsx: Synthetic control weights for each specification
- sdid_results.dta/xlsx: Treatment effects, SEs, p-values for each specification
- fig_speccurve_*.pdf/jpg: Specification curve plots

TODO:
1) Recreate IRS sample with solely counties in ACS sample.

Authors: John Iselin

For more information, contact john.iselin@yale.edu

*******************************************************************************/


** Start log file 
capture log close log_02
log using "${logs}02_log_sdid_${date}", replace text name(log_02)

** Parameters 
local reps = 100

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

** Define and standardize covariates
** Note: prop_tax_rate added for non-IRS full sample specifications
local all_covariates "population per_capita_income prop_tax_rate"
foreach v of local all_covariates {
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

** Before your loops, set up weights dataset
preserve
clear
set obs 0
gen fips = .
gen weight = .
gen sample_data = ""
gen out = ""
gen sample = ""
gen controls = .
gen exclusion = .
save "${results}sdid/sdid_weights.dta", replace
clear
restore

** Set up treatment effects dataset
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
save "${results}sdid/sdid_results.dta", replace
clear
restore 

** Loop over IRS and ACS Samples
foreach data of varlist irs_sample_1 irs_sample_2 acs_period_1 acs_period_2  {

	** Define covariates based on data type
	** irs_sample_1 uses basic covariates; all others add property tax rate
	if "`data'" == "irs_sample_1" local covariates "population per_capita_income"
	else local covariates "population per_capita_income prop_tax_rate"

	** Different sets of outcome variables
	if "`data'" == "irs_sample_1" local out_type "irs"
	else if "`data'" == "irs_sample_2" local out_type "irs"
	else local out_type "acs1 acs2"

	** Loop over Outcome var type 
	foreach type of local out_type {
		
		** Labels
		if "`data'" == "irs_sample_1" local out_txt "irs_full_16_22"
		else if "`data'" == "irs_sample_2" local out_txt "irs_389_16_22"
		else if "`data'" == "acs_period_1" & "`type'" == "acs1" local out_txt "acs_16_22_all"
		else if "`data'" == "acs_period_1" & "`type'" == "acs2" local out_txt "acs_16_22_col"
		else if "`data'" == "acs_period_1" & "`type'" == "acs3" local out_txt "acs_16_22_noc"
		else if "`data'" == "acs_period_1" & "`type'" == "acs4" local out_txt "acs_16_22_3D"
		else if "`data'" == "acs_period_2" & "`type'" == "acs1" local out_txt "acs_16_24_all"
		else if "`data'" == "acs_period_2" & "`type'" == "acs2" local out_txt "acs_16_24_col"
		else if "`data'" == "acs_period_2" & "`type'" == "acs3" local out_txt "acs_16_24_noc"
		else if "`data'" == "acs_period_2" & "`type'" == "acs4" local out_txt "acs_16_24_3D"

		** Check if subfolder exists, create if not
		capture mkdir "${results}sdid/`out_txt'"

		** Loop over samples
		foreach samp of varlist sample_all sample_urban95 sample_urban95_covid {	
			
			** Loop over exclusion of 2019-2020 period 
			forvalues exl = 1(-1)0 {
				
				** Define sample 	
				gen sample = `samp' == 1 & `data' == 1 
				if `exl' == 1 replace sample = 0 if year == 2020 
				
				** Clear stored values 
				eststo clear 
				
				** Loop over migration type 
				foreach migr in "net" "in" "out" {
					
					** Loop over outcomes 
					foreach out of varlist	n1_`migr'_rate_`type'	///
											n2_`migr'_rate_`type' 	///
											agi_`migr'_rate_`type' {
					
						** Store label 
						local label : variable label `out'
						
						** Loop over inclusion of covariates
						forvalues c = 0/1 {
						
							** Covariates
							if `c' == 0 local covars ""
							else if `c' == 1 local covars "covariates(`covariates', projected)"

							** Covariates for sdid_event (doesn't support 'projected' option)
							if `c' == 0 local covars_event ""
							else if `c' == 1 local covars_event "covariates(`covariates')"
							
							** File Name 
							if `exl' == 0 local path "${results}sdid/`out_txt'/fig_`out_txt'_`out'_`c'_`samp'_"
							if `exl' == 1 local path "${results}sdid/`out_txt'/fig_`out_txt'_`out'_`c'_`samp'_excl2020_"
		
							
							** Run SDID
							eststo sdid_`out'_`c': sdid `out' fips year Treated	///
								if sample == 1,			 	///
								vce(placebo) 				///
								`covars'					///
								reps(`reps')				///
								graph graph_export("`path'", .pdf)

							** Store model output 
							local tmp_tau = e(ATT)
							local tmp_se = e(se)
							matrix omega = e(omega)
							
							** Store pre-treatment means and county counts 
							qui summ `out' if multnomah == 1 & Treated == 0
							local tmp_premean = r(mean)
							estadd scalar mean = r(mean)
							
							qui summ `out' if year == 2021 & sample == 1
							local tmp_ncounties = r(N)
							estadd scalar count = r(N)	
							
							** Preserve ATE / SE
							preserve
							clear
							qui set obs 1
							gen sample_data = "`out_txt'"
							gen sample = "`samp'"
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
							append using "${results}sdid/sdid_results.dta"
							compress
							save "${results}sdid/sdid_results.dta", replace
							clear
							restore
							
							** Store weights
							preserve
							clear
							svmat double omega, names(col)
							rename c2 fips
							rename c1 weight
							gen sample_data = "`out_txt'"
							gen out = "`out'"
							gen sample = "`samp'"
							gen controls = `c'
							gen exclusion = `exl'
							drop if weight == 0
							drop if missing(fips )
							order sample_data sample out controls exclusion fips weight
							sort weight
							append using "${results}sdid/sdid_weights.dta"
							compress
							save "${results}sdid/sdid_weights.dta", replace
							clear 
							restore 
							
							** Run event-study
							sdid_event `out' fips year Treated			///
								if sample == 1,			 			///
								`covars_event'						///
								vce(placebo) 						///
								brep(`reps') 						///
								placebo(all)

							** Clean up sdid_event internal variables
							capture drop ever_treated*
							
							** Create Figure

							** Store max year
							qui summ year if multnomah == 1 & sample == 1
							local max_yr = r(max)
							dis "`max_yr'"

							** Move results from matrix to data
							qui count if multnomah == 1 & sample == 1
							local ct = r(N)
							local ct = `ct' + 1
							matrix list e(H)
							mat res = e(H)[2..`ct',1..5]

							** Preserve data before plotting (expand creates duplicate obs)
							preserve

							** Move Matrix results to data
							svmat res

							** Generate ID variable
							gen id = `max_yr' - _n + 1 if !missing(res1)

							** Update labeling for exclusion of 2020
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

							if `exl' == 0 local path "${results}sdid/`out_txt'/fig_`out_txt'_`out'_`c'_`samp'_eventstudy.jpg"
							if `exl' == 1 local path "${results}sdid/`out_txt'/fig_`out_txt'_`out'_`c'_`samp'_excl2020_eventstudy.jpg"

							graph export "`path'", 	///
								as(jpg) name("Graph") quality(100) replace

							** Restore data (removes expanded rows and temp variables)
							restore 
											
						} // END COVAR LOOP 
					
					} // END OUTCOME LOOP 
					
					** Determine name
					if `exl' == 0 local path "${results}sdid/`out_txt'/tab_sdid_`out_txt'_`migr'_`samp'.tex"
					if `exl' == 1 local path "${results}sdid/`out_txt'/tab_sdid_`out_txt'_`migr'_`samp'_excl2020.tex"
					

					** Table of results
					if "`data'" == "irs_sample_1" | "`data'" == "irs_sample_2" {
					
					esttab 	sdid_n1_`migr'_rate_`type'_0 sdid_n1_`migr'_rate_`type'_1	///
							sdid_n2_`migr'_rate_`type'_0 sdid_n2_`migr'_rate_`type'_1	///
							sdid_agi_`migr'_rate_`type'_0 sdid_agi_`migr'_rate_`type'_1 ///
						using "`path'",								///
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
					else {
					esttab 	sdid_n1_`migr'_rate_`type'_0 sdid_n1_`migr'_rate_`type'_1	///
							sdid_n2_`migr'_rate_`type'_0 sdid_n2_`migr'_rate_`type'_1	///
							sdid_agi_`migr'_rate_`type'_0 sdid_agi_`migr'_rate_`type'_1 ///
						using "`path'",								///
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

				
				} // END MIGRATION TYPE LOOP 
				
				** Drop sample var 
				drop sample 
				
			} // END EXCLUSION LOOP 
			
		} // END SAMPLE LOOP 
	
	} // END OUT TYPE 
	
} // END DATA LOOP 


** After all loops complete, export weights
use "${results}sdid/sdid_weights.dta", clear
export excel using "${results}sdid/sdid_weights.xlsx", firstrow(variables) replace

** Export treatment effects
use "${results}sdid/sdid_results.dta", clear
export excel using "${results}sdid/sdid_results.xlsx", firstrow(variables) replace

/*******************************************************************************
SPECIFICATION CURVE ANALYSIS
Creates specification curve plots showing treatment effects across all
specifications for each outcome type and migration direction.

Coefficient colors:
- Navy: Statistically significant (p<0.05), not preferred
- Light blue: Statistically insignificant, not preferred
- Red: Statistically significant (p<0.05), preferred specification
- Orange: Statistically insignificant, preferred specification

Preferred specifications are defined by the local macro `preferred_specs` below.
*******************************************************************************/

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

** Calculate statistical significance (p < 0.05)
gen significant = pval < 0.05

** =============================================================================
** DEFINE PREFERRED SPECIFICATIONS
** Modify these conditions to change which specifications are highlighted
** as "preferred" in the specification curve plots.
** =============================================================================

gen preferred = 0

** Example preferred specification criteria:
** - ACS data (full period 2016-2024)
** - Urban counties (top 5%)
** - With covariates
** - Not excluding 2020
** Modify these conditions as needed for your analysis.


** IRS FULL SAMPLE 
replace preferred = 1 if 									///
	data_type == "IRS" & 									///
	inlist(sample, "sample_all", "sample_urban95_covid") &	///
	controls == 1 &											/// 
	exclusion == 1 											//

** ACS COLLEGE SAMPLE 
replace preferred = 1 if 									///
	data_type == "ACS College" & 							///
	inlist(sample, "sample_all", "sample_urban95_covid") &	///
	controls == 1 &											/// 
	exclusion == 1 											//

** Display count of preferred specifications
dis "Number of preferred specifications: "
count if preferred == 1

** =============================================================================
** CREATE SPECIFICATION CURVE PLOTS
** =============================================================================

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

		** ---------------------------------------------------------------------
		** Create variables for significance and preferred-based coloring
		** Four categories:
		**   1. Significant + Not Preferred (navy)
		**   2. Insignificant + Not Preferred (ltblue)
		**   3. Significant + Preferred (red)
		**   4. Insignificant + Preferred (orange)
		** ---------------------------------------------------------------------

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

		** Count specifications in each category for legend
		qui count if significant == 1 & preferred == 0
		local n_sig_notpref = r(N)
		qui count if significant == 0 & preferred == 0
		local n_insig_notpref = r(N)
		qui count if significant == 1 & preferred == 1
		local n_sig_pref = r(N)
		qui count if significant == 0 & preferred == 1
		local n_insig_pref = r(N)

		** ---------------------------------------------------------------------
		** Create upper panel: Coefficient plot with CIs colored by significance
		** and preferred status
		** ---------------------------------------------------------------------
		twoway 	(rcap ci_lo_sig_notpref ci_hi_sig_notpref spec_rank, 		///
					lc(navy) lw(vthin)) 									///
				(rcap ci_lo_insig_notpref ci_hi_insig_notpref spec_rank, 	///
					lc(ltblue) lw(vthin)) 									///
				(rcap ci_lo_sig_pref ci_hi_sig_pref spec_rank, 				///
					lc(cranberry) lw(thin)) 								///
				(rcap ci_lo_insig_pref ci_hi_insig_pref spec_rank, 			///
					lc(orange) lw(thin)) 									///
				(scatter tau_sig_notpref spec_rank, 						///
					mc(navy) ms(O) msize(vsmall)) 							///
				(scatter tau_insig_notpref spec_rank, 						///
					mc(ltblue) ms(O) msize(vsmall)) 						///
				(scatter tau_sig_pref spec_rank, 							///
					mc(cranberry) ms(D) msize(small)) 						///
				(scatter tau_insig_pref spec_rank, 							///
					mc(orange) ms(D) msize(small)), 						///
			legend(order(5 "Sig. (p<0.05)" 6 "Insig." 						///
						 7 "Sig., Preferred" 8 "Insig., Preferred") 		///
				   rows(1) pos(6) size(vsmall)) 							///
			ytitle("Treatment Effect (pp)") 								///
			xtitle("") 														///
			title("`otype_label': `migr_label'", size(medium)) 				///
			yline(0, lc(red) lp(dash)) 										///
			xlabel(none) 													///
			name(coef_`otype'_`migr', replace)

		** ---------------------------------------------------------------------
		** Create lower panel: Specification indicators
		** ---------------------------------------------------------------------
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
			xtitle("Specification (ranked by effect size)")					///
			ylabel(	-1 "All Counties"										///
					-2 "Urban 95%"											///
					-3 "COVID Match"										///
					-4 "Covariates"											///
					-5 "Excl. 2020"											///
					-6 "IRS Data"											///
					-7 "ACS All"											///
					-8 "ACS College",										///
				angle(0) labsize(vsmall))									///
			xlabel(none)													///
			name(spec_`otype'_`migr', replace)

		** Combine panels
		graph combine coef_`otype'_`migr' spec_`otype'_`migr',				///
			cols(1)															///
			xcommon															///
			imargin(zero)

		** Export combined figure
		graph export "${results}sdid/fig_speccurve_`otype'_`migr'.pdf", replace
		graph export "${results}sdid/fig_speccurve_`otype'_`migr'.jpg", as(jpg) quality(100) replace

		** Clean up
		graph drop coef_`otype'_`migr' spec_`otype'_`migr'

		restore

	} // END MIGRATION LOOP
} // END OUTCOME TYPE LOOP

clear

** Repeat with ACS 


** Close log
clear 
log close log_02
