/*******************************************************************************
File Name: 		02_sdid_analysis.do
Creator: 		John Iselin
Date Update:	November 26, 2025

Called by: 00_multnomah.do

Purpose: Preform synthetic difference-in-difference estiamtion. 

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
keep year fips state* county* *_net_3 *_out_1 *_out_2 
order year fips state* county* 

** Merge with ACS Data 
merge 1:1 year fips using "${data}working/acs_county_gross_18plus", gen(merge_acs_1)

** Keep require variables 
keep year fips state* county* *_net_3 *_out_1 *_out_2 merge_acs_*

** Label acs samples 
rename persons_* acs1_persons_* 
rename households_* acs1_households_*
rename dollars_* acs1_dollars_*

** Merge with ACS Data 
merge 1:1 year fips using "${data}working/acs_county_gross_college", gen(merge_acs_2)

** Keep require variables 
keep year fips state* county* *_net_3 *_out_1 *_out_2 merge_acs_*

** Label acs samples 
rename persons_* acs2_persons_*
rename households_* acs2_households_*
rename dollars_* acs2_dollars_*

** Merge with ACS Data 
merge 1:1 year fips using "${data}working/acs_county_gross_nocollege", gen(merge_acs_3)

** Keep require variables 
keep year fips state* county* *_net_3 *_out_1 *_out_2 merge_acs_*

** Label acs samples 
rename persons_* acs3_persons_*
rename households_* acs3_households_*
rename dollars_* acs3_dollars_*

** TODO add High and Low Education Samples 

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
gen irs_sample = inrange(year, 2016, 2022)

** Generate ACS Period Indicators 
gen acs_period_1 = merge_acs_1 != 1 & inrange(year, 2016, 2022)
gen acs_period_2 = merge_acs_1 != 1 

** Make sure we have a balanced panel of ACS counties 
gen tmp = merge_acs_1 != 1 
bysort fips: egen ct_tmp = total(tmp)
replace acs_period_1 = 0 if ct_tmp != 9 
replace acs_period_2 = 0 if ct_tmp != 9 
drop tmp ct_tmp

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
graph export "${results}fig_kmeans.jpg", as(jpg) name("Graph") quality(100) replace 		
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
	
	gen `x'_rate_irs = 100 * (`x'_net_3 / (`x'_out_1 + `x'_out_2))
	
} // END OUTCOME TYPE LOOP 

** Label var 
label var n1_rate_irs	"Net domestic migration rate, returns (%)"
label var n2_rate_irs 	"Net domestic migration rate, exemptions (%)"
label var agi_rate_irs 	"Net domestic migration rate, AGI (%)"


** Define outcome variables (ACS)
** Loop over three datasets (18+ = 1, college degree == 2, no college degree == 3)
forvalues i = 1/3{
	
	if `i' == 1 local txt "(18+)"
	else if `i' == 2 local txt "(College)"
	else if `i' == 3 local txt "(No College)"
	
	gen n1_rate_acs`i' = 100 * (acs`i'_households_net_3 / (acs`i'_households_out_1 + acs`i'_households_out_2))
	gen n2_rate_acs`i' = 100 * (acs`i'_persons_net_3 / (acs`i'_persons_out_1 + acs`i'_persons_out_2))
	gen agi_rate_acs`i' = 100 * (acs`i'_dollars_net_3 / (acs`i'_dollars_out_1 + acs`i'_dollars_out_2))
	
	label var n1_rate_acs`i' 	"Net domestic migration rate `txt', HHs (%)"
	label var n2_rate_acs`i' 	"Net domestic migration rate `txt', persons (%)"
	label var agi_rate_acs`i' 	"Net domestic migration rate `txt', total income (%)"
	
} // END DATA LOOP 

** Generate triple-difference vars 
gen n1_rate_acs4 = n1_rate_acs2 - n1_rate_acs3
gen n2_rate_acs4 = n2_rate_acs2 - n2_rate_acs3
gen agi_rate_acs4 = agi_rate_acs2 - agi_rate_acs3

label var n1_rate_acs4 		"Net domestic migration rate (College less No College), HHs (%)"
label var n2_rate_acs4 		"Net domestic migration rate (College less No College), persons (%)"
label var agi_rate_acs4 	"Net domestic migration rate (College less No College), total income (%)"

** Declare panel
xtset fips year 

** Tag unique fips 
egen unique = tag(fips)
tab year unique

** Loop over IRS and ACS Samples  
foreach data of varlist acs_period_1 acs_period_2 irs_sample {

	** Different sets of outcome variables 
	if "`data'" == "irs_sample" local out_type "irs"
	else local out_type "acs1 acs2 acs3 acs4"

	** Loop over Outcome var type 
	foreach type of local out_type {
		
		** Labels 
		if "`data'" == "irs_sample" local out_txt "irs_16_22"
		else if "`data'" == "acs_period_1" & "`type'" == "acs1" local out_txt "acs_16_22_all"
		else if "`data'" == "acs_period_1" & "`type'" == "acs2" local out_txt "acs_16_22_col"
		else if "`data'" == "acs_period_1" & "`type'" == "acs3" local out_txt "acs_16_22_noc"
		else if "`data'" == "acs_period_1" & "`type'" == "acs4" local out_txt "acs_16_22_3D"
		else if "`data'" == "acs_period_2" & "`type'" == "acs1" local out_txt "acs_16_24_all"
		else if "`data'" == "acs_period_2" & "`type'" == "acs2" local out_txt "acs_16_24_col"
		else if "`data'" == "acs_period_2" & "`type'" == "acs3" local out_txt "acs_16_24_noc"
		else if "`data'" == "acs_period_2" & "`type'" == "acs4" local out_txt "acs_16_24_3D"

		** Loop over samples 
		foreach samp of varlist sample_all sample_urban95 sample_urban95_covid {	
			
			** Loop over exclusion of 2019-2020 period 
			forvalues exl = 1(-1)0 {
				
				** Define sample 	
				gen sample = `samp' == 1 & `data' == 1 
				if `exl' == 1 replace sample = 0 if year == 2020 
				
				** Clear stored values 
				eststo clear 		
				
				** Loop over outcomes 
				foreach out of varlist n1_rate_`type' n2_rate_`type' agi_rate_`type' {
				
					** Store label 
					local label : variable label `out'
					
					** Loop over inclusion of covariates
					forvalues c = 0/1 {
					
						** Covariates 
						if `c' == 0 local covars ""
						else if `c' == 1 local covars "covariates(`covariates', projected)"
						
						** File Name 
						if `exl' == 0 local path "${results}fig_`out_txt'_`out'_`c'_`samp'_"
						if `exl' == 1 local path "${results}fig_`out_txt'_`out'_`c'_`samp'_excl2020_"
	
						
						** Run SDID
						eststo sdid_`out'_`c': sdid `out' fips year Treated	///
							if sample == 1,			 	///
							vce(placebo) 				///
							`covar'						///
							reps(`reps')				///
							graph graph_export("`path'", .pdf) 
							
						** Estadd counties  
						qui summ `out' if year == 2021 & sample == 1
						estadd scalar count = r(N)	
							
						** Estadd mean 
						qui summ `out' if multnomah == 1 & Treated == 0 
						estadd scalar mean = r(mean)

						** Run event-study 
						sdid_event `out' fips year Treated			///
							if sample == 1,			 			///
							`covar'								///
							vce(placebo) 						///
							brep(`reps') 						///
							placebo(all)
						
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
			
						** Move Matrix results to data 
						svmat res
						
						** Generate ID variable
						gen id = `max_yr' - _n + 1 if !missing(res1)
						
						** Update labeling for exclusion of 
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

						if `exl' == 0 local path "${results}fig_`out_txt'_`out'_`c'_`samp'_eventstudy.jpg"
						if `exl' == 1 local path "${results}fig_`out_txt'_`out'_`c'_`samp'_excl2020_eventstudy.jpg"
	
							
						graph export "`path'", 	///
							as(jpg) name("Graph") quality(100) replace 		

						** Clean up 
						drop res1 res2 res3 res4 res5 id 
						if `exl' == 1 {
							drop if tag == 1
							drop tag 
						}
						** Update Count
						local ct = `ct' + 1 
										
					} // END COVAR LOOP 
				
				} // END OUTCOME LOOP 
				
				** Determine name 
				if `exl' == 0 local path "${results}tab_sdid_`out_txt'_`samp'.tex"
				if `exl' == 1 local path "${results}tab_sdid_`out_txt'_`samp'_excl2020.tex"

				** Table of results 
				esttab 	sdid_n1_rate_`type'_0 sdid_n1_rate_`type'_1			///
						sdid_n2_rate_`type'_0 sdid_n2_rate_`type'_1			///
						sdid_agi_rate_`type'_0 sdid_agi_rate_`type'_1 using	///
				"`path'",										///
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
				
				** Drop sample var 
				drop sample 
				
			} // END EXCLUSION LOOP 
			
		} // END SAMPLE LOOP 
	
	} // END OUT TYPE 
	
} // END DATA LOOP 

clear

** Repeat with ACS 


** Close log
clear 
log close log_02
