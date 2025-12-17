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

** Define treated state 
gen multnomah = state_fips == 41 & county_fips == 51
label var multnomah "Indicator for Multnomah County, Oregon"	
	
** Define treatment indicator 
gen Treated = multnomah == 1 & year >= 2020
label var Treated "Treatment indicator for Multnomah County, Oregon"	


** Organize data 
order year fips state_* county_* 
sort fips year  
isid fips year 

** Keep only sample with non-missing base populations
tab year county_name if missing(n1_in_1 ) | n1_in_1 == 0
drop if missing(n1_in_1 ) | n1_in_1 == 0

** Keep only sample with observations in each year 
bysort fips: gen ct = _N
tab county_name state_name  if ct != 7 
drop if ct != 7 
drop ct 

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

** Define Sample 	
drop if state_name == "Alaska"
drop if state_name == "Hawaii"
drop if state_name == "California"
drop if state_name == "Washington"
drop if state_name == "Oregon" & multnomah == 0

** Define covariates 
local covariates "population per_capita_income"

** Standardize covariates 
foreach v of local covariates {
	egen tmp_v = std(`v') 
	replace `v' = tmp_v
	drop tmp_v
} // END COVAR LOOP 

** Define outcome variables 
foreach x in "n1" "n2" "agi" {
	gen `x'_rate = 100 * (`x'_net_3 / (`x'_out_1 + `x'_out_2))
} // END OUTCOME TYPE LOOP 

** Label var 
label var n1_rate "Net domestic migration rate, returns (%)"
label var n2_rate "Net domestic migration rate, exemptions (%)"
label var agi_rate "Net domestic migration rate, AGI (%)"

** Declare panel
xtset fips year 

** Tag unique fips 
egen unique = tag(fips)
tab year unique

** Loop over samples 
foreach samp of varlist sample_all sample_urban95 sample_urban98 {	
		
	** Clear stored values 
	eststo clear 		
		
	** Loop over outcomes 
	foreach out of varlist n1_rate n2_rate agi_rate {
		
		** Store label 
		local label : variable label `out'
			
		** Loop over inclusion of covariates
		forvalues c = 0/1 {
			
			if `c' == 0 local covars ""
			else if `c' == 1 local covars "covariates(`covariates', projected)"
			dis "`covars'"
			
			** Run SDID
			eststo sdid_`out'_`c': sdid `out' fips year Treated	///
				if `samp' == 1,			 	///
				vce(placebo) 				///
				`covar'						///
				reps(`reps')				///
				graph graph_export("${results}fig_`out'_`c'_`samp'_", .pdf) 
				
			** Estadd counties  
			qui summ `out' if year == 2020 & `samp' == 1
			estadd scalar count = r(N)	
				
			** Estadd mean 
			qui summ `out' if multnomah == 1 & Treated == 0 
			estadd scalar mean = r(mean)

			** Run event-study 
			sdid_event `out' fips year Treated			///
				if `samp' == 1,			 			///
				`covar'								///
				vce(placebo) 						///
				brep(`reps') 						///
				placebo(all)
			
			** Create Figure 
			
			** Move results from matrix to data 
			matrix list e(H)
			mat res_att_`ct' = e(H)[1,1..4]
			mat res = e(H)[2..8,1..5]
			
			** Move Matrix results to data 
			svmat res
			
			** Generate ID variable
			gen id = 2021 - _n + 1 if !missing(res1)
			label var id "Tax year (origin)"
			
			** Sort 
			sort id
			
			** Plot
			twoway 	(rcap res3 res4 id, lc(gs10) fc(gs11%50))	///
					(scatter res1 id, mc(black)),				///		
				legend(off) ytitle("`label'") 					///
				yline(0, lc(red) lp(-)) 						///
				xline(2019.5, lc(black) lp(solid))				///
				ylabel(-10(2.5)10, format(%9.1f))

			graph export "${results}fig_`out'_`c'_`samp'_eventstudy.jpg", 	///
				as(jpg) name("Graph") quality(100) replace 		

			** Clean up 
			drop res1 res2 res3 res4 res5 id 

			** Update Count
			local ct = `ct' + 1 
							
		} // END COVAR LOOP 
	
	} // END OUTCOME LOOP 
	
	
	** Table of results 
	esttab 	sdid_n1_rate_0 sdid_n1_rate_1			///
			sdid_n2_rate_0 sdid_n2_rate_1			///
			sdid_agi_rate_0 sdid_agi_rate_1 using	///
	"${results}tab_sdid_irs_`samp'.tex",			///
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
	
			
} // END SAMPLE LOOP 
	

** Close log
clear 
log close log_02
