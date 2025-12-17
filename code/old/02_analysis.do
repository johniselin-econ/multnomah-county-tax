/*****************************************************************************
* Program: 		02_analysis.do 
* Author(s): 	John Iselin 
* Date: 		October 27, 2025
* Description: 	Evaluate Migration Effect



*****************************************************************************/

** Preliminaries
clear 

** Save date as a string
local date = string( d(`c(current_date)'), "%dCY-N-D" )

** Log file
capture log close log_02
log using "${logs}02_analysis_`date'.log", replace name(log_02)

** Load data 
use "${data}working/irs_county_gross", replace 

** Merge with Demographic data 
merge m:1 fips using "${data}working/demographics_2020", 	///
	gen(demo_merge) keep(master match) 

** Keep 48 contiguous states + DC
drop if state_name == "Alaska"
drop if state_name == "Hawaii"

** Organize data 
order year fips state_* county_* 
sort fips year  
isid fips year 

** Keep only sample with non-missing base populations
tab year county_name  if missing(n1_in_1 ) | n1_in_1 == 0
drop if missing(n1_in_1 ) | n1_in_1 == 0

** Keep only sample with observations in each year 
bysort fips: gen ct = _N
tab county_name state_name  if ct != 7 
drop if ct != 7 
drop ct 

** Tag treated state
gen multnomah = state_fips == 41 & county_fips == 51
label var multnomah "Multnomah, OR"

** Identify samples 
gen sample_nonwest = !inlist(state_fips, 6, 41, 53) | multnomah == 1 
gen sample_kmeans = (sample_nonwest == 1 & group_kmeans == 19) | multnomah == 1 

tab group_kmeans multnomah  

** Define treatment 
gen treated = multnomah == 1 & year >= 2020

** Define treatment variables 
gen n1_rate_3 = n1_net_3 / (n1_out_1 + n1_out_2)
gen agi_rate_3 = agi_net_3 / (agi_out_1 + agi_out_2)

** Label var 
label var n1_rate_3 "Net domestic migration rate, returns"
label var agi_rate_3 "Net domestic migration rate, AGI"

** Declare panel
xtset fips year 

** Loop over outcomes 
foreach out of varlist n1_rate_3 agi_rate_3 {
		
	** Store label 
	local label : variable label `out'
		
	** Create empty matrices to store results 
	matrix table_`out' = J(2, 2, .)

	** Rownames 
	matrix rownames table_`out' = ATT SE
				
	** Start count
	local ct = 1
				
	** Loop over samples
	foreach sample of varlist sample_nonwest sample_kmeans {
			
		** Run event-study 
			 
		** Create Figure 
		sdid_event `out' fips year treated if `sample' == 1, 	///
			vce(placebo) brep(100) placebo(all)
		
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
			xline(2019.5, lc(black) lp(solid))

		graph export "${results}fig_irs_`out'_`sample'.jpg", 	///
			as(jpg) name("Graph") quality(100) replace 		

		
		** Clean up 
		drop res1 res2 res3 res4 res5 id 

		** Update Count
		local ct = `ct' + 1 
						
	} // END SAMPLE LOOP 
	
			
} // END OUTCOME LOOP 
	
clear 

svmat res_att_1
gen out = "Returns"
gen sample = "Non-westcoast"
svmat res_att_2
replace out = "Returns" if missing(out)
replace sample = "Kmeans" if missing(sample)
svmat res_att_3
replace out = "AGI" if missing(out)
replace sample = "Non-westcoast" if missing(sample)	
svmat res_att_4
replace out = "AGI" if missing(out)
replace sample = "Kmeans" if missing(sample)

clear 

** Load ACS data 
use "${data}working/acs_migration_file", clear 
 
** Tag treated state
gen multnomah = state_fips_o == 41 & county_fips_d == 51
label var multnomah "Multnomah, OR"

** Close log
log close log_02
