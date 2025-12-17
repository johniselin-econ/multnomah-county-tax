/*******************************************************************************
File Name: 		02_fig_distr_income_irs.do
Creator: 		John Iselin
Date Update:	November 14, 2025

Called by: 

Purpose: 

Authors: John Iselin 

For more information, contact XXXX

*******************************************************************************/


** Start log file 
capture log close log_02
log using "${logs}02_log_tab_XXX_${date}", replace text name(log_02)

** Make FIPS code from state and county fips codes 
capture program drop make_fips
program define make_fips
    syntax varlist(min=2 max=2 numeric), GEN(name)

    quietly {
        tempvar s c

        local v1 : word 1 of `varlist'
        local v2 : word 2 of `varlist'

        gen `s' = string(`v1', "%02.0f")
        gen `c' = string(`v2', "%03.0f")

        gen `gen' = real(`s' + `c')
    }
end

** Load migration statistics via the IRS SOI 
use "${data}working/irs_county_flow.dta", clear  

** Define out- and in-flows relative to Multnomah County 
gen origin_multnomah = state_fips_o == 41 & county_fips_o == 51
gen destin_multnomah = state_fips_d == 41 & county_fips_d == 51

** Keep if connected to Multnomah in some way 
keep if destin_multnomah == 1 | origin_multnomah == 1 

** Save as temporary file 
tempfile multnomah_flow
save `multnomah_flow'

** Destination 
keep if destin_multnomah == 1 

** Keep only required variables 
keep year state_*_o county_*_o n1 n2 agi

** Rename state and county variables 
rename state_*_o state_*
rename county_*_o county_* 

** Rename values to relect out-migration 
rename n1 n1_in
rename n2 n2_in 
rename agi agi_in

** Save as temporary file 
tempfile multnomah_flow_in
save `multnomah_flow_in'
clear 

** Load preserved flow data 
use `multnomah_flow'

** Origin 
keep if origin_multnomah == 1 

** Keep only required variables 
keep year state_*_d county_*_d n1* n2* agi* 

** Rename state and county variables 
rename state_*_d state_*
rename county_*_d county_* 

** Rename values to relect out-migration 
rename n1 n1_out 
rename n2 n2_out 
rename agi agi_out 

** Merge with in-migration data 
merge 1:1 year state_fips county_fips using `multnomah_flow_in', 	///
	gen(out_mig_merge)

** Fill in missing values with 0s
foreach var of varlist n1_in n1_out n2_in n2_out agi_in agi_out {
	replace `var' = 0 if missing(`var')
	
} // END VAR LOOP 

** For values with out_mig_merge == 2, copy over appropriate Multnomah totals
foreach var of varlist *_mover *_total {
	
	bysort year: egen tmp1 = mean(`var')
	replace `var' = tmp1 if missing(`var') 
	drop tmp1 

} // END VAR LOOP 

** Generate net migration 
gen n1_net = n1_in - n1_out 
gen n2_net = n2_in - n2_out 
gen agi_net = agi_in - agi_out 

** Generate plotting variables

** Loop over statistics 
foreach x in "n1" "n2" "agi" {
	
	** Labels 
	if "`x'" == "n1" local txt_x "number of returns"
	if "`x'" == "n2" local txt_x "number of exemptions"
	if "`x'" == "agi" local txt_x "AGI"

	** Loop over net, in, and out-migration 
	foreach y in "net" "in" "out" {
		
		** Labels 
		local txt_y = strproper("`y'")

		** Generate variable (Migration / Origin Total)
		gen share_`x'_`y' = `x'_`y' / `x'_total
		label var share_`x'_`y' 	///
			"`txt_y' migration (`txt_x') as a share of orign-county total"
			
		** Generate rankings
		bysort year: egen rank_`x'_`y' = rank(`x'_`y'), field 
		label var rank_`x'_`y'  	///
			"Ranking of `y' migration (`txt_x')"
		
	} // END MIGRATION TYPE LOOP 
	
} // END STATISTICS LOOP 


** Make fips codes
make_fips state_fips county_fips, gen(fips)

** Organize file 
order year fips state* county* out_mig_merge 



** Save data 
save "${data}working/data_multnomah_flows.dta", replace 
export excel using "${data}data_multnomah_flows.xlsx", 	///
	firstrow(variable) sheet(data, replace)
clear 



** Close 
*clear 
log close log_02 
