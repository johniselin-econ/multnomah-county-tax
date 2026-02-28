/*****************************************************************************
* File:        01g_irs_agi.do
* Purpose:     Import and clean IRS SOI county-level aggregate data (AGI by bracket)
* Called by:   01_clean_data.do
* Outputs:     data/working/irs_county_all.dta
******************************************************************************/

** Loop over years (extended back to 2011 for appendix)
forvalues y = 11(1)22 {

	** 2012 uses a different filename convention
	if `y' == 12 {
		local fn_agi "12cyallagi.csv"
	}
	else {
		local fn_agi "`y'incyallagi.csv"
	}

	** Import data
	import delimited "${data}irs/`fn_agi'", clear

	** Describe data
	des

	** Generate year
	gen year = 2000 + `y'

	** Define AGI groups
	label var agi_stub "AGI Brackets"
	label values agi_stub lb_agi

	** Create missing columns for years where they don't exist
	** (2011 lacks mars1/mars4; 2011-12 lack n02650/a02650/elderly)
	foreach v in mars1 mars4 elderly n02650 a02650 {
		capture confirm variable `v'
		if _rc {
			gen `v' = .
		}
	}

	** Define set of variables to keep
	keep state* county* agi_stub year n1 mars1 mars2 mars4 n2 elderly ///
		a00100 n02650 a02650 n00200 a00200

	** Rename variables
	rename a00100 agi
	rename n02650 n_total_inc
	rename a02650 a_total_inc
	rename n00200 n_wage
	rename a00200 a_wage
	rename statefips state_fips
	rename state state_abb
	rename countyfips county_fips
	rename countyname county_name

	** Rescale
	replace agi = 1000 * agi
	replace a_total_inc = 1000 * a_total_inc if !missing(a_total_inc)
	replace a_wage = 1000 * a_wage

	** Label
	label var n1 "Number of returns"
	label var mars1 "Number of single returns"
	label var mars2 "Number of MFJ returns"
	label var mars4 "Number of HoH returns"
	label var n2 "Number of individuals"
	label var elderly "Number of returns with one individual over 60"
	label var agi "Adjusted Gross Income (AGI)"
	label var n_total_inc "Number of returns with total income"
	label var a_total_inc "Total income amount"
	label var n_wage "Number of returns with wage income"
	label var a_wage "Wage income amount"

	** Sort
	sort year state_fips county_fips agi_stub

	** Order
	order year state* county* agi_stub

	** Save
	save "${data}working/irs_county_all_`y'", replace

	clear

} // END YEAR LOOP

** Append data

** Loop over years (extended back to 2011 for appendix)
forvalues y = 11(1)22 {

	** Append
	append using "${data}working/irs_county_all_`y'"

	} // END YEAR LOOP

** Save file
save "${data}working/irs_county_all", replace

** Generate fips variable
make_fips state_fips county_fips, gen(fips)

** Save file
save "${data}working/irs_county_all", replace
clear
