/*****************************************************************************
* File:        01f_irs_migration.do
* Purpose:     Import and clean IRS SOI county- and state-level migration data
* Called by:   01_clean_data.do
* Outputs:     data/working/irs_county_gross.dta     (county in/out/net migration)
*              data/working/irs_county_flow.dta       (county origin-destination)
*              data/working/irs_county_gross_in.dta
*              data/working/irs_county_gross_out.dta
*              data/working/irs_county_flow_YY.dta    (per-year flow files)
*              data/working/irs_county_gross_in_YY.dta
*              data/working/irs_county_gross_out_YY.dta
*              data/working/irs_state_gross.dta       (state in/out/net migration)
******************************************************************************/

//--------------------------------------------------
// STEP 5a: County-Level IRS Migration
//--------------------------------------------------

** Loop over years (extended back to 2012 for appendix data quality analysis)
forvalues y = 12(1)22 {

	local start = `y' - 1
	local end = `y'

	** Import data (out)
	import delimited "${data}irs/countyoutflow`start'`end'.csv", clear

	** Describe data
	des

	** Generate year (end year)
	gen year = 2000 + `y'

	** Drop Regional Values
	drop if y2_state == "DS"

	** Drop Foreign Migration
	drop if y2_state == "FR"

	** Drop observations without a county
	drop if y1_countyfips == 0

	** Deal with suppressed values
	unsuppress n1 n2 agi

	** Drop unnecc variables
	drop y2_state y2_countyname

	** Create two versions: gross and net
	tempfile tmp
	save `tmp'

	** Gross first

	** Keep gross categories
	** IRS aggregate pseudo-FIPS codes:
	**   y2_statefips == 96 = total migration (all categories)
	**   y2_statefips == 97 = domestic migration subtotals
	**     county == 0 = total domestic, county == 1 = within-state, county == 3 = interstate
	**   y2_statefips == 98 = foreign migration
	** Self-matches (same state+county in y1 and y2) = non-movers.
	keep if ///
		(y1_statefips == y2_statefips & y1_countyfips == y2_countyfips) |	///
		inlist(y2_statefips, 96, 97, 98)

	** Clean up
	gen move_type = 0

	** Stayers
	replace move_type = 1 if	(y1_statefips == y2_statefips) & 	///
								(y1_countyfips == y2_countyfips)

	** Movers
	replace move_type = 2 if 	y2_statefips == 96 		// ALL
	replace move_type = 3 if 	y2_statefips == 97 & 	///
								y2_countyfips == 0 		// Domestic Total
	replace move_type = 4 if 	y2_statefips == 97 & 	///
								y2_countyfips == 1 		// Within-state
	replace move_type = 5 if 	y2_statefips == 97 & 	///
								y2_countyfips == 3 		// Between-states
	replace move_type = 6 if 	y2_statefips == 98 		// Foreign

	** Label movers
	label values move_type lb_move_type

	** Generate total category
	foreach var of varlist n1 n2 agi {

		gen tmp = `var' if inlist(move_type, 1, 2)
		bysort y1_statefips y1_countyfips: egen `var'_total = total(tmp)
		drop tmp

	} // END VAR LOOP

	** Drop unnecc variables
	drop y2_*

	** Sort
	sort year y1_statefips y1_countyfips move_type

	** Order
	order year y1_statefips y1_countyfips move_type

	** Rename
	rename y1_countyfips county_fips
	rename y1_statefips state_fips

	** Label variables
	label var year "Tax year (year before move)"
	label var state_fips "State FIPS code (origin state)"
	label var county_fips "County FIPS code (origin county)"
	label var move_type "Mover category"
	label var n1 "Number of returns"
	label var n2 "Number of exemptions"
	label var agi "Adjusted Gross Income"
	label var n1_total "Number of returns, county total (origin)"
	label var n2_total "Number of exemptions, county total (origin)"
	label var agi_total "Adjusted Gross Income, county total (origin)"

	** Save
	save "${data}working/irs_county_gross_out_`y'", replace

	** Create version for merge with net
	keep if move_type == 3

	** Rename variables
	rename n1 n1_mover
	rename n2 n2_mover
	rename agi agi_mover

	label var n1_mover "Number of domestic mover returns"
	label var n2_mover "Number of domestic mover exemptions"
	label var agi_mover "Adjusted Gross Income, domestic movers"

	** Save as temp file
	tempfile merge
	save `merge'
	clear

	** Next, create flow file
	use `tmp', clear

	** Drop aggregate values
	drop if inlist(y2_statefips, 96, 97, 98)
	drop if (y1_statefips == y2_statefips & y1_countyfips == y2_countyfips)

	** Sort
	sort year y1_statefips y1_countyfips y2_statefips y2_countyfips

	** Order
	order year y1_statefips y1_countyfips y2_statefips y2_countyfips


	** Rename
	rename y1_countyfips county_fips
	rename y1_statefips state_fips
	rename y2_countyfips y2_county_fips
	rename y2_statefips y2_state_fips

	** Label variables
	label var year "Tax year (year before move)"
	label var state_fips "State FIPS code (origin state)"
	label var county_fips "County FIPS code (origin county)"
	label var y2_state_fips "State FIPS code (dest. state)"
	label var y2_county_fips "County FIPS code (dest. county)"
	label var n1 "Number of returns"
	label var n2 "Number of exemptions"
	label var agi "Adjusted Gross Income"

	** Merge with county of origin data
	merge m:1 state_fips county_fips using `merge', nogen keep(master match)

	** Rename
	rename state_fips state_fips_o
	rename county_fips county_fips_o
	rename y2_* *_d

	** Drop unnecc variable
	drop move_type

	** Save
	save "${data}working/irs_county_flow_`y'", replace
	clear

	** Import data (in)
	import delimited "${data}irs/countyinflow`start'`end'.csv", clear

	** Describe data
	des

	** Generate year
	gen year = 2000 + `y'

	** Drop Regional Values
	drop if y1_state == "DS"

	** Drop Foreign Migration
	drop if y1_state == "FR"

	** Drop observations with no county ID
	drop if y2_countyfips == 0

	** Keep gross categories
	keep if ///
		(y1_statefips == y2_statefips & y1_countyfips == y2_countyfips) |	///
		inlist(y1_statefips, 96, 97, 98)

	** Clean up
	gen move_type = 0

	** Deal with suppressed values
	unsuppress n1 n2 agi

	** Stayers
	replace move_type = 1 if	(y1_statefips == y2_statefips) & 	///
								(y1_countyfips == y2_countyfips)

	** Movers
	replace move_type = 2 if 	y1_statefips == 96 		// ALL
	replace move_type = 3 if 	y1_statefips == 97 & 	///
								y1_countyfips == 0 		// Domestic Total
	replace move_type = 4 if 	y1_statefips == 97 & 	///
								y1_countyfips == 1 		// Within-state
	replace move_type = 5 if 	y1_statefips == 97 & 	///
								y1_countyfips == 3 		// Between-states
	replace move_type = 6 if 	y1_statefips == 98 		// Foreign

	** Label move variable
	label values move_type lb_move_type

	** Generate total category
	foreach var of varlist n1 n2 agi {

		gen tmp = `var' if inlist(move_type, 1, 2)
		bysort y2_statefips y2_countyfips: egen `var'_total = total(tmp)
		drop tmp

	} // END VAR LOOP

	** Drop unnecc variables
	drop y1_*

	** Sort
	sort year y2_statefips y2_countyfips move_type

	** Order
	order year y2_statefips y2_countyfips move_type

	** Rename
	rename y2_countyfips county_fips
	rename y2_statefips state_fips

	** Label variables
	label var year "Tax year (year before move)"
	label var state_fips "State FIPS code (dest. state)"
	label var county_fips "County FIPS code (dest. county)"
	label var move_type "Mover category"
	label var n1 "Number of returns"
	label var n2 "Number of exemptions"
	label var agi "Adjusted Gross Income"
	label var n1_total "Number of returns, county total (dest.)"
	label var n2_total "Number of exemptions, county total (dest.)"
	label var agi_total "Adjusted Gross Income, county total (dest.)"

	** Save
	save "${data}working/irs_county_gross_in_`y'", replace
	clear

} // END YEAR LOOP

//--------------------------------------------------
// Append yearly files into combined datasets
//--------------------------------------------------

** Loop over datasets
foreach file in "irs_county_gross_in" "irs_county_gross_out" "irs_county_flow"{


	** Loop over years (extended back to 2012 for appendix)
	forvalues y = 12(1)22 {

		** Append
		append using "${data}working/`file'_`y'"

	} // END YEAR LOOP


	** Order and sort flow file
	if "`file'" == "irs_county_flow" {

		** Loop over origin and destination state
		foreach x in "o" "d" {

			** Rename
			rename *_fips_`x' *_fips

			** Merge with county and state names
			merge m:1 state_fips county_fips using "${data}working/ids", 	///
				keep(match) nogen

			** Rename
			rename *_fips *_fips_`x'
			rename *_name *_name_`x'

			** Generate county IDS
			make_fips state_fips_`x' county_fips_`x', gen(fips_`x')

		} // END ORIGIN / DESTINATION LOOP

		** Order file
		order year state_*_o county_*_o state_*_d county_*_d
		sort year state_*_o county_*_o state_*_d county_*_d

	} // END MIGRATION FLOW IF-STATEMENT

	else {

		** Merge with county and state names
		merge m:1 state_fips county_fips using "${data}working/ids", 	///
				keep(match) nogen

		** Order
		order year state_* county* move_type

	}

	** Save file
	save "${data}working/`file'", replace
	clear

} // END FILE LOOP

//--------------------------------------------------
// Build combined gross file (in + out + net)
//--------------------------------------------------

** Create gross file with in and out migration
use "${data}working/irs_county_gross_in", clear

** Rename
rename n1 n1_in_
rename n2 n2_in_
rename agi agi_in_
rename *_total *_total_in

** Reshape
reshape wide n1_in_ n2_in_ agi_in_, i(year state_fips county_fips) j(move_type)

** Define locals
local txt1 "Non-movers"
local txt2 "All movers"
local txt3 "Domestic movers"
local txt4 "Within-state movers"
local txt5 "Inter-state movers"
local txt6 "Foreign movers"

** Label variables, in loop
foreach n of numlist 1(1)6 {

	label var n1_in_`n' "Returns, in-migration, `txt`n''"
	label var n2_in_`n' "Exemptions, in-migration, `txt`n''"
	label var agi_in_`n' "AGI, in-migration, `txt`n''"

} // END NUMLIST LOOP

** Preserve
tempfile gross_in
save `gross_in'
clear

** Create gross file with in and out migration
use "${data}working/irs_county_gross_out", clear

** Rename
rename n1 n1_out_
rename n2 n2_out_
rename agi agi_out_
rename *_total *_total_out

** Reshape
reshape wide n1_out_ n2_out_ agi_out_, i(year state_fips county_fips) j(move_type)

** Define locals
local txt1 "Non-movers"
local txt2 "All movers"
local txt3 "Domestic movers"
local txt4 "Within-state movers"
local txt5 "Inter-state movers"
local txt6 "Foreign movers"

** Label variables, in loop
foreach n of numlist 1(1)6 {

	label var n1_out_`n' "Returns, out-migration, `txt`n''"
	label var n2_out_`n' "Exemptions, out-migration, `txt`n''"
	label var agi_out_`n' "AGI, out-migration, `txt`n''"

} // END NUMLIST LOOP

** Merge data
merge 1:1 year state_fips county_fips using `gross_in', keep(match) nogen

** Text for correct matching (non-movers should match perfectly)
summ n*_*_1 agi_*_1

** Define net migration variables

** Loop over variable
foreach a in "n1" "n2" "agi" {

	if "`a'" == "n1" local txt "Returns"
	else if "`a'" == "n2" local txt "Exemptions"
	else if "`a'" == "agi" local txt "AGI"

	** Loop over type of movers
	forvalues n = 2/6 {

		** Clean up missing values
		replace `a'_in_`n' = 0 if missing(`a'_in_`n')
		replace `a'_out_`n' = 0 if missing(`a'_out_`n')

		** Generate net
		gen `a'_net_`n' = `a'_in_`n' - `a'_out_`n'
		label var `a'_net_`n' "`txt', net-migration, `txt`n''"

	} // END MOVER TYPE LOOP

} // END VARIABLE LOOP

** Generate fips variable
*make_fips state_fips county_fips, gen(fips)

** Save file
save "${data}working/irs_county_gross", replace
clear

//--------------------------------------------------
// STEP 5b: State-Level IRS Migration
//--------------------------------------------------

** Loop over years (extended back to 2012 for appendix)
forvalues y = 12(1)22 {

	local start = `y' - 1
	local end = `y'

	** ---- State Outflow ----
	import delimited "${data}irs/stateoutflow`start'`end'.csv", clear

	** Generate year
	gen year = 2000 + `y'

	** Drop summary rows (same state, foreign, regions)
	drop if y2_state == "DS"
	drop if y2_state == "FR"

	** Deal with suppressed values
	unsuppress n1 n2 agi

	** Keep gross categories (stayers + aggregate mover rows)
	keep if ///
		(y1_statefips == y2_statefips & y1_statefips != 96 & y1_statefips != 97 & y1_statefips != 98) | ///
		inlist(y2_statefips, 96, 97, 98)

	** Create move_type
	gen move_type = 0

	** Stayers (same state)
	replace move_type = 1 if (y1_statefips == y2_statefips)

	** Movers
	replace move_type = 2 if y2_statefips == 96		// All movers
	** State-level files use "Total Migration-US" or "Total Migration US"
	** (hyphen variation across file vintages), so we match with strpos()
	** rather than an exact string comparison.
	replace move_type = 3 if y2_statefips == 97 & 	///
		(strpos(y2_state_name, "Total Migration-US") | ///
		 strpos(y2_state_name, "Total Migration US"))	// Domestic total
	replace move_type = 4 if y2_statefips == 97 & 	///
		(strpos(y2_state_name, "Total Migration-Same State") | ///
		 strpos(y2_state_name, "Total Migration Same State"))	// Within-state
	replace move_type = 6 if y2_statefips == 98		// Foreign

	label values move_type lb_move_type

	** Drop destination columns
	drop y2_*

	** Rename
	rename y1_statefips state_fips

	** Keep required variables
	keep year state_fips move_type n1 n2 agi

	** Sort
	sort year state_fips move_type

	** Save
	tempfile state_out_`y'
	save `state_out_`y''
	clear

	** ---- State Inflow ----
	import delimited "${data}irs/stateinflow`start'`end'.csv", clear

	** Generate year
	gen year = 2000 + `y'

	** Drop summary rows
	drop if y1_state == "DS"
	drop if y1_state == "FR"

	** Deal with suppressed values
	unsuppress n1 n2 agi

	** Keep gross categories
	keep if ///
		(y1_statefips == y2_statefips & y2_statefips != 96 & y2_statefips != 97 & y2_statefips != 98) | ///
		inlist(y1_statefips, 96, 97, 98)

	** Create move_type
	gen move_type = 0

	** Stayers
	replace move_type = 1 if (y1_statefips == y2_statefips)

	** Movers
	replace move_type = 2 if y1_statefips == 96		// All movers
	replace move_type = 3 if y1_statefips == 97 & 	///
		(strpos(y1_state_name, "Total Migration-US") | ///
		 strpos(y1_state_name, "Total Migration US"))	// Domestic total
	replace move_type = 4 if y1_statefips == 97 & 	///
		(strpos(y1_state_name, "Total Migration-Same State") | ///
		 strpos(y1_state_name, "Total Migration Same State"))	// Within-state
	replace move_type = 6 if y1_statefips == 98		// Foreign

	label values move_type lb_move_type

	** Drop origin columns
	drop y1_*

	** Rename
	rename y2_statefips state_fips

	** Keep required variables
	keep year state_fips move_type n1 n2 agi

	** Sort
	sort year state_fips move_type

	** Save
	tempfile state_in_`y'
	save `state_in_`y''
	clear

} // END YEAR LOOP

** Append outflow files
clear
forvalues y = 12(1)22 {
	append using `state_out_`y''
}

** Reshape wide by move_type
rename n1 n1_out_
rename n2 n2_out_
rename agi agi_out_
reshape wide n1_out_ n2_out_ agi_out_, i(year state_fips) j(move_type)

** Save outflow
tempfile state_gross_out
save `state_gross_out'
clear

** Append inflow files
forvalues y = 12(1)22 {
	append using `state_in_`y''
}

** Reshape wide by move_type
rename n1 n1_in_
rename n2 n2_in_
rename agi agi_in_
reshape wide n1_in_ n2_in_ agi_in_, i(year state_fips) j(move_type)

** Merge inflow and outflow
merge 1:1 year state_fips using `state_gross_out', keep(match) nogen

** Generate net migration variables
foreach a in "n1" "n2" "agi" {

	if "`a'" == "n1" local txt "Returns"
	else if "`a'" == "n2" local txt "Exemptions"
	else if "`a'" == "agi" local txt "AGI"

	forvalues n = 2/3 {

		** Clean up missing values
		replace `a'_in_`n' = 0 if missing(`a'_in_`n')
		replace `a'_out_`n' = 0 if missing(`a'_out_`n')

		** Generate net
		gen `a'_net_`n' = `a'_in_`n' - `a'_out_`n'
		label var `a'_net_`n' "`txt', net-migration, type `n'"

	} // END MOVER TYPE LOOP

} // END VARIABLE LOOP

** Merge state names
merge m:1 state_fips using "${data}working/state_ids", keep(master match) nogen

** Label variables
label var year "Tax year"
label var state_fips "State FIPS code"
label var n1_out_1 "Returns, non-movers (out file)"
label var n1_out_2 "Returns, all movers (out file)"
label var n1_out_3 "Returns, domestic movers (out file)"
label var n1_in_1 "Returns, non-movers (in file)"
label var n1_in_2 "Returns, all movers (in file)"
label var n1_in_3 "Returns, domestic movers (in file)"

** Order
order year state_fips state_name
sort state_fips year

** Save
save "${data}working/irs_state_gross", replace
clear
