/*****************************************************************************
* File:        01e_acs.do
* Purpose:     Load and clean ACS microdata; build migration flow and
*              gross-migration datasets
* Called by:   01_clean_data.do
* Outputs:     data/working/acs_migration_file.dta    (person-level microdata)
*              data/working/acs_county_flow.dta        (origin-destination flows)
*              data/working/acs_county_gross_25plus.dta
*              data/working/acs_county_gross_college.dta
*              data/working/acs_county_gross_nokids.dta
******************************************************************************/

//--------------------------------------------------
// Load and append ACS yearly CSVs
//--------------------------------------------------

** Load data
forvalues y = $start_year_acs(1)$end_year_acs {

	** Import CSV
	import delimited using "${data}acs/acs_`y'", varnames(1) clear case(lower)

	** Save as temporary data
	tempfile acs_`y'
	save `acs_`y''
	clear

} // END YEAR LOOP

** Append data
forvalues y = $start_year_acs(1)$end_year_acs {

	append using `acs_`y''

} // END YEAR LOOP

** Des
tab year

//--------------------------------------------------
// Household composition variables
//--------------------------------------------------

** Define # of adults and kids in HHs
gen adult = age >= 18
gen child = age < 18
bysort year serial: gen hh_size = _N
bysort year serial: egen hh_adult_ct = total(adult)
bysort year serial: egen hh_child_ct = total(child)

** Define HH-wide indicators for creation of gross migration files
gen college = educd >= 101
gen hh_any_child = hh_child_ct > 0
bysort year serial: egen hh_any_college = max(college)

** Define weighted # of people
bysort year serial: egen hh_perwt = total(perwt)

** Sample 18+
drop if child == 1
drop child adult

//--------------------------------------------------
// Drop foreign movers
//--------------------------------------------------
** migplac1 > 56 indicates a non-US origin (foreign countries, territories,
** or "abroad not specified"). migrate1 == 4 means "moved from abroad."
** Both are dropped because the analysis focuses on domestic migration only.
drop if migplac1 > 56
drop if migrate1 == 4

//--------------------------------------------------
// Set up origin/destination variables
//--------------------------------------------------

** Rename variables
rename statefip state_fips_d
rename countyfip county_fips_d

** Set up origin data
fre migrate1
drop migrate1d
tab migplac1
rename migplac1 state_fips_o
tab migcounty1
rename migcounty1 county_fips_o

** Use migrate1 to assign origin counties:
** Non-movers (migrate1 == 1): origin = destination (stayed in same house),
** so we copy the destination county/state to the origin fields.
** Same-state movers (migrate1 == 2): state of origin = state of destination
** (migplac1 may be missing for within-state moves in some ACS years).
** Interstate movers (migrate1 == 3): origin state/county come from
** migplac1/migcounty1, which are already assigned above.

** Within same house
replace state_fips_o = state_fips_d if migrate1 == 1
replace county_fips_o = county_fips_d if migrate1 == 1

** Within same state
replace state_fips_o = state_fips_d if migrate1 == 2

** Generate county IDS
foreach x in "o" "d" {

	make_fips state_fips_`x' county_fips_`x', gen(fips_`x')

}

** Check for within-state migration
gen same_county = fips_o == fips_d
tab year same_county
tab year same_county if migrate1 == 2

** Tag HH head
gen byte hh_head = (relate == 1)

** Compress file
compress

** Save
save "${data}working/acs_migration_file", replace

//--------------------------------------------------
// Build county-level flow file
//--------------------------------------------------

** Keep only observations with valid origin/destination counties and YEAR
drop if missing(year) | missing(fips_o) | missing(fips_d)

** Clean income (treat missing as 0; keep negative values as reported)
replace inctot = 0 if missing(inctot)
gen double income_wt = inctot * perwt
label var income_wt "Person income (INCTOT) weighted by PERWT"

** Persons + income totals by origin/destination/year

preserve
keep year fips_o fips_d perwt income_wt
collapse (sum) persons=perwt income_total=income_wt, by(year fips_o fips_d)
tempfile acs_pi
save `acs_pi'
restore

** Households by origin/destination/year
** Use HHWT among household heads (RELATE==1)

preserve
keep if hh_head == 1
keep year fips_o fips_d hhwt
collapse (sum) households=hhwt, by(year fips_o fips_d)
tempfile acs_hh
save `acs_hh'
restore

** Merge persons/income with households
use `acs_pi', clear
merge 1:1 year fips_o fips_d using `acs_hh', nogen

label var persons "Estimated number of persons (sum PERWT)"
label var households "Estimated number of households (sum HHWT among heads)"
label var income_total "Estimated total personal income (sum INCTOT*PERWT)"

** Derive state/county components for merges with name crosswalk
gen int state_fips_o  = floor(fips_o/1000)
gen int county_fips_o = mod(fips_o,1000)
gen int state_fips_d  = floor(fips_d/1000)
gen int county_fips_d = mod(fips_d,1000)

label var state_fips_o "State FIPS (origin)"
label var county_fips_o "County FIPS (origin)"
label var state_fips_d "State FIPS (destination)"
label var county_fips_d "County FIPS (destination)"

** Merge in names (from NHGIS IDs snapshot)

** Loop over origin and destination states
foreach x in "d" "o" {

	preserve

	** Load County IDs
	use "${data}working/ids", clear

	** Rename
	rename 	(state_fips county_fips state_name county_name)	///
			(state_fips_`x' county_fips_`x' state_name_`x' county_name_`x')

	** Save as temporary file
	tempfile ids_`x'
	save `ids_`x''

	** Restore and merge data
	restore
	merge m:1 state_fips_`x' county_fips_`x' using `ids_`x'', keep(master match) nogen

} // END ORIGIN - DESTINATION LOOP

** Organize data
order year ///
    state_fips_o county_fips_o state_name_o county_name_o fips_o ///
    state_fips_d county_fips_d state_name_d county_name_d fips_d ///
    persons households income_total

** Sort data
sort year state_fips_o county_fips_o state_fips_d county_fips_d

** Identify other counties (suppression)
replace county_name_o = "Other" if county_fips_o == 0
replace county_name_d = "Other" if county_fips_d == 0

** Save data
save "${data}working/acs_county_flow", replace
clear

//--------------------------------------------------
// Build gross-migration files (using program from 01a)
//--------------------------------------------------

** All (25+)
acs_make_gross_migration using "${data}working/acs_migration_file", ///
    saving("${data}working/acs_county_gross_25plus") replace sample("age >= 25")

** College-degrees
acs_make_gross_migration  using "${data}working/acs_migration_file", ///
    saving("${data}working/acs_county_gross_college") replace sample("hh_any_college == 1 & age >= 25")

** No Kids
acs_make_gross_migration using "${data}working/acs_migration_file", ///
    saving("${data}working/acs_county_gross_nokids") replace sample("hh_any_child == 0 & age >= 25")
