/*****************************************************************************
* Program: 			01_clean_data.do 
* Author(s): 		John Iselin 
* Date Updated:		December 12, 2025 (added automated downloads for IRS & BEA)

*** Demographic data via IPUMS NHGIS 
** Via https://www.nhgis.org/
** Downloaded on October 19, 2025
** EXTRACT DETAILS in "nhgis0029_ts_nominal_county_codebook"

*** Economic data via BEA Regional Economic Accounts (CAINC1)
** Via https://apps.bea.gov/regional/downloadzip.htm
** Downloaded on November 27, 2025

*** ACS individual data via IPUMS USA 
** Via https://usa.ipums.org/usa/index.shtml
** Downloaded on October 19, 2025
** EXTRACT DETAILS in "usa_00152"

*** IRS SOI County-Level Migration Files
** Via https://www.irs.gov/statistics/soi-tax-stats-migration-data
** Downloaded on October 19, 2025

*** IRS SOI County-Level Files
** Via https://www.irs.gov/statistics/soi-tax-stats-county-data
** Downloaded on November 13, 2025


*******************************************************************************/

** Start log file 
capture log close log_01
log using "${logs}01_log_data_clean_${pr_name}_${date}", replace text name(log_01)

//--------------------------------------------------
// STEP -1: Acquire raw data (automated where possible)
//--------------------------------------------------
/*
This block downloads public-use source data directly from official URLs if not present locally.

Automated downloads included:
  - IRS SOI county-to-county migration: countyinflowYYZZ.csv / countyoutflowYYZZ.csv
  - IRS SOI county data: YYincyallagi.csv
  - BEA Regional Economic Accounts (CAINC1): CAINC1.zip (unzips to CAINC1__ALL_AREAS_*.csv and related files)

Not automated here (authentication required):
  - IPUMS NHGIS extract (nhgis0029) used below
  - IPUMS USA microdata extract (usa_00152) used via "${code}ipums_code"
To fully automate IPUMS downloads, you'll need to add an API-token-based workflow (IPUMS API).
*/

* Ensure expected directory structure exists
capture mkdir "${data}"
capture mkdir "${data}demographic"
capture mkdir "${data}demographic/CAINC1"
capture mkdir "${data}demographic/nhgis0029_csv"
capture mkdir "${data}irs"

* ----------------------------
* IRS SOI: migration files
* ----------------------------
local irs_base "https://www.irs.gov/pub/irs-soi"

forvalues yy = 15/21 {
    local zz = `yy' + 1
    local fn_out "countyoutflow`yy'`zz'.csv"
    local fn_in  "countyinflow`yy'`zz'.csv"

    capture confirm file "${data}irs/`fn_out'"
    if _rc {
        di as txt "Downloading (IRS SOI) `fn_out' ..."
        copy "`irs_base'/`fn_out'" "${data}irs/`fn_out'", replace
    }

    capture confirm file "${data}irs/`fn_in'"
    if _rc {
        di as txt "Downloading (IRS SOI) `fn_in' ..."
        copy "`irs_base'/`fn_in'" "${data}irs/`fn_in'", replace
    }
}

* ----------------------------
* IRS SOI: county income (AGI) files
* ----------------------------
forvalues yy = 15/22 {
    local fn_inc "`yy'incyallagi.csv"

    capture confirm file "${data}irs/`fn_inc'"
    if _rc {
        di as txt "Downloading (IRS SOI) `fn_inc' ..."
        copy "`irs_base'/`fn_inc'" "${data}irs/`fn_inc'", replace
    }
}

* ----------------------------
* BEA Regional: CAINC1.zip
* ----------------------------
local bea_dir "${data}demographic/CAINC1"
local bea_url "https://apps.bea.gov/regional/zip/CAINC1.zip"
local bea_zip "`bea_dir'/CAINC1.zip"

* If we don't already have a CAINC1 "_ALL_AREAS" file, download + unzip the ZIP.
local bea_files : dir "`bea_dir'" files "CAINC1__ALL_AREAS_*.csv"
if "`bea_files'"=="" {
    local bea_files : dir "`bea_dir'" files "CAINC1__ALL_STATES_*.csv"
}

if "`bea_files'"=="" {
    di as txt "Downloading (BEA) CAINC1.zip ..."
    copy "`bea_url'" "`bea_zip'", replace

    local curdir "`c(pwd)'"
    cd "`bea_dir'"
    unzipfile "CAINC1.zip", replace
    cd "`curdir'"

    capture erase "`bea_zip'"
}

* ----------------------------
* NHGIS: verify expected extract file exists (requires IPUMS account)
// NOTE: this file must exist for STEP 1 to run.
* ----------------------------
capture confirm file "${data}demographic/nhgis0029_csv/nhgis0029_ts_nominal_county.csv"
if _rc {
    di as error "Missing NHGIS extract file:"
    di as error "  ${data}demographic/nhgis0029_csv/nhgis0029_ts_nominal_county.csv"
    di as error "Download NHGIS extract nhgis0029 from https://www.nhgis.org/ and unzip it to the path above."
    exit 601
}

//--------------------------------------------------
// STEP 0: Preliminary Set-Up 
//--------------------------------------------------

** Define labels 
label define lb_move_type 	0 "ERROR"				///
							1 "Non-movers"			///
							2 "All movers"			///
							3 "Domestic movers"		///
							4 "Within-state movers"	///
							5 "Inter-state movers"	///
							6 "Foreign movers", modify
							
label define lb_agi 		1 "Under $1"			///
							2 "$1 under $10K"		///
							3 "$10K under $25K"		///
							4 "$25K under $50K"		///
							5 "$50K under $75K"		///
							6 "$75K under $100K"	///
							7 "$100K under $200K"	///
							8 "$200K or more", modify 							


** Define Programs

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

** Reclassify suppressed values as 0 

capture program drop unsuppress
program define unsuppress
    syntax varlist

    foreach v of varlist `varlist' {
        replace `v' = 0 if `v' == -1
    }
end


//-----------------------------------------------------------
// STEP 1: Import and Clean Demographic Data via IPUMS + BEA  
//-----------------------------------------------------------

** Import data 
import delimited 	///
	"${data}demographic/nhgis0029_csv/nhgis0029_ts_nominal_county.csv", clear 

** Describe data 
des 

** Drop unnecc variables 
drop gisjoin statenh countynh name 

** Rename 
rename state state_name 
rename statefp state_fips 
rename county county_name 
rename countyfp county_fips 
rename av0aa population 
rename d15aa pop_urban 
rename d15ab pop_rural 
rename b79aa median_income 
rename av0aam population_margin
rename b79aam median_income_margin 

** Create urban percent 
gen percent_urban = pop_urban / population

** Label variables 
label var state_name "State name"
label var state_fips "State FIPS code"
label var county_name "County name"
label var county_fips "County FIPS code"
label var population "Population count"
label var pop_rural "Rural population count"
label var pop_urban "Urban population count"
label var percent_urban "Percent of population in urban areas"
label var median_income "Median household income (prior year)"
label var population_margin "ACS margin for error: population"
label var median_income_margin "ACS margin for error: median income"

** Save as temporary file 
tempfile demo
save `demo'

** Create three datasets 

** (1) Basic state and county IDs 
keep if year == "2020" 
keep state* county*

** Save as state and county Ids 
save "${data}working/ids", replace 
clear  

** (2) Population data 
use `demo'
keep if !missing(pop_urban) 
tab year 

** Keep 2020 
keep if year == "2020"
drop year median_income* population_margin

** Save data
save "${data}working/population_2020", replace 
clear  

** (3) 2015-2019 ACS data 
use `demo'
keep if !missing(median_income) 
tab year 

** Keep 2020 
keep if year == "2015-2019"
drop year pop_rural pop_urban percent_urban

** Save data
save "${data}working/acs_2015_2019_data", replace 

** Rename for merge 
rename population population_acs

** Merge with other data 
merge 1:1 state_fips county_fips using "${data}working/population_2020", 		///
	keep(match) nogen 

** Generate fips variable
make_fips state_fips county_fips, gen(fips)

** Save data
save "${data}working/demographics_2020", replace 

** Load BEA Data 
local bea_dir "${data}demographic/CAINC1"

* Pick the most recent CAINC1 ALL_AREAS file (fallback to ALL_STATES if needed)
local bea_files : dir "`bea_dir'" files "CAINC1__ALL_AREAS_*.csv"
if "`bea_files'"=="" {
    local bea_files : dir "`bea_dir'" files "CAINC1__ALL_STATES_*.csv"
}
if "`bea_files'"=="" {
    di as error "No BEA CAINC1 CSV found in `bea_dir'. Re-run STEP -1 or check your download."
    exit 601
}

local n_bea : word count `bea_files'
local bea_csv : word `n_bea' of `bea_files'
import delimited "`bea_dir'/`bea_csv'", clear


** Drop unnecc variables 
drop region tablename industryclassification unit geoname 

** Drop empty cells 
drop if missing(linecode)

** Update names 
rename geofips fips 
replace fips = subinstr(fips, `"""', "", .)
destring fips, replace 

** Keep population and per-capita income, dropping personal income (total)
tab description linecode
drop if linecode == 1 	
drop description
	
** Get V* to be in terms of years 
** V9 == 1969 
forvalues i = 9/63 {
	
	local j = 1960 + `i'
	rename v`i' value`j'
	
} // END I LOOP 

** Reshape 
reshape long value, i(fips linecode) j(year)
reshape wide value, i(fips year ) j(linecode)

** Keep years 
keep if inrange(year, 2015, 2023)

** Rename values 
rename value2 population 
rename value3 per_capita_income

** Drop if missing values 
drop if population == "(NA)"

** Keep only counties with all observations 
bysort fips: gen ct = _N 
tab ct
keep if ct == 9
drop ct

** Destring 
destring population, replace 
destring per_capita_income, replace 

** Save data
save "${data}working/bea_economics", replace 


//----------------------------------------------------
// STEP 2: Import and Clean ACS Micro Data via IPUMS 
//----------------------------------------------------

*** RUN IPUMS-PROVIDED CODE AFTER POINTING TO CORRECT LOCATION 
cd "${data}acs/"
do "${code}ipums_code"
cd "${dir}"

** Des 
tab year 

** Sample 18+
keep if age >= 18 

** Sample not living abroad last year 
drop if migplac1 > 56 
drop if migrate1 == 4 

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

** Use migrate1 to update values 

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

** Compress file 
compress 

** Save 
save "${data}working/acs_migration_file", replace 

** Sample 1: Everyone in Multnoma County (d)
keep if state_fips_o == 41 & county_fips_o == 51

** Save data 
save "${data}working/multnomah_acs_data.dta", replace 
export excel using "${data}multnomah_acs_data.xlsx", 	///
	firstrow(variable) sheet(data, replace)
clear 

//----------------------------------------------------
// STEP 3: Import and Clean IRS Migration Data 
//----------------------------------------------------

** Loop over years 
forvalues y = 15(1)21 {
	
	local start = `y'
	local end = `y' + 1 
	
	** Import data (out)
	import delimited "${data}irs/countyoutflow`start'`end'.csv", clear 
	
	** Describe data 
	des 
	
	** Generate year 
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
	
	** Dropo unnecc variable 
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

** Append data 

** Loop over datasets 
foreach file in "irs_county_gross_in" "irs_county_gross_out" "irs_county_flow"{
	
		
	** Loop over years 
	forvalues y = 15(1)21 {

		** Append 
		append using "${data}working/`file'_`y'"
		
	} // END YEAR LOOP 
	
	
	** Order and sort flow file 
	if "`file'" == "irs_county_flow" {
		
		** Loop over orgin and destination state 
		foreach x in "o" "d" {
			
			** Rename 
			rename *_fips_`x' *_fips 
			
			** Merge with county and state names 
			merge m:1 state_fips county_fips using "${data}working/ids", 	///
				keep(match) nogen 
				
			** Rename 
			rename *_fips *_fips_`x' 
			rename *_name *_name_`x' 

			
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

** Create gross file with in and out migration 
use "${data}working/irs_county_gross_in", clear 

** Rename 
rename n1 n1_in_
rename n2 n2_in_
rename agi agi_in_
rename rename *_total *_total_in

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
rename *_total_out

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
make_fips state_fips county_fips, gen(fips)

** Save file 
save "${data}working/irs_county_gross", replace 
clear

//-----------------------------------------------------
// STEP 4: Import and Clean IRS County-Level Aggr. Data 
//-----------------------------------------------------

** Loop over years 
forvalues y = 15(1)22 {
	
	
	** Import data (out)
	import delimited "${data}irs/`y'incyallagi.csv", clear 

	** Describe data 
	des 
	
	** Generate year 
	gen year = 2000 + `y' 
	
	** Define AGI groups 
	label var agi_stub "AGI Brackets"
	label values agi_stub lb_agi 
	
	** Define set of variables to keep 
	keep state* county* agi_stub year n1 mars1 mars2 mars4 n2 elderly 	///
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
	replace a_total_inc = 1000 * a_total_inc
	replace a_wage = 1000 * a_wage 
	
	** Label 
	label var n1 "Number of returns"
	label var mars1 "Number of single returns"
	label var mars1 "Number of MFJ returns"
	label var mars1 "Number of HoH returns"
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

** Loop over years 
forvalues y = 15(1)22 {

	** Append 
	append using "${data}working/irs_county_all_`y'"
		
	} // END YEAR LOOP 
	
** Save file 
save "${data}working/irs_county_all", replace 
	
** Generate fips variable
make_fips state_fips county_fips, gen(fips)

** Save file 
save "${data}working/irs_county_all", replace 

** Close log
log close log_01