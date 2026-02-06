/*****************************************************************************
* Program: 			01_clean_data.do 
* Author(s): 		John Iselin 
* Date Updated:		October 19, 2025

**** Data that has been / will be automatically download

** Demographic data via IPUMS NHGIS 
** Via https://www.nhgis.org/
** Downloaded on October 19, 2025
** EXTRACT DETAILS in "nhgis0031_ts_nominal_county_codebook"

** Economic data via BEA Regional Economic Accounts (CAINC1)
** Via https://apps.bea.gov/regional/downloadzip.htm

** ACS individual data via IPUMS USA 
** Via https://usa.ipums.org/usa/index.shtml
** Downloaded via R program 

** IRS SOI County-Level Migration Files
** Via https://www.irs.gov/statistics/soi-tax-stats-migration-data

** IRS SOI County-Level Files
** Via https://www.irs.gov/statistics/soi-tax-stats-county-data

** NYTimes COVID Cases and Deaths 
** Via https://github.com/nytimes/covid-19-data

**** Data that must be manually downloaded

** DOL Childcare Cost Data 
** Via www.dol.gov/sites/dolgov/files/WB/NDCP2022.xlsx

** BLS Unemployment data 
** Via https://download.bls.gov/pub/time.series/la/la.data.64.County 

** County Centroids (center of population)
** Via https://arc-gis-hub-home-arcgishub.hub.arcgis.com/datasets/myUTK::popcenterstate-us-csv/about

*******************************************************************************/

** Start log file 
capture log close log_01
log using "${logs}01_log_data_clean_${pr_name}_${date}", replace text name(log_01)

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


** Create a gross-migration file via ACS 
capture program drop acs_make_gross_migration
program define acs_make_gross_migration
    version 16.0
    /*
      Build county-year gross migration totals from ACS microdata with origin/destination counties.

      Outputs (wide):
        persons_in_*, persons_out_*, persons_net_*
        households_in_*, households_out_*, households_net_*
        dollars_in_*, dollars_out_*, dollars_net_*

      Move-type indices (mirrors your IRS convention as closely as possible):
        1 = Non-movers (same county)
        2 = All movers (different county) = 4 + 5
        3 = Domestic movers (same as 2 here; foreign already dropped upstream)
        4 = Within-state movers (different county, same state)
        5 = Inter-state movers (different state)
    */

    syntax using/ [if] [in], SAVING(string) [REPLACE] ///
        [ IDSFILE(string) ///
          YEARVAR(name) ORIGFIPS(name) DESTFIPS(name) ///
          PERSONWT(name) HHWT(name) HHPERWT(name) HEADVAR(name) INCOME(name) SAMPLE(string)]

    // Defaults consistent with your 01_clean_data.do
    if "`idsfile'"  == "" local idsfile  "${data}working/ids"
    if "`yearvar'"  == "" local yearvar  year
    if "`origfips'" == "" local origfips fips_o
    if "`destfips'" == "" local destfips fips_d
	if "`personwt'" == "" local personwt perwt
    if "`hhperwt'"  == "" local hhperwt  hh_perwt
    if "`hhwt'"     == "" local hhwt     hhwt
    if "`headvar'"  == "" local headvar  hh_head
    if "`income'"   == "" local income   inctot

    // Load microdata (optionally subset via if/in)
    use "`using'" `if' `in', clear
	
	if "`sample'" != "" keep if `sample'

    // Basic checks
    foreach v in `yearvar' `origfips' `destfips' `personwt' `hhwt' `headvar' `income' {
        capture confirm variable `v'
        if _rc {
            di as err "Required variable `v' not found in `using'."
            exit 198
        }
    }

    // Keep only valid year/origin/destination
    drop if missing(`yearvar') | missing(`origfips') | missing(`destfips')

    // Income: treat missing as 0 (keep negatives as reported)
    replace `income' = 0 if missing(`income')

    // Build weighted components at the person level
    gen double persons_wt = `hhperwt' if `headvar' == 1
    gen double dollars_wt = `income' * `personwt'
    gen double households_wt = `hhwt' if `headvar' == 1
    replace households_wt = 0 if missing(households_wt)

    // Collapse to origin-destination-year flow first
    keep `yearvar' `origfips' `destfips' persons_wt dollars_wt households_wt
    collapse (sum) persons=persons_wt dollars=dollars_wt households=households_wt, ///
        by(`yearvar' `origfips' `destfips')

    // Derive state/county components for mover-type logic
    gen int state_o  = floor(`origfips'/1000)
    gen int state_d  = floor(`destfips'/1000)

    gen byte same_county = (`origfips' == `destfips')
    gen byte same_state  = (state_o == state_d)
    gen byte within_state_mover = same_state & !same_county
    gen byte inter_state_mover  = !same_state

    // -----------------------
    // IN-MIGRATION (by destination county)
    // -----------------------
    preserve
        gen long fips = `destfips'
        gen int state_fips  = floor(fips/1000)
        gen int county_fips = mod(fips, 1000)

        // type 1/4/5 components
        foreach m in persons households dollars {
            gen double `m'_1 = `m' if same_county
            gen double `m'_4 = `m' if within_state_mover
            gen double `m'_5 = `m' if inter_state_mover
        }

        collapse (sum) persons_1 persons_4 persons_5 ///
                       households_1 households_4 households_5 ///
                       dollars_1 dollars_4 dollars_5, ///
                by(`yearvar' fips state_fips county_fips)

        // build 2 and 3
        gen double persons_2    = persons_4 + persons_5
        gen double persons_3    = persons_2
        gen double households_2 = households_4 + households_5
        gen double households_3 = households_2
        gen double dollars_2    = dollars_4 + dollars_5
        gen double dollars_3    = dollars_2

        // rename to *_in_*
        foreach t in 1 2 3 4 5 {
            rename persons_`t'    persons_in_`t'
            rename households_`t' households_in_`t'
            rename dollars_`t'    dollars_in_`t'
        }

        tempfile __in
        save `__in', replace
    restore

    // -----------------------
    // OUT-MIGRATION (by origin county)
    // -----------------------
    preserve
        gen long fips = `origfips'
        gen int state_fips  = floor(fips/1000)
        gen int county_fips = mod(fips, 1000)

        foreach m in persons households dollars {
            gen double `m'_1 = `m' if same_county
            gen double `m'_4 = `m' if within_state_mover
            gen double `m'_5 = `m' if inter_state_mover
        }

        collapse (sum) persons_1 persons_4 persons_5 ///
                       households_1 households_4 households_5 ///
                       dollars_1 dollars_4 dollars_5, ///
                by(`yearvar' fips state_fips county_fips)

        gen double persons_2    = persons_4 + persons_5
        gen double persons_3    = persons_2
        gen double households_2 = households_4 + households_5
        gen double households_3 = households_2
        gen double dollars_2    = dollars_4 + dollars_5
        gen double dollars_3    = dollars_2

        foreach t in 1 2 3 4 5 {
            rename persons_`t'    persons_out_`t'
            rename households_`t' households_out_`t'
            rename dollars_`t'    dollars_out_`t'
        }

        tempfile __out
        save `__out', replace
    restore

    // -----------------------
    // Merge in/out; compute net
    // -----------------------
    use `__in', clear
    merge 1:1 `yearvar' fips state_fips county_fips using `__out', nogen

    // Replace missings with 0 prior to net calcs (counties can be only in or only out)
    foreach m in persons households dollars {
        foreach t in 1 2 3 4 5 {
            replace `m'_in_`t'  = 0 if missing(`m'_in_`t')
            replace `m'_out_`t' = 0 if missing(`m'_out_`t')
        }
    }

    // Net = in - out (types 2/3/4/5 are the meaningful migration nets; 1 will be ~0 by construction)
    foreach m in persons households dollars {
        foreach t in 2 3 4 5 {
            gen double `m'_net_`t' = `m'_in_`t' - `m'_out_`t'
        }
    }

    // Merge names
    merge m:1 state_fips county_fips using "`idsfile'", keep(master match) nogen

    // Labels
    label var fips "County FIPS (state*1000 + county)"
    label var state_fips "State FIPS"
    label var county_fips "County FIPS"

    label var persons_in_2  "Persons, in-migration, all movers"
    label var persons_out_2 "Persons, out-migration, all movers"
    label var persons_net_2 "Persons, net migration, all movers"

    label var households_in_2  "Households, in-migration, all movers (HH heads)"
    label var households_out_2 "Households, out-migration, all movers (HH heads)"
    label var households_net_2 "Households, net migration, all movers (HH heads)"

    label var dollars_in_2  "Dollars, in-migration, all movers (INCTOT*PERWT)"
    label var dollars_out_2 "Dollars, out-migration, all movers (INCTOT*PERWT)"
    label var dollars_net_2 "Dollars, net migration, all movers (INCTOT*PERWT)"

    order `yearvar' fips state_fips county_fips state_name county_name, first
    sort `yearvar' state_fips county_fips
    compress

    // Save
    save "`saving'", `replace'
	clear
	
end

********************************************************************************
** STEP 1: Download and check non-IPUMS data 
********************************************************************************

** Ensure expected directory structure exists
capture mkdir "${data}"
capture mkdir "${data}working"
capture mkdir "${data}demographic"
capture mkdir "${data}demographic/CAINC1"
capture mkdir "${data}demographic/nhgis0031_csv"
capture mkdir "${data}demographic/dol"
capture mkdir "${data}demographic/bls"
capture mkdir "${data}irs"
capture mkdir "${data}covid"

** This block downloads public-use source data directly from official URLs if 
** not present locally.
/*
Automated downloads included:
  - IRS SOI county-to-county migration: countyinflowYYZZ.csv / countyoutflowYYZZ.csv
  - IRS SOI county data: YYincyallagi.csv
  - BEA Regional Economic Accounts (CAINC1): CAINC1.zip (unzips to CAINC1__ALL_AREAS_*.csv and related files)
*/

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
* NYTimes COVID Data 
* ----------------------------
local covid_dir "${data}covid"
local covid_url "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

* If we don't already have a COVID file, download.
local covid_file : dir "`covid_dir'" files "covid_nyt.csv"
if "`covid_file'"=="" {
    local covid_file : dir "`covid_dir'" files "covid_nyt.csv"
}

if "`covid_file'"=="" {
    di as txt "Downloading (COVID)  ..."
    copy "`covid_url'" "`covid_dir'/covid_nyt.csv", replace
}

** This block checks that non-automatically downloaded data are present: 
/*
Non-automtated downloads included:
  - DOL childcare data (through 2022)
  - BLS unemployment data 
*/

** Define paths 
local dol_dir "${data}demographic/dol/NDCP2022.xlsx"
local bls_dir "${data}demographic/bls/la.data.64.County" 

capture confirm file `dol_dir' // Check if the file exists

if _rc != 0 {
    display "Error: The file `dol_dir' was not found."
    display "Execution of the do-file is stopping."
    exit // Stops the do-file/program
}

capture confirm file `bls_dir' // Check if the file exists

if _rc != 0 {
    display "Error: The file `bls_dir' was not found."
    display "Execution of the do-file is stopping."
    exit // Stops the do-file/program
}



********************************************************************************
** STEP 2: Clean demographic + economic data via IPUMS, BLS, and BEA 
********************************************************************************

** Import data 
import delimited 	///
	"${data}demographic/nhgis0031_csv/nhgis0031_ts_nominal_county.csv", clear 

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

** Make FIPS 
make_fips state_fips county_fips, gen(fips)

** Save as state and county Ids 
save "${data}working/ids", replace 

** Save state IDS 
keep state_fips state_name 
duplicates drop 

** Save as state and county Ids 
save "${data}working/state_ids", replace 

clear  

** (2) Population data 
use `demo'
keep if !missing(pop_urban) 
tab year 

** Keep 2020 
keep if year == "2020"
drop year median_income* population_margin

** Make FIPS 
make_fips state_fips county_fips, gen(fips)

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

** Make FIPS 
make_fips state_fips county_fips, gen(fips)

** Save data
save "${data}working/acs_2015_2019_data", replace 

** Rename for merge 
rename population population_acs

** Merge with other data 
merge 1:1 state_fips county_fips using "${data}working/population_2020", 		///
	keep(match) nogen 

** Save data
save "${data}working/demographics_2020", replace 

** Load BEA Data 
import delimited "${data}demographic/CAINC1/CAINC1__ALL_AREAS_1969_2023.csv",	///
	clear 

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
keep if inrange(year, 2015, ${end_year_acs})

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

** Extrapolate through 2024 
expand 2 if year == 2023, gen(tag)
replace year = 2024 if tag == 1 
replace population = . if tag == 1 
replace per_capita_income = . if tag == 1 

** Get list of all counties 
qui levelsof fips, local(fips)

** Loop over variables
foreach v of varlist population per_capita_income {
	
	** Loop over all FIPS 
    foreach c of local fips {
		
		quietly{
			
			regress `v' year if fips == `c' & tag != 1 
			predict `v'_hat
			replace `v' = `v'_hat if fips == `c' & year == 2024
			drop `v'_hat
			
		} // END QUIET
    } // END FIPS LOOP 
} // END VAR LOOP 

** Drop tag 
drop tag

** Save data
save "${data}working/bea_economics", replace 

** Load BLS Unemployment data 
import delimited "${data}demographic\bls\la.data.64.County", clear 

** Keep annual average 
keep if period == "M13"
drop period 

** Keep only Unemployment Rate 
gen measure = substr(series_id, 20,1)
keep if measure == "3"
drop measure 

** Keep years 
keep if inrange(year, 2015, ${end_year_acs})
tab year 

** Define counties 
gen fips = substr(series_id, 6,5)
destring fips, replace 
drop series_id 
isid fips year 

** Drop PR 
drop if fips > 60000
drop footnote_codes 

** Update names 
rename value unemp 
order year fips unemp 

destring unemp, replace 

** Save data
save "${data}working/bls_unemployment", replace 

** Load County Centroids 
import delimited "${data}demographic\PopCenterCounty_US.csv", clear 

** Keep required years (2010)
keep if year == 2010 

** Keep required variables 
keep geographicindentifier latitude longitude 

** Rename 
rename geographicindentifier fips 
rename latitude lat 
rename longitude lon 

** Drop PR 
drop if fips > 60000

** Save data
save "${data}working/pop_centers", replace 




********************************************************************************
** STEP 3: Import and Clean NYTimes COVID-19 Data 
********************************************************************************

** Import data 
import delimited using "${data}covid/covid_nyt.csv", varnames(1) clear case(lower) 

** Describe data 
des 

** Set up date information 
generate num_date = date(date, "YMD")
format num_date %td
drop date 

** Rename 
rename state state_name 
rename county county_name 
rename num_date date 

** keep only counties 
keep if !missing(fips)

** Keep in 50 states 
drop if state_name == "Puerto Rico"
drop if state_name == "Virgin Islands"
drop if state_name == "Northern Mariana Islands"

** Sort 
sort date fips 

** Create panel 
xtset fips date

** Fill in panel 
tsfill, full 

** Fill in missing values 
replace cases = 0 if missing(cases)
replace deaths = 0 if missing(deaths)

** Preserve data 
preserve 

** Preserve fips codes and names 
keep if !missing(state_name)
keep if !missing(county_name)
duplicates drop fips state_name county_name, force 

** Save as temporary data 
tempfile state_county_names 
save `state_county_names'
clear 

** Restore 
restore 

** Drop and merge in names 
drop state_name county_name
merge m:1 fips using `state_county_names', keep(master match) nogen 

** Get year, month, day 
gen year = year(date)
gen month = month(date)
gen day = day(date)

** Order data 
order date year month day fips state county cases deaths 

** Calculate cumulative cases and deaths 
bysort fips (date): gen cases_cum = sum(cases)
bysort fips (date): gen deaths_cum = sum(deaths)

** Merge population data (2020)
merge m:1 fips using "${data}working/population_2020", keep(match) nogen  

** Save file 
save ${data}working/covid_cleaned.dta, replace 

** Keep one observation per month 
keep year month fips state_name county_name cases deaths population
collapse (sum) cases deaths (mean) population, by(year month fips state_name county_name) 
sort year month fips 
egen date = group(year month)
drop year month 

** Calculate cumulative cases and deaths 
bysort fips (date): gen cases_cum = sum(cases)
bysort fips (date): gen deaths_cum = sum(deaths)

** Generate per capita figures
replace cases_cum = 1000 * cases_cum / population
replace deaths_cum = 1000 * deaths_cum / population
drop population cases deaths

** Reshape wide 
reshape wide cases_cum deaths_cum, i(fips state_name county_name) j(date)

** Save file 
save ${data}working/covid_cleaned_wide.dta, replace 
clear


********************************************************************************
** STEP 4: Import and Clean IPUMS ACS Data via IPUMS 
********************************************************************************

** Load data 
forvalues y = 2015(1)$end_year_acs {
	
	** Import CSV
	import delimited using "${data}acs/acs_`y'", varnames(1) clear case(lower)
	
	** Save as temporary data 
	tempfile acs_`y'
	save `acs_`y''
	clear 
	
} // END YEAR LOOP 

** Append data 
forvalues y = 2015(1)$end_year_acs {
	
	append using `acs_`y''	
	
} // END YEAR LOOP 

** Des 
tab year 

** Define # of adults and kids in HHs 
gen adult = age >= 18 
gen child = age < 18 
bysort year serial: gen hh_size = _N 
bysort year serial: egen hh_adult_ct = total(adult)
bysort year serial: egen hh_child_ct = total(child)

** Define HH-wide indicators for creation of gross migration files 
gen college = educd >= 0 
gen hh_any_child = hh_child_ct > 0
bysort year serial: egen hh_any_college = max(college)

** Define weighted # of people 
bysort year serial: egen hh_perwt = total(perwt)

** Sample 18+
drop if child == 1 
drop child adult 

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

** Tag HH head 
gen byte hh_head = (relate == 1)

** Compress file 
compress 

** Save 
save "${data}working/acs_migration_file", replace 

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


** Loop over orgin and destination states
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

** Create gross-migration files for ACS 

** All (25+)
acs_make_gross_migration using "${data}working/acs_migration_file", ///
    saving("${data}working/acs_county_gross_25plus") replace sample("age >= 25")
	
** College-degrees 
acs_make_gross_migration  using "${data}working/acs_migration_file", ///
    saving("${data}working/acs_county_gross_college") replace sample("hh_any_college == 1" & "age >= 25")
	
** No Kids
acs_make_gross_migration using "${data}working/acs_migration_file", ///
    saving("${data}working/acs_county_gross_nokids") replace sample("hh_any_child == 0" & "age >= 25")
	
********************************************************************************
** STEP 5: Import and Clean IRS Migration Data 
********************************************************************************
	
** Loop over years 
forvalues y = 16(1)22 {
	
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
	forvalues y = 16(1)22 {

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

********************************************************************************
** STEP 6: Import and Clean IRS County-Level Aggr. Data 
********************************************************************************

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

********************************************************************************
** STEP 7: Import and Clean DOL Childcare Cost Data 
********************************************************************************

** Import data 
import excel using "${data}demographic/dol/NDCP2022.xlsx", firstrow case(lower) clear

** Drop if not in 50 states + DC 
drop if state_fips == 72

** Keep in year range 
keep if inrange(studyyear, 2015, 2022)

** Keep required variables 
keep county_fips_code studyyear me 	///
	mcinfant mctoddler mcpreschool 	///
	mfccinfant mfcctoddler mfccpreschool
	
rename county_fips fips 
rename studyyear year 

** Fill-in missing variables 
xtset fips year 
tsfill, full

** Interpotate
foreach var of varlist me mc* mf* {
	bys fips: ipolate `var' year, g(tmp1)
	replace `var' = tmp1 if missing(`var')
	drop tmp1
} // END VAR LOOP 

** Generate value as a percent of median income 
gen mc_infant_med = mcinfant / me 
gen mc_toddler_med = mctoddler / me 
gen mc_preschool_med = mcpreschool / me 
gen mf_infant_med = mfccinfant / me 
gen mf_toddler_med = mfcctoddler / me 
gen mf_preschool_med = mfccpreschool / me 

** Drop unnecc. variables 
drop me mcinfant mctoddler mcpreschool mfcc* 

** Inflate forwards by two years, by county and variable 
local ct = ${end_year_acs} - 2022 + 1 
expand `ct' if year == 2022
by fips year, sort: replace year = year + _n - 1 if year == 2022 & _n > 1

** Get list of all counties 
qui levelsof fips, local(fips)

** Loop over variables
foreach v of varlist mc_* mf_* {
	
	** Replace 2023 + 2024 values with missings 
    replace `v' = . if year > 2022
	
	** Loop over all FIPS 
    foreach c of local fips {
		
		quietly{
			
			** Run if not missing too many observations 
			count if !missing(`v') & fips == `c' 
			if `r(N)' > 3 {
				regress `v' year if fips == `c'
				predict `v'_hat
				replace `v' = `v'_hat if fips == `c' & year > 2022
				drop `v'_hat
			} // END IF-STATEMENT
			else drop if fips == `c'
		} // END QUIET
    } // END FIPS LOOP 
} // END VAR LOOP 

** Save file
save "${data}working/dol_childcare", replace


********************************************************************************
** STEP 8: Calculate County-Level Property Tax Rates from ACS Data
********************************************************************************

** Load ACS migration file (contains proptx99, valueh, qprotx99, qvalueh)
use "${data}working/acs_migration_file", clear

** Keep household heads only (relate == 1 for household reference person)
keep if relate == 1

** Generate FIPS code for destination county
gen fips = fips_d

** Drop if missing FIPS
drop if missing(fips)

** Drop observations where PROPTX99 == 0 (N/A - not applicable)
drop if proptx99 == 0

** Convert PROPTX99 codes to dollar midpoint values
** Based on IPUMS coding scheme: https://usa.ipums.org/usa-action/variables/PROPTX99#codes_section
gen proptx_dollars = .

** Code 1: None (0)
replace proptx_dollars = 0 if proptx99 == 1

** Code 2: $1-49 -> midpoint $25
replace proptx_dollars = 25 if proptx99 == 2

** Codes 3-12: $50-99, $100-149, ... $500-549 (increments of 50, midpoints)
forvalues i = 3/12 {
    local lower = (`i' - 3) * 50 + 50
    local upper = `lower' + 49
    local midpoint = (`lower' + `upper') / 2
    replace proptx_dollars = `midpoint' if proptx99 == `i'
}

** Codes 13-22: $550-599, $600-699, ... $1000-1099 (transitioning to $100 increments)
replace proptx_dollars = 575 if proptx99 == 13
replace proptx_dollars = 650 if proptx99 == 14
replace proptx_dollars = 750 if proptx99 == 15
replace proptx_dollars = 850 if proptx99 == 16
replace proptx_dollars = 950 if proptx99 == 17
replace proptx_dollars = 1050 if proptx99 == 18
replace proptx_dollars = 1150 if proptx99 == 19
replace proptx_dollars = 1250 if proptx99 == 20
replace proptx_dollars = 1350 if proptx99 == 21
replace proptx_dollars = 1450 if proptx99 == 22

** Codes 23+: Higher ranges with $100 increments then $500/$1000 at top
** $1500-1599, $1600-1699, ... up to high values
forvalues i = 23/62 {
    local lower = (`i' - 23) * 100 + 1500
    local upper = `lower' + 99
    local midpoint = (`lower' + `upper') / 2
    replace proptx_dollars = `midpoint' if proptx99 == `i'
}

** Codes 63+: Higher brackets ($5500+)
replace proptx_dollars = 5750 if proptx99 == 63
replace proptx_dollars = 6250 if proptx99 == 64
replace proptx_dollars = 6750 if proptx99 == 65
replace proptx_dollars = 7250 if proptx99 == 66
replace proptx_dollars = 7750 if proptx99 == 67
replace proptx_dollars = 8500 if proptx99 == 68
replace proptx_dollars = 9500 if proptx99 == 69

** For codes 70+: Use approximate midpoints for higher brackets
** These are $10000+ ranges
forvalues i = 70/100 {
    if proptx_dollars == . & proptx99 == `i' {
        local midpoint = 10000 + (`i' - 70) * 1000
        replace proptx_dollars = `midpoint' if proptx99 == `i'
    }
}

** Top codes (very high property taxes)
replace proptx_dollars = 75000 if proptx99 >= 140 & proptx99 < 159
replace proptx_dollars = 100000 if proptx99 == 159

** Label variable
label var proptx_dollars "Property tax ($ midpoint from PROPTX99 codes)"

** Drop if property tax could not be assigned or home value is missing/zero
drop if missing(proptx_dollars)
drop if missing(valueh) | valueh == 0 | valueh == 9999999

** Calculate property tax rate (as percentage of home value)
gen prop_rate = 100 * proptx_dollars / valueh
label var prop_rate "Property tax rate (% of home value)"

** ============================================================================
** VERSION 1: Overall (all observations)
** ============================================================================
preserve

** Collapse to county X year, weighted by household weight
collapse (mean) prop_rate_mean = prop_rate ///
         (semean) prop_rate_se = prop_rate ///
         (count) prop_rate_n = prop_rate ///
         [fw = hhwt], by(year fips)

** Label variables
label var prop_rate_mean "Mean property tax rate (% of home value)"
label var prop_rate_se "SE of property tax rate"
label var prop_rate_n "Number of observations"

** Generate state and county FIPS
gen state_fips = floor(fips / 1000)
gen county_fips = mod(fips, 1000)

** Merge with county names
merge m:1 state_fips county_fips using "${data}working/ids", keep(master match) nogen

** Handle suppressed counties (county_fips == 0): merge state names and set county to "Other"
merge m:1 state_fips using "${data}working/state_ids", keep(master match) update nogen
replace county_name = "Other" if county_fips == 0

** Order variables
order year fips state_fips county_fips state_name county_name prop_rate_mean prop_rate_se prop_rate_n

** Sort
sort fips year

** Save overall version
save "${data}working/property_tax_rates_overall", replace

** Export to CSV
export delimited using "${data}working/property_tax_rates_overall.csv", replace

restore

** ============================================================================
** VERSION 2: Excluding allocated values (qprotx99 != 4, qvalueh != 4)
** ============================================================================

** Drop observations where values are allocated (quality flag == 4)
drop if qprotx99 == 4
drop if qvalueh == 4

** Collapse to county X year, weighted by household weight
collapse (mean) prop_rate_mean = prop_rate ///
         (semean) prop_rate_se = prop_rate ///
         (count) prop_rate_n = prop_rate ///
         [fw = hhwt], by(year fips)

** Label variables
label var prop_rate_mean "Mean property tax rate (% of home value, excl. allocated)"
label var prop_rate_se "SE of property tax rate (excl. allocated)"
label var prop_rate_n "Number of observations (excl. allocated)"

** Generate state and county FIPS
gen state_fips = floor(fips / 1000)
gen county_fips = mod(fips, 1000)

** Merge with county names
merge m:1 state_fips county_fips using "${data}working/ids", keep(master match) nogen

** Handle suppressed counties (county_fips == 0): merge state names and set county to "Other"
merge m:1 state_fips using "${data}working/state_ids", keep(master match) update nogen
replace county_name = "Other" if county_fips == 0

** Order variables
order year fips state_fips county_fips state_name county_name prop_rate_mean prop_rate_se prop_rate_n

** Sort
sort fips year

** Save version excluding allocated
save "${data}working/property_tax_rates_excl_allocated", replace

** Export to CSV
export delimited using "${data}working/property_tax_rates_excl_allocated.csv", replace

** Display summary
dis "Property tax rate calculation complete."
dis "Overall version saved to: ${data}working/property_tax_rates_overall.dta"
dis "Excluding allocated saved to: ${data}working/property_tax_rates_excl_allocated.dta"

clear


** Close log
log close log_01
