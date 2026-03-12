/*****************************************************************************
* File:        01c_demographics.do
* Purpose:     Clean NHGIS demographic data, BEA economics, BLS unemployment,
*              and county centroids
* Called by:   01_clean_data.do
* Outputs:     data/working/ids.dta
*              data/working/state_ids.dta
*              data/working/population_2020.dta
*              data/working/acs_2015_2019_data.dta
*              data/working/demographics_2020.dta
*              data/working/bea_economics.dta
*              data/working/bls_unemployment.dta
*              data/working/pop_centers.dta
******************************************************************************/

//--------------------------------------------------
// NHGIS Demographic Data
//--------------------------------------------------

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

** Save as state and county IDs
save "${data}working/ids", replace

** Save state IDs
keep state_fips state_name
duplicates drop

** Save as state IDs
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

//--------------------------------------------------
// BEA Regional Economic Accounts (CAINC1)
//--------------------------------------------------

** Load BEA Data — resolve whichever filename variant BEA provided
local bea_dir "${data}demographic/CAINC1"
local bea_file : dir "`bea_dir'" files "CAINC1__ALL_AREAS_*.csv"
if `"`bea_file'"' == "" {
	local bea_file : dir "`bea_dir'" files "CAINC1__ALL_STATES_*.csv"
}
if `"`bea_file'"' == "" {
	di as err "ERROR: No CAINC1 CSV found in `bea_dir'"
	exit 601
}
import delimited "`bea_dir'/`bea_file'", clear

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
forvalues i = 9/64 {

	local j = 1960 + `i'
	rename v`i' value`j'

} // END I LOOP

** Reshape
reshape long value, i(fips linecode) j(year)
reshape wide value, i(fips year ) j(linecode)

** Keep years
keep if inrange(year, ${start_year_acs}, ${end_year_acs})

** Rename values
rename value2 population
rename value3 per_capita_income

** Drop string "(NA)" values before converting to numeric
qui count if population == "(NA)"
if r(N) > 0 di as txt "  Dropped " r(N) " obs with population == (NA)"
drop if population == "(NA)"

qui count if per_capita_income == "(NA)"
if r(N) > 0 di as txt "  Dropped " r(N) " obs with per_capita_income == (NA)"
drop if per_capita_income == "(NA)"

** Now safe to convert
destring population, replace
destring per_capita_income, replace

** Keep only counties with all observations
bysort fips: gen ct = _N
tab ct
qui summ ct
local full_years = `r(max)'
qui count if ct < `full_years'
di as txt "  Dropping " r(N) " county-year obs from unbalanced panel (require `full_years' years)"
keep if ct == `full_years'
drop ct

** Save data
save "${data}working/bea_economics", replace

//--------------------------------------------------
// BLS Unemployment (LAUS)
//--------------------------------------------------

** Load BLS Unemployment data
import delimited "${data}demographic/bls/la.data.64.County", clear

** Keep annual average
keep if period == "M13"
drop period

** Keep only Unemployment Rate
gen measure = substr(series_id, 20,1)
keep if measure == "3"
drop measure

** Keep years
keep if inrange(year, ${start_year_acs}, ${end_year_acs})
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

//--------------------------------------------------
// County Centroids (center of population)
//--------------------------------------------------

** Load County Centroids
import delimited "${data}demographic/PopCenterCounty_US.csv", clear

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
clear
