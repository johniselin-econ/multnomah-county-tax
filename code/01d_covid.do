/*****************************************************************************
* File:        01d_covid.do
* Purpose:     Import and clean NYTimes COVID-19 county-level data
* Called by:   01_clean_data.do
* Outputs:     data/working/covid_cleaned.dta      (daily panel)
*              data/working/covid_cleaned_wide.dta  (monthly, reshaped wide)
******************************************************************************/

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

** Zero-fill missing values: the NYTimes COVID file is cumulative, so a
** county-date missing from the raw data means zero recorded cases/deaths
** on that date (the county had not yet appeared in reporting). Filling
** with zero before computing cumulative sums ensures the panel is complete.
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
save "${data}working/covid_cleaned.dta", replace

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
save "${data}working/covid_cleaned_wide.dta", replace
clear
