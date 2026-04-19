/*****************************************************************************
* File:        01d_covid.do
* Purpose:     Import and clean NYTimes COVID-19 county-level data
*              Import and clean JII COVID restriction-duration data
* Called by:   01_clean_data.do
* Outputs:     data/working/covid_cleaned.dta      (daily panel)
*              data/working/covid_cleaned_wide.dta  (monthly, reshaped wide)
*              data/working/jii_stringency.dta      (cross-sectional, 5 measures)
******************************************************************************/

** Import data
import delimited using "${data}covid/covid_nyt.csv", varnames(1) clear case(lower)

** Describe data
des

** Convert the string `date` column (YYYY-MM-DD) to a numeric Stata date,
** then replace the original string column so downstream code works with a
** numeric date throughout.
generate num_date = date(date, "YMD")
format num_date %td
drop date
rename num_date date

** Rename text columns to project convention
rename state state_name
rename county county_name

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
merge m:1 fips using `state_county_names', gen(names_mrg) keep(master match)
project_report_merge, gen(names_mrg) tag("covid_names")

** Get year, month, day
gen year = year(date)
gen month = month(date)
gen day = day(date)

** Order data
order date year month day fips state_name county_name cases deaths

** Calculate cumulative cases and deaths
bysort fips (date): gen cases_cum = sum(cases)
bysort fips (date): gen deaths_cum = sum(deaths)

** Merge population data (2020)
merge m:1 fips using "${data}working/population_2020", gen(pop_mrg) keep(match)
project_report_merge, gen(pop_mrg) tag("covid_pop2020")

** Save file
compress
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
compress
save "${data}working/covid_cleaned_wide.dta", replace
clear


********************************************************************************
** JII COVID Stringency Data
** Source: JII Covid data.dta — restriction-duration measures (days) by county
** Used for: COVID policy stringency k-means donor pool
********************************************************************************

** Load JII data
if !fileexists("${data}JII Covid data.dta") {
	dis as error "ERROR: JII Covid data.dta not found at ${data}JII Covid data.dta"
	error 601
}
use "${data}JII Covid data.dta", clear

** Drop missing FIPS (1 row)
drop if missing(scfips)

** Rename FIPS
rename scfips fips

** Keep restriction-duration variables
keep fips msahodays restclosedays gatherbandays strictgatherbandays maskpubdays

** Label variables
label var msahodays "Days under stay-at-home order"
label var restclosedays "Days restaurants closed"
label var gatherbandays "Days under gathering ban"
label var strictgatherbandays "Days under strict gathering ban"
label var maskpubdays "Days under public mask mandate"

** Sanity-check Multnomah County values against the JII data vintage we
** originally cleaned against. If JII re-releases and values drift, we want
** a visible warning rather than a hard pipeline halt.
** Source: JII COVID-19 US State Policy database (policy durations in days).
local jii_vintage "JII COVID-19 US State Policy data (accessed 2024-03)"
foreach pair in msahodays=88 restclosedays=109 gatherbandays=295 ///
    strictgatherbandays=284 maskpubdays=184 {
    local v : word 1 of `=subinstr("`pair'", "=", " ", 1)'
    local expected : word 2 of `=subinstr("`pair'", "=", " ", 1)'
    qui summ `v' if fips == 41051, meanonly
    if r(mean) != `expected' {
        di as error "  WARNING: `v' for Multnomah = " r(mean) ///
            ", expected `expected' (`jii_vintage')"
    }
}

** Save
compress
save "${data}working/jii_stringency.dta", replace
clear
