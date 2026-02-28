/*****************************************************************************
* File:        01h_auxiliary.do
* Purpose:     Clean DOL childcare cost data and calculate county-level
*              property tax rates from ACS microdata
* Called by:   01_clean_data.do
* Outputs:     data/working/dol_childcare.dta
*              data/working/property_tax_rates_overall.dta  (+ .csv)
*              data/working/property_tax_rates_excl_allocated.dta (+ .csv)
******************************************************************************/

//--------------------------------------------------
// DOL Childcare Cost Data
//--------------------------------------------------

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

** Interpolate gaps and carry forward for 2023-24:
** DOL data ends in 2022 (biennial survey with gaps). We use Stata's ipolate
** to linearly interpolate interior missing years, then extrapolate 2023-24
** via per-county OLS regression on year. Counties with fewer than 4
** non-missing observations are dropped (insufficient data for trend).
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


//--------------------------------------------------
// County-Level Property Tax Rates from ACS Data
//--------------------------------------------------

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

** Convert PROPTX99 codes to dollar midpoint values.
** PROPTX99 is a categorical variable from IPUMS that records annual property
** tax payments in coded brackets. We assign the midpoint dollar value of
** each bracket so we can compute a continuous tax rate. The mapping follows
** the IPUMS coding scheme:
**   https://usa.ipums.org/usa-action/variables/PROPTX99#codes_section
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
    local midpoint = 10000 + (`i' - 70) * 1000
    replace proptx_dollars = `midpoint' if proptx99 == `i'
}

** Top codes (very high property taxes)
replace proptx_dollars = 75000 if proptx99 >= 140 & proptx99 < 159
replace proptx_dollars = 100000 if proptx99 == 159

** Label variable
label var proptx_dollars "Property tax ($ midpoint from PROPTX99 codes)"

** Drop if property tax could not be assigned or home value is missing/zero
drop if missing(proptx_dollars)
drop if missing(valueh) | valueh == 0 | valueh == 9999999

** Calculate property tax rate as a simple ratio of tax paid to home value.
** This is an effective rate that does not adjust for homestead exemptions
** or other deductions — it reflects the household's reported payment
** divided by reported home value.
gen prop_rate = 100 * proptx_dollars / valueh
label var prop_rate "Property tax rate (% of home value)"

********************************************************************************
** VERSION 1: Overall (all observations)
********************************************************************************
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

********************************************************************************
** VERSION 2: Excluding allocated values (qprotx99 != 4, qvalueh != 4)
********************************************************************************

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
