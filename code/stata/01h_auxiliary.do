/*****************************************************************************
* File:        01h_auxiliary.do
* Purpose:     Clean DOL childcare cost data and calculate county-level
*              property tax rates from ACS microdata
* Called by:   01_clean_data.do
* Inputs:      data/working/acs_proptx_slim.dta (slim subset from 01e_acs.do)
* Outputs:     data/working/dol_childcare.dta
*              data/working/property_tax_rates_overall.dta
*              data/working/property_tax_rates_excl_allocated.dta
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

rename county_fips_code fips
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

** Generate value as a percent of median income.
** Naming mirrors the DOL source columns exactly: market-care (mc) columns
** keep the 2-char prefix; market family-childcare (mfcc) columns keep all
** 4 chars, so the output prefix matches the input prefix in both groups.
gen mc_infant_med = mcinfant / me
gen mc_toddler_med = mctoddler / me
gen mc_preschool_med = mcpreschool / me
gen mfcc_infant_med = mfccinfant / me
gen mfcc_toddler_med = mfcctoddler / me
gen mfcc_preschool_med = mfccpreschool / me

** Drop the source columns. List `mfcc` siblings explicitly — `mfcc*` would
** also match the freshly-generated mfcc_*_med columns and silently delete them.
drop me mcinfant mctoddler mcpreschool mfccinfant mfcctoddler mfccpreschool

** Inflate forwards by two years, by county and variable
local ct = ${end_year_acs} - 2022 + 1
expand `ct' if year == 2022
by fips year, sort: replace year = year + _n - 1 if year == 2022 & _n > 1

** Extrapolate 2023-24 by county using county-specific linear trends.
** Counties with <=3 observed values for a given variable are left missing for
** extrapolated years rather than being dropped from the full childcare panel.
foreach v of varlist mc_* mfcc_* {
    tempfile coef_`v'

	** Replace 2023 + 2024 values with missings
    replace `v' = . if year > 2022

    preserve
        keep if year <= 2022 & !missing(`v')
        statsby b_cons=_b[_cons] b_year=_b[year] n_obs=e(N), by(fips) clear: ///
            regress `v' year
        keep if n_obs > 3
        save `coef_`v'', replace
    restore

    merge m:1 fips using `coef_`v'', keep(master match) nogen
    replace `v' = b_cons + b_year * year if year > 2022 & missing(`v')

    qui count if year > 2022 & missing(`v')
    if r(N) > 0 {
        di as txt "  `v': left " r(N) " extrapolated obs missing (insufficient county history)"
    }

    drop b_cons b_year n_obs
} // END VAR LOOP

** Save file
compress
save "${data}working/dol_childcare", replace


//--------------------------------------------------
// County-Level Property Tax Rates from ACS Data
//--------------------------------------------------

** Load slim ACS property-tax subset (8 columns from 01e_acs.do, ~200 MB vs 4.3 GB)
use "${data}working/acs_proptx_slim", clear

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
** each bracket so we can compute a continuous tax rate. The bracket edges
** below are read off the extract's own DDI value labels (2026-10):
**   https://usa.ipums.org/usa-action/variables/PROPTX99#codes_section
** (The previous mapping assumed $100 brackets from code 13 and $1,000
** brackets from code 70, one bracket early in each case; e.g. code 22 is
** $1,000-1,099, not $1,400-1,499, so taxes above ~$950 were overstated by
** about $400 per household.)
gen proptx_dollars = .

** Code 1: None
replace proptx_dollars = 0 if proptx99 == 1

** Code 2: $1-49 -> midpoint
replace proptx_dollars = 25 if proptx99 == 2

** Codes 3-21: $50 brackets, $50-99 ... $950-999
forvalues i = 3/21 {
    local lower = 50 + (`i' - 3) * 50
    replace proptx_dollars = `lower' + 24.5 if proptx99 == `i'
}

** Codes 22-56: $100 brackets, $1,000-1,099 ... $4,400-4,499
forvalues i = 22/56 {
    local lower = 1000 + (`i' - 22) * 100
    replace proptx_dollars = `lower' + 49.5 if proptx99 == `i'
}

** Code 57 is "$4500" in the 1990 samples only (never in ACS); codes 58-62
** are $100 brackets, $4,500-4,599 ... $4,900-4,999
replace proptx_dollars = 4500 if proptx99 == 57
forvalues i = 58/62 {
    local lower = 4500 + (`i' - 58) * 100
    replace proptx_dollars = `lower' + 49.5 if proptx99 == `i'
}

** Codes 63-68: two $500 brackets, then $1,000 brackets
replace proptx_dollars = 5249.5 if proptx99 == 63    // $5,000-5,499
replace proptx_dollars = 5749.5 if proptx99 == 64    // $5,500-5,999
replace proptx_dollars = 6499.5 if proptx99 == 65    // $6,000-6,999
replace proptx_dollars = 7499.5 if proptx99 == 66    // $7,000-7,999
replace proptx_dollars = 8499.5 if proptx99 == 67    // $8,000-8,999
replace proptx_dollars = 9499.5 if proptx99 == 68    // $9,000-9,999

** Code 69: $10,000-10,999 from 2018 on. In the 2012-2017 ACS samples it is
** the open-ended top code "$10,000+", so there the bracket midpoint is a
** lower-bound-plus-$500 approximation.
replace proptx_dollars = 10499.5 if proptx99 == 69

** Codes 70-158: $1,000 brackets, $11,000-11,999 ... $99,000-99,999
forvalues i = 70/158 {
    local lower = 11000 + (`i' - 70) * 1000
    replace proptx_dollars = `lower' + 499.5 if proptx99 == `i'
}

** Code 159: $100,000+ (2018-onward samples; open-ended, lower bound used)
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

** Sanity check: HHWT must be fractional for analytic weighting
qui summ hhwt
assert r(min) > 0
di as text "HHWT range: " %9.4f r(min) " to " %9.4f r(max) ///
   ", mean: " %9.4f r(mean)

********************************************************************************
** VERSION 1: Overall (all observations)
********************************************************************************
preserve

** Collapse to county X year, weighted by household expansion weight
collapse (mean) prop_rate_mean = prop_rate ///
         (count) prop_rate_n = prop_rate ///
         [aw = hhwt], by(year fips)

** Label variables
label var prop_rate_mean "Mean property tax rate (% of home value)"
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
order year fips state_fips county_fips state_name county_name prop_rate_mean prop_rate_n

** Sort
sort fips year

** Save overall version
compress
save "${data}working/property_tax_rates_overall", replace

restore

********************************************************************************
** VERSION 2: Excluding allocated values (qprotx99 != 4, qvalueh != 4)
********************************************************************************

** Drop observations where values are allocated (quality flag == 4)
drop if qprotx99 == 4
drop if qvalueh == 4

** Collapse to county X year, weighted by household expansion weight
collapse (mean) prop_rate_mean = prop_rate ///
         (count) prop_rate_n = prop_rate ///
         [aw = hhwt], by(year fips)

** Label variables
label var prop_rate_mean "Mean property tax rate (% of home value, excl. allocated)"
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
order year fips state_fips county_fips state_name county_name prop_rate_mean prop_rate_n

** Sort
sort fips year

** Save version excluding allocated
compress
save "${data}working/property_tax_rates_excl_allocated", replace

** Display summary
dis "Property tax rate calculation complete."
dis "Overall version saved to: ${data}working/property_tax_rates_overall.dta"
dis "Excluding allocated saved to: ${data}working/property_tax_rates_excl_allocated.dta"

clear
