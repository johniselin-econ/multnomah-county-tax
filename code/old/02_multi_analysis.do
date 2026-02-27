/*******************************************************************************
File Name: 		02_multi_analysis.do
Creator: 		John Iselin
Date Update:	December 26, 2025

Called by: 00_multnomah.do

Purpose: Preform multinomial Logit Analysis. 

Authors: John Iselin 

For more information, contact john.iselin@yale.edu

*******************************************************************************/

** Start log file 
capture log close log_02
log using "${logs}02_log_multi_${date}", replace text name(log_02)

** Preliminary Steps
** (1) Define list of counties (using averages from 2016-2019)

** Load ACS Data 
use "${data}working/acs_migration_file", replace

** Sample restrictions
keep if inrange(year, 2016, 2019)		// Sample: 2016-2019
drop if qmigplc1 == 4					// Error in migration place 
drop if inlist(state_fips_o, 2, 15)   	// Alaska and Hawaii
drop if inlist(state_fips_d, 2, 15)   	// Alaska and Hawaii
drop if ftotinc < 0 					// Negative Family Income 	
drop if county_fips_d == 0 | county_fips_o == 0 // Cannot identify counties 

** Keep required variables
keep year fips_d state_fips_d county_fips_d fips_o state_fips_o county_fips_o perwt 

** Define multnomah variables 
gen multnomah_o = (state_fips_o == 41 & county_fips_o == 51)
gen multnomah_d = (state_fips_d == 41 & county_fips_d == 51)

** Keep if in Multnomah county in origin or destination 
keep if multnomah_d == 1 | multnomah_o == 1 

** Collapse 
collapse (sum) perwt, by(fips_d state_fips_d county_fips_d fips_o state_fips_o county_fips_o )

** Define set of counties with majority of out-migration 
preserve 
keep if state_fips_o == 41 & county_fips_o == 51 
keep if fips_d != fips_o
drop *fips_o 
rename *fips_d *fips 
gen migrants_out = perwt 
keep fips state_fips county_fips migrants_out 

** Save as temporary dataset 
tempfile multnomah_out 
save `multnomah_out'
restore 

** Define set of counties with majority of in-migration 
keep if state_fips_d == 41 & county_fips_d == 51 
keep if fips_d != fips_o
drop *fips_d 
rename *fips_o *fips 
gen migrants_in = perwt 
keep fips state_fips county_fips migrants_in 

** Merge in out-migration 
merge 1:1 fips state_fips county_fips using `multnomah_out', gen(merge)

** Generate total number of migrants 
gen migrants_tot = migrants_in + migrants_out

** Loop over in and out migration 
foreach var of varlist migrants_in migrants_out migrants_tot {
	
	replace `var' = 0 if missing(`var')
	sort `var'
	gen `var'_sum = sum(`var')
	egen `var'_total = total(`var') 
	gen cum_`var' = `var'_sum / `var'_total 
	gen sample_`var' = cum_`var' > .20
	tab sample_`var'
	tab state_fips if sample_`var' == 1 
	
}

** Define cumulative sample 
gen sample_migrants = sample_migrants_in == 1 & sample_migrants_out == 1
table sample_migrants, stat(sum migrants_in migrants_out  )
keep if sample_migrants == 1 
drop sample_migrants_* 
keep *fips

** Save as state and county Ids 
merge 1:1 fips using  "${data}working/ids", keep(match) nogen 

** Order 
order fips state* county* 

** Save 
save "${data}working/acs_county_sample", replace 
export excel using "${data}working/acs_county_sample.xlsx", replace firstrow(variables)
clear 

** (2) Define after-tax income proxies (using 2015 ACS data)

** Get TAXSIM State codes 
import delimited "${data}taxsim_state_codes.csv", clear 

** Save as temporary data
tempfile taxsim_state_codes
save `taxsim_state_codes'
clear

** Load ACS Data 
use "${data}working/acs_migration_file", replace

** Store cpi adjustments 
forvalues y = 2015 / 2023 {
	
	qui summ cpi99 if year == `y'
	local cpi_`y' = r(mean)
	
} // END YEAR LOOP 

** Sample restrictions
keep if year == 2015					// Sample: 2015
drop if inlist(state_fips_d, 2, 15)   	// Alaska and Hawaii
drop if qinctot == 4 					// Imputed Income 

** Identifies spouses and create tax unit IDs 
gen num = pernum
replace num = sploc if sploc != 0 & sploc < pernum 
egen double taxsimid = group(serial num)

** Get self and spouse age 
bysort taxsimid: egen min_age = min(age)
bysort taxsimid: egen max_age = max(age)
gen page = age 
gen sage = 0 
replace sage = min_age if max_age == age & sploc != 0
replace sage = max_age if min_age == age & sploc != 0
drop max_age min_age 

** Get number of children using max of nchild 
bysort taxsimid: egen depx = max(nchild)

** Get total taxable personal income 
gen inctot_tax = inctot - incsupp - incwelfr
drop incsupp incwelfr

** Generate tax-unit-level income vars 
foreach var of varlist inctot incwage incearn incinvst incother {
	
	bysort taxsimid: egen unit_`var' = total(`var')
	
} // END INCOME LOOP 

** Generate taxsim income variables 
gen pwages = incwage
gen swages = unit_incwage - incwage
gen psemp = incearn - incwage 
gen ssemp = unit_incearn - unit_incwage - psemp
gen ltcg  = unit_incinvst
gen otherprop = unit_incother

** Define local income variables 
local inc_vars "pwages swages psemp ssemp ltcg otherprop" 

** Keep required variables
keep year taxsimid state_fips_d perwt		/// Basics 
	page sage depx									/// Demographics 
	`inc_vars'										/// Income variables
	age nchild marst educd							/// Categorical Variables 

** Rename variables 
rename *fips_d *fips

** Store state fips 
qui levelsof state_fips, local(fips)

** Age
recode age ///
    (18/24   = 1 "18-24") ///
    (25/44   = 2 "25-44") ///
    (45/64   = 3 "45-64") ///
    (65/max  = 4 "65+"), ///
    gen(cat_age)
label var cat_age "Age categories"
tab year cat_age, m 

** Marital status
recode marst ///
    (1       = 1 "Married") ///
    (2/3     = 2 "Separated") ///
    (4/5     = 3 "Divorced / Widowed") ///
    (6       = 4 "Single"), ///
    gen(cat_married)
label var cat_married "Marriage categories"
tab year cat_married, m 

** Kids at home
recode nchild ///
    (0       = 0 "0 Children") ///
    (1       = 1 "1 Child") ///
    (2       = 2 "2 Children") ///
    (3/max   = 3 "3+ Children"), ///
    gen(cat_child)
label var cat_child "Number of Children"
tab year cat_child, m 
 
** Education
recode educd ///
    (min/61  = 1 "Less than HS") ///
    (62/71   = 2 "HS Diploma") ///
    (80/100  = 3 "Some College") ///
    (101/max = 4 "College Degree"), ///
    gen(cat_educ)
label var cat_educ "Education categories"
tab year cat_educ, m 

** Collapse to state X education X age X marital status X kids level 
collapse (mean) `inc_vars' page sage depx [fw = perwt], by(cat_married cat_educ cat_child cat_age)

** Get round values 
replace depx = round(depx)
replace depx = 3 if depx > 3 
replace page = round(page)
replace sage = round(sage)
replace sage = 0 if cat_married != 1

** Update income variables 
replace swages = 0 if cat_married != 1
replace ssemp = 0 if cat_married != 1

** Generate year
gen year = . 
gen state_fips = . 

** Save as temporary file 
tempfile income_profiles 
save `income_profiles'
clear 

** Expand to all years 
forvalues y = 2016/2023 {
	
	** Expand to all states 
	foreach st of local fips {
	
		** Append data 
		append using `income_profiles' 
		
		** Update year 
		replace year = `y' if missing(year)
		
		** Update state 
		replace state_fips = `st' if missing(state_fips)
	
	} // END STATE LOOP 
		
	** Update income variables 
	foreach var of local inc_vars {
			
		replace `var' = `var' * `cpi_2015' / `cpi_`y'' if year == `y'
			
	} // END INC VAR LOOP 
	
} // END YEAR LOOP 

** Merge with IDS 
merge m:1 state_fips using "${data}working/state_ids", keep(match) nogen 

** Merge with TAXSIM codes 
merge m:1 state_name using `taxsim_state_codes', keep(match) nogen 
 
** Mstat 
gen mstat = 1 
replace mstat = 2 if cat_married == 1 
replace mstat = 6 if cat_married == 3

** Generate taxsimid 
egen double taxsimid = group(year state_fips cat_married cat_educ cat_child cat_age)
isid taxsimid 

** Organize data 
order taxsimid year state* cat_* depx page sage 

** Save 
save "${data}working/acs_income_profiles", replace 

** Prep for taxsim 
rename state_soi state
keep taxsimid state year mstat page sage depx `inc_vars'

** Run through TAXSIM by state and year 
cd "${data}working"
taxsimlocal35, full
cd ${dir}
clear 

** Merge back with data 
use "${data}working/acs_income_profiles", clear 
merge 1:1 taxsimid using "${data}working\taxsim_out.dta", keep(match) nogen 

** Keep relavent variables 
keep year state_fips state_name cat_married cat_educ cat_child cat_age 	///
	v10 fiitax siitax fica

** Rename taxim variables 
rename v10 agi 
rename fiitax tax_fed 
rename siitax tax_st 
rename fica tax_pay

** Save data 
save "${data}working/acs_income_profiles", replace 

** Rangejoin on list of counties in sample (dropping all others)
joinby state_fips using "${data}working/acs_county_sample"
tab state_name

** Add Multnomah County 
expand 2 if fips == 41005, gen(tag)
replace fips = 41051 if tag == 1
replace county_fips = 51 if tag == 1 
replace county_name = "Multnomah County" if tag == 1 
drop tag  
 
** Local taxes 
gen tax_local = 0 
tab state_name
 
/* 
** Methodology: Local Income taxes 
** If a state is not identified by the following Tax Foundation report, assume 0 
** https://taxfoundation.org/research/all/state/local-income-taxes-2023/
** (exception: SF county in California)

** (1) San Francisco 
replace tax_local = ... if fips == 6075	& year < 2021

** (2) NYC 
replace tax_local = ... if inlist(fips, 36047, 36061, 36081) 

** (3) Cuyahoga County, OH (Cleveland)
40109	40	Oklahoma	109	Oklahoma County
41005	41	Oregon	5	Clackamas County
41017	41	Oregon	17	Deschutes County
41029	41	Oregon	29	Jackson County
41039	41	Oregon	39	Lane County
41047	41	Oregon	47	Marion County
41067	41	Oregon	67	Washington County
48113	48	Texas	113	Dallas County
49035	49	Utah	35	Salt Lake County
53011	53	Washington	11	Clark County
53033	53	Washington	33	King County
53053	53	Washington	53	Pierce County
53061	53	Washington	61	Snohomish County
53063	53	Washington	63	Spokane County
53067	53	Washington	67	Thurston County
*/

** Define after-tax income 
gen after_tax_inc = agi - tax_fed - tax_st - tax_pay

** Save
save "${data}working/acs_income_profiles", replace  

** (3) Create county-by-year dataset with the following values
** 	- County centroids 
** 	- Population 
** 	- Unemployment 
** 	- Childcare costs 
use "${data}working/bls_unemployment", clear
merge m:1 fips using "${data}working/ids", keep(master match) nogen 
merge m:1 fips using "${data}working/pop_centers", keep(master match)  gen(merge_centr)
merge 1:1 year fips using "${data}working/bea_economics", keep(master match) gen(merge_bea)
merge 1:1 year fips using "${data}working/dol_childcare", keep(master match) gen(merge_cc)


** Merge with pool of counties 
merge m:1 fips using "${data}working/acs_county_sample", gen(merge_pool)

** Keep if in pool of counties 
keep if merge_pool == 3
drop merge_pool _merge 

** Organize data 
order year fips state* county* 
sort  fips state* county* year

** Check for data 
tab merge_centr
tab merge_bea
tab merge_cc
** Note missing DC data for ChildCare 
drop merge_centr merge_bea merge_cc

** Save
save "${data}working/acs_county_sample_demo", replace  
clear 

****************************************************
** Step 1: Construct dataset
****************************************************

** Load ACS Data 
use "${data}working/acs_migration_file", replace

** Sample restrictions
keep if inrange(year, 2016, 2019)		// Sample: 2016-2019
drop if qmigplc1 == 4					// Error in migration place 
drop if inlist(state_fips_o, 2, 15)   	// Alaska and Hawaii
drop if inlist(state_fips_d, 2, 15)   	// Alaska and Hawaii
drop if ftotinc < 0 					// Negative Family Income 	
drop if county_fips_d == 0 | county_fips_o == 0 // Cannot identify counties 

** Keep if in donor pool in both origin and destination 
foreach x in "o" "d" {
	
	** Merge 
	gen fips = fips_`x'
	merge m:1 fips using "${data}working/acs_county_sample", gen(merge_pool_`x')
	drop fips state_fips county_fips 
	rename state_name state_name_`x'
	rename county_name county_name_`x'
	
} // END ORIGIN DESTINATION LOOP 

** Show tabulation of merge 
tab merge_pool_d merge_pool_o
keep if merge_pool_d == 3 
keep if merge_pool_o == 3
drop merge_pool_d merge_pool_o

** Define multnomah variables 
gen multnomah_o = (state_fips_o == 41 & county_fips_o == 51)
gen multnomah_d = (state_fips_d == 41 & county_fips_d == 51)
label var multnomah_d "In Multnomah County in destination year"
label var multnomah_o "In Multnomah County in origin year"

** Generate unique id 
egen double id = group(year serial pernum)
isid id 
label var id "Unique person X year ID"

** Keep required variables 
keep year state_fips* fips* county_fips* 	///
	id perwt multnomah_* same_county 		/// 
	age nchild marst educd					/// Categorical Variables 

** Order 
order id year *_o *_d perwt same_county age nchild marst educd
	
	
** Construct demographic cells 
	
** Age
recode age ///
    (18/24   = 1 "18-24") ///
    (25/44   = 2 "25-44") ///
    (45/64   = 3 "45-64") ///
    (65/max  = 4 "65+"), ///
    gen(cat_age)
label var cat_age "Age categories"
tab year cat_age, m 

** Marital status
recode marst ///
    (1       = 1 "Married") ///
    (2/3     = 2 "Separated") ///
    (4/5     = 3 "Divorced / Widowed") ///
    (6       = 4 "Single"), ///
    gen(cat_married)
label var cat_married "Marriage categories"
tab year cat_married, m 

** Kids at home
recode nchild ///
    (0       = 0 "0 Children") ///
    (1       = 1 "1 Child") ///
    (2       = 2 "2 Children") ///
    (3/max   = 3 "3+ Children"), ///
    gen(cat_child)
label var cat_child "Number of Children"
tab year cat_child, m 
 
** Education
recode educd ///
    (min/61  = 1 "Less than HS") ///
    (62/71   = 2 "HS Diploma") ///
    (80/100  = 3 "Some College") ///
    (101/max = 4 "College Degree"), ///
    gen(cat_educ)
label var cat_educ "Education categories"
tab year cat_educ, m 

** Drop vars 
drop age marst nchild educd 

** Count number of unique counties 
egen unique = tag(fips_d)
qui count if unique == 1
local J = r(N)
drop unique 

** Load origin-county controls 
gen fips = fips_o 
merge m:1 fips year using "${data}working/acs_county_sample_demo", 		///
		keep(master match) gen(merge_county_o)
		
** Rename 
rename lat orig_lat
rename lon orig_lon	
rename unemp orig_unemp 
rename population orig_pop 
rename per_capita_income orig_inc 
rename mc* orig_mc*
rename mf* orig_mf* 		
drop sample_* 
drop state_fips county_fips 
rename state_name state_name_o 
rename county_name county_name_o 

** Organize data 
order id year *_o *_d

** Labels 
label var year "ACS sample year"
label var state_fips_o "Origin State (FIPS)"
label var state_name_o "Origin State (name)"
label var state_fips_d "Dest. State (FIPS)"
label var county_fips_o "Origin County (FIPS)"
label var county_name_o "Origin County (name)"
label var county_fips_d "Dest. County (FIPS)"
label var fips_o "Origin FIPS code "
label var fips_d "Dest. FIPS code"
label var perwt "Person weight"
label var same_county "Indicator for staying in same county"
label var orig_lat "Latitude (origin)"
label var orig_lon "Longitude (origin)"
label var orig_unemp "Unemployment rate (origin)"
label var orig_pop "County population (origin)"
label var orig_inc "Per-capita income (origin)"
label var orig_mc_infant_med "Infant center cost as a percent of median income (origin)"
label var orig_mc_toddler_med "Toddler center cost as a percent of median income (origin)"
label var orig_mc_preschool_med "Preschool center cost as a percent of median income (origin)"
label var orig_mf_infant_med "Infant family cost as a percent of median income (origin)"
label var orig_mf_toddler_med "Toddler family cost as a percent of median income (origin)"
label var orig_mf_preschool_med "Preschool family cost as a percent of median income (origin)"

** Merge with After-tax income calculations 
merge m:1 year fips cat_age cat_married cat_child cat_educ 	///
	using "${data}working/acs_income_profiles", 			///
	keep(match) nogen 

** Drop unnecc variables 
drop state_fips state_name county_fips county_name sample_migrants_in sample_migrants_out sample_migrants_tot sample_migrants_all _merge tax_fed tax_st tax_pay tax_local agi fips

** Rename variable 
rename after_tax_inc orig_after_tax_inc 
label var orig_after_tax_inc "After-tax income (USD) (origin)"

** Compress data 
compress 

** Save 
save "${data}working/person_year.dta", replace
clear 

** Load county pool data 
use "${data}working/acs_county_sample"

** Keep required variables 
keep fips state_fips county_fips

** Renaem 
rename fips alt_fips_d 
rename state_fips alt_state_fips_d
rename county_fips alt_county_fips_d 

** Tag IDs 
sort alt_fips_d
gen alt_id = _n 

** Save as working file 
tempfile alt 
save `alt'
clear 

** Create unique observation 
use "${data}working/person_year.dta", clear 

** Expand by J
expand `J'
bysort id: gen int alt_id = _n
merge m:1 alt_id using `alt', keep(match) nogen

* Choice indicators
gen byte chosen = (alt_fips_d == fips_d)

** Merge with origin and destination county-level controls
gen fips = alt_fips_d
merge m:1 fips year using "${data}working/acs_county_sample_demo", 		///
		keep(master match) gen(merge_alt_county_d)
		
** Rename 
rename lat alt_lat
rename lon alt_lon	
rename unemp alt_unemp 
rename population alt_pop 
rename per_capita_income alt_inc 
rename mc* alt_mc*
rename mf* alt_mf* 		
drop sample_* 
drop state_fips county_fips 
rename state_name alt_state_name_d
rename county_name alt_county_name_d 

** Labels 
label var alt_lat "Latitude (alt dest.)"
label var alt_lon "Longitude (alt dest.)"
label var alt_unemp "Unemployment rate (alt dest.)"
label var alt_pop "County population (alt dest.)"
label var alt_inc "Per-capita income (alt dest.)"
label var alt_mc_infant_med "Infant center cost as a percent of median income (alt dest.)"
label var alt_mc_toddler_med "Toddler center cost as a percent of median income (alt dest.)"
label var alt_mc_preschool_med "Preschool center cost as a percent of median income (alt dest.)"
label var alt_mf_infant_med "Infant family cost as a percent of median income (alt dest.)"
label var alt_mf_toddler_med "Toddler family cost as a percent of median income (alt dest.)"
label var alt_mf_preschool_med "Preschool family cost as a percent of median income (alt dest.)"

** Merge with After-tax income calculations 
merge m:1 year fips cat_age cat_married cat_child cat_educ 	///
	using "${data}working/acs_income_profiles", 			///
	keep(match) nogen 

** Drop unnecc variables 
drop fips state_fips state_name county_fips county_name sample_migrants_in sample_migrants_out sample_migrants_tot sample_migrants_all _merge tax_fed tax_st tax_pay tax_local agi

** Rename variable 
rename after_tax_inc alt_after_tax_inc 
label var orig_after_tax_inc "After-tax income (USD) (alt dest.)"

** Calculate distance 
egen unique = tag(fips_o alt_fips_d)
geodist orig_lat orig_lon alt_lat alt_lon if unique == 1, generate(tmp)
bysort fips_o alt_fips_d: egen distance = mean(tmp)
drop tmp 

tab distance if unique == 1 & same_county == 1 
             
** Compress data 
compress 

** Save 
save "${data}working/multi_dta.dta", replace

****************************************************
** Step 2: Estimate Model
****************************************************

cmset id alt_fips_d 

cmclogit chosen distance alt_after_tax_inc alt_unemp 							///
	XX INERACTIOONS [fw = perwt], 				///
	casevars(i.cat_age i.cat_married i.cat_educ i.cat_child i.year i.fips_o)	///
	vce(cluster fips_o)
	
	

** Close log
log close log_02
