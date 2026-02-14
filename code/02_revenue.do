/*******************************************************************************
File Name: 		02_revenue.do
Creator: 		John Iselin
Date Update:	February 2026

Purpose: 	Revenue effects of tax-induced migration from Multnomah County's
			Preschool for All (PFA) income tax. Builds a microsimulation tax
			model using 2019 ACS microdata, calibrates to IRS administrative
			totals, computes baseline PFA and Oregon income tax revenue, and
			simulates revenue loss from tax-induced out-migration via Monte Carlo.

Called by: 	00_multnomah.do

Authors: John Iselin

For more information, contact john.iselin@yale.edu
*******************************************************************************/

********************************************************************************
** SECTION 0: Setup & Parameters
********************************************************************************

** Start log file
capture log close log_02rev
log using "${logs}02_log_revenue_${date}", name(log_02rev) replace text

** Parameters
scalar effect_agi = 0.02			// estimated net out-migration effect on AGI
scalar effect_agi_oregon = 0.02		// Oregon-level effect
local  cpi_2019_to_2022 = 1.136	// CPI-U inflation factor 2019→2022

** PFA tax brackets (2022)
local pfa_thresh1_single = 125000
local pfa_thresh2_single = 250000
local pfa_thresh1_joint  = 200000
local pfa_thresh2_joint  = 400000
local pfa_rate = 0.015

** Create output directory
capture mkdir "${results}revenue"

********************************************************************************
** SECTION 0B: SDID Estimation of Migration Effects
********************************************************************************

dis ""
dis "=============================================="
dis "Section 0B: SDID estimation of migration effects"
dis "=============================================="

** Check if SDID panel data exists
capture confirm file "${data}working/sdid_analysis_data.dta"
if _rc == 0 {

	** Save current data state
	preserve

	** Load SDID panel data
	use "${data}working/sdid_analysis_data.dta", clear

	** Keep required variables 
	keep fips year Treated irs_sample_1 sample_all population per_capita_income agi_net_rate_irs agi_net_rate_irs5 
	
	** ---- Run 1: effect_agi (county-level, domestic type 3) ----
	dis "Running SDID for effect_agi (agi_net_rate_irs)..."
	capture noisily sdid agi_net_rate_irs fips year Treated ///
		if irs_sample_1 == 1 & sample_all == 1 & year != 2020, ///
		vce(noinference)  ///
		covariates(population per_capita_income, projected)

	if _rc == 0 {
		** Extract ATT from e(ATT) — tau is in percentage points
		scalar tau_agi = e(ATT)
		scalar effect_agi = abs(tau_agi) / 100
		dis "  SDID effect_agi: tau = " %8.4f tau_agi " pp -> effect = " %8.4f effect_agi
	}
	else {
		dis "  SDID for effect_agi failed, using default: " %8.4f effect_agi
	}

	** ---- Run 2: effect_agi_oregon (state-level, interstate type 5) ----
	dis "Running SDID for effect_agi_oregon (agi_net_rate_irs5)..."
	capture noisily sdid agi_net_rate_irs5 fips year Treated ///
		if irs_sample_1 == 1 & sample_all == 1 & year != 2020, ///
		vce(noinference)  ///
		covariates(population per_capita_income, projected)

	if _rc == 0 {
		scalar tau_agi_oregon = e(ATT)
		scalar effect_agi_oregon = abs(tau_agi_oregon) / 100
		dis "  SDID effect_agi_oregon: tau = " %8.4f tau_agi_oregon " pp -> effect = " %8.4f effect_agi_oregon
	}
	else {
		dis "  SDID for effect_agi_oregon failed, using default: " %8.4f effect_agi_oregon
	}

	** Restore original data state
	restore
}
else {
	dis "  SDID panel data not found. Using default effects."
}

dis "  Final parameters:"
dis "    effect_agi         = " %8.4f effect_agi
dis "    effect_agi_oregon  = " %8.4f effect_agi_oregon

********************************************************************************
** SECTION 1: Load 2019 ACS Microdata for Multnomah County
********************************************************************************

dis ""
dis "=============================================="
dis "Section 1: Load ACS 2019 microdata"
dis "=============================================="

import delimited "${data}acs/acs_2019.csv", clear

** Filter to Multnomah County, Oregon
keep if statefip == 41 & countyfip == 51

** Drop group quarters
drop if gq > 2

** Keep ages 18+ (tax-filing-relevant population)
keep if age >= 18

** Handle top-coded / missing income
foreach v of varlist inctot incwage incbus00 incinvst incearn {
	replace `v' = . if `v' == 9999999
}

** Create household ID
gen double hh_id = serial

** Summarize
dis "ACS 2019 Multnomah County observations (18+, non-GQ): " _N

********************************************************************************
** SECTION 2: Create Tax Units
********************************************************************************

dis ""
dis "=============================================="
dis "Section 2: Create tax units"
dis "=============================================="

** -------------------------------------------------------------------------
** (a) Link married couples via SPLOC
** -------------------------------------------------------------------------

gen unit_id = pernum
replace unit_id = sploc if marst == 1 & sploc != 0 & pernum > sploc
label var unit_id "Unique ID for tax units"

** Count of individuals per tax unit
bysort hh_id unit_id: gen byte unit_ct = _N

** -------------------------------------------------------------------------
** (b) Filing status
** -------------------------------------------------------------------------

gen byte married = inlist(marst, 1, 2)		// married, spouse present or absent
gen byte mfs = (marst == 3 & sploc == 0)	// married filing separately

gen byte filing_status = 1					// single (default)
replace filing_status = 2 if married == 1	// MFJ
replace filing_status = 6 if mfs == 1		// MFS
label var filing_status "Filing status (1=single, 2=MFJ, 6=MFS)"

** -------------------------------------------------------------------------
** (c) Dependents (simplified — use NCHILD, capped at 3)
** -------------------------------------------------------------------------

gen byte depx = min(nchild, 3)
label var depx "Dependent exemptions (capped at 3)"

** -------------------------------------------------------------------------
** (d) Income variable construction (at tax-unit level)
** -------------------------------------------------------------------------

** Nominal income variables
gen double incwage_nom = max(incwage, 0)
gen double incse_nom = incearn - incwage			// self-employment
gen double incinvst_nom = incinvst					// investment (can be negative)
gen double inctot_nom = inctot
gen double incwel_nom = 0
replace incwel_nom = incwelfr if !missing(incwelfr) & incwelfr != 999999

** Subtract untaxed welfare income from total income (floor at 0)
replace inctot_nom = max(inctot_nom - incwel_nom, 0)

** Tax-unit aggregation
foreach v in inctot incwage incse incinvst {
	replace `v'_nom = 0 if `v'_nom == .  
	bysort hh_id unit_id: egen double `v'_tax = total(`v'_nom)
}

** -------------------------------------------------------------------------
** (e) Primary filer flag
** -------------------------------------------------------------------------

gen byte primary_filer = (unit_id == pernum)
label var primary_filer "Primary filer in tax unit"

** -------------------------------------------------------------------------
** (f) Compute tax-unit AGI proxy
** -------------------------------------------------------------------------

gen double agi_proxy = inctot_tax
label var agi_proxy "Tax-unit AGI proxy (total income)"

** Summary
dis "Number of tax units: "
count if primary_filer == 1

********************************************************************************
** SECTION 3: Load IRS County Data for Raking Targets
********************************************************************************

dis ""
dis "=============================================="
dis "Section 3: Load IRS county data for raking"
dis "=============================================="

** Save ACS data
tempfile acs_data
save `acs_data'

** Import 2019 IRS county data
import delimited "${data}irs/19incyallagi.csv", clear

** Keep Multnomah County
keep if statefips == 41 & countyfips == 51

** Rename variables (following 01_clean_data.do pattern)
rename n1 irs_n1
rename mars2 irs_mars2
rename a00100 irs_agi
rename a00200 irs_wages
rename n04470 irs_n_itemizers
rename a04470 irs_itemded

** Rescale (IRS reports in thousands)
replace irs_agi = irs_agi * 1000
replace irs_wages = irs_wages * 1000
replace irs_itemded = irs_itemded * 1000

** Keep relevant variables
keep agi_stub irs_n1 irs_mars2 irs_agi irs_wages irs_n_itemizers irs_itemded

** Drop the "all" stub (agi_stub == 0) for bracket-level calibration
drop if agi_stub == 0

** Label AGI stubs
label define lb_agi_stub 1 "Under $1" 2 "$1-$10k" 3 "$10k-$25k" ///
	4 "$25k-$50k" 5 "$50k-$75k" 6 "$75k-$100k" 7 "$100k-$200k" 8 "$200k+"
label values agi_stub lb_agi_stub

** Display IRS targets
list agi_stub irs_n1 irs_mars2 irs_agi irs_wages, sep(0)

** Total Multnomah County AGI (sum across brackets)
qui summ irs_agi
scalar total_irs_agi_2019 = r(sum)
dis "Total Multnomah County AGI (2019): $" %15.0fc total_irs_agi_2019

** Save as tempfile
tempfile irs_targets
save `irs_targets'

********************************************************************************
** SECTION 4: Raking / Calibration
********************************************************************************

dis ""
dis "=============================================="
dis "Section 4: Raking / calibration"
dis "=============================================="

** Reload ACS data
use `acs_data', clear

** -------------------------------------------------------------------------
** (a) Create AGI brackets matching IRS stubs
** -------------------------------------------------------------------------

gen byte agi_stub = .
replace agi_stub = 1 if agi_proxy < 1
replace agi_stub = 2 if agi_proxy >= 1     & agi_proxy < 10000
replace agi_stub = 3 if agi_proxy >= 10000 & agi_proxy < 25000
replace agi_stub = 4 if agi_proxy >= 25000 & agi_proxy < 50000
replace agi_stub = 5 if agi_proxy >= 50000 & agi_proxy < 75000
replace agi_stub = 6 if agi_proxy >= 75000 & agi_proxy < 100000
replace agi_stub = 7 if agi_proxy >= 100000 & agi_proxy < 200000
replace agi_stub = 8 if agi_proxy >= 200000

label values agi_stub lb_agi_stub

** -------------------------------------------------------------------------
** (b) GREG calibration (weights match IRS counts + AGI + wages by bracket)
** -------------------------------------------------------------------------

** Keep only primary filers for calibration
keep if primary_filer == 1

** Merge IRS targets
merge m:1 agi_stub using `irs_targets', keep(master match) nogen

gen byte is_mfj = (filing_status == 2)

** Create explicit auxiliary variables for calibration (32 constraints)
forvalues s = 1/8 {
	gen byte d_stub_`s' = (agi_stub == `s')
	gen byte d_mfj_`s' = (agi_stub == `s') * is_mfj
	gen double agi_stub_`s' = agi_proxy * (agi_stub == `s')
	gen double wages_stub_`s' = incwage_tax * (agi_stub == `s')
}

** Build population totals matrix (1 × 32)
matrix pop_totals = J(1, 32, .)
local cnames ""
forvalues s = 1/8 {
	qui sum irs_n1 if agi_stub == `s', meanonly
	matrix pop_totals[1, `s'] = r(mean)
	local cnames "`cnames' d_stub_`s'"
}
forvalues s = 1/8 {
	qui sum irs_mars2 if agi_stub == `s', meanonly
	matrix pop_totals[1, `=8+`s''] = r(mean)
	local cnames "`cnames' d_mfj_`s'"
}
forvalues s = 1/8 {
	qui sum irs_agi if agi_stub == `s', meanonly
	matrix pop_totals[1, `=16+`s''] = r(mean)
	local cnames "`cnames' agi_stub_`s'"
}
forvalues s = 1/8 {
	qui sum irs_wages if agi_stub == `s', meanonly
	matrix pop_totals[1, `=24+`s''] = r(mean)
	local cnames "`cnames' wages_stub_`s'"
}
matrix colnames pop_totals = `cnames'

** GREG calibration (Stata 15+ svycal)
svycal regress d_stub_1-d_stub_8 d_mfj_1-d_mfj_8 ///
	agi_stub_1-agi_stub_8 wages_stub_1-wages_stub_8 ///
	[pw = perwt], generate(cal_wt) totals(pop_totals) ll(0)
label var cal_wt "Calibrated weight (GREG: IRS counts + AGI + wages)"

** Clean up auxiliary variables
drop d_stub_* d_mfj_* agi_stub_* wages_stub_*

** -------------------------------------------------------------------------
** (c) Itemizer assignment
** -------------------------------------------------------------------------

** Within each AGI bracket, randomly assign itemizer status
gen double u_item = runiform()
bysort agi_stub (u_item): gen double cumshare = _n / _N

** Compute itemizer share
gen double item_share = irs_n_itemizers / irs_n1
replace item_share = min(item_share, 1)		// cap at 100%

** Assign itemizer status (top fraction within bracket)
gen byte itemizer = (cumshare > (1 - item_share))

** Assign average itemized deduction amount
gen double itemded_amt = 0
replace itemded_amt = (irs_itemded / irs_n_itemizers) if itemizer == 1 & irs_n_itemizers > 0
label var itemizer "Assigned as itemizer (matched to IRS)"
label var itemded_amt "Itemized deduction amount"

drop u_item cumshare

** -------------------------------------------------------------------------
** (d) Verification: compare calibrated totals to IRS
** -------------------------------------------------------------------------

dis ""
dis "GREG calibration verification: ACS calibrated vs IRS targets"
dis "-------------------------------------------------------------"

gen byte _mfj_temp = (filing_status == 2)
forvalues s = 1/8 {
	qui summ cal_wt if agi_stub == `s'
	local acs_n = r(sum)
	qui summ irs_n1 if agi_stub == `s', meanonly
	local irs_n = r(mean)

	** MFJ check
	qui summ _mfj_temp [aw=cal_wt] if agi_stub == `s'
	local acs_mfj = r(sum_w) * r(mean)
	qui summ irs_mars2 if agi_stub == `s', meanonly
	local irs_mfj = r(mean)

	** AGI check (calibrated)
	qui summ agi_proxy [aw=cal_wt] if agi_stub == `s'
	local acs_agi_sum = r(sum_w) * r(mean)
	qui summ irs_agi if agi_stub == `s', meanonly
	local irs_agi_val = r(mean)

	** Wages check (calibrated)
	qui summ incwage_tax [aw=cal_wt] if agi_stub == `s'
	local acs_wages_sum = r(sum_w) * r(mean)
	qui summ irs_wages if agi_stub == `s', meanonly
	local irs_wages_val = r(mean)

	dis "Stub `s': N=" %10.0f `acs_n' " vs " %10.0f `irs_n' ///
		"  |  MFJ=" %8.0f `acs_mfj' " vs " %8.0f `irs_mfj' ///
		"  |  AGI=" %14.0f `acs_agi_sum' " (IRS " %14.0f `irs_agi_val' ")" ///
		"  |  Wages=" %14.0f `acs_wages_sum' " (IRS " %14.0f `irs_wages_val' ")"
}
drop _mfj_temp

********************************************************************************
** SECTION 5: Inflate to 2022
********************************************************************************

dis ""
dis "=============================================="
dis "Section 5: Inflate incomes to 2022 dollars"
dis "=============================================="

** Apply CPI inflation factor to all dollar amounts
foreach v of varlist incwage_nom incwage_tax incse_nom incse_tax ///
	incinvst_nom incinvst_tax inctot_nom inctot_tax agi_proxy ///
	itemded_amt incwel_nom {
	replace `v' = `v' * `cpi_2019_to_2022'
}

** Set year to 2022
replace year = 2022

dis "Income inflated from 2019 to 2022 using CPI factor: `cpi_2019_to_2022'"

********************************************************************************
** SECTION 6: TAXSIM Calculation
********************************************************************************

dis ""
dis "=============================================="
dis "Section 6: TAXSIM calculation"
dis "=============================================="

** -------------------------------------------------------------------------
** (a) Prepare TAXSIM input variables
** -------------------------------------------------------------------------

** State: Oregon FIPS 41 → SOI 38
gen state = 38

** Unique tax unit ID
sort hh_id unit_id pernum
gen double taxsimid = _n
label var taxsimid "TAXSIM tax unit ID"

** Marital status
gen byte mstat = filing_status
label var mstat "TAXSIM marital status"

** Primary taxpayer age
gen page = age
label var page "TAXSIM primary taxpayer age"

** Spouse age
gen sage = 0
bysort hh_id unit_id (pernum): gen tmp_max_age = age[_N]
bysort hh_id unit_id (pernum): gen tmp_min_age = age[1]
replace sage = tmp_max_age if age == tmp_min_age & married == 1 & unit_ct > 1
replace sage = tmp_min_age if age == tmp_max_age & married == 1 & unit_ct > 1
drop tmp_max_age tmp_min_age
label var sage "TAXSIM spouse age"

** Primary wages (own wage income, non-negative)
gen double pwages = max(incwage_nom, 0)
label var pwages "TAXSIM primary wages"

** Spousal wages (tax unit wages minus own wages, non-negative)
gen double swages = max(incwage_tax - incwage_nom, 0)
label var swages "TAXSIM spouse wages"

** Primary self-employment income
gen double psemp = incse_nom
label var psemp "TAXSIM primary self-employment"

** Spousal self-employment income
gen double ssemp = incse_tax - incse_nom
label var ssemp "TAXSIM spouse self-employment"

** Interest/dividend income (investment income, non-negative)
gen double intrec = max(incinvst_tax, 0)
label var intrec "TAXSIM interest/dividend income"

** Other property income (residual)
gen double otherprop = inctot_tax
replace otherprop = otherprop - max(incwage_tax, 0)	// wages
replace otherprop = otherprop - incse_tax				// SE income
replace otherprop = otherprop - incinvst_tax			// investment
replace otherprop = otherprop - incwel_nom				// welfare (non-taxable)
replace otherprop = max(otherprop, 0)					// floor at zero
label var otherprop "TAXSIM other property income"

** Itemization control
gen byte idtl = 0					// default: use larger of standard/itemized
label var idtl "TAXSIM itemization control"

** Other itemized deductions for TAXSIM
gen double otheritem = itemded_amt
label var otheritem "TAXSIM other itemized deductions"

** -------------------------------------------------------------------------
** (b) Run TAXSIM
** -------------------------------------------------------------------------

** Save full data before TAXSIM
tempfile pre_taxsim
save `pre_taxsim'

** Keep TAXSIM input variables
keep taxsimid year state mstat depx page sage pwages swages ///
	psemp ssemp intrec otherprop idtl otheritem

** Order for TAXSIM
order taxsimid year state mstat depx page sage pwages swages ///
	psemp ssemp intrec otherprop idtl otheritem

** Run TAXSIM locally
cd "${data}working"
taxsimlocal35, full replace

if _rc != 0 {
	di as error "TAXSIM failed — check installation"
	cd "${dir}"
	use `pre_taxsim', clear

	** Fallback: estimate taxes without TAXSIM
	dis "Using simplified tax calculator as fallback"

	** Approximate Oregon state income tax (simplified progressive schedule)
	** 2022 Oregon brackets (single): 5% up to $3,750, 7% $3,750-$9,450,
	** 9% $9,450-$125k, 9.9% above $125k
	gen double taxable_income = max(agi_proxy - cond(mstat == 2, 25900, 12950), 0)
	gen double siitax = 0.05 * min(taxable_income, 3750) ///
		+ 0.07 * max(min(taxable_income, 9450) - 3750, 0) ///
		+ 0.09 * max(min(taxable_income, 125000) - 9450, 0) ///
		+ 0.099 * max(taxable_income - 125000, 0)
	gen double fiitax = 0	// placeholder
}
else {
	** Load TAXSIM results
	clear
	import delimited results.raw, clear

	cd "${dir}"

	** Clean results
	destring taxsimid, replace force
	drop if missing(taxsimid)

	** Get State taxable income 
	rename v36 taxable_income

	keep taxsimid fiitax siitax taxable_income

	** Save TAXSIM results
	tempfile taxsim_results
	save `taxsim_results'

	** Reload pre-TAXSIM data and merge
	use `pre_taxsim', clear
	merge 1:1 taxsimid using `taxsim_results', keep(master match) nogen
}

** Label tax variables
label var fiitax "Federal income tax (TAXSIM)"
label var siitax "Oregon state income tax (TAXSIM)"
label var taxable_income "Oregon taxable income (TAXSIM)"

** Verification
dis ""
dis "TAXSIM sanity checks:"
summ siitax [aw=cal_wt], detail
summ fiitax [aw=cal_wt], detail
summ taxable_income [aw=cal_wt], detail

********************************************************************************
** SECTION 7: Multnomah PFA Tax Calculator
********************************************************************************

dis ""
dis "=============================================="
dis "Section 7: PFA tax calculation"
dis "=============================================="

** Thresholds depend on filing status
gen double pfa_thresh1 = cond(mstat == 2, `pfa_thresh1_joint', `pfa_thresh1_single')
gen double pfa_thresh2 = cond(mstat == 2, `pfa_thresh2_joint', `pfa_thresh2_single')

** PFA tax = 1.5% on (taxinc - thresh1) + additional 1.5% on (taxinc - thresh2)
gen double pfa_tax = `pfa_rate' * max(taxable_income - pfa_thresh1, 0) ///
	+ `pfa_rate' * max(taxable_income - pfa_thresh2, 0)
label var pfa_tax "PFA tax liability"

** Summary
dis "PFA tax distribution:"
summ pfa_tax [aw=cal_wt], detail
summ pfa_tax [aw=cal_wt] if pfa_tax > 0, detail

********************************************************************************
** SECTION 8: Baseline Revenue
********************************************************************************

dis ""
dis "=============================================="
dis "Section 8: Baseline revenue"
dis "=============================================="

** Baseline PFA revenue
gen double wtd_pfa = pfa_tax * cal_wt
qui summ wtd_pfa
scalar baseline_pfa_revenue = r(sum)
dis "Baseline PFA revenue: $" %15.0fc baseline_pfa_revenue

** Baseline Oregon state income tax
gen double wtd_siitax = siitax * cal_wt
qui summ wtd_siitax
scalar baseline_state_revenue = r(sum)
dis "Baseline Oregon state income tax revenue: $" %15.0fc baseline_state_revenue

** Flag impacted (subject to PFA tax)
gen byte impacted = (taxable_income > pfa_thresh1)
label var impacted "Subject to PFA tax"

** Count impacted
qui count if impacted == 1
dis "Number of impacted tax units: " r(N)
qui summ cal_wt if impacted == 1
dis "Weighted number of impacted filers: " %10.0fc r(sum)

** Save working data
tempfile revenue_data
save `revenue_data'
clear 

********************************************************************************
** SECTION 9: Migration Revenue Effect 
********************************************************************************

dis ""
dis "=============================================="
dis "Section 9: Migration Revenue Effects"
dis "=============================================="

** -------------------------------------------------------------------------
** (a) Compute X (AGI loss from migration effect)
** -------------------------------------------------------------------------

** Load IRS gross migration files for Multnomah County
** Average net out-migration AGI across pre-treatment years (2017-2020)

tempfile flow_data
local first_flow = 1

foreach yr in 1718 1819 1920 {

	** Outflow: Multnomah = origin (state 41, county 51)
	import delimited "${data}irs/countyoutflow`yr'.csv", clear
	keep if y1_statefips == 41 & y1_countyfips == 51
	keep if y2_statefips == 97 & inlist(y2_countyfips, 0, 3) 
	
	gen double out_agi = agi * 1000		// IRS reports in thousands
	gen str5 flow_year = "`yr'"
	keep y2_countyfips flow_year out_agi
	rename y2_countyfips state
	tempfile out_`yr'
	save `out_`yr''

	** Inflow: Multnomah = destination (state 41, county 51)
	import delimited "${data}irs/countyinflow`yr'.csv", clear
	keep if y2_statefips == 41 & y2_countyfips == 51
	keep if y1_statefips == 97 & inlist(y1_countyfips, 0, 3) 

	gen double in_agi = agi * 1000
	gen str5 flow_year = "`yr'"
	keep y1_countyfips flow_year in_agi
	rename y1_countyfips state
	
	** Merge with outflow
	merge 1:1 flow_year state using `out_`yr'', nogen

	if `first_flow' == 1 {
		save `flow_data'
		local first_flow = 0
	}
	else {
		append using `flow_data'
		save `flow_data', replace
	}
}

** Compute net out-migration AGI
gen double net_outmig_agi = out_agi - in_agi

replace state = 1 if state == 0 
replace state = 2 if state == 3

dis ""
dis "IRS gross migration flows for Multnomah County (pre-treatment):"
list flow_year state out_agi in_agi net_outmig_agi, sep(0)

** Compute X using AGI stock (not flows)
** SDID coefficient = change in (net_AGI_migration / total_AGI),
** so AGI loss = coefficient * total_AGI_stock
scalar total_agi_2022 = total_irs_agi_2019 * `cpi_2019_to_2022'
dis ""
dis "Total Multnomah County AGI (2019): $" %15.0fc total_irs_agi_2019
dis "Total Multnomah County AGI (2022 $): $" %15.0fc total_agi_2022

scalar X_1 = effect_agi * total_agi_2022
dis "AGI loss from overall migration effect (X_1): $" %15.0fc X_1

scalar X_2 = effect_agi_oregon * total_agi_2022
dis "AGI loss from out-of-state migration effect (X_2): $" %15.0fc X_2


** -------------------------------------------------------------------------
** (b) Compute out-migration probability
** -------------------------------------------------------------------------

** Reload revenue data
use `revenue_data', clear

drop if cal_wt == 0 

** Total AGI of impacted tax units
qui summ agi_proxy [aw=cal_wt] if impacted == 1
scalar agi_impacted = r(sum_w) * r(mean)
dis "Total AGI of impacted filers: $" %15.0fc agi_impacted

** p = probability of out-migration for impacted units
scalar p_migrate = X_1 / agi_impacted
dis "Migration probability (p): " %8.6f p_migrate

** p = probability of out-of-state-migration for impacted units
scalar p_migrate_state = X_2 / agi_impacted
dis "Out-of-State Migration probability (p): " %8.6f p_migrate_state

********************************************************************************
** SECTION 10: Oregon and Multnomah State Revenue Effect
********************************************************************************

dis ""
dis "=============================================="
dis "Section 10: Oregon state revenue effect"
dis "=============================================="

** Average state tax rate on impacted residents
qui summ siitax [aw=cal_wt] if impacted == 1
scalar total_state_tax_impacted = r(sum_w) * r(mean)

qui summ agi_proxy [aw=cal_wt] if impacted == 1
scalar total_agi_impacted = r(sum_w) * r(mean)

scalar avg_state_rate = total_state_tax_impacted / total_agi_impacted
dis "Average effective state tax rate on impacted: " %6.4f avg_state_rate

** Oregon revenue loss from departing AGI
scalar oregon_revenue_loss = avg_state_rate * X_2
dis "Oregon revenue loss from migration effect: $" %15.0fc oregon_revenue_loss

dis ""
dis "=============================================="
dis "Section 10: Multnomah state revenue effect"
dis "=============================================="

** Average state tax rate on impacted residents
qui summ pfa_tax [aw=cal_wt] if impacted == 1
scalar total_mt_tax_impacted = r(sum_w) * r(mean)

qui summ agi_proxy [aw=cal_wt] if impacted == 1
scalar total_agi_impacted = r(sum_w) * r(mean)

scalar avg_mt_rate = total_mt_tax_impacted / total_agi_impacted
dis "Average effective MT tax rate on impacted: " %6.4f avg_mt_rate

** Oregon revenue loss from departing AGI
scalar mt_revenue_loss = avg_mt_rate * X_1
dis "Multnomah county revenue loss from migration effect: $" %15.0fc mt_revenue_loss

********************************************************************************
** SECTION 11: Output — Tables & Figures
********************************************************************************

dis ""
dis "=============================================="
dis "Section 11: Output tables and figures"
dis "=============================================="

** -------------------------------------------------------------------------
** (a) Summary table
** -------------------------------------------------------------------------

** Display summary
dis ""
dis "=================================================================="
dis "REVENUE IMPACT SUMMARY"
dis "=================================================================="
dis ""
dis "Baseline PFA revenue:                    $" %15.0fc baseline_pfa_revenue
dis "Baseline Oregon state revenue:           $" %15.0fc baseline_state_revenue
dis ""
dis "Multnomah PFA revenue loss (analytical): $" %15.0fc mt_revenue_loss
dis "Oregon state revenue loss (analytical):  $" %15.0fc oregon_revenue_loss
dis "=================================================================="

** Export summary table to Excel
preserve
clear
set obs 4

gen str50 metric = ""
gen double value = .

replace metric = "Baseline PFA revenue"                    in 1
replace value = baseline_pfa_revenue                       in 1
replace metric = "Baseline Oregon state revenue"           in 2
replace value = baseline_state_revenue                     in 2
replace metric = "Multnomah PFA revenue loss (analytical)" in 3
replace value = mt_revenue_loss                            in 3
replace metric = "Oregon state revenue loss (analytical)"  in 4
replace value = oregon_revenue_loss                        in 4

export excel "${results}revenue/tbl_revenue_summary.xlsx", ///
	firstrow(variables) replace
restore

** -------------------------------------------------------------------------
** (b) Table of tax-unit-level statistics by AGI bracket
** -------------------------------------------------------------------------

** Collapse by AGI bracket
preserve

** Number of impacted filers
gen double wt_impacted = cal_wt if impacted == 1
gen double wt_pfa_tax = pfa_tax * cal_wt
gen double wt_agi = agi_proxy * cal_wt

collapse (sum) n_filers=cal_wt n_impacted=wt_impacted ///
	total_pfa=wt_pfa_tax total_agi=wt_agi, by(agi_stub)

** Average PFA tax
gen double avg_pfa = total_pfa / n_impacted if n_impacted > 0
replace avg_pfa = 0 if missing(avg_pfa)

** Revenue share
egen double total_pfa_all = total(total_pfa)
gen double pfa_share = total_pfa / total_pfa_all * 100

** Labels
label var n_filers "Weighted filers"
label var n_impacted "Weighted impacted filers"
label var avg_pfa "Average PFA tax ($)"
label var pfa_share "Share of PFA revenue (%)"

** Display
list agi_stub n_filers n_impacted avg_pfa total_pfa pfa_share, sep(0)

** Export
export excel "${results}revenue/tbl_pfa_by_bracket.xlsx", ///
	firstrow(variables) replace

restore

** -------------------------------------------------------------------------
** (d) Summary by filing status
** -------------------------------------------------------------------------

preserve

gen double wt_impacted = cal_wt if impacted == 1
gen double wt_pfa_tax = pfa_tax * cal_wt

collapse (sum) n_filers=cal_wt n_impacted=wt_impacted ///
	total_pfa=wt_pfa_tax, by(mstat)

** Label filing status
label define lb_mstat 1 "Single" 2 "MFJ" 6 "MFS"
label values mstat lb_mstat

list mstat n_filers n_impacted total_pfa, sep(0)

restore

** =========================================================================
** Save final dataset
** =========================================================================

save "${data}working/revenue_microsim.dta", replace

dis ""
dis "=============================================="
dis "02_revenue.do complete."
dis "Output files:"
dis "  ${results}revenue/tbl_revenue_summary.xlsx"
dis "  ${results}revenue/tbl_pfa_by_bracket.xlsx"
dis "  ${data}working/revenue_microsim.dta"
dis "=============================================="

capture log close log_02rev
