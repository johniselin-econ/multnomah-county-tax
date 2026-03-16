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

** Load shared project defaults and helper programs
local cwd = subinstr("`c(pwd)'", "\", "/", .)
local suffix "/code/stata"
if "${dir}" == "" {
	if length("`cwd'") >= length("`suffix'") & ///
		substr("`cwd'", length("`cwd'") - length("`suffix'") + 1, .) == "`suffix'" {
		global dir = substr("`cwd'", 1, length("`cwd'") - length("`suffix'"))
	}
	else {
		global dir "`cwd'"
	}
}
if "${code}" == "" global code "${dir}/code/stata/"
do "${code}00_stata_config.do"

********************************************************************************
** SECTION 0: Setup & Parameters
********************************************************************************

** Start log file
capture log close log_02rev
log using "${logs}02_log_revenue_${date}", name(log_02rev) replace text

project_set_seed, context("02_revenue.do") offset(40)

** Parameters
scalar effect_agi = 0.02			// estimated net out-migration effect on AGI
scalar effect_agi_oregon = 0.02		// Oregon-level effect
local  cpi_2019_to_2022 = 1.136	// CPI-U inflation factor 2019→2022

** Actual revenue (for scaling simulated effects)
local actual_pfa_revenue = 187000000			// PFA actual collections
local actual_oregon_revenue = 11772886000		// Oregon individual income tax

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

** Load SDID estimates from stored results (produced by 02_sdid_analysis.do)
** Preferred specs mirror project_mark_preferred_main in 00_stata_config.do:
**   - IRS (16-22) × {sample_all, sample_stringency} × {domestic, out-of-state}
**   - ACS College (16-24) × {sample_all, sample_stringency} × {domestic, out-of-state}
** All with controls == 1 & exclusion == 1 (excl. 2020)
capture confirm file "${results}sdid/sdid_results.dta"
if _rc == 0 {
	project_assert_manifest using "${results}sdid/sdid_results_manifest.dta", ///
		artifact("sdid_results")

	preserve
	use "${results}sdid/sdid_results.dta", clear

	** ---- Preferred spec lookup ----
	** Parallel locals define 4 preferred specs per direction
	** Suffixes for scalar names
	local suf_1 "irs_all"
	local suf_2 "irs_string"
	local suf_3 "acs_col_all"
	local suf_4 "acs_col_string"

	** Domestic (type 3) — used for PFA county revenue loss
	local dom_sdata_1 "irs_full_16_22"
	local dom_sdata_2 "irs_full_16_22"
	local dom_sdata_3 "acs_16_24_col"
	local dom_sdata_4 "acs_16_24_col"
	local dom_ovar_1  "agi_net_rate_irs"
	local dom_ovar_2  "agi_net_rate_irs"
	local dom_ovar_3  "agi_net_rate_acs2"
	local dom_ovar_4  "agi_net_rate_acs2"

	** Out-of-state (type 5) — used for Oregon state revenue loss
	local out_sdata_1 "irs_outstate_full_16_22"
	local out_sdata_2 "irs_outstate_full_16_22"
	local out_sdata_3 "acs_outstate_16_24_col"
	local out_sdata_4 "acs_outstate_16_24_col"
	local out_ovar_1  "agi_net_rate_irs_outstate"
	local out_ovar_2  "agi_net_rate_irs_outstate"
	local out_ovar_3  "agi_net_rate_acs2_outstate"
	local out_ovar_4  "agi_net_rate_acs2_outstate"

	** Samples (shared across domestic and out-of-state)
	local samp_1 "sample_all"
	local samp_2 "sample_stringency"
	local samp_3 "sample_all"
	local samp_4 "sample_stringency"

	** Labels for display
	local lbl_1 "IRS, all counties"
	local lbl_2 "IRS, stringency match"
	local lbl_3 "ACS College, all counties"
	local lbl_4 "ACS College, stringency"

	** ---- Domestic preferred specs ----
	dis ""
	dis "  Preferred SDID estimates — domestic (type 3):"
	dis "  {hline 60}"
	forvalues i = 1/4 {
		qui summ tau if sample_data == "`dom_sdata_`i''" ///
			& sample == "`samp_`i''" ///
			& outcome == "`dom_ovar_`i''" ///
			& controls == 1 & exclusion == 1, meanonly
		if r(N) == 1 {
			scalar tau_dom_`suf_`i'' = r(mean)
			scalar effect_dom_`suf_`i'' = abs(r(mean)) / 100
			dis "    `lbl_`i'': tau = " %8.4f tau_dom_`suf_`i'' " pp, effect = " %8.6f effect_dom_`suf_`i''
		}
		else {
			dis as error "    `lbl_`i'': NOT FOUND in sdid_results.dta"
		}
	}

	** ---- Out-of-state preferred specs ----
	dis ""
	dis "  Preferred SDID estimates — out-of-state (type 5):"
	dis "  {hline 60}"
	forvalues i = 1/4 {
		qui summ tau if sample_data == "`out_sdata_`i''" ///
			& sample == "`samp_`i''" ///
			& outcome == "`out_ovar_`i''" ///
			& controls == 1 & exclusion == 1, meanonly
		if r(N) == 1 {
			scalar tau_out_`suf_`i'' = r(mean)
			scalar effect_out_`suf_`i'' = abs(r(mean)) / 100
			dis "    `lbl_`i'': tau = " %8.4f tau_out_`suf_`i'' " pp, effect = " %8.6f effect_out_`suf_`i''
		}
		else {
			dis as error "    `lbl_`i'': NOT FOUND in sdid_results.dta"
		}
	}

	restore

	** ---- Set primary scalars used by revenue calculations ----
	** Primary: IRS, all counties (most conservative / broadest sample)
	scalar effect_agi = effect_dom_irs_all
	scalar effect_agi_oregon = effect_out_irs_all
}
else {
	dis as error "WARNING: SDID results (sdid_results.dta) not found. Run 02_sdid_analysis.do first."
	dis as error "         Using hardcoded default effects. Results are PLACEHOLDERS."
}

dis ""
dis "  Primary parameters for revenue calculation:"
dis "    effect_agi         = " %8.6f effect_agi
dis "    effect_agi_oregon  = " %8.6f effect_agi_oregon

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

********************************************************************************
** (a) Link married couples via SPLOC
********************************************************************************

gen unit_id = pernum
replace unit_id = sploc if marst == 1 & sploc != 0 & pernum > sploc
label var unit_id "Unique ID for tax units"

** Count of individuals per tax unit
bysort hh_id unit_id: gen byte unit_ct = _N

********************************************************************************
** (b) Filing status
********************************************************************************

gen byte married = inlist(marst, 1, 2)		// married, spouse present or absent
gen byte mfs = (marst == 3 & sploc == 0)	// married filing separately

gen byte filing_status = 1					// single (default)
replace filing_status = 2 if married == 1	// MFJ
replace filing_status = 6 if mfs == 1		// MFS
label var filing_status "Filing status (1=single, 2=MFJ, 6=MFS)"

********************************************************************************
** (c) Dependents (simplified — use NCHILD, capped at 3)
********************************************************************************

gen byte depx = min(nchild, 3)
label var depx "Dependent exemptions (capped at 3)"

********************************************************************************
** (d) Income variable construction (at tax-unit level)
********************************************************************************

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

********************************************************************************
** (e) Primary filer flag
********************************************************************************

gen byte primary_filer = (unit_id == pernum)
label var primary_filer "Primary filer in tax unit"

********************************************************************************
** (f) Compute tax-unit AGI proxy
********************************************************************************

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

********************************************************************************
** (a) Create AGI brackets matching IRS stubs
********************************************************************************

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

********************************************************************************
** (b) GREG calibration (weights match IRS counts + AGI + wages by bracket)
********************************************************************************

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

********************************************************************************
** (c) Itemizer assignment
********************************************************************************

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

********************************************************************************
** (d) Verification: compare calibrated totals to IRS
********************************************************************************

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

********************************************************************************
** (a) Prepare TAXSIM input variables
********************************************************************************

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

********************************************************************************
** (b) Run TAXSIM
********************************************************************************

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

** ---- Compute college share of AGI ----
** Used to scale ACS College spec τ (estimated on college-educated only)
** to the full AGI base for revenue calculations
gen byte college_filer = (educd >= 101) if !missing(educd)
qui summ agi_proxy [aw=cal_wt] if college_filer == 1
scalar agi_college = r(sum_w) * r(mean)
qui summ agi_proxy [aw=cal_wt]
scalar agi_total = r(sum_w) * r(mean)
scalar college_agi_share = agi_college / agi_total
dis "College share of AGI: " %6.4f college_agi_share " (" %5.2f college_agi_share*100 "%)"
drop college_filer

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

********************************************************************************
** (a) Compute X (AGI loss from migration effect)
********************************************************************************

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


********************************************************************************
** (b) Compute out-migration probability
********************************************************************************

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

** PFA revenue loss from departing AGI
scalar mt_revenue_loss = avg_mt_rate * X_1
dis "Multnomah county revenue loss from migration effect: $" %15.0fc mt_revenue_loss

********************************************************************************
** SECTION 10B: Revenue Scaling to Actual Collections
********************************************************************************

dis ""
dis "=============================================="
dis "Section 10B: Revenue scaling to actual collections"
dis "=============================================="

** Dynamic revenue = R_s + R_m (where R_m is negative, i.e. a loss)
** R_dyn = baseline - loss
scalar pfa_dynamic_revenue = baseline_pfa_revenue - mt_revenue_loss
scalar oregon_dynamic_revenue = baseline_state_revenue - oregon_revenue_loss

** Migration share = R_m / (R_s + R_m) = loss / dynamic_revenue
** (positive number representing the % reduction)
scalar pfa_migration_share = mt_revenue_loss / pfa_dynamic_revenue
scalar oregon_migration_share = oregon_revenue_loss / oregon_dynamic_revenue

** Implied actual revenue loss = share × actual revenue
scalar pfa_implied_loss = pfa_migration_share * `actual_pfa_revenue'
scalar oregon_implied_loss = oregon_migration_share * `actual_oregon_revenue'

dis ""
dis "PFA Dynamic Revenue (simulated):       $" %15.0fc pfa_dynamic_revenue
dis "PFA Migration Share:                     " %8.4f pfa_migration_share " (" %5.2f pfa_migration_share*100 "%)"
dis "PFA Implied Actual Loss:                $" %15.0fc pfa_implied_loss
dis ""
dis "Oregon Dynamic Revenue (simulated):     $" %15.0fc oregon_dynamic_revenue
dis "Oregon Migration Share:                  " %8.4f oregon_migration_share " (" %5.2f oregon_migration_share*100 "%)"
dis "Oregon Implied Actual Loss:             $" %15.0fc oregon_implied_loss

********************************************************************************
** SECTION 11: Output — Tables & Figures
********************************************************************************

dis ""
dis "=============================================="
dis "Section 11: Output tables and figures"
dis "=============================================="

********************************************************************************
** (a) Summary table
********************************************************************************

** Display summary
dis ""
dis "=================================================================="
dis "REVENUE IMPACT SUMMARY"
dis "=================================================================="
dis ""
dis "--- Simulated (Microsim) ---"
dis "Baseline PFA revenue (R_s):              $" %15.0fc baseline_pfa_revenue
dis "PFA migration loss (R_m):                $" %15.0fc mt_revenue_loss
dis "PFA dynamic revenue (R_s - R_m):         $" %15.0fc pfa_dynamic_revenue
dis "PFA migration share (R_m / dynamic):      " %8.4f pfa_migration_share " (" %5.2f pfa_migration_share*100 "%)"
dis ""
dis "Baseline Oregon revenue (R_s):           $" %15.0fc baseline_state_revenue
dis "Oregon migration loss (R_m):             $" %15.0fc oregon_revenue_loss
dis "Oregon dynamic revenue (R_s - R_m):      $" %15.0fc oregon_dynamic_revenue
dis "Oregon migration share (R_m / dynamic):   " %8.4f oregon_migration_share " (" %5.2f oregon_migration_share*100 "%)"
dis ""
dis "--- Scaled to Actual Revenue ---"
dis "Actual PFA revenue:                      $" %15.0fc `actual_pfa_revenue'
dis "Implied PFA loss from migration:         $" %15.0fc pfa_implied_loss
dis ""
dis "Actual Oregon revenue:                   $" %15.0fc `actual_oregon_revenue'
dis "Implied Oregon loss from migration:      $" %15.0fc oregon_implied_loss
dis "=================================================================="

** Export summary table to Excel
preserve
clear
set obs 12

gen str60 metric = ""
gen double value = .

replace metric = "Baseline PFA revenue (simulated)"        in 1
replace value = baseline_pfa_revenue                       in 1
replace metric = "PFA migration loss (simulated)"          in 2
replace value = mt_revenue_loss                            in 2
replace metric = "PFA dynamic revenue (simulated)"         in 3
replace value = pfa_dynamic_revenue                        in 3
replace metric = "PFA migration share (%)"                 in 4
replace value = pfa_migration_share * 100                  in 4
replace metric = "Actual PFA revenue"                      in 5
replace value = `actual_pfa_revenue'                       in 5
replace metric = "Implied PFA loss from migration"         in 6
replace value = pfa_implied_loss                           in 6
replace metric = "Baseline Oregon revenue (simulated)"     in 7
replace value = baseline_state_revenue                     in 7
replace metric = "Oregon migration loss (simulated)"       in 8
replace value = oregon_revenue_loss                        in 8
replace metric = "Oregon dynamic revenue (simulated)"      in 9
replace value = oregon_dynamic_revenue                     in 9
replace metric = "Oregon migration share (%)"              in 10
replace value = oregon_migration_share * 100               in 10
replace metric = "Actual Oregon revenue"                   in 11
replace value = `actual_oregon_revenue'                    in 11
replace metric = "Implied Oregon loss from migration"      in 12
replace value = oregon_implied_loss                        in 12

export excel "${results}revenue/tbl_revenue_summary.xlsx", ///
	firstrow(variables) replace
restore

********************************************************************************
** (b) Table of tax-unit-level statistics by AGI bracket
********************************************************************************

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

********************************************************************************
** (d) Summary by filing status
********************************************************************************

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

********************************************************************************
** Save final dataset
********************************************************************************

save "${data}working/revenue_microsim.dta", replace

********************************************************************************
** Export revenue parameters for elasticity calculations
** (Stata scalars do not persist across do-files)
********************************************************************************

clear
set obs 1
gen double avg_mt_rate = scalar(avg_mt_rate)
gen double avg_state_rate = scalar(avg_state_rate)
gen double baseline_pfa_revenue = scalar(baseline_pfa_revenue)
gen double baseline_state_revenue = scalar(baseline_state_revenue)
gen double total_agi_2022 = scalar(total_agi_2022)
gen double pfa_migration_share = scalar(pfa_migration_share)
gen double oregon_migration_share = scalar(oregon_migration_share)
gen double pfa_implied_loss = scalar(pfa_implied_loss)
gen double oregon_implied_loss = scalar(oregon_implied_loss)
save "${data}working/revenue_parameters.dta", replace
project_build_signature, artifact("sdid_results")
project_write_manifest using "${data}working/revenue_parameters_manifest.dta", ///
	artifact("revenue_parameters") script("02_revenue.do") ///
	upstream("`r(signature)'")

dis ""
dis "Exported revenue_parameters.dta:"
dis "  avg_mt_rate           = " %8.6f avg_mt_rate
dis "  avg_state_rate        = " %8.6f avg_state_rate
dis "  baseline_pfa_revenue  = $" %15.0fc baseline_pfa_revenue
dis "  baseline_state_revenue= $" %15.0fc baseline_state_revenue
dis "  total_agi_2022        = $" %15.0fc total_agi_2022
dis "  pfa_migration_share   = " %8.6f pfa_migration_share
dis "  oregon_migration_share= " %8.6f oregon_migration_share
dis "  pfa_implied_loss      = $" %15.0fc pfa_implied_loss
dis "  oregon_implied_loss   = $" %15.0fc oregon_implied_loss

********************************************************************************
** SECTION 12: Distribution of Revenue Effects Across SDID Specifications
********************************************************************************

dis ""
dis "=============================================="
dis "Section 12: Revenue effect distribution"
dis "=============================================="

** plotplainblind palette
local col_fill    "86 180 233"		// sky — histogram fill
local col_irs     "213 94 0"		// vermillion — IRS preferred
local col_acs     "0 114 178"		// sea — ACS College preferred

capture confirm file "${results}sdid/sdid_results.dta"
if _rc == 0 {
	project_assert_manifest using "${results}sdid/sdid_results_manifest.dta", ///
		artifact("sdid_results")

	use "${results}sdid/sdid_results.dta", clear

	** Parse outcome type and migration direction
	gen outcome_type = ""
	replace outcome_type = "n1" if strpos(outcome, "n1_") > 0
	replace outcome_type = "n2" if strpos(outcome, "n2_") > 0
	replace outcome_type = "agi" if strpos(outcome, "agi_") > 0

	gen migration = ""
	replace migration = "net" if strpos(outcome, "_net_") > 0
	replace migration = "in" if strpos(outcome, "_in_") > 0
	replace migration = "out" if strpos(outcome, "_out_") > 0

	** Parse out-of-state flag
	gen outstate = strpos(outcome, "_outstate") > 0 | strpos(outcome, "_irs5") > 0

	** Parse data type
	gen data_type = ""
	replace data_type = "IRS" if strpos(outcome, "_irs") > 0 & outstate == 0
	replace data_type = "IRS (Out-of-State)" if strpos(outcome, "_irs") > 0 & outstate == 1
	replace data_type = "ACS All" if strpos(outcome, "_acs1") > 0 & outstate == 0
	replace data_type = "ACS College" if strpos(outcome, "_acs2") > 0 & outstate == 0
	replace data_type = "ACS All (Out-of-State)" if strpos(outcome, "_acs1") > 0 & outstate == 1
	replace data_type = "ACS College (Out-of-State)" if strpos(outcome, "_acs2") > 0 & outstate == 1

	** Parse period
	gen period_type = ""
	replace period_type = "16-22" if strpos(sample_data, "16_22") > 0
	replace period_type = "16-22" if strpos(outcome, "_irs") > 0 & period_type == ""
	replace period_type = "16-24" if strpos(sample_data, "16_24") > 0

	project_mark_preferred_main

	** ================================================================
	** (a) PFA revenue effect distribution (domestic/county-level AGI net)
	** ================================================================

	preserve

	** Keep AGI net migration specs, domestic (not out-of-state)
	keep if outcome_type == "agi" & migration == "net" & outstate == 0

	qui count
	local n_specs = r(N)
	dis "PFA revenue distribution: `n_specs' specifications"

	if `n_specs' > 0 {

		** Compute implied revenue effect for each specification
		** effect_i = abs(tau_i) / 100
		** For ACS College specs, scale by college share of AGI since τ
		** is estimated on college-educated migration only
		** X_i = effect_i * total_agi_2022
		** R_m_i = avg_mt_rate * X_i
		** dynamic_i = baseline_pfa_revenue - R_m_i
		** implied_loss_i = (R_m_i / dynamic_i) * actual_pfa_revenue
		gen double effect_i = abs(tau) / 100
		replace effect_i = effect_i * scalar(college_agi_share) ///
			if strpos(data_type, "ACS") > 0
		gen double X_i = effect_i * scalar(total_agi_2022)
		gen double R_m_i = scalar(avg_mt_rate) * X_i
		gen double dynamic_i = scalar(baseline_pfa_revenue) - R_m_i
		gen double share_i = R_m_i / dynamic_i * 100
		gen double implied_loss_i = (R_m_i / dynamic_i) * `actual_pfa_revenue' / 1e6

		** Build individual vertical lines for each preferred spec
		** IRS preferred in vermillion, ACS College preferred in sea blue
		qui count if preferred == 1
		local n_pref = r(N)
		local pref_overlays ""
		local irs_j = 0
		local acs_j = 0
		forvalues i = 1/`=_N' {
			if preferred[`i'] == 1 {
				local v = implied_loss_i[`i']
				local dt = data_type[`i']
				if strpos("`dt'", "IRS") > 0 {
					local ++irs_j
					local pref_overlays `"`pref_overlays' (scatteri 0 `v' 1 `v', recast(line) lcolor("`col_irs'") lwidth(medthick) lpattern(dash))"'
				}
				else {
					local ++acs_j
					local pref_overlays `"`pref_overlays' (scatteri 0 `v' 1 `v', recast(line) lcolor("`col_acs'") lwidth(medthick) lpattern(dash))"'
				}
			}
		}

		** Legend order: 1=histogram, then IRS lines (2..1+irs_j), then ACS lines
		local leg_irs = 2
		local leg_acs = 2 + `irs_j'

		** Summary
		dis "PFA implied loss distribution ($ millions):"
		summ implied_loss_i, detail

		** Histogram with one dashed line per preferred spec
		twoway (histogram implied_loss_i, 								///
				fcolor("`col_fill'") lcolor(white) lwidth(thin) 		///
				bin(20) fraction) 										///
			`pref_overlays',											///
			graphregion(color(white)) 									///
			title("Distribution of Implied PFA Revenue Loss", 			///
				size(medium)) 											///
			subtitle("Across `n_specs' SDID specifications", 			///
				size(small)) 											///
			xtitle("Implied Revenue Loss ($ millions)") 				///
			ytitle("Fraction of Specifications") 						///
			legend(order(`leg_irs' "IRS Preferred" 						///
				`leg_acs' "ACS College Preferred") 						///
				ring(1) pos(6) rows(1) size(small))

		graph export "${results}revenue/fig_revenue_dist_pfa.pdf", replace
		graph export "${results}revenue/fig_revenue_dist_pfa.jpg", ///
			as(jpg) quality(100) replace

		** Overleaf copy
		if ${overleaf} == 1 {
			graph export "${ol_fig}fig_revenue_dist_pfa.pdf", replace
		}
	}

	restore

	** ================================================================
	** (b) Oregon revenue effect distribution (interstate/out-of-state AGI net)
	** ================================================================

	** Keep AGI net migration specs, out-of-state only
	keep if outcome_type == "agi" & migration == "net" & outstate == 1

	qui count
	local n_specs = r(N)
	dis "Oregon revenue distribution: `n_specs' specifications"

	if `n_specs' > 0 {

		** Compute implied revenue effect for each specification
		** Scale ACS College specs by college share of AGI
		gen double effect_i = abs(tau) / 100
		replace effect_i = effect_i * scalar(college_agi_share) ///
			if strpos(data_type, "ACS") > 0
		gen double X_i = effect_i * scalar(total_agi_2022)
		gen double R_m_i = scalar(avg_state_rate) * X_i
		gen double dynamic_i = scalar(baseline_state_revenue) - R_m_i
		gen double share_i = R_m_i / dynamic_i * 100
		gen double implied_loss_i = (R_m_i / dynamic_i) * `actual_oregon_revenue' / 1e6

		** Build individual vertical lines for each preferred spec
		qui count if preferred == 1
		local n_pref = r(N)
		local pref_overlays ""
		local irs_j = 0
		local acs_j = 0
		forvalues i = 1/`=_N' {
			if preferred[`i'] == 1 {
				local v = implied_loss_i[`i']
				local dt = data_type[`i']
				if strpos("`dt'", "IRS") > 0 {
					local ++irs_j
					local pref_overlays `"`pref_overlays' (scatteri 0 `v' 1 `v', recast(line) lcolor("`col_irs'") lwidth(medthick) lpattern(dash))"'
				}
				else {
					local ++acs_j
					local pref_overlays `"`pref_overlays' (scatteri 0 `v' 1 `v', recast(line) lcolor("`col_acs'") lwidth(medthick) lpattern(dash))"'
				}
			}
		}

		local leg_irs = 2
		local leg_acs = 2 + `irs_j'

		** Summary
		dis "Oregon implied loss distribution ($ millions):"
		summ implied_loss_i, detail

		** Histogram with one dashed line per preferred spec
		twoway (histogram implied_loss_i, 								///
				fcolor("`col_fill'") lcolor(white) lwidth(thin)			///
				bin(20) fraction) 										///
			`pref_overlays',											///
			graphregion(color(white)) 									///
			title("Distribution of Implied Oregon Revenue Loss", 		///
				size(medium)) 											///
			subtitle("Across `n_specs' SDID specifications", 			///
				size(small)) 											///
			xtitle("Implied Revenue Loss ($ millions)") 				///
			ytitle("Fraction of Specifications") 						///
			legend(order(`leg_irs' "IRS Preferred" 						///
				`leg_acs' "ACS College Preferred") 						///
				ring(1) pos(6) rows(1) size(small))

		graph export "${results}revenue/fig_revenue_dist_oregon.pdf", replace
		graph export "${results}revenue/fig_revenue_dist_oregon.jpg", ///
			as(jpg) quality(100) replace

		** Overleaf copy
		if ${overleaf} == 1 {
			graph export "${ol_fig}fig_revenue_dist_oregon.pdf", replace
		}
	}

}
else {
	dis as error "WARNING: sdid_results.dta not found. Skipping revenue distribution plots."
}

dis ""
dis "=============================================="
dis "02_revenue.do complete."
dis "Output files:"
dis "  ${results}revenue/tbl_revenue_summary.xlsx"
dis "  ${results}revenue/tbl_pfa_by_bracket.xlsx"
dis "  ${results}revenue/fig_revenue_dist_pfa.pdf"
dis "  ${results}revenue/fig_revenue_dist_oregon.pdf"
dis "  ${data}working/revenue_microsim.dta"
dis "=============================================="

capture log close log_02rev
