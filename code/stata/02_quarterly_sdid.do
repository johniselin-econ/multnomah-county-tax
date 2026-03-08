/*******************************************************************************
File Name: 		02_quarterly_sdid.do
Creator: 		John Iselin
Date Update:	March 2026

Called by: 00_multnomah.do

Purpose: Perform synthetic difference-in-difference estimation on quarterly
         county-level outcomes from QWI (employment/earnings) and QCEW
         (establishments/wages).

         Two estimation phases handle different data windows:
         Phase 1 — QCEW (2016 Q1 – 2024 Q4): ln_estabs, ln_wage, ln_emp
         Phase 2 — QWI  (2016 Q1 – latest):   ln_emp, ln_emp_ba,
                                                ln_earns, ln_earns_ba

         Supports both sequential and parallel processing modes.

Requirements (for parallel mode):
- parallel package: net install parallel, from(https://raw.github.com/gvegayon/parallel/stable/) replace

Inputs:
- ${data}qcew/qcew_YYYY_QN.csv (QCEW quarterly county data)
- ${data}qwi/qwi_YYYY_QN.csv (QWI quarterly county data)
- ${data}working/demographics_2020.dta (demographics)
- ${data}working/bea_economics.dta (BEA time-varying population + PCI)
- ${data}working/covid_cleaned_wide.dta (COVID data)
- ${data}working/property_tax_rates_overall.dta (property tax rates)
- ${data}working/age_shares_county.dta (age shares for demographic k-means)
- ${data}working/jii_stringency.dta (COVID stringency)

Outputs:
- ${results}sdid/quarterly/ — Tables, event study figures, spec curves
- quarterly_sdid_results.dta: Treatment effects

Authors: John Iselin

For more information, contact john.iselin@yale.edu

*******************************************************************************/


** Start log file
capture log close log_02_quarterly
log using "${logs}02_log_quarterly_sdid_${date}", replace text name(log_02_quarterly)

** plotplainblind palette (RGB) — consistent across all figures
local col_sig_notpref  "0 114 178"    // sea (p7) — sig, not preferred
local col_insig_notpref "86 180 233"  // sky (p3) — insig, not preferred
local col_sig_pref     "213 94 0"     // vermillion (p6) — sig, preferred
local col_insig_pref   "230 159 0"    // orangebrown (p8) — insig, preferred
local col_zero         "204 121 167"  // reddish (p5) — zero line
local col_ref          "153 153 153"  // gs10 (p2) — reference lines

** Number of bootstrap replications
local reps = 100

** Analysis window
local start_year = ${start_year_irs_analysis}
local end_year   = ${end_year_acs}

** Fallback defaults for standalone execution (not via 00_multnomah.do)
if "${use_parallel}" == "" global use_parallel 0
if "${n_clusters}" == ""   global n_clusters 1

** Initialize parallel processing if enabled
if ${use_parallel} == 1 {
	parallel initialize ${n_clusters}, force
}


********************************************************************************
** PHASE 1: QCEW DATA PREPARATION
********************************************************************************

dis _n "========================================"
dis "PHASE 1: Loading QCEW data..."
dis "========================================"

** Import and append all QCEW quarterly CSVs
clear
tempfile qcew_built
local first_qcew = 1

forvalues y = `start_year'/`end_year' {
	forvalues q = 1/4 {

		local fn "${data}qcew/qcew_`y'_Q`q'.csv"
		capture confirm file "`fn'"
		if _rc != 0 {
			dis "  Skipping `fn' (not found)"
			continue
		}

		** Import with area_fips as string to preserve leading zeros
		import delimited using "`fn'", clear stringcols(1)

		** Create numeric fips
		destring area_fips, gen(fips) force
		drop if missing(fips)

		** Create quarterly date
		gen yq = yq(year, qtr)
		format yq %tq

		** Keep needed variables
		keep fips yq year qtrly_estabs month*_emplvl total_qtrly_wages

		** Combine monthly employment (average)
		egen qtrly_emp_avg = rowmean(month*_emplvl)
		drop month*_emplvl

		if `first_qcew' == 1 {
			save `qcew_built', replace
			local first_qcew = 0
		}
		else {
			append using `qcew_built'
			save `qcew_built', replace
		}

	}
}

** Load final appended QCEW
use `qcew_built', clear
dis "QCEW rows loaded: " _N

** Drop duplicates (shouldn't exist, but safety)
duplicates drop fips yq, force

** Drop counties with missing outcomes
drop if missing(qtrly_estabs) | missing(total_qtrly_wages) | missing(qtrly_emp_avg)
drop if qtrly_estabs <= 0 | total_qtrly_wages <= 0 | qtrly_emp_avg <= 0

** Create log outcomes
gen ln_qcew_estabs   = ln(qtrly_estabs)
gen ln_qcew_wage = ln(total_qtrly_wages)
gen ln_qcew_emp = ln(qtrly_emp_avg)
label var ln_qcew_estabs	"Log Quarterly Establishments"
label var ln_qcew_wage 		"Log Total Quarterly Wages"
label var ln_qcew_emp 		"Log Avg. Monthly Employment"

********************************************************************************
** QCEW MERGES
********************************************************************************

** Merge with Demographics (2020)
merge m:1 fips using "${data}working/demographics_2020", ///
	gen(demo_merge) keep(master match)
keep if demo_merge == 3
drop demo_merge

** Rename population from demographics
rename population pop_census

** Merge with BEA Economics (time-varying population, by year)
merge m:1 year fips using "${data}working/bea_economics", ///
	gen(econ_merge) keep(master match)
keep if econ_merge == 3
drop econ_merge

** Merge with COVID-19 Data
merge m:1 fips using "${data}working/covid_cleaned_wide.dta", ///
	gen(covid_merge) keep(master match)

** Merge with Property Tax Rates
merge m:1 year fips using "${data}working/property_tax_rates_overall", ///
	gen(proptx_merge) keep(master match) keepusing(prop_rate_mean prop_rate_se)

rename prop_rate_mean prop_tax_rate
rename prop_rate_se prop_tax_rate_se
label var prop_tax_rate "Mean property tax rate (% of home value)"

** Merge with Age Shares (for demographic k-means sample)
merge m:1 fips using "${data}working/age_shares_county", ///
	gen(age_merge) keep(master match)

** Merge with JII COVID Stringency Data
merge m:1 fips using "${data}working/jii_stringency.dta", ///
	gen(jii_merge) keep(master match)

** Organize
order yq year fips state_* county_*
sort fips yq

** Keep balanced panel
capture drop ct
bysort fips: gen ct = _N
qui summ ct
drop if ct < `r(max)'
drop ct

********************************************************************************
** QCEW SAMPLE CONSTRUCTION
********************************************************************************

** Define treated county
gen multnomah = state_fips == 41 & county_fips == 51
label var multnomah "Indicator for Multnomah County, Oregon"

** Define treatment indicator (quarterly: treatment starts 2021 Q1)
gen Treated = multnomah == 1 & yq >= tq(2021q1)
label var Treated "Treatment indicator"

** Define sample 1: All counties
gen sample_all = 1
label var sample_all "All counties (excluding AK, CA, HI, OR, WA)"

** Define sample 2: Urban counties (top 5% urbanization)
** Use first quarter of 2020 to compute cutoff
summ percent_urban if yq == tq(2020q1), de
local cutoff = r(p95)
gen sample_urban95 = percent_urban >= `cutoff'
label var sample_urban95 "Urban counties (top 5%)"

** Drop states (same as main SDID)
drop if state_name == "Alaska"
drop if state_name == "Hawaii"
drop if state_name == "California"
drop if state_name == "Washington"
drop if state_name == "Oregon" & multnomah == 0

** Define sample 3: COVID k-means match
cluster kmeans cases_cum* deaths_cum* if ///
	sample_urban95 == 1 & yq == tq(2020q1) & covid_merge == 3, k(5) gen(kmean)
bysort fips: egen kmean_group = mean(kmean)

gen tmp1 = kmean if sample_urban95 == 1 & yq == tq(2020q1) & covid_merge == 3 & multnomah == 1
egen tmp2 = mean(tmp1)
gen sample_urban95_covid = sample_urban95 == 1 & kmean_group == tmp2
drop tmp1 tmp2
label var sample_urban95_covid "Urban counties (top 5%) w. COVID k-means match"

** Define sample 4: Demographic k-means match
gen pci_pre = per_capita_income if yq == tq(2020q1)
bysort fips: egen pci_pre_fill = mean(pci_pre)
drop pci_pre
rename pci_pre_fill pci_pre

foreach v in pci_pre pop_census share_under_24 share_over_65 percent_urban {
	egen std_`v' = std(`v') if sample_urban95 == 1 & yq == tq(2020q1)
}

cluster kmeans std_pci_pre std_pop_census std_share_under_24 std_share_over_65 std_percent_urban if ///
	yq == tq(2020q1) & !missing(share_under_24), k(5) gen(kmean_demog)
bysort fips: egen kmean_demog_group = mean(kmean_demog)

gen tmp1 = kmean_demog if yq == tq(2020q1) & !missing(share_under_24) & multnomah == 1
egen tmp2 = mean(tmp1)
gen sample_demog = kmean_demog_group == tmp2
drop tmp1 tmp2 std_* pci_pre
label var sample_demog "Counties with Demographic Kmean Match (excluding AK, CA, HI OR, WA)"

** Define sample 5: COVID stringency k-means match (JII restriction-duration)
qui summ percent_urban if yq == tq(2020q1), de
local p90 = r(p90)
gen urban_top10 = percent_urban >= `p90'

foreach v in msahodays restclosedays gatherbandays strictgatherbandays maskpubdays {
	egen std_`v' = std(`v') if urban_top10 == 1 & yq == tq(2020q1) & jii_merge == 3
}

cluster kmeans std_msahodays std_restclosedays std_gatherbandays 	///
	std_strictgatherbandays std_maskpubdays if 						///
	urban_top10 == 1 & yq == tq(2020q1) & jii_merge == 3, k(5) gen(kmean_string)
bysort fips: egen kmean_string_group = mean(kmean_string)

gen tmp1 = kmean_string if urban_top10 == 1 & yq == tq(2020q1) & jii_merge == 3 & multnomah == 1
egen tmp2 = mean(tmp1)
gen sample_stringency = urban_top10 == 1 & kmean_string_group == tmp2
drop tmp1 tmp2 std_* urban_top10 kmean_string kmean_string_group
label var sample_stringency "Urban counties (top 10%) w. COVID stringency k-means match"

********************************************************************************
** QCEW OUTCOME SETUP
********************************************************************************

local qcew_outcomes "ln_qcew_estabs ln_qcew_wage ln_qcew_emp"

** Drop counties with missing log outcomes
foreach x of local qcew_outcomes {
	drop if missing(`x')
}

** Re-enforce balanced panel after drops
bysort fips: gen ct = _N
qui summ ct
drop if ct < `r(max)'
drop ct

** Short labels for tables and spec curve titles
local lbl_ln_qcew_estabs	"Log Establishments (QCEW)"
local lbl_ln_qcew_wage		"Log Total Quarterly Wages (QCEW)"
local lbl_ln_qcew_emp 		"Log Avg. Monthly Employment (QCEW)"

** Standardize covariates
local all_covariates "population per_capita_income"
foreach v of local all_covariates {
	egen tmp_v = std(`v')
	replace `v' = tmp_v
	drop tmp_v
}

** Declare panel
xtset fips yq

** Save prepared QCEW data
save "${data}working/quarterly_qcew_sdid_data.dta", replace


********************************************************************************
** PHASE 2: QWI DATA PREPARATION
********************************************************************************

dis _n "========================================"
dis "PHASE 2: Loading QWI data..."
dis "========================================"

** Import and append all QWI quarterly CSVs
clear
tempfile qwi_built
local first_qwi = 1

forvalues y = `start_year'/`end_year' {
	forvalues q = 1/4 {

		local fn "${data}qwi/qwi_`y'_Q`q'.csv"
		capture confirm file "`fn'"
		if _rc != 0 {
			dis "  Skipping `fn' (not found)"
			continue
		}

		import delimited using "`fn'", clear

		** Note: import delimited lowercases all column names automatically.
		** QWI CSV headers (Emp, EarnS, etc.) become emp, earns, etc.

		** Keep only total sex (sex == 0) and education groups E0 (all) + E4 (BA+)
		keep if sex == 0
		keep if education == "E0" | education == "E4"

		** Parse time variable (format: "YYYY-QN")
		gen year = real(substr(time, 1, 4))
		gen quarter = real(substr(time, 7, 1))
		gen yq = yq(year, quarter)
		format yq %tq

		** Keep needed variables
		keep fips yq year education emp earns

		** Generate total earnings
		gen tot_earns = emp * earns
		drop earns
		rename tot_earns earns

		if `first_qwi' == 1 {
			save `qwi_built', replace
			local first_qwi = 0
		}
		else {
			append using `qwi_built'
			save `qwi_built', replace
		}

	}
}

** Check if any QWI data was loaded
if `first_qwi' == 1 {
	dis "WARNING: No QWI data files found. Skipping Phase 2."
	local skip_qwi = 1
}
else {
	local skip_qwi = 0
}

if `skip_qwi' == 0 {

** Load final appended QWI
use `qwi_built', clear
dis "QWI rows loaded (before reshape): " _N

** Destring fips if needed
capture destring fips, replace force

** Reshape wide on education: E0 and E4 become separate columns
reshape wide emp earns, i(fips yq year) j(education) string

** Rename to readable names
rename empE0   emp_all
rename earnsE0 earns_all
rename empE4   emp_ba
rename earnsE4 earns_ba

** Drop missing/zero values
drop if missing(emp_all) | emp_all <= 0
drop if missing(earns_all) | earns_all <= 0
drop if (!missing(emp_ba) & emp_ba <= 0) | (!missing(earns_ba) & earns_ba <= 0)

** Create log outcomes
gen ln_qwi_emp      = ln(emp_all)
gen ln_qwi_emp_ba   = ln(emp_ba)
gen ln_qwi_earns    = ln(earns_all)
gen ln_qwi_earns_ba = ln(earns_ba)

label var ln_qwi_emp      "Log Employment (All, QWI)"
label var ln_qwi_emp_ba   "Log Employment (BA+, QWI)"
label var ln_qwi_earns    "Log Total Monthly Earnings (All, QWI)"
label var ln_qwi_earns_ba "Log Total Monthly Earnings (BA+, QWI)"

********************************************************************************
** QWI MERGES (same sequence as QCEW)
********************************************************************************

** Merge with Demographics (2020)
merge m:1 fips using "${data}working/demographics_2020", ///
	gen(demo_merge) keep(master match)
keep if demo_merge == 3
drop demo_merge

** Rename population from demographics
rename population pop_census

** Merge with BEA Economics (time-varying population, by year)
merge m:1 year fips using "${data}working/bea_economics", ///
	gen(econ_merge) keep(master match)
keep if econ_merge == 3
drop econ_merge

** Merge with COVID-19 Data
merge m:1 fips using "${data}working/covid_cleaned_wide.dta", ///
	gen(covid_merge) keep(master match)

** Merge with Property Tax Rates
merge m:1 year fips using "${data}working/property_tax_rates_overall", ///
	gen(proptx_merge) keep(master match) keepusing(prop_rate_mean prop_rate_se)

rename prop_rate_mean prop_tax_rate
rename prop_rate_se prop_tax_rate_se
label var prop_tax_rate "Mean property tax rate (% of home value)"

** Merge with Age Shares
merge m:1 fips using "${data}working/age_shares_county", ///
	gen(age_merge) keep(master match)

** Merge with JII COVID Stringency Data
merge m:1 fips using "${data}working/jii_stringency.dta", ///
	gen(jii_merge) keep(master match)

** Organize
order yq year fips state_* county_*
sort fips yq

** Keep balanced panel
capture drop ct
bysort fips: gen ct = _N
qui summ ct
drop if ct < `r(max)'
drop ct

********************************************************************************
** QWI SAMPLE CONSTRUCTION (same logic as QCEW)
********************************************************************************

** Define treated county
gen multnomah = state_fips == 41 & county_fips == 51
label var multnomah "Indicator for Multnomah County, Oregon"

** Define treatment indicator
gen Treated = multnomah == 1 & yq >= tq(2021q1)
label var Treated "Treatment indicator"

** Define sample 1: All counties
gen sample_all = 1
label var sample_all "All counties (excluding AK, CA, HI, OR, WA)"

** Define sample 2: Urban counties (top 5% urbanization)
summ percent_urban if yq == tq(2020q1), de
local cutoff = r(p95)
gen sample_urban95 = percent_urban >= `cutoff'
label var sample_urban95 "Urban counties (top 5%)"

** Drop states (same as main SDID)
drop if state_name == "Alaska"
drop if state_name == "Hawaii"
drop if state_name == "California"
drop if state_name == "Washington"
drop if state_name == "Oregon" & multnomah == 0

** Define sample 3: COVID k-means match
cluster kmeans cases_cum* deaths_cum* if ///
	sample_urban95 == 1 & yq == tq(2020q1) & covid_merge == 3, k(5) gen(kmean)
bysort fips: egen kmean_group = mean(kmean)

gen tmp1 = kmean if sample_urban95 == 1 & yq == tq(2020q1) & covid_merge == 3 & multnomah == 1
egen tmp2 = mean(tmp1)
gen sample_urban95_covid = sample_urban95 == 1 & kmean_group == tmp2
drop tmp1 tmp2
label var sample_urban95_covid "Urban counties (top 5%) w. COVID k-means match"

** Define sample 4: Demographic k-means match
gen pci_pre = per_capita_income if yq == tq(2020q1)
bysort fips: egen pci_pre_fill = mean(pci_pre)
drop pci_pre
rename pci_pre_fill pci_pre

foreach v in pci_pre pop_census share_under_24 share_over_65 percent_urban {
	egen std_`v' = std(`v') if sample_urban95 == 1 & yq == tq(2020q1)
}

cluster kmeans std_pci_pre std_pop_census std_share_under_24 std_share_over_65 std_percent_urban if ///
	yq == tq(2020q1) & !missing(share_under_24), k(5) gen(kmean_demog)
bysort fips: egen kmean_demog_group = mean(kmean_demog)

gen tmp1 = kmean_demog if yq == tq(2020q1) & !missing(share_under_24) & multnomah == 1
egen tmp2 = mean(tmp1)
gen sample_demog = kmean_demog_group == tmp2
drop tmp1 tmp2 std_* pci_pre
label var sample_demog "Counties with Demographic Kmean Match (excluding AK, CA, HI OR, WA)"

** Define sample 5: COVID stringency k-means match (JII restriction-duration)
qui summ percent_urban if yq == tq(2020q1), de
local p90 = r(p90)
gen urban_top10 = percent_urban >= `p90'

foreach v in msahodays restclosedays gatherbandays strictgatherbandays maskpubdays {
	egen std_`v' = std(`v') if urban_top10 == 1 & yq == tq(2020q1) & jii_merge == 3
}

cluster kmeans std_msahodays std_restclosedays std_gatherbandays 	///
	std_strictgatherbandays std_maskpubdays if 						///
	urban_top10 == 1 & yq == tq(2020q1) & jii_merge == 3, k(5) gen(kmean_string)
bysort fips: egen kmean_string_group = mean(kmean_string)

gen tmp1 = kmean_string if urban_top10 == 1 & yq == tq(2020q1) & jii_merge == 3 & multnomah == 1
egen tmp2 = mean(tmp1)
gen sample_stringency = urban_top10 == 1 & kmean_string_group == tmp2
drop tmp1 tmp2 std_* urban_top10 kmean_string kmean_string_group
label var sample_stringency "Urban counties (top 10%) w. COVID stringency k-means match"

********************************************************************************
** QWI OUTCOME SETUP
********************************************************************************

local qwi_outcomes "ln_qwi_emp ln_qwi_emp_ba ln_qwi_earns ln_qwi_earns_ba"

** Drop counties with missing log outcomes
foreach x of local qwi_outcomes {
	drop if missing(`x')
}

** Re-enforce balanced panel
bysort fips: gen ct = _N
qui summ ct
drop if ct < `r(max)'
drop ct

** Short labels
local lbl_ln_qwi_emp      "Log Employment (All, QWI)"
local lbl_ln_qwi_emp_ba   "Log Employment (BA+, QWI)"
local lbl_ln_qwi_earns    "Log Total Earnings (All, QWI)"
local lbl_ln_qwi_earns_ba "Log Total Earnings (BA+, QWI)"

** Standardize covariates
local all_covariates "population per_capita_income"
foreach v of local all_covariates {
	egen tmp_v = std(`v')
	replace `v' = tmp_v
	drop tmp_v
}

** Declare panel
xtset fips yq

** Save prepared QWI data
save "${data}working/quarterly_qwi_sdid_data.dta", replace

} // END skip_qwi == 0


********************************************************************************
** OUTPUT SETUP
********************************************************************************

capture mkdir "${results}sdid/quarterly"


********************************************************************************
** PARALLEL MODE: DEFINE PROGRAMS AND SETUP
********************************************************************************

if ${use_parallel} == 1 {

	** Create table-level specification grid
	** Table units defined by: phase (qcew/qwi) x sample (5) x exclusion (2)
	** QCEW: 5 x 2 = 10 tables, each with 3 outcomes x 2 cov = 6 specs
	** QWI:  5 x 2 = 10 tables, each with 4 outcomes x 2 cov = 8 specs
	** Total: up to 20 tables (10 if QWI skipped)

	preserve
	clear

	local table_id = 0

	** Initialize empty dataset
	set obs 0
	gen table_id = .
	gen phase = ""
	gen samp_var = ""
	gen exclusion = .

	save "${data}working/quarterly_table_grid.dta", replace

	** Build QCEW tables
	foreach samp in "sample_all" "sample_urban95" "sample_urban95_covid" "sample_demog" "sample_stringency" {
		forvalues exl = 0/1 {

			local table_id = `table_id' + 1

			clear
			set obs 1
			gen table_id = `table_id'
			gen phase = "qcew"
			gen samp_var = "`samp'"
			gen exclusion = `exl'

			append using "${data}working/quarterly_table_grid.dta"
			save "${data}working/quarterly_table_grid.dta", replace

		}
	}

	** Build QWI tables (only if data available)
	if `skip_qwi' == 0 {
		foreach samp in "sample_all" "sample_urban95" "sample_urban95_covid" "sample_demog" "sample_stringency" {
			forvalues exl = 0/1 {

				local table_id = `table_id' + 1

				clear
				set obs 1
				gen table_id = `table_id'
				gen phase = "qwi"
				gen samp_var = "`samp'"
				gen exclusion = `exl'

				append using "${data}working/quarterly_table_grid.dta"
				save "${data}working/quarterly_table_grid.dta", replace

			}
		}
	}

	** Load and verify grid
	use "${data}working/quarterly_table_grid.dta", clear
	dis "Total quarterly table units: " _N
	sort table_id
	save "${data}working/quarterly_table_grid.dta", replace

	restore

	** Define program to run all SDID specifications for one table unit
	capture program drop run_quarterly_table
	program define run_quarterly_table
		syntax, table_id(integer) data_path(string) results_path(string) reps(integer)

		** Color palette (must be redefined inside program scope)
		local col_zero "204 121 167"

		** Load table specification from grid
		preserve
		use "`data_path'working/quarterly_table_grid.dta", clear
		keep if table_id == `table_id'

		** Extract specification parameters
		local samp_var = samp_var[1]
		local exl = exclusion[1]
		local phase = phase[1]

		restore

		** Load phase-specific dataset and define outcomes
		if "`phase'" == "qcew" {
			use "`data_path'working/quarterly_qcew_sdid_data.dta", clear
			xtset fips yq

			local outcomes "ln_qcew_estabs ln_qcew_wage ln_qcew_emp"
			local lbl_ln_qcew_estabs "Log Establishments (QCEW)"
			local lbl_ln_qcew_wage   "Log Total Quarterly Wages (QCEW)"
			local lbl_ln_qcew_emp    "Log Avg. Monthly Employment (QCEW)"
			local tab_prefix "qcew"
		}
		else {
			use "`data_path'working/quarterly_qwi_sdid_data.dta", clear
			xtset fips yq

			local outcomes "ln_qwi_emp ln_qwi_emp_ba ln_qwi_earns ln_qwi_earns_ba"
			local lbl_ln_qwi_emp      "Log Employment (All, QWI)"
			local lbl_ln_qwi_emp_ba   "Log Employment (BA+, QWI)"
			local lbl_ln_qwi_earns    "Log Total Earnings (All, QWI)"
			local lbl_ln_qwi_earns_ba "Log Total Earnings (BA+, QWI)"
			local tab_prefix "qwi"
		}

		** Define sample
		gen sample = `samp_var' == 1
		if `exl' == 1 replace sample = 0 if inrange(yq, tq(2020q1), tq(2020q4))

		** Skip if no treated unit in sample
		qui count if multnomah == 1 & sample == 1
		if r(N) == 0 {
			dis "Skipping table `table_id': no treated unit in sample"
			exit
		}

		** Create output directory
		capture mkdir "`results_path'sdid/quarterly"

		** Covariates
		local covariates "population per_capita_income"

		** Clear stored estimates
		eststo clear

		** Loop over outcome variables
		foreach out of local outcomes {

			** Store label
			local label : variable label `out'

			** Loop over inclusion of covariates
			forvalues c = 0/1 {

				** Covariates for sdid
				if `c' == 0 local covars ""
				else if `c' == 1 local covars "covariates(`covariates', projected)"

				** Covariates for sdid_event
				if `c' == 0 local covars_event ""
				else if `c' == 1 local covars_event "covariates(`covariates')"

				** File paths for figures
				if `exl' == 0 local path "`results_path'sdid/quarterly/fig_quarterly_`out'_`c'_`samp_var'_"
				if `exl' == 1 local path "`results_path'sdid/quarterly/fig_quarterly_`out'_`c'_`samp_var'_excl2020_"

				** Run SDID
				capture noisily {
					eststo sdid_`out'_`c': sdid `out' fips yq Treated	///
						if sample == 1,				///
						vce(placebo) 				///
						`covars'					///
						reps(`reps')				///
						graph graph_export("`path'", .pdf)
				}

				if _rc != 0 {
					dis "SDID failed for `out' c=`c' exl=`exl' samp=`samp_var'. Skipping."
					continue
				}

				** Store results
				local tmp_tau = e(ATT)
				local tmp_se = e(se)

				** Pre-treatment mean and county count
				qui summ `out' if multnomah == 1 & Treated == 0 & sample == 1
				local tmp_premean = r(mean)
				estadd scalar mean = r(mean)

				** County count: use first post-treatment quarter
				qui summ `out' if yq == tq(2021q1) & sample == 1
				local tmp_ncounties = r(N)
				estadd scalar count = r(N)

				** Save treatment effect results to temp file
				preserve
				clear
				qui set obs 1
				gen table_id = `table_id'
				gen sample_data = "`phase'"
				gen sample = "`samp_var'"
				gen outcome = "`out'"
				gen controls = `c'
				gen exclusion = `exl'
				gen tau = `tmp_tau'
				gen se = `tmp_se'
				gen pval = 2 * (1 - normal(abs(tau/se)))
				gen ci_lower = tau - 1.96 * se
				gen ci_upper = tau + 1.96 * se
				gen n_counties = `tmp_ncounties'
				gen pre_mean = `tmp_premean'
				gen significant = abs(tau/se) > 1.96
				save "`results_path'sdid/temp_quarterly_results/results_`table_id'_`out'_`c'.dta", replace
				restore

				** Run event study
				capture noisily {
					sdid_event `out' fips yq Treated	///
						if sample == 1,				///
						`covars_event'				///
						vce(placebo) 				///
						brep(`reps') 				///
						placebo(all)
				}

				local event_rc = _rc
				capture drop ever_treated*

				if `event_rc' == 0 {

					** Store max quarter
					qui summ yq if multnomah == 1 & sample == 1
					local max_yq = r(max)

					** Extract matrix
					qui count if multnomah == 1 & sample == 1
					local ct = r(N)
					local ct = `ct' + 1

					capture mat res = e(H)[2..`ct',1..5]
					if _rc != 0 {
						continue
					}

					** Preserve and plot
					preserve
					svmat res
					gen id = `max_yq' - _n + 1 if !missing(res1)
					format id %tq

					if `exl' == 1 {
						** Shift pre-gap IDs down by 4 quarters (they are
						** misaligned because the matrix skips 2020 Q1-Q4).
						replace id = id - 4 if id <= tq(2020q4)

						** Insert 4 missing-value markers for excluded 2020 quarters.
						local new_obs = _N + 4
						set obs `new_obs'
						forvalues eq = 0/3 {
							local row = _N - 3 + `eq'
							replace id = tq(2020q1) + `eq' in `row'
						}
					}
					label var id "Quarter"
					sort id

					twoway	(rcap res3 res4 id, lc(gs10) fc(gs11%50))	///
							(scatter res1 id, mc(black)),				///
						legend(off) ytitle("`label'")					///
						yline(0, lc("`col_zero'") lp(-))				///
						xline(`=tq(2020q4) + 0.5', lc(black) lp(solid))	///
						xlabel(, format(%tqCCYY!qq))

					if `exl' == 1 local evpath "`results_path'sdid/quarterly/fig_quarterly_`out'_`c'_`samp_var'_excl2020_eventstudy.jpg"
					else local evpath "`results_path'sdid/quarterly/fig_quarterly_`out'_`c'_`samp_var'_eventstudy.jpg"

					graph export "`evpath'", as(jpg) name("Graph") quality(100) replace
					restore
				}

			} // END COVAR LOOP

		} // END OUTCOME LOOP

		** Table of results
		if `exl' == 0 local tabfname "tab_quarterly_`tab_prefix'_`samp_var'.tex"
		if `exl' == 1 local tabfname "tab_quarterly_`tab_prefix'_`samp_var'_excl2020.tex"

		local _dests `""`results_path'sdid/quarterly/`tabfname'""'
		if ${overleaf} == 1 {
			local _dests `"`_dests' "${ol_tab}`tabfname'""'
		}

		** Build dynamic esttab model list
		local est_list ""
		local mtitle_list ""
		local mgroup_labels ""
		local mgroup_pattern ""
		foreach out of local outcomes {
			local est_list "`est_list' sdid_`out'_0 sdid_`out'_1"
			local mtitle_list `"`mtitle_list' "No Cov." "Cov.""'
			local mgroup_labels `"`mgroup_labels' "`lbl_`out''""'
			local mgroup_pattern "`mgroup_pattern' 1 0"
		}

		foreach _outfile of local _dests {
		capture noisily {
			esttab `est_list'	///
				using "`_outfile'",								///
			starlevel("*" 0.10 "**" 0.05 "***" 0.01)		///
			b(%-9.3f) se(%-9.3f) replace 					///
			mgroups(`mgroup_labels',						///
				pattern(`mgroup_pattern'))					///
			mtitle(`mtitle_list')							///
			stats(count mean, 								///
				fmt(%9.0fc %9.3fc) 							///
				labels("Number of Counties" "Pre-treatment mean"))
		}
		} // end foreach _outfile

		dis "Completed table `table_id': `phase' / `samp_var' / excl=`exl'"

	end

	** Define wrapper program for parallel execution
	capture program drop parallel_quarterly_wrapper
	program define parallel_quarterly_wrapper
		** Increase matsize for sdid_event memory requirements
		set matsize 5000
		** Store all table_ids upfront (run_quarterly_table will overwrite the dataset)
		local n_obs = _N
		forvalues i = 1/`n_obs' {
			local tid_`i' = table_id[`i']
		}

		** Now loop through and process each table
		forvalues i = 1/`n_obs' {
			dis "Worker processing quarterly table `tid_`i'' (`i' of `n_obs' in this chunk)"
			run_quarterly_table, table_id(`tid_`i'') data_path("${data}") results_path("${results}") reps(100)
		}
	end

} // END PARALLEL SETUP


********************************************************************************
** MAIN ESTIMATION
********************************************************************************

if ${use_parallel} == 1 {

	********************************************************************************
	** PARALLEL ESTIMATION
	********************************************************************************

	** Create temp directory for results
	capture mkdir "${results}sdid/temp_quarterly_results"

	** Load table grid
	use "${data}working/quarterly_table_grid.dta", clear
	local n_tables = _N
	dis "Running `n_tables' quarterly table units in parallel..."

	** Cost-balanced worker assignment via snake ordering
	** Tables vary in cost (n^2 in counties). Snake ordering balances load.
	preserve

	** Step 1: Count distinct counties per sample per phase
	** QCEW counts
	use "${data}working/quarterly_qcew_sdid_data.dta", clear

	foreach samp in "sample_all" "sample_urban95" "sample_urban95_covid" "sample_demog" "sample_stringency" {
		qui count if `samp' == 1 & yq == tq(2021q1)
		local nc_qcew_`samp' = r(N)
	}

	** QWI counts (if available)
	if `skip_qwi' == 0 {
		use "${data}working/quarterly_qwi_sdid_data.dta", clear

		foreach samp in "sample_all" "sample_urban95" "sample_urban95_covid" "sample_demog" "sample_stringency" {
			qui count if `samp' == 1 & yq == tq(2021q1)
			local nc_qwi_`samp' = r(N)
		}
	}

	** Build cost lookup table
	clear
	local n_phases = cond(`skip_qwi' == 0, 2, 1)
	local n_samples = 5
	qui set obs `=`n_phases' * `n_samples''
	gen phase = ""
	gen samp_var = ""
	gen n_counties = .
	gen cost = .

	local row = 0
	foreach samp in "sample_all" "sample_urban95" "sample_urban95_covid" "sample_demog" "sample_stringency" {
		local row = `row' + 1
		qui replace phase = "qcew" in `row'
		qui replace samp_var = "`samp'" in `row'
		local nc = `nc_qcew_`samp''
		qui replace n_counties = `nc' in `row'
		qui replace cost = `nc' * `nc' in `row'
	}

	if `skip_qwi' == 0 {
		foreach samp in "sample_all" "sample_urban95" "sample_urban95_covid" "sample_demog" "sample_stringency" {
			local row = `row' + 1
			qui replace phase = "qwi" in `row'
			qui replace samp_var = "`samp'" in `row'
			local nc = `nc_qwi_`samp''
			qui replace n_counties = `nc' in `row'
			qui replace cost = `nc' * `nc' in `row'
		}
	}

	tempfile cost_lookup
	save `cost_lookup'

	** Step 2: Merge cost weights into the table grid
	use "${data}working/quarterly_table_grid.dta", clear
	merge m:1 phase samp_var using `cost_lookup', keep(master match) nogen

	** Step 3: Snake ordering (LPT heuristic) for balanced worker assignment
	** Sort tables by cost descending so heaviest tables are assigned first
	gsort -cost table_id

	** Snake pattern: 1,2,...,k, k,k-1,...,1, 1,2,...,k, ...
	** This ensures each worker gets roughly equal total cost
	local k = ${n_clusters}
	gen worker = .
	local direction = 1  // 1 = forward, -1 = backward
	local w = 1          // current worker

	forvalues i = 1/`=_N' {
		qui replace worker = `w' in `i'

		** Advance worker in current direction
		if `direction' == 1 {
			if `w' < `k' {
				local w = `w' + 1
			}
			else {
				** Hit top: reverse direction (stay on same worker for next)
				local direction = -1
			}
		}
		else {
			if `w' > 1 {
				local w = `w' - 1
			}
			else {
				** Hit bottom: reverse direction (stay on same worker for next)
				local direction = 1
			}
		}
	}

	** Step 4: Sort by worker so parallel's contiguous row split aligns
	sort worker table_id

	** Diagnostic: show per-worker cost balance with phase breakdown
	dis _n "=== Cost-Balanced Worker Assignment (Quarterly) ==="

	** Count totals by phase
	qui count if phase == "qcew"
	local n_qcew = r(N)
	qui count if phase == "qwi"
	local n_qwi = r(N)
	dis "Tables: `=_N' (`n_qcew' QCEW + `n_qwi' QWI)  Workers: `k'"

	tempvar worker_cost worker_count
	bysort worker: egen `worker_cost' = total(cost)
	bysort worker: egen `worker_count' = count(table_id)

	** Display per-worker summary with QCEW/QWI breakdown
	forvalues w = 1/`k' {
		qui summ `worker_cost' if worker == `w'
		if r(N) > 0 {
			local wc = r(mean)
			qui summ `worker_count' if worker == `w'
			local wn = r(mean)
			qui count if worker == `w' & phase == "qcew"
			local wn_qcew = r(N)
			qui count if worker == `w' & phase == "qwi"
			local wn_qwi = r(N)
			dis "  Worker `w': `wn' tables (`wn_qcew' QCEW, `wn_qwi' QWI), cost = " %12.0fc `wc'
		}
	}

	qui summ `worker_cost'
	local max_cost = r(max)
	local min_cost = r(min)
	local imbalance = (`max_cost' - `min_cost') / `max_cost' * 100
	dis "  Imbalance: " %4.1f `imbalance' "% (max-min)/max"

	** Show full table assignment listing
	dis _n "  Table assignments:"
	forvalues i = 1/`=_N' {
		local t_id = table_id[`i']
		local t_phase = phase[`i']
		local t_samp = samp_var[`i']
		local t_exl = exclusion[`i']
		local t_w = worker[`i']
		local t_c = cost[`i']
		dis "    Table `t_id': `t_phase' / `t_samp' / excl=`t_exl' -> Worker `t_w' (cost=" %9.0fc `t_c' ")"
	}
	dis "==========================================" _n

	** Save table IDs in worker-sorted order for parallel processing
	sort worker table_id
	keep table_id
	save "${data}working/quarterly_table_ids.dta", replace

	restore

	** Load table IDs and run in parallel
	use "${data}working/quarterly_table_ids.dta", clear

	** Run parallel estimation
	dis "Starting parallel quarterly SDID estimation at $S_TIME..."
	timer clear 3
	timer on 3

	parallel, prog(parallel_quarterly_wrapper run_quarterly_table): parallel_quarterly_wrapper

	timer off 3
	timer list 3
	dis "Parallel quarterly estimation completed at $S_TIME"

	** Combine all treatment effect results
	dis "Combining quarterly results from parallel workers..."
	clear
	local files : dir "${results}sdid/temp_quarterly_results" files "results_*.dta"
	local first = 1

	foreach f of local files {
		if `first' == 1 {
			use "${results}sdid/temp_quarterly_results/`f'", clear
			local first = 0
		}
		else {
			append using "${results}sdid/temp_quarterly_results/`f'"
		}
	}

	** Save combined results
	drop table_id
	order sample_data sample outcome controls exclusion	///
		tau se pval ci_lower ci_upper n_counties pre_mean significant
	compress
	save "${results}sdid/quarterly/quarterly_sdid_results.dta", replace

	** Clean up temp directory
	shell rmdir "${results}sdid/temp_quarterly_results" /s /q

	** Clean up temporary files
	capture erase "${data}working/quarterly_qcew_sdid_data.dta"
	if `skip_qwi' == 0 capture erase "${data}working/quarterly_qwi_sdid_data.dta"
	capture erase "${data}working/quarterly_table_grid.dta"
	capture erase "${data}working/quarterly_table_ids.dta"

	dis "Parallel quarterly results combined and saved."

}
else {

	********************************************************************************
	** SEQUENTIAL ESTIMATION
	********************************************************************************

	** Set up results dataset
	preserve
	clear
	set obs 0
	gen sample_data = ""
	gen sample = ""
	gen outcome = ""
	gen controls = .
	gen exclusion = .
	gen tau = .
	gen se = .
	gen pval = .
	gen ci_lower = .
	gen ci_upper = .
	gen n_counties = .
	gen pre_mean = .
	gen significant = .
	save "${results}sdid/quarterly/quarterly_sdid_results.dta", replace
	clear
	restore

	** ============================================================
	** PHASE 1: QCEW ESTIMATION (SEQUENTIAL)
	** ============================================================

	dis _n "========================================"
	dis "PHASE 1: QCEW Estimation (Sequential)"
	dis "========================================"

	** Reload QCEW data
	use "${data}working/quarterly_qcew_sdid_data.dta", clear
	xtset fips yq

	** Define outcomes and labels
	local qcew_outcomes "ln_qcew_estabs ln_qcew_wage ln_qcew_emp"
	local lbl_ln_qcew_estabs	"Log Establishments (QCEW)"
	local lbl_ln_qcew_wage		"Log Total Quarterly Wages (QCEW)"
	local lbl_ln_qcew_emp 		"Log Avg. Monthly Employment (QCEW)"

	** Covariates
	local covariates "population per_capita_income"

	** Loop over samples
	foreach samp of varlist sample_all sample_urban95 sample_urban95_covid sample_demog sample_stringency {

		** Loop over exclusion of 2020
		forvalues exl = 1(-1)0 {

			** Define sample
			gen sample = `samp' == 1
			if `exl' == 1 replace sample = 0 if inrange(yq, tq(2020q1), tq(2020q4))

			** Clear stored values
			eststo clear

			** Loop over outcome variables
			foreach out of local qcew_outcomes {

				** Store label
				local label : variable label `out'

				** Loop over inclusion of covariates
				forvalues c = 0/1 {

					** Covariates
					if `c' == 0 local covars ""
					else if `c' == 1 local covars "covariates(`covariates', projected)"

					** Covariates for sdid_event
					if `c' == 0 local covars_event ""
					else if `c' == 1 local covars_event "covariates(`covariates')"

					** File Name
					if `exl' == 0 local path "${results}sdid/quarterly/fig_quarterly_`out'_`c'_`samp'_"
					if `exl' == 1 local path "${results}sdid/quarterly/fig_quarterly_`out'_`c'_`samp'_excl2020_"

					** Run SDID
					capture noisily {
						eststo sdid_`out'_`c': sdid `out' fips yq Treated	///
							if sample == 1,				///
							vce(placebo) 				///
							`covars'					///
							reps(`reps')				///
							graph graph_export("`path'", .pdf)
					}

					if _rc != 0 {
						dis "SDID failed for `out' c=`c' exl=`exl' samp=`samp'. Skipping."
						continue
					}

					** Store results
					local tmp_tau = e(ATT)
					local tmp_se = e(se)

					** Pre-treatment mean and county count
					qui summ `out' if multnomah == 1 & Treated == 0 & sample == 1
					local tmp_premean = r(mean)
					estadd scalar mean = r(mean)

					** County count: use first post-treatment quarter
					qui summ `out' if yq == tq(2021q1) & sample == 1
					local tmp_ncounties = r(N)
					estadd scalar count = r(N)

					** Save treatment effects
					preserve
					clear
					qui set obs 1
					gen sample_data = "qcew"
					gen sample = "`samp'"
					gen outcome = "`out'"
					gen controls = `c'
					gen exclusion = `exl'
					gen tau = `tmp_tau'
					gen se = `tmp_se'
					gen pval = 2 * (1 - normal(abs(tau/se)))
					gen ci_lower = tau - 1.96 * se
					gen ci_upper = tau + 1.96 * se
					gen n_counties = `tmp_ncounties'
					gen pre_mean = `tmp_premean'
					gen significant = abs(tau/se) > 1.96
					order sample_data sample outcome controls exclusion	///
						tau se pval ci_lower ci_upper n_counties pre_mean significant
					append using "${results}sdid/quarterly/quarterly_sdid_results.dta"
					compress
					save "${results}sdid/quarterly/quarterly_sdid_results.dta", replace
					clear
					restore

					** Run event study
					capture noisily {
						sdid_event `out' fips yq Treated	///
							if sample == 1,				///
							`covars_event'				///
							vce(placebo) 				///
							brep(`reps') 				///
							placebo(all)
					}

					local event_rc = _rc
					capture drop ever_treated*

					if `event_rc' == 0 {

						** Store max quarter
						qui summ yq if multnomah == 1 & sample == 1
						local max_yq = r(max)

						** Extract matrix
						qui count if multnomah == 1 & sample == 1
						local ct = r(N)
						local ct = `ct' + 1

						capture mat res = e(H)[2..`ct',1..5]
						if _rc != 0 {
							continue
						}

						** Preserve and plot
						preserve
						svmat res
						gen id = `max_yq' - _n + 1 if !missing(res1)
						format id %tq

						if `exl' == 1 {
							** Shift pre-gap IDs down by 4 quarters (they are
							** misaligned because the matrix skips 2020 Q1-Q4).
							replace id = id - 4 if id <= tq(2020q4)

							** Insert 4 missing-value markers for excluded 2020 quarters.
							local new_obs = _N + 4
							set obs `new_obs'
							forvalues eq = 0/3 {
								local row = _N - 3 + `eq'
								replace id = tq(2020q1) + `eq' in `row'
							}
						}
						label var id "Quarter"
						sort id

						twoway	(rcap res3 res4 id, lc(gs10) fc(gs11%50))	///
								(scatter res1 id, mc(black)),				///
							legend(off) ytitle("`label'")					///
							yline(0, lc("`col_zero'") lp(-))				///
							xline(`=tq(2020q4) + 0.5', lc(black) lp(solid))	///
							xlabel(, format(%tqCCYY!qq))

						if `exl' == 1 local evpath "${results}sdid/quarterly/fig_quarterly_`out'_`c'_`samp'_excl2020_eventstudy.jpg"
						else local evpath "${results}sdid/quarterly/fig_quarterly_`out'_`c'_`samp'_eventstudy.jpg"

						graph export "`evpath'", as(jpg) name("Graph") quality(100) replace
						restore
					}

				} // END COVAR LOOP

			} // END OUTCOME LOOP

			** Table of results (QCEW: 3 outcomes x 2 covariate settings = 6 columns)
			if `exl' == 0 local tabfname "tab_quarterly_qcew_`samp'.tex"
			if `exl' == 1 local tabfname "tab_quarterly_qcew_`samp'_excl2020.tex"

			local _dests `""${results}sdid/quarterly/`tabfname'""'
			if ${overleaf} == 1 {
				local _dests `"`_dests' "${ol_tab}`tabfname'""'
			}

			** Build dynamic esttab model list
			local est_list ""
			local mtitle_list ""
			local mgroup_labels ""
			local mgroup_pattern ""
			foreach out of local qcew_outcomes {
				local est_list "`est_list' sdid_`out'_0 sdid_`out'_1"
				local mtitle_list `"`mtitle_list' "No Cov." "Cov.""'
				local mgroup_labels `"`mgroup_labels' "`lbl_`out''""'
				local mgroup_pattern "`mgroup_pattern' 1 0"
			}

			foreach _outfile of local _dests {
			capture noisily {
				esttab `est_list'	///
					using "`_outfile'",								///
				starlevel("*" 0.10 "**" 0.05 "***" 0.01)		///
				b(%-9.3f) se(%-9.3f) replace 					///
				mgroups(`mgroup_labels',						///
					pattern(`mgroup_pattern'))					///
				mtitle(`mtitle_list')							///
				stats(count mean, 								///
					fmt(%9.0fc %9.3fc) 							///
					labels("Number of Counties" "Pre-treatment mean"))
			}
			} // end foreach _outfile

			** Drop sample var
			drop sample

		} // END EXCLUSION LOOP

	} // END SAMPLE LOOP

	** Clean up QCEW temp data
	capture erase "${data}working/quarterly_qcew_sdid_data.dta"


	** ============================================================
	** PHASE 2: QWI ESTIMATION (SEQUENTIAL)
	** ============================================================

	if `skip_qwi' == 0 {

	dis _n "========================================"
	dis "PHASE 2: QWI Estimation (Sequential)"
	dis "========================================"

	** Reload QWI data
	use "${data}working/quarterly_qwi_sdid_data.dta", clear
	xtset fips yq

	** Define outcomes and labels
	local qwi_outcomes "ln_qwi_emp ln_qwi_emp_ba ln_qwi_earns ln_qwi_earns_ba"
	local lbl_ln_qwi_emp      "Log Employment (All, QWI)"
	local lbl_ln_qwi_emp_ba   "Log Employment (BA+, QWI)"
	local lbl_ln_qwi_earns    "Log Total Earnings (All, QWI)"
	local lbl_ln_qwi_earns_ba "Log Total Earnings (BA+, QWI)"

	** Covariates
	local covariates "population per_capita_income"

	** Loop over samples
	foreach samp of varlist sample_all sample_urban95 sample_urban95_covid sample_demog sample_stringency {

		** Loop over exclusion of 2020
		forvalues exl = 1(-1)0 {

			** Define sample
			gen sample = `samp' == 1
			if `exl' == 1 replace sample = 0 if inrange(yq, tq(2020q1), tq(2020q4))

			** Clear stored values
			eststo clear

			** Loop over outcome variables
			foreach out of local qwi_outcomes {

				** Store label
				local label : variable label `out'

				** Loop over inclusion of covariates
				forvalues c = 0/1 {

					** Covariates
					if `c' == 0 local covars ""
					else if `c' == 1 local covars "covariates(`covariates', projected)"

					** Covariates for sdid_event
					if `c' == 0 local covars_event ""
					else if `c' == 1 local covars_event "covariates(`covariates')"

					** File Name
					if `exl' == 0 local path "${results}sdid/quarterly/fig_quarterly_`out'_`c'_`samp'_"
					if `exl' == 1 local path "${results}sdid/quarterly/fig_quarterly_`out'_`c'_`samp'_excl2020_"

					** Run SDID
					capture noisily {
						eststo sdid_`out'_`c': sdid `out' fips yq Treated	///
							if sample == 1,				///
							vce(placebo) 				///
							`covars'					///
							reps(`reps')				///
							graph graph_export("`path'", .pdf)
					}

					if _rc != 0 {
						dis "SDID failed for `out' c=`c' exl=`exl' samp=`samp'. Skipping."
						continue
					}

					** Store results
					local tmp_tau = e(ATT)
					local tmp_se = e(se)

					** Pre-treatment mean and county count
					qui summ `out' if multnomah == 1 & Treated == 0 & sample == 1
					local tmp_premean = r(mean)
					estadd scalar mean = r(mean)

					** County count
					qui summ `out' if yq == tq(2021q1) & sample == 1
					local tmp_ncounties = r(N)
					estadd scalar count = r(N)

					** Save treatment effects
					preserve
					clear
					qui set obs 1
					gen sample_data = "qwi"
					gen sample = "`samp'"
					gen outcome = "`out'"
					gen controls = `c'
					gen exclusion = `exl'
					gen tau = `tmp_tau'
					gen se = `tmp_se'
					gen pval = 2 * (1 - normal(abs(tau/se)))
					gen ci_lower = tau - 1.96 * se
					gen ci_upper = tau + 1.96 * se
					gen n_counties = `tmp_ncounties'
					gen pre_mean = `tmp_premean'
					gen significant = abs(tau/se) > 1.96
					order sample_data sample outcome controls exclusion	///
						tau se pval ci_lower ci_upper n_counties pre_mean significant
					append using "${results}sdid/quarterly/quarterly_sdid_results.dta"
					compress
					save "${results}sdid/quarterly/quarterly_sdid_results.dta", replace
					clear
					restore

					** Run event study
					capture noisily {
						sdid_event `out' fips yq Treated	///
							if sample == 1,				///
							`covars_event'				///
							vce(placebo) 				///
							brep(`reps') 				///
							placebo(all)
					}

					local event_rc = _rc
					capture drop ever_treated*

					if `event_rc' == 0 {

						** Store max quarter
						qui summ yq if multnomah == 1 & sample == 1
						local max_yq = r(max)

						** Extract matrix
						qui count if multnomah == 1 & sample == 1
						local ct = r(N)
						local ct = `ct' + 1

						capture mat res = e(H)[2..`ct',1..5]
						if _rc != 0 {
							continue
						}

						** Preserve and plot
						preserve
						svmat res
						gen id = `max_yq' - _n + 1 if !missing(res1)
						format id %tq

						if `exl' == 1 {
							** Shift pre-gap IDs down by 4 quarters (they are
							** misaligned because the matrix skips 2020 Q1-Q4).
							replace id = id - 4 if id <= tq(2020q4)

							** Insert 4 missing-value markers for excluded 2020 quarters.
							local new_obs = _N + 4
							set obs `new_obs'
							forvalues eq = 0/3 {
								local row = _N - 3 + `eq'
								replace id = tq(2020q1) + `eq' in `row'
							}
						}
						label var id "Quarter"
						sort id

						twoway	(rcap res3 res4 id, lc(gs10) fc(gs11%50))	///
								(scatter res1 id, mc(black)),				///
							legend(off) ytitle("`label'")					///
							yline(0, lc("`col_zero'") lp(-))				///
							xline(`=tq(2020q4) + 0.5', lc(black) lp(solid))	///
							xlabel(, format(%tqCCYY!qq))

						if `exl' == 1 local evpath "${results}sdid/quarterly/fig_quarterly_`out'_`c'_`samp'_excl2020_eventstudy.jpg"
						else local evpath "${results}sdid/quarterly/fig_quarterly_`out'_`c'_`samp'_eventstudy.jpg"

						graph export "`evpath'", as(jpg) name("Graph") quality(100) replace
						restore
					}

				} // END COVAR LOOP

			} // END OUTCOME LOOP

			** Table of results (QWI: 4 outcomes x 2 covariate settings = 8 columns)
			if `exl' == 0 local tabfname "tab_quarterly_qwi_`samp'.tex"
			if `exl' == 1 local tabfname "tab_quarterly_qwi_`samp'_excl2020.tex"

			local _dests `""${results}sdid/quarterly/`tabfname'""'
			if ${overleaf} == 1 {
				local _dests `"`_dests' "${ol_tab}`tabfname'""'
			}

			** Build dynamic esttab model list
			local est_list ""
			local mtitle_list ""
			local mgroup_labels ""
			local mgroup_pattern ""
			foreach out of local qwi_outcomes {
				local est_list "`est_list' sdid_`out'_0 sdid_`out'_1"
				local mtitle_list `"`mtitle_list' "No Cov." "Cov.""'
				local mgroup_labels `"`mgroup_labels' "`lbl_`out''""'
				local mgroup_pattern "`mgroup_pattern' 1 0"
			}

			foreach _outfile of local _dests {
			capture noisily {
				esttab `est_list'	///
					using "`_outfile'",								///
				starlevel("*" 0.10 "**" 0.05 "***" 0.01)		///
				b(%-9.3f) se(%-9.3f) replace 					///
				mgroups(`mgroup_labels',						///
					pattern(`mgroup_pattern'))					///
				mtitle(`mtitle_list')							///
				stats(count mean, 								///
					fmt(%9.0fc %9.3fc) 							///
					labels("Number of Counties" "Pre-treatment mean"))
			}
			} // end foreach _outfile

			** Drop sample var
			drop sample

		} // END EXCLUSION LOOP

	} // END SAMPLE LOOP

	** Clean up QWI temp data
	capture erase "${data}working/quarterly_qwi_sdid_data.dta"

	} // END skip_qwi == 0

} // END SEQUENTIAL/PARALLEL BRANCH


********************************************************************************
** EXPORT COMBINED RESULTS
********************************************************************************

use "${results}sdid/quarterly/quarterly_sdid_results.dta", clear
export excel using "${results}sdid/quarterly/quarterly_sdid_results.xlsx", firstrow(variables) replace


********************************************************************************
** SPECIFICATION CURVE ANALYSIS
********************************************************************************

/*******************************************************************************
Creates specification curve plots showing treatment effects across all
specifications for each outcome variable.

Coefficient colors:
- Sea (p7): Statistically significant (p<0.05), not preferred
- Sky (p3): Statistically insignificant, not preferred
- Vermillion (p6): Statistically significant (p<0.05), preferred specification
- Orangebrown (p8): Statistically insignificant, preferred specification

Preferred specifications: Urban 95% COVID match + covariates + excl 2020
*******************************************************************************/

** Load treatment effects
use "${results}sdid/quarterly/quarterly_sdid_results.dta", clear

** Create specification indicators for bottom panel
gen spec_all = sample == "sample_all"
gen spec_urban95 = sample == "sample_urban95"
gen spec_covid = sample == "sample_urban95_covid"
gen spec_demog = sample == "sample_demog"
gen spec_stringency = sample == "sample_stringency"
gen spec_covars = controls == 1
gen spec_excl2020 = exclusion == 1

** Calculate statistical significance (p < 0.05)
replace significant = pval < 0.05 if missing(significant)

** Define preferred specifications
gen preferred = 0
replace preferred = 1 if 									///
	sample == "sample_urban95_covid" &						///
	controls == 1 &											///
	exclusion == 1

** Display count of preferred specifications
dis "Number of preferred specifications: "
count if preferred == 1

** All 7 outcomes for spec curves
local all_outcomes 		"ln_qcew_estabs ln_qcew_wage ln_qcew_emp ln_qwi_emp ln_qwi_emp_ba ln_qwi_earns ln_qwi_earns_ba"
local lbl_ln_qcew_estabs	"Log Establishments (QCEW)"
local lbl_ln_qcew_wage		"Log Total Quarterly Wages (QCEW)"
local lbl_ln_qcew_emp		"Log Avg. Monthly Employment (QCEW)"
local lbl_ln_qwi_emp     	"Log Employment (All, QWI)"
local lbl_ln_qwi_emp_ba  	"Log Employment (BA+, QWI)"
local lbl_ln_qwi_earns    	"Log Total Earnings (All, QWI)"
local lbl_ln_qwi_earns_ba 	"Log Total Earnings (BA+, QWI)"

foreach out of local all_outcomes {

	** Preserve full data
	preserve

	** Keep only this outcome
	keep if outcome == "`out'"

	** Check if we have data
	qui count
	if r(N) == 0 {
		restore
		continue
	}

	** Sort by effect size and create rank
	sort tau
	gen spec_rank = _n
	local n_specs = _N

	** Create variables for significance and preferred-based coloring
	** Significant, not preferred
	gen tau_sig_notpref = tau if significant == 1 & preferred == 0
	gen ci_lo_sig_notpref = ci_lower if significant == 1 & preferred == 0
	gen ci_hi_sig_notpref = ci_upper if significant == 1 & preferred == 0

	** Insignificant, not preferred
	gen tau_insig_notpref = tau if significant == 0 & preferred == 0
	gen ci_lo_insig_notpref = ci_lower if significant == 0 & preferred == 0
	gen ci_hi_insig_notpref = ci_upper if significant == 0 & preferred == 0

	** Significant, preferred
	gen tau_sig_pref = tau if significant == 1 & preferred == 1
	gen ci_lo_sig_pref = ci_lower if significant == 1 & preferred == 1
	gen ci_hi_sig_pref = ci_upper if significant == 1 & preferred == 1

	** Insignificant, preferred
	gen tau_insig_pref = tau if significant == 0 & preferred == 1
	gen ci_lo_insig_pref = ci_lower if significant == 0 & preferred == 1
	gen ci_hi_insig_pref = ci_upper if significant == 0 & preferred == 1

	** Upper panel: Coefficient plot with CIs
	twoway 	(rcap ci_lo_sig_notpref ci_hi_sig_notpref spec_rank, 		///
				lc("`col_sig_notpref'") lw(vthin)) 						///
			(rcap ci_lo_insig_notpref ci_hi_insig_notpref spec_rank, 	///
				lc("`col_insig_notpref'") lw(vthin)) 					///
			(rcap ci_lo_sig_pref ci_hi_sig_pref spec_rank, 				///
				lc("`col_sig_pref'") lw(thin)) 							///
			(rcap ci_lo_insig_pref ci_hi_insig_pref spec_rank, 			///
				lc("`col_insig_pref'") lw(thin)) 						///
			(scatter tau_sig_notpref spec_rank, 						///
				mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
			(scatter tau_insig_notpref spec_rank, 						///
				mc("`col_insig_notpref'") ms(O) msize(vsmall)) 		///
			(scatter tau_sig_pref spec_rank, 							///
				mc("`col_sig_pref'") ms(D) msize(small)) 				///
			(scatter tau_insig_pref spec_rank, 							///
				mc("`col_insig_pref'") ms(D) msize(small)), 			///
		legend(order(5 "Sig. (p<0.05)" 6 "Insig." 						///
					 7 "Sig., Preferred" 8 "Insig., Preferred") 		///
			   rows(1) pos(6) size(vsmall)) 							///
		ytitle("Treatment Effect", size(vsmall)) 						///
		ylabel(, labsize(vsmall))										///
		xtitle("") 														///
		title("`lbl_`out''", size(medium)) 								///
		yline(0, lc("`col_zero'") lp(dash)) 							///
		xlabel(none) 													///
		xscale(range(0.5 `=`n_specs'+0.5'))								///
		plotregion(margin(l+12))										///
		name(coef_`out', replace)

	** Lower panel: Specification indicators
	gen y_all = -1 if spec_all == 1
	gen y_urban = -2 if spec_urban95 == 1
	gen y_covid = -3 if spec_covid == 1
	gen y_demog = -4 if spec_demog == 1
	gen y_stringency = -5 if spec_stringency == 1
	gen y_covars = -6 if spec_covars == 1
	gen y_excl = -7 if spec_excl2020 == 1

	twoway 	(scatter y_all spec_rank, mc("`col_sig_notpref'") ms(O) msize(vsmall))		///
			(scatter y_urban spec_rank, mc("`col_sig_notpref'") ms(O) msize(vsmall))	///
			(scatter y_covid spec_rank, mc("`col_sig_notpref'") ms(O) msize(vsmall))	///
			(scatter y_demog spec_rank, mc("`col_sig_notpref'") ms(O) msize(vsmall))	///
			(scatter y_stringency spec_rank, mc("`col_sig_notpref'") ms(O) msize(vsmall))	///
			(scatter y_covars spec_rank, mc("`col_sig_notpref'") ms(O) msize(vsmall))	///
			(scatter y_excl spec_rank, mc("`col_sig_notpref'") ms(O) msize(vsmall)),	///
		legend(off)														///
		ytitle("")														///
		xtitle("Specification (ranked by effect size)")					///
		ylabel(	-1 "All Counties"										///
				-2 "Urban 95%"											///
				-3 "COVID Match"										///
				-4 "Demog. Match"										///
				-5 "Stringency"											///
				-6 "Covariates"											///
				-7 "Excl. 2020",										///
			angle(0) labsize(vsmall))									///
		xlabel(none)													///
		xscale(range(0.5 `=`n_specs'+0.5'))								///
		name(spec_`out', replace)

	** Combine panels
	graph combine coef_`out' spec_`out',								///
		cols(1)															///
		xcommon															///
		imargin(zero)

	** Export combined figure
	graph export "${results}sdid/quarterly/fig_speccurve_quarterly_`out'.pdf", replace
	graph export "${results}sdid/quarterly/fig_speccurve_quarterly_`out'.jpg", as(jpg) quality(100) replace
	if ${overleaf} == 1 {
		graph export "${ol_fig}fig_speccurve_quarterly_`out'.pdf", replace
	}

	** Clean up
	graph drop coef_`out' spec_`out'

	restore

} // END OUTCOME LOOP


********************************************************************************
** FINISH
********************************************************************************

dis ""
dis "=============================================="
dis "QUARTERLY SDID ANALYSIS COMPLETE"
dis "=============================================="
dis "Results saved to:"
dis "  - ${results}sdid/quarterly/quarterly_sdid_results.dta"
dis "  - ${results}sdid/quarterly/tab_quarterly_qcew_*.tex"
dis "  - ${results}sdid/quarterly/tab_quarterly_qwi_*.tex"
dis "  - ${results}sdid/quarterly/fig_quarterly_*_eventstudy.jpg"
dis "  - ${results}sdid/quarterly/fig_speccurve_quarterly_*.pdf"
dis "=============================================="

** Close log
clear
log close log_02_quarterly
