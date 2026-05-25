/*******************************************************************************
File Name: 		02_sdid_analysis.do
Creator: 		John Iselin
Date Update:	January 2026

Called by: 00_multnomah.do

Purpose: Perform synthetic difference-in-difference estimation.
         Supports both sequential and parallel processing modes.

Requirements (for parallel mode):
- parallel package: net install parallel, from(https://raw.github.com/gvegayon/parallel/stable/) replace

Outputs:
- sdid_results.dta/xlsx: Treatment effects for each specification
- sdid_results.dta/xlsx: Treatment effects, SEs, p-values for each specification
- sdid_event_results.dta: Machine-readable event-study coefficients
- fig_speccurve_*.pdf/jpg: Specification curve plots

Authors: John Iselin

For more information, contact john.iselin@yale.edu

*******************************************************************************/


** Load shared project defaults and helper programs
if "${code}" == "" {
	local _cwd = subinstr("`c(pwd)'", "\", "/", .)
	if regexm("`_cwd'", "(.*)/code/stata$") global code "`_cwd'/"
	else global code "`_cwd'/code/stata/"
}
do "${code}00_stata_config.do"
** 01a_programs.do is normally sourced by 00_multnomah.do; source defensively
** so standalone invocation also has project_parse_outcome_components.
do "${code}01a_programs.do"
do "${code}02_spec_engine.do"

** Start log file
capture log close log_02
log using "${logs}02_log_sdid_${date}", replace text name(log_02)

********************************************************************************
** CONFIGURATION
********************************************************************************

** plotplainblind palette (RGB) — pulled from globals set in 00_stata_config.do
** so both SDID spec curves and elasticity spec curves share one palette.
local col_sig_notpref   "${col_sig_notpref}"
local col_insig_notpref "${col_insig_notpref}"
local col_sig_pref      "${col_sig_pref}"
local col_insig_pref    "${col_insig_pref}"
local col_zero          "${col_zero}"
local col_ref           "${col_ref}"

** Number of bootstrap replications for SDID
local reps = 100

project_set_seed, context("02_sdid_analysis.do") offset(10)

** Optional: rerun only a subset of data blocks (e.g., "acs_period_2")
local data_vars "irs_sample_1 irs_sample_2 acs_period_1 acs_period_2"
if "${sdid_data_filter}" != "" local data_vars "${sdid_data_filter}"

** Initialize parallel processing if enabled
if ${use_parallel} == 1 {
	parallel initialize ${n_clusters}, force
}

********************************************************************************
** DATA PREPARATION
********************************************************************************

** Load data
use "${data}working/irs_county_gross", replace

** Keep required variables
keep year fips state* county* *_net_3 *_out_1 *_out_2 *_in_3 *_out_3 *_net_5 *_in_5 *_out_5
order year fips state* county*

** Merge with ACS Data
merge 1:1 year fips using "${data}working/acs_county_gross_25plus", gen(merge_acs_1)

** Keep required variables
keep year fips state* county* *_net_3 *_out_1 *_out_2 *_in_3 *_out_3 *_net_5 *_in_5 *_out_5 merge_acs_*

** Label acs samples
rename persons_* acs1_persons_*
rename households_* acs1_households_*
rename dollars_* acs1_dollars_*

** Merge with ACS Data
merge 1:1 year fips using "${data}working/acs_county_gross_college", gen(merge_acs_2)

** Keep required variables
keep year fips state* county* *_net_3 *_out_1 *_out_2 *_in_3 *_out_3 *_net_5 *_in_5 *_out_5 merge_acs_*

** Label acs samples
rename persons_* acs2_persons_*
rename households_* acs2_households_*
rename dollars_* acs2_dollars_*

** Drop "other counties"
drop if county_fips == 0
drop if year < 2016					// Sample: 2016-2024 (IRS/ACS data start 2012)

** Merge with Demographic data
merge m:1 fips using "${data}working/demographics_2020", 	///
	gen(demo_merge) keep(master match)

** Show match
tab state_name demo_merge, m
tab year demo_merge, m
project_report_merge, gen(demo_merge) tag("demographics_2020") keep_merge

** Keep if matched
keep if demo_merge == 3
drop demo_merge

** Rename
rename population pop_census

** Merge with BEA Economics (time-varying population + PCI)
merge m:1 year fips using "${data}working/bea_economics", 	///
	gen(econ_merge) keep(master match)

** Show match
tab state_name econ_merge, m
tab year econ_merge, m
project_report_merge, gen(econ_merge) tag("bea_economics") keep_merge

** Keep if matched
keep if econ_merge == 3
drop econ_merge

** Merge with COVID-19 Data
merge m:1 fips using "${data}working/covid_cleaned_wide.dta", 	///
	gen(covid_merge) keep(master match )
project_report_merge, gen(covid_merge) tag("covid_wide") keep_merge

** Show match
tab state_name covid_merge, m
tab year covid_merge, m

** Merge with Property Tax Rates (time-varying)
merge m:1 year fips using "${data}working/property_tax_rates_overall", ///
	gen(proptx_merge) keep(master match) keepusing(prop_rate_mean)

** Show match
tab state_name proptx_merge, m
tab year proptx_merge, m
project_report_merge, gen(proptx_merge) tag("property_tax")

** Rename for clarity
rename prop_rate_mean prop_tax_rate
label var prop_tax_rate "Mean property tax rate (% of home value)"

** Merge with Census Age Shares (time-invariant, ACS 2015-2019)
merge m:1 fips using "${data}working/age_shares_county", ///
	gen(age_merge) keep(master match)

** Show match
tab state_name age_merge, m
project_report_merge, gen(age_merge) tag("age_shares")

** Organize data
order year fips state_* county_*
sort fips year
isid fips year

** Keep only sample with non-missing base populations
tab county_name year if (missing(n1_out_1 ) | n1_out_1 == 0 ) & year <= 2022
drop if (missing(n1_out_1 ) | n1_out_1 == 0 ) & year <= 2022

** Keep only counties with observations in every IRS year (2016-2022)
** Note: balanced panel is required for SDID estimation. IRS-only
**       counties span 2016-2022; ACS-matched counties extend to 2024.
**       Require IRS-period balance for all counties here; ACS-period
**       balance is enforced separately via acs_period indicators below.
bysort fips: egen ct_irs = total(inrange(year, 2016, 2022))
local n_irs_years = 2022 - 2016 + 1
drop if ct_irs < `n_irs_years'
drop ct_irs

** Generate IRS sample
gen irs_sample_1 = inrange(year, 2016, 2022)
gen irs_sample_2 = inrange(year, 2016, 2022) & merge_acs_1 != 1

** Generate ACS Period Indicators
gen acs_period_1 = merge_acs_1 != 1 & inrange(year, 2016, 2022)
gen acs_period_2 = merge_acs_1 != 1 & inrange(year, 2016, 2024)

** Make sure we have a balanced panel of ACS counties
gen tmp = merge_acs_1 != 1
bysort fips: egen ct_tmp = total(tmp)
qui summ ct_tmp
replace acs_period_1 = 0 if ct_tmp < `r(max)'
replace acs_period_2 = 0 if ct_tmp < `r(max)'
replace irs_sample_2 = 0 if ct_tmp < `r(max)'
drop tmp ct_tmp

tab year irs_sample_1
tab year irs_sample_2
tab year acs_period_1
tab year acs_period_2
tab state_name acs_period_1

** Define treated state
gen multnomah = state_fips == 41 & county_fips == 51
label var multnomah "Indicator for Multnomah County, Oregon"

** Define treatment indicator
gen Treated = multnomah == 1 & year > 2020
label var Treated "Treatment indicator for Multnomah County, Oregon"

** Define sample 1: All counties
gen sample_all = 1
label var sample_all "All counties (excluding AK, CA, HI OR, WA)"

** Define sample 2: Counties in top 95 percent
qui summ percent_urban if year == 2020, de
local cutoff = r(p95)
tab state_name multnomah if percent_urban >= `cutoff' & year == 2020
gen sample_urban95 = percent_urban >= `cutoff' // All counties
label var sample_urban95 "Urban counties (top 5%) (excluding AK, CA, HI OR, WA)"
tab sample_urban95 if year == 2020

** Compute top-25% urban threshold for clustered donor pools
** Note: must be computed before state drops (like sample_urban95) so
**       Multnomah is evaluated against the full county distribution
qui summ percent_urban if year == 2020, de
local p75 = r(p75)
gen urban_top75 = percent_urban >= `p75'

** Define sample 6 (narrow): 20 similar cities + Multnomah from Harvard Growth
** Lab Metroverse similar-cities tool. Defined BEFORE the state drops so the
** narrow-keepers (Sacramento, Seattle) survive the drops below. Clark/WA
** (Vancouver) is intentionally excluded from the narrow pool: PFA-induced
** commuter and short-range migration between Multnomah and Vancouver spills
** directly into the donor county and would violate SUTVA.
** Source of truth: resources/narrow_pool_fips.csv (edit there, not here).
load_narrow_pool

** Flag narrow-only keeper counties (Sacramento, Seattle) so the other 5
** pools and the k-means clustering steps below can exclude them. Multnomah
** itself is in narrow but should still appear in every other pool, so the
** flag is restricted to non-Multnomah CA/WA/OR narrow members.
gen narrow_only = sample_narrow == 1 & multnomah == 0 & ///
    inlist(state_name, "California", "Washington", "Oregon")
label var narrow_only "County belongs only to narrow pool (CA/WA/OR keeper)"

** Define Sample of States. Drop AK/HI unconditionally; for CA/WA/OR keep
** narrow-pool members so the similar-cities donor frame is preserved.
drop if state_name == "Alaska"
drop if state_name == "Hawaii"
drop if state_name == "California" & sample_narrow == 0
drop if state_name == "Washington" & sample_narrow == 0
drop if state_name == "Oregon" & multnomah == 0 & sample_narrow == 0

** Force the five main pool indicators to 0 for narrow-only keepers so they
** are restricted to the narrow donor pool only. The kmeans-based pools below
** add `narrow_only == 0` to their cluster-input cells for the same reason.
replace sample_all     = 0 if narrow_only == 1
replace sample_urban95 = 0 if narrow_only == 1

** Define sample 3: Counties in top 95 + covid
cluster kmeans cases_cum* deaths_cum* if 	///
	urban_top75 == 1 & year == 2020 & covid_merge == 3 & narrow_only == 0, k(5) gen(kmean)
bysort fips: egen kmean_group = mean(kmean)

** Pull out kmeans cluster with Multnomah
gen tmp1 = kmean if urban_top75 == 1 & year == 2020 & covid_merge == 3 & multnomah == 1
egen tmp2 = mean(tmp1)
gen sample_urban75_covid = urban_top75 == 1 & kmean_group == tmp2 & narrow_only == 0
drop tmp1 tmp2
label var sample_urban75_covid "Urban counties (top 25%) w. COVID k-means match (excluding AK, CA, HI OR, WA)"
tab sample_urban75_covid if year == 2020

** Define sample 4: Demographic k-means
** Standardize clustering inputs (pre-treatment per-capita income + population + urban share + age shares)
gen pci_pre = per_capita_income if year == 2020
bysort fips: egen pci_pre_fill = mean(pci_pre)
drop pci_pre
rename pci_pre_fill pci_pre

** Generate tag for not missing values 
gen not_missing = 1 

foreach v in pci_pre pop_census share_under_24 share_over_65 percent_urban {
	egen std_`v' = std(`v') if year == 2020
	replace not_missing = 0 if missing(`v') 
}

cluster kmeans std_pci_pre std_pop_census std_share_under_24 std_share_over_65 std_percent_urban if ///
	year == 2020 & not_missing == 1 & narrow_only == 0, k(10) gen(kmean_demog)
bysort fips: egen kmean_demog_group = mean(kmean_demog)

** Pull out kmeans cluster containing Multnomah
gen tmp1 = kmean_demog if year == 2020 & multnomah == 1
egen tmp2 = mean(tmp1)
gen sample_demog = kmean_demog_group == tmp2 & narrow_only == 0
drop tmp1 tmp2 std_* pci_pre
label var sample_demog "Counties with Demographic Kmean Match (excluding AK, CA, HI OR, WA)"
tab sample_demog if year == 2020

** Define sample 5: COVID stringency k-means match (JII restriction-duration)
merge m:1 fips using "${data}working/jii_stringency.dta", gen(jii_merge) keep(master match)
project_report_merge, gen(jii_merge) tag("jii_stringency") keep_merge

** Standardize 5 stringency vars within urban top-75%
foreach v in msahodays restclosedays gatherbandays strictgatherbandays maskpubdays {
	egen std_`v' = std(`v') if urban_top75 == 1 & year == 2020 & jii_merge == 3
}


** K-means on standardized stringency measures
cluster kmeans std_msahodays std_restclosedays std_gatherbandays 	///
	std_strictgatherbandays std_maskpubdays if 						///
	urban_top75 == 1 & year == 2020 & jii_merge == 3 & narrow_only == 0, k(5) gen(kmean_string)
bysort fips: egen kmean_string_group = mean(kmean_string)

** Identify Multnomah's cluster
gen tmp1 = kmean_string if urban_top75 == 1 & year == 2020 & jii_merge == 3 & multnomah == 1
egen tmp2 = mean(tmp1)
gen sample_stringency = urban_top75 == 1 & kmean_string_group == tmp2 & narrow_only == 0
drop tmp1 tmp2 std_* urban_top75 kmean_string kmean_string_group
label var sample_stringency "Urban counties (top 25%) w. COVID stringency k-means match"
tab sample_stringency if year == 2020

** Define and standardize covariates
** Note: prop_tax_rate added for non-IRS full sample specifications
local all_covariates "population per_capita_income prop_tax_rate"
foreach v of local all_covariates {
	egen tmp_v = std(`v')
	replace `v' = tmp_v
	drop tmp_v
} // END COVAR LOOP

** Define outcome variables (IRS)
foreach x in "n1" "n2" "agi" {

	if "`x'" == "n1" local xtxt "returns"
	else if "`x'" == "n2" local xtxt "exemptions"
	else if "`x'" == "agi" local xtxt "AGI"

	** Loop over migration type
	foreach y in "net" "in" "out" {

			if "`y'" == "net" local ytxt "Net domestic migration"
			else if "`y'" == "in" local ytxt "Domestic in-migration"
			else if "`y'" == "out" local ytxt "Domestic out-migration"

			** Generate
			gen `x'_`y'_rate_irs = 100 * (`x'_`y'_3 / (`x'_out_1 + `x'_out_2))

			** Label var
			label var `x'_`y'_rate_irs	"`ytxt' rate, `xtxt' (%)"

	} // END MIGRATION TYPE LOOP

} // END OUTCOME TYPE LOOP

** Define outcome variables (IRS, out-of-state movers - type 5)
foreach x in "n1" "n2" "agi" {

	if "`x'" == "n1" local xtxt "returns"
	else if "`x'" == "n2" local xtxt "exemptions"
	else if "`x'" == "agi" local xtxt "AGI"

	** Loop over migration type
	foreach y in "net" "in" "out" {

			if "`y'" == "net" local ytxt "Net out-of-state migration"
			else if "`y'" == "in" local ytxt "Out-of-state in-migration"
			else if "`y'" == "out" local ytxt "Out-of-state out-migration"

			** Generate
			gen `x'_`y'_rate_irs_outstate = 100 * (`x'_`y'_5 / (`x'_out_1 + `x'_out_2))

			** Label var
			label var `x'_`y'_rate_irs_outstate	"`ytxt' rate, `xtxt' (%)"

	} // END MIGRATION TYPE LOOP

} // END OUTCOME TYPE LOOP (IRS OUTSTATE)

** Define outcome variables (ACS)

** Rename for loop
rename acs*_households_* acs*_n1_*
rename acs*_persons_* acs*_n2_*
rename acs*_dollars_* acs*_agi_*

** Loop over sample
** (25+ = 1, college degree == 2, no college degree == 3)
forvalues i = 1/2{

	if `i' == 1 local itxt ""
	else if `i' == 2 local itxt " (College)"


	** Define outcome variables (ACS)
	foreach x in "n1" "n2" "agi" {

		if "`x'" == "n1" local xtxt "HHs"
		else if "`x'" == "n2" local xtxt "persons"
		else if "`x'" == "agi" local xtxt "total income"


		** Loop over migration type
		foreach y in "net" "in" "out" {

				if "`y'" == "net" local ytxt "Net domestic migration"
				else if "`y'" == "in" local ytxt "Domestic in-migration"
				else if "`y'" == "out" local ytxt "Domestic out-migration"

				** Generate
				gen `x'_`y'_rate_acs`i' = 100 * (acs`i'_`x'_`y'_3 / (acs`i'_`x'_out_1 + acs`i'_`x'_out_2))

				** Label var
				label var `x'_`y'_rate_acs`i' "`ytxt' rate, `xtxt'`itxt' (%)"

		} // END MIGRATION TYPE LOOP

	} // END OUTCOME TYPE LOOP

} // END SAMPLE LOOP

** Define outcome variables (ACS, out-of-state - type 5)
forvalues i = 1/2 {

	if `i' == 1 local itxt ""
	else if `i' == 2 local itxt " (College)"

	foreach x in "n1" "n2" "agi" {

		if "`x'" == "n1" local xtxt "HHs"
		else if "`x'" == "n2" local xtxt "persons"
		else if "`x'" == "agi" local xtxt "total income"

		** Loop over migration type
		foreach y in "net" "in" "out" {

				if "`y'" == "net" local ytxt "Net out-of-state migration"
				else if "`y'" == "in" local ytxt "Out-of-state in-migration"
				else if "`y'" == "out" local ytxt "Out-of-state out-migration"

				** Generate
				gen `x'_`y'_rate_acs`i'_outstate = 100 * (acs`i'_`x'_`y'_5 / (acs`i'_`x'_out_1 + acs`i'_`x'_out_2))

				** Label var
				label var `x'_`y'_rate_acs`i'_outstate "`ytxt' rate, `xtxt'`itxt' (%)"

		} // END MIGRATION TYPE LOOP

	} // END OUTCOME TYPE LOOP

} // END SAMPLE LOOP (ACS OUTSTATE)

** Declare panel
xtset fips year

** Label var
label var year "Year (destination)"

compress
save "${data}working/sdid_analysis_data.dta", replace
project_write_manifest using "${data}working/sdid_analysis_data_manifest.dta", ///
	artifact("sdid_analysis_data") script("02_sdid_analysis.do")


********************************************************************************
** PARALLEL MODE: DEFINE PROGRAMS AND SETUP
********************************************************************************

if ${use_parallel} == 1 {

	** Create table-level specification grid
	preserve
	clear

	** Table units are defined by:
	** - data_var (irs_sample_1, irs_sample_2, acs_period_1, acs_period_2)
	** - out_type (irs, acs1, acs2) - but tied to data_var
	** - samp_var (sample_all, sample_urban95, sample_urban75_covid, sample_demog, sample_stringency)
	** - exclusion (0, 1)
	** - migr_type (net, in, out)

	local table_id = 0

	** Initialize empty dataset
	set obs 0
	gen table_id = .
	gen data_var = ""
	gen out_type = ""
	gen out_txt = ""
	gen samp_var = ""
	gen exclusion = .
	gen migr_type = ""

	save "${data}working/table_grid.dta", replace

	** Build table grid
	foreach data in `data_vars' {

		** Different sets of outcome variable types
		if "`data'" == "irs_sample_1" | "`data'" == "irs_sample_2" {
			local out_types "irs irs_outstate"
		}
		else {
			local out_types "acs1 acs2 acs1_outstate acs2_outstate"
		}

		foreach type of local out_types {

			** Labels
			if "`data'" == "irs_sample_1" & "`type'" == "irs" local out_txt "irs_full_16_22"
			else if "`data'" == "irs_sample_1" & "`type'" == "irs_outstate" local out_txt "irs_outstate_full_16_22"
			else if "`data'" == "irs_sample_2" & "`type'" == "irs" local out_txt "irs_389_16_22"
			else if "`data'" == "irs_sample_2" & "`type'" == "irs_outstate" local out_txt "irs_outstate_389_16_22"
			else if "`data'" == "acs_period_1" & "`type'" == "acs1" local out_txt "acs_16_22_all"
			else if "`data'" == "acs_period_1" & "`type'" == "acs2" local out_txt "acs_16_22_col"
			else if "`data'" == "acs_period_1" & "`type'" == "acs1_outstate" local out_txt "acs_outstate_16_22_all"
			else if "`data'" == "acs_period_1" & "`type'" == "acs2_outstate" local out_txt "acs_outstate_16_22_col"
			else if "`data'" == "acs_period_2" & "`type'" == "acs1" local out_txt "acs_16_24_all"
			else if "`data'" == "acs_period_2" & "`type'" == "acs2" local out_txt "acs_16_24_col"
			else if "`data'" == "acs_period_2" & "`type'" == "acs1_outstate" local out_txt "acs_outstate_16_24_all"
			else if "`data'" == "acs_period_2" & "`type'" == "acs2_outstate" local out_txt "acs_outstate_16_24_col"

			foreach samp in "sample_all" "sample_urban95" "sample_urban75_covid" "sample_demog" "sample_stringency" "sample_narrow" {

				forvalues exl = 0/1 {
					foreach migr in "net" "in" "out" {

						local table_id = `table_id' + 1

						** Add row to grid
						clear
						set obs 1
						gen table_id = `table_id'
						gen data_var = "`data'"
						gen out_type = "`type'"
						gen out_txt = "`out_txt'"
						gen samp_var = "`samp'"
						gen exclusion = `exl'
						gen migr_type = "`migr'"

						append using "${data}working/table_grid.dta"
						save "${data}working/table_grid.dta", replace

					}
				}
			}
		}
	}

	** Load and verify grid
	use "${data}working/table_grid.dta", clear
	dis "Total table units: " _N
	sort table_id
	save "${data}working/table_grid.dta", replace

	restore

	** Define program to run all SDID specifications for one table
	capture program drop run_sdid_table
	program define run_sdid_table
		syntax, table_id(integer) data_path(string) results_path(string) reps(integer)

		** Color palette (must be redefined inside program scope)
		local col_zero "204 121 167"

		** Load table specification from grid
		preserve
		use "`data_path'working/table_grid.dta", clear
		keep if table_id == `table_id'

		** Extract specification parameters
		local data_var = data_var[1]
		local out_type = out_type[1]
		local out_txt = out_txt[1]
		local samp_var = samp_var[1]
		local exl = exclusion[1]
		local migr = migr_type[1]

		restore

		** Create output directory
		capture mkdir "`results_path'sdid/`out_txt'"

		** Clear stored estimates
		eststo clear

		** Loop over outcomes (n1, n2, agi) and covariate settings (0, 1)
		foreach outvar in "n1" "n2" "agi" {

			** Full outcome variable name
			local outcome "`outvar'_`migr'_rate_`out_type'"

			** Loop over covariate settings
			forvalues c = 0/1 {

				** File paths for figures
				if `exl' == 0 local path "`results_path'sdid/`out_txt'/fig_`out_txt'_`outcome'_`c'_`samp_var'_"
				if `exl' == 1 local path "`results_path'sdid/`out_txt'/fig_`out_txt'_`outcome'_`c'_`samp_var'_excl2020_"

				** ─── Skip if result already exists (resume mode) ───
				if ${resume} == 1 {
					capture confirm file "`results_path'sdid/temp_results/results_`table_id'_`outvar'_`c'.dta"
					if _rc == 0 {
						dis "RESUME: Skipping table `table_id' `outvar' c=`c' (result exists)"
						continue
					}
				}

				** Check if this spec qualifies for event study
				local run_event = 1
				if "${event_study_mode}" == "preferred" {
					local run_event = 0
					if inlist("`samp_var'", "sample_all", "sample_stringency", "sample_narrow") & `c' == 1 & `exl' == 1 {
						if "`out_txt'" == "irs_full_16_22" | "`out_txt'" == "acs_16_24_col" {
							local run_event = 1
						}
					}
				}

				** Run SDID through shared engine. fit_spec_sdid declares
				** GRAPHBASE(string asis); pass via compound quotes so the
				** quote chars don't survive into the macro and end up doubled
				** when the engine re-wraps the path in graph_export("...").
				capture noisily {
					fit_spec_sdid, sampledata("`out_txt'") sample(`samp_var') ///
						outcome(`outcome') controls(`c') exclusion(`exl') ///
						eventstudy(`run_event') reps(`reps') graphbase(`"`path'"')
				}

				if _rc != 0 {
					local _failed_rc = _rc
					dis "SDID failed for `outcome' c=`c' exl=`exl' samp=`samp_var'. Skipping."
					sdid_log_failure, rc(`_failed_rc') script("02_sdid_analysis") ///
						tableid("`table_id'") outcome("`outcome'") c(`c') exl(`exl') ///
						samp("`samp_var'") context("parallel-worker")
					continue
				}

				local tmp_tau = r(tau)
				local tmp_se = r(se)
				local tmp_premean = r(pre_mean)
				local tmp_ncounties = r(n_counties)
				local label : variable label `outcome'
				local event_ok = r(event_ok)
				if `event_ok' == 1 {
					tempname event_res
					matrix `event_res' = r(event_res)
					matrix colnames `event_res' = id res1 res3 res4
				}

				estadd scalar mean = `tmp_premean'
				estadd scalar count = `tmp_ncounties'
				eststo sdid_`outvar'_`c'

				** Save treatment effect results
				preserve
				clear
				set obs 1
				gen table_id = `table_id'
				gen sample_data = "`out_txt'"
				gen sample = "`samp_var'"
				gen outcome = "`outcome'"
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
				save "`results_path'sdid/temp_results/results_`table_id'_`outvar'_`c'.dta", replace
				restore

				if `run_event' == 1 & `event_ok' == 1 {

					preserve
					clear
					svmat double `event_res', names(col)

					if `exl' == 1 {
						expand 2 if id == 2019, gen(tag)
						replace id = 2020 if tag == 1
						replace res1 = . if tag == 1
						replace res3 = . if tag == 1
						replace res4 = . if tag == 1
					}
					label var id "Year (destination)"
					sort id

					twoway 	(rcap res3 res4 id, lc(gs10) fc(gs11%50))	///
							(scatter res1 id, mc(black)),				///
						legend(off) ytitle("`label'") 					///
						yline(0, lc("`col_zero'") lp(-)) 						///
						xline(2020.5, lc(black) lp(solid))				///
						ylabel(-10(2.5)10, format(%9.1f))

					if `exl' == 1 local evpath "`results_path'sdid/`out_txt'/fig_`out_txt'_`outcome'_`c'_`samp_var'_excl2020_eventstudy.jpg"
					else local evpath "`results_path'sdid/`out_txt'/fig_`out_txt'_`outcome'_`c'_`samp_var'_eventstudy.jpg"

					graph export "`evpath'", as(jpg) name("Graph") quality(100) replace

					gen str40 sample_data = "`out_txt'"
					gen str40 sample = "`samp_var'"
					gen str60 outcome = "`outcome'"
					gen controls = `c'
					gen exclusion = `exl'
					gen event_year = id
					gen event_tau = res1
					gen event_ci_lo = res3
					gen event_ci_hi = res4
					gen event_se = (event_ci_hi - event_ci_lo) / (2 * 1.96) ///
						if !missing(event_ci_lo) & !missing(event_ci_hi)
					gen outstate = strpos("`outcome'", "_outstate") > 0
					gen preferred = inlist("`samp_var'", "sample_all", "sample_stringency", "sample_narrow") ///
						& `c' == 1 & `exl' == 1 ///
						& inlist("`out_txt'", "irs_full_16_22", "acs_16_24_col", ///
							"irs_outstate_full_16_22", "acs_outstate_16_24_col")
					keep sample_data sample outcome controls exclusion ///
						event_year event_tau event_se event_ci_lo event_ci_hi ///
						outstate preferred
					drop if missing(event_tau)
					save "`results_path'sdid/temp_event_results/event_`table_id'_`outvar'_`c'.dta", replace
					restore

				} // END event study conditional

			} // END COVAR LOOP

		} // END OUTCOME LOOP

		** Generate table for this migration type (all 6 specs)
		if `exl' == 0 local tabpath "`results_path'sdid/`out_txt'/tab_sdid_`out_txt'_`migr'_`samp_var'.tex"
		if `exl' == 1 local tabpath "`results_path'sdid/`out_txt'/tab_sdid_`out_txt'_`migr'_`samp_var'_excl2020.tex"

		** Save table locally and to Overleaf
		local _dests `""`tabpath'""'
		if ${overleaf} == 1 {
			if `exl' == 0 local ol_tabname "tab_sdid_`out_txt'_`migr'_`samp_var'.tex"
			if `exl' == 1 local ol_tabname "tab_sdid_`out_txt'_`migr'_`samp_var'_excl2020.tex"
			local _dests `"`_dests' "${ol_tab}`ol_tabname'""'
		}

		foreach _outfile of local _dests {

		** Generate table based on data type
		if "`data_var'" == "irs_sample_1" | "`data_var'" == "irs_sample_2" {
			capture noisily {
				esttab 	sdid_n1_0 sdid_n1_1	///
						sdid_n2_0 sdid_n2_1	///
						sdid_agi_0 sdid_agi_1 ///
					using "`_outfile'",								///
					starlevel("*" 0.10 "**" 0.05 "***" 0.01)		///
					b(%-9.3f) se(%-9.3f) replace 					///
					mgroups("Returns" "Exemptions" "AGI", 			///
						pattern(1 0 1 0 1 0) )						///
					mtitle(	"No Covariates" "Covariates"			///
							"No Covariates" "Covariates"			///
							"No Covariates" "Covariates")			///
					stats(count mean, 								///
						fmt(%9.0fc %9.3fc) 							///
						labels("Number of Counties" "Pre-treatment mean"))
			}
		}
		else {
			capture noisily {
				esttab 	sdid_n1_0 sdid_n1_1	///
						sdid_n2_0 sdid_n2_1	///
						sdid_agi_0 sdid_agi_1 ///
					using "`_outfile'",								///
					starlevel("*" 0.10 "**" 0.05 "***" 0.01)		///
					b(%-9.3f) se(%-9.3f) replace 					///
					mgroups("Households" "Adults" "Household Income",	///
						pattern(1 0 1 0 1 0) )						///
					mtitle(	"No Covariates" "Covariates"			///
							"No Covariates" "Covariates"			///
							"No Covariates" "Covariates")			///
					stats(count mean, 								///
						fmt(%9.0fc %9.3fc) 							///
						labels("Number of Counties" "Pre-treatment mean"))
			}
		}

		} // end foreach _outfile

		dis "Completed table `table_id': `out_txt' / `migr' / `samp_var' / excl=`exl'"

	end

	** Define wrapper program for parallel execution.
	**
	** Each worker re-sources 02_spec_engine.do so that fit_spec_sdid /
	** load_spec_panel are pinned to the current on-disk version rather
	** than to whatever copy the parent serialized via `program list` at
	** dispatch time. That export path is fragile: a stale in-memory copy
	** in the parent (e.g. with `string asis` option types from an earlier
	** iteration) propagates as-is to every worker and produces opaque
	** rc=198 syntax errors on every spec. Re-sourcing the engine file
	** here keeps the workers synchronized with disk.
	**
	** 00_stata_config.do and 01a_programs.do are NOT re-sourced: their
	** programs are stable, their globals are forwarded by parallel, and
	** sourcing 00_stata_config.do under concurrent worker load triggers
	** sporadic rc=199 from the SSC `which` checks racing on the ado-path
	** cache. Those programs come in via parallel's prog() list instead.
	capture program drop parallel_sdid_wrapper
	program define parallel_sdid_wrapper
		capture noisily do "${code}02_spec_engine.do"
		if _rc != 0 {
			di as error "parallel_sdid_wrapper: 02_spec_engine.do failed (rc=`=_rc')"
			exit _rc
		}

		** Store all table_ids upfront (run_sdid_table will overwrite the dataset)
		local n_obs = _N
		forvalues i = 1/`n_obs' {
			local tid_`i' = table_id[`i']
		}

		** Now loop through and process each table
		forvalues i = 1/`n_obs' {
			dis "Worker processing table `tid_`i'' (`i' of `n_obs' in this chunk)"
			run_sdid_table, table_id(`tid_`i'') data_path("${data}") results_path("${results}") reps(100)
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

	** Create temp directories for results
	capture mkdir "${results}sdid/temp_results"
	capture mkdir "${results}sdid/temp_event_results"

	** Create subfolders for each output type
	foreach out_txt in "irs_full_16_22" "irs_outstate_full_16_22" "irs_389_16_22" "irs_outstate_389_16_22" "acs_16_22_all" "acs_16_22_col" "acs_16_24_all" "acs_16_24_col" "acs_outstate_16_22_all" "acs_outstate_16_22_col" "acs_outstate_16_24_all" "acs_outstate_16_24_col" {
		capture mkdir "${results}sdid/`out_txt'"
	}

	** Load table grid
	use "${data}working/table_grid.dta", clear
	local n_tables = _N
	dis "Running `n_tables' table units in parallel (each with 6 SDID specs)..."

	** Cost-balanced worker assignment via snake ordering
	** Tables vary enormously in cost (n^2 in counties). Instead of random
	** shuffling, we assign tables to workers so each gets ~equal total cost.
	preserve

	** Step 1: Count distinct counties per (sample x data) combination
	** This determines the SDID cost since computation is O(n^2) in counties
	use "${data}working/sdid_analysis_data.dta", clear

	** Compute all county counts in a single pass (avoid reloading data).
	** Note: use numeric index to avoid Stata's 32-char macro name limit
	local _idx = 0
	foreach samp in "sample_all" "sample_urban95" "sample_urban75_covid" "sample_demog" "sample_stringency" "sample_narrow" {
		foreach data_v in `data_vars' {
			local _idx = `_idx' + 1
			qui count if `samp' == 1 & `data_v' == 1 & year == 2021
			local nc_`_idx' = r(N)
		}
	}

	** Build cost lookup table from stored counts.
	** Six donor pools × n_data sample variants.
	clear
	local n_data : word count `data_vars'
	local n_combos = 6 * `n_data'
	qui set obs `n_combos'
	gen samp_var = ""
	gen data_var = ""
	gen n_counties = .
	gen cost = .

	local _idx = 0
	local row = 0
	foreach samp in "sample_all" "sample_urban95" "sample_urban75_covid" "sample_demog" "sample_stringency" "sample_narrow" {
		foreach data_v in `data_vars' {
			local _idx = `_idx' + 1
			local row = `row' + 1
			qui replace samp_var = "`samp'" in `row'
			qui replace data_var = "`data_v'" in `row'
			local nc = `nc_`_idx''
			qui replace n_counties = `nc' in `row'
			qui replace cost = `nc' * `nc' in `row'
		}
	}

	tempfile cost_lookup
	save `cost_lookup'

	** Step 2: Merge cost weights into the table grid
	use "${data}working/table_grid.dta", clear
	merge m:1 samp_var data_var using `cost_lookup', keep(master match) nogen

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
	** parallel splits via _cut = ceil(_n * k / _N), giving worker 1 the
	** first N/k rows, worker 2 the next N/k, etc.
	sort worker table_id

	** Diagnostic: show per-worker cost balance
	dis _n "=== Cost-Balanced Worker Assignment ==="
	dis "Tables: `=_N'  Workers: `k'"
	tempvar worker_cost worker_count
	bysort worker: egen `worker_cost' = total(cost)
	bysort worker: egen `worker_count' = count(table_id)

	** Display per-worker summary
	forvalues w = 1/`k' {
		qui summ `worker_cost' if worker == `w'
		local wc = r(mean)
		qui summ `worker_count' if worker == `w'
		local wn = r(mean)
		dis "  Worker `w': `wn' tables, cost = " %12.0fc `wc'
	}

	qui summ `worker_cost'
	local max_cost = r(max)
	local min_cost = r(min)
	local imbalance = (`max_cost' - `min_cost') / `max_cost' * 100
	dis "  Imbalance: " %4.1f `imbalance' "% (max-min)/max"
	dis "==========================================" _n

	** Save table IDs in worker-sorted order for parallel processing
	** Re-sort after diagnostics (bysort may have disrupted within-worker order)
	sort worker table_id
	keep table_id
	save "${data}working/table_ids.dta", replace

	restore

	** Load table IDs and run in parallel
	use "${data}working/table_ids.dta", clear

	** Run parallel estimation
	dis "Starting parallel SDID estimation at $S_TIME..."
	timer clear 1
	timer on 1

	** Widen linesize so `program list` headers like
	** "project_build_signature, rclass:" don't wrap; parallel's exporter
	** regex (parallel_export_programs.mata) requires the trailing ":" on
	** the same line, and a wrapped header is silently dropped, leaving a
	** stray `end` that crashes every worker with rc=199.
	**
	** Note: fit_spec_sdid and load_spec_panel are intentionally OMITTED
	** from prog() — the wrapper re-sources 02_spec_engine.do at the top
	** of each worker. That avoids the version-drift trap where the
	** parent's in-memory copy of those programs gets serialized to
	** workers even after the on-disk source has changed.
	local _orig_linesize = c(linesize)
	set linesize 255

	parallel, prog(parallel_sdid_wrapper run_sdid_table sdid_log_failure ///
		project_assert_manifest project_build_signature): parallel_sdid_wrapper

	set linesize `_orig_linesize'
	local parallel_rc = _rc

	timer off 1
	timer list 1
	if `parallel_rc' != 0 {
		dis as error "Parallel SDID estimation failed with rc=`parallel_rc'. Skipping combine/export to avoid overwriting outputs."
		exit `parallel_rc'
	}
	dis "Parallel estimation completed at $S_TIME"

	** Combine all treatment effect results
	dis "Combining results from parallel workers..."
	clear
	local files : dir "${results}sdid/temp_results" files "results_*.dta"
	local first = 1

	foreach f of local files {
		if `first' == 1 {
			use "${results}sdid/temp_results/`f'", clear
			local first = 0
		}
		else {
			append using "${results}sdid/temp_results/`f'"
		}
	}

	** Save combined results
	order sample_data sample outcome controls exclusion tau se pval ci_lower ci_upper n_counties pre_mean significant
	compress
	save "${results}sdid/sdid_results.dta", replace
	project_write_manifest using "${results}sdid/sdid_results_manifest.dta", ///
		artifact("sdid_results") script("02_sdid_analysis.do")
	export excel using "${results}sdid/sdid_results.xlsx", firstrow(variables) replace

	** Clean up temp directory
	shell rmdir "${results}sdid/temp_results" /s /q

	** Combine event-study results from parallel workers
	clear
	local event_files : dir "${results}sdid/temp_event_results" files "event_*.dta"
	local first_event = 1

	foreach f of local event_files {
		if `first_event' == 1 {
			use "${results}sdid/temp_event_results/`f'", clear
			local first_event = 0
		}
		else {
			append using "${results}sdid/temp_event_results/`f'"
		}
	}

	if `first_event' == 1 {
		clear
		set obs 1
		gen str40 sample_data = ""
		gen str40 sample = ""
		gen str60 outcome = ""
		gen controls = .
		gen exclusion = .
		gen event_year = .
		gen event_tau = .
		gen event_se = .
		gen event_ci_lo = .
		gen event_ci_hi = .
		gen outstate = .
		gen preferred = .
		drop in 1
	}

	if ${resume} == 1 {
		capture confirm file "${results}sdid/sdid_event_results.dta"
		if _rc == 0 {
			append using "${results}sdid/sdid_event_results.dta"
			duplicates drop sample_data sample outcome controls exclusion event_year, force
		}
	}

	order sample_data sample outcome controls exclusion event_year ///
		event_tau event_se event_ci_lo event_ci_hi outstate preferred
	compress
	save "${results}sdid/sdid_event_results.dta", replace
	project_write_manifest using "${results}sdid/sdid_event_results_manifest.dta", ///
		artifact("sdid_event_results") script("02_sdid_analysis.do")

	** Clean up event-study temp directory
	shell rmdir "${results}sdid/temp_event_results" /s /q

	** Clean up temporary files
	capture erase "${data}working/table_grid.dta"
	capture erase "${data}working/table_ids.dta"

	dis "Parallel results combined and saved."

}
else {

	********************************************************************************
	** SEQUENTIAL ESTIMATION
	********************************************************************************

	** Open postfile for results accumulation (O(1) per spec)
	capture mkdir "${results}sdid/temp_event_results"
	local pf_path "${results}sdid/sdid_results.dta"
	if ${resume} == 1 {
		local pf_path "${results}sdid/sdid_results_new.dta"
	}
	capture postclose pf_results
	tempname pf_results
	postfile `pf_results' str40(sample_data sample) str60(outcome) ///
		controls exclusion tau se pval ci_lower ci_upper            ///
		n_counties pre_mean significant                             ///
		using "`pf_path'", replace

	** ─── CHECKPOINT: load completed specs for resume mode ───
	local n_done = 0
	if ${resume} == 1 {
		capture confirm file "${results}sdid/sdid_results.dta"
		if _rc == 0 {
			preserve
			use "${results}sdid/sdid_results.dta", clear
			qui count
			local n_done = r(N)
			if `n_done' > 0 {
				** Build lookup key and store in mata associative array.
				** Drop any stale _done_set from a prior aborted run before
				** (re-)creating so results don't silently inherit old state.
				capture mata: mata drop _done_set
				gen _done_key = sample_data + "|" + sample + "|" + outcome ///
					+ "|" + string(controls, "%1.0f") + "|" + string(exclusion, "%1.0f")
				mata: _done_set = asarray_create()
				mata: for (_i=1; _i<=st_nobs(); _i++) asarray(_done_set, st_sdata(_i, "_done_key"), 1)
				dis "RESUME MODE: `n_done' specs already completed. Skipping those."
			}
			restore
		}
	}

	local event_seq = 0

	** Loop over IRS and ACS Samples
	foreach data in `data_vars' {

		** Different sets of outcome variables
		if "`data'" == "irs_sample_1" local out_type "irs irs_outstate"
		else if "`data'" == "irs_sample_2" local out_type "irs irs_outstate"
		else local out_type "acs1 acs2 acs1_outstate acs2_outstate"

		** Loop over Outcome var type
		foreach type of local out_type {

			** Labels
			if "`data'" == "irs_sample_1" & "`type'" == "irs" local out_txt "irs_full_16_22"
			else if "`data'" == "irs_sample_1" & "`type'" == "irs_outstate" local out_txt "irs_outstate_full_16_22"
			else if "`data'" == "irs_sample_2" & "`type'" == "irs" local out_txt "irs_389_16_22"
			else if "`data'" == "irs_sample_2" & "`type'" == "irs_outstate" local out_txt "irs_outstate_389_16_22"
			else if "`data'" == "acs_period_1" & "`type'" == "acs1" local out_txt "acs_16_22_all"
			else if "`data'" == "acs_period_1" & "`type'" == "acs2" local out_txt "acs_16_22_col"
			else if "`data'" == "acs_period_1" & "`type'" == "acs1_outstate" local out_txt "acs_outstate_16_22_all"
			else if "`data'" == "acs_period_1" & "`type'" == "acs2_outstate" local out_txt "acs_outstate_16_22_col"
			else if "`data'" == "acs_period_2" & "`type'" == "acs1" local out_txt "acs_16_24_all"
			else if "`data'" == "acs_period_2" & "`type'" == "acs2" local out_txt "acs_16_24_col"
			else if "`data'" == "acs_period_2" & "`type'" == "acs1_outstate" local out_txt "acs_outstate_16_24_all"
			else if "`data'" == "acs_period_2" & "`type'" == "acs2_outstate" local out_txt "acs_outstate_16_24_col"

			** Check if subfolder exists, create if not
			capture mkdir "${results}sdid/`out_txt'"

			** Loop over samples
			foreach samp of varlist sample_all sample_urban95 sample_urban75_covid sample_demog sample_stringency sample_narrow {

				** Loop over exclusion of 2020
				forvalues exl = 1(-1)0 {

					** Clear stored values
					eststo clear

					** Loop over migration type
					foreach migr in "net" "in" "out" {

						** Loop over outcomes
						foreach out of varlist	n1_`migr'_rate_`type'	///
												n2_`migr'_rate_`type' 	///
												agi_`migr'_rate_`type' {

							** Store label
							local label : variable label `out'

							** Loop over inclusion of covariates
							forvalues c = 0/1 {

								** File Name
								if `exl' == 0 local path "${results}sdid/`out_txt'/fig_`out_txt'_`out'_`c'_`samp'_"
								if `exl' == 1 local path "${results}sdid/`out_txt'/fig_`out_txt'_`out'_`c'_`samp'_excl2020_"

								** ─── Skip if already completed (resume mode) ───
								if `n_done' > 0 {
									local _this_key "`out_txt'|`samp'|`out'|`c'|`exl'"
									mata: st_local("_skip", strofreal(asarray_contains(_done_set, st_local("_this_key"))))
									if `_skip' == 1 {
										dis "RESUME: Skipping `out' c=`c' exl=`exl' samp=`samp'"
										continue
									}
								}

								** Check if this spec qualifies for event study
								local run_event = 1
								if "${event_study_mode}" == "preferred" {
									local run_event = 0
									if inlist("`samp'", "sample_all", "sample_stringency", "sample_narrow") & `c' == 1 & `exl' == 1 {
										if "`out_txt'" == "irs_full_16_22" | "`out_txt'" == "acs_16_24_col" {
											local run_event = 1
										}
									}
								}

								** Run SDID through shared engine. graphbase passed
								** via compound quotes — see note at parallel call
								** site for why.
								capture noisily {
									fit_spec_sdid, sampledata("`out_txt'") sample(`samp') ///
										outcome(`out') controls(`c') exclusion(`exl') ///
										eventstudy(`run_event') reps(`reps') graphbase(`"`path'"')
								}

								if _rc != 0 {
									local _failed_rc = _rc
									dis "SDID failed for `out' c=`c' exl=`exl' samp=`samp'. Skipping."
									sdid_log_failure, rc(`_failed_rc') script("02_sdid_analysis") ///
										tableid("`out_txt'") outcome("`out'") c(`c') exl(`exl') ///
										samp("`samp'") context("main-serial")
									continue
								}

								local tmp_tau = r(tau)
								local tmp_se = r(se)
								local tmp_premean = r(pre_mean)
								local tmp_ncounties = r(n_counties)
								local event_ok = r(event_ok)
								if `event_ok' == 1 {
									tempname event_res
									matrix `event_res' = r(event_res)
									matrix colnames `event_res' = id res1 res3 res4
								}

								estadd scalar mean = `tmp_premean'
								estadd scalar count = `tmp_ncounties'
								eststo sdid_`out'_`c'

								** Post results to postfile (O(1) append)
								local tmp_pval = 2 * (1 - normal(abs(`tmp_tau'/`tmp_se')))
								local tmp_ci_lo = `tmp_tau' - 1.96 * `tmp_se'
								local tmp_ci_hi = `tmp_tau' + 1.96 * `tmp_se'
								local tmp_sig = abs(`tmp_tau'/`tmp_se') > 1.96
								post `pf_results' ("`out_txt'") ("`samp'") ("`out'") ///
									(`c') (`exl') (`tmp_tau') (`tmp_se') (`tmp_pval') ///
									(`tmp_ci_lo') (`tmp_ci_hi') (`tmp_ncounties')     ///
									(`tmp_premean') (`tmp_sig')

								if `run_event' == 1 & `event_ok' == 1 {

									preserve
									clear
									svmat double `event_res', names(col)

									if `exl' == 1 {
										expand 2 if id == 2019, gen(tag)
										replace id = 2020 if tag == 1
										replace res1 = . if tag == 1
										replace res3 = . if tag == 1
										replace res4 = . if tag == 1
									}
									label var id "Year (destination)"
									sort id

									twoway 	(rcap res3 res4 id, lc(gs10) fc(gs11%50))	///
											(scatter res1 id, mc(black)),				///
										legend(off) ytitle("`label'") 					///
										yline(0, lc("`col_zero'") lp(-)) 						///
										xline(2020.5, lc(black) lp(solid))

									if `exl' == 0 local path "${results}sdid/`out_txt'/fig_`out_txt'_`out'_`c'_`samp'_eventstudy.jpg"
									if `exl' == 1 local path "${results}sdid/`out_txt'/fig_`out_txt'_`out'_`c'_`samp'_excl2020_eventstudy.jpg"

									graph export "`path'", 	///
										as(jpg) name("Graph") quality(100) replace

									gen str40 sample_data = "`out_txt'"
									gen str40 sample = "`samp'"
									gen str60 outcome = "`out'"
									gen controls = `c'
									gen exclusion = `exl'
									gen event_year = id
									gen event_tau = res1
									gen event_ci_lo = res3
									gen event_ci_hi = res4
									gen event_se = (event_ci_hi - event_ci_lo) / (2 * 1.96) ///
										if !missing(event_ci_lo) & !missing(event_ci_hi)
									gen outstate = strpos("`out'", "_outstate") > 0
									gen preferred = inlist("`samp'", "sample_all", "sample_stringency", "sample_narrow") ///
										& `c' == 1 & `exl' == 1 ///
										& inlist("`out_txt'", "irs_full_16_22", "acs_16_24_col", ///
											"irs_outstate_full_16_22", "acs_outstate_16_24_col")
									keep sample_data sample outcome controls exclusion ///
										event_year event_tau event_se event_ci_lo event_ci_hi ///
										outstate preferred
									drop if missing(event_tau)
									local ++event_seq
									local event_file "${results}sdid/temp_event_results/event_`event_seq'.dta"
									save "`event_file'", replace
									restore

								} // END event study conditional

							} // END COVAR LOOP

						} // END OUTCOME LOOP

						** Determine name
						if `exl' == 0 local path "${results}sdid/`out_txt'/tab_sdid_`out_txt'_`migr'_`samp'.tex"
						if `exl' == 1 local path "${results}sdid/`out_txt'/tab_sdid_`out_txt'_`migr'_`samp'_excl2020.tex"

						** Save table locally and to Overleaf
						local _dests `""`path'""'
						if ${overleaf} == 1 {
							if `exl' == 0 local ol_fname "tab_sdid_`out_txt'_`migr'_`samp'.tex"
							if `exl' == 1 local ol_fname "tab_sdid_`out_txt'_`migr'_`samp'_excl2020.tex"
							local _dests `"`_dests' "${ol_tab}`ol_fname'""'
						}

						foreach _outfile of local _dests {

						** Table of results (capture in case some eststo entries
						** are missing during a resume run)
						if "`data'" == "irs_sample_1" | "`data'" == "irs_sample_2" {

						capture noisily {
						esttab 	sdid_n1_`migr'_rate_`type'_0 sdid_n1_`migr'_rate_`type'_1	///
								sdid_n2_`migr'_rate_`type'_0 sdid_n2_`migr'_rate_`type'_1	///
								sdid_agi_`migr'_rate_`type'_0 sdid_agi_`migr'_rate_`type'_1 ///
							using "`_outfile'",							///
						starlevel("*" 0.10 "**" 0.05 "***" 0.01)		///
						b(%-9.3f) se(%-9.3f) replace 					///
						mgroups("Returns" "Exemptions" "AGI", 			///
							pattern(1 0 1 0 1 0) )						///
						mtitle(	"No Covariates" "Covariates"			///
								"No Covariates" "Covariates"			///
								"No Covariates" "Covariates")			///
						stats(count mean, 								///
							fmt(%9.0fc %9.3fc) 							///
							labels("Number of Counties" "Pre-treatment mean"))
						}
						}
						else {
						capture noisily {
						esttab 	sdid_n1_`migr'_rate_`type'_0 sdid_n1_`migr'_rate_`type'_1	///
								sdid_n2_`migr'_rate_`type'_0 sdid_n2_`migr'_rate_`type'_1	///
								sdid_agi_`migr'_rate_`type'_0 sdid_agi_`migr'_rate_`type'_1 ///
							using "`_outfile'",							///
						starlevel("*" 0.10 "**" 0.05 "***" 0.01)		///
						b(%-9.3f) se(%-9.3f) replace 					///
						mgroups("Households" "Adults" "Household Income",	///
							pattern(1 0 1 0 1 0) )						///
						mtitle(	"No Covariates" "Covariates"			///
								"No Covariates" "Covariates"			///
								"No Covariates" "Covariates")			///
						stats(count mean, 								///
							fmt(%9.0fc %9.3fc) 							///
							labels("Number of Counties" "Pre-treatment mean"))
						}

						}

						} // end foreach _outfile

					} // END MIGRATION TYPE LOOP

				} // END EXCLUSION LOOP

			} // END SAMPLE LOOP

		} // END OUT TYPE

	} // END DATA LOOP


	** Close postfile and finalize results
	postclose `pf_results'

	** Resume mode: merge new results into existing file
	if ${resume} == 1 {
		use "${results}sdid/sdid_results.dta", clear
		append using "${results}sdid/sdid_results_new.dta"
		save "${results}sdid/sdid_results.dta", replace
		capture erase "${results}sdid/sdid_results_new.dta"
	}

	** Clean up mata checkpoint lookup. `capture` prevents an error if an
	** earlier failure in the loop dropped _done_set prematurely.
	capture mata: mata drop _done_set

	** Export treatment effects
	use "${results}sdid/sdid_results.dta", clear
	project_write_manifest using "${results}sdid/sdid_results_manifest.dta", ///
		artifact("sdid_results") script("02_sdid_analysis.do")
	export excel using "${results}sdid/sdid_results.xlsx", firstrow(variables) replace

	** Combine event-study results
	clear
	local event_files : dir "${results}sdid/temp_event_results" files "event_*.dta"
	local first_event = 1

	foreach f of local event_files {
		if `first_event' == 1 {
			use "${results}sdid/temp_event_results/`f'", clear
			local first_event = 0
		}
		else {
			append using "${results}sdid/temp_event_results/`f'"
		}
	}

	if `first_event' == 1 {
		clear
		set obs 1
		gen str40 sample_data = ""
		gen str40 sample = ""
		gen str60 outcome = ""
		gen controls = .
		gen exclusion = .
		gen event_year = .
		gen event_tau = .
		gen event_se = .
		gen event_ci_lo = .
		gen event_ci_hi = .
		gen outstate = .
		gen preferred = .
		drop in 1
	}

	if ${resume} == 1 {
		capture confirm file "${results}sdid/sdid_event_results.dta"
		if _rc == 0 {
			append using "${results}sdid/sdid_event_results.dta"
			duplicates drop sample_data sample outcome controls exclusion event_year, force
		}
	}

	order sample_data sample outcome controls exclusion event_year ///
		event_tau event_se event_ci_lo event_ci_hi outstate preferred
	compress
	save "${results}sdid/sdid_event_results.dta", replace
	project_write_manifest using "${results}sdid/sdid_event_results_manifest.dta", ///
		artifact("sdid_event_results") script("02_sdid_analysis.do")

	** Clean up event-study temp directory
	shell rmdir "${results}sdid/temp_event_results" /s /q

} // END SEQUENTIAL ESTIMATION

********************************************************************************
** SPECIFICATION CURVE ANALYSIS
********************************************************************************

/*******************************************************************************
Creates specification curve plots showing treatment effects across all
specifications for each outcome type and migration direction.

Coefficient colors (plotplainblind palette):
- Sea (p7): Statistically significant (p<0.05), not preferred
- Sky (p3): Statistically insignificant, not preferred
- Vermillion (p6): Statistically significant (p<0.05), preferred specification
- Orangebrown (p8): Statistically insignificant, preferred specification

Preferred specifications are defined by the local macro `preferred_specs` below.
*******************************************************************************/

** Create output subdirectory
capture mkdir "${results}sdid/spec_curves"

** Load treatment effects
use "${results}sdid/sdid_results.dta", clear

** Parse outcome / sample_data into spec metadata + spec_* indicator
** family via the shared helper (01a_programs.do).
project_parse_outcome_components, indicators

** Calculate statistical significance (p < 0.05)
replace significant = pval < 0.05 if missing(significant)

********************************************************************************
** DEFINE PREFERRED SPECIFICATIONS
** Shared preferred-spec logic lives in 00_stata_config.do so downstream
** scripts use the exact same highlighted specifications.
********************************************************************************

project_mark_preferred_main

** Display count of preferred specifications
dis "Number of preferred specifications: "
count if preferred == 1

********************************************************************************
** CREATE SPECIFICATION CURVE PLOTS
********************************************************************************

** Loop over outcome types, migration directions, and plot sets
foreach otype in "n1" "n2" "agi" {
	foreach migr in "net" "in" "out" {
		foreach pset in "main" "outstate" {

		** Preserve full data
		preserve

		** Keep only relevant specifications
		keep if outcome_type == "`otype'" & migration == "`migr'"

		** Filter by plot set
		if "`pset'" == "main" {
			drop if inlist(data_type, "IRS (Out-of-State)", "IRS (389, Out-of-State)", ///
								     "ACS All (Out-of-State)", "ACS College (Out-of-State)")
		}
		else if "`pset'" == "outstate" {
			keep if inlist(data_type, "IRS (Out-of-State)", "IRS (389, Out-of-State)", ///
								     "ACS All (Out-of-State)", "ACS College (Out-of-State)")
		}

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

		********************************************************************************
		** Create variables for significance and preferred-based coloring
		** Four categories:
		**   1. Significant + Not Preferred (sea)
		**   2. Insignificant + Not Preferred (sky)
		**   3. Significant + Preferred (vermillion)
		**   4. Insignificant + Preferred (orangebrown)
		********************************************************************************

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

		** Count specifications in each category for legend
		qui count if significant == 1 & preferred == 0
		local n_sig_notpref = r(N)
		qui count if significant == 0 & preferred == 0
		local n_insig_notpref = r(N)
		qui count if significant == 1 & preferred == 1
		local n_sig_pref = r(N)
		qui count if significant == 0 & preferred == 1
		local n_insig_pref = r(N)

		********************************************************************************
		** Single unified specification curve plot
		** Coefficients in upper zone, specification indicators in lower zone.
		** Using a single twoway guarantees perfect x-axis alignment.
		********************************************************************************

		** Compute dynamic placement for indicator zone below coefficients
		qui su ci_lower
		local ci_min = r(min)
		qui su ci_upper
		local ci_max = r(max)

		** Separator between coefficient and indicator zones
		local sep_y = floor(`ci_min') - 1.5

		** Indicator y-positions start below separator
		local ind_top = floor(`ci_min') - 3

		** Coefficient y-axis tick range
		local tick_lo = floor(`ci_min')
		local tick_hi = ceil(`ci_max')

		** Indicator list per plot set (main = county-level, outstate =
		** out-of-state movers). The shared rendering block below builds
		** y_<ind> scatter layers and ylabel entries from this list.
		if "`pset'" == "main" {
			local indic_list spec_all spec_urban95 spec_covid spec_demog ///
				spec_stringency spec_narrow spec_covars spec_excl2020   ///
				spec_irs spec_irs_389 spec_acs_all spec_acs_col         ///
				spec_16_22 spec_16_24
		}
		else {
			local indic_list spec_all spec_urban95 spec_covid spec_demog ///
				spec_stringency spec_narrow spec_covars spec_excl2020   ///
				spec_irs_outstate spec_irs_outstate_389                 ///
				spec_acs_all_outstate spec_acs_col_outstate             ///
				spec_16_22 spec_16_24
		}

		** Label dictionary shared with elast_speccurve_plot. Outstate
		** entries reuse the county-level wording; the figure title carries
		** the out-of-state qualifier so the indicator-panel row labels stay
		** identical across psets.
		local lbl_spec_all              `"All Counties"'
		local lbl_spec_urban95          `"Urban (Top 5%)"'
		local lbl_spec_covid            `"COVID Match"'
		local lbl_spec_demog            `"Demographic Match"'
		local lbl_spec_stringency       `"Stringency Match"'
		local lbl_spec_narrow           `"Narrow Pool"'
		local lbl_spec_covars           `"Covariates"'
		local lbl_spec_excl2020         `"Excl. 2020"'
		local lbl_spec_irs              `"IRS (all counties)"'
		local lbl_spec_irs_389          `"IRS (ACS counties)"'
		local lbl_spec_irs_outstate     `"IRS (all counties)"'
		local lbl_spec_irs_outstate_389 `"IRS (ACS counties)"'
		local lbl_spec_acs_all          `"ACS All"'
		local lbl_spec_acs_all_outstate `"ACS All"'
		local lbl_spec_acs_col          `"ACS College"'
		local lbl_spec_acs_col_outstate `"ACS College"'
		local lbl_spec_16_22            `"16-22"'
		local lbl_spec_16_24            `"16-24"'

		** Build the indicator-row scatter layers and ylabel entries in a
		** single loop. yp positions descend from ind_top with a 1-unit gap.
		local ind_layers `""'
		local ind_ylabels `""'
		local _row = 0
		foreach ind of local indic_list {
			local ++_row
			local _yp = `ind_top' - `_row' + 1
			capture drop y_`ind'
			gen double y_`ind' = `_yp' if `ind' == 1
			local ind_layers `"`ind_layers' (scatter y_`ind' spec_rank, mc("`col_sig_notpref'") ms(O) msize(vsmall))"'
			local ind_ylabels `"`ind_ylabels' `_yp' "`lbl_`ind''""'
		}

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
					mc("`col_insig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter tau_sig_pref spec_rank, 							///
					mc("`col_sig_pref'") ms(D) msize(small)) 				///
				(scatter tau_insig_pref spec_rank, 							///
					mc("`col_insig_pref'") ms(D) msize(small)) 				///
				`ind_layers'                                              	///
			, yline(`sep_y', lc(gs12) lp(solid) lw(vthin)) 					///
			  yline(0, lc("`col_zero'") lp(dash)) 							///
			  ylabel(`tick_lo'(1)`tick_hi', labsize(vsmall) nogrid) 		///
			  ylabel(`ind_ylabels'                                          ///
				, labsize(vsmall) angle(0) notick nogrid add) 				///
			  legend(order(5 "Sig. (p<0.05)" 6 "Insig." 					///
						   7 "Sig., Preferred" 8 "Insig., Preferred") 		///
					 rows(1) pos(6) size(vsmall)) 							///
			  ytitle("Treatment Effect (pp)", size(vsmall)) 				///
			  xtitle("Specification (ranked by effect size)", size(vsmall)) ///
			  xlabel(none) 													///
			  xscale(range(0.5 `=`n_specs'+0.5')) 							///
			  graphregion(color(white)) 									///
			  name(speccurve_`otype'_`migr', replace)

		** File suffix for out-of-state plots
		if "`pset'" == "outstate" local fsuffix "_outstate"
		else local fsuffix ""

		** Export figure
		graph export "${results}sdid/spec_curves/fig_speccurve_`otype'_`migr'`fsuffix'.pdf", replace
		graph export "${results}sdid/spec_curves/fig_speccurve_`otype'_`migr'`fsuffix'.jpg", as(jpg) quality(100) replace
		if ${overleaf} == 1 {
			graph export "${ol_fig}fig_speccurve_`otype'_`migr'`fsuffix'.pdf", replace
		}

		** Clean up
		graph drop speccurve_`otype'_`migr'

		restore

		} // END PLOT SET LOOP
	} // END MIGRATION LOOP
} // END OUTCOME TYPE LOOP

********************************************************************************
** META-REGRESSION: SPECIFICATION INFLUENCE ANALYSIS
********************************************************************************

/*
Treat the SDID results as a dataset and run OLS regressions with
tau (treatment effect) as the dependent variable and specification
choice indicators as regressors. Produces coefplots showing which
researcher decisions (donor pool, data source, covariates, etc.)
drive the most variation in estimated treatment effects.
*/

** Palette for influence coefplots
local col_pool    "0 114 178"     // sea (p7)
local col_data    "213 94 0"      // vermillion (p6)
local col_other   "0 158 115"     // bluish green
local col_zero    "153 153 153"   // gs10 (p2)

** Create output subdirectory
capture mkdir "${results}sdid/influence"

** Reload clean results (spec curve section modified the data)
use "${results}sdid/sdid_results.dta", clear

** Parse outcome type from variable name
gen outcome_type = ""
replace outcome_type = "n1"  if strpos(outcome, "n1_") > 0
replace outcome_type = "n2"  if strpos(outcome, "n2_") > 0
replace outcome_type = "agi" if strpos(outcome, "agi_") > 0

** Parse migration direction
gen migration = ""
replace migration = "net" if strpos(outcome, "_net_") > 0
replace migration = "in"  if strpos(outcome, "_in_") > 0
replace migration = "out" if strpos(outcome, "_out_") > 0

** 1. Donor pool (6 levels, base = All Counties)
gen donor_pool = .
replace donor_pool = 1 if sample == "sample_all"
replace donor_pool = 2 if sample == "sample_urban95"
replace donor_pool = 3 if sample == "sample_urban75_covid"
replace donor_pool = 4 if sample == "sample_demog"
replace donor_pool = 5 if sample == "sample_stringency"
replace donor_pool = 6 if sample == "sample_narrow"
label define donor_pool_lbl 1 "All Counties" 2 "Urban 95%" 			///
	3 "COVID Match" 4 "Demog. Match" 5 "Stringency" 6 "Narrow"
label values donor_pool donor_pool_lbl

** 2. Data source (4 levels, base = IRS Full)
gen data_src = .
replace data_src = 1 if strpos(sample_data, "_full_") > 0
replace data_src = 2 if strpos(sample_data, "_389_") > 0
replace data_src = 3 if strpos(sample_data, "acs_") > 0 			///
	& strpos(sample_data, "_all") > 0
replace data_src = 4 if strpos(sample_data, "acs_") > 0 			///
	& strpos(sample_data, "_col") > 0
label define data_src_lbl 1 "IRS (Full)" 2 "IRS (ACS Counties)" 	///
	3 "ACS (All 25+)" 4 "ACS (College)"
label values data_src data_src_lbl

** 3. Out-of-state movers (binary)
gen outstate = strpos(sample_data, "outstate") > 0
label var outstate "Out-of-State Movers"

** 4. Extended period (binary: 16-24 vs 16-22)
gen period_1624 = strpos(sample_data, "16_24") > 0
label var period_1624 "Extended Period (16-24)"

** 5. Label existing binary variables
label var controls "With Covariates"
label var exclusion "Exclude 2020"

** Verify no missing specification choices
assert !missing(donor_pool)
assert !missing(data_src)

** Summary of specification choices
dis _n "Specification choice summary:"
tab donor_pool
tab data_src
tab outstate
tab period_1624
tab controls
tab exclusion

** Loop over geographic scope, outcome types, and migration directions
** Run meta-regressions separately for out-of-county and out-of-state
foreach geo in 0 1 {

	if `geo' == 0 {
		local geo_label "Out-of-County"
		local geo_suffix ""
	}
	else {
		local geo_label "Out-of-State"
		local geo_suffix "_outstate"
	}

	dis _n "============================================================"
	dis "META-REGRESSION: `geo_label' Estimates"
	dis "============================================================"

foreach otype in "n1" "n2" "agi" {
	foreach migr in "out" "in" "net" {

		** Labels for titles
		if "`otype'" == "n1"  local otype_label "Returns"
		else if "`otype'" == "n2"  local otype_label "Exemptions"
		else if "`otype'" == "agi" local otype_label "AGI"

		if "`migr'" == "net" local migr_label "Net Migration"
		else if "`migr'" == "in"  local migr_label "In-Migration"
		else if "`migr'" == "out" local migr_label "Out-Migration"

		** Preserve and subset to geographic scope + outcome + migration
		preserve
		keep if outcome_type == "`otype'" & migration == "`migr'" ///
			& outstate == `geo'

		** Check sufficient observations
		qui count
		local n_specs = r(N)
		if `n_specs' < 10 {
			dis "Skipping `otype' `migr' (`geo_label'): only `n_specs' specifications."
			restore
			continue
		}

		dis _n "========================================================"
		dis "`otype_label': `migr_label' — `geo_label' (`n_specs' specifications)"
		dis "========================================================"

		** Run meta-regression
		** LHS: SDID treatment effect (tau)
		** RHS: Indicators for each specification decision
		** Base categories: All Counties (donor pool), IRS Full (data source)
		reg tau 	ib1.donor_pool 		///
					ib1.data_src 		///
					period_1624 		///
					controls 			///
					exclusion, 			///
					robust
		estimates store meta_full

		** Panel 1: donor-pool choices
		coefplot meta_full, drop(_cons) noomitted 					///
			keep(2.donor_pool 3.donor_pool 4.donor_pool 5.donor_pool 6.donor_pool) ///
			xline(0, lc("`col_zero'") lp(dash)) 					///
			coeflabels( 											///
				2.donor_pool = `""Urban" "(Top 5%)""' 				///
				3.donor_pool = `""COVID" "Match""' 					///
				4.donor_pool = `""Demographic" "Match""' 			///
				5.donor_pool = `""Stringency" "Match""' 			///
				6.donor_pool = `""Narrow" "Pool""' 					///
			) 														///
			msymbol(D) mcolor("`col_pool'") 						///
			ciopts(lcolor("`col_pool'")) 							///
			graphregion(color(white)) plotregion(color(white)) 		///
			xtitle("Effect on SDID estimate (pp)", size(vsmall)) 	///
			legend(off)												///
			name(inf_pool_`otype'_`migr'_`geo', replace)

		** Panel 2: data-source choices
		coefplot meta_full, drop(_cons) noomitted 					///
			keep(2.data_src 3.data_src 4.data_src) 				///
			xline(0, lc("`col_zero'") lp(dash)) 					///
			coeflabels( 											///
				2.data_src = `""IRS" "(ACS sample)""' 				///
				3.data_src = `""ACS" "(All 25+)""' 					///
				4.data_src = `""ACS" "(College)""' 					///
			) 														///
			msymbol(D) mcolor("`col_data'") 						///
			ciopts(lcolor("`col_data'")) 							///
			graphregion(color(white)) plotregion(color(white)) 		///
			xtitle("Effect on SDID estimate (pp)", size(vsmall)) 	///
			legend(off)												///
			name(inf_data_`otype'_`migr'_`geo', replace)

		** Panel 3: other specification choices
		coefplot meta_full, drop(_cons) noomitted 					///
			keep(period_1624 controls exclusion) 					///
			xline(0, lc("`col_zero'") lp(dash)) 					///
			coeflabels( 											///
				period_1624 = `""Extended Period" "(16-24)""' 		///
				controls    = "Covariates" 							///
				exclusion   = `""Exclude" "2020""' 					///
			) 														///
			msymbol(D) mcolor("`col_other'") 						///
			ciopts(lcolor("`col_other'")) 							///
			graphregion(color(white)) plotregion(color(white)) 		///
			xtitle("Effect on SDID estimate (pp)", size(vsmall)) 	///
			legend(off)												///
			name(inf_other_`otype'_`migr'_`geo', replace)

		graph combine 												///
			inf_pool_`otype'_`migr'_`geo' 							///
			inf_data_`otype'_`migr'_`geo' 							///
			inf_other_`otype'_`migr'_`geo', 						///
			cols(3) xcommon imargin(2 2 2 2) 						///
			graphregion(color(white))

		** Export
		graph export "${results}sdid/influence/fig_sdid_influence_`otype'_`migr'`geo_suffix'.pdf", replace
		graph export "${results}sdid/influence/fig_sdid_influence_`otype'_`migr'`geo_suffix'.jpg", ///
			as(jpg) quality(100) replace

		** Overleaf copy
		if ${overleaf} == 1 {
			graph export 											///
				"${ol_fig}fig_sdid_influence_`otype'_`migr'`geo_suffix'.pdf", replace
		}

		restore

	} // END MIGRATION LOOP
} // END OUTCOME TYPE LOOP
} // END GEOGRAPHIC SCOPE LOOP

********************************************************************************
** FINISH
********************************************************************************

** Report completion
dis ""
dis "=============================================="
dis "SDID ANALYSIS COMPLETE"
dis "=============================================="
dis "Parallel mode: ${use_parallel}"
dis "Results saved to:"
dis "  - ${results}sdid/sdid_results.dta"
dis "  - ${results}sdid/sdid_results.xlsx"
dis "  - ${results}sdid/*/tab_sdid_*.tex (tables)"
dis "  - ${results}sdid/fig_speccurve_*.pdf"
dis "  - ${results}sdid/fig_sdid_influence_*.pdf"
dis "=============================================="

** Close log
clear
log close log_02
