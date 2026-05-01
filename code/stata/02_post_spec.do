/*******************************************************************************
File Name:      02_post_spec.do
Creator:        John Iselin
Date Created:   2026-04-24

Purpose:        Single pass over sdid_results.dta + sdid_event_results.dta.
                For each AGI spec row, parse metadata, call the spec-engine
                programs (compute_spec_elasticities, compute_spec_revenue),
                and write spec_results.dta — the canonical per-spec artifact
                consumed by 02_tables_figures.do (A4).

                Replaces the per-spec calculation blocks that previously
                lived in:
                  - 02_elasticities.do Section 1 (elasticity arithmetic)
                  - 02_revenue.do Section 12 (revenue-loss distribution)

                spec_results.dta is a superset of the previous
                elasticity_results.dta: same columns (so downstream code
                can swap with no schema drift) plus two new revenue-loss
                columns (pfa_loss, state_loss).

Called by:      00_multnomah.do
Requires:       ${results}sdid/sdid_results.dta              (02_sdid_analysis.do)
                ${results}sdid/sdid_event_results.dta        (02_sdid_analysis.do)
                ${data}working/revenue_parameters.dta        (02_revenue_microsim.do)
                02_spec_engine.do (sourced at top for helper programs)

Outputs:        ${results}elasticities/spec_results.dta
                ${results}elasticities/spec_results_manifest.dta

Authors: John Iselin

For more information, contact john.iselin@yale.edu
*******************************************************************************/

** Load shared project defaults
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
** 01a_programs.do is normally sourced by 00_multnomah.do before this file
** runs, but we source it defensively here so that 02_post_spec.do can be
** invoked standalone during development. `capture program drop` in each
** program definition makes this idempotent.
do "${code}01a_programs.do"
do "${code}02_spec_engine.do"

********************************************************************************
** SECTION 0: Setup & Load
********************************************************************************

capture log close log_02pspec
log using "${logs}02_log_post_spec_${date}", name(log_02pspec) replace text

project_set_seed, context("02_post_spec.do") offset(55)

** Load revenue scalars + derived denominators (all four delta_ln_ntr flavors)
load_revenue_params

** Validate SDID inputs exist
capture confirm file "${results}sdid/sdid_results.dta"
if _rc != 0 {
	dis as error "ERROR: sdid_results.dta not found. Run 02_sdid_analysis.do first."
	log close log_02pspec
	error 601
}
capture confirm file "${results}sdid/sdid_event_results.dta"
if _rc != 0 {
	dis as error "ERROR: sdid_event_results.dta not found."
	log close log_02pspec
	error 601
}
project_assert_manifest using "${results}sdid/sdid_results_manifest.dta", ///
	artifact("sdid_results")
project_assert_manifest using "${results}sdid/sdid_event_results_manifest.dta", ///
	artifact("sdid_event_results")

capture mkdir "${results}elasticities"

dis ""
dis "=============================================="
dis "02_post_spec.do: per-spec elasticity + revenue"
dis "=============================================="

********************************************************************************
** SECTION 1: Load SDID results and parse metadata
********************************************************************************

use "${results}sdid/sdid_results.dta", clear

dis "Total specifications loaded: " _N

** Parse outcome components via anchored regex. Same pattern used in the
** pre-restructure 02_elasticities.do §1 so spec_results.dta parsing
** matches row-for-row.
gen outcome_type = ""
replace outcome_type = regexs(1) if regexm(outcome, "^(n1|n2|agi)_")

gen migration = ""
replace migration = regexs(1) if regexm(outcome, "^[a-z0-9]+_(net|in|out)_rate_")

assert inlist(outcome_type, "n1", "n2", "agi")
assert inlist(migration, "net", "in", "out")

gen data_type = ""
replace data_type = "IRS" if regexm(outcome, "_irs(_|$)") & !regexm(outcome, "_irs_outstate")
replace data_type = "IRS (Out-of-State)" if regexm(outcome, "_irs_outstate")
replace data_type = "IRS (389)" if regexm(sample_data, "irs_389") & !regexm(outcome, "_irs_outstate")
replace data_type = "IRS (389, Out-of-State)" if regexm(sample_data, "irs_389") & regexm(outcome, "_irs_outstate")
replace data_type = "ACS All (Out-of-State)" if regexm(outcome, "_acs1_outstate")
replace data_type = "ACS College (Out-of-State)" if regexm(outcome, "_acs2_outstate")
replace data_type = "ACS All" if regexm(outcome, "_acs1(_|$)") & !regexm(outcome, "_acs1_outstate")
replace data_type = "ACS College" if regexm(outcome, "_acs2(_|$)") & !regexm(outcome, "_acs2_outstate")
assert data_type != ""

gen period_type = ""
replace period_type = "16-22" if regexm(outcome, "_irs(_|$)")
replace period_type = "16-22" if regexm(sample_data, "16_22")
replace period_type = "16-24" if regexm(sample_data, "16_24")

********************************************************************************
** SECTION 2: Attach outstate flag + event-study data
********************************************************************************

** Cache sdid_event_results.dta once; three downstream preserve blocks
** (outstate merge, event-study wide reshape, later per-row matrix build)
** read from the tempfile instead of reloading the .dta.
tempfile event_src
preserve
use "${results}sdid/sdid_event_results.dta", clear
save `event_src'
restore

** Canonical outstate flag: event_results.dta carries it on every row.
tempfile outstate_src
preserve
use `event_src', clear
bysort sample_data sample outcome controls exclusion: keep if _n == 1
keep sample_data sample outcome controls exclusion outstate
save `outstate_src'
restore

merge 1:1 sample_data sample outcome controls exclusion using `outstate_src', ///
	keep(master match) nogenerate

** Fallback for any spec not in event_results (formula matches
** 02_sdid_analysis.do:762 so definitions cannot drift).
replace outstate = (regexm(outcome, "_outstate") | regexm(outcome, "_irs5")) ///
	if missing(outstate)
assert !missing(outstate)

********************************************************************************
** SECTION 3: Filters — drop IRS (389), keep only AGI
********************************************************************************

drop if inlist(data_type, "IRS (389)", "IRS (389, Out-of-State)")
drop if strpos(sample_data, "irs_389") > 0
keep if outcome_type == "agi"

dis "AGI specifications after filters: " _N
assert _N > 0

** Preferred flag
project_mark_preferred_main
qui count if preferred == 1
dis "Preferred highlighted specs: " r(N)

********************************************************************************
** SECTION 4: Merge event-study τ's (wide format)
********************************************************************************

** We keep only post-treatment years — those are what feed the stock-ε
** accumulator in compute_spec_elasticities. Event years outside
** [pfa_start_year, pfa_start_year + max post horizon] are not used.
** The range 2021–2024 covers IRS (through 2022) and ACS (through 2024).
tempfile event_wide
preserve
use `event_src', clear
keep if inrange(event_year, 2021, 2024)
keep sample_data sample outcome controls exclusion event_year event_tau
reshape wide event_tau, i(sample_data sample outcome controls exclusion) j(event_year)
save `event_wide'
restore

merge 1:1 sample_data sample outcome controls exclusion using `event_wide', ///
	keep(master match) gen(_event_mrg)
project_report_merge, gen(_event_mrg) tag("event_wide")

** Any spec that lacks post-year event estimates gets missing event_tau*
** columns; the engine's stock-ε block handles missings via `continue`.

********************************************************************************
** SECTION 5: Per-row engine calls
********************************************************************************

** Spec-specific stock denominators (spec_engine returns scale_total and
** scale_taxbase but not the dln denominators; compute here so they
** appear as columns in spec_results.dta for Excel recalc sheets).
gen double stock_dln_ntr = delta_ln_ntr_total
replace stock_dln_ntr = delta_ln_ntr_total_college ///
	if inlist(data_type, "ACS College", "ACS College (Out-of-State)")
gen double stock_dln_ntr_shs = delta_ln_ntr_total_shs
replace stock_dln_ntr_shs = delta_ln_ntr_total_college_shs ///
	if inlist(data_type, "ACS College", "ACS College (Out-of-State)")

** Initialize output columns (same names as pre-restructure elasticity_results.dta
** plus pfa_loss / state_loss). Declaring upfront lets the qui loop use
** `replace … in `i'` which is faster than rebuilding with gen per row.
foreach v in beta_kleven beta_se_kleven beta_kleven_shs beta_se_kleven_shs ///
		flow_e flow_se flow_e_shs flow_se_shs                               ///
		scale_total scale_taxbase                                           ///
		cum_tau_common cum_tau_full H_common H_full                         ///
		ln_stock_chg_common_total ln_stock_chg_full_total                   ///
		ln_stock_chg_common_imp ln_stock_chg_full_imp                       ///
		stock_elast_total_common stock_elast_total_full stock_elast_total_ann ///
		stock_elast_imp_common stock_elast_imp_full stock_elast_imp_ann     ///
		stock_elast_total_common_shs stock_elast_total_full_shs stock_elast_total_ann_shs ///
		stock_elast_imp_common_shs stock_elast_imp_full_shs stock_elast_imp_ann_shs ///
		pfa_loss state_loss {
	gen double `v' = .
}

local post_years 2021 2022 2023 2024
local n_years : word count `post_years'

dis ""
dis "Looping over " _N " specs..."
timer clear 1
timer on 1

qui {
	forvalues i = 1/`=_N' {
		local tau_i      = tau[`i']
		local se_i       = se[`i']
		local pre_mean_i = pre_mean[`i']
		local mig_i      = migration[`i']
		local dt_i       = data_type[`i']
		local os_i       = outstate[`i']

		** Build event_taus matrix for net specs (rows = year, tau).
		** Non-matching year columns get missing, which the engine skips.
		tempname etau
		matrix `etau' = J(`n_years', 2, .)
		local k 0
		foreach yr of local post_years {
			local ++k
			capture confirm variable event_tau`yr'
			if _rc == 0 {
				matrix `etau'[`k', 1] = `yr'
				matrix `etau'[`k', 2] = event_tau`yr'[`i']
			}
		}

		** Elasticities (option names have no underscores — Stata `syntax`
		** doesn't support them reliably; see note in 02_spec_engine.do)
		if "`mig_i'" == "net" {
			compute_spec_elasticities, tau(`tau_i') se(`se_i') ///
				premean(`pre_mean_i') migration("`mig_i'") ///
				datatype("`dt_i'") eventtaus(`etau')
		}
		else {
			compute_spec_elasticities, tau(`tau_i') se(`se_i') ///
				premean(`pre_mean_i') migration("`mig_i'") ///
				datatype("`dt_i'")
		}

		replace beta_kleven        = r(beta)          in `i'
		replace beta_se_kleven     = r(beta_se)       in `i'
		replace beta_kleven_shs    = r(beta_shs)      in `i'
		replace beta_se_kleven_shs = r(beta_se_shs)   in `i'
		replace flow_e             = r(flow_e)        in `i'
		replace flow_se            = r(flow_se)       in `i'
		replace flow_e_shs         = r(flow_e_shs)    in `i'
		replace flow_se_shs        = r(flow_se_shs)   in `i'
		replace scale_total        = r(scale_total)   in `i'
		replace scale_taxbase      = r(scale_taxbase) in `i'
		replace H_common           = r(H_common)      in `i'
		replace H_full             = r(H_full)        in `i'
		replace cum_tau_common     = r(cum_tau_common) in `i'
		replace cum_tau_full       = r(cum_tau_full)   in `i'
		replace ln_stock_chg_common_total = r(ln_common_tot) in `i'
		replace ln_stock_chg_full_total   = r(ln_full_tot)   in `i'
		replace ln_stock_chg_common_imp   = r(ln_common_imp) in `i'
		replace ln_stock_chg_full_imp     = r(ln_full_imp)   in `i'
		replace stock_elast_total_common     = r(stock_common)     in `i'
		replace stock_elast_total_full       = r(stock_full)       in `i'
		replace stock_elast_total_ann        = r(stock_ann)        in `i'
		replace stock_elast_imp_common       = r(stock_imp_common) in `i'
		replace stock_elast_imp_full         = r(stock_imp_full)   in `i'
		replace stock_elast_imp_ann          = r(stock_imp_ann)    in `i'
		replace stock_elast_total_common_shs = r(stock_common_shs)     in `i'
		replace stock_elast_total_full_shs   = r(stock_full_shs)       in `i'
		replace stock_elast_total_ann_shs    = r(stock_ann_shs)        in `i'
		replace stock_elast_imp_common_shs   = r(stock_imp_common_shs) in `i'
		replace stock_elast_imp_full_shs     = r(stock_imp_full_shs)   in `i'
		replace stock_elast_imp_ann_shs      = r(stock_imp_ann_shs)    in `i'

		** Revenue
		compute_spec_revenue, tau(`tau_i') migration("`mig_i'") ///
			outstate(`os_i') datatype("`dt_i'")
		replace pfa_loss   = r(pfa_loss)   in `i'
		replace state_loss = r(state_loss) in `i'

		matrix drop `etau'
	}
}

timer off 1
timer list 1

********************************************************************************
** SECTION 6: Confidence intervals
********************************************************************************
** Same ±1.96·SE convention used in the pre-restructure 02_elasticities.do.
** Caveat that these SEs treat revenue parameters as known constants is
** documented in todo.md Priority 1 (TODO-1.1) and addressed by Phase B.
foreach pair in ///
		"beta_kleven beta_se_kleven" ///
		"beta_kleven_shs beta_se_kleven_shs" ///
		"flow_e flow_se" ///
		"flow_e_shs flow_se_shs" {
	local est : word 1 of `pair'
	local se  : word 2 of `pair'
	confirm variable `se'
	gen double `est'_ci_lo = `est' - 1.96 * `se'
	gen double `est'_ci_hi = `est' + 1.96 * `se'
}

********************************************************************************
** SECTION 7: Variable labels
********************************************************************************

label var outcome_type     "Outcome family (n1 / n2 / agi)"
label var migration        "Migration direction (net / in / out)"
label var data_type        "Data source label (IRS, ACS All, ACS College, ...)"
label var period_type      "Sample period (16-22 / 16-24)"
label var outstate         "1 = out-of-state migration only"

label var scale_total      "Scale to total AGI (1 or college_agi_share)"
label var scale_taxbase    "Scale to impacted AGI (scale_total / impacted_agi_share)"
label var stock_dln_ntr    "Spec-specific Δln(1-τ_total) used for stock elasticities"
label var stock_dln_ntr_shs "Spec-specific Δln(1-τ_total) including SHS 1%"

label var beta_kleven      "Kleven semi-elasticity: (tau/100) / Δln(1-τ_total)"
label var beta_se_kleven   "SE of Kleven semi-elasticity"
label var beta_kleven_shs  "Kleven semi-elasticity with SHS-inclusive denominator"
label var beta_se_kleven_shs "SE of SHS-inclusive Kleven semi-elasticity"
label var flow_e           "Flow elasticity: -(tau/pre_mean) / Δln(1-τ_total)"
label var flow_se          "SE of flow elasticity"
label var flow_e_shs       "Flow elasticity with SHS-inclusive denominator"
label var flow_se_shs      "SE of SHS-inclusive flow elasticity"

capture label var cum_tau_common "Sum of event-study tau over 2021-2022"
capture label var cum_tau_full   "Sum of event-study tau over all post years"
capture label var H_common       "Number of post rows cumulated (2021-2022)"
capture label var H_full         "Number of post rows cumulated (all post)"
capture label var ln_stock_chg_common_total "Cum. log stock change (total AGI, 2021-2022)"
capture label var ln_stock_chg_full_total   "Cum. log stock change (total AGI, all post)"
capture label var ln_stock_chg_common_imp   "Cum. log stock change (impacted AGI, 2021-2022)"
capture label var ln_stock_chg_full_imp     "Cum. log stock change (impacted AGI, all post)"

capture label var stock_elast_total_common "Stock elasticity (total AGI, 2021-22)"
capture label var stock_elast_total_full   "Stock elasticity (total AGI, all post)"
capture label var stock_elast_total_ann    "Annualized stock elasticity (total AGI)"
capture label var stock_elast_imp_common   "Stock elasticity (impacted AGI, 2021-22)"
capture label var stock_elast_imp_full     "Stock elasticity (impacted AGI, all post)"
capture label var stock_elast_imp_ann      "Annualized stock elasticity (impacted AGI)"
capture label var stock_elast_total_common_shs "Stock elasticity +SHS (total AGI, 2021-22)"
capture label var stock_elast_total_full_shs   "Stock elasticity +SHS (total AGI, all post)"
capture label var stock_elast_total_ann_shs    "Annualized stock elasticity +SHS (total AGI)"
capture label var stock_elast_imp_common_shs   "Stock elasticity +SHS (impacted AGI, 2021-22)"
capture label var stock_elast_imp_full_shs     "Stock elasticity +SHS (impacted AGI, all post)"
capture label var stock_elast_imp_ann_shs      "Annualized stock elasticity +SHS (impacted AGI)"

label var pfa_loss   "Implied PFA revenue loss ($M; net-domestic specs only)"
label var state_loss "Implied Oregon revenue loss from Multnomah out-migration ($M; net-outstate specs only)"

********************************************************************************
** SECTION 8: Save
********************************************************************************

** Sort for deterministic row order across runs.
sort sample_data sample outcome controls exclusion
compress

save "${results}elasticities/spec_results.dta", replace

project_build_signature, artifact("spec_results")
project_write_manifest using "${results}elasticities/spec_results_manifest.dta", ///
	artifact("spec_results") script("02_post_spec.do") ///
	upstream("`r(signature)'")

qui count if preferred == 1 & migration == "net" & outstate == 0 & !missing(stock_elast_total_common)
if r(N) == 0 {
	dis as error "ERROR: No preferred domestic net-migration specs produced stock elasticity."
	dis as error "       Regenerate sdid_event_results.dta from 02_sdid_analysis.do."
	log close log_02pspec
	error 2001
}

dis ""
dis "=============================================="
dis "02_post_spec.do complete."
dis "Rows in spec_results.dta: " _N
dis "Output:  ${results}elasticities/spec_results.dta"
dis "Manifest: ${results}elasticities/spec_results_manifest.dta"
dis "=============================================="

dis ""
dis "--- Preferred net-migration β / stock ε / revenue-loss summary ---"
list data_type sample migration outstate ///
	beta_kleven beta_kleven_shs ///
	stock_elast_total_common stock_elast_total_common_shs ///
	pfa_loss state_loss ///
	if preferred == 1 & migration == "net", sep(0) abbreviate(25)

capture log close log_02pspec
