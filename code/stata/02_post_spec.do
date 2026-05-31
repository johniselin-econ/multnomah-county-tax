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
if "${dir}" == "" {
    local _cwd = subinstr("`c(pwd)'", "\", "/", .)
    if regexm("`_cwd'", "(.*)/code/(stata|utils)$") global dir = regexs(1)
    else global dir "`_cwd'"
}
do "${dir}/code/utils/globals.do"
** 01a_programs.do is normally sourced by 00_multnomah.do before this file
** runs, but we source it defensively here so that 02_post_spec.do can be
** invoked standalone during development. `capture program drop` in each
** program definition makes this idempotent.
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

** Parse outcome / sample_data into the canonical spec-metadata columns.
** Centralized in 01a_programs.do (project_parse_outcome_components) so
** the ad-hoc parsing blocks that used to live in this file +
** 02_sdid_analysis.do share one implementation.
project_parse_outcome_components

assert inlist(outcome_type, "n1", "n2", "agi")
assert inlist(migration, "net", "in", "out")
assert data_type != ""

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

** Optional cross-check against event_results.dta. The helper already set
** outstate from outcome/sample_data; this merge is retained as an audit
** trail (and to surface any drift between event_results' outstate flag
** and this file's parser). With nogenerate, Stata's default behavior
** keeps the master (helper-generated) value on matched rows — so the
** merge is effectively read-only here.
tempfile outstate_src
preserve
use `event_src', clear
bysort sample_data sample outcome controls exclusion: keep if _n == 1
keep sample_data sample outcome controls exclusion outstate
rename outstate outstate_evt
save `outstate_src'
restore

merge 1:1 sample_data sample outcome controls exclusion using `outstate_src', ///
	keep(master match) nogenerate
capture confirm variable outstate_evt
if _rc == 0 {
	qui count if !missing(outstate_evt) & outstate != outstate_evt
	if r(N) > 0 {
		dis as error "WARNING: outstate disagrees with event_results on " r(N) " rows."
	}
	drop outstate_evt
}

assert !missing(outstate)

********************************************************************************
** SECTION 3: Filter to AGI outcomes
********************************************************************************
** IRS (389) rows are retained: the spec engine treats them identically to
** IRS (no scaling branch) and the elasticity / revenue spec curves expose
** a dedicated "IRS (ACS counties)" indicator row that depends on them.

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
		pfa_loss state_loss scale ///
		X_pfa R_m_pfa dynamic_pfa ratio_pfa baseline_pfa actual_pfa ///
		X_state R_m_state dynamic_state ratio_state baseline_state actual_state {
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
		replace scale      = r(scale)      in `i'

		** Revenue-decomposition intermediates (populated only for net specs in
		** the matching outstate branch; missing otherwise). Used by the
		** appendix table tbl_revenue_decomposition.tex.
		replace X_pfa          = r(X_pfa)          in `i'
		replace R_m_pfa        = r(R_m_pfa)        in `i'
		replace dynamic_pfa    = r(dynamic_pfa)    in `i'
		replace ratio_pfa      = r(ratio_pfa)      in `i'
		replace baseline_pfa   = r(baseline_pfa)   in `i'
		replace actual_pfa     = r(actual_pfa)     in `i'
		replace X_state        = r(X_state)        in `i'
		replace R_m_state      = r(R_m_state)      in `i'
		replace dynamic_state  = r(dynamic_state)  in `i'
		replace ratio_state    = r(ratio_state)    in `i'
		replace baseline_state = r(baseline_state) in `i'
		replace actual_state   = r(actual_state)   in `i'

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

label var scale            "Revenue-branch scale factor: 1 (IRS/ACS All) or college_agi_share (ACS College); from compute_spec_revenue. Used by tbl_revenue_decomposition.tex."
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

** Revenue-decomposition intermediates (populated for net specs in their
** applicable outstate branch; otherwise missing). Used to build the
** appendix walk-through table tbl_revenue_decomposition.tex.
label var X_pfa          "AGI moving, PFA branch ($; effect_scaled x total_agi_2022)"
label var R_m_pfa        "Static PFA loss R_m ($; avg_mt_rate x X_pfa)"
label var dynamic_pfa    "PFA dynamic baseline ($; baseline_pfa - R_m_pfa)"
label var ratio_pfa      "R_m / dynamic ratio, PFA branch (decimal)"
label var baseline_pfa   "Baseline PFA revenue, simulated ($; constant across specs)"
label var actual_pfa     "Actual PFA revenue ($; constant across specs)"
label var X_state        "AGI moving, Oregon branch ($; effect_scaled x total_agi_2022)"
label var R_m_state      "Static Oregon loss R_m ($; avg_state_rate x X_state)"
label var dynamic_state  "Oregon dynamic baseline ($; baseline_state - R_m_state)"
label var ratio_state    "R_m / dynamic ratio, Oregon branch (decimal)"
label var baseline_state "Baseline Oregon revenue, Multnomah scope ($; constant)"
label var actual_state   "Actual Oregon revenue, Multnomah scope ($; constant)"

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

********************************************************************************
** SECTION 8: DiD-IMPLIED SEMI-ELASTICITIES
********************************************************************************
** Put the ACS individual-level DiD (College x Post) estimates on the same
** semi-elasticity metric as the SDID results, for the comparison in the ACS
** DiD results section. The DiD treats college-educated Multnomah residents, so
** we use the headline (PFA+SHS) college net-of-tax denominator
** delta_ln_ntr_total_college_shs (the same one compute_spec_elasticities uses
** for ACS-college specs). ACS DiD outcomes are scaled x100, so the coefficient
** b is in percentage points:
**     semi_elast = (b / 100) / delta_ln_ntr_total_college_shs
** b > 0 (more out-migration) with the negative denominator yields a negative
** semi-elasticity, matching the SDID gross-out-migration sign convention. The
** SE is propagated treating the (fixed) denominator as known, mirroring
** compute_spec_elasticities (beta_se).
capture confirm file "${results}did/did_coefficients.dta"
if _rc == 0 {
	preserve
	use "${results}did/did_coefficients.dta", clear
	gen double dln_college_shs = scalar(delta_ln_ntr_total_college_shs)
	gen double semi_elast      = (b  / 100) / dln_college_shs
	gen double semi_elast_se   = (se / 100) / abs(dln_college_shs)
	label var semi_elast      "DiD-implied semi-elasticity (headline PFA+SHS, college denominator)"
	label var semi_elast_se   "SE (denominator treated as fixed)"
	label var dln_college_shs "Delta-ln(1-tau_total), headline college denominator"
	order outcome b se semi_elast semi_elast_se dln_college_shs n
	save "${results}did/did_elasticities.dta", replace
	dis ""
	dis "--- DiD-implied semi-elasticities (headline PFA+SHS, college denominator) ---"
	list outcome b se semi_elast semi_elast_se, noobs sep(0) abbreviate(20)
	dis "Output: ${results}did/did_elasticities.dta"
	restore
}
else {
	dis as txt "Note: ${results}did/did_coefficients.dta not found -- run 02_did_analysis.do first to enable DiD semi-elasticities."
}

capture log close log_02pspec
