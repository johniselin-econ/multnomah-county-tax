/*******************************************************************************
File Name: 		02_elasticities.do
Creator: 		John Iselin
Date Update:	April 2026

Purpose: 	Calculate Kleven-style migration semi-elasticities and flow
			elasticities from SDID treatment effect estimates for the
			Preschool for All (PFA) income tax.

			Denominator is the total net-of-tax rate (Kleven et al. 2024,
			NBER WP 32153):
			  Δln(1−τ_total) = ln((1−τ_post) / (1−τ_pre))
			where τ_total = federal + state + FICA employee share + PFA.

			Formulas (all on SDID ATT τ in pp of migration rate):
			  Semi-elasticity β (Kleven eq. 4):
			      β = (τ/100) / Δln(1−τ_total)
			  Flow elasticity (gross only; undefined for net when pre_mean=0):
			      ε_flow = -(τ / pre_mean) / Δln(1−τ_total)
			  Horizon-H cumulative stock elasticity (directly estimable):
			      ε_stock,H = Δln(S_H) / Δln(1−τ_total)

			where Δln(S_H) is built from annual net-flow effects accumulated into
			the AGI stock through post year H. We report both total-AGI-base and
			impacted-AGI-base variants. This is not the Kleven et al. steady-state
			stock elasticity β · (T+1)/2, which requires a demographic lifespan T
			that we do not estimate.

Called by: 	00_multnomah.do
Requires:	${data}working/revenue_parameters.dta (from 02_revenue_microsim.do)
			${results}sdid/sdid_results.dta (from 02_sdid_analysis.do)
			${results}sdid/sdid_event_results.dta (from 02_sdid_analysis.do)

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
** Local helper programs (LaTeX scaffolding)
********************************************************************************

** Open a threeparttable with caption/label and begin the tabular.
** HANDLE = open file handle; CAP = caption (may contain TeX); LBL = \label tag
** COLS = tabular column spec; FONTSIZE = optional size macro (e.g. "footnotesize")
capture program drop elast_tex_open
program define elast_tex_open
	syntax, HANDLE(string) CAP(string asis) LBL(string) ///
		COLS(string asis) [FONTSIZE(string)]
	file write `handle' "\begin{table}[htbp]" _n
	file write `handle' "\centering" _n
	file write `handle' "\begin{threeparttable}" _n
	file write `handle' `"\caption{`cap'}"' _n
	file write `handle' "\label{`lbl'}" _n
	if "`fontsize'" != "" file write `handle' "\`fontsize'" _n
	file write `handle' `"\begin{tabular}{`cols'}"' _n
	file write `handle' "\toprule" _n
end

** Close the tabular and start the tablenotes block; caller writes note bodies
** then calls elast_tex_close to finish.
capture program drop elast_tex_notes_open
program define elast_tex_notes_open
	syntax, HANDLE(string)
	file write `handle' "\bottomrule" _n
	file write `handle' "\end{tabular}" _n
	file write `handle' "\begin{tablenotes}" _n
	file write `handle' "\small" _n
	file write `handle' "\item \textit{Notes:} " _n
end

** Close tablenotes, threeparttable, and table environments.
capture program drop elast_tex_close
program define elast_tex_close
	syntax, HANDLE(string)
	file write `handle' "\end{tablenotes}" _n
	file write `handle' "\end{threeparttable}" _n
	file write `handle' "\end{table}" _n
end

** Write one panel (out or in migration) of the gross in/out elasticity table.
** Assumes the current dataset has been preserve-filtered by the caller and
** holds these variables: data_type, sample, migration, migr_label, and the
** formatted string columns tau_str, se_str, beta_str, beta_se_str,
** fe_str, fe_se_str (generated once per table). DIRECTION is "out" or "in".
capture program drop elast_inout_panel
program define elast_inout_panel
	syntax, HANDLE(string) DIRECTION(string)

	local N = _N
	local prev_dt ""

	forvalues i = 1/`N' {
		if migration[`i'] != "`direction'" continue

		local dt     = data_type[`i']
		local smp    = subinstr(sample[`i'], "sample_", "", .)
		local smp    = proper("`smp'")
		local mg     = migr_label[`i']
		local t_val  = tau_str[`i']
		local se_val = se_str[`i']
		local b      = beta_str[`i']
		local b_se   = beta_se_str[`i']
		local fe     = fe_str[`i']
		local fe_se  = fe_se_str[`i']

		if "`fe'" == "" local fe "--"
		if "`fe_se'" == "" local fe_se ""

		if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
			file write `handle' "\addlinespace" _n
		}
		local prev_dt "`dt'"

		file write `handle' "`dt' & `smp' & `mg' & `t_val' & `b' & `fe' \\" _n
		file write `handle' " & & & `se_val' & `b_se' & `fe_se' \\" _n
	}
end

********************************************************************************
** SECTION 0: Setup & Parameters
********************************************************************************

** Start log file
capture log close log_02elast
log using "${logs}02_log_elasticities_${date}", name(log_02elast) replace text

project_set_seed, context("02_elasticities.do") offset(50)

** Drop any stale revenue-parameter scalars from a prior run. Stata `scalar`
** is a global namespace that survives `clear`, so a stale value from an
** earlier (differently-named) run could silently shadow the fresh load.
foreach s in avg_mt_rate avg_state_rate baseline_pfa_revenue total_agi_2022  ///
	agi_total agi_impacted impacted_agi_share agi_college college_agi_share   ///
	agi_college_impacted college_impacted_agi_share                           ///
	avg_mt_rate_college_impacted avg_total_rate avg_total_rate_pre            ///
	avg_total_rate_college avg_total_rate_pre_college                         ///
	avg_shs_rate avg_shs_rate_college                                         ///
	avg_total_rate_with_shs avg_total_rate_pre_with_shs                       ///
	avg_total_rate_col_with_shs avg_total_rate_pre_col_with_shs               ///
	delta_t delta_ln_ntr_total delta_ln_ntr_total_college                     ///
	delta_ln_ntr_total_shs delta_ln_ntr_total_college_shs {
	capture scalar drop `s'
}

** Validate Overleaf globals up-front. String comparison is empty-safe;
** an unquoted `if ${overleaf} == 1` expands to `if  == 1` (syntax error) when unset.
if "${overleaf}" == "1" {
	foreach g in ol_fig ol_tab {
		if "${`g'}" == "" {
			dis as error "ERROR: \${overleaf}=1 but \${`g'} is unset in 00_stata_config.do."
			exit 198
		}
	}
}

** Common-support window for cumulative stock elasticities.
** IRS migration data currently ends in 2022; update `common_end_year` when
** 2023 IRS SOI data lands and can be merged into the ACS panel.
local pfa_start_year  = 2021		// PFA tax took effect
local common_end_year = 2022		// last year of IRS-ACS overlap

** Create output directory
capture mkdir "${results}elasticities"

dis ""
dis "=============================================="
dis "Section 0: Load parameters"
dis "=============================================="

** Load revenue parameters exported by 02_revenue_microsim.do
mata: st_local("rp_exists", strofreal(fileexists("${data}working/revenue_parameters.dta")))
if `rp_exists' == 0 {
	dis as error "ERROR: revenue_parameters.dta not found."
	dis as error "       Run 02_revenue_microsim.do first."
	log close log_02elast
	error 601
}
project_assert_manifest using "${data}working/revenue_parameters_manifest.dta", ///
	artifact("revenue_parameters")

preserve
use "${data}working/revenue_parameters.dta", clear
scalar avg_mt_rate = avg_mt_rate[1]
scalar avg_state_rate = avg_state_rate[1]
scalar baseline_pfa_revenue = baseline_pfa_revenue[1]
scalar total_agi_2022 = total_agi_2022[1]
scalar agi_total = agi_total[1]
scalar agi_impacted = agi_impacted[1]
scalar impacted_agi_share = impacted_agi_share[1]
scalar agi_college = agi_college[1]
scalar college_agi_share = college_agi_share[1]
scalar agi_college_impacted = agi_college_impacted[1]
scalar college_impacted_agi_share = college_impacted_agi_share[1]
scalar avg_mt_rate_college_impacted = avg_mt_rate_college_impacted[1]
scalar avg_total_rate = avg_total_rate[1]
scalar avg_total_rate_pre = avg_total_rate_pre[1]
scalar avg_total_rate_college = avg_total_rate_college[1]
scalar avg_total_rate_pre_college = avg_total_rate_pre_college[1]
scalar avg_shs_rate                  = avg_shs_rate[1]
scalar avg_shs_rate_college          = avg_shs_rate_college[1]
scalar avg_total_rate_with_shs       = avg_total_rate_with_shs[1]
scalar avg_total_rate_pre_with_shs   = avg_total_rate_pre_with_shs[1]
scalar avg_total_rate_col_with_shs   = avg_total_rate_col_with_shs[1]
scalar avg_total_rate_pre_col_with_shs = avg_total_rate_pre_col_with_shs[1]
restore

** Compute tax change terms
** PFA was new in 2021, so Δt = avg_mt_rate (retained for reporting in table notes)
scalar delta_t = avg_mt_rate

** Total net-of-tax rate change (Kleven et al. 2024 denominator)
** Uses full tax burden: federal + state + FICA employee share + PFA
** Δln(1−τ_total) = ln((1−τ_post) / (1−τ_pre)); negative for a tax increase
scalar delta_ln_ntr_total = ln((1 - avg_total_rate) / (1 - avg_total_rate_pre))
scalar delta_ln_ntr_total_college = ln((1 - avg_total_rate_college) ///
	/ (1 - avg_total_rate_pre_college))

** SHS-inclusive denominator (PFA + Metro Supportive Housing Services 1%).
** SHS applies at the same thresholds as PFA tier 1; for a Multnomah vs.
** non-Metro donor pool this is part of the differential change in 2021.
** Reported as a parallel sensitivity set — not a replacement.
scalar delta_ln_ntr_total_shs = ln((1 - avg_total_rate_with_shs) ///
	/ (1 - avg_total_rate_pre_with_shs))
scalar delta_ln_ntr_total_college_shs = ln((1 - avg_total_rate_col_with_shs) ///
	/ (1 - avg_total_rate_pre_col_with_shs))

dis ""
dis "Revenue parameters:"
dis "  avg_mt_rate       = " %10.6f avg_mt_rate
dis "  avg_state_rate    = " %10.6f avg_state_rate
dis "  avg_total_rate    = " %10.6f avg_total_rate " (post-PFA)"
dis "  avg_total_rate_pre= " %10.6f avg_total_rate_pre " (pre-PFA)"
dis "  impacted share    = " %10.6f impacted_agi_share
dis "  college share     = " %10.6f college_agi_share
dis "  Δt (PFA)          = " %10.6f delta_t
dis "  avg SHS rate      = " %10.6f avg_shs_rate
dis "  avg total+SHS rate= " %10.6f avg_total_rate_with_shs
dis "  Δln(1−τ) total NTR= " %10.6f delta_ln_ntr_total
dis "  Δln(1−τ) NTR+SHS  = " %10.6f delta_ln_ntr_total_shs

** Sanity checks (hard errors — a scale bug in 02_revenue_microsim.do should halt
** the pipeline, not print a warning and produce absurd elasticities).
if delta_t < 0.001 | delta_t > 0.05 {
	dis as error "ERROR: avg_mt_rate = " %8.6f delta_t " outside [0.001, 0.05]"
	dis as error "       Inspect TAXSIM v25 inputs in 02_revenue_microsim.do and verify"
	dis as error "       avg_mt_rate is on the [0,1] scale (not [0,100])."
	log close log_02elast
	error 459
}
if avg_total_rate < 0.20 | avg_total_rate > 0.55 {
	dis as error "ERROR: avg_total_rate = " %8.6f avg_total_rate ///
		" outside [0.20, 0.55]"
	dis as error "       Inspect 02_revenue_microsim.do tax-total aggregation."
	log close log_02elast
	error 459
}

********************************************************************************
** SECTION 1: Load SDID Results & Compute Elasticities
********************************************************************************

dis ""
dis "=============================================="
dis "Section 1: Load SDID results and compute elasticities"
dis "=============================================="

** Check that SDID results exist
mata: st_local("sd_exists", strofreal(fileexists("${results}sdid/sdid_results.dta")))
if `sd_exists' == 0 {
	dis as error "ERROR: sdid_results.dta not found."
	dis as error "       Run 02_sdid_analysis.do first."
	log close log_02elast
	error 601
}
project_assert_manifest using "${results}sdid/sdid_results_manifest.dta", ///
	artifact("sdid_results")

mata: st_local("ev_exists", strofreal(fileexists("${results}sdid/sdid_event_results.dta")))
if `ev_exists' == 0 {
	dis as error "ERROR: sdid_event_results.dta not found."
	dis as error "       Run 02_sdid_analysis.do after the event-study export update."
	log close log_02elast
	error 601
}
project_assert_manifest using "${results}sdid/sdid_event_results_manifest.dta", ///
	artifact("sdid_event_results")

use "${results}sdid/sdid_results.dta", clear

dis "Total specifications loaded: " _N

** ---- Parse outcome variable to extract components ----
** Outcome names follow <type>_<direction>_rate_<source>[_outstate] (from
** 02_sdid_analysis.do). Use anchored regex to be robust against token
** substrings appearing elsewhere (e.g. "_in_" inside "_within_").

gen outcome_type = ""
replace outcome_type = regexs(1) if regexm(outcome, "^(n1|n2|agi)_")

gen migration = ""
replace migration = regexs(1) if regexm(outcome, "^[a-z0-9]+_(net|in|out)_rate_")

** Validate regex matched every row. Old strpos-based implementation
** produced the same labels; assertion catches silent drift if outcome naming
** ever changes upstream.
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

** Cache sdid_event_results.dta once; two downstream preserve blocks
** (outstate lookup here, cumulative-tau aggregation in Section 1) read
** from the tempfile instead of reloading the .dta.
tempfile event_src
preserve
use "${results}sdid/sdid_event_results.dta", clear
save `event_src'
restore

** Read outstate from sdid_event_results.dta (canonical source) rather than
** re-deriving. Event-study file is produced by 02_sdid_analysis.do:762 and
** carries outstate on every row; collapse to one value per spec and merge.
tempfile outstate_src
preserve
use `event_src', clear
bysort sample_data sample outcome controls exclusion: keep if _n == 1
keep sample_data sample outcome controls exclusion outstate
save `outstate_src'
restore

merge 1:1 sample_data sample outcome controls exclusion using `outstate_src', ///
	keep(master match) nogenerate

** Fallback for any spec not in event_results (e.g. event study failed):
** same formula as 02_sdid_analysis.do:762 so definitions cannot drift.
replace outstate = (regexm(outcome, "_outstate") | regexm(outcome, "_irs5")) ///
	if missing(outstate)
assert !missing(outstate)

** Exclude IRS 389 (ACS-matched counties) — keep only IRS with all counties
drop if inlist(data_type, "IRS (389)", "IRS (389, Out-of-State)")
drop if strpos(sample_data, "irs_389") > 0

** ---- Mark highlighted specifications ----
project_mark_preferred_main

** ---- Keep only AGI ----
keep if outcome_type == "agi"
dis "AGI specifications: " _N

** ---- Compute elasticities on all AGI specs ----

** Kleven et al. 2024 semi-elasticity (NBER WP 32153 eq. 4):
**   β = (τ / 100) / Δln(1−τ_total)
** Interpretation: the effect on the decimal-scale migration rate of a unit
** change in ln(1−τ_total). For a tax hike, Δln(1−τ_total) < 0.
**   Out-migration: τ > 0 (more outflow) → β = (+)/(−) = NEGATIVE.
**   In-migration:  τ < 0 (less inflow) → β = (−)/(−) = POSITIVE.
** Kleven reports β_out ≈ −0.17 for Swedish top-2% wealth holders — the
** negative sign reflects that migration rises as the net-of-tax rate falls.
gen double beta_kleven    = (tau / 100) / delta_ln_ntr_total
gen double beta_se_kleven = (se  / 100) / abs(delta_ln_ntr_total)

** SHS-inclusive β (PFA + Metro SHS 1%).
gen double beta_kleven_shs    = (tau / 100) / delta_ln_ntr_total_shs
gen double beta_se_kleven_shs = (se  / 100) / abs(delta_ln_ntr_total_shs)

** Flow elasticity (Kleven total NTR):
**   ε_flow = −(τ / pre_mean) / Δln(1−τ_total)
** Defined for gross migration only — undefined for net when pre_mean ≈ 0.
gen double flow_e  = -(tau / pre_mean) / delta_ln_ntr_total ///
	if inlist(migration, "in", "out") & !missing(pre_mean) & pre_mean != 0
gen double flow_se = (se / abs(pre_mean)) / abs(delta_ln_ntr_total) ///
	if inlist(migration, "in", "out") & !missing(pre_mean) & pre_mean != 0

** SHS-inclusive flow elasticity.
gen double flow_e_shs  = -(tau / pre_mean) / delta_ln_ntr_total_shs ///
	if inlist(migration, "in", "out") & !missing(pre_mean) & pre_mean != 0
gen double flow_se_shs = (se / abs(pre_mean)) / abs(delta_ln_ntr_total_shs) ///
	if inlist(migration, "in", "out") & !missing(pre_mean) & pre_mean != 0

** Scales for the horizon-H stock elasticities (below).
** scale_total: aligns the SDID numerator base — 1 for full-population
** migration rates (IRS, ACS All), college_agi_share for ACS College.
** scale_taxbase: rescales further to the *impacted* AGI base by
** dividing by impacted_agi_share.
gen double scale_total = 1
replace scale_total = college_agi_share ///
	if inlist(data_type, "ACS College", "ACS College (Out-of-State)")

** Guard against a new data_type silently getting the default scale_total=1.
assert scale_total == 1 ///
	if !inlist(data_type, "ACS College", "ACS College (Out-of-State)")
assert scale_total == college_agi_share ///
	if  inlist(data_type, "ACS College", "ACS College (Out-of-State)")

gen double scale_taxbase = scale_total / impacted_agi_share

** CIs for estimates that carry a SE from SDID bootstrap.
** Note: these SEs treat revenue parameters as known constants; see todo.md
** Priority 1 (TODO-1.1) for the delta-method and bootstrap alternatives.
**
** All four SE variables are generated unconditionally just above this loop,
** so a plain `confirm` fails loudly if a rename ever drifts the pair list
** out of sync with the gen statements — unlike `capture confirm`, which
** would silently skip the CI generation.
foreach pair in ///
		"beta_kleven beta_se_kleven" ///
		"flow_e flow_se" ///
		"beta_kleven_shs beta_se_kleven_shs" ///
		"flow_e_shs flow_se_shs" {
	local est : word 1 of `pair'
	local se  : word 2 of `pair'
	confirm variable `se'
	gen double `est'_ci_lo = `est' - 1.96 * `se'
	gen double `est'_ci_hi = `est' + 1.96 * `se'
}

tempfile event_cum

preserve
use `event_src', clear
keep if strpos(outcome, "agi_") > 0 & strpos(outcome, "_net_") > 0

gen byte post_common = inrange(event_year, `pfa_start_year', `common_end_year') ///
	& !missing(event_tau)
gen byte post_full = event_year >= `pfa_start_year' & !missing(event_tau)
gen double tau_common = event_tau if post_common
gen double tau_full = event_tau if post_full

gen double ev_scale_total = 1
replace ev_scale_total = college_agi_share ///
	if regexm(outcome, "_acs2(_|$)") | regexm(outcome, "_acs2_outstate")
gen double ev_scale_impacted = ev_scale_total / impacted_agi_share

assert 1 + (event_tau / 100) * ev_scale_total > 0 if post_full
assert 1 + (event_tau / 100) * ev_scale_impacted > 0 if post_full

gen double ln_stock_chg_common_total = ln(1 + (event_tau / 100) * ev_scale_total) ///
	if post_common
gen double ln_stock_chg_full_total = ln(1 + (event_tau / 100) * ev_scale_total) ///
	if post_full
gen double ln_stock_chg_common_imp = ln(1 + (event_tau / 100) * ev_scale_impacted) ///
	if post_common
gen double ln_stock_chg_full_imp = ln(1 + (event_tau / 100) * ev_scale_impacted) ///
	if post_full

collapse (sum) cum_tau_common=tau_common cum_tau_full=tau_full ///
	ln_stock_chg_common_total=ln_stock_chg_common_total ///
	ln_stock_chg_full_total=ln_stock_chg_full_total ///
	ln_stock_chg_common_imp=ln_stock_chg_common_imp ///
	ln_stock_chg_full_imp=ln_stock_chg_full_imp ///
	H_common=post_common H_full=post_full, ///
	by(sample_data sample outcome controls exclusion)

** Groups with no post-treatment rows: missing, not zero
replace cum_tau_common = . if H_common == 0
replace cum_tau_full   = . if H_full   == 0
save `event_cum'
restore

** Confirm key uniqueness on both sides before the 1:1 merge
isid sample_data sample outcome controls exclusion
preserve
use `event_cum', clear
isid sample_data sample outcome controls exclusion
restore

merge 1:1 sample_data sample outcome controls exclusion using `event_cum', gen(evcum_mrg)
project_report_merge, gen(evcum_mrg) tag("event_cum")

** ---- Horizon-H cumulative stock elasticities ----
** Interpretation: percent change in the AGI stock through post-treatment
** year H divided by the percent change in the after-tax rate, where the stock
** change is built from cumulated annual net-flow effects.
**
** We report:
**   (a) total-base stock elasticities for literature comparison
**   (b) impacted-base stock elasticities for revenue interpretation
**
** This remains distinct from the Kleven et al. (2024) steady-state stock
** elasticity β · (T+1)/2, which requires a demographic lifespan T.
gen double stock_dln_ntr = delta_ln_ntr_total
replace stock_dln_ntr = delta_ln_ntr_total_college ///
	if inlist(data_type, "ACS College", "ACS College (Out-of-State)")

** SHS-inclusive spec-specific denominator (parallel to stock_dln_ntr)
gen double stock_dln_ntr_shs = delta_ln_ntr_total_shs
replace stock_dln_ntr_shs = delta_ln_ntr_total_college_shs ///
	if inlist(data_type, "ACS College", "ACS College (Out-of-State)")

gen double stock_elast_total_common = ///
	ln_stock_chg_common_total / stock_dln_ntr if !missing(ln_stock_chg_common_total)
gen double stock_elast_total_full   = ///
	ln_stock_chg_full_total / stock_dln_ntr if !missing(ln_stock_chg_full_total)
gen double stock_elast_total_ann    = ///
	stock_elast_total_full / H_full if H_full > 0

gen double stock_elast_imp_common = ///
	ln_stock_chg_common_imp / stock_dln_ntr if !missing(ln_stock_chg_common_imp)
gen double stock_elast_imp_full   = ///
	ln_stock_chg_full_imp / stock_dln_ntr if !missing(ln_stock_chg_full_imp)
gen double stock_elast_imp_ann    = ///
	stock_elast_imp_full / H_full if H_full > 0

** SHS-inclusive stock elasticities. The ln_stock_chg_* numerators are data
** objects (built from event-study net-flow effects); only the denominator
** differs between the main and SHS-inclusive series.
gen double stock_elast_total_common_shs = ///
	ln_stock_chg_common_total / stock_dln_ntr_shs if !missing(ln_stock_chg_common_total)
gen double stock_elast_total_full_shs   = ///
	ln_stock_chg_full_total / stock_dln_ntr_shs if !missing(ln_stock_chg_full_total)
gen double stock_elast_total_ann_shs    = ///
	stock_elast_total_full_shs / H_full if H_full > 0

gen double stock_elast_imp_common_shs = ///
	ln_stock_chg_common_imp / stock_dln_ntr_shs if !missing(ln_stock_chg_common_imp)
gen double stock_elast_imp_full_shs   = ///
	ln_stock_chg_full_imp / stock_dln_ntr_shs if !missing(ln_stock_chg_full_imp)
gen double stock_elast_imp_ann_shs    = ///
	stock_elast_imp_full_shs / H_full if H_full > 0

** ---- Variable labels for saved output ----
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
capture label var ln_stock_chg_common_total "Cum. log stock change (total AGI base, 2021-2022)"
capture label var ln_stock_chg_full_total   "Cum. log stock change (total AGI base, all post)"
capture label var ln_stock_chg_common_imp   "Cum. log stock change (impacted AGI base, 2021-2022)"
capture label var ln_stock_chg_full_imp     "Cum. log stock change (impacted AGI base, all post)"
capture label var H_common       "Number of post rows cumulated (2021-2022)"
capture label var H_full         "Number of post rows cumulated (all post years)"

capture label var stock_elast_total_common "Stock elasticity (total AGI base, 2021-22)"
capture label var stock_elast_total_full   "Stock elasticity (total AGI base, all post)"
capture label var stock_elast_total_ann    "Annualized stock elasticity (total AGI base, all post)"
capture label var stock_elast_imp_common   "Stock elasticity (impacted AGI base, 2021-22)"
capture label var stock_elast_imp_full     "Stock elasticity (impacted AGI base, all post)"
capture label var stock_elast_imp_ann      "Annualized stock elasticity (impacted AGI base, all post)"

capture label var stock_elast_total_common_shs "Stock elasticity +SHS (total AGI, 2021-22)"
capture label var stock_elast_total_full_shs   "Stock elasticity +SHS (total AGI, all post)"
capture label var stock_elast_total_ann_shs    "Annualized stock elasticity +SHS (total AGI)"
capture label var stock_elast_imp_common_shs   "Stock elasticity +SHS (impacted AGI, 2021-22)"
capture label var stock_elast_imp_full_shs     "Stock elasticity +SHS (impacted AGI, all post)"
capture label var stock_elast_imp_ann_shs      "Annualized stock elasticity +SHS (impacted AGI)"

** Save full dataset (all AGI specs with elasticities)
** Sort before save for deterministic row order across runs — isid at
** line 392 guarantees uniqueness but not order.
sort sample_data sample outcome controls exclusion
compress
save "${results}elasticities/elasticity_results.dta", replace

** Report highlighted AGI counts
qui count if preferred == 1
local n_preferred = r(N)
dis "Highlighted AGI specifications: `n_preferred'"

if `n_preferred' == 0 {
	dis as error "ERROR: No highlighted AGI specifications found."
	log close log_02elast
	error 2000
}

qui count if preferred == 1 & migration == "net" & outstate == 0 ///
	& !missing(stock_elast_total_common)
if r(N) == 0 {
	dis as error "ERROR: No highlighted domestic net-migration specs have stock elasticity results."
	dis as error "       Regenerate sdid_event_results.dta from 02_sdid_analysis.do."
	log close log_02elast
	error 2001
}

** Display highlighted AGI specs
dis ""
dis "Highlighted AGI specifications for elasticity table:"
list data_type sample migration tau se pre_mean ///
	beta_kleven stock_elast_total_common stock_elast_imp_ann ///
	if preferred == 1, sep(0) abbreviate(22)

********************************************************************************
** SECTION 2: LaTeX Table (AGI, Highlighted Specs Only)
********************************************************************************

dis ""
dis "=============================================="
dis "Section 2: LaTeX elasticity table"
dis "=============================================="

** Shared rate strings used in notes for both tables (defined once).
local pfa_pct : di %5.3f delta_t * 100
local pfa_pct = strtrim("`pfa_pct'")
local total_pct : di %5.1f avg_total_rate * 100
local total_pct = strtrim("`total_pct'")

** =========================================================================
** (a) Main table: highlighted AGI net migration
** =========================================================================

preserve
** Domestic (county-level) net migration only; out-of-state in appendix
keep if preferred == 1 & migration == "net" & outstate == 0

** Formatted strings. Emit cfi only for non-missing rows so the table loop
** can render missing values as "--" rather than a bare period.
gen str20 tau_str = string(tau, "%9.3f")
gen str20 se_str = "(" + string(se, "%9.3f") + ")"
gen str20 beta_str = string(beta_kleven, "%9.3f")
gen str20 beta_se_str = "(" + string(beta_se_kleven, "%9.3f") + ")"
gen str20 stock_common_str = string(stock_elast_total_common, "%9.3f") ///
	if !missing(stock_elast_total_common)

** Write LaTeX table
tempname fh
file open `fh' using "${results}elasticities/tbl_elasticities.tex", write replace

elast_tex_open, handle(`fh') ///
	cap("Highlighted AGI Net-Migration Elasticities (Kleven 2024 Framework)") ///
	lbl("tab:elasticities") cols("ll ccc")
file write `fh' "Data & Sample & $\hat{\tau}$ (pp) & Semi-$\varepsilon$ $\beta$ & Stock $\varepsilon$ \\" _n
file write `fh' " & & & (Kleven) & (Total AGI, 2021--2022) \\" _n
file write `fh' "\midrule" _n

** Sort for table output
sort data_type sample
local N = _N
local prev_dt ""

forvalues i = 1/`N' {
	local dt = data_type[`i']
	local smp = subinstr(sample[`i'], "sample_", "", .)
	local smp = proper("`smp'")
	local t_val = tau_str[`i']
	local se_val = se_str[`i']
	local b = beta_str[`i']
	local b_se = beta_se_str[`i']
	local stock = stock_common_str[`i']
	if "`stock'" == "" local stock "--"

	** Add spacing between data-type groups
	if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
		file write `fh' "\addlinespace" _n
	}
	local prev_dt "`dt'"

	** Point estimates row
	file write `fh' "`dt' & `smp' & `t_val' & `b' & `stock' \\" _n
	file write `fh' " & & `se_val' & `b_se' & \\" _n
}

elast_tex_notes_open, handle(`fh')
file write `fh' "Semi-elasticity $\beta$ follows Kleven et al.\ (2024, NBER WP 32153): " _n
file write `fh' "$\beta = (\hat{\tau}/100) / \Delta\ln(1-\tau_\text{total})$, where $\tau_\text{total}$ is the combined " _n
file write `fh' "federal income + Oregon state income + FICA employee share + PFA rate on impacted filers. " _n
file write `fh' "A negative $\beta$ for out-migration (or positive for in-migration) indicates more migration when the net-of-tax rate falls. " _n
file write `fh' "Kleven's informal reading of $\beta$ as ``pp change in the migration rate per pp change in the tax rate'' holds only when $\tau$ is small: " _n
file write `fh' "formally $\beta \approx -(1 - \bar{\tau}_\text{total}) \cdot (\Delta\text{mig}_\text{pp}/\Delta\tau_\text{pp})$, so at the Multnomah total rate of `total_pct'\% the log-NTR $\beta$ is roughly $(1-\bar{\tau}_\text{total}) \approx 0.60\times$ the naive pp-per-pp reading " _n
file write `fh' "(equivalently, the naive reading is $1/(1-\bar{\tau}_\text{total}) \approx 1.67\times$ $\beta$). " _n
file write `fh' "Stock elasticity is reported with respect to the after-tax rate: $\varepsilon_{\text{stock},H} = \Delta\ln S_H / \Delta\ln(1-\tau_\text{total})$. " _n
file write `fh' "For each post year $h$, we build the stock recursively from net migration effects using $\Delta\ln S_h = \ln(1 + \hat{\tau}_h s_\text{scale}/100)$ and sum those log changes through horizon $H$. " _n
file write `fh' "The table reports the 2021--2022 stock elasticity on the total AGI base, where $s_\text{scale} = 1$ for IRS and ACS All and $s_\text{scale} = s_\text{college}$ for ACS College. " _n
file write `fh' "Impacted-base stock elasticities are exported to the Excel workbook for revenue calculations. " _n
file write `fh' "This is a horizon-$H$ stock object, \emph{not} the Kleven steady-state stock elasticity $\beta \cdot (T+1)/2$, which would require a demographic lifespan $T$ that we do not estimate. " _n
file write `fh' "Positive values indicate that the AGI stock shrinks when the tax rate rises because the after-tax rate falls. " _n
file write `fh' "Average effective PFA rate: `pfa_pct'\%; average total tax rate on impacted filers: `total_pct'\%. " _n
file write `fh' "FICA reflects the employee share only. " _n
file write `fh' "Flow elasticities for gross migration are in Appendix Table~\ref{tab:elasticities_inout}. " _n
file write `fh' "Standard errors in parentheses are reported for $\hat{\tau}$ and $\beta$ only; " _n
file write `fh' "the current pipeline does not export joint event-study covariance matrices for the stock elasticity." _n
elast_tex_close, handle(`fh')

file close `fh'
restore

** =========================================================================
** (b) Appendix table: AGI out- and in-migration, Kleven β and flow ε
** =========================================================================

** Preferred net AGI comparison table: tau, after-tax change, flow semi,
** and stock elasticities
preserve
keep if preferred == 1 & migration == "net" & outstate == 0

gen str30 sample_label = ""
replace sample_label = "All counties" if sample == "sample_all"
replace sample_label = "COVID-Stringent Counties" if sample == "sample_stringency"
replace sample_label = proper(subinstr(sample, "sample_", "", .)) if sample_label == ""

gen str20 tau_str = string(tau, "%9.3f")
gen str20 dln_ntr_str = string(stock_dln_ntr, "%9.4f")
gen str20 beta_str = string(beta_kleven, "%9.3f")
gen str20 stock_common_str = string(stock_elast_total_common, "%9.3f") ///
	if !missing(stock_elast_total_common)
gen str20 stock_full_str = string(stock_elast_total_full, "%9.3f") ///
	if !missing(stock_elast_total_full)
gen str20 stock_ann_str = string(stock_elast_total_ann, "%9.3f") ///
	if !missing(stock_elast_total_ann)

tempname fh2
file open `fh2' using "${results}elasticities/tbl_elasticities_stock_compare.tex", write replace

elast_tex_open, handle(`fh2') ///
	cap("Preferred AGI Net-Migration: Flow and Stock Elasticities") ///
	lbl("tab:elasticities_stock_compare") cols("llcccccc")
file write `fh2' "Data & Sample & $\hat{\tau}$ (pp) & $\Delta \ln(1-t)$ & Flow Semi-$\varepsilon$ & \multicolumn{3}{c}{Stock Elasticity} \\" _n
file write `fh2' " & & & & & Common & Full & Annualized \\" _n
file write `fh2' "\cmidrule(lr){6-8}" _n
file write `fh2' "\midrule" _n

sort data_type sample
local N = _N
local prev_dt ""

forvalues i = 1/`N' {
	local dt = data_type[`i']
	local smp = sample_label[`i']
	local t_val = tau_str[`i']
	local dln = dln_ntr_str[`i']
	local b = beta_str[`i']
	local sc = stock_common_str[`i']
	local sf = stock_full_str[`i']
	local sa = stock_ann_str[`i']

	if "`sc'" == "" local sc "--"
	if "`sf'" == "" local sf "--"
	if "`sa'" == "" local sa "--"

	if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
		file write `fh2' "\addlinespace" _n
	}
	local prev_dt "`dt'"

	file write `fh2' "`dt' & `smp' & `t_val' & `dln' & `b' & `sc' & `sf' & `sa' \\" _n
}

elast_tex_notes_open, handle(`fh2')
file write `fh2' "$\hat{\tau}$ is the SDID coefficient on the AGI net-migration rate, reported in percentage points. " _n
file write `fh2' "$\Delta \ln(1-t)$ is the change in the log after-tax rate used in the elasticity denominator; for ACS College, the subgroup-specific after-tax change is used. " _n
file write `fh2' "Flow semi-elasticity is $\beta = (\hat{\tau}/100)/\Delta\ln(1-t)$. " _n
file write `fh2' "Stock elasticities are calculated on the total AGI base from cumulated net-migration event-study effects: $\varepsilon_{\text{stock},H} = \Delta\ln S_H / \Delta\ln(1-t)$. " _n
file write `fh2' "Common uses the 2021--2022 IRS-ACS overlap window, Full uses all available post years, and Annualized equals Full divided by the number of post years. " _n
file write `fh2' "Positive stock elasticities indicate that the AGI stock shrinks when the tax rate rises because the after-tax rate falls. " _n
elast_tex_close, handle(`fh2')

file close `fh2'
restore

preserve
keep if preferred == 1 & inlist(migration, "out", "in") & outstate == 0

** Formatted strings
gen str20 tau_str = string(tau, "%9.3f")
gen str20 se_str = "(" + string(se, "%9.3f") + ")"
gen str20 beta_str = string(beta_kleven, "%9.3f")
gen str20 beta_se_str = "(" + string(beta_se_kleven, "%9.3f") + ")"
gen str20 fe_str = string(flow_e, "%9.3f") if !missing(flow_e)
gen str20 fe_se_str = "(" + string(flow_se, "%9.3f") + ")" if !missing(flow_se)

** Migration label
gen str20 migr_label = ""
replace migr_label = "In" if migration == "in"
replace migr_label = "Out" if migration == "out"

tempname fh
file open `fh' using "${results}elasticities/tbl_elasticities_inout.tex", write replace

elast_tex_open, handle(`fh') ///
	cap("Highlighted Gross AGI Migration Elasticities (Kleven 2024 Framework)") ///
	lbl("tab:elasticities_inout") cols("lll ccc") ///
	fontsize("footnotesize")
file write `fh' "Data & Sample & Dir.\ & $\hat{\tau}$ (pp) & Semi-$\varepsilon$ $\beta$ & Flow $\varepsilon$ \\" _n
file write `fh' "\midrule" _n

sort data_type sample migration

** Panel A: Out-migration
file write `fh' "\addlinespace" _n
file write `fh' "\multicolumn{6}{l}{\textit{Panel A: Out-Migration}} \\" _n
file write `fh' "\addlinespace" _n
elast_inout_panel, handle(`fh') direction("out")

** Panel B: In-migration
file write `fh' "\addlinespace[0.75em]" _n
file write `fh' "\midrule" _n
file write `fh' "\addlinespace" _n
file write `fh' "\multicolumn{6}{l}{\textit{Panel B: In-Migration}} \\" _n
file write `fh' "\addlinespace" _n
elast_inout_panel, handle(`fh') direction("in")

elast_tex_notes_open, handle(`fh')
file write `fh' "Semi-elasticity $\beta$ follows Kleven et al.\ (2024, NBER WP 32153): " _n
file write `fh' "$\beta = (\hat{\tau}/100) / \Delta\ln(1-\tau_\text{total})$, where $\tau_\text{total}$ is the combined " _n
file write `fh' "federal income + Oregon state income + FICA employee share + PFA rate on impacted filers. " _n
file write `fh' "Flow elasticity: $\varepsilon_\text{flow} = -(\hat{\tau}/\bar{r}_\text{pre}) / \Delta\ln(1-\tau_\text{total})$ " _n
file write `fh' "where $\bar{r}_\text{pre}$ is the pre-treatment migration rate; undefined for net migration (rate $\approx 0$). " _n
file write `fh' "FICA reflects the employee share only. " _n
file write `fh' "Sign convention: $\beta = (\hat{\tau}/100) / \Delta\ln(1-\tau_\text{total})$ with $\Delta\ln(1-\tau_\text{total}) < 0$ under a tax hike. " _n
file write `fh' "For out-migration, \emph{negative} $\beta$ indicates a larger outflow when the tax rate rises; " _n
file write `fh' "for in-migration, \emph{positive} $\beta$ indicates a smaller inflow. " _n
file write `fh' "Average effective PFA rate: `pfa_pct'\%; total rate on impacted filers: `total_pct'\%. " _n
file write `fh' "Standard errors in parentheses, derived from SDID bootstrap SEs." _n
elast_tex_close, handle(`fh')

file close `fh'
restore

** =========================================================================
** (c) SHS-inclusive parallel tables (PFA + Metro SHS 1% in the denominator)
** Structure mirrors (a)/(b) exactly — only the denominator changes, so we
** reuse all rendering patterns with *_shs variables.
** =========================================================================

local shs_pct        : di %5.3f avg_shs_rate * 100
local shs_pct        = strtrim("`shs_pct'")
local total_shs_pct  : di %5.1f avg_total_rate_with_shs * 100
local total_shs_pct  = strtrim("`total_shs_pct'")

** ---- SHS main table: AGI net migration (domestic only) ----
preserve
keep if preferred == 1 & migration == "net" & outstate == 0

gen str20 tau_str = string(tau, "%9.3f")
gen str20 se_str = "(" + string(se, "%9.3f") + ")"
gen str20 beta_str = string(beta_kleven_shs, "%9.3f")
gen str20 beta_se_str = "(" + string(beta_se_kleven_shs, "%9.3f") + ")"
gen str20 stock_common_str = string(stock_elast_total_common_shs, "%9.3f") ///
	if !missing(stock_elast_total_common_shs)

tempname fh_shs
file open `fh_shs' using "${results}elasticities/tbl_elasticities_shs.tex", ///
	write replace

elast_tex_open, handle(`fh_shs') ///
	cap("Highlighted AGI Net-Migration Elasticities Including Metro SHS 1\% (Kleven 2024 Framework)") ///
	lbl("tab:elasticities_shs") cols("ll ccc")
file write `fh_shs' "Data & Sample & $\hat{\tau}$ (pp) & Semi-$\varepsilon$ $\beta$ & Stock $\varepsilon$ \\" _n
file write `fh_shs' " & & & (Kleven, +SHS) & (Total AGI, 2021--2022, +SHS) \\" _n
file write `fh_shs' "\midrule" _n

sort data_type sample
local N = _N
local prev_dt ""

forvalues i = 1/`N' {
	local dt = data_type[`i']
	local smp = subinstr(sample[`i'], "sample_", "", .)
	local smp = proper("`smp'")
	local t_val = tau_str[`i']
	local se_val = se_str[`i']
	local b = beta_str[`i']
	local b_se = beta_se_str[`i']
	local stock = stock_common_str[`i']
	if "`stock'" == "" local stock "--"

	if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
		file write `fh_shs' "\addlinespace" _n
	}
	local prev_dt "`dt'"

	file write `fh_shs' "`dt' & `smp' & `t_val' & `b' & `stock' \\" _n
	file write `fh_shs' " & & `se_val' & `b_se' & \\" _n
}

elast_tex_notes_open, handle(`fh_shs')
file write `fh_shs' "This table repeats Table~\ref{tab:elasticities} using a denominator that also includes the Portland Metro Supportive Housing Services (SHS) tax: " _n
file write `fh_shs' "a flat 1\% on income above \$125{,}000 single / \$200{,}000 joint, effective 2021. " _n
file write `fh_shs' "SHS applies throughout Metro (Multnomah, Washington, and Clackamas counties); with a national SDID donor pool, SHS is part of the differential tax change for Multnomah in 2021, " _n
file write `fh_shs' "so including it in $\Delta\ln(1-\tau_\text{total})$ produces a more conservative (smaller in magnitude) $\beta$. " _n
file write `fh_shs' "Average effective SHS rate on impacted filers: `shs_pct'\%; total rate including SHS: `total_shs_pct'\%. " _n
file write `fh_shs' "Point estimates of $\hat{\tau}$ are unchanged relative to Table~\ref{tab:elasticities} — only the denominator differs. " _n
file write `fh_shs' "Standard errors in parentheses treat revenue parameters as known." _n
elast_tex_close, handle(`fh_shs')

file close `fh_shs'
restore

** ---- SHS stock-compare table (net only) ----
preserve
keep if preferred == 1 & migration == "net" & outstate == 0

gen str30 sample_label = ""
replace sample_label = "All counties" if sample == "sample_all"
replace sample_label = "COVID-Stringent Counties" if sample == "sample_stringency"
replace sample_label = proper(subinstr(sample, "sample_", "", .)) if sample_label == ""

gen str20 tau_str = string(tau, "%9.3f")
gen str20 dln_ntr_str = string(stock_dln_ntr_shs, "%9.4f")
gen str20 beta_str = string(beta_kleven_shs, "%9.3f")
gen str20 stock_common_str = string(stock_elast_total_common_shs, "%9.3f") ///
	if !missing(stock_elast_total_common_shs)
gen str20 stock_full_str = string(stock_elast_total_full_shs, "%9.3f") ///
	if !missing(stock_elast_total_full_shs)
gen str20 stock_ann_str = string(stock_elast_total_ann_shs, "%9.3f") ///
	if !missing(stock_elast_total_ann_shs)

tempname fh2_shs
file open `fh2_shs' using "${results}elasticities/tbl_elasticities_stock_compare_shs.tex", ///
	write replace

elast_tex_open, handle(`fh2_shs') ///
	cap("Preferred AGI Net-Migration: Flow and Stock Elasticities Including Metro SHS 1\%") ///
	lbl("tab:elasticities_stock_compare_shs") cols("llcccccc")
file write `fh2_shs' "Data & Sample & $\hat{\tau}$ (pp) & $\Delta \ln(1-t)$ & Flow Semi-$\varepsilon$ & \multicolumn{3}{c}{Stock Elasticity} \\" _n
file write `fh2_shs' " & & & (+SHS) & (+SHS) & Common & Full & Annualized \\" _n
file write `fh2_shs' "\cmidrule(lr){6-8}" _n
file write `fh2_shs' "\midrule" _n

sort data_type sample
local N = _N
local prev_dt ""

forvalues i = 1/`N' {
	local dt = data_type[`i']
	local smp = sample_label[`i']
	local t_val = tau_str[`i']
	local dln = dln_ntr_str[`i']
	local b = beta_str[`i']
	local sc = stock_common_str[`i']
	local sf = stock_full_str[`i']
	local sa = stock_ann_str[`i']

	if "`sc'" == "" local sc "--"
	if "`sf'" == "" local sf "--"
	if "`sa'" == "" local sa "--"

	if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
		file write `fh2_shs' "\addlinespace" _n
	}
	local prev_dt "`dt'"

	file write `fh2_shs' "`dt' & `smp' & `t_val' & `dln' & `b' & `sc' & `sf' & `sa' \\" _n
}

elast_tex_notes_open, handle(`fh2_shs')
file write `fh2_shs' "SHS-inclusive version of Table~\ref{tab:elasticities_stock_compare}. " _n
file write `fh2_shs' "$\Delta\ln(1-t)$ and all elasticity columns use the combined PFA + Metro SHS denominator. " _n
file write `fh2_shs' "Average effective SHS rate on impacted filers: `shs_pct'\%. " _n
file write `fh2_shs' "Interpretation and sign conventions follow Table~\ref{tab:elasticities_stock_compare}." _n
elast_tex_close, handle(`fh2_shs')

file close `fh2_shs'
restore

** ---- SHS in/out table (gross migration) ----
preserve
keep if preferred == 1 & inlist(migration, "out", "in") & outstate == 0

gen str20 tau_str = string(tau, "%9.3f")
gen str20 se_str = "(" + string(se, "%9.3f") + ")"
gen str20 beta_str = string(beta_kleven_shs, "%9.3f")
gen str20 beta_se_str = "(" + string(beta_se_kleven_shs, "%9.3f") + ")"
gen str20 fe_str = string(flow_e_shs, "%9.3f") if !missing(flow_e_shs)
gen str20 fe_se_str = "(" + string(flow_se_shs, "%9.3f") + ")" if !missing(flow_se_shs)

gen str20 migr_label = ""
replace migr_label = "In" if migration == "in"
replace migr_label = "Out" if migration == "out"

tempname fh_shs_io
file open `fh_shs_io' using "${results}elasticities/tbl_elasticities_inout_shs.tex", ///
	write replace

elast_tex_open, handle(`fh_shs_io') ///
	cap("Highlighted Gross AGI Migration Elasticities Including Metro SHS 1\% (Kleven 2024 Framework)") ///
	lbl("tab:elasticities_inout_shs") cols("lll ccc") ///
	fontsize("footnotesize")
file write `fh_shs_io' "Data & Sample & Dir.\ & $\hat{\tau}$ (pp) & Semi-$\varepsilon$ $\beta$ & Flow $\varepsilon$ \\" _n
file write `fh_shs_io' "\midrule" _n

sort data_type sample migration

** Panel A: Out-migration
file write `fh_shs_io' "\addlinespace" _n
file write `fh_shs_io' "\multicolumn{6}{l}{\textit{Panel A: Out-Migration}} \\" _n
file write `fh_shs_io' "\addlinespace" _n
elast_inout_panel, handle(`fh_shs_io') direction("out")

** Panel B: In-migration
file write `fh_shs_io' "\addlinespace[0.75em]" _n
file write `fh_shs_io' "\midrule" _n
file write `fh_shs_io' "\addlinespace" _n
file write `fh_shs_io' "\multicolumn{6}{l}{\textit{Panel B: In-Migration}} \\" _n
file write `fh_shs_io' "\addlinespace" _n
elast_inout_panel, handle(`fh_shs_io') direction("in")

elast_tex_notes_open, handle(`fh_shs_io')
file write `fh_shs_io' "SHS-inclusive version of Table~\ref{tab:elasticities_inout}. " _n
file write `fh_shs_io' "Denominator includes PFA + Metro SHS 1\%; $\hat{\tau}$ is unchanged. " _n
file write `fh_shs_io' "Average effective SHS rate on impacted filers: `shs_pct'\%; total rate including SHS: `total_shs_pct'\%. " _n
file write `fh_shs_io' "Sign conventions follow Table~\ref{tab:elasticities_inout}." _n
elast_tex_close, handle(`fh_shs_io')

file close `fh_shs_io'
restore

** ---- Copy to Overleaf ----
if "${overleaf}" == "1" {
	copy "${results}elasticities/tbl_elasticities.tex" ///
		"${ol_tab}tbl_elasticities.tex", replace
	copy "${results}elasticities/tbl_elasticities_stock_compare.tex" ///
		"${ol_tab}tbl_elasticities_stock_compare.tex", replace
	copy "${results}elasticities/tbl_elasticities_inout.tex" ///
		"${ol_tab}tbl_elasticities_inout.tex", replace
	copy "${results}elasticities/tbl_elasticities_shs.tex" ///
		"${ol_tab}tbl_elasticities_shs.tex", replace
	copy "${results}elasticities/tbl_elasticities_stock_compare_shs.tex" ///
		"${ol_tab}tbl_elasticities_stock_compare_shs.tex", replace
	copy "${results}elasticities/tbl_elasticities_inout_shs.tex" ///
		"${ol_tab}tbl_elasticities_inout_shs.tex", replace
}

** Export Excel workbook
** Sheet 1: full raw results
export excel using "${results}elasticities/tbl_elasticities.xlsx", ///
	sheet("full_results") firstrow(variables) replace

** Sheet 2: curated calculation components for easy Excel recomputation
preserve
keep sample_data sample outcome controls exclusion preferred data_type ///
	period_type migration outstate tau se pre_mean ///
	scale_total scale_taxbase stock_dln_ntr stock_dln_ntr_shs ///
	cum_tau_common H_common cum_tau_full H_full ///
	ln_stock_chg_common_total ln_stock_chg_full_total ///
	ln_stock_chg_common_imp ln_stock_chg_full_imp ///
	beta_kleven beta_se_kleven flow_e flow_se ///
	beta_kleven_shs beta_se_kleven_shs flow_e_shs flow_se_shs ///
	stock_elast_total_common stock_elast_total_full stock_elast_total_ann ///
	stock_elast_imp_common stock_elast_imp_full stock_elast_imp_ann ///
	stock_elast_total_common_shs stock_elast_total_full_shs stock_elast_total_ann_shs ///
	stock_elast_imp_common_shs stock_elast_imp_full_shs stock_elast_imp_ann_shs

rename sample_data spec_sample_data
rename sample spec_sample
rename outcome spec_outcome
rename controls spec_controls
rename exclusion spec_exclusion
rename preferred spec_highlighted
rename data_type spec_data_type
rename period_type spec_period_type
rename migration spec_migration
rename outstate spec_outstate

gen double input_tau_pp = tau
gen double input_tau_decimal = tau / 100
gen double input_tau_se_pp = se
gen double input_pre_mean_rate = pre_mean
gen double input_delta_pfa_rate = delta_t
gen double input_avg_tot_rate_post = avg_total_rate
gen double input_avg_tot_rate_pre = avg_total_rate_pre
gen double input_dln_ntr_total = delta_ln_ntr_total
gen double input_avg_tot_rate_post_col = avg_total_rate_college
gen double input_avg_tot_rate_pre_col = avg_total_rate_pre_college
gen double input_dln_ntr_total_col = delta_ln_ntr_total_college
gen double input_stock_dln_ntr = stock_dln_ntr
gen double input_impacted_agi_share = impacted_agi_share
gen double input_col_agi_share = college_agi_share
gen double input_scale_total_agi = scale_total
gen double input_scale_impacted_agi = scale_taxbase
gen double input_cum_tau_common_pp = cum_tau_common
gen double input_cum_tau_full_pp = cum_tau_full
gen double input_common_horizon_yrs = H_common
gen double input_full_horizon_yrs = H_full
gen double input_ln_stock_chg_common_total = ln_stock_chg_common_total
gen double input_ln_stock_chg_full_total = ln_stock_chg_full_total
gen double input_ln_stock_chg_common_imp = ln_stock_chg_common_imp
gen double input_ln_stock_chg_full_imp = ln_stock_chg_full_imp

** SHS-inclusive inputs (same τ̂; denominator differs)
gen double input_avg_shs_rate = avg_shs_rate
gen double input_avg_tot_rate_post_shs = avg_total_rate_with_shs
gen double input_avg_tot_rate_pre_shs = avg_total_rate_pre_with_shs
gen double input_dln_ntr_total_shs = delta_ln_ntr_total_shs
gen double input_avg_tot_rate_post_col_shs = avg_total_rate_col_with_shs
gen double input_avg_tot_rate_pre_col_shs = avg_total_rate_pre_col_with_shs
gen double input_dln_ntr_total_col_shs = delta_ln_ntr_total_college_shs
gen double input_stock_dln_ntr_shs = stock_dln_ntr_shs

gen double result_net_of_tax_semi_elast = beta_kleven
gen double result_net_of_tax_semi_se = beta_se_kleven
gen double result_gross_flow_elast = flow_e
gen double result_gross_flow_elast_se = flow_se
gen double result_stock_elast_total_common = stock_elast_total_common
gen double result_stock_elast_total_full = stock_elast_total_full
gen double result_stock_elast_total_ann = stock_elast_total_ann
gen double result_stock_elast_imp_common = stock_elast_imp_common
gen double result_stock_elast_imp_full = stock_elast_imp_full
gen double result_stock_elast_imp_ann = stock_elast_imp_ann

** SHS-inclusive results
gen double result_net_of_tax_semi_elast_shs = beta_kleven_shs
gen double result_net_of_tax_semi_se_shs = beta_se_kleven_shs
gen double result_gross_flow_elast_shs = flow_e_shs
gen double result_gross_flow_elast_se_shs = flow_se_shs
gen double result_stock_elast_tot_common_shs = stock_elast_total_common_shs
gen double result_stock_elast_tot_full_shs = stock_elast_total_full_shs
gen double result_stock_elast_tot_ann_shs = stock_elast_total_ann_shs
gen double result_stock_elast_imp_common_shs = stock_elast_imp_common_shs
gen double result_stock_elast_imp_full_shs = stock_elast_imp_full_shs
gen double result_stock_elast_imp_ann_shs = stock_elast_imp_ann_shs

	order spec_sample_data spec_sample spec_outcome spec_controls spec_exclusion ///
		spec_highlighted spec_data_type spec_period_type spec_migration ///
		spec_outstate ///
		input_tau_pp input_tau_decimal input_tau_se_pp input_pre_mean_rate ///
		input_delta_pfa_rate input_avg_tot_rate_post input_avg_tot_rate_pre ///
		input_dln_ntr_total ///
		input_avg_tot_rate_post_col input_avg_tot_rate_pre_col ///
		input_dln_ntr_total_col input_stock_dln_ntr ///
		input_avg_shs_rate ///
		input_avg_tot_rate_post_shs input_avg_tot_rate_pre_shs ///
		input_dln_ntr_total_shs ///
		input_avg_tot_rate_post_col_shs input_avg_tot_rate_pre_col_shs ///
		input_dln_ntr_total_col_shs input_stock_dln_ntr_shs ///
		input_impacted_agi_share input_col_agi_share ///
		input_scale_total_agi input_scale_impacted_agi ///
		input_cum_tau_common_pp input_cum_tau_full_pp ///
		input_common_horizon_yrs input_full_horizon_yrs ///
		input_ln_stock_chg_common_total input_ln_stock_chg_full_total ///
		input_ln_stock_chg_common_imp input_ln_stock_chg_full_imp ///
		result_net_of_tax_semi_elast result_net_of_tax_semi_se ///
		result_gross_flow_elast result_gross_flow_elast_se ///
		result_stock_elast_total_common result_stock_elast_total_full ///
		result_stock_elast_total_ann ///
		result_stock_elast_imp_common result_stock_elast_imp_full ///
		result_stock_elast_imp_ann ///
		result_net_of_tax_semi_elast_shs result_net_of_tax_semi_se_shs ///
		result_gross_flow_elast_shs result_gross_flow_elast_se_shs ///
		result_stock_elast_tot_common_shs result_stock_elast_tot_full_shs ///
		result_stock_elast_tot_ann_shs ///
		result_stock_elast_imp_common_shs result_stock_elast_imp_full_shs ///
		result_stock_elast_imp_ann_shs

export excel using "${results}elasticities/tbl_elasticities.xlsx", ///
	sheet("recalc_components") firstrow(variables) sheetreplace
restore

** Sheet 2b: preferred net stock comparison table for quick use
preserve
keep if preferred == 1 & migration == "net" & outstate == 0

gen str30 table_sample = ""
replace table_sample = "All counties" if sample == "sample_all"
replace table_sample = "COVID-Stringent Counties" if sample == "sample_stringency"
replace table_sample = proper(subinstr(sample, "sample_", "", .)) if table_sample == ""

keep data_type table_sample tau stock_dln_ntr beta_kleven ///
	stock_elast_total_common stock_elast_total_full stock_elast_total_ann

rename data_type table_data
rename tau table_tau_pp
rename stock_dln_ntr table_dln_aftertax
rename beta_kleven table_flow_semi_elast
rename stock_elast_total_common table_stock_common
rename stock_elast_total_full table_stock_full
rename stock_elast_total_ann table_stock_annualized

order table_data table_sample table_tau_pp table_dln_aftertax ///
	table_flow_semi_elast table_stock_common table_stock_full ///
	table_stock_annualized

export excel using "${results}elasticities/tbl_elasticities.xlsx", ///
	sheet("preferred_net_stock") firstrow(variables) sheetreplace
export delimited using "${results}elasticities/preferred_net_stock.csv", replace
restore

** Sheet 2c: SHS-inclusive preferred net stock comparison
preserve
keep if preferred == 1 & migration == "net" & outstate == 0

gen str30 table_sample = ""
replace table_sample = "All counties" if sample == "sample_all"
replace table_sample = "COVID-Stringent Counties" if sample == "sample_stringency"
replace table_sample = proper(subinstr(sample, "sample_", "", .)) if table_sample == ""

keep data_type table_sample tau stock_dln_ntr_shs beta_kleven_shs ///
	stock_elast_total_common_shs stock_elast_total_full_shs stock_elast_total_ann_shs

rename data_type table_data
rename tau table_tau_pp
rename stock_dln_ntr_shs table_dln_aftertax_shs
rename beta_kleven_shs table_flow_semi_elast_shs
rename stock_elast_total_common_shs table_stock_common_shs
rename stock_elast_total_full_shs table_stock_full_shs
rename stock_elast_total_ann_shs table_stock_annualized_shs

order table_data table_sample table_tau_pp table_dln_aftertax_shs ///
	table_flow_semi_elast_shs table_stock_common_shs table_stock_full_shs ///
	table_stock_annualized_shs

export excel using "${results}elasticities/tbl_elasticities.xlsx", ///
	sheet("preferred_net_stock_shs") firstrow(variables) sheetreplace
export delimited using "${results}elasticities/preferred_net_stock_shs.csv", replace
restore

** Sheet 3: one-row run parameters / denominators
preserve
clear
set obs 1
gen double avg_mt_rate_pfa = avg_mt_rate
gen double avg_state_rate = avg_state_rate
gen double avg_total_rate_post = avg_total_rate
gen double avg_total_rate_pre = avg_total_rate_pre
gen double avg_total_rate_post_col = avg_total_rate_college
gen double avg_total_rate_pre_col = avg_total_rate_pre_college
gen double delta_pfa_rate = delta_t
gen double delta_ln_ntr_total = delta_ln_ntr_total
gen double delta_ln_ntr_total_col = delta_ln_ntr_total_college
gen double agi_total = agi_total
gen double agi_impacted = agi_impacted
gen double impacted_agi_share = impacted_agi_share
gen double agi_col = agi_college
gen double col_agi_share = college_agi_share
gen double agi_col_impacted = agi_college_impacted
gen double col_impacted_agi_share = college_impacted_agi_share
gen double avg_mt_rate_col_impacted = avg_mt_rate_college_impacted

** SHS-inclusive companions
gen double avg_shs_rate = avg_shs_rate
gen double avg_shs_rate_college = avg_shs_rate_college
gen double avg_total_rate_post_shs = avg_total_rate_with_shs
gen double avg_total_rate_pre_shs = avg_total_rate_pre_with_shs
gen double avg_total_rate_post_col_shs = avg_total_rate_col_with_shs
gen double avg_total_rate_pre_col_shs = avg_total_rate_pre_col_with_shs
gen double delta_ln_ntr_total_shs = delta_ln_ntr_total_shs
gen double delta_ln_ntr_total_col_shs = delta_ln_ntr_total_college_shs

export excel using "${results}elasticities/tbl_elasticities.xlsx", ///
	sheet("run_parameters") firstrow(variables) sheetreplace
restore

** Sheet 4: variable guide for workbook fields
preserve
clear
tempname guideh
tempfile guide
postfile `guideh' str20 sheet str40 variable_name str244 description using `guide', replace

post `guideh' ("recalc_components") ("spec_sample_data") ("Underlying source panel used for the SDID estimate, such as irs_full_16_22 or acs_16_24_col.")
post `guideh' ("recalc_components") ("spec_sample") ("Donor-pool restriction used in the specification, such as sample_all or sample_stringency.")
post `guideh' ("recalc_components") ("spec_outcome") ("Outcome variable used in the SDID estimate.")
post `guideh' ("recalc_components") ("spec_controls") ("Indicator for whether the SDID specification includes covariates.")
post `guideh' ("recalc_components") ("spec_exclusion") ("Indicator for whether the specification excludes 2020 from estimation.")
post `guideh' ("recalc_components") ("spec_highlighted") ("Equals 1 for the highlighted benchmark specifications used in the paper.")
post `guideh' ("recalc_components") ("spec_data_type") ("Human-readable data-source label, such as IRS, ACS All, or ACS College.")
post `guideh' ("recalc_components") ("spec_period_type") ("Sample window label for the underlying panel, such as 16-22 or 16-24.")
post `guideh' ("recalc_components") ("spec_migration") ("Migration-flow type: net, in, or out.")
post `guideh' ("recalc_components") ("spec_outstate") ("Equals 1 for out-of-state migration outcomes and 0 for national outcomes.")
post `guideh' ("recalc_components") ("input_tau_pp") ("Estimated SDID treatment effect in percentage points.")
post `guideh' ("recalc_components") ("input_tau_decimal") ("Estimated SDID treatment effect converted from percentage points to decimal units.")
post `guideh' ("recalc_components") ("input_tau_se_pp") ("Bootstrap standard error for the SDID treatment effect, in percentage points.")
post `guideh' ("recalc_components") ("input_pre_mean_rate") ("Pre-period mean migration rate used as the base for gross-flow elasticities.")
post `guideh' ("recalc_components") ("input_delta_pfa_rate") ("Average PFA tax-rate increase used for semi-elasticity calculations.")
post `guideh' ("recalc_components") ("input_avg_tot_rate_post") ("Average post-policy total tax rate for the impacted filer base.")
post `guideh' ("recalc_components") ("input_avg_tot_rate_pre") ("Average pre-policy total tax rate for the impacted filer base.")
post `guideh' ("recalc_components") ("input_dln_ntr_total") ("Change in log net-of-tax rate using the total tax burden for the main impacted filer base.")
post `guideh' ("recalc_components") ("input_avg_tot_rate_post_col") ("Average post-policy total tax rate for the college proxy subgroup.")
post `guideh' ("recalc_components") ("input_avg_tot_rate_pre_col") ("Average pre-policy total tax rate for the college proxy subgroup.")
post `guideh' ("recalc_components") ("input_dln_ntr_total_col") ("Change in log net-of-tax rate using the total tax burden for the college proxy subgroup.")
post `guideh' ("recalc_components") ("input_stock_dln_ntr") ("Specification-specific change in log net-of-tax rate used in the stock elasticity calculation.")
post `guideh' ("recalc_components") ("input_impacted_agi_share") ("Share of county AGI accounted for by filers directly affected by the tax.")
post `guideh' ("recalc_components") ("input_col_agi_share") ("Share of county AGI accounted for by the ACS college proxy subgroup.")
post `guideh' ("recalc_components") ("input_scale_total_agi") ("Scale factor that maps the SDID outcome onto the total AGI base.")
post `guideh' ("recalc_components") ("input_scale_impacted_agi") ("Scale factor that maps the SDID outcome onto the impacted AGI base.")
post `guideh' ("recalc_components") ("input_cum_tau_common_pp") ("Sum of event-study treatment effects over the common IRS-ACS post window, in percentage points.")
post `guideh' ("recalc_components") ("input_cum_tau_full_pp") ("Sum of event-study treatment effects over the full available post window, in percentage points.")
post `guideh' ("recalc_components") ("input_common_horizon_yrs") ("Number of post-policy years in the common cumulative horizon.")
post `guideh' ("recalc_components") ("input_full_horizon_yrs") ("Number of post-policy years in the full cumulative horizon.")
post `guideh' ("recalc_components") ("input_ln_stock_chg_common_total") ("Cumulative log change in the total AGI stock over the common IRS-ACS post window.")
post `guideh' ("recalc_components") ("input_ln_stock_chg_full_total") ("Cumulative log change in the total AGI stock over the full available post window.")
post `guideh' ("recalc_components") ("input_ln_stock_chg_common_imp") ("Cumulative log change in the impacted AGI stock over the common IRS-ACS post window.")
post `guideh' ("recalc_components") ("input_ln_stock_chg_full_imp") ("Cumulative log change in the impacted AGI stock over the full available post window.")
post `guideh' ("recalc_components") ("result_net_of_tax_semi_elast") ("Net-of-tax semi-elasticity: SDID effect divided by the change in the log net-of-tax rate.")
post `guideh' ("recalc_components") ("result_net_of_tax_semi_se") ("Standard error for the net-of-tax semi-elasticity.")
post `guideh' ("recalc_components") ("result_gross_flow_elast") ("Gross-flow elasticity for in- or out-migration, using the pre-period gross flow mean as the base.")
post `guideh' ("recalc_components") ("result_gross_flow_elast_se") ("Standard error for the gross-flow elasticity.")
post `guideh' ("recalc_components") ("result_stock_elast_total_common") ("Cumulative stock elasticity on the total AGI base over the common IRS-ACS post window.")
post `guideh' ("recalc_components") ("result_stock_elast_total_full") ("Cumulative stock elasticity on the total AGI base over the full available post window.")
post `guideh' ("recalc_components") ("result_stock_elast_total_ann") ("Annualized cumulative stock elasticity on the total AGI base over the full available post window.")
post `guideh' ("recalc_components") ("result_stock_elast_imp_common") ("Cumulative stock elasticity on the impacted AGI base over the common IRS-ACS post window.")
post `guideh' ("recalc_components") ("result_stock_elast_imp_full") ("Cumulative stock elasticity on the impacted AGI base over the full available post window.")
post `guideh' ("recalc_components") ("result_stock_elast_imp_ann") ("Annualized cumulative stock elasticity on the impacted AGI base over the full available post window.")

post `guideh' ("preferred_net_stock") ("table_data") ("Data source label for the preferred domestic net-migration specification.")
post `guideh' ("preferred_net_stock") ("table_sample") ("Presentation-ready donor-pool label used in the preferred comparison table.")
post `guideh' ("preferred_net_stock") ("table_tau_pp") ("SDID coefficient on the AGI net-migration rate, in percentage points.")
post `guideh' ("preferred_net_stock") ("table_dln_aftertax") ("Change in the log after-tax rate used in the elasticity denominator.")
post `guideh' ("preferred_net_stock") ("table_flow_semi_elast") ("Flow semi-elasticity, computed as (tau/100) divided by the change in the log after-tax rate.")
post `guideh' ("preferred_net_stock") ("table_stock_common") ("Total-AGI stock elasticity over the common 2021-2022 IRS-ACS post window.")
post `guideh' ("preferred_net_stock") ("table_stock_full") ("Total-AGI stock elasticity over the full available post window.")
post `guideh' ("preferred_net_stock") ("table_stock_annualized") ("Annualized total-AGI stock elasticity over the full available post window.")

post `guideh' ("run_parameters") ("avg_mt_rate_pfa") ("Average effective PFA tax rate used as the policy-rate change.")
post `guideh' ("run_parameters") ("avg_state_rate") ("Average Oregon state income-tax rate for the impacted filer base.")
post `guideh' ("run_parameters") ("avg_total_rate_post") ("Average post-policy total tax rate for the main impacted filer base.")
post `guideh' ("run_parameters") ("avg_total_rate_pre") ("Average pre-policy total tax rate for the main impacted filer base.")
post `guideh' ("run_parameters") ("avg_total_rate_post_col") ("Average post-policy total tax rate for the college proxy subgroup.")
post `guideh' ("run_parameters") ("avg_total_rate_pre_col") ("Average pre-policy total tax rate for the college proxy subgroup.")
post `guideh' ("run_parameters") ("delta_pfa_rate") ("Average PFA tax-rate increase used in semi-elasticity calculations.")
post `guideh' ("run_parameters") ("delta_ln_ntr_total") ("Change in log net-of-tax rate for the main impacted filer base.")
post `guideh' ("run_parameters") ("delta_ln_ntr_total_col") ("Change in log net-of-tax rate for the college proxy subgroup.")
post `guideh' ("run_parameters") ("agi_total") ("Total county AGI used as the broad stock base for scaling.")
post `guideh' ("run_parameters") ("agi_impacted") ("County AGI attributable to filers directly affected by the PFA tax.")
post `guideh' ("run_parameters") ("impacted_agi_share") ("Share of county AGI attributable to filers directly affected by the PFA tax.")
post `guideh' ("run_parameters") ("agi_col") ("County AGI attributable to the ACS college proxy subgroup.")
post `guideh' ("run_parameters") ("col_agi_share") ("Share of county AGI attributable to the ACS college proxy subgroup.")
post `guideh' ("run_parameters") ("agi_col_impacted") ("County AGI attributable to college-proxy filers who are also in the impacted tax base.")
post `guideh' ("run_parameters") ("col_impacted_agi_share") ("Share of county AGI attributable to college-proxy filers in the impacted tax base.")
post `guideh' ("run_parameters") ("avg_mt_rate_col_impacted") ("Average effective PFA tax rate for the college-proxy subgroup within the impacted tax base.")

** SHS-inclusive guide entries
post `guideh' ("recalc_components") ("input_avg_shs_rate") ("Average effective Metro SHS rate (1% flat) on impacted filers.")
post `guideh' ("recalc_components") ("input_avg_tot_rate_post_shs") ("Post-policy total tax rate on impacted filers including SHS.")
post `guideh' ("recalc_components") ("input_avg_tot_rate_pre_shs") ("Pre-policy total tax rate on impacted filers (SHS was new in 2021).")
post `guideh' ("recalc_components") ("input_dln_ntr_total_shs") ("Change in log net-of-tax rate including SHS for the main impacted filer base.")
post `guideh' ("recalc_components") ("input_avg_tot_rate_post_col_shs") ("Post-policy total tax rate on college-impacted filers including SHS.")
post `guideh' ("recalc_components") ("input_avg_tot_rate_pre_col_shs") ("Pre-policy total tax rate on college-impacted filers.")
post `guideh' ("recalc_components") ("input_dln_ntr_total_col_shs") ("Change in log net-of-tax rate including SHS for the college proxy subgroup.")
post `guideh' ("recalc_components") ("input_stock_dln_ntr_shs") ("Specification-specific Δln(1−τ) including SHS used in stock elasticity.")
post `guideh' ("recalc_components") ("result_net_of_tax_semi_elast_shs") ("Kleven semi-elasticity β computed with the PFA+SHS denominator.")
post `guideh' ("recalc_components") ("result_net_of_tax_semi_se_shs") ("Standard error of the SHS-inclusive semi-elasticity.")
post `guideh' ("recalc_components") ("result_gross_flow_elast_shs") ("Gross-flow elasticity (in/out) with the SHS-inclusive denominator.")
post `guideh' ("recalc_components") ("result_gross_flow_elast_se_shs") ("Standard error of the SHS-inclusive gross-flow elasticity.")
post `guideh' ("recalc_components") ("result_stock_elast_tot_common_shs") ("Stock elasticity on the total AGI base, 2021–2022 window, SHS-inclusive.")
post `guideh' ("recalc_components") ("result_stock_elast_tot_full_shs") ("Stock elasticity on the total AGI base, full post horizon, SHS-inclusive.")
post `guideh' ("recalc_components") ("result_stock_elast_tot_ann_shs") ("Annualized stock elasticity on the total AGI base, SHS-inclusive.")
post `guideh' ("recalc_components") ("result_stock_elast_imp_common_shs") ("Stock elasticity on the impacted AGI base, 2021–2022 window, SHS-inclusive.")
post `guideh' ("recalc_components") ("result_stock_elast_imp_full_shs") ("Stock elasticity on the impacted AGI base, full post horizon, SHS-inclusive.")
post `guideh' ("recalc_components") ("result_stock_elast_imp_ann_shs") ("Annualized stock elasticity on the impacted AGI base, SHS-inclusive.")

post `guideh' ("preferred_net_stock_shs") ("table_data") ("Data source label for the preferred domestic net-migration specification.")
post `guideh' ("preferred_net_stock_shs") ("table_sample") ("Presentation-ready donor-pool label used in the SHS-inclusive comparison table.")
post `guideh' ("preferred_net_stock_shs") ("table_tau_pp") ("SDID coefficient on AGI net-migration rate, in percentage points (same as main sheet).")
post `guideh' ("preferred_net_stock_shs") ("table_dln_aftertax_shs") ("Change in log after-tax rate including Metro SHS used in the elasticity denominator.")
post `guideh' ("preferred_net_stock_shs") ("table_flow_semi_elast_shs") ("Kleven semi-elasticity with SHS-inclusive denominator.")
post `guideh' ("preferred_net_stock_shs") ("table_stock_common_shs") ("Total-AGI stock elasticity over 2021–2022, SHS-inclusive denominator.")
post `guideh' ("preferred_net_stock_shs") ("table_stock_full_shs") ("Total-AGI stock elasticity over full post horizon, SHS-inclusive denominator.")
post `guideh' ("preferred_net_stock_shs") ("table_stock_annualized_shs") ("Annualized total-AGI stock elasticity, SHS-inclusive denominator.")

post `guideh' ("run_parameters") ("avg_shs_rate") ("Average effective Metro SHS rate (1% flat) on impacted filers.")
post `guideh' ("run_parameters") ("avg_shs_rate_college") ("Average effective SHS rate on college-proxy filers within the impacted base.")
post `guideh' ("run_parameters") ("avg_total_rate_post_shs") ("Post-policy total tax rate including SHS for the main impacted filer base.")
post `guideh' ("run_parameters") ("avg_total_rate_pre_shs") ("Pre-policy total tax rate for the main impacted filer base (SHS was new in 2021).")
post `guideh' ("run_parameters") ("avg_total_rate_post_col_shs") ("Post-policy total tax rate including SHS for the college proxy subgroup.")
post `guideh' ("run_parameters") ("avg_total_rate_pre_col_shs") ("Pre-policy total tax rate for the college proxy subgroup.")
post `guideh' ("run_parameters") ("delta_ln_ntr_total_shs") ("Change in log net-of-tax rate for the main impacted filer base, SHS-inclusive.")
post `guideh' ("run_parameters") ("delta_ln_ntr_total_col_shs") ("Change in log net-of-tax rate for the college proxy subgroup, SHS-inclusive.")

postclose `guideh'
use `guide', clear
export excel using "${results}elasticities/tbl_elasticities.xlsx", ///
	sheet("variable_guide") firstrow(variables) sheetreplace
restore

dis "Main table:         ${results}elasticities/tbl_elasticities.tex"
dis "Appendix table:     ${results}elasticities/tbl_elasticities_inout.tex"
dis "Stock compare:      ${results}elasticities/tbl_elasticities_stock_compare.tex"
dis "SHS main table:     ${results}elasticities/tbl_elasticities_shs.tex"
dis "SHS appendix:       ${results}elasticities/tbl_elasticities_inout_shs.tex"
dis "SHS stock compare:  ${results}elasticities/tbl_elasticities_stock_compare_shs.tex"
dis "Excel:              ${results}elasticities/tbl_elasticities.xlsx"

********************************************************************************
** SECTION 3: Elasticity Distribution Figures
********************************************************************************

dis ""
dis "=============================================="
dis "Section 3: Elasticity distribution figures"
dis "=============================================="

** plotplainblind palette
local col_fill "86 180 233"		// sky — histogram fill
local col_irs  "213 94 0"		// vermillion — IRS preferred
local col_acs  "0 114 178"		// sea — ACS College preferred

** One non-faceted histogram per (migration × metric). Overlay vertical
** dashed lines mark the preferred specs, IRS vs. ACS by color. Overlay
** height is scaled to the histogram's max bar count so preferred-spec
** markers don't swamp the visible distribution.
**
** NOTE: Stata `syntax` option names cannot contain underscores; use
** colfill / colirs / colacs (not col_fill etc.).

capture program drop elast_hist_plot
program define elast_hist_plot
	syntax, VAR(varname numeric) XTITLE(string asis) FILE(string) ///
		COLFILL(string) COLIRS(string) COLACS(string) [HBINS(integer 25)]

	qui count if !missing(`var')
	if r(N) == 0 {
		dis as text "  No non-missing `var' values — skipping `file'."
		exit
	}

	** Compute per-bin counts so we can scale preferred-spec overlays to
	** the histogram's max bar height.
	qui summ `var' if !missing(`var')
	local xmin = r(min)
	local xmax = r(max)
	local bw   = (`xmax' - `xmin') / `hbins'
	tempvar bin binct
	if `bw' > 0 {
		qui gen `bin' = floor((`var' - `xmin') / `bw') if !missing(`var')
		qui replace `bin' = `hbins' - 1 if `bin' == `hbins'
		qui bysort `bin': gen `binct' = _N if !missing(`bin')
		qui summ `binct'
		local max_count = r(max)
	}
	else {
		** Degenerate: all values equal. One bin, one bar at full count.
		local max_count = r(N)
	}

	** Build overlay list for preferred specs, scaled to 0-to-max_count.
	local acc ""
	local irs_j = 0
	local acs_j = 0
	forvalues i = 1/`=_N' {
		if preferred[`i'] == 1 & !missing(`var'[`i']) {
			local v = `var'[`i']
			local dt = data_type[`i']
			if strpos("`dt'", "IRS") > 0 {
				local ++irs_j
				local acc `"`acc' (scatteri 0 `v' `max_count' `v', recast(line) lcolor("`colirs'") lwidth(medthick) lpattern(dash))"'
			}
			else if strpos("`dt'", "ACS College") > 0 {
				local ++acs_j
				local acc `"`acc' (scatteri 0 `v' `max_count' `v', recast(line) lcolor("`colacs'") lwidth(medthick) lpattern(dash))"'
			}
		}
	}

	** Build legend order conditionally — a layer index is valid only if
	** the corresponding category has at least one overlay line.
	local legorder `"1 "All Specifications""'
	if `irs_j' > 0 {
		local legorder `"`legorder' 2 "IRS Preferred""'
	}
	if `acs_j' > 0 {
		local legacs_idx = 2 + `irs_j'
		local legorder `"`legorder' `legacs_idx' "ACS College Preferred""'
	}

	twoway (histogram `var' if !missing(`var'), ///
				fcolor("`colfill'") lcolor(white) lwidth(thin) ///
				bin(`hbins') frequency) ///
	       `acc' ///
	    , xtitle(`"`xtitle'"') ///
	      ytitle("Number of Specifications") ///
	      graphregion(color(white)) ///
	      legend(order(`legorder') ///
	             ring(1) pos(6) rows(1) size(small) region(lcolor(white))) ///
	      ysize(3) xsize(8)

	graph export "`file'.pdf", replace
	graph export "`file'.png", as(png) width(2400) replace
end

** ---- Semi-elasticity β histograms (one per migration direction × denominator)
** The paper's two primary objects are β (semi-elasticity) and ε_stock,H
** (horizon-H stock elasticity). Flow ε is still computed and shown in the
** appendix tables but is no longer a figure.
foreach migr in "net" "in" "out" {

	if "`migr'" == "net" local migr_title "Net"
	if "`migr'" == "in"  local migr_title "In"
	if "`migr'" == "out" local migr_title "Out"

	preserve
	keep if migration == "`migr'"

	qui count
	local n_all = r(N)

	if `n_all' == 0 {
		dis "No AGI `migr'-migration specs found. Skipping."
		restore
		continue
	}

	qui count if preferred == 1
	local n_pref = r(N)

	dis ""
	dis "--- `migr_title'-migration: `n_all' AGI specs (`n_pref' highlighted) ---"
	dis as text "Kleven semi-elasticity beta distribution (`migr'):"
	summ beta_kleven, detail

	** Kleven semi-elasticity β — PFA-only total-rate denominator (primary).
	elast_hist_plot, var(beta_kleven) ///
		xtitle(`"{&beta} = ({&tau}/100) / {&Delta}ln(1{&minus}{&tau}{subscript:total})"') ///
		file("${results}elasticities/fig_elast_beta_`migr'") ///
		colfill("`col_fill'") colirs("`col_irs'") colacs("`col_acs'")

	** Kleven semi-elasticity β — PFA+SHS denominator (sensitivity).
	dis as text "Kleven semi-elasticity beta distribution (`migr'), +SHS:"
	summ beta_kleven_shs, detail
	elast_hist_plot, var(beta_kleven_shs) ///
		xtitle(`"{&beta} (PFA+SHS) = ({&tau}/100) / {&Delta}ln(1{&minus}{&tau}{subscript:total+SHS})"') ///
		file("${results}elasticities/fig_elast_beta_`migr'_shs") ///
		colfill("`col_fill'") colirs("`col_irs'") colacs("`col_acs'")

	restore
}

** ---- Horizon-H stock elasticity histograms (net migration only) ----
** Stock ε is built from cumulated net-flow effects; it is undefined for
** gross in/out rates. Report the common 2021-2022 window (matches the main
** table and gives clean IRS-ACS overlap).
preserve
keep if migration == "net"

qui count if !missing(stock_elast_total_common)
if r(N) > 0 {
	dis ""
	dis as text "Stock elasticity distribution (net, 2021-2022 window):"
	summ stock_elast_total_common if !missing(stock_elast_total_common), detail

	elast_hist_plot, var(stock_elast_total_common) ///
		xtitle(`"{&epsilon}{subscript:stock,H} = {&Delta}ln S{subscript:H} / {&Delta}ln(1{&minus}{&tau}{subscript:total})"') ///
		file("${results}elasticities/fig_elast_stock_net_common") ///
		colfill("`col_fill'") colirs("`col_irs'") colacs("`col_acs'")
}

qui count if !missing(stock_elast_total_common_shs)
if r(N) > 0 {
	dis as text "Stock elasticity distribution (net, 2021-2022 window, +SHS):"
	summ stock_elast_total_common_shs if !missing(stock_elast_total_common_shs), detail

	elast_hist_plot, var(stock_elast_total_common_shs) ///
		xtitle(`"{&epsilon}{subscript:stock,H} (PFA+SHS) = {&Delta}ln S{subscript:H} / {&Delta}ln(1{&minus}{&tau}{subscript:total+SHS})"') ///
		file("${results}elasticities/fig_elast_stock_net_common_shs") ///
		colfill("`col_fill'") colirs("`col_irs'") colacs("`col_acs'")
}
restore

** Overleaf copy — β for net migration is the appendix figure.
** Preserve the legacy filename `fig_elasticity_dist_net.pdf` on the Overleaf
** side so the manuscript's \includegraphics path does not need updating.
** New SHS and stock figures use their native filenames; wire them into the
** manuscript when ready.
if "${overleaf}" == "1" {
	foreach base in ///
		fig_elast_beta_net fig_elast_beta_in fig_elast_beta_out ///
		fig_elast_beta_net_shs fig_elast_beta_in_shs fig_elast_beta_out_shs ///
		fig_elast_stock_net_common fig_elast_stock_net_common_shs {
		capture confirm file "${results}elasticities/`base'.pdf"
		if _rc == 0 {
			copy "${results}elasticities/`base'.pdf" ///
				"${ol_fig}`base'.pdf", replace
		}
	}
	** Legacy rename: preserved for manuscript paths that haven't migrated.
	capture confirm file "${results}elasticities/fig_elast_beta_net.pdf"
	if _rc == 0 {
		copy "${results}elasticities/fig_elast_beta_net.pdf" ///
			"${ol_fig}fig_elasticity_dist_net.pdf", replace
	}
}

dis ""
dis "Figures exported to: ${results}elasticities/fig_elast_{beta,stock}_*.pdf"

********************************************************************************
** SECTION 4: Summary
********************************************************************************

dis ""
dis "=============================================="
dis "Section 4: Summary"
dis "=============================================="

dis ""
dis "=================================================================="
dis "ELASTICITY SUMMARY — AGI HIGHLIGHTED SPECIFICATIONS"
dis "=================================================================="
dis ""
dis "Average effective PFA rate (Δt):       " %8.4f delta_t " (" %5.3f delta_t * 100 "%)"
dis "Average effective SHS rate:            " %8.4f avg_shs_rate " (" %5.3f avg_shs_rate * 100 "%)"
dis "Average total tax rate (post-PFA):     " %8.4f avg_total_rate " (" %5.1f avg_total_rate * 100 "%)"
dis "Average total tax rate (pre-PFA):      " %8.4f avg_total_rate_pre " (" %5.1f avg_total_rate_pre * 100 "%)"
dis "Average total tax rate (post-PFA+SHS): " %8.4f avg_total_rate_with_shs ///
	" (" %5.1f avg_total_rate_with_shs * 100 "%)"
dis "Δln(1−τ) total NTR (Kleven):           " %8.6f delta_ln_ntr_total
dis "Δln(1−τ) total NTR incl. SHS:          " %8.6f delta_ln_ntr_total_shs
dis ""

dis "--- Kleven semi-elasticity β, flow elasticity, and stock elasticity (PRIMARY, PFA denominator) ---"
list data_type sample migration tau se ///
	beta_kleven flow_e ///
	stock_elast_total_common stock_elast_imp_ann ///
	if preferred == 1, sep(0) abbreviate(25)

dis ""
dis "--- SHS-inclusive sensitivity (denominator = PFA + SHS) ---"
list data_type sample migration ///
	beta_kleven_shs flow_e_shs ///
	stock_elast_total_common_shs stock_elast_imp_ann_shs ///
	if preferred == 1, sep(0) abbreviate(25)

dis ""
dis "=================================================================="

dis ""
dis "=============================================="
dis "02_elasticities.do complete."
dis "Output files:"
dis "  ${results}elasticities/tbl_elasticities.tex"
dis "  ${results}elasticities/tbl_elasticities_inout.tex"
dis "  ${results}elasticities/tbl_elasticities_stock_compare.tex"
dis "  ${results}elasticities/tbl_elasticities_shs.tex"
dis "  ${results}elasticities/tbl_elasticities_inout_shs.tex"
dis "  ${results}elasticities/tbl_elasticities_stock_compare_shs.tex"
dis "  ${results}elasticities/tbl_elasticities.xlsx"
dis "  ${results}elasticities/fig_elast_beta_{net,in,out}.{pdf,png}"
dis "  ${results}elasticities/fig_elast_beta_{net,in,out}_shs.{pdf,png}"
dis "  ${results}elasticities/fig_elast_stock_net_common.{pdf,png}"
dis "  ${results}elasticities/fig_elast_stock_net_common_shs.{pdf,png}"
dis "  ${results}elasticities/elasticity_results.dta"
dis "=============================================="

capture log close log_02elast
