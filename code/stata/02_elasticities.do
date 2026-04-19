/*******************************************************************************
File Name: 		02_elasticities.do
Creator: 		John Iselin
Date Update:	March 2026

Purpose: 	Calculate migration-rate semi-elasticities, gross-flow elasticities,
			and stock elasticities from SDID treatment effect estimates for the
			Preschool for All (PFA) income tax.

			Three denominator variants:
			  (1) Total NTR (Kleven et al. 2020): Δln(1−τ_total) where
			      τ_total = federal + state + FICA + PFA. PRIMARY.
			  (2) PFA-only NTR: Δln(1−τ_pfa). Diagnostic/appendix.
			  (3) Arc (midpoint): Δ(1−τ) / (1−τ̄). Supplementary.

			Formulas:
			  Flow semi-elasticity:  ε_semi = τ / (Δt × 100)
			  Flow elasticity:       ε_flow = -(τ / pre_mean) / Δln(1−t)
			  ATT-implied stock:     ε_att = -(τ / 100) × scale / Δln(1−t)
			  Cumulative stock:      ε_cum = -(Σh τ_h / 100) × scale / Δln(1−t)

			where τ = SDID ATT (pp of migration rate), Δt = avg effective PFA rate,
			and pre_mean = pre-treatment migration rate.

			Note: Main stock elasticities cumulate post-treatment event-study
			coefficients and rescale them to the impacted AGI base. The pooled
			ATT-based stock elasticity is retained as a diagnostic appendix object.
			FICA reflects the employee share only (TAXSIM v25).

Called by: 	00_multnomah.do
Requires:	${data}working/revenue_parameters.dta (from 02_revenue.do)
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

********************************************************************************
** SECTION 0: Setup & Parameters
********************************************************************************

** Start log file
capture log close log_02elast
log using "${logs}02_log_elasticities_${date}", name(log_02elast) replace text

project_set_seed, context("02_elasticities.do") offset(50)

** Create output directory
capture mkdir "${results}elasticities"

dis ""
dis "=============================================="
dis "Section 0: Load parameters"
dis "=============================================="

** Load revenue parameters exported by 02_revenue.do
mata: st_local("rp_exists", strofreal(fileexists("${data}working/revenue_parameters.dta")))
if `rp_exists' == 0 {
	dis as error "ERROR: revenue_parameters.dta not found."
	dis as error "       Run 02_revenue.do first."
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
restore

** Compute tax change terms
** PFA was new in 2021, so Δt = avg_mt_rate (change from 0)
scalar delta_t = avg_mt_rate
scalar delta_ln_ntr = ln(1 - avg_mt_rate)		// Δln(1−t) PFA only, negative for tax increase

** Total net-of-tax rate change (Kleven et al. 2020 standard)
** Uses full tax burden: federal + state + FICA + PFA
** Δln(1−τ_total) = ln((1−τ_post) / (1−τ_pre))
scalar delta_ln_ntr_total = ln((1 - avg_total_rate) / (1 - avg_total_rate_pre))
scalar delta_ln_ntr_total_college = ln((1 - avg_total_rate_college) ///
	/ (1 - avg_total_rate_pre_college))

** Arc (midpoint) net-of-tax rate change
scalar ntr_post = 1 - avg_total_rate
scalar ntr_pre  = 1 - avg_total_rate_pre
scalar ntr_mid  = (ntr_post + ntr_pre) / 2
scalar delta_ntr_arc = (ntr_post - ntr_pre) / ntr_mid	// negative for tax increase

dis ""
dis "Revenue parameters:"
dis "  avg_mt_rate       = " %10.6f avg_mt_rate
dis "  avg_state_rate    = " %10.6f avg_state_rate
dis "  avg_total_rate    = " %10.6f avg_total_rate " (post-PFA)"
dis "  avg_total_rate_pre= " %10.6f avg_total_rate_pre " (pre-PFA)"
dis "  impacted share    = " %10.6f impacted_agi_share
dis "  college share     = " %10.6f college_agi_share
dis "  Δt (PFA)          = " %10.6f delta_t
dis "  Δln(1−t) PFA only = " %10.6f delta_ln_ntr
dis "  Δln(1−t) total NTR= " %10.6f delta_ln_ntr_total
dis "  Arc Δ(1−t)/(1−t̄) = " %10.6f delta_ntr_arc

** Sanity checks
if delta_t < 0.001 | delta_t > 0.05 {
	dis as error "WARNING: avg_mt_rate = " %8.6f delta_t " — outside expected range [0.001, 0.05]"
	dis as error "         Elasticities may be very large or small. Verify 02_revenue.do output."
}
if avg_total_rate < 0.20 | avg_total_rate > 0.55 {
	dis as error "WARNING: avg_total_rate = " %8.6f avg_total_rate ///
		" — outside expected range [0.20, 0.55]"
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

** Flow semi-elasticity: ε_semi = τ / (Δt × 100)
** Units: pp change in migration rate per pp of tax rate
gen double flow_semi_e = tau / (delta_t * 100)
gen double flow_semi_se = se / (delta_t * 100)

** Flow elasticity (PFA-only NTR): ε_flow = −(τ / pre_mean) / Δln(1−t_pfa)
** Diagnostic/appendix only — uses PFA rate alone as denominator
** Note: undefined for net migration where pre_mean ≈ 0
gen double flow_e = -(tau / pre_mean) / delta_ln_ntr ///
	if inlist(migration, "in", "out") & !missing(pre_mean) & pre_mean != 0
gen double flow_se = (se / abs(pre_mean)) / abs(delta_ln_ntr) ///
	if inlist(migration, "in", "out") & !missing(pre_mean) & pre_mean != 0

** Flow elasticity (total NTR, Kleven et al. 2020 standard):
** ε_flow = −(τ / pre_mean) / Δln(1−τ_total)
** Uses total tax burden (federal + state + FICA + PFA) as denominator
** This is the PRIMARY flow elasticity — comparable to the migration-tax literature
gen double flow_e_total = -(tau / pre_mean) / delta_ln_ntr_total ///
	if inlist(migration, "in", "out") & !missing(pre_mean) & pre_mean != 0
gen double flow_se_total = (se / abs(pre_mean)) / abs(delta_ln_ntr_total) ///
	if inlist(migration, "in", "out") & !missing(pre_mean) & pre_mean != 0

** Flow elasticity (arc / midpoint formula):
** Uses average of pre- and post-reform NTR in denominator
gen double flow_e_arc = -(tau / pre_mean) / delta_ntr_arc ///
	if inlist(migration, "in", "out") & !missing(pre_mean) & pre_mean != 0
gen double flow_se_arc = (se / abs(pre_mean)) / abs(delta_ntr_arc) ///
	if inlist(migration, "in", "out") & !missing(pre_mean) & pre_mean != 0

** Stock elasticity: ε_stock = −(τ / 100) × scale / Δln(1−t)
** Sign convention: negative means tax base shrinks when tax rate rises
** Uses 100 (full stock) as denominator, not pre_mean → well-defined for all migration types
gen double scale_total = 1
replace scale_total = college_agi_share ///
	if inlist(data_type, "ACS College", "ACS College (Out-of-State)")

gen double scale_taxbase = scale_total / impacted_agi_share

** ATT-implied stock — PFA-only NTR (diagnostic/appendix)
gen double stock_e_att_total = -(tau / 100) * scale_total / delta_ln_ntr
gen double stock_se_att_total = (se / 100) * scale_total / abs(delta_ln_ntr)
gen double stock_e_att_taxbase = -(tau / 100) * scale_taxbase / delta_ln_ntr
gen double stock_se_att_taxbase = (se / 100) * scale_taxbase / abs(delta_ln_ntr)

** ATT-implied stock — total NTR (Kleven et al.)
gen double stock_e_att_taxbase_kleven = -(tau / 100) * scale_taxbase / delta_ln_ntr_total
gen double stock_se_att_taxbase_kleven = (se / 100) * scale_taxbase / abs(delta_ln_ntr_total)

** ATT-implied stock — arc
gen double stock_e_att_taxbase_arc = -(tau / 100) * scale_taxbase / delta_ntr_arc
gen double stock_se_att_taxbase_arc = (se / 100) * scale_taxbase / abs(delta_ntr_arc)

** CIs — iterate over est/se pairs (subinstr on "_e" is fragile for "flow_semi_e")
foreach pair in ///
		"flow_semi_e flow_semi_se"                             ///
		"flow_e flow_se"                                       ///
		"flow_e_total flow_se_total"                           ///
		"flow_e_arc flow_se_arc"                               ///
		"stock_e_att_taxbase stock_se_att_taxbase"             ///
		"stock_e_att_taxbase_kleven stock_se_att_taxbase_kleven" ///
		"stock_e_att_taxbase_arc stock_se_att_taxbase_arc" {
	local est : word 1 of `pair'
	local se  : word 2 of `pair'
	capture confirm variable `se'
	if _rc == 0 {
		gen double `est'_ci_lo = `est' - 1.96 * `se'
		gen double `est'_ci_hi = `est' + 1.96 * `se'
	}
}

tempfile event_cum

preserve
use `event_src', clear
keep if strpos(outcome, "agi_") > 0 & strpos(outcome, "_net_") > 0
drop if outstate == 1

gen byte post_common = inrange(event_year, 2021, 2022) & !missing(event_tau)
gen byte post_full = event_year >= 2021 & !missing(event_tau)
gen double tau_common = event_tau if post_common
gen double tau_full = event_tau if post_full

collapse (sum) cum_tau_common=tau_common cum_tau_full=tau_full ///
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

** ---- Cumulative stock elasticities (PFA-only NTR, diagnostic) ----
gen double stock_e_cum_common_total = -(cum_tau_common / 100) * scale_total / delta_ln_ntr ///
	if !missing(cum_tau_common)
gen double stock_e_cum_common_taxbase = -(cum_tau_common / 100) * scale_taxbase / delta_ln_ntr ///
	if !missing(cum_tau_common)
gen double stock_e_cum_full_total = -(cum_tau_full / 100) * scale_total / delta_ln_ntr ///
	if !missing(cum_tau_full)
gen double stock_e_cum_full_taxbase = -(cum_tau_full / 100) * scale_taxbase / delta_ln_ntr ///
	if !missing(cum_tau_full)
gen double stock_e_ann_full_total = stock_e_cum_full_total / H_full if H_full > 0
gen double stock_e_ann_full_taxbase = stock_e_cum_full_taxbase / H_full if H_full > 0

** ---- Cumulative stock elasticities (total NTR, Kleven et al. — PRIMARY) ----
gen double stock_e_cum_common_tb_kleven = -(cum_tau_common / 100) ///
	* scale_taxbase / delta_ln_ntr_total if !missing(cum_tau_common)
gen double stock_e_cum_full_tb_kleven = -(cum_tau_full / 100) ///
	* scale_taxbase / delta_ln_ntr_total if !missing(cum_tau_full)
gen double stock_e_ann_full_tb_kleven = stock_e_cum_full_tb_kleven ///
	/ H_full if H_full > 0

** ---- Cumulative stock elasticities (arc, supplementary) ----
gen double stock_e_cum_common_taxbase_arc = -(cum_tau_common / 100) ///
	* scale_taxbase / delta_ntr_arc if !missing(cum_tau_common)
gen double stock_e_cum_full_taxbase_arc = -(cum_tau_full / 100) ///
	* scale_taxbase / delta_ntr_arc if !missing(cum_tau_full)
gen double stock_e_ann_full_taxbase_arc = stock_e_cum_full_taxbase_arc ///
	/ H_full if H_full > 0

** ---- Variable labels for saved output ----
label var outcome_type     "Outcome family (n1 / n2 / agi)"
label var migration        "Migration direction (net / in / out)"
label var data_type        "Data source label (IRS, ACS All, ACS College, ...)"
label var period_type      "Sample period (16-22 / 16-24)"
label var outstate         "1 = out-of-state migration only"

label var scale_total      "Scale to total AGI (1 or college_agi_share)"
label var scale_taxbase    "Scale to impacted AGI (scale_total / impacted_agi_share)"

label var flow_semi_e      "Flow semi-elasticity: tau / (delta_t * 100)"
label var flow_semi_se     "SE of flow semi-elasticity"
label var flow_e           "Flow elasticity, PFA-only NTR (diagnostic)"
label var flow_se          "SE of flow elasticity (PFA-only NTR)"
label var flow_e_total     "Flow elasticity, total NTR (Kleven 2020; PRIMARY)"
label var flow_se_total    "SE of flow elasticity (total NTR)"
label var flow_e_arc       "Flow elasticity, arc / midpoint NTR"
label var flow_se_arc      "SE of flow elasticity (arc)"

label var stock_e_att_total           "ATT stock elasticity (PFA NTR, total-AGI base)"
label var stock_se_att_total          "SE of ATT stock elasticity (PFA NTR, total base)"
label var stock_e_att_taxbase         "ATT stock elasticity (PFA NTR, impacted-AGI base)"
label var stock_se_att_taxbase        "SE of ATT stock elasticity (PFA NTR, impacted base)"
label var stock_e_att_taxbase_kleven  "ATT stock elasticity (total NTR, impacted base; Kleven)"
label var stock_se_att_taxbase_kleven "SE of ATT stock elasticity (total NTR, impacted base)"
label var stock_e_att_taxbase_arc     "ATT stock elasticity (arc NTR, impacted base)"
label var stock_se_att_taxbase_arc    "SE of ATT stock elasticity (arc NTR, impacted base)"

capture label var cum_tau_common "Sum of event-study tau over 2021-2022"
capture label var cum_tau_full   "Sum of event-study tau over all post years"
capture label var H_common       "Number of post rows cumulated (2021-2022)"
capture label var H_full         "Number of post rows cumulated (all post years)"

capture label var stock_e_cum_common_total       "Cum. stock e (PFA NTR, total base, 2021-22)"
capture label var stock_e_cum_common_taxbase     "Cum. stock e (PFA NTR, impacted base, 2021-22)"
capture label var stock_e_cum_full_total         "Cum. stock e (PFA NTR, total base, all post)"
capture label var stock_e_cum_full_taxbase       "Cum. stock e (PFA NTR, impacted base, all post)"
capture label var stock_e_ann_full_total         "Annualized stock e (PFA NTR, total base)"
capture label var stock_e_ann_full_taxbase       "Annualized stock e (PFA NTR, impacted base)"
capture label var stock_e_cum_common_tb_kleven   "Cum. stock e (total NTR, impacted base, 2021-22; PRIMARY)"
capture label var stock_e_cum_full_tb_kleven     "Cum. stock e (total NTR, impacted base, all post; PRIMARY)"
capture label var stock_e_ann_full_tb_kleven     "Annualized stock e (total NTR, impacted base; PRIMARY)"
capture label var stock_e_cum_common_taxbase_arc "Cum. stock e (arc NTR, impacted base, 2021-22)"
capture label var stock_e_cum_full_taxbase_arc   "Cum. stock e (arc NTR, impacted base, all post)"
capture label var stock_e_ann_full_taxbase_arc   "Annualized stock e (arc NTR, impacted base)"

** Save full dataset (all AGI specs with elasticities)
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
	& !missing(stock_e_cum_common_taxbase)
if r(N) == 0 {
	dis as error "ERROR: No highlighted domestic net-migration specs have cumulative stock elasticities."
	dis as error "       Regenerate sdid_event_results.dta from 02_sdid_analysis.do."
	log close log_02elast
	error 2001
}

** Display highlighted AGI specs
dis ""
dis "Highlighted AGI specifications for elasticity table:"
list data_type sample migration tau se pre_mean ///
	flow_semi_e stock_e_cum_common_tb_kleven stock_e_ann_full_tb_kleven ///
	if preferred == 1, sep(0) abbreviate(20)

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

** Formatted strings
gen str12 tau_str = string(tau, "%9.3f")
gen str12 se_str = "(" + string(se, "%9.3f") + ")"
gen str12 fsemi_str = string(flow_semi_e, "%9.3f")
gen str12 fsemi_se_str = "(" + string(flow_semi_se, "%9.3f") + ")"
gen str12 stock_common_kleven_str = string(stock_e_cum_common_tb_kleven, "%9.3f")
gen str12 stock_ann_kleven_str = string(stock_e_ann_full_tb_kleven, "%9.3f")

** Write LaTeX table
tempname fh
file open `fh' using "${results}elasticities/tbl_elasticities.tex", write replace

elast_tex_open, handle(`fh') ///
	cap("Highlighted AGI Net-Migration Elasticities") ///
	lbl("tab:elasticities") cols("ll cccc")
file write `fh' "Data & Sample & $\hat{\tau}$ (pp) & Semi-$\varepsilon$ & Cum.\ Stock $\varepsilon$ & Ann.\ Stock $\varepsilon$ \\" _n
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
	local fs = fsemi_str[`i']
	local fs_se = fsemi_se_str[`i']
	local stock_common = stock_common_kleven_str[`i']
	local stock_ann = stock_ann_kleven_str[`i']

	** Add spacing between data-type groups
	if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
		file write `fh' "\addlinespace" _n
	}
	local prev_dt "`dt'"

	** Point estimates row
	file write `fh' "`dt' & `smp' & `t_val' & `fs' & `stock_common' & `stock_ann' \\" _n
	file write `fh' " & & `se_val' & `fs_se' & & \\" _n
}

elast_tex_notes_open, handle(`fh')
file write `fh' "Semi-elasticity: pp change in AGI net migration rate per pp of PFA tax rate. " _n
file write `fh' "Cumulative and annualized stock elasticities use the total net-of-tax rate " _n
file write `fh' "(federal income + Oregon state income + FICA employee share + PFA) " _n
file write `fh' "as the denominator, following Kleven et al.\ (2020). " _n
file write `fh' "Cumulative stock elasticity sums post-treatment AGI net-migration event-study " _n
file write `fh' "coefficients over 2021--2022 and rescales to the impacted AGI base. " _n
file write `fh' "Annualized stock elasticity uses all available post-treatment years and divides by the number of post years. " _n
file write `fh' "Negative values indicate a smaller AGI base when the tax rate rises. " _n
file write `fh' "Average effective PFA rate: `pfa_pct'\%; average total tax rate on impacted filers: `total_pct'\%. " _n
file write `fh' "FICA reflects the employee share only. " _n
file write `fh' "Flow elasticities for gross migration are in Appendix Table~\ref{tab:elasticities_inout}. " _n
file write `fh' "Standard errors in parentheses are reported for $\hat{\tau}$ and the semi-elasticity only; " _n
file write `fh' "the current pipeline does not export joint event-study covariance matrices for cumulative stock elasticities." _n
elast_tex_close, handle(`fh')

file close `fh'
restore

** =========================================================================
** (b) Appendix table: AGI out- and in-migration, semi-ε and full ε
** =========================================================================

preserve
keep if preferred == 1 & inlist(migration, "out", "in") & outstate == 0

** Formatted strings
gen str12 tau_str = string(tau, "%9.3f")
gen str12 se_str = "(" + string(se, "%9.3f") + ")"
gen str12 fsemi_str = string(flow_semi_e, "%9.3f")
gen str12 fsemi_se_str = "(" + string(flow_semi_se, "%9.3f") + ")"
gen str12 fe_total_str = string(flow_e_total, "%9.3f") if !missing(flow_e_total)
gen str12 fe_total_se_str = "(" + string(flow_se_total, "%9.3f") + ")" if !missing(flow_se_total)
gen str12 fe_str = string(flow_e, "%9.3f") if !missing(flow_e)
gen str12 fe_se_str = "(" + string(flow_se, "%9.3f") + ")" if !missing(flow_se)
gen str12 ste_kleven_str = string(stock_e_att_taxbase_kleven, "%9.3f")
gen str12 ste_kleven_se_str = "(" + string(stock_se_att_taxbase_kleven, "%9.3f") + ")"

** Migration label
gen str20 migr_label = ""
replace migr_label = "In" if migration == "in"
replace migr_label = "Out" if migration == "out"

tempname fh
file open `fh' using "${results}elasticities/tbl_elasticities_inout.tex", write replace

elast_tex_open, handle(`fh') ///
	cap("Highlighted Gross AGI Migration Elasticities") ///
	lbl("tab:elasticities_inout") cols("lll ccccc") ///
	fontsize("footnotesize")
file write `fh' "Data & Sample & Dir.\ & $\hat{\tau}$ (pp) & Semi-$\varepsilon$ & Flow $\varepsilon$ & Flow $\varepsilon$ & ATT Stock $\varepsilon$ \\" _n
file write `fh' " & & & & & (Total NTR) & (PFA Only) & (Total NTR) \\" _n
file write `fh' "\midrule" _n

** Panel A header
file write `fh' "\addlinespace" _n
file write `fh' "\multicolumn{8}{l}{\textit{Panel A: Out-Migration}} \\" _n
file write `fh' "\addlinespace" _n

sort data_type sample migration
local N = _N
local prev_dt ""

** Out-migration rows
forvalues i = 1/`N' {
	if migration[`i'] != "out" continue
	local dt = data_type[`i']
	local smp = subinstr(sample[`i'], "sample_", "", .)
	local smp = proper("`smp'")
	local mg = migr_label[`i']
	local t_val = tau_str[`i']
	local se_val = se_str[`i']
	local fs = fsemi_str[`i']
	local fs_se = fsemi_se_str[`i']
	local fet = fe_total_str[`i']
	local fet_se = fe_total_se_str[`i']
	local fe = fe_str[`i']
	local fe_se = fe_se_str[`i']
	local ste = ste_kleven_str[`i']
	local ste_se = ste_kleven_se_str[`i']

	if "`fet'" == "" local fet "--"
	if "`fet_se'" == "" local fet_se ""
	if "`fe'" == "" local fe "--"
	if "`fe_se'" == "" local fe_se ""

	if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
		file write `fh' "\addlinespace" _n
	}
	local prev_dt "`dt'"

	file write `fh' "`dt' & `smp' & `mg' & `t_val' & `fs' & `fet' & `fe' & `ste' \\" _n
	file write `fh' " & & & `se_val' & `fs_se' & `fet_se' & `fe_se' & `ste_se' \\" _n
}

** Panel B header
file write `fh' "\addlinespace[0.75em]" _n
file write `fh' "\midrule" _n
file write `fh' "\addlinespace" _n
file write `fh' "\multicolumn{8}{l}{\textit{Panel B: In-Migration}} \\" _n
file write `fh' "\addlinespace" _n

local prev_dt ""

** In-migration rows
forvalues i = 1/`N' {
	if migration[`i'] != "in" continue
	local dt = data_type[`i']
	local smp = subinstr(sample[`i'], "sample_", "", .)
	local smp = proper("`smp'")
	local mg = migr_label[`i']
	local t_val = tau_str[`i']
	local se_val = se_str[`i']
	local fs = fsemi_str[`i']
	local fs_se = fsemi_se_str[`i']
	local fet = fe_total_str[`i']
	local fet_se = fe_total_se_str[`i']
	local fe = fe_str[`i']
	local fe_se = fe_se_str[`i']
	local ste = ste_kleven_str[`i']
	local ste_se = ste_kleven_se_str[`i']

	if "`fet'" == "" local fet "--"
	if "`fet_se'" == "" local fet_se ""
	if "`fe'" == "" local fe "--"
	if "`fe_se'" == "" local fe_se ""

	if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
		file write `fh' "\addlinespace" _n
	}
	local prev_dt "`dt'"

	file write `fh' "`dt' & `smp' & `mg' & `t_val' & `fs' & `fet' & `fe' & `ste' \\" _n
	file write `fh' " & & & `se_val' & `fs_se' & `fet_se' & `fe_se' & `ste_se' \\" _n
}

elast_tex_notes_open, handle(`fh')
file write `fh' "Semi-elasticity: pp change in AGI migration rate per pp of PFA tax rate. " _n
file write `fh' "Flow $\varepsilon$ (Total NTR) uses the total net-of-tax rate " _n
file write `fh' "(federal + state + FICA + PFA) as denominator, following Kleven et al.\ (2020). " _n
file write `fh' "Flow $\varepsilon$ (PFA Only) uses only the PFA rate (shown for comparison). " _n
file write `fh' "ATT-implied stock elasticity rescales the pooled SDID ATT to the impacted AGI base " _n
file write `fh' "using the total NTR denominator; shown as a diagnostic. " _n
file write `fh' "FICA reflects the employee share only. " _n
file write `fh' "Negative values indicate a smaller AGI base when the tax rate rises. " _n
file write `fh' "Average effective PFA rate: `pfa_pct'\%; total rate on impacted filers: `total_pct'\%. " _n
file write `fh' "Standard errors in parentheses, derived from SDID bootstrap SEs." _n
elast_tex_close, handle(`fh')

file close `fh'
restore

** ---- Copy to Overleaf ----
if ${overleaf} == 1 {
	copy "${results}elasticities/tbl_elasticities.tex" ///
		"${ol_tab}tbl_elasticities.tex", replace
	copy "${results}elasticities/tbl_elasticities_inout.tex" ///
		"${ol_tab}tbl_elasticities_inout.tex", replace
}

** Export Excel (all AGI specs)
export excel using "${results}elasticities/tbl_elasticities.xlsx", ///
	firstrow(variables) replace

dis "Main table:     ${results}elasticities/tbl_elasticities.tex"
dis "Appendix table: ${results}elasticities/tbl_elasticities_inout.tex"
dis "Excel:          ${results}elasticities/tbl_elasticities.xlsx"

********************************************************************************
** SECTION 3: Elasticity Distribution Figures
********************************************************************************

dis ""
dis "=============================================="
dis "Section 3: Elasticity distribution figures"
dis "=============================================="

** plotplainblind palette
local col_fill    "86 180 233"		// sky — histogram fill
local col_irs     "213 94 0"		// vermillion — IRS preferred
local col_acs     "0 114 178"		// sea — ACS College preferred

** Histogram bin count, used by all three panels
local hbins 20

** ---- Loop over migration directions ----
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
	dis as text "Semi-elasticity distribution (`migr'):"
	summ flow_semi_e, detail

	** Build individual vertical lines for each highlighted spec (panel a)
	** IRS in vermillion, ACS College in sea blue
	local pref_semi_overlays ""
	local irs_j = 0
	local acs_j = 0
	forvalues i = 1/`=_N' {
		if preferred[`i'] == 1 {
			local v = flow_semi_e[`i']
			local dt = data_type[`i']
			if strpos("`dt'", "IRS") > 0 {
				local ++irs_j
				local pref_semi_overlays `"`pref_semi_overlays' (scatteri 0 `v' 1 `v', recast(line) lcolor("`col_irs'") lwidth(medthick) lpattern(dash))"'
			}
			else {
				local ++acs_j
				local pref_semi_overlays `"`pref_semi_overlays' (scatteri 0 `v' 1 `v', recast(line) lcolor("`col_acs'") lwidth(medthick) lpattern(dash))"'
			}
		}
	}
	local leg_irs = 2
	local leg_acs = 2 + `irs_j'

	** ---- Panel (a): Flow semi-elasticity ----
	twoway (histogram flow_semi_e, 									///
			fcolor("`col_fill'") lcolor(white) lwidth(thin) 		///
			bin(`hbins') fraction) 									///
		`pref_semi_overlays',										///
		graphregion(color(white)) 									///
		xtitle("Semi-{&epsilon} (pp migration rate per pp tax rate)") ///
		ytitle("Fraction of Specifications") 						///
		legend(order(`leg_irs' "IRS Benchmarks" 					///
			`leg_acs' "ACS College Benchmarks") 					///
			ring(1) pos(6) rows(1) size(small)) 					///
		name(panel_a, replace) nodraw

	** ---- Panel (b): Flow elasticity ----
	** Drop specs with missing flow_e (zero pre_mean)
	qui count if !missing(flow_e)
	local n_fe = r(N)

	if `n_fe' > 0 {
		qui count if preferred == 1 & !missing(flow_e)
		local n_pref_fe = r(N)

		dis as text "Flow-elasticity (PFA NTR) distribution (`migr'):"
		summ flow_e if !missing(flow_e), detail

		** Build individual vertical lines for each highlighted spec (panel b)
		local pref_fe_overlays ""
		local irs_j2 = 0
		local acs_j2 = 0
		forvalues i = 1/`=_N' {
			if preferred[`i'] == 1 & !missing(flow_e[`i']) {
				local v = flow_e[`i']
				local dt = data_type[`i']
				if strpos("`dt'", "IRS") > 0 {
					local ++irs_j2
					local pref_fe_overlays `"`pref_fe_overlays' (scatteri 0 `v' 1 `v', recast(line) lcolor("`col_irs'") lwidth(medthick) lpattern(dash))"'
				}
				else {
					local ++acs_j2
					local pref_fe_overlays `"`pref_fe_overlays' (scatteri 0 `v' 1 `v', recast(line) lcolor("`col_acs'") lwidth(medthick) lpattern(dash))"'
				}
			}
		}
		local leg_irs2 = 2
		local leg_acs2 = 2 + `irs_j2'

		twoway (histogram flow_e if !missing(flow_e), 					///
				fcolor("`col_fill'") lcolor(white) lwidth(thin)			///
				bin(`hbins') fraction) 									///
			`pref_fe_overlays',											///
			graphregion(color(white)) 									///
			xtitle("{&epsilon} (% {&Delta} migration rate / % {&Delta} net-of-tax rate)") ///
			ytitle("Fraction of Specifications") 						///
			legend(order(`leg_irs2' "IRS Benchmarks" 				///
				`leg_acs2' "ACS College Benchmarks") 				///
				ring(1) pos(6) rows(1) size(small)) 				///
			name(panel_b, replace) nodraw

		** ---- Panel (c): Flow elasticity (Total NTR, Kleven et al.) ----
		qui count if !missing(flow_e_total)
		local n_fet = r(N)

		if `n_fet' > 0 {
			dis as text "Flow-elasticity (total NTR) distribution (`migr'):"
			summ flow_e_total if !missing(flow_e_total), detail

			** Build vertical lines for total-NTR panel
			local pref_fet_overlays ""
			local irs_j3 = 0
			local acs_j3 = 0
			forvalues i = 1/`=_N' {
				if preferred[`i'] == 1 & !missing(flow_e_total[`i']) {
					local v = flow_e_total[`i']
					local dt = data_type[`i']
					if strpos("`dt'", "IRS") > 0 {
						local ++irs_j3
						local pref_fet_overlays `"`pref_fet_overlays' (scatteri 0 `v' 1 `v', recast(line) lcolor("`col_irs'") lwidth(medthick) lpattern(dash))"'
					}
					else {
						local ++acs_j3
						local pref_fet_overlays `"`pref_fet_overlays' (scatteri 0 `v' 1 `v', recast(line) lcolor("`col_acs'") lwidth(medthick) lpattern(dash))"'
					}
				}
			}
			local leg_irs3 = 2
			local leg_acs3 = 2 + `irs_j3'

			twoway (histogram flow_e_total if !missing(flow_e_total), 	///
					fcolor("`col_fill'") lcolor(white) lwidth(thin)		///
					bin(`hbins') fraction) 								///
				`pref_fet_overlays',									///
				graphregion(color(white)) 								///
				xtitle("{&epsilon} (% {&Delta} migration rate / % {&Delta} total net-of-tax rate)") ///
				ytitle("Fraction of Specifications") 					///
				legend(order(`leg_irs3' "IRS Benchmarks" 			///
					`leg_acs3' "ACS College Benchmarks") 			///
					ring(1) pos(6) rows(1) size(small)) 			///
				name(panel_c, replace) nodraw

			** ---- Combine three panels (stacked vertically) ----
			graph combine panel_a panel_b panel_c, 						///
				rows(3) graphregion(color(white))

			graph export "${results}elasticities/fig_elasticity_dist_`migr'.pdf", replace
			graph export "${results}elasticities/fig_elasticity_dist_`migr'.png", ///
				as(png) width(2400) replace

			** Overleaf copy — net migration only (appendix figure)
			if "`migr'" == "net" & ${overleaf} == 1 {
				graph export "${ol_fig}fig_elasticity_dist_net.pdf", replace
			}

			graph drop panel_a panel_b panel_c
		}
		else {
			** Only two panels (no total-NTR data)
			graph combine panel_a panel_b, 								///
				rows(2) graphregion(color(white))

			graph export "${results}elasticities/fig_elasticity_dist_`migr'.pdf", replace
			graph export "${results}elasticities/fig_elasticity_dist_`migr'.png", ///
				as(png) width(2400) replace

			if "`migr'" == "net" & ${overleaf} == 1 {
				graph export "${ol_fig}fig_elasticity_dist_net.pdf", replace
			}

			graph drop panel_a panel_b
		}
	}
	else {
		dis "  No valid flow elasticities for `migr'-migration. Skipping figure."
		graph drop panel_a
	}

	restore
}

dis ""
dis "Figures exported to: ${results}elasticities/fig_elasticity_dist_*.pdf"

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
dis "Average effective PFA rate (Δt):     " %8.4f delta_t " (" %5.3f delta_t * 100 "%)"
dis "Average total tax rate (post-PFA):   " %8.4f avg_total_rate " (" %5.1f avg_total_rate * 100 "%)"
dis "Average total tax rate (pre-PFA):    " %8.4f avg_total_rate_pre " (" %5.1f avg_total_rate_pre * 100 "%)"
dis "Δln(1−t) PFA only:                   " %8.6f delta_ln_ntr
dis "Δln(1−t) total NTR (Kleven):         " %8.6f delta_ln_ntr_total
dis "Arc Δ(1−t)/(1−t̄):                   " %8.6f delta_ntr_arc
dis ""

dis "--- Total-NTR (Kleven et al.) elasticities (PRIMARY) ---"
list data_type sample migration tau se ///
	flow_semi_e stock_e_cum_common_tb_kleven ///
	stock_e_ann_full_tb_kleven ///
	if preferred == 1, sep(0) abbreviate(25)

dis ""
dis "--- PFA-only NTR elasticities (diagnostic) ---"
list data_type sample migration ///
	flow_e stock_e_cum_common_taxbase ///
	stock_e_ann_full_taxbase stock_e_att_taxbase ///
	if preferred == 1, sep(0) abbreviate(25)

dis ""
dis "=================================================================="

dis ""
dis "=============================================="
dis "02_elasticities.do complete."
dis "Output files:"
dis "  ${results}elasticities/tbl_elasticities.tex"
dis "  ${results}elasticities/tbl_elasticities_inout.tex"
dis "  ${results}elasticities/tbl_elasticities.xlsx"
dis "  ${results}elasticities/fig_elasticity_dist_net.pdf"
dis "  ${results}elasticities/fig_elasticity_dist_in.pdf"
dis "  ${results}elasticities/fig_elasticity_dist_out.pdf"
dis "  ${results}elasticities/elasticity_results.dta"
dis "=============================================="

capture log close log_02elast
