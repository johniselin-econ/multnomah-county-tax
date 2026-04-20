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
			  Horizon-H cumulative flow impact (diagnostic / appendix):
			      cum_flow_impact_H = -(Σh τ_h / 100) × scale_taxbase
			                          / Δln(1−τ_total)

			The Kleven eq. 5 steady-state stock elasticity
			ε_stock = β · (T+1)/2 requires a demographic lifespan T that we
			do not estimate. It is NOT computed here. `cum_flow_impact_H` is a
			directly-estimable horizon-H object, not a steady-state stock ε.

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

** Drop any stale revenue-parameter scalars from a prior run. Stata `scalar`
** is a global namespace that survives `clear`, so a stale value from an
** earlier (differently-named) run could silently shadow the fresh load.
foreach s in avg_mt_rate avg_state_rate baseline_pfa_revenue total_agi_2022  ///
	agi_total agi_impacted impacted_agi_share agi_college college_agi_share   ///
	agi_college_impacted college_impacted_agi_share                           ///
	avg_mt_rate_college_impacted avg_total_rate avg_total_rate_pre            ///
	avg_total_rate_college avg_total_rate_pre_college                         ///
	delta_t delta_ln_ntr_total delta_ln_ntr_total_college {
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
** PFA was new in 2021, so Δt = avg_mt_rate (retained for reporting in table notes)
scalar delta_t = avg_mt_rate

** Total net-of-tax rate change (Kleven et al. 2024 denominator)
** Uses full tax burden: federal + state + FICA employee share + PFA
** Δln(1−τ_total) = ln((1−τ_post) / (1−τ_pre)); negative for a tax increase
scalar delta_ln_ntr_total = ln((1 - avg_total_rate) / (1 - avg_total_rate_pre))
scalar delta_ln_ntr_total_college = ln((1 - avg_total_rate_college) ///
	/ (1 - avg_total_rate_pre_college))

dis ""
dis "Revenue parameters:"
dis "  avg_mt_rate       = " %10.6f avg_mt_rate
dis "  avg_state_rate    = " %10.6f avg_state_rate
dis "  avg_total_rate    = " %10.6f avg_total_rate " (post-PFA)"
dis "  avg_total_rate_pre= " %10.6f avg_total_rate_pre " (pre-PFA)"
dis "  impacted share    = " %10.6f impacted_agi_share
dis "  college share     = " %10.6f college_agi_share
dis "  Δt (PFA)          = " %10.6f delta_t
dis "  Δln(1−τ) total NTR= " %10.6f delta_ln_ntr_total

** Sanity checks (hard errors — a scale bug in 02_revenue.do should halt
** the pipeline, not print a warning and produce absurd elasticities).
if delta_t < 0.001 | delta_t > 0.05 {
	dis as error "ERROR: avg_mt_rate = " %8.6f delta_t " outside [0.001, 0.05]"
	dis as error "       Inspect TAXSIM v25 inputs in 02_revenue.do and verify"
	dis as error "       avg_mt_rate is on the [0,1] scale (not [0,100])."
	log close log_02elast
	error 459
}
if avg_total_rate < 0.20 | avg_total_rate > 0.55 {
	dis as error "ERROR: avg_total_rate = " %8.6f avg_total_rate ///
		" outside [0.20, 0.55]"
	dis as error "       Inspect 02_revenue.do tax-total aggregation."
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

** Flow elasticity (Kleven total NTR):
**   ε_flow = −(τ / pre_mean) / Δln(1−τ_total)
** Defined for gross migration only — undefined for net when pre_mean ≈ 0.
gen double flow_e  = -(tau / pre_mean) / delta_ln_ntr_total ///
	if inlist(migration, "in", "out") & !missing(pre_mean) & pre_mean != 0
gen double flow_se = (se / abs(pre_mean)) / abs(delta_ln_ntr_total) ///
	if inlist(migration, "in", "out") & !missing(pre_mean) & pre_mean != 0

** Scales for the horizon-H cumulative flow impact (below).
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

** CIs for the two estimates that carry a SE from SDID bootstrap.
foreach pair in ///
		"beta_kleven beta_se_kleven" ///
		"flow_e flow_se" {
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

gen byte post_common = inrange(event_year, `pfa_start_year', `common_end_year') ///
	& !missing(event_tau)
gen byte post_full = event_year >= `pfa_start_year' & !missing(event_tau)
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

** ---- Horizon-H cumulative flow impact ----
** Interpretation: cumulative fraction of the impacted AGI base lost through
** post-treatment year H (net of in-migration response), per log-point change
** in total NTR. Directly estimable from the data.
**
** This is NOT the Kleven et al. (2024) steady-state stock elasticity
** ε_stock = β · (T+1)/2, which requires a demographic lifespan T
** (average years an individual stays above the PFA threshold). We do not
** estimate T, so we do not compute ε_stock; cum_flow_impact_H converges
** to ε_stock only in the limit H → T.
gen double cum_flow_impact_common = -(cum_tau_common / 100) ///
	* scale_taxbase / delta_ln_ntr_total if !missing(cum_tau_common)
gen double cum_flow_impact_full   = -(cum_tau_full / 100) ///
	* scale_taxbase / delta_ln_ntr_total if !missing(cum_tau_full)
gen double cum_flow_impact_annual = cum_flow_impact_full / H_full if H_full > 0

** ---- Variable labels for saved output ----
label var outcome_type     "Outcome family (n1 / n2 / agi)"
label var migration        "Migration direction (net / in / out)"
label var data_type        "Data source label (IRS, ACS All, ACS College, ...)"
label var period_type      "Sample period (16-22 / 16-24)"
label var outstate         "1 = out-of-state migration only"

label var scale_total      "Scale to total AGI (1 or college_agi_share)"
label var scale_taxbase    "Scale to impacted AGI (scale_total / impacted_agi_share)"

label var beta_kleven      "Kleven semi-elasticity: (tau/100) / Δln(1-τ_total)"
label var beta_se_kleven   "SE of Kleven semi-elasticity"
label var flow_e           "Flow elasticity: -(tau/pre_mean) / Δln(1-τ_total)"
label var flow_se          "SE of flow elasticity"

capture label var cum_tau_common "Sum of event-study tau over 2021-2022"
capture label var cum_tau_full   "Sum of event-study tau over all post years"
capture label var H_common       "Number of post rows cumulated (2021-2022)"
capture label var H_full         "Number of post rows cumulated (all post years)"

capture label var cum_flow_impact_common "Cum. flow impact (impacted base, 2021-22, horizon-H)"
capture label var cum_flow_impact_full   "Cum. flow impact (impacted base, all post, horizon-H)"
capture label var cum_flow_impact_annual "Annualized cum. flow impact (impacted base, all post)"

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
	& !missing(cum_flow_impact_common)
if r(N) == 0 {
	dis as error "ERROR: No highlighted domestic net-migration specs have cumulative flow impact."
	dis as error "       Regenerate sdid_event_results.dta from 02_sdid_analysis.do."
	log close log_02elast
	error 2001
}

** Display highlighted AGI specs
dis ""
dis "Highlighted AGI specifications for elasticity table:"
list data_type sample migration tau se pre_mean ///
	beta_kleven cum_flow_impact_common cum_flow_impact_annual ///
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

** Formatted strings
gen str20 tau_str = string(tau, "%9.3f")
gen str20 se_str = "(" + string(se, "%9.3f") + ")"
gen str20 beta_str = string(beta_kleven, "%9.3f")
gen str20 beta_se_str = "(" + string(beta_se_kleven, "%9.3f") + ")"
gen str20 cfi_common_str = string(cum_flow_impact_common, "%9.3f")

** Write LaTeX table
tempname fh
file open `fh' using "${results}elasticities/tbl_elasticities.tex", write replace

elast_tex_open, handle(`fh') ///
	cap("Highlighted AGI Net-Migration Elasticities (Kleven 2024 Framework)") ///
	lbl("tab:elasticities") cols("ll ccc")
file write `fh' "Data & Sample & $\hat{\tau}$ (pp) & Semi-$\varepsilon$ $\beta$ & Cum.\ Flow Impact \\" _n
file write `fh' " & & & (Kleven) & (2021--2022) \\" _n
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
	local cfi = cfi_common_str[`i']

	** Add spacing between data-type groups
	if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
		file write `fh' "\addlinespace" _n
	}
	local prev_dt "`dt'"

	** Point estimates row
	file write `fh' "`dt' & `smp' & `t_val' & `b' & `cfi' \\" _n
	file write `fh' " & & `se_val' & `b_se' & \\" _n
}

elast_tex_notes_open, handle(`fh')
file write `fh' "Semi-elasticity $\beta$ follows Kleven et al.\ (2024, NBER WP 32153): " _n
file write `fh' "$\beta = (\hat{\tau}/100) / \Delta\ln(1-\tau_\text{total})$, where $\tau_\text{total}$ is the combined " _n
file write `fh' "federal income + Oregon state income + FICA employee share + PFA rate on impacted filers. " _n
file write `fh' "A negative $\beta$ for out-migration (or positive for in-migration) indicates more migration when the net-of-tax rate falls. " _n
file write `fh' "Kleven's informal reading of $\beta$ as ``pp change in the migration rate per pp change in the tax rate'' holds only when $\tau$ is small: " _n
file write `fh' "formally $\beta \approx -(1 - \bar{\tau}_\text{total}) \cdot (\Delta\text{mig}_\text{pp}/\Delta\tau_\text{pp})$, so at the Multnomah total rate of `total_pct'\% the magnitude is roughly $1/(1-\bar{\tau}_\text{total}) \approx " _n
file write `fh' "1.5$-$1.7$\times$ the naive pp-per-pp reading. " _n
file write `fh' "Cumulative flow impact is $-(\sum_{h=2021}^{2022} \hat{\tau}_h / 100) \cdot s_\text{scale} / \Delta\ln(1-\tau_\text{total})$ with $s_\text{scale} = 1/s_\text{impacted}$ for IRS and ACS All and $s_\text{scale} = s_\text{college}/s_\text{impacted}$ for ACS College: " _n
file write `fh' "cumulative fraction of the impacted AGI base lost through post-treatment year $H$ per log-point of NTR. " _n
file write `fh' "This is a horizon-$H$ object, \emph{not} a steady-state stock elasticity. " _n
file write `fh' "The Kleven steady-state stock elasticity $\varepsilon_\text{stock} = \beta \cdot (T+1)/2$ " _n
file write `fh' "requires the demographic lifespan $T$ of the impacted population, which we do not estimate. " _n
file write `fh' "Positive values indicate a shrinking AGI base when the tax rate rises. " _n
file write `fh' "Average effective PFA rate: `pfa_pct'\%; average total tax rate on impacted filers: `total_pct'\%. " _n
file write `fh' "FICA reflects the employee share only. " _n
file write `fh' "Flow elasticities for gross migration are in Appendix Table~\ref{tab:elasticities_inout}. " _n
file write `fh' "Standard errors in parentheses are reported for $\hat{\tau}$ and $\beta$ only; " _n
file write `fh' "the current pipeline does not export joint event-study covariance matrices for the cumulative flow impact." _n
elast_tex_close, handle(`fh')

file close `fh'
restore

** =========================================================================
** (b) Appendix table: AGI out- and in-migration, Kleven β and flow ε
** =========================================================================

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

** Panel A header
file write `fh' "\addlinespace" _n
file write `fh' "\multicolumn{6}{l}{\textit{Panel A: Out-Migration}} \\" _n
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
	local b = beta_str[`i']
	local b_se = beta_se_str[`i']
	local fe = fe_str[`i']
	local fe_se = fe_se_str[`i']

	if "`fe'" == "" local fe "--"
	if "`fe_se'" == "" local fe_se ""

	if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
		file write `fh' "\addlinespace" _n
	}
	local prev_dt "`dt'"

	file write `fh' "`dt' & `smp' & `mg' & `t_val' & `b' & `fe' \\" _n
	file write `fh' " & & & `se_val' & `b_se' & `fe_se' \\" _n
}

** Panel B header
file write `fh' "\addlinespace[0.75em]" _n
file write `fh' "\midrule" _n
file write `fh' "\addlinespace" _n
file write `fh' "\multicolumn{6}{l}{\textit{Panel B: In-Migration}} \\" _n
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
	local b = beta_str[`i']
	local b_se = beta_se_str[`i']
	local fe = fe_str[`i']
	local fe_se = fe_se_str[`i']

	if "`fe'" == "" local fe "--"
	if "`fe_se'" == "" local fe_se ""

	if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
		file write `fh' "\addlinespace" _n
	}
	local prev_dt "`dt'"

	file write `fh' "`dt' & `smp' & `mg' & `t_val' & `b' & `fe' \\" _n
	file write `fh' " & & & `se_val' & `b_se' & `fe_se' \\" _n
}

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

** ---- Copy to Overleaf ----
if "${overleaf}" == "1" {
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

** Histogram bin count
local hbins 20

** Helper: build preferred-spec overlay lines for a given variable.
** Sets three locals in the calling scope:
**   pref_overlays : the twoway overlay string
**   n_irs         : count of IRS preferred specs plotted
**   n_acs         : count of ACS preferred specs plotted
capture program drop elast_build_overlays
program define elast_build_overlays
	syntax, VAR(name) COL_IRS(string) COL_ACS(string)
	** Accumulate into program-local `acc' (c_local only writes to caller,
	** it does not read from caller, so referring to `pref_overlays' during
	** accumulation would reference the program-local and silently overwrite).
	local acc ""
	local irs_j = 0
	local acs_j = 0
	forvalues i = 1/`=_N' {
		if preferred[`i'] == 1 & !missing(`var'[`i']) {
			local v = `var'[`i']
			local dt = data_type[`i']
			if strpos("`dt'", "IRS") > 0 {
				local ++irs_j
				local acc `"`acc' (scatteri 0 `v' 1 `v', recast(line) lcolor("`col_irs'") lwidth(medthick) lpattern(dash))"'
			}
			else {
				local ++acs_j
				local acc `"`acc' (scatteri 0 `v' 1 `v', recast(line) lcolor("`col_acs'") lwidth(medthick) lpattern(dash))"'
			}
		}
	}
	c_local pref_overlays `"`acc'"'
	c_local n_irs = `irs_j'
	c_local n_acs = `acs_j'
end

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
	dis as text "Kleven semi-elasticity β distribution (`migr'):"
	summ beta_kleven, detail

	** ---- Panel (a): Kleven semi-elasticity β ----
	elast_build_overlays, var(beta_kleven) col_irs("`col_irs'") col_acs("`col_acs'")
	local leg_irs = 2
	local leg_acs = 2 + `n_irs'

	twoway (histogram beta_kleven, 									///
			fcolor("`col_fill'") lcolor(white) lwidth(thin) 		///
			bin(`hbins') fraction) 									///
		`pref_overlays',											///
		graphregion(color(white)) 									///
		xtitle("{&beta} = ({&tau}/100) / {&Delta}ln(1{&minus}{&tau}{subscript:total})") ///
		ytitle("Fraction of Specifications") 						///
		legend(order(`leg_irs' "IRS Benchmarks" 					///
			`leg_acs' "ACS College Benchmarks") 					///
			ring(1) pos(6) rows(1) size(small)) 					///
		name(panel_a, replace) nodraw

	** ---- Panel (b): Flow elasticity (Kleven total NTR) ----
	qui count if !missing(flow_e)
	local n_fe = r(N)

	if `n_fe' > 0 {
		dis as text "Flow elasticity (total NTR) distribution (`migr'):"
		summ flow_e if !missing(flow_e), detail

		elast_build_overlays, var(flow_e) col_irs("`col_irs'") col_acs("`col_acs'")
		local leg_irs2 = 2
		local leg_acs2 = 2 + `n_irs'

		twoway (histogram flow_e if !missing(flow_e), 					///
				fcolor("`col_fill'") lcolor(white) lwidth(thin)			///
				bin(`hbins') fraction) 									///
			`pref_overlays',											///
			graphregion(color(white)) 									///
			xtitle("{&epsilon}{subscript:flow} = {&minus}({&tau}/pre{&minus}mean) / {&Delta}ln(1{&minus}{&tau}{subscript:total})") ///
			ytitle("Fraction of Specifications") 						///
			legend(order(`leg_irs2' "IRS Benchmarks" 				///
				`leg_acs2' "ACS College Benchmarks") 				///
				ring(1) pos(6) rows(1) size(small)) 				///
			name(panel_b, replace) nodraw

		graph combine panel_a panel_b, 								///
			rows(2) graphregion(color(white))

		graph export "${results}elasticities/fig_elasticity_dist_`migr'.pdf", replace
		graph export "${results}elasticities/fig_elasticity_dist_`migr'.png", ///
			as(png) width(2400) replace

		if "`migr'" == "net" & "${overleaf}" == "1" {
			graph export "${ol_fig}fig_elasticity_dist_net.pdf", replace
		}

		graph drop panel_a panel_b
	}
	else {
		** Net migration: pre_mean ≈ 0 → flow_e undefined; β panel only.
		graph combine panel_a, graphregion(color(white))

		graph export "${results}elasticities/fig_elasticity_dist_`migr'.pdf", replace
		graph export "${results}elasticities/fig_elasticity_dist_`migr'.png", ///
			as(png) width(2400) replace

		if "`migr'" == "net" & "${overleaf}" == "1" {
			graph export "${ol_fig}fig_elasticity_dist_net.pdf", replace
		}

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
dis "Δln(1−τ) total NTR (Kleven):         " %8.6f delta_ln_ntr_total
dis ""

dis "--- Kleven semi-elasticity β and flow elasticity (PRIMARY) ---"
list data_type sample migration tau se ///
	beta_kleven flow_e ///
	cum_flow_impact_common cum_flow_impact_annual ///
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
