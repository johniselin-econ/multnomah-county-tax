/*******************************************************************************
File Name: 		02_elasticities.do
Creator: 		John Iselin
Date Update:	February 2026

Purpose: 	Calculate flow and stock elasticities of migration with respect to the
			Preschool for All (PFA) income tax from SDID treatment effect estimates.

			Formulas:
			  Flow semi-elasticity:  ε_semi = τ / (Δt × 100)
			  Flow elasticity:       ε_flow = (τ / pre_mean) / Δln(1−t)
			  Stock elasticity:      ε_stock = (τ / 100) / Δln(1−t)

			where τ = SDID ATT (pp of migration rate), Δt = avg effective PFA rate,
			Δln(1−t) = ln(1 − avg_mt_rate), and pre_mean = pre-treatment migration rate.

			Note: Since τ is in pp of the migration rate (= flow/stock × 100),
			the flow semi-elasticity already captures the stock-normalized effect.
			The stock elasticity uses 100 (the full stock) as denominator rather
			than pre_mean, following Moretti & Wilson (2017, AER).

Called by: 	00_multnomah.do
Requires:	${data}working/revenue_parameters.dta (from 02_revenue.do)
			${results}sdid/sdid_results.dta (from 02_sdid_analysis.do)

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
capture confirm file "${data}working/revenue_parameters.dta"
if _rc != 0 {
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
restore

** Compute tax change terms
** PFA was new in 2021, so Δt = avg_mt_rate (change from 0)
scalar delta_t = avg_mt_rate
scalar delta_ln_ntr = ln(1 - avg_mt_rate)		// Δln(1−t), negative for tax increase

dis ""
dis "Revenue parameters:"
dis "  avg_mt_rate     = " %10.6f avg_mt_rate
dis "  avg_state_rate  = " %10.6f avg_state_rate
dis "  Δt              = " %10.6f delta_t
dis "  Δln(1−t)        = " %10.6f delta_ln_ntr

** Sanity check: avg_mt_rate should be in a reasonable range
if delta_t < 0.001 | delta_t > 0.05 {
	dis as error "WARNING: avg_mt_rate = " %8.6f delta_t " — outside expected range [0.001, 0.05]"
	dis as error "         Elasticities may be very large or small. Verify 02_revenue.do output."
}

********************************************************************************
** SECTION 1: Load SDID Results & Compute Elasticities
********************************************************************************

dis ""
dis "=============================================="
dis "Section 1: Load SDID results and compute elasticities"
dis "=============================================="

** Check that SDID results exist
capture confirm file "${results}sdid/sdid_results.dta"
if _rc != 0 {
	dis as error "ERROR: sdid_results.dta not found."
	dis as error "       Run 02_sdid_analysis.do first."
	log close log_02elast
	error 601
}
project_assert_manifest using "${results}sdid/sdid_results_manifest.dta", ///
	artifact("sdid_results")

use "${results}sdid/sdid_results.dta", clear

dis "Total specifications loaded: " _N

** ---- Parse outcome variable to extract components ----
** (Mirrors 02_sdid_analysis.do specification curve section)

gen outcome_type = ""
replace outcome_type = "n1" if strpos(outcome, "n1_") > 0
replace outcome_type = "n2" if strpos(outcome, "n2_") > 0
replace outcome_type = "agi" if strpos(outcome, "agi_") > 0

gen migration = ""
replace migration = "net" if strpos(outcome, "_net_") > 0
replace migration = "in" if strpos(outcome, "_in_") > 0
replace migration = "out" if strpos(outcome, "_out_") > 0

gen data_type = ""
replace data_type = "IRS" if strpos(outcome, "_irs") > 0 & strpos(outcome, "_irs_outstate") == 0
replace data_type = "IRS (Out-of-State)" if strpos(outcome, "_irs_outstate") > 0
replace data_type = "IRS (389)" if strpos(sample_data, "irs_389") > 0 & strpos(outcome, "_irs_outstate") == 0
replace data_type = "IRS (389, Out-of-State)" if strpos(sample_data, "irs_389") > 0 & strpos(outcome, "_irs_outstate") > 0
replace data_type = "ACS All (Out-of-State)" if strpos(outcome, "_acs1_outstate") > 0
replace data_type = "ACS College (Out-of-State)" if strpos(outcome, "_acs2_outstate") > 0
replace data_type = "ACS All" if strpos(outcome, "_acs1") > 0 & strpos(outcome, "_acs1_outstate") == 0
replace data_type = "ACS College" if strpos(outcome, "_acs2") > 0 & strpos(outcome, "_acs2_outstate") == 0

gen period_type = ""
replace period_type = "16-22" if strpos(outcome, "_irs") > 0
replace period_type = "16-22" if strpos(sample_data, "16_22") > 0
replace period_type = "16-24" if strpos(sample_data, "16_24") > 0

** Parse out-of-state flag (matches revenue code)
gen outstate = strpos(outcome, "_outstate") > 0 | strpos(outcome, "_irs5") > 0

** ---- Mark preferred specifications ----
project_mark_preferred_main

** ---- Keep only AGI ----
keep if outcome_type == "agi"
dis "AGI specifications: " _N

** ---- Compute elasticities on all AGI specs ----

** Flow semi-elasticity: ε_semi = τ / (Δt × 100)
** Units: pp change in migration rate per pp of tax rate
gen double flow_semi_e = tau / (delta_t * 100)
gen double flow_semi_se = se / (delta_t * 100)

** Flow elasticity: ε_flow = −(τ / pre_mean) / Δln(1−t)
** Sign convention: negative means migration rate worsens when tax rate rises
** (negated to match Kleven et al. convention w.r.t. tax rate, not net-of-tax)
** Note: undefined for net migration where pre_mean ≈ 0
gen double flow_e = -(tau / pre_mean) / delta_ln_ntr if !missing(pre_mean) & pre_mean != 0
gen double flow_se = (se / abs(pre_mean)) / abs(delta_ln_ntr) if !missing(pre_mean) & pre_mean != 0

** Stock elasticity: ε_stock = −(τ / 100) / Δln(1−t)
** Sign convention: negative means tax base shrinks when tax rate rises
** (negated relative to the net-of-tax formulation to match Kleven et al. convention)
** Uses 100 (full stock) as denominator, not pre_mean → well-defined for all migration types
gen double stock_e = -(tau / 100) / delta_ln_ntr
gen double stock_se = (se / 100) / abs(delta_ln_ntr)

** CIs
foreach v in flow_semi_e flow_e stock_e {
	local sev = subinstr("`v'", "_e", "_se", 1)
	gen double `v'_ci_lo = `v' - 1.96 * `sev'
	gen double `v'_ci_hi = `v' + 1.96 * `sev'
}

** Save full dataset (all AGI specs with elasticities)
save "${results}elasticities/elasticity_results.dta", replace

** Report preferred AGI counts
qui count if preferred == 1
local n_preferred = r(N)
dis "Preferred AGI specifications: `n_preferred'"

if `n_preferred' == 0 {
	dis as error "ERROR: No preferred AGI specifications found."
	log close log_02elast
	error 2000
}

** Display preferred AGI specs
dis ""
dis "Preferred AGI specifications for elasticity table:"
list data_type sample migration tau se pre_mean ///
	flow_semi_e flow_e stock_e if preferred == 1, sep(0) abbreviate(20)

********************************************************************************
** SECTION 2: LaTeX Table (AGI, Preferred Specs Only)
********************************************************************************

dis ""
dis "=============================================="
dis "Section 2: LaTeX elasticity table"
dis "=============================================="

** =========================================================================
** (a) Main table: AGI net migration, semi-elasticities only
** =========================================================================

preserve
** Domestic (county-level) net migration only; out-of-state in appendix
keep if preferred == 1 & migration == "net" & outstate == 0

** Formatted strings
gen str12 tau_str = string(tau, "%9.3f")
gen str12 se_str = "(" + string(se, "%9.3f") + ")"
gen str12 fsemi_str = string(flow_semi_e, "%9.3f")
gen str12 fsemi_se_str = "(" + string(flow_semi_se, "%9.3f") + ")"
gen str12 ste_str = string(stock_e, "%9.3f")
gen str12 ste_se_str = "(" + string(stock_se, "%9.3f") + ")"

** Write LaTeX table
tempname fh
file open `fh' using "${results}elasticities/tbl_elasticities.tex", write replace

file write `fh' "\begin{table}[htbp]" _n
file write `fh' "\centering" _n
file write `fh' "\begin{threeparttable}" _n
file write `fh' "\caption{Implied AGI Net Migration Elasticities}" _n
file write `fh' "\label{tab:elasticities}" _n
file write `fh' "\begin{tabular}{ll ccc}" _n
file write `fh' "\toprule" _n
file write `fh' "Data & Sample & $\hat{\tau}$ (pp) & Semi-$\varepsilon$ & Stock $\varepsilon$ \\" _n
file write `fh' "\midrule" _n

** Sort for table output
sort data_type sample
local N = _N
local prev_dt = ""

forvalues i = 1/`N' {
	local dt = data_type[`i']
	local smp = subinstr(sample[`i'], "sample_", "", .)
	local smp = proper("`smp'")
	local t_val = tau_str[`i']
	local se_val = se_str[`i']
	local fs = fsemi_str[`i']
	local fs_se = fsemi_se_str[`i']
	local ste = ste_str[`i']
	local ste_se = ste_se_str[`i']

	** Add spacing between data-type groups
	if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
		file write `fh' "\addlinespace" _n
	}
	local prev_dt "`dt'"

	** Point estimates row
	file write `fh' "`dt' & `smp' & `t_val' & `fs' & `ste' \\" _n
	** Standard errors row
	file write `fh' " & & `se_val' & `fs_se' & `ste_se' \\" _n
}

file write `fh' "\bottomrule" _n
file write `fh' "\end{tabular}" _n
file write `fh' "\begin{tablenotes}" _n
file write `fh' "\small" _n
file write `fh' "\item \textit{Notes:} " _n
file write `fh' "Semi-elasticity: pp change in AGI net migration rate per pp of PFA tax rate. " _n
file write `fh' "Stock elasticity: \% change in county AGI stock for a 1\% increase in the tax rate, " _n
file write `fh' "using the full AGI stock as the base (Moretti and Wilson, 2017). " _n
file write `fh' "Negative values indicate the tax base shrinks when the tax rate rises. " _n
local pfa_pct : di %5.3f delta_t * 100
local pfa_pct = strtrim("`pfa_pct'")

file write `fh' "Average effective PFA rate: `pfa_pct'\%. " _n
file write `fh' "Flow elasticities for gross migration are in Appendix Table~\ref{tab:elasticities_inout}. " _n
file write `fh' "Standard errors in parentheses, derived from SDID bootstrap SEs." _n
file write `fh' "\end{tablenotes}" _n
file write `fh' "\end{threeparttable}" _n
file write `fh' "\end{table}" _n

file close `fh'
restore

** =========================================================================
** (b) Appendix table: AGI out- and in-migration, semi-ε and full ε
** =========================================================================

preserve
keep if preferred == 1 & inlist(migration, "out", "in")

** Formatted strings
gen str12 tau_str = string(tau, "%9.3f")
gen str12 se_str = "(" + string(se, "%9.3f") + ")"
gen str12 fsemi_str = string(flow_semi_e, "%9.3f")
gen str12 fsemi_se_str = "(" + string(flow_semi_se, "%9.3f") + ")"
gen str12 fe_str = string(flow_e, "%9.3f") if !missing(flow_e)
gen str12 fe_se_str = "(" + string(flow_se, "%9.3f") + ")" if !missing(flow_se)
gen str12 ste_str = string(stock_e, "%9.3f")
gen str12 ste_se_str = "(" + string(stock_se, "%9.3f") + ")"

** Migration label
gen str20 migr_label = ""
replace migr_label = "In" if migration == "in"
replace migr_label = "Out" if migration == "out"

tempname fh
file open `fh' using "${results}elasticities/tbl_elasticities_inout.tex", write replace

file write `fh' "\begin{table}[htbp]" _n
file write `fh' "\centering" _n
file write `fh' "\begin{threeparttable}" _n
file write `fh' "\caption{Implied AGI Migration Elasticities: Out- and In-Migration}" _n
file write `fh' "\label{tab:elasticities_inout}" _n
file write `fh' "\footnotesize" _n
file write `fh' "\begin{tabular}{lll cccc}" _n
file write `fh' "\toprule" _n
file write `fh' "Data & Sample & Direction & $\hat{\tau}$ (pp) & Semi-$\varepsilon$ & Flow $\varepsilon$ & Stock $\varepsilon$ \\" _n
file write `fh' "\midrule" _n

** Panel A header
file write `fh' "\addlinespace" _n
file write `fh' "\multicolumn{7}{l}{\textit{Panel A: Out-Migration}} \\" _n
file write `fh' "\addlinespace" _n

sort data_type sample migration
local N = _N
local prev_dt = ""

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
	local fe = fe_str[`i']
	local fe_se = fe_se_str[`i']
	local ste = ste_str[`i']
	local ste_se = ste_se_str[`i']

	if "`fe'" == "" local fe "--"
	if "`fe_se'" == "" local fe_se ""

	if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
		file write `fh' "\addlinespace" _n
	}
	local prev_dt "`dt'"

	file write `fh' "`dt' & `smp' & `mg' & `t_val' & `fs' & `fe' & `ste' \\" _n
	file write `fh' " & & & `se_val' & `fs_se' & `fe_se' & `ste_se' \\" _n
}

** Panel B header
file write `fh' "\addlinespace[0.75em]" _n
file write `fh' "\midrule" _n
file write `fh' "\addlinespace" _n
file write `fh' "\multicolumn{7}{l}{\textit{Panel B: In-Migration}} \\" _n
file write `fh' "\addlinespace" _n

local prev_dt = ""

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
	local fe = fe_str[`i']
	local fe_se = fe_se_str[`i']
	local ste = ste_str[`i']
	local ste_se = ste_se_str[`i']

	if "`fe'" == "" local fe "--"
	if "`fe_se'" == "" local fe_se ""

	if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
		file write `fh' "\addlinespace" _n
	}
	local prev_dt "`dt'"

	file write `fh' "`dt' & `smp' & `mg' & `t_val' & `fs' & `fe' & `ste' \\" _n
	file write `fh' " & & & `se_val' & `fs_se' & `fe_se' & `ste_se' \\" _n
}

file write `fh' "\bottomrule" _n
file write `fh' "\end{tabular}" _n
file write `fh' "\begin{tablenotes}" _n
file write `fh' "\small" _n
file write `fh' "\item \textit{Notes:} " _n
file write `fh' "Semi-elasticity: pp change in AGI migration rate per pp of PFA tax rate. " _n
file write `fh' "Flow elasticity: \% change in migration rate for a 1\% increase in the tax rate, " _n
file write `fh' "using the pre-treatment migration rate as the base. " _n
file write `fh' "Stock elasticity: \% change in county AGI stock for a 1\% increase in the tax rate, " _n
file write `fh' "using the full AGI stock as the base (Moretti and Wilson, 2017). " _n
file write `fh' "Negative values indicate the measure worsens when the tax rate rises. " _n
local pfa_pct : di %5.3f delta_t * 100
local pfa_pct = strtrim("`pfa_pct'")
file write `fh' "Average effective PFA rate: `pfa_pct'\%. " _n
file write `fh' "Standard errors in parentheses, derived from SDID bootstrap SEs." _n
file write `fh' "\end{tablenotes}" _n
file write `fh' "\end{threeparttable}" _n
file write `fh' "\end{table}" _n

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
	dis "--- `migr_title'-migration: `n_all' AGI specs (`n_pref' preferred) ---"
	summ flow_semi_e, detail

	** Build individual vertical lines for each preferred spec (panel a)
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
			bin(20) fraction) 										///
		`pref_semi_overlays',										///
		graphregion(color(white)) 									///
		title("(a) Flow Semi-Elasticity", size(medium)) 			///
		xtitle("Semi-{&epsilon} (pp migration rate per pp tax rate)") ///
		ytitle("Fraction of Specifications") 						///
		legend(order(`leg_irs' "IRS Preferred" 						///
			`leg_acs' "ACS College Preferred") 						///
			ring(1) pos(6) rows(1) size(small)) 					///
		name(panel_a, replace) nodraw

	** ---- Panel (b): Flow elasticity ----
	** Drop specs with missing flow_e (zero pre_mean)
	qui count if !missing(flow_e)
	local n_fe = r(N)

	if `n_fe' > 0 {
		qui count if preferred == 1 & !missing(flow_e)
		local n_pref_fe = r(N)

		summ flow_e if !missing(flow_e), detail

		** Build individual vertical lines for each preferred spec (panel b)
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
				bin(20) fraction) 										///
			`pref_fe_overlays',											///
			graphregion(color(white)) 									///
			title("(b) Flow Elasticity (Net-of-Tax)", size(medium))		///
			xtitle("{&epsilon} (% {&Delta} migration rate / % {&Delta} net-of-tax rate)") ///
			ytitle("Fraction of Specifications") 						///
			legend(order(`leg_irs2' "IRS Preferred" 					///
				`leg_acs2' "ACS College Preferred") 					///
				ring(1) pos(6) rows(1) size(small)) 				///
			name(panel_b, replace) nodraw

		** ---- Combine panels (stacked vertically) ----
		graph combine panel_a panel_b, 									///
			rows(2) graphregion(color(white)) 							///
			title("Distribution of Implied AGI `migr_title'-Migration Elasticities", ///
				size(medlarge)) 										///
			subtitle("Across `n_all' SDID specifications", 			///
				size(small))

		graph export "${results}elasticities/fig_elasticity_dist_`migr'.pdf", replace
		graph export "${results}elasticities/fig_elasticity_dist_`migr'.png", ///
			as(png) width(2400) replace

		** Overleaf copy — net migration only (appendix figure)
		if "`migr'" == "net" & ${overleaf} == 1 {
			graph export "${ol_fig}fig_elasticity_dist_net.pdf", replace
		}

		graph drop panel_a panel_b
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
dis "ELASTICITY SUMMARY — AGI PREFERRED SPECIFICATIONS"
dis "=================================================================="
dis ""
dis "Average effective PFA rate (Δt):  " %8.4f delta_t " (" %5.3f delta_t * 100 "%)"
dis "Δln(1−t):                         " %8.6f delta_ln_ntr
dis ""

list data_type sample migration tau se ///
	flow_semi_e flow_e stock_e ///
	if preferred == 1, sep(0) abbreviate(20)

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
