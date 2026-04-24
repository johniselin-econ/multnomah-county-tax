/*******************************************************************************
File Name:      02_tables_figures.do
Creator:        John Iselin
Date Created:   2026-04-24

Purpose:        Renders every elasticity and revenue-distribution table and
                figure from spec_results.dta. No arithmetic beyond formatting
                and overlay construction — all spec-level computation was done
                by 02_post_spec.do using the spec engine.

                Replaces:
                  - 02_elasticities.do §2 (6 LaTeX tables + Excel workbook)
                  - 02_elasticities.do §3 (8 distribution figures)
                  - 02_revenue.do §12 (2 revenue-distribution figures, stripped
                    in commit 110dfac; returns here)

                Wired into the orchestrator in commit A5; until then this
                file must be invoked manually for testing.

Called by:      00_multnomah.do (starting in commit A5)
Requires:       ${results}elasticities/spec_results.dta (from 02_post_spec.do)
                ${data}working/revenue_parameters.dta (scalars for table notes)
                02_spec_engine.do (sourced at top — provides elast_tex_* and
                                   elast_inout_panel and elast_hist_plot helpers)

Outputs:        ${results}elasticities/
                    tbl_elasticities.tex            main table (PFA)
                    tbl_elasticities_stock_compare.tex  stock compare (PFA)
                    tbl_elasticities_inout.tex      gross in/out (PFA)
                    tbl_elasticities_shs.tex        main table (PFA+SHS)
                    tbl_elasticities_stock_compare_shs.tex
                    tbl_elasticities_inout_shs.tex
                    tbl_elasticities.xlsx           5 sheets
                    preferred_net_stock.csv
                    preferred_net_stock_shs.csv
                    fig_elast_beta_{net,in,out}{,_shs}.{pdf,png}
                    fig_elast_stock_net_common{,_shs}.{pdf,png}
                ${results}revenue/
                    fig_revenue_dist_pfa.{pdf,png}
                    fig_revenue_dist_oregon.{pdf,png}
                ${ol_tab}/, ${ol_fig}/ if ${overleaf}==1 (copies)

Authors: John Iselin

For more information, contact john.iselin@yale.edu
*******************************************************************************/

** ------------------------------------------------------------------
** Bootstrap project paths
** ------------------------------------------------------------------
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
** 01a_programs.do is normally sourced by 00_multnomah.do; source defensively
** so 02_tables_figures.do can run standalone during development.
do "${code}01a_programs.do"
do "${code}02_spec_engine.do"

capture log close log_02tf
log using "${logs}02_log_tables_figures_${date}", name(log_02tf) replace text

project_set_seed, context("02_tables_figures.do") offset(60)

dis ""
dis "=============================================="
dis "02_tables_figures.do: render tables + figures"
dis "=============================================="

** Validate inputs
capture confirm file "${results}elasticities/spec_results.dta"
if _rc != 0 {
	dis as error "ERROR: spec_results.dta not found. Run 02_post_spec.do first."
	log close log_02tf
	error 601
}

** Overleaf globals check (matches pattern from 02_post_spec.do's predecessor)
if "${overleaf}" == "1" {
	foreach g in ol_fig ol_tab {
		if "${`g'}" == "" {
			dis as error "ERROR: \${overleaf}=1 but \${`g'} is unset."
			log close log_02tf
			error 198
		}
	}
}

capture mkdir "${results}elasticities"
capture mkdir "${results}revenue"

** ------------------------------------------------------------------
** Load revenue-parameter scalars. Needed for:
**   - avg_shs_rate / avg_total_rate / avg_total_rate_with_shs etc. in
**     table notes and Excel "run_parameters" sheet
**   - delta_ln_ntr_total and friends if any inline calc emerges
** load_revenue_params also computes the four delta_ln_ntr_total* scalars.
** ------------------------------------------------------------------
load_revenue_params

** `delta_t` was a convenience scalar in the pre-restructure 02_elasticities.do
** pointing at avg_mt_rate (PFA is a new tax, so Δt = avg_mt_rate). Preserve
** the name for compatibility with the Excel recalc_components sheet.
scalar delta_t = avg_mt_rate

** ------------------------------------------------------------------
** Load spec_results.dta — everything below operates on this in-memory.
** ------------------------------------------------------------------
use "${results}elasticities/spec_results.dta", clear
dis "Loaded " _N " spec rows."

********************************************************************************
** SECTION 1: LaTeX tables
********************************************************************************

dis ""
dis "=============================================="
dis "Section 1: LaTeX tables"
dis "=============================================="

** Shared rate strings used in notes.
local pfa_pct : di %5.3f delta_t * 100
local pfa_pct = strtrim("`pfa_pct'")
local total_pct : di %5.1f avg_total_rate * 100
local total_pct = strtrim("`total_pct'")

** =========================================================================
** (a) Main table: highlighted AGI net migration
** =========================================================================

preserve
keep if preferred == 1 & migration == "net" & outstate == 0

gen str20 tau_str = string(tau, "%9.3f")
gen str20 se_str = "(" + string(se, "%9.3f") + ")"
gen str20 beta_str = string(beta_kleven, "%9.3f")
gen str20 beta_se_str = "(" + string(beta_se_kleven, "%9.3f") + ")"
gen str20 stock_common_str = string(stock_elast_total_common, "%9.3f") ///
	if !missing(stock_elast_total_common)

tempname fh
file open `fh' using "${results}elasticities/tbl_elasticities.tex", write replace

elast_tex_open, handle(`fh') ///
	cap("Highlighted AGI Net-Migration Elasticities (Kleven 2024 Framework)") ///
	lbl("tab:elasticities") cols("ll ccc")
file write `fh' "Data & Sample & $\hat{\tau}$ (pp) & Semi-$\varepsilon$ $\beta$ & Stock $\varepsilon$ \\" _n
file write `fh' " & & & (Kleven) & (Total AGI, 2021--2022) \\" _n
file write `fh' "\midrule" _n

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
		file write `fh' "\addlinespace" _n
	}
	local prev_dt "`dt'"

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
** (b) Stock-compare table: preferred AGI net migration
** =========================================================================

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

** =========================================================================
** (c) Gross in/out table
** =========================================================================

preserve
keep if preferred == 1 & inlist(migration, "out", "in") & outstate == 0

gen str20 tau_str = string(tau, "%9.3f")
gen str20 se_str = "(" + string(se, "%9.3f") + ")"
gen str20 beta_str = string(beta_kleven, "%9.3f")
gen str20 beta_se_str = "(" + string(beta_se_kleven, "%9.3f") + ")"
gen str20 fe_str = string(flow_e, "%9.3f") if !missing(flow_e)
gen str20 fe_se_str = "(" + string(flow_se, "%9.3f") + ")" if !missing(flow_se)

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

file write `fh' "\addlinespace" _n
file write `fh' "\multicolumn{6}{l}{\textit{Panel A: Out-Migration}} \\" _n
file write `fh' "\addlinespace" _n
elast_inout_panel, handle(`fh') direction("out")

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
** (d) SHS-inclusive main table
** =========================================================================

local shs_pct        : di %5.3f avg_shs_rate * 100
local shs_pct        = strtrim("`shs_pct'")
local total_shs_pct  : di %5.1f avg_total_rate_with_shs * 100
local total_shs_pct  = strtrim("`total_shs_pct'")

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

** =========================================================================
** (e) SHS stock-compare table
** =========================================================================

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

** =========================================================================
** (f) SHS gross in/out table
** =========================================================================

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

file write `fh_shs_io' "\addlinespace" _n
file write `fh_shs_io' "\multicolumn{6}{l}{\textit{Panel A: Out-Migration}} \\" _n
file write `fh_shs_io' "\addlinespace" _n
elast_inout_panel, handle(`fh_shs_io') direction("out")

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

** ---- Copy LaTeX tables to Overleaf ----
if "${overleaf}" == "1" {
	foreach f in tbl_elasticities tbl_elasticities_stock_compare ///
		tbl_elasticities_inout tbl_elasticities_shs ///
		tbl_elasticities_stock_compare_shs tbl_elasticities_inout_shs {
		copy "${results}elasticities/`f'.tex" "${ol_tab}`f'.tex", replace
	}
}

********************************************************************************
** SECTION 2: Excel workbook
********************************************************************************

dis ""
dis "Section 2: Excel workbook"

** Sheet 1: full raw results
export excel using "${results}elasticities/tbl_elasticities.xlsx", ///
	sheet("full_results") firstrow(variables) replace

** Sheet 2: curated calculation components
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

** Sheet 2b: preferred net stock comparison
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

** Sheet 2c: SHS-inclusive preferred net stock
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

** Sheet 3: one-row run parameters
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

** Sheet 4: variable guide
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

** SHS-inclusive entries
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

** Reload the spec dataset for Section 3 (figures)
use "${results}elasticities/spec_results.dta", clear

********************************************************************************
** SECTION 3: Elasticity distribution figures
********************************************************************************

dis ""
dis "=============================================="
dis "Section 3: Elasticity distribution figures"
dis "=============================================="

** plotplainblind palette
local col_fill "86 180 233"		// sky — histogram fill
local col_irs  "213 94 0"		// vermillion — IRS preferred
local col_acs  "0 114 178"		// sea — ACS College preferred

** ---- β histograms (PFA + SHS) for each migration direction ----
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

	elast_hist_plot, var(beta_kleven) ///
		xtitle(`"{&beta} = ({&tau}/100) / {&Delta}ln(1{&minus}{&tau}{subscript:total})"') ///
		file("${results}elasticities/fig_elast_beta_`migr'") ///
		colfill("`col_fill'") colirs("`col_irs'") colacs("`col_acs'")

	dis as text "Kleven semi-elasticity beta distribution (`migr'), +SHS:"
	summ beta_kleven_shs, detail
	elast_hist_plot, var(beta_kleven_shs) ///
		xtitle(`"{&beta} (PFA+SHS) = ({&tau}/100) / {&Delta}ln(1{&minus}{&tau}{subscript:total+SHS})"') ///
		file("${results}elasticities/fig_elast_beta_`migr'_shs") ///
		colfill("`col_fill'") colirs("`col_irs'") colacs("`col_acs'")

	restore
}

** ---- Stock ε histograms (net migration only; PFA + SHS) ----
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

** Overleaf copy for elasticity figures. Preserve legacy filename
** fig_elasticity_dist_net.pdf as a copy of fig_elast_beta_net.pdf so the
** manuscript's \includegraphics paths don't need updating.
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
	capture confirm file "${results}elasticities/fig_elast_beta_net.pdf"
	if _rc == 0 {
		copy "${results}elasticities/fig_elast_beta_net.pdf" ///
			"${ol_fig}fig_elasticity_dist_net.pdf", replace
	}
}

********************************************************************************
** SECTION 4: Revenue-distribution figures
********************************************************************************
** Restored here from the pre-A2 02_revenue.do §12. pfa_loss (for net-domestic
** specs) and state_loss (for net-outstate specs) are already populated in
** spec_results.dta by 02_post_spec.do calling compute_spec_revenue — the job
** of this section is just to histogram them with preferred-spec overlays.

dis ""
dis "=============================================="
dis "Section 4: Revenue-loss distribution figures"
dis "=============================================="

** PFA: net-domestic specs, pfa_loss column
preserve
keep if migration == "net" & outstate == 0 & !missing(pfa_loss)

qui count
if r(N) > 0 {
	dis ""
	dis "PFA implied loss distribution ($ millions), " _N " specs:"
	summ pfa_loss, detail

	elast_hist_plot, var(pfa_loss) ///
		xtitle("Implied PFA Revenue Loss ($ millions)") ///
		file("${results}revenue/fig_revenue_dist_pfa") ///
		colfill("`col_fill'") colirs("`col_irs'") colacs("`col_acs'")
}
else {
	dis as text "No pfa_loss values available — skipping fig_revenue_dist_pfa."
}
restore

** Oregon: net-outstate specs, state_loss column
preserve
keep if migration == "net" & outstate == 1 & !missing(state_loss)

qui count
if r(N) > 0 {
	dis ""
	dis "Oregon implied loss distribution ($ millions), " _N " specs:"
	summ state_loss, detail

	elast_hist_plot, var(state_loss) ///
		xtitle("Implied Oregon State Revenue Loss ($ millions)") ///
		file("${results}revenue/fig_revenue_dist_oregon") ///
		colfill("`col_fill'") colirs("`col_irs'") colacs("`col_acs'")
}
else {
	dis as text "No state_loss values available — skipping fig_revenue_dist_oregon."
}
restore

** Overleaf copy for revenue-dist figures.
if "${overleaf}" == "1" {
	foreach base in fig_revenue_dist_pfa fig_revenue_dist_oregon {
		capture confirm file "${results}revenue/`base'.pdf"
		if _rc == 0 {
			copy "${results}revenue/`base'.pdf" ///
				"${ol_fig}`base'.pdf", replace
		}
	}
}

********************************************************************************
** SECTION 5: Summary
********************************************************************************

dis ""
dis "=============================================="
dis "02_tables_figures.do complete."
dis "Outputs:"
dis "  ${results}elasticities/tbl_elasticities.tex"
dis "  ${results}elasticities/tbl_elasticities_inout.tex"
dis "  ${results}elasticities/tbl_elasticities_stock_compare.tex"
dis "  ${results}elasticities/tbl_elasticities_shs.tex"
dis "  ${results}elasticities/tbl_elasticities_inout_shs.tex"
dis "  ${results}elasticities/tbl_elasticities_stock_compare_shs.tex"
dis "  ${results}elasticities/tbl_elasticities.xlsx"
dis "  ${results}elasticities/preferred_net_stock.csv"
dis "  ${results}elasticities/preferred_net_stock_shs.csv"
dis "  ${results}elasticities/fig_elast_beta_{net,in,out}{,_shs}.{pdf,png}"
dis "  ${results}elasticities/fig_elast_stock_net_common{,_shs}.{pdf,png}"
dis "  ${results}revenue/fig_revenue_dist_pfa.{pdf,png}"
dis "  ${results}revenue/fig_revenue_dist_oregon.{pdf,png}"
dis "=============================================="

capture log close log_02tf
