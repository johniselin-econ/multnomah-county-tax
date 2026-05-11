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
                                   elast_inout_panel helpers; elast_speccurve_plot
                                   is defined locally in this file)

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
                    fig_speccurve_elast_beta_{net,in,out}{,_shs}.{pdf,png}
                    fig_speccurve_elast_stock{,_shs}.{pdf,png}
                ${results}revenue/
                    fig_speccurve_revenue_pfa.{pdf,png}
                    fig_speccurve_revenue_oregon.{pdf,png}
                ${results}sdid/preferred_overlays/
                    fig_overlay_donorpool_<sdtag>_<migr>_eventstudy.{pdf,jpg}   (12 figs)
                    fig_overlay_dataset_<scope>_<migr>_eventstudy.{pdf,jpg}    (6 figs)
                ${ol_tab}/, ${ol_fig}/ if ${overleaf}==1 (copies)

Authors: John Iselin

TODO: 
1) Check to be sure revenue figures are annual!
2) Check color alignment of figures - make sure dashed lines are colored / labeled correctly

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

** ------------------------------------------------------------------
** Bootstrap-CI flag (Phase B5).
**
** When ${show_bootstrap_cis} == 1, the highlighted LaTeX tables render
** percentile CIs `[lo, hi]` on the second line beneath each point
** estimate (replacing the `(SE)` line, since analytic vce(placebo) SEs
** are not run-to-run reproducible — see V1 verification, 2026-04-27).
** Default 0 (current point-estimate-only output, byte-identical to
** pre-B5 behavior).
**
** Requires bootstrap_cis.dta on disk; produced by:
**   do "${code}02_bootstrap.do"        (parallel via Stata `parallel` ado, or
**                                       serial when use_parallel=0)
**   do "${code}02_bootstrap_tables.do"
** ------------------------------------------------------------------
if "${show_bootstrap_cis}" == "" global show_bootstrap_cis = 0
if ${show_bootstrap_cis} == 1 {
	capture confirm file "${results}bootstrap/bootstrap_cis.dta"
	if _rc != 0 {
		dis as error "ERROR: \${show_bootstrap_cis}=1 but ${results}bootstrap/bootstrap_cis.dta not found."
		dis as error "       Run 02_bootstrap.do + 02_bootstrap_tables.do first."
		log close log_02tf
		error 601
	}
}

** ------------------------------------------------------------------
** Local helper programs (table + figure rendering)
**
** elast_inout_panel — writes one migration-direction panel of the
** gross in/out tables. Assumes the current dataset has data_type,
** sample, migration, migr_label, and the formatted str20 columns
** tau_str, se_str, beta_str, beta_se_str (generated once per table
** in the caller's preserve block). The gross tables report
** $\hat{\tau}$ and the Kleven semi-elasticity only; stock elasticity
** is reported on the net-migration tables (Tables 2 / A3) instead.
**
** elast_speccurve_plot — writes a specification-curve plot with
** ranked point estimates (and bootstrap CI whiskers if available) in
** the upper zone, plus a configurable indicator-dot panel below.
** Mirrors the SDID spec-curve template at 02_sdid_analysis.do:1452-1828.
** Exports .pdf and .png at 2400px. Replaced the old elast_hist_plot
** histogram view.
**
** These helpers previously lived in 02_elasticities.do and moved here
** when that file was retired in commit A5 (21c612c). 02_tables_figures.do
** is the sole consumer; keeping them local avoids growing the engine
** module with rendering-only code.
** ------------------------------------------------------------------

capture program drop elast_inout_panel
program define elast_inout_panel
	** BETACIVAR: variable name of the pre-built bootstrap-CI string
	** for the Kleven semi-elasticity in the working dataset. Defaults
	** to flow_semi_ci_str; SHS caller overrides to flow_semi_shs_ci_str.
	** Only consulted when ${show_bootstrap_cis} == 1.
	** Note the concatenated option name: Stata's `syntax` parser does
	** not reliably accept option names containing underscores.
	syntax, HANDLE(string) DIRECTION(string) [BETACIVAR(name)]

	if "`betacivar'" == "" local betacivar flow_semi_ci_str

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

		if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
			file write `handle' "\addlinespace" _n
		}
		local prev_dt "`dt'"

		file write `handle' "`dt' & `smp' & `mg' & `t_val' & `b' \\" _n
		if ${show_bootstrap_cis} == 1 {
			local tau_ci  = tau_ci_str[`i']
			local beta_ci = `betacivar'[`i']
			file write `handle' " & & & `tau_ci' & `beta_ci' \\" _n
		}
		else {
			file write `handle' " & & & `se_val' & `b_se' \\" _n
		}
	}
end

** ------------------------------------------------------------------
** elast_speccurve_plot
**
** Specification-curve plot for derived elasticities and revenue losses.
** Mirrors the SDID spec-curve template at 02_sdid_analysis.do:1452-1828
** so the figures share visual language. Replaces the old elast_hist_plot
** histogram view.
**
** Coefficient zone (top): point estimates ranked ascending by `var`,
** colored by (significant × preferred). Whiskers from `lovar`/`hivar`
** when supplied (typically the bootstrap CI columns merged from
** bootstrap_cis.dta); falls back to dot-only when those columns are
** absent or all-missing.
**
** Indicator zone (bottom): one row per spec_* dummy in INDICATORS().
** Dummies are constructed on the fly from `sample`, `data_type`, and
** `period_type`, so the helper does not require schema additions to
** spec_results.dta.
**
** Required syntax:
**     elast_speccurve_plot, var(<num>) ytitle("...") file("...") ///
**         indicators("name1 name2 ...")                          ///
**         [lovar(<colname>) hivar(<colname>)]                    ///
**         [colsignotpref("...") colinsignotpref("...")           ///
**          colsigpref("...")    colinsigpref("...")              ///
**          colzero("...")]
**
** ytitle is the metric label rendered on the y-axis (e.g.,
** "{&beta} = ({&tau}/100) / {&Delta}ln(1{&minus}{&tau})"). In the
** legacy elast_hist_plot signature the same string was passed as
** xtitle because the metric was the x-axis of the histogram.
**
** lovar/hivar take *string* column names rather than `varname numeric`
** so the caller can reference bootstrap CI columns that exist only when
** ${show_bootstrap_cis} == 1. The helper validates with `confirm numeric
** variable` internally and falls back to dot-only when columns are
** absent or all-missing.
**
** Color args default to the ${col_*} globals set in 00_stata_config.do.
** Recognized indicator names (each maps to one row of dots):
**     spec_irs spec_irs_outstate spec_acs_all spec_acs_all_outstate
**     spec_acs_col spec_acs_col_outstate
**     spec_all spec_stringency spec_urban95 spec_demog spec_covid
**     spec_16_22 spec_16_24
**     spec_covars spec_excl2020
**
** Side effects: drops temporary spec_*, tau_*pref, ci_*pref, y_*, and
** spec_rank columns it creates. Operates on the current dataset
** in-memory and assumes the caller has already filtered to the
** relevant subset (e.g., by migration / outstate).
** ------------------------------------------------------------------
capture program drop elast_speccurve_plot
program define elast_speccurve_plot
	** Accept LOVAR / HIVAR as strings (not varname numeric) so callers can
	** name columns that exist only when ${show_bootstrap_cis}==1. We confirm
	** them ourselves below; callers don't need to branch on the flag.
	syntax , VAR(varname numeric) YTITLE(string asis) FILE(string)        ///
		INDICATORS(string)                                                ///
		[ LOVAR(string) HIVAR(string)                                     ///
		  COLSIGNOTPREF(string) COLINSIGNOTPREF(string)                   ///
		  COLSIGPREF(string)    COLINSIGPREF(string)                      ///
		  COLZERO(string) ]

	** Color defaults from globals
	if "`colsignotpref'"   == "" local colsignotpref   "${col_sig_notpref}"
	if "`colinsignotpref'" == "" local colinsignotpref "${col_insig_notpref}"
	if "`colsigpref'"      == "" local colsigpref      "${col_sig_pref}"
	if "`colinsigpref'"    == "" local colinsigpref    "${col_insig_pref}"
	if "`colzero'"         == "" local colzero         "${col_zero}"

	** Drop rows where var is missing — a spec without a defined estimate
	** has nothing to plot. Operates on a temporary working copy via preserve
	** so the caller's data is untouched on exit.
	preserve
	qui keep if !missing(`var')
	qui count
	if r(N) == 0 {
		dis as text "  No non-missing `var' values — skipping `file'."
		restore
		exit
	}
	local n_specs = r(N)

	** Sort and rank — ascending by `var`, ties broken arbitrarily.
	sort `var'
	gen long spec_rank = _n

	** Decide whether bootstrap CIs are usable. Both lovar and hivar must
	** be supplied, both must exist as numeric variables in memory, AND at
	** least one row must have both non-missing.
	local has_ci = 0
	if "`lovar'" != "" & "`hivar'" != "" {
		capture confirm numeric variable `lovar'
		local lovar_ok = (_rc == 0)
		capture confirm numeric variable `hivar'
		local hivar_ok = (_rc == 0)
		if `lovar_ok' & `hivar_ok' {
			qui count if !missing(`lovar') & !missing(`hivar')
			if r(N) > 0 local has_ci = 1
		}
	}

	** Four-category split on (significant, preferred).
	gen double v_sig_notpref   = `var' if significant == 1 & preferred == 0
	gen double v_insig_notpref = `var' if significant == 0 & preferred == 0
	gen double v_sig_pref      = `var' if significant == 1 & preferred == 1
	gen double v_insig_pref    = `var' if significant == 0 & preferred == 1

	if `has_ci' {
		gen double cilo_sig_notpref   = `lovar' if significant == 1 & preferred == 0
		gen double cihi_sig_notpref   = `hivar' if significant == 1 & preferred == 0
		gen double cilo_insig_notpref = `lovar' if significant == 0 & preferred == 0
		gen double cihi_insig_notpref = `hivar' if significant == 0 & preferred == 0
		gen double cilo_sig_pref      = `lovar' if significant == 1 & preferred == 1
		gen double cihi_sig_pref      = `hivar' if significant == 1 & preferred == 1
		gen double cilo_insig_pref    = `lovar' if significant == 0 & preferred == 1
		gen double cihi_insig_pref    = `hivar' if significant == 0 & preferred == 1
	}

	** y-axis range for the coefficient zone. Use CI extremes if available,
	** otherwise the var range, with a small pad.
	if `has_ci' {
		qui summ `lovar'
		local y_min = r(min)
		qui summ `hivar'
		local y_max = r(max)
	}
	else {
		qui summ `var'
		local y_min = r(min)
		local y_max = r(max)
	}
	if missing(`y_min') | missing(`y_max') {
		** Degenerate guard — should not happen given the n>0 check above.
		local y_min = 0
		local y_max = 1
	}
	local pad = max((`y_max' - `y_min') * 0.05, 0.0001)
	local y_min = `y_min' - `pad'
	local y_max = `y_max' + `pad'
	** Force y=0 inside the coefficient zone so the dashed zero reference
	** line never lands among the indicator dots below.
	local y_min = min(`y_min', 0)
	local y_max = max(`y_max', 0)
	local data_range = `y_max' - `y_min'

	** Adaptive tick step for the coefficient zone — six bins cover the full
	** range of metrics this helper sees (β ≈ 0.05 → state_loss ≈ 100s).
	if      `data_range' >= 100  local tick_step = 25
	else if `data_range' >= 50   local tick_step = 10
	else if `data_range' >= 20   local tick_step = 5
	else if `data_range' >= 5    local tick_step = 1
	else if `data_range' >= 2    local tick_step = 0.5
	else if `data_range' >= 0.5  local tick_step = 0.1
	else                          local tick_step = 0.05
	local tick_lo = floor(`y_min' / `tick_step') * `tick_step'
	local tick_hi =  ceil(`y_max' / `tick_step') * `tick_step'

	** Indicator zone scales with the data range so the same template works
	** for elasticities (β ~ 0–0.5) and revenue losses ($M, range 10–60+).
	** Separator sits 15% of data_range below y_min; rows step down by 7%.
	local sep_y    = `y_min' - 0.15 * `data_range'
	local row_step = 0.07 * `data_range'
	local ind_top  = `sep_y' - `row_step'

	** Build spec_* dummies on the fly (caller may have filtered, so dummies
	** can be all-zero on some rows; that's fine — the y_<name> = . path
	** suppresses dots for those rows).
	gen byte spec_all              = sample == "sample_all"
	gen byte spec_stringency       = sample == "sample_stringency"
	gen byte spec_urban95          = sample == "sample_urban95"
	gen byte spec_demog            = sample == "sample_demog"
	gen byte spec_covid            = sample == "sample_urban75_covid"
	gen byte spec_covars           = controls == 1
	gen byte spec_excl2020         = exclusion == 1
	gen byte spec_irs              = data_type == "IRS"
	gen byte spec_irs_outstate     = data_type == "IRS (Out-of-State)"
	gen byte spec_acs_all          = data_type == "ACS All"
	gen byte spec_acs_all_outstate = data_type == "ACS All (Out-of-State)"
	gen byte spec_acs_col          = data_type == "ACS College"
	gen byte spec_acs_col_outstate = data_type == "ACS College (Out-of-State)"
	gen byte spec_16_22            = period_type == "16-22"
	gen byte spec_16_24            = period_type == "16-24"

	** Indicator label dictionary. Order in `indicators` is presentation
	** order, top to bottom.
	local lbl_spec_all              `"All Counties"'
	local lbl_spec_urban95          `"Urban (Top 5%)"'
	local lbl_spec_covid            `"COVID Match"'
	local lbl_spec_demog            `"Demographic Match"'
	local lbl_spec_stringency       `"Stringency Match"'
	local lbl_spec_covars           `"Covariates"'
	local lbl_spec_excl2020         `"Excl. 2020"'
	local lbl_spec_irs              `"IRS"'
	local lbl_spec_irs_outstate     `"IRS (Out-of-State)"'
	local lbl_spec_acs_all          `"ACS All"'
	local lbl_spec_acs_all_outstate `"ACS All (Out-of-State)"'
	local lbl_spec_acs_col          `"ACS College"'
	local lbl_spec_acs_col_outstate `"ACS College (Out-of-State)"'
	local lbl_spec_16_22            `"16-22"'
	local lbl_spec_16_24            `"16-24"'

	** For each requested indicator, generate y-coord var and accumulate
	** scatter layers + ylabel pairs.
	local ind_layers `""'
	local ind_ylabels `""'
	local row_idx = 0
	foreach ind_name of local indicators {
		capture confirm variable `ind_name'
		if _rc != 0 {
			dis as error "elast_speccurve_plot: unrecognized indicator `ind_name'"
			restore
			exit 198
		}
		local row_idx = `row_idx' + 1
		local ypos = `ind_top' - (`row_idx' - 1) * `row_step'
		gen double y_`ind_name' = `ypos' if `ind_name' == 1
		local ind_layers `"`ind_layers' (scatter y_`ind_name' spec_rank, mc("`colsignotpref'") ms(O) msize(vsmall))"'
		local ind_ylabels `"`ind_ylabels' `ypos' "`lbl_`ind_name''" "'
	}

	** Coefficient-zone layers. rcaps only if we have CIs.
	local coef_layers `""'
	if `has_ci' {
		local coef_layers `"`coef_layers' (rcap cilo_sig_notpref   cihi_sig_notpref   spec_rank, lc("`colsignotpref'")   lw(vthin))"'
		local coef_layers `"`coef_layers' (rcap cilo_insig_notpref cihi_insig_notpref spec_rank, lc("`colinsignotpref'") lw(vthin))"'
		local coef_layers `"`coef_layers' (rcap cilo_sig_pref      cihi_sig_pref      spec_rank, lc("`colsigpref'")      lw(thin))"'
		local coef_layers `"`coef_layers' (rcap cilo_insig_pref    cihi_insig_pref    spec_rank, lc("`colinsigpref'")    lw(thin))"'
	}
	** Scatter layers — these are the four legend entries (5..8 if CIs, 1..4 if not).
	local coef_layers `"`coef_layers' (scatter v_sig_notpref   spec_rank, mc("`colsignotpref'")   ms(O) msize(vsmall))"'
	local coef_layers `"`coef_layers' (scatter v_insig_notpref spec_rank, mc("`colinsignotpref'") ms(O) msize(vsmall))"'
	local coef_layers `"`coef_layers' (scatter v_sig_pref      spec_rank, mc("`colsigpref'")      ms(D) msize(small))"'
	local coef_layers `"`coef_layers' (scatter v_insig_pref    spec_rank, mc("`colinsigpref'")    ms(D) msize(small))"'

	** Legend points to the four scatter layers regardless of rcap presence.
	if `has_ci' {
		local leg_order `"5 "Sig. (p<0.05)" 6 "Insig." 7 "Sig., Preferred" 8 "Insig., Preferred""'
	}
	else {
		local leg_order `"1 "Sig. (p<0.05)" 2 "Insig." 3 "Sig., Preferred" 4 "Insig., Preferred""'
	}

	twoway `coef_layers' `ind_layers'                                                       ///
		, yline(`sep_y', lc(gs12) lp(solid) lw(vthin))                                     ///
		  yline(0, lc("`colzero'") lp(dash))                                               ///
		  ylabel(`tick_lo'(`tick_step')`tick_hi', labsize(vsmall) nogrid)                  ///
		  ylabel(`ind_ylabels', labsize(vsmall) angle(0) notick nogrid add)                ///
		  legend(order(`leg_order') rows(1) pos(6) size(vsmall))                           ///
		  ytitle(`"`ytitle'"', size(vsmall))                                               ///
		  xtitle("Specification (ranked by estimate)", size(vsmall))                       ///
		  xlabel(none)                                                                      ///
		  xscale(range(0.5 `=`n_specs'+0.5'))                                              ///
		  graphregion(color(white))                                                        ///
		  ysize(5) xsize(8)

	graph export "`file'.pdf", replace
	graph export "`file'.png", as(png) width(2400) replace
	dis as text "  Wrote `file'.{pdf,png} (`n_specs' specs, has_ci=`has_ci')"

	restore
end


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

** ------------------------------------------------------------------
** Optional bootstrap-CI merge (Phase B5).
**
** Joins bootstrap_cis.dta onto the highlighted-spec subset by the
** common keys. After the merge, the dataset has both point-estimate
** columns (tau, beta_kleven, stock_elast_*) AND CI columns
** (tau_lo, flow_semi_lo, stock_total_common_lo, ...). Note the naming
** asymmetry: spec_results stores Kleven semi-ε as `beta_kleven`
** while bootstrap_cis stores it as `flow_semi` (matching what
** compute_spec_elasticities returns as r(beta)). The rendering code
** below knows the mapping; we don't rename either side.
**
** After merge, pre-compute `<var>_ci_str` columns once so each of the
** six LaTeX tables can pull them via [i] indexing in its render loop
** without redoing the formatting.
** ------------------------------------------------------------------
if ${show_bootstrap_cis} == 1 {
	merge m:1 sample_data sample migration outstate controls exclusion ///
		using "${results}bootstrap/bootstrap_cis.dta", ///
		keep(master match) ///
		keepusing(*_lo *_hi *_median *_n) nogen
	dis "Merged bootstrap CIs onto " _N " rows."

	** Pre-compute CI strings for every CI-relevant column. Some specs
	** legitimately have missing CIs (stock_* on in/out specs, etc.) —
	** those get an empty string and the render loop blanks that cell.
	foreach v in tau flow_semi flow_semi_shs flow_e flow_e_shs ///
		stock_total_common stock_total_common_shs ///
		stock_total_full   stock_total_full_shs ///
		stock_total_ann    stock_total_ann_shs ///
		stock_imp_common   stock_imp_common_shs ///
		stock_imp_full     stock_imp_full_shs ///
		stock_imp_ann      stock_imp_ann_shs ///
		pfa_loss state_loss {
		capture confirm variable `v'_lo
		if _rc == 0 {
			gen str30 `v'_ci_str = ""
			replace `v'_ci_str = ///
				"[" + strtrim(string(`v'_lo, "%9.3f")) + ", " + ///
					strtrim(string(`v'_hi, "%9.3f")) + "]" ///
				if !missing(`v'_lo) & !missing(`v'_hi)
		}
	}
}

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
	cap("Highlighted AGI Net-Migration Elasticities, PFA-Only Sensitivity") ///
	lbl("tab:elasticities_pfa_only") cols("ll ccc")
file write `fh' "Data & Sample & $\hat{\tau}$ (pp) & Semi-$\varepsilon$ & Stock $\varepsilon$ \\" _n
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
	if ${show_bootstrap_cis} == 1 {
		local tau_ci   = tau_ci_str[`i']
		local beta_ci  = flow_semi_ci_str[`i']
		local stock_ci = stock_total_common_ci_str[`i']
		file write `fh' " & & `tau_ci' & `beta_ci' & `stock_ci' \\" _n
	}
	else {
		file write `fh' " & & `se_val' & `b_se' & \\" _n
	}
}

elast_tex_notes_open, handle(`fh')
file write `fh' "PFA-only sensitivity counterpart to Table~\ref{tab:elasticities}. " _n
file write `fh' "Point estimates of $\hat{\tau}$ are unchanged; only the denominator differs. " _n
file write `fh' "Here $\tau_\text{total}$ excludes the Metro SHS contribution (average total rate on impacted filers: `total_pct'\%). " _n
file write `fh' "Because the denominator $|\Delta\ln(1-\tau_\text{total})|$ is smaller without SHS, $|\beta|$ and stock $\varepsilon$ are correspondingly larger than in Table~\ref{tab:elasticities}. " _n
file write `fh' "See Appendix~\ref{sec:appb_elast} for formulas. " _n
file write `fh' "Bracketed values are 95\% donor-cluster bootstrap percentile CIs. " _n
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
	if ${show_bootstrap_cis} == 1 {
		** dln_ntr is a fixed denominator (no CI) — blank that column.
		local tau_ci   = tau_ci_str[`i']
		local beta_ci  = flow_semi_ci_str[`i']
		local sc_ci    = stock_total_common_ci_str[`i']
		local sf_ci    = stock_total_full_ci_str[`i']
		local sa_ci    = stock_total_ann_ci_str[`i']
		file write `fh2' " & & `tau_ci' & & `beta_ci' & `sc_ci' & `sf_ci' & `sa_ci' \\" _n
	}
}

elast_tex_notes_open, handle(`fh2')
file write `fh2' "$\hat{\tau}$ is the SDID coefficient on the AGI net-migration rate, reported in percentage points. " _n
file write `fh2' "$\Delta \ln(1-t)$ is the change in the log after-tax rate used in the elasticity denominator; for ACS College, the subgroup-specific after-tax change is used. " _n
file write `fh2' "Flow semi-elasticity is $\beta = (\hat{\tau}/100)/\Delta\ln(1-t)$. " _n
file write `fh2' "Stock elasticities are calculated on the total AGI base from cumulated net-migration event-study effects: $\varepsilon_{\text{stock},H} = \Delta\ln S_H / \Delta\ln(1-t)$. " _n
file write `fh2' "Common uses the 2021--2022 IRS-ACS overlap window, Full uses all available post years, and Annualized equals Full divided by the number of post years. " _n
file write `fh2' "Positive stock elasticities indicate that the AGI stock shrinks when the tax rate rises because the after-tax rate falls. " _n
elast_tex_notes_inference, handle(`fh2') stock
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

gen str20 migr_label = ""
replace migr_label = "In" if migration == "in"
replace migr_label = "Out" if migration == "out"

tempname fh
file open `fh' using "${results}elasticities/tbl_elasticities_inout.tex", write replace

elast_tex_open, handle(`fh') ///
	cap("Highlighted Gross AGI Migration Elasticities, PFA-Only Sensitivity") ///
	lbl("tab:elasticities_inout_pfa_only") cols("lll cc") ///
	fontsize("footnotesize")
file write `fh' "Data & Sample & Dir.\ & $\hat{\tau}$ (pp) & Semi-$\varepsilon$ \\" _n
file write `fh' "\midrule" _n

sort data_type sample migration

file write `fh' "\addlinespace" _n
file write `fh' "\multicolumn{5}{l}{\textit{Panel A: Out-Migration}} \\" _n
file write `fh' "\addlinespace" _n
elast_inout_panel, handle(`fh') direction("out")

file write `fh' "\addlinespace[0.75em]" _n
file write `fh' "\midrule" _n
file write `fh' "\addlinespace" _n
file write `fh' "\multicolumn{5}{l}{\textit{Panel B: In-Migration}} \\" _n
file write `fh' "\addlinespace" _n
elast_inout_panel, handle(`fh') direction("in")

elast_tex_notes_open, handle(`fh')
file write `fh' "PFA-only sensitivity counterpart to Table~\ref{tab:elasticities_inout}. " _n
file write `fh' "Point estimates of $\hat{\tau}$ are unchanged; only the denominator differs (excludes Metro SHS; average total rate on impacted filers: `total_pct'\%). " _n
file write `fh' "Because the denominator is smaller without SHS, $|\beta|$ is correspondingly larger than in Table~\ref{tab:elasticities_inout}. " _n
file write `fh' "See Appendix~\ref{sec:appb_elast} for formulas. " _n
file write `fh' "Bracketed values are 95\% donor-cluster bootstrap percentile CIs. " _n
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
	cap("Highlighted AGI Net-Migration Elasticities") ///
	lbl("tab:elasticities") cols("ll ccc")
file write `fh_shs' "Data & Sample & $\hat{\tau}$ (pp) & Semi-$\varepsilon$ & Stock $\varepsilon$ \\" _n
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
	if ${show_bootstrap_cis} == 1 {
		** SHS variant: beta + stock columns use _shs CI variables.
		local tau_ci   = tau_ci_str[`i']
		local beta_ci  = flow_semi_shs_ci_str[`i']
		local stock_ci = stock_total_common_shs_ci_str[`i']
		file write `fh_shs' " & & `tau_ci' & `beta_ci' & `stock_ci' \\" _n
	}
	else {
		file write `fh_shs' " & & `se_val' & `b_se' & \\" _n
	}
}

elast_tex_notes_open, handle(`fh_shs')
file write `fh_shs' "$\hat{\tau}$ is the SDID coefficient on the AGI net migration rate (percentage points). " _n
file write `fh_shs' "Semi-elasticity $\beta = (\hat{\tau}/100)/\Delta\ln(1-\tau_\text{total})$ and stock elasticity $\varepsilon_{\text{stock},H} = \Delta\ln S_H / \Delta\ln(1-\tau_\text{total})$, where $S_H$ is the cumulative AGI stock at horizon $H$, are computed against the joint PFA + Metro SHS rate change; both took effect January~1, 2021. " _n
file write `fh_shs' "Average total marginal rate on impacted filers: `total_shs_pct'\% (federal + Oregon state + FICA-employee + PFA + SHS). " _n
file write `fh_shs' "The stock object is the 2-year cumulative AGI-stock change on the total AGI base; see Appendix~\ref{sec:appb_elast} for the recursion and Appendix Table~\ref{tab:elasticities_pfa_only} for a PFA-only-denominator sensitivity. " _n
file write `fh_shs' "Gross out- and in-migration semi-elasticities are in Appendix Table~\ref{tab:elasticities_inout}. " _n
file write `fh_shs' "Positive $\beta$ or stock $\varepsilon$ indicates AGI inflow / stock shrinks when the after-tax rate falls. " _n
file write `fh_shs' "Bracketed values are 95\% donor-cluster bootstrap percentile CIs (treats tax parameters and microsimulation denominators as fixed). " _n
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
	if ${show_bootstrap_cis} == 1 {
		** SHS variant: beta + stock columns use _shs CI variables.
		** dln_ntr is a fixed denominator (no CI) — blank that column.
		local tau_ci   = tau_ci_str[`i']
		local beta_ci  = flow_semi_shs_ci_str[`i']
		local sc_ci    = stock_total_common_shs_ci_str[`i']
		local sf_ci    = stock_total_full_shs_ci_str[`i']
		local sa_ci    = stock_total_ann_shs_ci_str[`i']
		file write `fh2_shs' " & & `tau_ci' & & `beta_ci' & `sc_ci' & `sf_ci' & `sa_ci' \\" _n
	}
}

elast_tex_notes_open, handle(`fh2_shs')
file write `fh2_shs' "SHS-inclusive version of Table~\ref{tab:elasticities_stock_compare}. " _n
file write `fh2_shs' "$\Delta\ln(1-t)$ and all elasticity columns use the combined PFA + Metro SHS denominator. " _n
file write `fh2_shs' "Average effective SHS rate on impacted filers: `shs_pct'\%. " _n
file write `fh2_shs' "Interpretation and sign conventions follow Table~\ref{tab:elasticities_stock_compare}." _n
elast_tex_notes_inference, handle(`fh2_shs') stock
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

gen str20 migr_label = ""
replace migr_label = "In" if migration == "in"
replace migr_label = "Out" if migration == "out"

tempname fh_shs_io
file open `fh_shs_io' using "${results}elasticities/tbl_elasticities_inout_shs.tex", ///
	write replace

elast_tex_open, handle(`fh_shs_io') ///
	cap("Highlighted Gross AGI Migration Elasticities") ///
	lbl("tab:elasticities_inout") cols("lll cc") ///
	fontsize("footnotesize")
file write `fh_shs_io' "Data & Sample & Dir.\ & $\hat{\tau}$ (pp) & Semi-$\varepsilon$ \\" _n
file write `fh_shs_io' "\midrule" _n

sort data_type sample migration

file write `fh_shs_io' "\addlinespace" _n
file write `fh_shs_io' "\multicolumn{5}{l}{\textit{Panel A: Out-Migration}} \\" _n
file write `fh_shs_io' "\addlinespace" _n
elast_inout_panel, handle(`fh_shs_io') direction("out") ///
	betacivar(flow_semi_shs_ci_str)

file write `fh_shs_io' "\addlinespace[0.75em]" _n
file write `fh_shs_io' "\midrule" _n
file write `fh_shs_io' "\addlinespace" _n
file write `fh_shs_io' "\multicolumn{5}{l}{\textit{Panel B: In-Migration}} \\" _n
file write `fh_shs_io' "\addlinespace" _n
elast_inout_panel, handle(`fh_shs_io') direction("in") ///
	betacivar(flow_semi_shs_ci_str)

elast_tex_notes_open, handle(`fh_shs_io')
file write `fh_shs_io' "$\hat{\tau}$ is the SDID coefficient on the AGI gross out- (Panel~A) or in- (Panel~B) migration rate (percentage points). " _n
file write `fh_shs_io' "Semi-elasticity $\beta = (\hat{\tau}/100)/\Delta\ln(1-\tau_\text{total})$ is computed against the joint PFA + Metro SHS rate change (average total marginal rate on impacted filers: `total_shs_pct'\%); see Appendix~\ref{sec:appb_elast} for the formula and Table~\ref{tab:elasticities} for the corresponding net-migration object. " _n
file write `fh_shs_io' "A PFA-only-denominator sensitivity is in Appendix Table~\ref{tab:elasticities_inout_pfa_only}. " _n
file write `fh_shs_io' "Sign convention: negative $\beta$ for out-migration indicates a larger outflow when the after-tax rate falls; positive $\beta$ for in-migration indicates a smaller inflow. " _n
file write `fh_shs_io' "Bracketed values are 95\% donor-cluster bootstrap percentile CIs. " _n
elast_tex_close, handle(`fh_shs_io')

file close `fh_shs_io'
restore

** =========================================================================
** (e) SDID coefficients table for the four highlighted specifications
**     (item 16 of the May 2026 revision TODO). Produces an appendix-style
**     table with point estimate, placebo-inference SE, and N counties for
**     each (data × sample × direction) cell.
** =========================================================================
preserve
use "${results}sdid/sdid_results.dta", clear

** Restrict to highlighted: agi outcomes, controls=1, exclusion=1, county-level
keep if controls == 1 & exclusion == 1
keep if inlist(sample_data, "irs_full_16_22", "acs_16_24_col")
keep if inlist(sample, "sample_all", "sample_stringency")
keep if regexm(outcome, "^agi_(net|in|out)_rate_(irs|acs2)$")

** Tag direction
gen str10 direction = "net" if regexm(outcome, "_net_")
replace direction = "in"     if regexm(outcome, "_in_")
replace direction = "out"    if regexm(outcome, "_out_")

** Tag data label and sample label
gen str20 data_label = "IRS"          if sample_data == "irs_full_16_22"
replace data_label   = "ACS College"  if sample_data == "acs_16_24_col"
gen str20 sample_label = "All counties"  if sample == "sample_all"
replace sample_label   = "Stringency"    if sample == "sample_stringency"

** Sort and pre-format
sort data_label sample_label direction
gen str20 tau_str = string(tau, "%9.3f")
gen str20 se_str  = "(" + string(se, "%9.3f") + ")"
gen str20 n_str   = string(n_counties, "%9.0fc")

tempname fh_sdid
file open `fh_sdid' using "${results}sdid/tab_sdid_preferred.tex", write replace

file write `fh_sdid' "\begin{table}[htbp]" _n
file write `fh_sdid' "\centering" _n
file write `fh_sdid' "\begin{threeparttable}" _n
file write `fh_sdid' "\footnotesize" _n
file write `fh_sdid' `"\caption{SDID Treatment-Effect Estimates: Highlighted Specifications}"' _n
file write `fh_sdid' "\label{tab:sdid_preferred}" _n
file write `fh_sdid' `"\begin{tabular}{l l c c c c}"' _n
file write `fh_sdid' "\toprule" _n
file write `fh_sdid' " & & Out-migration & In-migration & Net in-migration & N \\" _n
file write `fh_sdid' " Data & Sample & $\hat{\tau}$ (pp) & $\hat{\tau}$ (pp) & $\hat{\tau}$ (pp) & counties \\" _n
file write `fh_sdid' "\midrule" _n

** Iterate over the 4 (data, sample) cells and write 2 rows each:
** row 1: τ̂ for out / in / net + N
** row 2: (SE) for out / in / net (blank N cell)
foreach dt in "IRS" "ACS College" {
	foreach smp in "All counties" "Stringency" {
		** Find the three direction values for this cell
		local tau_out = ""
		local se_out  = ""
		local tau_in  = ""
		local se_in   = ""
		local tau_net = ""
		local se_net  = ""
		local n_val   = ""
		count if data_label == "`dt'" & sample_label == "`smp'"
		if r(N) > 0 {
			qui sum tau if data_label == "`dt'" & sample_label == "`smp'" & direction == "out", meanonly
			if r(N) > 0 {
				local i = 0
				forvalues k = 1/`=_N' {
					if data_label[`k'] == "`dt'" & sample_label[`k'] == "`smp'" & direction[`k'] == "out" {
						local tau_out = tau_str[`k']
						local se_out  = se_str[`k']
						local n_val   = n_str[`k']
					}
					if data_label[`k'] == "`dt'" & sample_label[`k'] == "`smp'" & direction[`k'] == "in" {
						local tau_in  = tau_str[`k']
						local se_in   = se_str[`k']
					}
					if data_label[`k'] == "`dt'" & sample_label[`k'] == "`smp'" & direction[`k'] == "net" {
						local tau_net = tau_str[`k']
						local se_net  = se_str[`k']
					}
				}
			}
		}
		file write `fh_sdid' "`dt' & `smp' & `tau_out' & `tau_in' & `tau_net' & `n_val' \\" _n
		file write `fh_sdid' " & & `se_out' & `se_in' & `se_net' & \\" _n
	}
	file write `fh_sdid' "\addlinespace" _n
}

file write `fh_sdid' "\bottomrule" _n
file write `fh_sdid' "\end{tabular}" _n
file write `fh_sdid' "\begin{tablenotes}" _n
file write `fh_sdid' "\small" _n
file write `fh_sdid' "\item \textit{Notes:} SDID treatment-effect estimates ($\hat{\tau}$, in percentage points) for the four highlighted specifications: IRS full sample (2016--2022) and ACS college sample (2016--2024), each with the all-counties donor pool and the stringency-matched donor pool. All specifications include time-varying covariates and exclude 2020. Standard errors in parentheses are from SDID placebo inference. \textit{N counties} is the number of donor counties in the synthetic-control pool plus Multnomah." _n
file write `fh_sdid' "\end{tablenotes}" _n
file write `fh_sdid' "\end{threeparttable}" _n
file write `fh_sdid' "\end{table}" _n

file close `fh_sdid'
restore
dis "Wrote: ${results}sdid/tab_sdid_preferred.tex"

** ---- Copy LaTeX tables to Overleaf ----
if "${overleaf}" == "1" {
	foreach f in tbl_elasticities tbl_elasticities_stock_compare ///
		tbl_elasticities_inout tbl_elasticities_shs ///
		tbl_elasticities_stock_compare_shs tbl_elasticities_inout_shs {
		copy "${results}elasticities/`f'.tex" "${ol_tab}`f'.tex", replace
	}
	capture confirm file "${results}sdid/tab_sdid_preferred.tex"
	if _rc == 0 {
		copy "${results}sdid/tab_sdid_preferred.tex" "${ol_tab}tab_sdid_preferred.tex", replace
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
gen double result_stock_elast_tot_com_shs = stock_elast_total_common_shs
gen double result_stock_elast_tot_full_shs = stock_elast_total_full_shs
gen double result_stock_elast_tot_ann_shs = stock_elast_total_ann_shs
gen double result_stock_elast_imp_com_shs = stock_elast_imp_common_shs
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
	result_stock_elast_tot_com_shs result_stock_elast_tot_full_shs ///
	result_stock_elast_tot_ann_shs ///
	result_stock_elast_imp_com_shs result_stock_elast_imp_full_shs ///
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
post `guideh' ("recalc_components") ("input_tau_se_pp") ("Placebo-inference standard error for the SDID treatment effect, in percentage points. This is sdid's vce(placebo) SE, not the donor-cluster bootstrap CI exported in bootstrap_cis.")
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
post `guideh' ("recalc_components") ("result_net_of_tax_semi_se") ("Standard error for the net-of-tax semi-elasticity. Propagated from the SDID treatment-effect placebo SE; treats revenue and tax parameters as fixed. Donor-cluster bootstrap percentile CIs are exported separately to bootstrap_cis.")
post `guideh' ("recalc_components") ("result_gross_flow_elast") ("Gross-flow elasticity for in- or out-migration, using the pre-period gross flow mean as the base.")
post `guideh' ("recalc_components") ("result_gross_flow_elast_se") ("Standard error for the gross-flow elasticity. Propagated from the SDID treatment-effect placebo SE; treats revenue and tax parameters as fixed.")
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
post `guideh' ("recalc_components") ("result_net_of_tax_semi_se_shs") ("Standard error of the SHS-inclusive semi-elasticity. Propagated from the SDID treatment-effect placebo SE; treats SHS rate and other revenue parameters as fixed.")
post `guideh' ("recalc_components") ("result_gross_flow_elast_shs") ("Gross-flow elasticity (in/out) with the SHS-inclusive denominator.")
post `guideh' ("recalc_components") ("result_gross_flow_elast_se_shs") ("Standard error of the SHS-inclusive gross-flow elasticity. Propagated from the SDID treatment-effect placebo SE; treats revenue parameters as fixed.")
post `guideh' ("recalc_components") ("result_stock_elast_tot_com_shs") ("Stock elasticity on the total AGI base, 2021–2022 window, SHS-inclusive.")
post `guideh' ("recalc_components") ("result_stock_elast_tot_full_shs") ("Stock elasticity on the total AGI base, full post horizon, SHS-inclusive.")
post `guideh' ("recalc_components") ("result_stock_elast_tot_ann_shs") ("Annualized stock elasticity on the total AGI base, SHS-inclusive.")
post `guideh' ("recalc_components") ("result_stock_elast_imp_com_shs") ("Stock elasticity on the impacted AGI base, 2021–2022 window, SHS-inclusive.")
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

** Bootstrap CI sheet (Phase B5). One row per highlighted spec; for each
** CI variable v, columns are v_median, v_lo, v_hi, v_n. Sheet only
** populated when ${show_bootstrap_cis} == 1; the variable_guide
** entries below stay regardless so the workbook structure is stable.
post `guideh' ("bootstrap_cis") ("spec_id") ("Highlighted-spec identifier (1..24); matches the spec grid in 02_bootstrap.do.")
post `guideh' ("bootstrap_cis") ("sample_data") ("Source panel block: irs_full_16_22, irs_outstate_full_16_22, acs_16_24_col, or acs_outstate_16_24_col.")
post `guideh' ("bootstrap_cis") ("sample") ("Donor-pool sample: sample_all or sample_stringency.")
post `guideh' ("bootstrap_cis") ("migration") ("Migration direction: net, in, or out.")
post `guideh' ("bootstrap_cis") ("outstate") ("1 if the spec uses out-of-state migration data, 0 otherwise.")
post `guideh' ("bootstrap_cis") ("data_type") ("Presentation label: IRS, IRS (Out-of-State), ACS College, or ACS College (Out-of-State).")
post `guideh' ("bootstrap_cis") ("controls") ("Always 1 in the bootstrap subset (covariates included).")
post `guideh' ("bootstrap_cis") ("exclusion") ("Always 1 in the bootstrap subset (year 2020 dropped from estimation).")
post `guideh' ("bootstrap_cis") ("tau_median") ("Bootstrap median of the SDID coefficient τ̂ on the migration rate (pp).")
post `guideh' ("bootstrap_cis") ("tau_lo") ("Lower percentile of τ̂ across donor-cluster bootstrap reps.")
post `guideh' ("bootstrap_cis") ("tau_hi") ("Upper percentile of τ̂ across donor-cluster bootstrap reps.")
post `guideh' ("bootstrap_cis") ("tau_n") ("Number of non-missing bootstrap reps used for τ̂.")
post `guideh' ("bootstrap_cis") ("flow_semi_*") ("Median/lo/hi/n for the Kleven semi-elasticity β. Suffix _shs is the SHS-inclusive variant.")
post `guideh' ("bootstrap_cis") ("flow_e_*") ("Median/lo/hi/n for the gross flow elasticity (in/out only). Suffix _shs is the SHS-inclusive variant.")
post `guideh' ("bootstrap_cis") ("stock_total_common_*") ("Median/lo/hi/n for the total-AGI stock elasticity over the common 2021–2022 window (net specs only). Suffix _shs is SHS-inclusive.")
post `guideh' ("bootstrap_cis") ("stock_total_full_*") ("Total-AGI stock elasticity over the full post horizon. Suffix _shs is SHS-inclusive.")
post `guideh' ("bootstrap_cis") ("stock_total_ann_*") ("Annualized total-AGI stock elasticity. Suffix _shs is SHS-inclusive.")
post `guideh' ("bootstrap_cis") ("stock_imp_common_*") ("Impacted-AGI stock elasticity over the common 2021–2022 window. Suffix _shs is SHS-inclusive.")
post `guideh' ("bootstrap_cis") ("stock_imp_full_*") ("Impacted-AGI stock elasticity over the full post horizon. Suffix _shs is SHS-inclusive.")
post `guideh' ("bootstrap_cis") ("stock_imp_ann_*") ("Annualized impacted-AGI stock elasticity. Suffix _shs is SHS-inclusive.")
post `guideh' ("bootstrap_cis") ("pfa_loss_*") ("Implied PFA revenue loss in $M (in-state net specs only).")
post `guideh' ("bootstrap_cis") ("state_loss_*") ("Implied Oregon revenue loss attributable to Multnomah out-migration, in $M (out-of-state net specs only). Scaled to Multnomah's IRS AGI share of statewide individual income tax.")

postclose `guideh'
use `guide', clear
export excel using "${results}elasticities/tbl_elasticities.xlsx", ///
	sheet("variable_guide") firstrow(variables) sheetreplace
restore

** ------------------------------------------------------------------
** New sheet: bootstrap_cis (Phase B5).
** Mirrors results/bootstrap/bootstrap_cis.dta. Empty when
** ${show_bootstrap_cis} == 0 — we still write a 1-row placeholder so
** the sheet exists in the workbook contract regardless of flag state.
** ------------------------------------------------------------------
preserve
if ${show_bootstrap_cis} == 1 {
	use "${results}bootstrap/bootstrap_cis.dta", clear
}
else {
	clear
	set obs 1
	gen str40 _placeholder = "Bootstrap CIs not generated this run."
	gen str120 _hint = "Re-run with: global show_bootstrap_cis = 1; do ${code}02_tables_figures.do"
}
export excel using "${results}elasticities/tbl_elasticities.xlsx", ///
	sheet("bootstrap_cis") firstrow(variables) sheetreplace
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

** Indicator templates — see elast_speccurve_plot header for the dictionary.
** β and stock-ε figures are not pre-filtered by outstate, so they show all
** six data_type rows. PFA / state revenue figures are pre-filtered to one
** outstate value each; the call sites use the leaner instate / outstate
** variants below in Section 4.
local indic_universal `"spec_irs spec_irs_outstate spec_acs_all spec_acs_all_outstate spec_acs_col spec_acs_col_outstate spec_all spec_stringency spec_urban95 spec_demog spec_covid spec_16_22 spec_16_24 spec_covars spec_excl2020"'

** ---- β spec curves (PFA + SHS) for each migration direction ----
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

	elast_speccurve_plot, var(beta_kleven) ///
		ytitle(`"Migration Semi-Elasticity ({&beta})"') ///
		file("${results}elasticities/fig_speccurve_elast_beta_`migr'") ///
		indicators("`indic_universal'") ///
		lovar(flow_semi_lo) hivar(flow_semi_hi)

	dis as text "Kleven semi-elasticity beta distribution (`migr'), +SHS:"
	summ beta_kleven_shs, detail
	elast_speccurve_plot, var(beta_kleven_shs) ///
		ytitle(`"Migration Semi-Elasticity ({&beta}, PFA+SHS)"') ///
		file("${results}elasticities/fig_speccurve_elast_beta_`migr'_shs") ///
		indicators("`indic_universal'") ///
		lovar(flow_semi_shs_lo) hivar(flow_semi_shs_hi)

	** ---- Flow elasticity spec curve (item 10) ----
	** flow_e is undefined for net migration (rate ~ 0 in pre period); skip
	** that direction. For in/out, produce a flow-elasticity distribution
	** figure parallel to the semi-elasticity / stock-elasticity ones.
	if "`migr'" != "net" {
		qui count if !missing(flow_e)
		if r(N) > 0 {
			dis as text "Flow elasticity distribution (`migr'):"
			summ flow_e if !missing(flow_e), detail
			elast_speccurve_plot, var(flow_e) ///
				ytitle(`"Migration Flow Elasticity"') ///
				file("${results}elasticities/fig_speccurve_elast_flow_`migr'") ///
				indicators("`indic_universal'") ///
				lovar(flow_e_lo) hivar(flow_e_hi)
		}
		qui count if !missing(flow_e_shs)
		if r(N) > 0 {
			dis as text "Flow elasticity distribution (`migr'), +SHS:"
			summ flow_e_shs if !missing(flow_e_shs), detail
			elast_speccurve_plot, var(flow_e_shs) ///
				ytitle(`"Migration Flow Elasticity (PFA+SHS)"') ///
				file("${results}elasticities/fig_speccurve_elast_flow_`migr'_shs") ///
				indicators("`indic_universal'") ///
				lovar(flow_e_shs_lo) hivar(flow_e_shs_hi)
		}
	}

	restore
}

** ---- Stock-ε spec curves: net migration (PFA + SHS) ----
preserve
keep if migration == "net"

qui count if !missing(stock_elast_total_common)
if r(N) > 0 {
	dis ""
	dis as text "Stock elasticity distribution (net, 2021-2022 window):"
	summ stock_elast_total_common if !missing(stock_elast_total_common), detail

	elast_speccurve_plot, var(stock_elast_total_common) ///
		ytitle(`"Migration Stock Elasticity"') ///
		file("${results}elasticities/fig_speccurve_elast_stock") ///
		indicators("`indic_universal'") ///
		lovar(stock_total_common_lo) hivar(stock_total_common_hi)
}

qui count if !missing(stock_elast_total_common_shs)
if r(N) > 0 {
	dis as text "Stock elasticity distribution (net, 2021-2022 window, +SHS):"
	summ stock_elast_total_common_shs if !missing(stock_elast_total_common_shs), detail

	elast_speccurve_plot, var(stock_elast_total_common_shs) ///
		ytitle(`"Migration Stock Elasticity (PFA+SHS)"') ///
		file("${results}elasticities/fig_speccurve_elast_stock_shs") ///
		indicators("`indic_universal'") ///
		lovar(stock_total_common_shs_lo) hivar(stock_total_common_shs_hi)
}
restore

** ---- Stock-ε spec curves: in / out migration (item 10) ----
foreach migr in in out {
	preserve
	keep if migration == "`migr'"
	qui count if !missing(stock_elast_total_common)
	if r(N) > 0 {
		dis as text "Stock elasticity distribution (`migr'):"
		summ stock_elast_total_common if !missing(stock_elast_total_common), detail
		elast_speccurve_plot, var(stock_elast_total_common) ///
			ytitle(`"Migration Stock Elasticity"') ///
			file("${results}elasticities/fig_speccurve_elast_stock_`migr'") ///
			indicators("`indic_universal'") ///
			lovar(stock_total_common_lo) hivar(stock_total_common_hi)
	}
	qui count if !missing(stock_elast_total_common_shs)
	if r(N) > 0 {
		dis as text "Stock elasticity distribution (`migr', +SHS):"
		summ stock_elast_total_common_shs if !missing(stock_elast_total_common_shs), detail
		elast_speccurve_plot, var(stock_elast_total_common_shs) ///
			ytitle(`"Migration Stock Elasticity (PFA+SHS)"') ///
			file("${results}elasticities/fig_speccurve_elast_stock_`migr'_shs") ///
			indicators("`indic_universal'") ///
			lovar(stock_total_common_shs_lo) hivar(stock_total_common_shs_hi)
	}
	restore
}

** Overleaf copy for elasticity spec-curve figures. Preserve the legacy
** fig_elasticity_dist_net.pdf alias (now sourced from the new spec-curve
** β-net file) so manuscript \includegraphics paths that still reference the
** old name continue to resolve.
if "${overleaf}" == "1" {
	foreach base in ///
		fig_speccurve_elast_beta_net fig_speccurve_elast_beta_in fig_speccurve_elast_beta_out ///
		fig_speccurve_elast_beta_net_shs fig_speccurve_elast_beta_in_shs fig_speccurve_elast_beta_out_shs ///
		fig_speccurve_elast_flow_in fig_speccurve_elast_flow_out ///
		fig_speccurve_elast_flow_in_shs fig_speccurve_elast_flow_out_shs ///
		fig_speccurve_elast_stock fig_speccurve_elast_stock_shs ///
		fig_speccurve_elast_stock_in fig_speccurve_elast_stock_out ///
		fig_speccurve_elast_stock_in_shs fig_speccurve_elast_stock_out_shs {
		capture confirm file "${results}elasticities/`base'.pdf"
		if _rc == 0 {
			copy "${results}elasticities/`base'.pdf" ///
				"${ol_fig}`base'.pdf", replace
		}
	}
	capture confirm file "${results}elasticities/fig_speccurve_elast_beta_net.pdf"
	if _rc == 0 {
		copy "${results}elasticities/fig_speccurve_elast_beta_net.pdf" ///
			"${ol_fig}fig_elasticity_dist_net.pdf", replace
	}
}

********************************************************************************
** SECTION 4: Revenue-distribution figures
********************************************************************************
** Restored here from the pre-A2 02_revenue.do §12. pfa_loss (for net-domestic
** specs) and state_loss (for net-outstate specs) are already populated in
** spec_results.dta by 02_post_spec.do calling compute_spec_revenue — the job
** of this section is just to render them as spec curves via
** elast_speccurve_plot, with bootstrap CI whiskers when available.

dis ""
dis "=============================================="
dis "Section 4: Revenue-loss distribution figures"
dis "=============================================="

** Indicator templates for revenue spec curves. PFA loss is defined only
** for outstate==0 (net-domestic); the indicator template drops the
** outstate data_type rows. State loss is the mirror — outstate==1 only.
local indic_instate  `"spec_irs spec_acs_all spec_acs_col spec_all spec_stringency spec_urban95 spec_demog spec_covid spec_16_22 spec_16_24 spec_covars spec_excl2020"'
local indic_outstate `"spec_irs_outstate spec_acs_all_outstate spec_acs_col_outstate spec_all spec_stringency spec_urban95 spec_demog spec_covid spec_16_22 spec_16_24 spec_covars spec_excl2020"'

** PFA: net-domestic specs, pfa_loss column
preserve
keep if migration == "net" & outstate == 0 & !missing(pfa_loss)

qui count
if r(N) > 0 {
	dis ""
	dis "PFA implied loss distribution ($ millions), " _N " specs:"
	summ pfa_loss, detail

	elast_speccurve_plot, var(pfa_loss) ///
		ytitle("Implied PFA Revenue Loss ($ millions)") ///
		file("${results}revenue/fig_speccurve_revenue_pfa") ///
		indicators("`indic_instate'") ///
		lovar(pfa_loss_lo) hivar(pfa_loss_hi)
}
else {
	dis as text "No pfa_loss values available — skipping fig_speccurve_revenue_pfa."
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

	elast_speccurve_plot, var(state_loss) ///
		ytitle("Implied Oregon Revenue Loss from Multnomah Out-Migration ($ millions)") ///
		file("${results}revenue/fig_speccurve_revenue_oregon") ///
		indicators("`indic_outstate'") ///
		lovar(state_loss_lo) hivar(state_loss_hi)
}
else {
	dis as text "No state_loss values available — skipping fig_speccurve_revenue_oregon."
}
restore

** Overleaf copy for revenue spec-curve figures.
if "${overleaf}" == "1" {
	foreach base in fig_speccurve_revenue_pfa fig_speccurve_revenue_oregon {
		capture confirm file "${results}revenue/`base'.pdf"
		if _rc == 0 {
			copy "${results}revenue/`base'.pdf" ///
				"${ol_fig}`base'.pdf", replace
		}
	}
}

********************************************************************************
** SECTION 5: Preferred-spec event-study overlays
********************************************************************************
** Reads sdid_event_results.dta (preferred==1 rows) and writes 18 overlay
** figures into ${results}sdid/preferred_overlays/. Two complementary views:
**   Set 1 (12 figs): donor-pool comparison — sample_all vs
**     sample_stringency, one figure per (sample_data × migration).
**   Set 2 (6 figs): donor-pool × dataset — IRS vs ACS College ×
**     sample_all vs sample_stringency, one figure per (scope × migration).
** In-state and out-of-state stay on separate figures.

dis ""
dis "=============================================="
dis "Section 5: Preferred-spec event-study overlays"
dis "=============================================="

** ${clean_figs} toggle (item 4 of May 2026 paper revision TODO).
** When 1, in-figure titles are suppressed so that the LaTeX \caption{}
** alone provides the title. Default 0 keeps the existing titled output.
if "${clean_figs}" == "" global clean_figs = 0

capture mkdir "${results}sdid/preferred_overlays"

capture confirm file "${results}sdid/sdid_event_results.dta"
if _rc != 0 {
	dis as error "  sdid_event_results.dta not found — skipping overlay figures."
}
else {

** Migration-direction display labels.
local lbl_migr_net `"Net AGI Migration"'
local lbl_migr_in  `"AGI In-Migration"'
local lbl_migr_out `"AGI Out-Migration"'

** sample_data display labels.
local lbl_sd_irs_full_16_22          `"IRS"'
local lbl_sd_acs_16_24_col           `"ACS College"'
local lbl_sd_irs_outstate_full_16_22 `"IRS (Out-of-State)"'
local lbl_sd_acs_outstate_16_24_col  `"ACS College (Out-of-State)"'

** ----------------------------------------------------------------
** Set 1 — donor-pool overlay (2 lines per figure; 12 figures)
** ----------------------------------------------------------------
** sample_all = vermillion (col_sig_pref); sample_stringency = sea
** (col_sig_notpref). x-offset ±0.10 to disambiguate same-year rcaps.

** Save current data to a tempfile (avoids nested-preserve r(621) inside loops).
tempfile orig_data
save `"`orig_data'"', replace

use "${results}sdid/sdid_event_results.dta", clear
keep if preferred == 1
keep if regexm(outcome, "^agi_")    // restrict to AGI outcomes; n1/n2 share preferred flag

** Short tag per sample_data for tidy filenames.
gen str40 sd_tag = ""
replace sd_tag = "irs"              if sample_data == "irs_full_16_22"
replace sd_tag = "acs_col"          if sample_data == "acs_16_24_col"
replace sd_tag = "irs_outstate"     if sample_data == "irs_outstate_full_16_22"
replace sd_tag = "acs_col_outstate" if sample_data == "acs_outstate_16_24_col"

foreach sd in irs_full_16_22 acs_16_24_col irs_outstate_full_16_22 acs_outstate_16_24_col {
	foreach migr in net in out {

		preserve
		keep if sample_data == "`sd'"
		keep if regexm(outcome, "_`migr'_rate_")
		keep if inlist(sample, "sample_all", "sample_stringency")

		qui count
		if r(N) == 0 {
			dis "  Set 1 skip — no rows for `sd' / `migr'."
			restore
			continue
		}

		local sdtag = sd_tag[1]
		local sd_label `"`lbl_sd_`sd''"'
		local migr_label `"`lbl_migr_`migr''"'

		** x-offset by sample.
		gen double event_year_off = event_year
		replace event_year_off = event_year_off - 0.10 if sample == "sample_all"
		replace event_year_off = event_year_off + 0.10 if sample == "sample_stringency"

		** Group-masked tau / lo / hi so each gets its own twoway layer.
		gen double tau_g1  = event_tau   if sample == "sample_all"
		gen double lo_g1   = event_ci_lo if sample == "sample_all"
		gen double hi_g1   = event_ci_hi if sample == "sample_all"
		gen double tau_g2  = event_tau   if sample == "sample_stringency"
		gen double lo_g2   = event_ci_lo if sample == "sample_stringency"
		gen double hi_g2   = event_ci_hi if sample == "sample_stringency"

		** Title suppressed when ${clean_figs} == 1 (paper version).
		local _title_opt `"title(`"`sd_label', `migr_label': Donor Pool Comparison"', size(medsmall))"'
		if ${clean_figs} == 1 local _title_opt ""

		twoway (rcap lo_g1 hi_g1 event_year_off, lc("${col_sig_pref}")    lw(medthin)) ///
		       (scatter tau_g1 event_year_off,   mc("${col_sig_pref}")    ms(O) msize(small)) ///
		       (rcap lo_g2 hi_g2 event_year_off, lc("${col_sig_notpref}") lw(medthin)) ///
		       (scatter tau_g2 event_year_off,   mc("${col_sig_notpref}") ms(O) msize(small)), ///
		    yline(0, lc("${col_zero}") lp(dash))                          ///
		    xline(2020.5, lc(black) lp(solid))                            ///
		    xlabel(2016(1)2024, labsize(small))                           ///
		    ylabel(, format(%9.1f) labsize(small))                        ///
		    legend(order(2 "All Counties" 4 "Stringency Match")           ///
		           rows(1) pos(6) size(small) region(lcolor(white)))      ///
		    `_title_opt'                                                  ///
		    ytitle(`"Event-study coefficient {&tau}{subscript:t} (pp)"', size(small)) ///
		    xtitle("Year", size(small))                                   ///
		    graphregion(color(white)) ysize(4) xsize(7)

		local fbase "${results}sdid/preferred_overlays/fig_overlay_donorpool_`sdtag'_`migr'_eventstudy"
		graph export "`fbase'.pdf", replace
		graph export "`fbase'.jpg", as(jpg) quality(100) replace
		dis as text "  Set 1: wrote `sdtag' / `migr'"

		restore
	} // END migr
} // END sd

** ----------------------------------------------------------------
** Set 2 — donor-pool × dataset overlay (4 lines per figure; 6 figures)
** ----------------------------------------------------------------
** Color mnemonic: warm = IRS, cool = ACS College, saturated = all,
** lighter = stringency. x-offsets -0.15 / -0.05 / +0.05 / +0.15.

use "${results}sdid/sdid_event_results.dta", clear
keep if preferred == 1
keep if regexm(outcome, "^agi_")

foreach scope in instate outstate {
	if "`scope'" == "instate" {
		local sd_irs irs_full_16_22
		local sd_acs acs_16_24_col
		local scope_label `"In-State"'
	}
	else {
		local sd_irs irs_outstate_full_16_22
		local sd_acs acs_outstate_16_24_col
		local scope_label `"Out-of-State"'
	}

	foreach migr in net in out {

		preserve
		keep if inlist(sample_data, "`sd_irs'", "`sd_acs'")
		keep if inlist(sample, "sample_all", "sample_stringency")
		keep if regexm(outcome, "_`migr'_rate_")

		qui count
		if r(N) == 0 {
			dis "  Set 2 skip — no rows for `scope' / `migr'."
			restore
			continue
		}

		local migr_label `"`lbl_migr_`migr''"'

		** x-offset by (data × pool).
		gen double event_year_off = event_year
		replace event_year_off = event_year_off - 0.15 if sample_data == "`sd_irs'" & sample == "sample_all"
		replace event_year_off = event_year_off - 0.05 if sample_data == "`sd_irs'" & sample == "sample_stringency"
		replace event_year_off = event_year_off + 0.05 if sample_data == "`sd_acs'" & sample == "sample_all"
		replace event_year_off = event_year_off + 0.15 if sample_data == "`sd_acs'" & sample == "sample_stringency"

		** Group masks (IRS = g1/g2; ACS = g3/g4).
		gen double tau_g1 = event_tau   if sample_data == "`sd_irs'" & sample == "sample_all"
		gen double lo_g1  = event_ci_lo if sample_data == "`sd_irs'" & sample == "sample_all"
		gen double hi_g1  = event_ci_hi if sample_data == "`sd_irs'" & sample == "sample_all"
		gen double tau_g2 = event_tau   if sample_data == "`sd_irs'" & sample == "sample_stringency"
		gen double lo_g2  = event_ci_lo if sample_data == "`sd_irs'" & sample == "sample_stringency"
		gen double hi_g2  = event_ci_hi if sample_data == "`sd_irs'" & sample == "sample_stringency"
		gen double tau_g3 = event_tau   if sample_data == "`sd_acs'" & sample == "sample_all"
		gen double lo_g3  = event_ci_lo if sample_data == "`sd_acs'" & sample == "sample_all"
		gen double hi_g3  = event_ci_hi if sample_data == "`sd_acs'" & sample == "sample_all"
		gen double tau_g4 = event_tau   if sample_data == "`sd_acs'" & sample == "sample_stringency"
		gen double lo_g4  = event_ci_lo if sample_data == "`sd_acs'" & sample == "sample_stringency"
		gen double hi_g4  = event_ci_hi if sample_data == "`sd_acs'" & sample == "sample_stringency"

		** Title suppressed when ${clean_figs} == 1 (paper version).
		local _title_opt `"title(`"`scope_label', `migr_label': Dataset and Donor Pool Comparison"', size(medsmall))"'
		if ${clean_figs} == 1 local _title_opt ""

		twoway (rcap lo_g1 hi_g1 event_year_off, lc("${col_sig_pref}")      lw(medthin)) ///
		       (scatter tau_g1 event_year_off,   mc("${col_sig_pref}")      ms(O) msize(small)) ///
		       (rcap lo_g2 hi_g2 event_year_off, lc("${col_insig_pref}")    lw(medthin)) ///
		       (scatter tau_g2 event_year_off,   mc("${col_insig_pref}")    ms(O) msize(small)) ///
		       (rcap lo_g3 hi_g3 event_year_off, lc("${col_sig_notpref}")   lw(medthin)) ///
		       (scatter tau_g3 event_year_off,   mc("${col_sig_notpref}")   ms(O) msize(small)) ///
		       (rcap lo_g4 hi_g4 event_year_off, lc("${col_insig_notpref}") lw(medthin)) ///
		       (scatter tau_g4 event_year_off,   mc("${col_insig_notpref}") ms(O) msize(small)), ///
		    yline(0, lc("${col_zero}") lp(dash))                            ///
		    xline(2020.5, lc(black) lp(solid))                              ///
		    xlabel(2016(1)2024, labsize(small))                             ///
		    ylabel(, format(%9.1f) labsize(small))                          ///
		    legend(order(2 "IRS, All"           4 "IRS, Stringency"         ///
		                 6 "ACS College, All"   8 "ACS College, Stringency") ///
		           rows(2) pos(6) size(small) region(lcolor(white)))        ///
		    `_title_opt'                                                    ///
		    ytitle(`"Event-study coefficient {&tau}{subscript:t} (pp)"', size(small)) ///
		    xtitle("Year", size(small))                                     ///
		    graphregion(color(white)) ysize(4) xsize(7)

		local fbase "${results}sdid/preferred_overlays/fig_overlay_dataset_`scope'_`migr'_eventstudy"
		graph export "`fbase'.pdf", replace
		graph export "`fbase'.jpg", as(jpg) quality(100) replace
		dis as text "  Set 2: wrote `scope' / `migr'"

		restore
	} // END migr
} // END scope

** Restore the data state from before Section 5.
use `"`orig_data'"', clear

} // END else (sdid_event_results.dta exists)


********************************************************************************
** SECTION 6: Summary
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
dis "  ${results}elasticities/fig_speccurve_elast_beta_{net,in,out}{,_shs}.{pdf,png}"
dis "  ${results}elasticities/fig_speccurve_elast_stock{,_shs}.{pdf,png}"
dis "  ${results}revenue/fig_speccurve_revenue_pfa.{pdf,png}"
dis "  ${results}revenue/fig_speccurve_revenue_oregon.{pdf,png}"
dis "  ${results}sdid/preferred_overlays/fig_overlay_donorpool_*_eventstudy.{pdf,jpg}"
dis "  ${results}sdid/preferred_overlays/fig_overlay_dataset_*_eventstudy.{pdf,jpg}"
dis "=============================================="

capture log close log_02tf
