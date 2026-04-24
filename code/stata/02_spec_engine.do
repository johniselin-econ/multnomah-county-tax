/*******************************************************************************
File Name:      02_spec_engine.do
Creator:        John Iselin
Date Created:   2026-04-24

Purpose:        Shared engine of pure per-specification programs used by
                downstream scripts (post_spec, tables_figures, bootstrap).

                The engine defines four public programs:

                  load_revenue_params       — reads revenue_parameters.dta
                                              into global scalars; computes
                                              the four delta_ln_ntr_total*
                                              denominators used by elasticity
                                              calculations.

                  compute_spec_elasticities — given one spec's tau, se,
                                              pre_mean, migration, data_type,
                                              and optional event-study matrix,
                                              returns Kleven semi-elasticity
                                              (beta), flow elasticity, and
                                              horizon-H stock elasticity — all
                                              in both PFA-only and PFA+SHS
                                              denominator variants.

                  compute_spec_revenue      — given one spec's tau, migration,
                                              outstate, and data_type, returns
                                              the implied PFA and Oregon state
                                              revenue losses (in $M), using
                                              the rescale-to-actual-collections
                                              formula from the current
                                              02_revenue_microsim.do Section 12.

                It also provides the LaTeX table scaffolding helpers
                (elast_tex_open / elast_tex_notes_open / elast_tex_close)
                that any table-generating driver can reuse.

                Programs are pure: they accept scalars / matrices as args,
                return via r(), and read shared rate-and-share scalars from
                globals set by load_revenue_params. No program touches disk
                except load_revenue_params (which reads one .dta file).

Callers:        02_post_spec.do (to be created, Phase A)
                02_tables_figures.do (to be created, Phase A)
                02_bootstrap.do (to be created, Phase B)

Requires:       ${data}working/revenue_parameters.dta (from 02_revenue_microsim.do)
                project_assert_manifest (from 00_stata_config.do)

Testing:        This file defines programs only. To verify arithmetic matches
                the pre-restructure 02_elasticities.do and 02_revenue_microsim.do §12
                output, source this file, then run 02_post_spec.do and diff
                spec_results.dta against elasticity_results.dta +
                the per-spec implied_loss_i column from 02_revenue_microsim.do §12.
                Acceptance: bit-identical numeric columns.

Authors: John Iselin

For more information, contact john.iselin@yale.edu
*******************************************************************************/

** ------------------------------------------------------------------
** LaTeX table scaffolding — reused across table-generating drivers
** (02_tables_figures.do and any future bootstrap-table driver).
** Originally lived in 02_elasticities.do; moved here in commit A1 and
** became the single definition after 02_elasticities.do was retired
** in Phase A A5.
** ------------------------------------------------------------------

** Open a threeparttable with caption/label and begin the tabular.
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

** Close the tabular and begin tablenotes; caller writes note bodies
** then calls elast_tex_close.
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

** ------------------------------------------------------------------
** load_revenue_params
**
** Reads revenue_parameters.dta and populates global scalars used by
** compute_spec_elasticities and compute_spec_revenue. Also computes
** the four derived delta_ln_ntr_total* denominators in one place so
** no downstream script has to redo the ln((1-post)/(1-pre)) arithmetic.
**
** Usage:
**     load_revenue_params
**     load_revenue_params using "somepath/revenue_parameters.dta"
**
** Side effects:
**     Sets ~20 scalars in the global scalar namespace. Drops stale
**     scalars of the same names first so a repeat call after a .dta
**     change actually refreshes the values.
**
** Source of truth for scalar list: the `gen double X = scalar(X)` block
** at the end of 02_revenue_microsim.do Section 11.
** ------------------------------------------------------------------
capture program drop load_revenue_params
program define load_revenue_params
	syntax [using/]

	local file "${data}working/revenue_parameters.dta"
	if "`using'" != "" local file "`using'"

	** Manifest check — fail fast if signature drifted upstream
	local mfile = subinstr("`file'", ".dta", "_manifest.dta", .)
	if fileexists("`mfile'") {
		project_assert_manifest using "`mfile'", artifact("revenue_parameters")
	}

	** Drop stale scalars. `scalar` is a global namespace that survives
	** `clear`, so stale values from a prior run would silently shadow
	** the fresh load.
	foreach s in avg_mt_rate avg_state_rate avg_total_rate avg_total_rate_pre ///
		avg_total_rate_college avg_total_rate_pre_college                     ///
		avg_shs_rate avg_shs_rate_college                                     ///
		avg_total_rate_with_shs avg_total_rate_pre_with_shs                   ///
		avg_total_rate_col_with_shs avg_total_rate_pre_col_with_shs           ///
		avg_mt_rate_college_impacted                                          ///
		baseline_pfa_revenue baseline_state_revenue                           ///
		total_agi_2022 agi_total agi_impacted agi_college                     ///
		agi_college_impacted impacted_agi_share college_agi_share             ///
		college_impacted_agi_share                                            ///
		delta_ln_ntr_total delta_ln_ntr_total_college                         ///
		delta_ln_ntr_total_shs delta_ln_ntr_total_college_shs {
		capture scalar drop `s'
	}

	preserve
	use "`file'", clear

	foreach v in avg_mt_rate avg_state_rate avg_total_rate avg_total_rate_pre ///
		avg_total_rate_college avg_total_rate_pre_college                     ///
		avg_shs_rate avg_shs_rate_college                                     ///
		avg_total_rate_with_shs avg_total_rate_pre_with_shs                   ///
		avg_total_rate_col_with_shs avg_total_rate_pre_col_with_shs           ///
		avg_mt_rate_college_impacted                                          ///
		baseline_pfa_revenue baseline_state_revenue                           ///
		total_agi_2022 agi_total agi_impacted agi_college                     ///
		agi_college_impacted impacted_agi_share college_agi_share             ///
		college_impacted_agi_share {
		capture confirm variable `v'
		if _rc == 0 {
			scalar `v' = `v'[1]
		}
		else {
			dis as error "load_revenue_params: missing column `v' in `file'"
			exit 111
		}
	}
	restore

	** Derived denominators — the four flavors of Δln(1-τ).
	** Main (non-college) and subgroup-specific (college proxy) versions,
	** each with and without Metro SHS added to the post-rate.
	scalar delta_ln_ntr_total         = ln((1 - avg_total_rate)         ///
	                                     / (1 - avg_total_rate_pre))
	scalar delta_ln_ntr_total_college = ln((1 - avg_total_rate_college) ///
	                                     / (1 - avg_total_rate_pre_college))
	scalar delta_ln_ntr_total_shs     = ln((1 - avg_total_rate_with_shs)     ///
	                                     / (1 - avg_total_rate_pre_with_shs))
	scalar delta_ln_ntr_total_college_shs = ln((1 - avg_total_rate_col_with_shs) ///
	                                         / (1 - avg_total_rate_pre_col_with_shs))

	** Sanity (hard errors — a scale bug upstream should halt now, not
	** silently propagate into nonsensical elasticities).
	if avg_mt_rate < 0.001 | avg_mt_rate > 0.05 {
		dis as error "load_revenue_params: avg_mt_rate = " %8.6f avg_mt_rate ///
			" outside [0.001, 0.05]"
		exit 459
	}
	if avg_total_rate < 0.20 | avg_total_rate > 0.55 {
		dis as error "load_revenue_params: avg_total_rate = " %8.6f avg_total_rate ///
			" outside [0.20, 0.55]"
		exit 459
	}
end

** ------------------------------------------------------------------
** compute_spec_elasticities, rclass
**
** Computes Kleven semi-elasticity (beta), flow elasticity (flow_e),
** and horizon-H cumulative stock elasticity (stock_common / stock_full
** / stock_ann), each in both PFA-only and PFA+SHS denominator variants.
**
** Formulas mirror current 02_elasticities.do Section 1 exactly — only
** the scaffolding moves here. A spec row's output from this program
** must equal the same row's value in elasticity_results.dta (pre-
** Phase-A), column-for-column.
**
** Required args:
**     TAU          SDID coefficient, in percentage points of the
**                  migration rate
**     SE           Bootstrap SE on tau, in pp
**     PRE_MEAN     Pre-treatment mean of the migration rate, in pp
**                  (used by flow elasticity denominator)
**     MIGRATION    "net" | "in" | "out"
**     DATA_TYPE    "IRS" | "IRS (Out-of-State)" | "ACS All" |
**                  "ACS All (Out-of-State)" | "ACS College" |
**                  "ACS College (Out-of-State)"
**
** Optional args:
**     EVENT_TAUS       Name of a 2-column matrix (row per post year);
**                      col 1 = event_year, col 2 = event_tau (pp).
**                      Required for net-migration stock elasticity;
**                      caller leaves empty for non-net specs.
**     PFA_START_YEAR   Default 2021 (first post-treatment year).
**     COMMON_END_YEAR  Default 2022 (last year of IRS-ACS overlap).
**
** Returns via r():
**     r(beta), r(beta_se), r(beta_shs), r(beta_se_shs)
**     r(flow_e), r(flow_se), r(flow_e_shs), r(flow_se_shs)
**         — missing when migration is "net" or pre_mean is zero
**     r(stock_common), r(stock_common_shs),
**     r(stock_full), r(stock_full_shs),
**     r(stock_ann), r(stock_ann_shs),
**     r(stock_imp_common), r(stock_imp_common_shs),
**     r(stock_imp_full), r(stock_imp_full_shs),
**     r(stock_imp_ann), r(stock_imp_ann_shs)
**         — missing when migration is not "net" or event_taus empty
**     r(H_common), r(H_full)    — post-year counts (0 if no event_taus)
**     r(cum_tau_common), r(cum_tau_full)         — sum of τ_h (pp) over
**         the common (2021–common_end) and full post windows. Missing
**         when the corresponding H is 0. Used by downstream Excel
**         recalc_components sheets.
**     r(ln_common_tot), r(ln_full_tot),
**     r(ln_common_imp), r(ln_full_imp)           — Δln(S_H) accumulators
**         (total AGI base and impacted AGI base). Stock elasticities
**         equal these divided by stock_dln_ntr; exposed here so callers
**         can record the raw numerator without recomputing.
**     r(scale_total), r(scale_taxbase)    — for caller's reference
** ------------------------------------------------------------------
capture program drop compute_spec_elasticities
program define compute_spec_elasticities, rclass
	** NOTE: Stata `syntax` option names cannot contain underscores reliably
	** — they cause the program-call to hang in batch mode. Option names
	** here are concatenated (premean, datatype, eventtaus, pfastart,
	** commonend) for that reason. See also elast_hist_plot header note.
	syntax, TAU(real) SE(real) PREMEAN(real) ///
		MIGRATION(string) DATATYPE(string) ///
		[EVENTTAUS(name) PFASTART(integer 2021) ///
		 COMMONEND(integer 2022)]

	** Map to intuitive locals used in the body
	local pre_mean       `premean'
	local data_type      `"`datatype'"'
	local event_taus     `eventtaus'
	local pfa_start_year `pfastart'
	local common_end_year `commonend'

	** Subgroup indicator — ACS College outcomes migrate only a
	** fraction of total AGI, so scale accordingly.
	local is_college = (strpos("`data_type'", "ACS College") > 0)

	if `is_college' {
		local scale_total = college_agi_share
		local dln     "delta_ln_ntr_total_college"
		local dln_shs "delta_ln_ntr_total_college_shs"
	}
	else {
		local scale_total = 1
		local dln     "delta_ln_ntr_total"
		local dln_shs "delta_ln_ntr_total_shs"
	}
	local scale_taxbase = `scale_total' / impacted_agi_share

	return scalar scale_total   = `scale_total'
	return scalar scale_taxbase = `scale_taxbase'

	** ----- Kleven semi-elasticity (always defined) -----
	return scalar beta        = (`tau' / 100) / `dln'
	return scalar beta_se     = (`se'  / 100) / abs(`dln')
	return scalar beta_shs    = (`tau' / 100) / `dln_shs'
	return scalar beta_se_shs = (`se'  / 100) / abs(`dln_shs')

	** ----- Flow elasticity (gross migration only) -----
	if inlist("`migration'", "in", "out") & `pre_mean' != 0 & !missing(`pre_mean') {
		return scalar flow_e      = -(`tau' / `pre_mean') / `dln'
		return scalar flow_se     = (`se'  / abs(`pre_mean')) / abs(`dln')
		return scalar flow_e_shs  = -(`tau' / `pre_mean') / `dln_shs'
		return scalar flow_se_shs = (`se'  / abs(`pre_mean')) / abs(`dln_shs')
	}
	else {
		return scalar flow_e      = .
		return scalar flow_se     = .
		return scalar flow_e_shs  = .
		return scalar flow_se_shs = .
	}

	** ----- Horizon-H stock elasticity (net migration only) -----
	** Built by accumulating annual net-migration event-study effects
	** into the log AGI stock change. Sum is over post years up to
	** COMMON_END_YEAR (clean IRS-ACS overlap) and over all post years
	** (full; source-dependent: IRS to 2022, ACS to 2024).
	**
	** Mathematically: Δln(S_H) = Σ_{h=PFA_START}^H ln(1 + (τ_h/100) * scale)
	** where scale = scale_total (total AGI base) or scale_taxbase
	** (impacted AGI base).
	**
	** Accumulators use `tempname` scalars (full double precision).
	** Using `local x = expr` for a running sum loses precision because
	** Stata formats the intermediate as a %g string on each assignment.
	tempname H_common H_full
	tempname cum_tau_common cum_tau_full
	tempname ln_common_tot ln_full_tot ln_common_imp ln_full_imp
	scalar `H_common'       = 0
	scalar `H_full'         = 0
	scalar `cum_tau_common' = 0
	scalar `cum_tau_full'   = 0
	scalar `ln_common_tot'  = 0
	scalar `ln_full_tot'    = 0
	scalar `ln_common_imp'  = 0
	scalar `ln_full_imp'    = 0

	if "`migration'" == "net" & "`event_taus'" != "" {
		local n = rowsof(`event_taus')
		forvalues i = 1/`n' {
			local yr    = `event_taus'[`i', 1]
			local tau_i = `event_taus'[`i', 2]
			if missing(`tau_i') continue
			if `yr' < `pfa_start_year' continue

			** Guard log-domain: τ_h must not push the term below −1
			if 1 + (`tau_i' / 100) * `scale_total' <= 0 {
				dis as error "compute_spec_elasticities: stock log-domain violated at year `yr' (tau = `tau_i', scale_total = `scale_total')"
				exit 459
			}
			if 1 + (`tau_i' / 100) * `scale_taxbase' <= 0 {
				dis as error "compute_spec_elasticities: stock log-domain violated at year `yr' (tau = `tau_i', scale_taxbase = `scale_taxbase')"
				exit 459
			}

			scalar `H_full'       = `H_full' + 1
			scalar `cum_tau_full' = `cum_tau_full' + `tau_i'
			scalar `ln_full_tot'  = `ln_full_tot' + ln(1 + (`tau_i' / 100) * `scale_total')
			scalar `ln_full_imp'  = `ln_full_imp' + ln(1 + (`tau_i' / 100) * `scale_taxbase')

			if `yr' <= `common_end_year' {
				scalar `H_common'       = `H_common' + 1
				scalar `cum_tau_common' = `cum_tau_common' + `tau_i'
				scalar `ln_common_tot'  = `ln_common_tot' + ln(1 + (`tau_i' / 100) * `scale_total')
				scalar `ln_common_imp'  = `ln_common_imp' + ln(1 + (`tau_i' / 100) * `scale_taxbase')
			}
		}
	}

	** H counts: return 0 only if the stock block ran and found no post rows;
	** return missing (.) when the stock block didn't execute at all (non-net
	** spec, or net spec without event_taus). Matches pre-restructure
	** 02_elasticities.do which left H_common/H_full missing for merge-misses.
	if "`migration'" == "net" & "`event_taus'" != "" {
		return scalar H_common = `H_common'
		return scalar H_full   = `H_full'
	}
	else {
		return scalar H_common = .
		return scalar H_full   = .
	}
	return scalar cum_tau_common = cond(`H_common' > 0, `cum_tau_common', .)
	return scalar cum_tau_full   = cond(`H_full'   > 0, `cum_tau_full',   .)
	return scalar ln_common_tot  = cond(`H_common' > 0, `ln_common_tot',  .)
	return scalar ln_full_tot    = cond(`H_full'   > 0, `ln_full_tot',    .)
	return scalar ln_common_imp  = cond(`H_common' > 0, `ln_common_imp',  .)
	return scalar ln_full_imp    = cond(`H_full'   > 0, `ln_full_imp',    .)

	if `H_common' > 0 {
		return scalar stock_common         = `ln_common_tot' / `dln'
		return scalar stock_common_shs     = `ln_common_tot' / `dln_shs'
		return scalar stock_imp_common     = `ln_common_imp' / `dln'
		return scalar stock_imp_common_shs = `ln_common_imp' / `dln_shs'
	}
	else {
		return scalar stock_common         = .
		return scalar stock_common_shs     = .
		return scalar stock_imp_common     = .
		return scalar stock_imp_common_shs = .
	}

	if `H_full' > 0 {
		return scalar stock_full        = `ln_full_tot' / `dln'
		return scalar stock_full_shs    = `ln_full_tot' / `dln_shs'
		return scalar stock_imp_full    = `ln_full_imp' / `dln'
		return scalar stock_imp_full_shs = `ln_full_imp' / `dln_shs'
		return scalar stock_ann         = (`ln_full_tot' / `dln')     / `H_full'
		return scalar stock_ann_shs     = (`ln_full_tot' / `dln_shs') / `H_full'
		return scalar stock_imp_ann     = (`ln_full_imp' / `dln')     / `H_full'
		return scalar stock_imp_ann_shs = (`ln_full_imp' / `dln_shs') / `H_full'
	}
	else {
		return scalar stock_full        = .
		return scalar stock_full_shs    = .
		return scalar stock_imp_full    = .
		return scalar stock_imp_full_shs = .
		return scalar stock_ann         = .
		return scalar stock_ann_shs     = .
		return scalar stock_imp_ann     = .
		return scalar stock_imp_ann_shs = .
	}
end

** ------------------------------------------------------------------
** compute_spec_revenue, rclass
**
** Computes implied PFA and Oregon state revenue losses for one
** specification, using the "rescale to actual collections" formula
** from the current 02_revenue_microsim.do §12:
**
**     effect       = |tau| / 100                # decimal migration rate
**     effect       *= college_agi_share         # scale for ACS subsets
**     X            = effect × total_agi_2022    # $ AGI leaving
**     R_m          = avg_rate × X               # $ static revenue loss
**     dynamic      = baseline - R_m             # $ post-migration baseline
**     implied_loss = (R_m / dynamic) × actual / 1e6   # $M, rescaled
**
** PFA loss uses avg_mt_rate, baseline_pfa_revenue, and actual_pfa_revenue
** and only defined for domestic (outstate == 0) net migration.
** Oregon state loss uses avg_state_rate, baseline_state_revenue,
** and actual_oregon_revenue; only defined for out-of-state
** (outstate == 1) net migration. Other (migration, outstate) combos
** return missing.
**
** Required args:
**     TAU          SDID coefficient, in pp
**     MIGRATION    "net" | "in" | "out"
**     OUTSTATE     0 | 1
**     DATA_TYPE    (as above)
**
** Requires ${actual_pfa_revenue} and ${actual_oregon_revenue} globals
** set by 00_stata_config.do.
**
** Returns via r():
**     r(pfa_loss)     $M implied PFA revenue loss (. if not applicable)
**     r(state_loss)   $M implied Oregon state revenue loss (. if not applicable)
**     r(effect_scaled)  decimal-rate migration effect after ACS subset scaling
**     r(scale)        1 (IRS/ACS All) or college_agi_share (ACS College)
** ------------------------------------------------------------------
capture program drop compute_spec_revenue
program define compute_spec_revenue, rclass
	** Syntax option names are concatenated (datatype not data_type) for
	** the same batch-mode reason documented in compute_spec_elasticities.
	syntax, TAU(real) MIGRATION(string) OUTSTATE(integer) DATATYPE(string)
	local data_type `"`datatype'"'

	** Match the 02_revenue_microsim.do §12 scaling: if data_type contains "ACS",
	** scale by college_agi_share — the current code treats BOTH
	** "ACS All" and "ACS College" as college-subset, which is arguably
	** a bug (ACS All specs don't scale) but we reproduce the existing
	** behavior here and can correct it in a separate pass once Phase A
	** is landed and producing bit-identical output.
	local is_acs = (strpos("`data_type'", "ACS") > 0)
	local scale = cond(`is_acs', college_agi_share, 1)

	local effect_scaled = abs(`tau') / 100 * `scale'

	return scalar pfa_loss   = .
	return scalar state_loss = .
	return scalar effect_scaled = `effect_scaled'
	return scalar scale      = `scale'

	** PFA revenue loss: defined for domestic (outstate == 0) net migration
	if "`migration'" == "net" & `outstate' == 0 {
		local X       = `effect_scaled' * total_agi_2022
		local R_m     = avg_mt_rate * `X'
		local dynamic = baseline_pfa_revenue - `R_m'
		if `dynamic' > 0 {
			return scalar pfa_loss = (`R_m' / `dynamic') * ${actual_pfa_revenue} / 1e6
		}
	}

	** Oregon state revenue loss: defined for out-of-state net migration
	if "`migration'" == "net" & `outstate' == 1 {
		local X       = `effect_scaled' * total_agi_2022
		local R_m     = avg_state_rate * `X'
		local dynamic = baseline_state_revenue - `R_m'
		if `dynamic' > 0 {
			return scalar state_loss = (`R_m' / `dynamic') * ${actual_oregon_revenue} / 1e6
		}
	}
end
