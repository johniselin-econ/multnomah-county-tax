/*******************************************************************************
File Name:      02_spec_engine.do
Creator:        John Iselin
Date Created:   2026-04-24

Purpose:        Shared engine of per-specification programs used by
                downstream scripts (post_spec, tables_figures, bootstrap,
                and eventually sdid_analysis once estimation is rewired).

                The engine defines seven public programs:

                  load_revenue_params       — reads revenue_parameters.dta
                                              into global scalars; computes
                                              the four delta_ln_ntr_total*
                                              denominators used by elasticity
                                              calculations.

                  load_spec_panel           — loads the prepared SDID panel
                                              for one sample_data block and
                                              returns the associated data_var,
                                              out_type, and covariate set.

                  fit_spec_sdid             — fits one SDID specification and
                                              optionally one event study;
                                              returns tau, se, pre_mean,
                                              n_counties, and event-study
                                              year/tau pairs.

                  donor_resample            — cluster-bootstrap resample of
                                              donor counties with replacement,
                                              keeping the treated county fixed
                                              and renaming duplicate donor
                                              draws to unique IDs.

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

                Arithmetic programs are pure: they accept scalars / matrices
                as args, return via r(), and read shared rate-and-share
                scalars from globals set by load_revenue_params.
                The panel-loading / estimation helpers intentionally replace
                the dataset in memory as part of their contract.

Callers:        02_post_spec.do
                02_tables_figures.do
                02_bootstrap.do (to be created, Phase B)
                02_sdid_analysis.do (to be rewired, Phase B)

Requires:       ${data}working/revenue_parameters.dta (from 02_revenue_microsim.do)
                project_assert_manifest (from 00_stata_config.do)

Testing:        This file defines programs only. To verify arithmetic matches
                the pre-restructure 02_elasticities.do and 02_revenue_microsim.do §12
                output, source this file, then run 02_post_spec.do and diff
                spec_results.dta against elasticity_results.dta +
                the per-spec implied_loss_i column from 02_revenue_microsim.do §12.
                Acceptance: bit-identical numeric columns.

                To verify the Phase-B helpers, compare one known spec's tau,
                se, pre_mean, and event-study point estimates from
                fit_spec_sdid against the current inline code in
                02_sdid_analysis.do.

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
	syntax, HANDLE(string) CAP(string) LBL(string) ///
		COLS(string) [FONTSIZE(string)]
	** char(92) is `\` — Stata's macro expansion eats a literal `\`
	** that immediately precedes a backtick-delimited local reference.
	local bs = char(92)
	file write `handle' "\begin{table}[htbp]" _n
	file write `handle' "\centering" _n
	file write `handle' "\begin{threeparttable}" _n
	if "`fontsize'" != "" file write `handle' "`bs'`fontsize'" _n
	file write `handle' `"\caption{`cap'}"' _n
	file write `handle' "\label{`lbl'}" _n
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
** elast_tex_notes_inference
**
** Emits a single sentence describing the inference shown in the table
** (parenthetical SEs vs. bracketed bootstrap percentile CIs), with the
** parameters-as-fixed caveat. Caller decides where in the notes to call
** this — typically as the last sentence before elast_tex_close.
**
** Branches on ${show_bootstrap_cis}:
**   == 1: bracketed CIs sentence with `${ci_level}\\%` interpolated
**   == 0: analytic SEs sentence with the cumulative-stock-SE caveat
**         (as it stands in the current point-estimate-only pipeline)
**
** STOCK option: when set, adds the cumulative-stock-SE clause to the
** SE branch. Tables with a stock-elasticity column (the main and
** stock-compare tables) pass STOCK; pure flow tables (gross in/out)
** omit it.
** ------------------------------------------------------------------
capture program drop elast_tex_notes_inference
program define elast_tex_notes_inference
	syntax, HANDLE(string) [STOCK]

	if "${show_bootstrap_cis}" == "" global show_bootstrap_cis = 0
	if "${ci_level}" == "" global ci_level = 95

	if ${show_bootstrap_cis} == 1 {
		file write `handle' "Bracketed values are ${ci_level}\% percentile confidence intervals from a " _n
		file write `handle' "donor-cluster bootstrap that resamples non-treated counties with replacement, " _n
		file write `handle' "holding the treated county fixed; revenue and tax parameters " _n
		file write `handle' "(federal/Oregon/FICA/PFA rates, AGI base, and microsimulation denominators) " _n
		file write `handle' "are treated as fixed throughout the bootstrap." _n
	}
	else {
		file write `handle' "Standard errors in parentheses are SDID placebo-inference SEs for $\hat{\tau}$ and the implied $\beta$. " _n
		if "`stock'" != "" {
			file write `handle' "Cumulative stock elasticities require joint event-study covariances that the current pipeline does not export, " _n
			file write `handle' "so analytic SEs are not reported for those columns; donor-cluster bootstrap CIs are available as a separate output." _n
		}
	}
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

	** Manifest check — fail fast if signature drifted upstream. Skipped
	** when the data file path has no `.dta` extension (e.g., a .tmp
	** tempfile from a future caller); subinstr would otherwise leave
	** mfile == file and try to validate the data itself as a manifest.
	** Mirrors the same guard in load_spec_panel.
	local mfile = subinstr("`file'", ".dta", "_manifest.dta", .)
	if "`mfile'" != "`file'" & fileexists("`mfile'") {
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
		actual_oregon_revenue statewide_oregon_revenue                        ///
		multnomah_agi_share total_oregon_agi_2019                             ///
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
		college_impacted_agi_share                                            ///
		actual_oregon_revenue statewide_oregon_revenue                        ///
		multnomah_agi_share total_oregon_agi_2019 {
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

	** Publish ${actual_oregon_revenue} as a macro global so compute_spec_revenue
	** picks up the Multnomah-share value when scripts are re-run standalone
	** (i.e., without re-running 02_revenue_microsim.do). 00_stata_config.do
	** sets ${statewide_oregon_revenue} but no longer sets actual_oregon_revenue
	** directly; that value depends on Multnomah's IRS AGI share, which is
	** computed during the microsim and persisted to revenue_parameters.dta.
	global actual_oregon_revenue    = scalar(actual_oregon_revenue)
	global statewide_oregon_revenue = scalar(statewide_oregon_revenue)

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
** load_spec_panel, rclass
**
** Loads the prepared SDID analysis panel for one sample_data block and
** returns the metadata the current inline code reconstructs repeatedly:
** the corresponding data-window indicator, outcome suffix, and
** covariate set.
**
** This helper intentionally replaces the dataset in memory. The caller
** owns persistence if it needs to preserve an existing dataset.
**
** Required args:
**     SAMPLEDATA    one of:
**                   irs_full_16_22
**                   irs_outstate_full_16_22
**                   irs_389_16_22
**                   irs_outstate_389_16_22
**                   acs_16_22_all
**                   acs_16_22_col
**                   acs_16_24_all
**                   acs_16_24_col
**                   acs_outstate_16_22_all
**                   acs_outstate_16_22_col
**                   acs_outstate_16_24_all
**                   acs_outstate_16_24_col
**
** Optional args:
**     DATAFILE      defaults to ${data}working/sdid_analysis_data.dta
**
** Returns via r():
**     r(sample_data)   echoed input sample_data
**     r(data_var)      panel-window indicator used for sample restriction
**     r(out_type)      outcome suffix used in outcome variable names
**     r(covariates)    covariate varlist for controls == 1
** ------------------------------------------------------------------
capture program drop load_spec_panel
program define load_spec_panel, rclass
	syntax, SAMPLEDATA(string) [DATAFILE(string)]

	local data_file "${data}working/sdid_analysis_data.dta"
	if `"`datafile'"' != "" local data_file `"`datafile'"'

	local data_var ""
	local out_type ""

	if "`sampledata'" == "irs_full_16_22" {
		local data_var "irs_sample_1"
		local out_type "irs"
	}
	else if "`sampledata'" == "irs_outstate_full_16_22" {
		local data_var "irs_sample_1"
		local out_type "irs_outstate"
	}
	else if "`sampledata'" == "irs_389_16_22" {
		local data_var "irs_sample_2"
		local out_type "irs"
	}
	else if "`sampledata'" == "irs_outstate_389_16_22" {
		local data_var "irs_sample_2"
		local out_type "irs_outstate"
	}
	else if "`sampledata'" == "acs_16_22_all" {
		local data_var "acs_period_1"
		local out_type "acs1"
	}
	else if "`sampledata'" == "acs_16_22_col" {
		local data_var "acs_period_1"
		local out_type "acs2"
	}
	else if "`sampledata'" == "acs_outstate_16_22_all" {
		local data_var "acs_period_1"
		local out_type "acs1_outstate"
	}
	else if "`sampledata'" == "acs_outstate_16_22_col" {
		local data_var "acs_period_1"
		local out_type "acs2_outstate"
	}
	else if "`sampledata'" == "acs_16_24_all" {
		local data_var "acs_period_2"
		local out_type "acs1"
	}
	else if "`sampledata'" == "acs_16_24_col" {
		local data_var "acs_period_2"
		local out_type "acs2"
	}
	else if "`sampledata'" == "acs_outstate_16_24_all" {
		local data_var "acs_period_2"
		local out_type "acs1_outstate"
	}
	else if "`sampledata'" == "acs_outstate_16_24_col" {
		local data_var "acs_period_2"
		local out_type "acs2_outstate"
	}
	else {
		dis as error "load_spec_panel: unsupported sample_data `sampledata'"
		exit 198
	}

	local covariates "population per_capita_income"
	if "`data_var'" != "irs_sample_1" local covariates "`covariates' prop_tax_rate"

	** Manifest check applies only to the canonical .dta artifact. Bootstrap
	** callers pass Stata tempfiles (.tmp), where subinstr leaves the path
	** unchanged and the data file itself would be treated as the manifest,
	** producing a spurious "variable artifact not found" rc=111.
	local mfile = subinstr("`data_file'", ".dta", "_manifest.dta", .)
	if "`mfile'" != "`data_file'" & fileexists("`mfile'") {
		project_assert_manifest using "`mfile'", artifact("sdid_analysis_data")
	}

	use "`data_file'", clear
	capture confirm variable `data_var'
	if _rc != 0 {
		dis as error "load_spec_panel: `data_var' not found in `data_file'"
		exit 111
	}

	keep if `data_var' == 1
	sort fips year
	isid fips year

	return local sample_data "`sampledata'"
	return local data_var "`data_var'"
	return local out_type "`out_type'"
	return local covariates "`covariates'"
end

** ------------------------------------------------------------------
** fit_spec_sdid, rclass
**
** Fits one SDID specification and optionally extracts event-study
** point estimates in machine-readable year/tau form.
**
** This helper intentionally replaces the dataset in memory by calling
** load_spec_panel. It does not write to disk or export graphs.
**
** Required args:
**     SAMPLEDATA    sample_data label understood by load_spec_panel
**     SAMPLE        donor-pool indicator variable (e.g. sample_all)
**     OUTCOME       outcome variable to estimate
**     CONTROLS      0 or 1
**     EXCLUSION     0 or 1 (drop 2020 from the estimation sample)
**
** Optional args:
**     EVENTSTUDY    0 or 1; if 1, attempt sdid_event and return
**                   year/tau and year/CI matrices
**     VCE           full vce/reps clause passed to sdid; default matches
**                   the current production path
**     REPS          bootstrap/placebo reps for the default VCE path
**     DATAFILE      alternate sdid_analysis_data.dta path
**     GRAPHBASE     base path passed through to
**                   graph graph_export("GRAPHBASE", .pdf)
**
** Returns via r():
**     r(tau), r(se), r(pre_mean), r(n_counties), r(event_ok)
**     r(sample_data), r(sample), r(outcome), r(covariates)
**     r(event_taus) when EVENTSTUDY == 1 and extraction succeeds
**     r(event_res)  four-column matrix: event_year, event_tau,
**                   event_ci_lo, event_ci_hi
** ------------------------------------------------------------------
capture program drop fit_spec_sdid
program define fit_spec_sdid, rclass
	syntax, SAMPLEDATA(string) SAMPLE(name) OUTCOME(name) ///
		CONTROLS(integer) EXCLUSION(integer) ///
		[EVENTSTUDY(integer 0) VCE(string asis) REPS(integer 100) ///
		 DATAFILE(string) GRAPHBASE(string)]

	load_spec_panel, sampledata("`sampledata'") datafile(`"`datafile'"')

	local covariates `"`r(covariates)'"'
	local sample_data `"`r(sample_data)'"'

	capture confirm variable `sample'
	if _rc != 0 {
		dis as error "fit_spec_sdid: sample variable `sample' not found"
		exit 111
	}
	capture confirm variable `outcome'
	if _rc != 0 {
		dis as error "fit_spec_sdid: outcome variable `outcome' not found"
		exit 111
	}
	capture confirm variable Treated
	if _rc != 0 {
		dis as error "fit_spec_sdid: Treated not found in loaded panel"
		exit 111
	}

	tempvar in_sample
	gen byte `in_sample' = `sample' == 1
	if `exclusion' == 1 replace `in_sample' = 0 if year == 2020

	qui count if multnomah == 1 & `in_sample' == 1
	if r(N) == 0 {
		dis as error "fit_spec_sdid: no treated observations remain in sample for `sample_data' / `sample'"
		exit 2000
	}

	local covars ""
	local covars_event ""
	tempname sdid_est
	if `controls' == 1 {
		local covars "covariates(`covariates')"
		local covars_event "covariates(`covariates')"
	}

	local vceopt `"`vce'"'
	if `"`vceopt'"' == "" local vceopt "vce(placebo) reps(`reps')"
	local graphopt ""
	if `"`graphbase'"' != "" local graphopt `"graph graph_export("`graphbase'", .pdf)"'

	capture noisily sdid `outcome' fips year Treated ///
		if `in_sample' == 1, ///
		`vceopt' ///
		`graphopt' ///
		`covars'
	if _rc != 0 {
		local failed_rc = _rc
		dis as error "fit_spec_sdid: sdid failed for `sample_data' / `sample' / `outcome'"
		exit `failed_rc'
	}
	** vce(noinference) leaves sdid without a full posted result, so a bare
	** `estimates store` raises rc=301 ("last estimates not found"). Capture
	** the store and gate the corresponding restore/drop on success. The
	** event-study block reads e(H) directly and does not need the stored
	** estimates handle.
	capture qui estimates store `sdid_est'
	local has_stored_est = (_rc == 0)

	local tmp_tau = e(ATT)
	local tmp_se = .
	capture local tmp_se = e(se)
	qui summ `outcome' if multnomah == 1 & Treated == 0 & `in_sample' == 1
	local tmp_premean = r(mean)
	qui count if year == 2021 & `in_sample' == 1
	local tmp_ncounties = r(N)
	local event_ok = 0

	if `eventstudy' == 1 {
		local eventopts "vce(placebo) brep(`reps') placebo(all)"
		if strpos(lower(`"`vceopt'"'), "noinference") > 0 {
			** sdid_event's vce() allowlist is {off, placebo, bootstrap} —
			** it rejects vce(noinference) even though the main `sdid`
			** command accepts it. vce(off) is sdid_event's equivalent
			** inference-skipping option. Bootstrap callers derive
			** uncertainty from the outer rep loop, so the inner solver
			** never needs an SE pass.
			local eventopts "vce(off)"
		}

		capture noisily sdid_event `outcome' fips year Treated ///
			if `in_sample' == 1, ///
			`covars_event' ///
			`eventopts'
		local event_rc = _rc
		capture drop ever_treated*

		if `event_rc' == 0 {
			qui summ year if multnomah == 1 & `in_sample' == 1
			local max_yr = r(max)

			** sdid_event's e(H) shape depends on the vce() option:
			**   vce(placebo) placebo(all):
			**     rows = 1 ATT + N_post + N_pre placebos = 1 + N_treated_obs
			**     cols = 5 (Estimate, SE, ci_lo, ci_hi, Switchers)
			**   vce(off) (used by the bootstrap path):
			**     rows = 1 ATT + N_post
			**     cols = 3 (Estimate, SE=., Switchers) — no CI columns
			** Read the matrix's actual shape instead of inferring from the
			** sample. ci_lo/ci_hi reads are gated on `has_ci' so vce(off)
			** doesn't trip rc=503 from indexing nonexistent columns.
			local n_eH_rows = rowsof(e(H))
			local n_eH_cols = colsof(e(H))
			local last_col  = min(`n_eH_cols', 5)
			local has_ci    = (`n_eH_cols' >= 4)

			tempname res event_taus rowmat
			capture matrix `res' = e(H)[2..`n_eH_rows', 1..`last_col']
			if _rc == 0 {
				tempname event_res
				local posted = 0
				local nrows = rowsof(`res')
				forvalues i = 1/`nrows' {
					local tau_i = `res'[`i', 1]
					if missing(`tau_i') continue
					local yr = `max_yr' - `i' + 1
					if `exclusion' == 1 & `yr' <= 2020 local yr = `yr' - 1
					local ci_lo_i = .
					local ci_hi_i = .
					if `has_ci' {
						local ci_lo_i = `res'[`i', 3]
						local ci_hi_i = `res'[`i', 4]
					}
					matrix `rowmat' = (`yr', `tau_i')
					tempname rowfull
					matrix `rowfull' = (`yr', `tau_i', `ci_lo_i', `ci_hi_i')
					if `posted' == 0 {
						matrix `event_taus' = `rowmat'
						matrix `event_res' = `rowfull'
						local posted = 1
					}
					else {
						matrix `event_taus' = `event_taus' \ `rowmat'
						matrix `event_res' = `event_res' \ `rowfull'
					}
				}
				if `posted' == 1 {
					local event_ok = 1
				}
			}
		}
	}
	if `has_stored_est' {
		capture quietly estimates restore `sdid_est'
		capture estimates drop `sdid_est'
	}
	return scalar tau = `tmp_tau'
	return scalar se = `tmp_se'
	return scalar pre_mean = `tmp_premean'
	return scalar n_counties = `tmp_ncounties'
	return scalar event_ok = `event_ok'
	return local sample_data "`sample_data'"
	return local sample "`sample'"
	return local outcome "`outcome'"
	return local covariates "`covariates'"
	if `event_ok' == 1 {
		return matrix event_taus = `event_taus'
		return matrix event_res = `event_res'
	}
end

** ------------------------------------------------------------------
** donor_resample
**
** Cluster-bootstrap resample of donor counties. Keeps the treated
** county fixed, resamples donor county IDs with replacement, and
** assigns duplicate donor draws unique new IDs so the estimator sees
** them as distinct units.
**
** This helper intentionally replaces the dataset in memory with the
** resampled panel.
**
** Required args:
**     TREATEDCOUNTY   numeric county identifier to keep fixed
**
** Optional args:
**     IDVAR           panel unit identifier; defaults to fips
**
** Returns via r():
**     r(n_donors)     number of distinct donor counties in the source panel
**     r(n_draws)      number of donor draws in the resample
** ------------------------------------------------------------------
capture program drop donor_resample
program define donor_resample, rclass
	syntax, TREATEDCOUNTY(integer) [IDVAR(name)]

	local id_var "`idvar'"
	if "`id_var'" == "" local id_var "fips"

	capture confirm variable `id_var'
	if _rc != 0 {
		dis as error "donor_resample: id variable `id_var' not found"
		exit 111
	}

	tempfile source treated donor_pool draws
	qui save `source', replace

	use `source', clear
	keep if `id_var' == `treatedcounty'
	save `treated', replace

	use `source', clear
	keep if `id_var' != `treatedcounty'
	levelsof `id_var', local(donors) clean
	local n_donors : word count `donors'
	if `n_donors' == 0 {
		dis as error "donor_resample: no donor counties remain after removing treated county"
		exit 2000
	}
	save `donor_pool', replace

	clear
	set obs `n_donors'
	gen int draw_idx = ceil(runiform() * `n_donors')
	gen long orig_id = .
	tokenize "`donors'"
	forvalues i = 1/`n_donors' {
		qui replace orig_id = real("``i''") if draw_idx == `i'
	}
	sort orig_id
	by orig_id: gen int dup_idx = _n
	gen long new_id = orig_id * 1000 + dup_idx
	keep orig_id new_id
	save `draws', replace

	use `donor_pool', clear
	rename `id_var' orig_id
	joinby orig_id using `draws'
	drop orig_id
	rename new_id `id_var'
	append using `treated'
	sort `id_var' year

	return scalar n_donors = `n_donors'
	return scalar n_draws = `n_donors'
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
	** commonend) for that reason. Same constraint applies to the
	** elast_speccurve_plot helper in 02_tables_figures.do.
	syntax, TAU(real) SE(real) PREMEAN(real) ///
		MIGRATION(string) DATATYPE(string) ///
		[EVENTTAUS(name) PFASTART(integer 2021) ///
		 COMMONEND(integer 2022)]

	** Precondition: the four delta_ln_ntr_total* + share scalars must be
	** populated. They live in the `scalar` global namespace and are set
	** by load_revenue_params. Without them, every elasticity below silently
	** returns missing — a failure mode that looks like a successful run.
	capture confirm scalar college_agi_share
	if _rc != 0 {
		dis as error "compute_spec_elasticities: revenue scalars not loaded — call load_revenue_params first."
		exit 111
	}

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
** SCOPE NOTE: actual_oregon_revenue is the Multnomah-resident share of
** statewide Oregon individual income tax (statewide collections × Multnomah
** AGI share, computed in 02_revenue_microsim.do Section 3). Both
** baseline_state_revenue and actual_oregon_revenue are therefore
** Multnomah-resident-only, so the (R_m / dynamic) × actual rescale is an
** apples-to-apples adjustment within the Multnomah scope. The statewide
** total is preserved as ${statewide_oregon_revenue} for reference but is
** not used in this calculation.
**
** Required args:
**     TAU          SDID coefficient, in pp
**     MIGRATION    "net" | "in" | "out"
**     OUTSTATE     0 | 1
**     DATA_TYPE    (as above)
**
** Requires ${actual_pfa_revenue} (set by 00_stata_config.do) and
** ${actual_oregon_revenue} (set by 02_revenue_microsim.do Section 3 once
** the Multnomah AGI share has been computed from IRS county data).
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
