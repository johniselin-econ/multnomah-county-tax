/*******************************************************************************
File Name:      02_bootstrap.do
Creator:        John Iselin
Date Created:   2026-04-24
Last Modified:  2026-04-28 — migrated from bash launcher to `parallel` package
                for cross-platform reproducibility (Phase B7).

Purpose:        Donor-cluster bootstrap driver for the highlighted SDID
                specifications. Each rep:
                  1) cluster-resamples donor counties via donor_resample,
                  2) refits SDID via fit_spec_sdid with vce(noinference)
                     so the solver skips its own placebo inference,
                  3) recomputes derived elasticities and revenue-loss
                     objects, and
                  4) writes one row per (rep, spec) to a per-rep .dta in
                     ${results}bootstrap/temp_draws/.

                After all reps finish, the driver appends every per-rep
                file into the canonical ${bootstrap_output} and writes a
                manifest. The temp directory is removed.

                Two execution paths share the same per-rep determinism
                contract (rep_seed = master_seed + offset + 997 * rep,
                independent of partition layout):

                  use_parallel == 1 (default if `parallel` is installed):
                    Workers fired by Vega's `parallel` ado. Cross-platform.

                  use_parallel == 0:
                    Serial fallback. Identical output to parallel mode
                    by construction. Useful for environments without
                    `parallel`, and for dev debugging.

                Only the highlighted specs that feed published tables
                are bootstrapped: IRS and ACS College (including their
                out-of-state variants) × {sample_all, sample_stringency,
                sample_narrow} × {net, in, out} with controls=1, exclusion=1.
                Mirrors project_mark_preferred_main in programs.do.

Called by:      00_multnomah.do (guarded by ${run_bootstrap})
Requires:       ${data}working/sdid_analysis_data.dta   (02_sdid_analysis.do)
                ${data}working/revenue_parameters.dta   (02_revenue_microsim.do)
                02_spec_engine.do                       (sourced for helpers)
                programs.do                         (setup_parallel)

Outputs:        ${results}bootstrap/bootstrap_draws.dta  (canonical, combined)
                ${results}bootstrap/bootstrap_draws_manifest.dta
                ${results}bootstrap/bootstrap_draws_manifest.csv
                ${data}working/bootstrap_spec_grid.dta   (intermediate)
                ${data}working/bootstrap_rep_grid.dta    (intermediate)

Globals (defaulted at top of this file unless noted):
    bootstrap_reps         total reps to run (default 20 — smoke)
    bootstrap_seed_offset  added to master_seed when deriving per-rep seeds
                           (default lives in globals.do alongside
                           master_seed)
    bootstrap_output       canonical combined draws .dta path
    use_parallel           project-wide parallel flag (globals.do)
    n_clusters             worker count (auto-capped by setup_parallel)
    resume                 if 1, skip reps whose per-rep .dta already exists

Authors: John Iselin

For more information, contact john.iselin@yale.edu
*******************************************************************************/

** ------------------------------------------------------------------
** SECTION 0: Setup
** ------------------------------------------------------------------

** Support standalone invocation: reconstruct project paths if a bare
** `do 02_bootstrap.do` is issued from code/stata with no orchestrator.
if "${dir}" == "" {
    local _cwd = subinstr("`c(pwd)'", "\", "/", .)
    if regexm("`_cwd'", "(.*)/code/(stata|utils)$") global dir = regexs(1)
    else global dir "`_cwd'"
}

do "${dir}/code/utils/globals.do"
do "${code}02_spec_engine.do"

** Bootstrap-specific globals.
** bootstrap_seed_offset lives in globals.do alongside master_seed
** so all randomness controls sit in one place.
if "${bootstrap_reps}" == ""        global bootstrap_reps        = 20
if "${bootstrap_output}" == ""      global bootstrap_output      "${results}bootstrap/bootstrap_draws.dta"

capture mkdir "${results}bootstrap"
capture mkdir "${results}bootstrap/temp_draws"

** Clear stale per-rep files unless we're resuming. Without this, a prior
** run's draws_rep_*.dta with a different master_seed or rep budget would
** silently combine into the canonical at aggregation — the rep-coverage
** check (Section 5) only verifies count and range, not seed lineage.
if ${resume} == 0 {
	local _stale : dir "${results}bootstrap/temp_draws" files "draws_rep_*.dta"
	local _n_stale : word count `_stale'
	if `_n_stale' > 0 {
		dis as text "Clearing `_n_stale' stale per-rep file(s) from prior runs (resume=0)"
		foreach f of local _stale {
			capture erase "${results}bootstrap/temp_draws/`f'"
		}
	}
}

capture log close log_02boot
log using "${logs}02_log_bootstrap_${date}", ///
	name(log_02boot) replace text

dis ""
dis "=============================================="
dis "02_bootstrap.do: donor-cluster bootstrap driver"
dis "=============================================="
dis "  master_seed          = ${master_seed}"
dis "  bootstrap_reps       = ${bootstrap_reps}"
dis "  bootstrap_seed_off   = ${bootstrap_seed_offset}"
dis "  use_parallel         = ${use_parallel}"
dis "  n_clusters           = ${n_clusters}"
dis "  resume               = ${resume}"
dis "=============================================="

** Preconditions — fail fast, do not silently fall back
confirm file "${data}working/sdid_analysis_data.dta"
confirm file "${data}working/revenue_parameters.dta"

load_revenue_params

** ------------------------------------------------------------------
** SECTION 1: Build the highlighted spec grid (saved to disk)
** ------------------------------------------------------------------
** The bootstrap only covers specs that appear in published tables.
** Mirrors project_mark_preferred_main: controls=1, exclusion=1, and
** sample ∈ {sample_all, sample_stringency, sample_narrow} for the six
** preferred (sample_data, out_type) blocks.
**
** Saved to ${data}working/bootstrap_spec_grid.dta so run_bootstrap_rep
** can `use` it inside each rep — workers in parallel mode run in their
** own Stata processes and need a stable on-disk artifact.
**
** Columns:
**   spec_id     sequential integer, 1..36
**   sample_data panel block key (understood by load_spec_panel)
**   out_type    per-panel outcome suffix
**   sample      donor-pool indicator (sample_all / sample_stringency / sample_narrow)
**   migration   "net" | "in" | "out"
**   outcome     full outcome variable name (agi_<mig>_rate_<out_type>)
**   data_type   presentation label consumed by spec_engine programs
**   outstate    0 = in-state, 1 = out-of-state only
**   controls    always 1 in preferred specs
**   exclusion   always 1 (drop 2020 from estimation)
**   event       1 iff migration == "net" (stock-ε needs event study)

preserve
clear
set obs 36

gen int    spec_id     = _n
gen str40  sample_data = ""
gen str20  out_type    = ""
gen str30  sample      = ""
gen str4   migration   = ""
gen str60  outcome     = ""
gen str30  data_type   = ""
gen byte   outstate    = .
gen byte   controls    = 1
gen byte   exclusion   = 1
gen byte   event       = .

local row = 0
foreach sd_block in "irs_full_16_22 irs IRS 0" ///
		"irs_outstate_full_16_22 irs_outstate IRS_(Out-of-State) 1" ///
		"acs_16_24_col acs2 ACS_College 0" ///
		"acs_outstate_16_24_col acs2_outstate ACS_College_(Out-of-State) 1" {
	tokenize `"`sd_block'"'
	local sd `1'
	local ot `2'
	local dt = subinstr("`3'", "_", " ", .)
	local os `4'
	foreach samp in "sample_all" "sample_stringency" "sample_narrow" {
		foreach mig in "net" "in" "out" {
			local ++row
			qui replace sample_data = "`sd'"  in `row'
			qui replace out_type    = "`ot'"  in `row'
			qui replace sample      = "`samp'" in `row'
			qui replace migration   = "`mig'" in `row'
			qui replace outcome     = "agi_`mig'_rate_`ot'" in `row'
			qui replace data_type   = "`dt'"   in `row'
			qui replace outstate    = `os'     in `row'
			qui replace event       = ("`mig'" == "net") in `row'
		}
	}
}
assert `row' == 36
compress
save "${data}working/bootstrap_spec_grid.dta", replace
restore

** ------------------------------------------------------------------
** SECTION 2: Define per-rep program (run_bootstrap_rep)
** ------------------------------------------------------------------
** Self-contained: takes one rep number, computes the deterministic
** seed, caches per-sample_data panels, fits all 36 specs, writes a
** single per-rep .dta to ${results}bootstrap/temp_draws/.
**
** This program is passed to parallel workers via prog() (parent's
** in-memory copy is serialized to each worker). The fit_spec_sdid
** family lives in 02_spec_engine.do which the wrapper re-sources
** explicitly to avoid the version-drift trap when spec_engine is
** edited mid-session.

capture program drop run_bootstrap_rep
program define run_bootstrap_rep
	syntax, rep(integer)

	** ---- Resume: skip if this rep's output already exists ----
	local out_path "${results}bootstrap/temp_draws/draws_rep_`rep'.dta"
	if ${resume} == 1 {
		capture confirm file "`out_path'"
		if _rc == 0 {
			dis "RESUME: skipping rep `rep' (output exists)"
			exit 0
		}
	}

	** ---- Deterministic per-rep seed (the V2.5-validated contract) ----
	local rep_seed = ${master_seed} + ${bootstrap_seed_offset} + 997 * `rep'
	set seed `rep_seed'

	dis ""
	dis "--- Rep `rep' (seed=`rep_seed') ---"

	local treated_fips = 41051

	** ---- Load the spec grid into local arrays ----
	** fit_spec_sdid replaces the dataset on every call (load_spec_panel
	** issues `use, clear`), so we cannot keep the grid in memory during
	** the spec loop. Pre-parse into macros, then drop.
	preserve
	use "${data}working/bootstrap_spec_grid.dta", clear
	local n_specs = _N
	forvalues i = 1/`n_specs' {
		local gr_spec_id_`i'  = spec_id[`i']
		local gr_sd_`i'       = sample_data[`i']
		local gr_samp_`i'     = sample[`i']
		local gr_mig_`i'      = migration[`i']
		local gr_outc_`i'     = outcome[`i']
		local gr_dt_`i'       = data_type[`i']
		local gr_os_`i'       = outstate[`i']
		local gr_c_`i'        = controls[`i']
		local gr_exl_`i'      = exclusion[`i']
		local gr_ev_`i'       = event[`i']
	}
	** Collapse to unique sample_data blocks for per-rep panel caching
	bysort sample_data (spec_id): keep if _n == 1
	local n_blocks = _N
	forvalues b = 1/`n_blocks' {
		local gr_block_sd_`b' = sample_data[`b']
	}
	restore

	** ---- Open the per-rep postfile ----
	** postfile accepts parenthesized varlists only for str# types;
	** numeric types must be declared variable-by-variable.
	tempname rep_pf
	postfile `rep_pf'                                                       ///
		int rep int spec_id                                                 ///
		str40(sample_data) str30(sample data_type) str4(migration)          ///
		byte outstate byte controls byte exclusion byte event_ok            ///
		double tau double se double pre_mean                                ///
		double flow_semi double flow_semi_shs                               ///
		double flow_e double flow_e_shs                                     ///
		double stock_total_common double stock_total_common_shs             ///
		double stock_total_full double stock_total_full_shs                 ///
		double stock_total_ann double stock_total_ann_shs                   ///
		double stock_imp_common double stock_imp_common_shs                 ///
		double stock_imp_full double stock_imp_full_shs                     ///
		double stock_imp_ann double stock_imp_ann_shs                       ///
		double pfa_loss double state_loss                                   ///
		using "`out_path'", replace

	** ---- Cache per-sample_data panels into tempfiles ----
	** Allocated once per rep; reused across the 6 specs that share each
	** sample_data block (2 samples × 3 migrations).
	forvalues b = 1/`n_blocks' {
		tempfile panel_`b'
	}
	forvalues b = 1/`n_blocks' {
		local sd_b `"`gr_block_sd_`b''"'

		preserve
		load_spec_panel, sampledata("`sd_b'")
		donor_resample, treatedcounty(`treated_fips')
		** isid fips year should still hold because donor_resample
		** renames duplicates to orig_id*1000 + dup_idx.
		qui isid fips year
		save "`panel_`b''", replace
		restore
	}

	** ---- Fit the 36 specs and post one row each ----
	local n_completed = 0
	local n_failed    = 0
	forvalues i = 1/`n_specs' {
		local spec_id_i  = `gr_spec_id_`i''
		local sd_i       `"`gr_sd_`i''"'
		local samp_i     `"`gr_samp_`i''"'
		local mig_i      `"`gr_mig_`i''"'
		local outcome_i  `"`gr_outc_`i''"'
		local dt_i       `"`gr_dt_`i''"'
		local os_i       = `gr_os_`i''
		local c_i        = `gr_c_`i''
		local exl_i      = `gr_exl_`i''
		local event_i    = `gr_ev_`i''

		** Look up the cached panel path for this sample_data block.
		** Tempfile path resolution must happen inside this loop so the
		** local-macro indirection (panel_`b') resolves correctly.
		local panel_path ""
		forvalues b = 1/`n_blocks' {
			if `"`gr_block_sd_`b''"' == `"`sd_i'"' {
				local panel_path `"`panel_`b''"'
			}
		}
		if `"`panel_path'"' == "" {
			dis as error "run_bootstrap_rep: could not locate cached panel for `sd_i'"
			local ++n_failed
			continue
		}

		** Fit SDID on the resampled panel, with inference disabled.
		** Uncertainty comes from the outer rep loop, not the solver.
		** fit_spec_sdid declares VCE(string asis); Stata's option parser
		** counts balanced parens, so vce(vce(noinference)) is unambiguous.
		capture noisily fit_spec_sdid, ///
			sampledata("`sd_i'") sample(`samp_i') outcome(`outcome_i') ///
			controls(`c_i') exclusion(`exl_i') ///
			eventstudy(`event_i') vce(vce(noinference)) ///
			datafile(`"`panel_path'"')

		if _rc != 0 {
			local _fit_rc = _rc
			dis as error "  rep=`rep' spec=`spec_id_i' (`sd_i'/`samp_i'/`mig_i'): fit_spec_sdid failed (rc=`_fit_rc'). Skipping."
			local ++n_failed
			continue
		}

		local tau_i      = r(tau)
		local se_i       = r(se)
		local premean_i  = r(pre_mean)
		local event_ok_i = r(event_ok)

		tempname etau
		if `event_ok_i' == 1 {
			matrix `etau' = r(event_taus)
		}

		** Elasticities: event_taus only for net specs; in/out skip the
		** stock block and return missings for stock_* columns.
		if "`mig_i'" == "net" & `event_ok_i' == 1 {
			compute_spec_elasticities, tau(`tau_i') se(`se_i') ///
				premean(`premean_i') migration("`mig_i'") ///
				datatype("`dt_i'") eventtaus(`etau')
		}
		else {
			compute_spec_elasticities, tau(`tau_i') se(`se_i') ///
				premean(`premean_i') migration("`mig_i'") ///
				datatype("`dt_i'")
		}

		local flow_semi_i      = r(beta)
		local flow_semi_shs_i  = r(beta_shs)
		local flow_e_i         = r(flow_e)
		local flow_e_shs_i     = r(flow_e_shs)
		local st_common_i      = r(stock_common)
		local st_common_shs_i  = r(stock_common_shs)
		local st_full_i        = r(stock_full)
		local st_full_shs_i    = r(stock_full_shs)
		local st_ann_i         = r(stock_ann)
		local st_ann_shs_i     = r(stock_ann_shs)
		local sti_common_i     = r(stock_imp_common)
		local sti_common_shs_i = r(stock_imp_common_shs)
		local sti_full_i       = r(stock_imp_full)
		local sti_full_shs_i   = r(stock_imp_full_shs)
		local sti_ann_i        = r(stock_imp_ann)
		local sti_ann_shs_i    = r(stock_imp_ann_shs)

		compute_spec_revenue, tau(`tau_i') migration("`mig_i'") ///
			outstate(`os_i') datatype("`dt_i'")
		local pfa_loss_i   = r(pfa_loss)
		local state_loss_i = r(state_loss)

		post `rep_pf' ///
			(`rep') (`spec_id_i') ///
			("`sd_i'") ///
			("`samp_i'") ("`dt_i'") ///
			("`mig_i'") ///
			(`os_i') (`c_i') (`exl_i') (`event_ok_i') ///
			(`tau_i') (`se_i') (`premean_i') ///
			(`flow_semi_i') (`flow_semi_shs_i') ///
			(`flow_e_i') (`flow_e_shs_i') ///
			(`st_common_i') (`st_common_shs_i') ///
			(`st_full_i') (`st_full_shs_i') ///
			(`st_ann_i') (`st_ann_shs_i') ///
			(`sti_common_i') (`sti_common_shs_i') ///
			(`sti_full_i') (`sti_full_shs_i') ///
			(`sti_ann_i') (`sti_ann_shs_i') ///
			(`pfa_loss_i') (`state_loss_i')

		if `event_ok_i' == 1 capture matrix drop `etau'
		local ++n_completed
	}

	postclose `rep_pf'

	** Add per-row run metadata. worker_id is set to a sentinel here
	** because parallel mode doesn't expose worker identity to the
	** child program. The columns are kept for schema continuity with
	** the legacy bash-launcher output (V2.5 parity ignores them).
	use "`out_path'", clear
	gen int    worker_id    = -1
	gen int    rep_start    = `rep'
	gen int    rep_end      = `rep'
	gen double master_seed  = ${master_seed}
	gen double seed_offset  = ${bootstrap_seed_offset}
	compress
	save "`out_path'", replace

	dis "  Rep `rep' done: `n_completed' specs completed, `n_failed' failed"
end

** ------------------------------------------------------------------
** SECTION 3: Define parallel wrapper
** ------------------------------------------------------------------
** Receives a slice of the rep-grid dataset in memory. Re-sources the
** spec engine inside the worker process so fit_spec_sdid / load_spec_panel
** are pinned to the on-disk version (avoids the version-drift trap
** documented in 02_sdid_analysis.do:778-785).
**
** globals.do and programs.do are NOT re-sourced here:
** sourcing them under concurrent worker load triggers sporadic rc=199
** from SSC `which` checks racing on the ado-path cache. Their globals
** are forwarded by parallel; their programs come in via prog().

capture program drop parallel_bootstrap_wrapper
program define parallel_bootstrap_wrapper
	capture noisily do "${code}02_spec_engine.do"
	if _rc != 0 {
		di as error "parallel_bootstrap_wrapper: 02_spec_engine.do failed (rc=`=_rc')"
		exit _rc
	}

	** Re-load revenue params inside each worker (load_revenue_params
	** populates global scalars consumed by compute_spec_revenue;
	** globals are forwarded by parallel but the helper's lazy-load
	** flag may not be).
	load_revenue_params

	** Capture all rep numbers up front. run_bootstrap_rep replaces the
	** dataset, so we cannot reference it after the first call.
	local n_obs = _N
	forvalues i = 1/`n_obs' {
		local rep_`i' = rep[`i']
	}

	dis "Worker received `n_obs' rep(s)"
	forvalues i = 1/`n_obs' {
		run_bootstrap_rep, rep(`rep_`i'')
	}
end

** ------------------------------------------------------------------
** SECTION 4: Build rep grid and dispatch
** ------------------------------------------------------------------

preserve
clear
set obs ${bootstrap_reps}
gen int rep = _n
compress
save "${data}working/bootstrap_rep_grid.dta", replace
restore

dis ""
dis "Built rep grid: ${bootstrap_reps} reps queued."

if ${use_parallel} == 1 {
	** ---- Parallel path ----
	** setup_parallel auto-caps n_clusters at processors_max / 4
	** (per the project's 4-core MP license; see programs.do).
	setup_parallel

	use "${data}working/bootstrap_rep_grid.dta", clear

	** Widen linesize so `program list` headers don't wrap; parallel's
	** exporter regex requires the trailing ":" on the same line. Long
	** signatures from the spec engine wrap at the default 79.
	** See 02_sdid_analysis.do:969-981 for the full incident note.
	local _orig_linesize = c(linesize)
	set linesize 255

	** processors() tells `parallel` how many cores each worker may use.
	** Default 0 means `set processors 1` — single-threaded — which makes
	** SDID's matrix solver ~10x slower per spec. We hand each worker the
	** full license-cap (c(processors_max)) so the solver runs at max speed.
	** setup_parallel's cap on n_clusters ensures total demand
	** (n_clusters * processors_max) doesn't exceed physical cores.
	local _proc_per_worker = c(processors_max)

	dis ""
	dis "Starting parallel bootstrap at $S_TIME (n_clusters=${n_clusters}, processors/worker=`_proc_per_worker')..."
	timer clear 1
	timer on 1

	parallel, prog(parallel_bootstrap_wrapper run_bootstrap_rep                 ///
			sdid_log_failure project_assert_manifest project_build_signature)   ///
		processors(`_proc_per_worker'):                                         ///
		parallel_bootstrap_wrapper

	set linesize `_orig_linesize'
	local parallel_rc = _rc

	timer off 1
	timer list 1
	if `parallel_rc' != 0 {
		dis as error ""
		dis as error "Parallel bootstrap failed with rc=`parallel_rc'."
		dis as error "Per-rep .dta files in ${results}bootstrap/temp_draws/ are preserved for inspection."
		dis as error "Inspect parallel-worker logs (auxiliary/parallel/) for details."
		exit `parallel_rc'
	}
	dis "Parallel bootstrap complete at $S_TIME"
}
else {
	** ---- Serial fallback ----
	use "${data}working/bootstrap_rep_grid.dta", clear

	dis ""
	dis "Starting serial bootstrap at $S_TIME..."
	timer clear 1
	timer on 1

	local n_obs = _N
	forvalues i = 1/`n_obs' {
		local rep_`i' = rep[`i']
	}
	forvalues i = 1/`n_obs' {
		run_bootstrap_rep, rep(`rep_`i'')
	}

	timer off 1
	timer list 1
	dis "Serial bootstrap complete at $S_TIME"
}

** ------------------------------------------------------------------
** SECTION 5: Aggregate per-rep files into the canonical .dta
** ------------------------------------------------------------------

local rep_files : dir "${results}bootstrap/temp_draws" files "draws_rep_*.dta"
local n_files : word count `rep_files'

if `n_files' == 0 {
	dis as error "Aggregation: no per-rep files found under ${results}bootstrap/temp_draws/"
	dis as error "Did the workers run? Check log files."
	exit 601
}

dis ""
dis "Aggregating `n_files' per-rep file(s)..."

clear
local first = 1
foreach f of local rep_files {
	if `first' == 1 {
		use "${results}bootstrap/temp_draws/`f'", clear
		local first = 0
	}
	else {
		append using "${results}bootstrap/temp_draws/`f'"
	}
}

count
local n_combined = r(N)

** Validate uniqueness: each (rep, spec_id) row should appear exactly once.
** Catches the case where a stale per-rep file survived from a prior run
** and got picked up by the aggregation glob alongside the fresh output.
capture isid rep spec_id
if _rc != 0 {
	dis as error ""
	dis as error "Aggregated draws contain duplicate (rep, spec_id) rows."
	dis as error "Most likely cause: stale draws_rep_*.dta from a prior run"
	dis as error "with different seed config got combined with new output."
	dis as error "Inspect ${results}bootstrap/temp_draws/ and re-run with"
	dis as error "resume=0 to wipe the temp directory."
	exit 459
}

** Validate rep coverage: distinct rep values must equal {1, ..., N}.
qui levelsof rep, local(reps_seen)
local nreps_seen : word count `reps_seen'
if `nreps_seen' != ${bootstrap_reps} {
	dis as error ""
	dis as error "Rep coverage mismatch:"
	dis as error "  expected ${bootstrap_reps} distinct reps, found `nreps_seen'."
	dis as error "  reps seen: `reps_seen'"
	dis as error "Aborting before overwriting canonical bootstrap_draws.dta."
	dis as error "Per-rep files in temp_draws/ are preserved for inspection."
	exit 459
}
qui sum rep
if r(min) != 1 | r(max) != ${bootstrap_reps} {
	dis as error ""
	dis as error "Rep range mismatch: expected [1..${bootstrap_reps}], got [`r(min)'..`r(max)']."
	exit 459
}

** Sanity check: net-migration specs must produce stock elasticities.
** If none do, the most likely cause is that sdid_event doesn't accept
** vce(noinference) under the installed package version, which makes
** event_ok=0 for every net spec and silently drops stock-ε draws —
** a failure mode that otherwise looks like a successful run.
qui count if migration == "net" & !missing(stock_total_common)
if r(N) == 0 {
	dis as error "WARNING: no net-migration spec produced a non-missing stock_total_common."
	dis as error "         Likely cause: sdid_event does not accept vce(noinference) on this install."
	dis as error "         Inspect 02_spec_engine.do:461-469 and adjust eventopts, then rerun."
}

sort rep spec_id
compress
save "${bootstrap_output}", replace
dis "Saved: ${bootstrap_output} (`n_combined' rows, `nreps_seen' reps)"

** ------------------------------------------------------------------
** SECTION 6: Manifest + cleanup
** ------------------------------------------------------------------

project_build_signature, artifact("bootstrap_draws")
local upstream "`r(signature)'|reps=${bootstrap_reps}|seed_off=${bootstrap_seed_offset}|use_parallel=${use_parallel}|n_clusters=${n_clusters}"
project_write_manifest ///
	using "${results}bootstrap/bootstrap_draws_manifest.dta", ///
	artifact("bootstrap_draws") script("02_bootstrap.do") ///
	upstream("`upstream'")

** Remove the temp directory now that aggregation succeeded. shell rmdir
** is platform-specific (Windows). The path conditionals follow the
** pattern from 02_sdid_analysis.do:1022.
if c(os) == "Windows" {
	shell rmdir "${results}bootstrap/temp_draws" /s /q
}
else {
	shell rm -rf "${results}bootstrap/temp_draws"
}

dis ""
dis "=============================================="
dis "02_bootstrap.do complete."
local _mode_label = cond(${use_parallel} == 1, "parallel", "serial")
dis "  Mode:         `_mode_label' (use_parallel=${use_parallel}, n_clusters=${n_clusters})"
dis "  Reps:         ${bootstrap_reps}"
dis "  Output:       ${bootstrap_output}"
dis "  Manifest:     ${results}bootstrap/bootstrap_draws_manifest.dta"
dis "=============================================="

capture log close log_02boot
