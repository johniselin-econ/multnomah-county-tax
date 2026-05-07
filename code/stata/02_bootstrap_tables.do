/*******************************************************************************
File Name:      02_bootstrap_tables.do
Creator:        John Iselin
Date Created:   2026-04-27

Purpose:        Collapse the bootstrap draws (one row per (rep, spec)) to
                percentile confidence intervals (one row per spec).
                Implements B4 from the bootstrap-restructure plan.

                Inputs:
                  results/bootstrap/bootstrap_draws.dta
                  results/bootstrap/bootstrap_draws_manifest.dta

                Output:
                  results/bootstrap/bootstrap_cis.dta
                  results/bootstrap/bootstrap_cis_manifest.dta

                For each spec (one row in the output, keyed by spec_id +
                sample_data + sample + migration + outstate + data_type),
                computes the requested percentile pair on every CI-relevant
                column. Default is the 95% interval (p2.5, p97.5);
                override via ${ci_level} in {90, 95, 99}.

                CI-relevant columns. Each gets:
                  `<var>_median`     bootstrap median (cross-check vs
                                     the point-estimate file)
                  `<var>_lo`         lower percentile
                  `<var>_hi`         upper percentile
                  `<var>_n`          number of non-missing draws used

                  tau                                   (SDID coefficient)
                  flow_semi, flow_semi_shs              (Kleven semi-ε)
                  flow_e,    flow_e_shs                 (gross flow ε)
                  stock_total_{common,full,ann}{,_shs}  (PFA-only stock ε)
                  stock_imp_{common,full,ann}{,_shs}    (impacted-AGI stock ε)
                  pfa_loss, state_loss                  (revenue $M)

                Specs where a column is all-missing in the draws (e.g.,
                stock_* for in/out migration specs) get `.` for median /
                lo / hi and 0 for `_n`. Downstream callers can detect
                thin CI support by checking `_n` < some threshold.

Called by:      02_tables_figures.do (when ${show_bootstrap_cis} == 1)
                Or invoked manually after a parallel-bootstrap run.

Requires:       results/bootstrap/bootstrap_draws.dta from
                02_bootstrap.do (parallel via Stata `parallel` ado, or
                serial when ${use_parallel}=0).
                project_assert_manifest, project_build_signature,
                project_write_manifest (from 00_stata_config.do).

Globals:        ci_level    integer in {90, 95, 99}; default 95
                            (sets percentile pair: 5/95, 2.5/97.5,
                            0.5/99.5 respectively).

Authors: John Iselin

For more information, contact john.iselin@yale.edu
*******************************************************************************/

** ------------------------------------------------------------------
** SECTION 0: Setup
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

if "${ci_level}" == "" global ci_level = 95
if !inlist(${ci_level}, 90, 95, 99) {
	dis as error "02_bootstrap_tables: ci_level must be 90, 95, or 99 (got: ${ci_level})"
	exit 198
}

** Map ci_level -> percentile pair. egen pctile() accepts fractional p().
if ${ci_level} == 90 {
	local p_lo = 5
	local p_hi = 95
}
else if ${ci_level} == 95 {
	local p_lo = 2.5
	local p_hi = 97.5
}
else {
	local p_lo = 0.5
	local p_hi = 99.5
}

local draws_file "${results}bootstrap/bootstrap_draws.dta"
local cis_file   "${results}bootstrap/bootstrap_cis.dta"

capture log close log_02bcis
log using "${logs}02_log_bootstrap_tables_${date}", ///
	name(log_02bcis) replace text

dis ""
dis "=============================================="
dis "02_bootstrap_tables.do: percentile CI builder"
dis "=============================================="
dis "  draws input:  `draws_file'"
dis "  cis output:   `cis_file'"
dis "  ci_level:     ${ci_level}% (p`p_lo' / p`p_hi')"
dis "=============================================="

** ------------------------------------------------------------------
** SECTION 1: Load draws and validate schema
** ------------------------------------------------------------------

confirm file "`draws_file'"

local mfile = subinstr("`draws_file'", ".dta", "_manifest.dta", .)
if "`mfile'" != "`draws_file'" & fileexists("`mfile'") {
	project_assert_manifest using "`mfile'", artifact("bootstrap_draws")
}

use "`draws_file'", clear

** This list mirrors the postfile schema in 02_bootstrap.do:214-223.
** If new columns are added there, add them here too — there is no
** automatic discovery (and the metadata columns worker_id, rep_start,
** etc. should NOT receive CIs).
local ci_vars ///
	tau ///
	flow_semi flow_semi_shs ///
	flow_e flow_e_shs ///
	stock_total_common stock_total_common_shs ///
	stock_total_full   stock_total_full_shs ///
	stock_total_ann    stock_total_ann_shs ///
	stock_imp_common   stock_imp_common_shs ///
	stock_imp_full     stock_imp_full_shs ///
	stock_imp_ann      stock_imp_ann_shs ///
	pfa_loss state_loss

foreach v of local ci_vars {
	capture confirm variable `v'
	if _rc != 0 {
		dis as error "02_bootstrap_tables: expected column `v' not found in `draws_file'."
		dis as error "         Schema may have drifted — check 02_bootstrap.do's postfile."
		exit 111
	}
}

qui count
local n_draws = r(N)
qui levelsof spec_id, local(spec_ids)
local n_specs : word count `spec_ids'
qui levelsof rep, local(reps)
local n_reps : word count `reps'

dis _newline "Loaded `n_draws' draws across `n_specs' specs and `n_reps' reps."

** ------------------------------------------------------------------
** SECTION 2: Compute per-spec percentile columns
** ------------------------------------------------------------------
**
** Stata's egen pctile(), by(spec_id) p(#) computes the same value for
** all rows within a spec, so each draw row gets its spec's percentile.
** That's wasteful for memory (24 specs × 20 reps = 480 rows holding
** redundant per-spec values) but trivial at this scale, and it lets
** us collapse with a single duplicates-drop instead of a loop.
**
** _n (count of non-missing draws per spec per var) is computed first
** so the median/lo/hi can detect all-missing groups and produce . .
** instead of failing.

foreach v of local ci_vars {
	qui egen long `v'_n      = count(`v'),         by(spec_id)
	qui egen double `v'_median = pctile(`v'),       by(spec_id) p(50)
	qui egen double `v'_lo     = pctile(`v'),       by(spec_id) p(`p_lo')
	qui egen double `v'_hi     = pctile(`v'),       by(spec_id) p(`p_hi')
}

** ------------------------------------------------------------------
** SECTION 3: Collapse to one row per spec
** ------------------------------------------------------------------

keep spec_id sample_data sample migration outstate data_type ///
	 controls exclusion ///
	 *_median *_lo *_hi *_n

duplicates drop spec_id, force
sort spec_id

** ------------------------------------------------------------------
** SECTION 4: Save canonical CI file + manifest
** ------------------------------------------------------------------

** Document the percentile pair in dataset notes for downstream
** consumers that don't read the manifest.
note: ci_level = ${ci_level}
note: p_lo = `p_lo'
note: p_hi = `p_hi'
note: source = `draws_file'
note: n_reps = `n_reps'

order spec_id sample_data sample migration outstate data_type ///
	  controls exclusion

compress
save "`cis_file'", replace
qui count
local n_cis = r(N)

dis _newline "Saved: `cis_file' (`n_cis' specs)"

project_build_signature, artifact("bootstrap_cis")
local upstream "`r(signature)'|ci_level=${ci_level}|p_lo=`p_lo'|p_hi=`p_hi'|n_specs=`n_cis'|n_reps=`n_reps'|source=bootstrap_draws.dta"
project_write_manifest ///
	using "${results}bootstrap/bootstrap_cis_manifest.dta", ///
	artifact("bootstrap_cis") script("02_bootstrap_tables.do") ///
	upstream("`upstream'")

dis ""
dis "=============================================="
dis "02_bootstrap_tables.do complete."
dis "  CI level:     ${ci_level}%"
dis "  Output specs: `n_cis'"
dis "  Output:       `cis_file'"
dis "  Manifest:     ${results}bootstrap/bootstrap_cis_manifest.dta"
dis "=============================================="

capture log close log_02bcis

exit, clear
