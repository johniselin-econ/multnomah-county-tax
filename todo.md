# TODO - Multnomah County Tax

Two active workstreams:

1. **Paper revision** — Phase 3 tex pass + remaining asset/code items.
2. **Bootstrap implementation** — V3/V4 reps and CI-rendering polish.

---

# Workstream 1: Paper revision

Original instructions: identify code vs tex changes, work through code changes
first, then make the tex changes in a single pass. For each change consider
references elsewhere in the text and the code-to-Overleaf pipeline.

Plan file: `quality_reports/plans/2026-05-06_paper-revision-todos.md`

## Status (last updated 2026-05-07)

- Code-side: ALL 12 items DONE (13, 14, 15, 9, 10, 16, 3, 21c, 2, 4, 8, 11).
- Tex-side: Phase 3 tex edits still pending — collected to a single edit pass.
- Next: Phase 3 single tex pass on `updated.tex` covering items 1, 5, 6, 7,
  12, 17, 18, 19, 20, 21a, 21b, 21d, 21e, 21f. Then Phase 4: verify + Overleaf
  compile.

## Items

* [DONE — Phase 3 tex pass] Drop the equations from the SDID text (4.1) — those are DID equations, not SDID equations.

* [DONE — code in `code/R/map_code.R`, 2026-05-07] *(Item 2)* Figure 1: OR/WA area with a Portland cutout that includes the Average Marginal Tax rate shading, with the legend below the full figure.
  → New `map_combined_tax.png` produced and synced to Overleaf. Built from `map1_with_box` overview + new `map2_tax_inset` (tax-shaded close-up, internal legend suppressed) + horizontal tax-rate legend strip below. Phase 3 tex pass switches updated.tex Fig 1 from `map2_tax.png` to `map_combined_tax.png`.

* [DONE — `code/R/fig_diagrams.R`, 2026-05-07] *(Item 3)* Figures 2 and 3: bigger text, no colored boxes; Figure 3 drop trailing "Outcome variables", "Key Controls", "Donor Pool Restrictions".
  → Bumped `tx()` font sizes by ~2 points; replaced colored fills with `NA`; deleted bottom-info section in `draw_empirical_approach`. Re-rendered and synced to Overleaf.

* [DONE — `fig_diagrams.R` and `02_tables_figures.do`, 2026-05-07] *(Item 4)* All figures: produce versions with and without titles/subtitles/notes; paper uses bare versions.
  → R-side conceptual diagrams now produce two variants: `fig_*.pdf` (paper) and `fig_*_titled.pdf` (slides). Stata-side `${clean_figs}` global toggle (default 0) wired through both preferred-overlay event-study blocks. Maps were already title-free. Spec curves keep minor titles for now.

* [PENDING — Phase 3 tex pass] *(Item 5)* Figure 6: use the OR/WA versions in main, West Coast in appendix. Just an `\includegraphics` swap; assets exist.

* [PENDING — Phase 3 tex pass] *(Item 6)* Figure 7: stack the two event-studies vertically rather than side-by-side. Subfigure layout change.

* [PENDING — Phase 3 tex pass] *(Item 7)* Figure 10: use newly-created specification curves. Switch `\includegraphics` from `fig_revenue_dist_*.pdf` to `fig_speccurve_revenue_*.pdf` (assets in Overleaf).

* [DONE — `02_descriptives_supp.do`, 2026-05-07] *(Item 8)* Table 1: drop Panel B; expand Panel A by replicating the structure for ACS as a new Panel B; add a counties column; include all five samples.
  → New `table1_combined.tex`: 2 panels (IRS + ACS College) × 6 rows × 9 cols. IRS uses 2018-19 vs 2021-22; ACS uses 2018-19 vs 2021-24. Synced to Overleaf.

* [DONE — `02_tables_figures.do`, 2026-05-07] *(Item 9)* Elasticity figures: clearer y-axis labels (e.g. "Migration Stock Elasticity"); define mathematically in the note.
  → Y-axis labels updated to "Migration Semi-Elasticity (β)", "Migration Stock Elasticity", "Migration Flow Elasticity" with PFA+SHS variants. Math definitions go in figure notes during Phase 3 tex pass.

* [DONE — `02_tables_figures.do`, 2026-05-07] *(Item 10)* Add Figure A3 counterparts for flow elasticities and semi-elasticities.
  → 4 new flow-elasticity distribution figures: `fig_speccurve_elast_flow_in.pdf`, `_out`, +SHS variants. Synced to Overleaf.

* [DONE — new `02_appendix_descriptives.do`, 2026-05-07] *(Item 11)* Re-think Table A1: one table per method (SDID, IRS county-to-county flow, ACS individual data).
  → Three method-specific tables synced to Overleaf:
    - `tableA1_sdid.tex`: 2 panels × 6 rows × 7 cols, time-pooled means by donor pool.
    - `tableA1_irs_flow.tex`: 2 panels (All / ACS-restricted) × 2 rows × 5 cols. Median n1 replaces "share with 0 movers" (IRS suppresses low-count flows).
    - `tableA1_acs.tex`: 2 panels (out / in samples) × 2 rows × 5 cols.
  → Old `tableA1_variables.tex` remains on disk; Phase 3 tex pass swaps `\input` lines to point at the three new files.

* [PENDING — Phase 3 tex pass] *(Item 12)* Drop Table A2.

* [DONE — `02_spec_engine.do`, 2026-05-06] *(Item 13)* Check extraneous quotation marks in elasticity measures.
  → Dropped `string asis` from `cap()` and `cols()` in `elast_tex_open`. Tables now have clean `\caption{...}` and `\begin{tabular}{...}`.

* [DONE code-side; PENDING tex pass for Metro-tax appendix counterpart] *(Item 14)* Table 2: footnote text after "each post year" was garbled; ensure last three columns are equally spaced and centered. Confirm whether the ATR used in elasticities includes the Metro tax — if not, add appendix table including Metro tax.
  → Math fix: rephrased to avoid `$h$` / `$H$` / `$T$` / `$s_\text{...}` patterns Stata's macro engine eats; used `char(96)+char(96)` for `` `` ``. Last 3 cols already centered with `ccc`.
  → ATR + Metro tax: SHS variant `tbl_elasticities_shs.tex` already exists; will be included as appendix counterpart in Phase 3 tex pass.

* [DONE — `02_tables_figures.do`, 2026-05-07] *(Item 15)* Table A3: `\footnotesize` above title; replace flow elasticities with stock elasticities.
  → Stock-elasticity column replaces flow column in `tbl_elasticities_inout.tex` (PFA + SHS). Header changed to "Stock ε". Notes updated. `\footnotesize` placement uses `char(92)`.

* [DONE — `02_tables_figures.do`, 2026-05-07] *(Item 16)* Add appendix table with coefficients and SEs from preferred SDID estimates.
  → New `results/sdid/tab_sdid_preferred.tex` (synced to Overleaf). 4 specs × 3 directions with τ̂, SE, N counties.

* [PENDING — Phase 3 tex pass] *(Item 17)* Add Appendix B section on the conditional means regression model — data, controls, equations. Pull from main.tex as needed.

* [PENDING — Phase 3 tex pass] *(Item 18)* Appendix B3: include relevant SDID equations, pulling from main.tex as needed.

* [PENDING — Phase 3 tex pass] *(Item 19)* Appendix B6: add a description of the bootstrap procedure for standard errors.

* [PENDING — Phase 3 tex pass + asset copies] *(Item 20)* Add Appendix C — IRS Migration Data Quality appendix from main.tex. Review and include figures from `results/appx_irs_data` as needed.

* [PARTIAL — code in `02_flow_analysis.do` is staged; some Phase 3 tex] *(Item 21)*
  * [PENDING] (a) `fig_strip_agi_mult.png` (and matching n1, n2) → Appendix referencing overall migration rate in Multnomah for context.
  * [PENDING] (b) DiD results table (use `results/did/tab_did_combined.tex`).
  * [DONE code-side; AUTO-GENERATES on next 02_flow_analysis.do run] (c) Flow results table parallel to DiD table.
    → Sample-tagged `estimates store` calls added inside the sample loop. End-of-script esttab block produces `results/flows/tab_flow_regression.tex` + auto-syncs to Overleaf when `${overleaf}=1`. Triggering requires re-running the full PPML pipeline (~30 min).
  * [PENDING] (d) Conditional mean figures for education and age (from `results/individual`).
  * [PENDING] (e) Specification curves for N1 and N2 (Net, Out, In; out-of-state and county-level). Assets in Overleaf.
  * [PENDING] (f) Influence figures from `results/sdid/influence` for AGI net/in/out.

---

# Workstream 2: Bootstrap implementation

Reflects the post-refactor Stata pipeline currently in the repo:
`02_revenue_microsim.do`, `02_spec_engine.do`, `02_post_spec.do`, `02_tables_figures.do`.
The old `02_elasticities.do` review notes were superseded by the items below.

## Section A - Active pipeline items

- [x] **Phase A spec-engine refactor landed in the repo** - the arithmetic that
  used to live in `02_elasticities.do` is now split across
  `02_spec_engine.do`, `02_post_spec.do`, and `02_tables_figures.do`, and the
  orchestrator calls the new files.

- [ ] **TODO-1.1: Bootstrap CIs for derived elasticities and revenue loss**
  - This is the main remaining methodological blocker for public release.
  - Target outputs:
    - percentile CIs for highlighted elasticity tables
    - percentile CIs for stock elasticities
    - percentile CIs for revenue-loss tables
  - Working assumption:
    - bootstrap is now the active path
    - delta-method `nlcom` is a fallback only if the bootstrap path stalls

- [ ] **Path-bootstrap deduplication (C2)**
  - Several analysis files still carry a path-detection preamble that overlaps
    with `00_stata_config.do`.
  - Medium cost, modest reward. Do after the methodological work, not before.

- [ ] **Optional Phase C renumbering**
  - Still worth considering after the bootstrap work is stable.
  - Not a blocker for correctness or release.

---

## Section B - Bootstrap implementation checklist

### B1. Extend `02_spec_engine.do`

- [x] Add `load_spec_panel`
  - Loads the correct panel for a given `sample_data`.
  - Returns the pre-SDID state currently assembled inside
    `02_sdid_analysis.do`.

- [x] Add `fit_spec_sdid, rclass`
  - Inputs:
    - `sample_data`
    - `sample`
    - `outcome`
    - `controls`
    - `exclusion`
    - `event_study`
    - `vce()`
  - Returns:
    - `r(tau)`
    - `r(se)`
    - `r(pre_mean)`
    - `r(event_taus)`

- [x] Add `donor_resample`
  - Resample donor counties with replacement.
  - Keep Multnomah fixed.
  - Rename duplicate donor draws to unique unit IDs.

### B2. Rewire `02_sdid_analysis.do`

- [x] Replace the inline SDID estimation block with `fit_spec_sdid`.
- [x] Preserve the current external contract:
  - `sdid_results.dta`
  - `sdid_event_results.dta`
  - parallel behavior
  - resume behavior
- [x] Verify point estimates are unchanged after the rewire.
  - V1 partial cf (594 specs from a partial pre-rewire baseline against the
    same-key subset of post-rewire results) shows zero mismatches in `tau`,
    `pre_mean`, `n_counties`, and all identifiers. SE/pval/CI columns differ
    on 100% of rows because `vce(placebo)` runs random donor relabelings
    without per-spec seed reset — that divergence is RNG noise inherent to
    the existing code path, not a rewire bug. See
    `quality_reports/plans/done/2026-04-26_sdid-rewire-v1-verification.md`.

### B3. Create `02_bootstrap.do`

- [x] Add a bootstrap driver that:
  - loads highlighted specs from the current spec grid
  - resamples donor counties
  - re-fits SDID
  - calls `compute_spec_elasticities`
  - calls `compute_spec_revenue`
  - streams draws to `bootstrap_draws.dta`

- [x] Add a manifest alongside bootstrap outputs capturing:
  - bootstrap reps
  - bootstrap seed
  - worker count / parallel mode
  - spec subset used
  - script name and date

- [x] Make bootstrap seeds deterministic across workers
  - per-rep seed = master_seed + bootstrap_seed_offset + 997 * rep
  - independent of worker_id or rep-block partition

- [x] Define a restart strategy before implementation
  - driver writes `bootstrap_draws_worker_<k>.dta` under `results/bootstrap/shards/`
  - worker 0 (default) also publishes `bootstrap_draws.dta`
  - multi-worker merge script is a separate follow-up

- [x] Live smoke test (`bootstrap_reps=20`) — passed 2026-04-27.
  - 480 draw rows completed, 0 fit failures, 0 `vce` syntax errors.
  - 160/160 net rows populated for every stock column (`stock_total_*`,
    `stock_imp_*`). Sanity-check warning at `02_bootstrap.do:413` did
    not fire.
  - Required two engine fixes along the way:
    `02_spec_engine.do:476-485` translates `noinference` -> `vce(off)`
    for `sdid_event` (its allowlist is `{off, placebo, bootstrap}`),
    and `02_spec_engine.do:507-525` reads `e(H)` shape dynamically
    instead of assuming the 5-column / `r(N)+1`-row layout that
    `vce(placebo)` produces (under `vce(off)`, `e(H)` is 3-col, 1+N_post
    rows).
- [x] Confirm `sdid_event` accepts `vce(noinference)` on installed version.
  - **No** — `sdid_event`'s `vce()` allowlist is `{off, placebo, bootstrap}`
    on the installed version (error: `Only off, placebo and bootstrap
    (dafalt) allowed`). Fixed 2026-04-27 in `02_spec_engine.do:476-485`
    by translating the bootstrap's `noinference` signal to `vce(off)`
    when calling `sdid_event`. Same intent (skip inner inference loop;
    uncertainty comes from the outer rep loop) but uses the option name
    `sdid_event` actually accepts.
  - driver now emits a WARNING if no net-spec produces `stock_total_common`.

### B3.5. Parallel launcher and shard combiner

The B3 driver supports multi-worker execution out of the box (disjoint
`bootstrap_rep_start`/`bootstrap_rep_end` ranges, per-worker shards,
worker-order-independent per-rep seeds). Two follow-up scripts complete
the multi-worker path. See plan §B3.5 for design details.

- [x] Create `02_bootstrap_combine.do` (2026-04-27)
  - iterates `results/bootstrap/shards/bootstrap_draws_worker_*.dta`
  - appends and `duplicates drop rep spec_id, force` (safety net)
  - writes canonical `${bootstrap_output}` (= `bootstrap_draws.dta`)
  - rewrites manifest with `K` and completed rep ranges
  - fails loudly on shard gap or rep-range overlap
  - idempotent — safe to rerun without re-firing workers
  - Self-test passed on K=1 single-shard smoke output: 480 rows in,
    480 rows out, validation messages clean, no dedup drops.
  - Implementation note: validation logic uses local-array variables
    (`ms_1, ms_2, ...; rs_1, rs_2, ...; order_1, ...`) rather than a
    metadata dataset, because a top-level `do` only allows one active
    `preserve` and we need to swap `use` between shards to read first-row
    metadata.

- [x] Add parallel launcher (2026-04-27, bash wrapper variant)
  - `code/stata/run_bootstrap_parallel.sh` — fires K independent
    `StataMP-64 /e` processes with worker-specific globals via the
    `_bootstrap_worker.do` shim, `wait`s on each PID individually
    (so any non-zero exit is detected), then invokes the combine
    script. Skips combine and exits rc=1 if any worker failed —
    shards stay on disk for inspection.
  - `code/stata/_bootstrap_worker.do` — six-line shim that reads
    three positional args (`worker_id rep_start rep_end`), promotes
    them to globals, sources `02_bootstrap.do`. Closes the auto-batch
    log immediately so K concurrent workers don't race on the
    `_bootstrap_worker.log` write target.
  - Rep partitioning: `floor(N/K)` per worker; first `(N mod K)`
    workers get one extra. Yields contiguous `[1..N]` cover with no
    gaps, which `02_bootstrap_combine.do`'s validation enforces.
  - Usage: `bash code/stata/run_bootstrap_parallel.sh K N` from repo
    root. `STATA_EXE` env var overrides the default Stata path.
  - Single-worker dev loop unchanged — keep using
    `do "code/stata/02_bootstrap.do"` directly.

- [x] Fix B3's worker-0-publishes-canonical hardcoding (2026-04-27,
  resolved by design rather than code change)
  - Concern was that `02_bootstrap.do:434-436` writes
    `bootstrap_draws.dta` when `bootstrap_worker_id == 0`, which would
    publish only worker 0's reps as canonical in multi-worker mode.
  - Resolution: the launcher always invokes `02_bootstrap_combine.do`
    after all workers exit, and combine overwrites the canonical with
    the K-way union. Worker 0's intermediate publish is briefly wrong
    in multi-worker mode but is corrected before the launcher returns.
    For K=1, worker 0's publish is the canonical — convenience preserved
    for the single-worker dev loop. No code change needed.
  - Failure mode to watch: if combine errors after worker 0 publishes
    but before union completes, canonical contains only worker 0's
    reps and looks valid. Mitigated by combine's contiguity validation
    (would fail loudly if shard coverage doesn't span [1..N]) and by
    the launcher's exit code 1 on combine failure.

- [x] V2.5 parallel parity test (passed 2026-04-28)
  - K=1 × 20 reps vs K=4 × 5 reps on `master_seed=56403`,
    `bootstrap_seed_offset=60000`. Sorted by `(rep, spec_id)`, `cf` on
    all 33 bootstrap-content columns (the 36-col schema minus the
    three metadata cols `worker_id`, `rep_start`, `rep_end` that
    legitimately differ across shards) returned **zero mismatches**.
  - 480 rows in both (24 specs × 20 reps); rep ranges [1..5] [6..10]
    [11..15] [16..20] in K=4 reconstruct K=1's [1..20] cleanly — no
    worker-order seed leak, no partition gap/overlap, no silent
    combine-dedup drops.
  - V2.5 cleared. Proceed to V3 (100 reps) and V4 (500 reps).
  - Test driver: `sandbox/_v25_parity_test.do`. Captured log:
    `quality_reports/parity_logs/2026-04-28_v25_parity_test.log`.
  - Original recipe (run from repo root):

    **Bash terminal (Git Bash, etc.):**
    ```
    cp results/bootstrap/bootstrap_draws.dta results/bootstrap/bootstrap_draws_K1.dta
    rm -rf results/bootstrap/shards/*.dta
    bash code/stata/run_bootstrap_parallel.sh 4 20
    ```

    **PowerShell equivalent:**
    ```
    Copy-Item results/bootstrap/bootstrap_draws.dta results/bootstrap/bootstrap_draws_K1.dta
    Remove-Item results/bootstrap/shards/*.dta -Force
    bash code/stata/run_bootstrap_parallel.sh 4 20
    ```
    (the launcher itself must be invoked through `bash` in either
    shell — its body is bash, not PowerShell.)

    Then in Stata:
    ```
    use "results/bootstrap/bootstrap_draws.dta", clear
    sort rep spec_id
    cf _all using "results/bootstrap/bootstrap_draws_K1.dta", verbose
    ```
    Acceptance: zero mismatches across all numeric columns.

### B4. Create `02_bootstrap_tables.do`

- [ ] Collapse bootstrap draws to percentile intervals by spec.
- [ ] Write `bootstrap_cis.dta`.
- [ ] Merge bootstrap CIs into the current table-rendering workflow.

### B5. Update `02_tables_figures.do`

- [ ] Add a `${show_bootstrap_cis}` flag.
- [ ] Keep CI-off behavior bit-for-bit compatible with current point-estimate
  outputs.
- [ ] When CI-on, render `[lo, hi]` rows or equivalent CI strings in the
  highlighted tables.

- [ ] Keep the current Excel workbook contract stable:
  - `recalc_components` remains point-estimate inputs and outputs
  - `run_parameters` remains the shared denominator/scalar sheet
  - `preferred_net_stock` and `preferred_net_stock_shs` remain simple
    presentation sheets
  - `variable_guide` remains the workbook dictionary

- [ ] Add bootstrap results in a new sheet instead of mutating the existing
  point-estimate sheets
  - recommended name: `bootstrap_cis`

### B6. Documentation and paper caveat

- [x] Update table notes to say bootstrap CIs come from donor-cluster
  resampling. (2026-04-28)
  - Added `elast_tex_notes_inference` helper to `02_spec_engine.do`
    that emits inference-language tablenotes for the 6 elasticity
    tables. Branches on `${show_bootstrap_cis}`:
    - CIs off: explicit attribution of parenthetical SEs to SDID
      placebo inference for $\hat{\tau}$ and the implied $\beta$,
      with a stock-elasticity caveat (where present) noting that
      analytic SEs require joint event-study covariances we don't
      export.
    - CIs on: bracketed values described as `${ci_level}\%`
      percentile CIs from a donor-cluster bootstrap, with revenue
      and tax parameters held fixed.
  - All 6 elasticity tables (`tbl_elasticities`, `_stock_compare`,
    `_inout`, `_shs`, `_stock_compare_shs`, `_inout_shs`) wired in.
  - Verified end-to-end via 02_tables_figures.do at both
    `${show_bootstrap_cis}=0` and `=1`.

- [x] Add the paper caveat that microsimulation denominators are treated
  as fixed inside the current bootstrap. (2026-04-28)
  - The CI-on tablenote explicitly enumerates: federal, Oregon,
    FICA, PFA rates, AGI base, and microsimulation denominators are
    treated as fixed throughout the bootstrap. This text is the
    canonical caveat; lift verbatim into paper prose where needed.
  - `variable_guide` Excel sheet descriptions for `_se` columns
    updated to: "Propagated from the SDID treatment-effect placebo
    SE; treats revenue and tax parameters as fixed."

- [x] Update `quality_reports` docs and `todo.md` once the bootstrap path
  is live. (2026-04-28)
  - Session log: `quality_reports/session_logs/2026-04-28_b7-parallel-migration.md`
  - todo.md Section B7 entry tracks migration status and lessons.
  - Cross-project Stata lessons saved to memory `stata-tips.md`.

### B7. Migrate bash launcher to Stata `parallel` (cross-platform)

  Motivation: bash launcher (`run_bootstrap_parallel.sh` +
  `_bootstrap_worker.do`) only works under Git Bash on Windows. The
  project is targeting a fully reproducible public release, so the
  parallel path needs to run on macOS and Linux without modification.
  Vega's `parallel` package is already used in the repo
  (`02_sdid_analysis.do`, `02_otherout_sdid.do`, `02_flow_analysis.do`).

  Plan: `quality_reports/plans/` has the design (or
  `~/.claude/plans/purrfect-dazzling-bee.md`).

  **Run procedure for the remaining V2.5 / V3 / V4 steps:
  `quality_reports/B7_RUN_PROCEDURE.md`** — pre-flight, command
  sequences, acceptance criteria, failure modes, and resume mechanism.

- [x] Rewrite `02_bootstrap.do` (2026-04-28)
  - Add `run_bootstrap_rep` program: takes one rep number, computes
    seed, caches panels, fits 24 specs, writes per-rep .dta to
    `${results}bootstrap/temp_draws/draws_rep_<rep>.dta`.
  - Add `parallel_bootstrap_wrapper` program: re-sources
    `02_spec_engine.do` (avoids version-drift trap), loops worker's
    rep slice.
  - Top-level driver branches on `${use_parallel}`. Parallel path
    invokes `parallel, prog(...) processors(c(processors_max)):
    parallel_bootstrap_wrapper` — `processors()` is critical, default
    is `set processors 1` which makes SDID single-threaded.
  - Aggregate logic (append per-rep files, validate rep coverage,
    write canonical) inlined at end of script — replaces
    `02_bootstrap_combine.do`.

- [x] Extend `setup_parallel` in `01a_programs.do` with core-aware
  cap (2026-04-28). Auto-caps `${n_clusters}` to
  `floor(physical_cores / per_mp_cores)`. Reads physical cores from
  `NUMBER_OF_PROCESSORS` env var on Windows, `sysctl -n hw.ncpu` on
  macOS, `nproc` on Linux. Skips cap with warning if detection fails.
  Derives `per_mp_cores` from `c(processors_max)` (license cap, not
  hardcoded 4).

- [x] Update `00_multnomah.do` (2026-04-28). Bootstrap block trimmed
  from 3 stages to 2 (driver → tables); `${skip_bootstrap_driver}`
  removed.

- [x] Rename obsolete files to `.legacy` (2026-04-28):
  `run_bootstrap_parallel.sh.legacy`, `_bootstrap_worker.do.legacy`,
  `02_bootstrap_combine.do.legacy`. Final delete after V2.5 re-passes.

- [x] N=2 K=2 minimal smoke (2026-04-28, 11:54). 48 rows, 2 reps,
  parallel scratch artifacts cleaned, temp_draws/ removed. Wall-clock
  ~15 min (1 rep per worker at K=2, with 4-core MP license shared
  between 2 workers ≈ 2 effective cores per worker).

- [x] Profile diagnostic (2026-04-28). Per-rep cost breakdown:
  grid-load 0s, panel-cache 1.2s, spec-loop 533s (22s/spec),
  metadata-save 0s. Spec loop dominates — bound by SDID solver and
  the 4-core MP license. Confirms parallel migration does not add
  meaningful overhead vs the bash launcher's per-rep cost.

- [ ] V2.5 parity re-run (in progress — launched 2026-04-28 ~11:55).
  N=20 K=2 parallel vs `bootstrap_draws_n20.dta` (bash K=4 N=20
  baseline archived this morning). Acceptance: zero mismatches on
  the 33 content columns. ETA ~2-2.5 hr.

- [ ] Delete `.legacy` files once V2.5 confirms parity.

- [ ] V3 (N=100 K=2 parallel) — overnight job, ETA ~12-13 hr.

- [ ] V4 (N=500 K=2 parallel) — publication run.

### Lessons learned (B7 incident notes)

- **Stata 19 doesn't have `c(processors_machine)`.** Use OS-side
  detection (env var or shell-out) for physical core count. License
  cap (`c(processors_max)`) is the per-instance demand, not the
  machine total.
- **Vega's `parallel` defaults each worker to `set processors 1`.**
  Required `processors(N)` option to give workers full multi-core
  speed. Default behavior was 10× slower per spec without the option.
- **The Stata MP license is GLOBALLY capped** at the licensed core
  count across all instances. K=2 instances each requesting 4 cores
  share 4 cores → 2 effective cores each. Throughput is bounded at
  `min(K, license_cap)` cores total.
- **Earlier "1.2 min/rep" baseline was wrong.** Actual K=1 N=20 took
  2 hr 44 min = 20.5 sec/spec. K=4 N=20 took ~2 hr. Both bound by
  the 4-core license — bash K=4 didn't run faster than K=1 on this
  machine because workers shared cores. The parallel migration
  doesn't hurt throughput meaningfully.

---

## Section C - Acceptance criteria for TODO-1.1

- [x] `02_spec_engine.do` contains `load_spec_panel`, `fit_spec_sdid`, and
  `donor_resample`.
- [x] `02_sdid_analysis.do` calls `fit_spec_sdid` and reproduces current
  `sdid_results.dta` and `sdid_event_results.dta` point estimates.
  Verified via partial cf 2026-04-27 — see B2 note above.
- [ ] `02_bootstrap.do` runs successfully at:
  - [x] 20 reps for development (passed 2026-04-27 via bash launcher;
    V2.5 re-running 2026-04-28 via parallel migration — see B7)
  - [ ] 100 reps for stress testing (V3 — ETA ~12-13 hr at K=2 parallel
    on 8-core box with 4-core MP license)
  - [ ] 500 reps for publication tables (V4)
- [x] `02_bootstrap_tables.do` produces `bootstrap_cis.dta` (2026-04-28).
- [x] `02_tables_figures.do` renders both: (B5, 2026-04-28)
  - current point-estimate outputs with CIs off (5/5 valid baselines
    byte-identical; 6th had stale before-snapshot but new OFF output
    has zero `[lo, hi]` content)
  - bootstrap-CI outputs with CIs on (all six tables exhibit the
    expected `[lo, hi]` substitution / addition; SHS variants use
    `_shs` CI columns correctly)
- [x] The Excel workbook includes a bootstrap CI sheet without breaking
  the current point-estimate sheets (2026-04-28). Sheet name
  `bootstrap_cis`; placeholder row when flag is off, 24 spec rows when
  on. `recalc_components`, `run_parameters`, `preferred_net_stock`,
  `preferred_net_stock_shs`, `variable_guide` all unchanged.
- [x] Table footnotes no longer imply that the current analytic SEs are
  fully correct for cumulative stock elasticities. (B6 — closed
  2026-04-28; see Section B6 above for the helper architecture.)

---

## Section D - Deferred / not doing for now

- **Shared `build_sdid_sample` extractor**
  - Still not worth doing unless another SDID variant is added.

- **Microsimulation bootstrap**
  - Out of scope for the current release cycle.
  - Can be added later if a full uncertainty envelope becomes necessary.

- **Delta-method interim CIs**
  - Not the active plan.
  - Can be revived if bootstrap timing becomes a problem.
