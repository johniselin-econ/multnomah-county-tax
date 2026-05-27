# TODO - Multnomah County Tax

Three active workstreams:

1. **Paper revision** — DONE; Phase 4 visual review of compiled PDF pending.
2. **Bootstrap implementation** — V4 (500 reps) publication run is the final release blocker.
3. **Pipeline hygiene** (added 2026-05-16) — findings from the critical-code-reviewer pipeline pass.

---

# Workstream 1: Paper revision

Original instructions: identify code vs tex changes, work through code changes
first, then make the tex changes in a single pass. For each change consider
references elsewhere in the text and the code-to-Overleaf pipeline.

Plan file: `quality_reports/plans/2026-05-06_paper-revision-todos.md`

## Status (last updated 2026-05-10)

- Code-side: ALL 12 items DONE (13, 14, 15, 9, 10, 16, 3, 21c, 2, 4, 8, 11).
- Tex-side: Phase 3 tex pass DONE (2026-05-10 session). All items 1, 5, 6, 7,
  12, 14 (tex tail), 17, 18, 19, 20, 21a, 21b, 21c, 21d, 21e, 21f resolved on
  `Conway_Iselin_Rork_2026.tex` (renamed from `updated.tex`). Item 22 orphan-ref
  audit clean.
- Next: Phase 4 — compile in Overleaf and visual review the rendered PDF
  (figure spacing, table layout, equation numbering, Appendix B/C math).

## Items

* [DONE — `Conway_Iselin_Rork_2026.tex` Phase 3 tex pass, 2026-05-10] *(Item 1)* Drop the equations from the SDID text (4.1) — those are DID equations, not SDID equations.
  → §4.1 had no equations to begin with. Cleanup landed in Appendix B `sec:appb_sdid`: removed the standalone `eq:did` baseline and `eq:eventstudy` extension display equations; rewrote the lead paragraph to define notation directly and go straight into the SDID weighted least-squares problem (`eq:sdid`). Event-study extension is now prose. No remaining `\ref{eq:did}` or `\ref{eq:eventstudy}` in the document.

* [DONE — code in `code/R/map_code.R`, 2026-05-07] *(Item 2)* Figure 1: OR/WA area with a Portland cutout that includes the Average Marginal Tax rate shading, with the legend below the full figure.
  → New `map_combined_tax.png` produced and synced to Overleaf. Built from `map1_with_box` overview + new `map2_tax_inset` (tax-shaded close-up, internal legend suppressed) + horizontal tax-rate legend strip below. Phase 3 tex pass switches updated.tex Fig 1 from `map2_tax.png` to `map_combined_tax.png`.

* [DONE — `code/R/fig_diagrams.R`, 2026-05-07] *(Item 3)* Figures 2 and 3: bigger text, no colored boxes; Figure 3 drop trailing "Outcome variables", "Key Controls", "Donor Pool Restrictions".
  → Bumped `tx()` font sizes by ~2 points; replaced colored fills with `NA`; deleted bottom-info section in `draw_empirical_approach`. Re-rendered and synced to Overleaf.

* [DONE — `fig_diagrams.R` and `02_tables_figures.do`, 2026-05-07] *(Item 4)* All figures: produce versions with and without titles/subtitles/notes; paper uses bare versions.
  → R-side conceptual diagrams now produce two variants: `fig_*.pdf` (paper) and `fig_*_titled.pdf` (slides). Stata-side `${clean_figs}` global toggle (default 0) wired through both preferred-overlay event-study blocks. Maps were already title-free. Spec curves keep minor titles for now.

* [DONE — Phase 3 tex pass, 2026-05-10] *(Item 5)* Figure 6: use the OR/WA versions in main, West Coast in appendix. Already in `Conway_Iselin_Rork_2026.tex` — Fig 6 uses `map_directional_agi_{out,in}_orwa.png`, West Coast variant is `fig:flow_maps_westcoast`.

* [DONE — Phase 3 tex pass, 2026-05-10] *(Item 6)* Figure 7: stack the two event-studies vertically. Already in `Conway_Iselin_Rork_2026.tex` — `fig:ppml_events` uses two full-width subfigures stacked with `\\[0.5em]`.

* [DONE — Phase 3 tex pass, 2026-05-10] *(Item 7)* Figure 10: use newly-created specification curves. Already in `Conway_Iselin_Rork_2026.tex` — Fig 10 uses `fig_speccurve_revenue_{pfa,oregon}.pdf`.

* [DONE — `02_descriptives_supp.do`, 2026-05-07] *(Item 8)* Table 1: drop Panel B; expand Panel A by replicating the structure for ACS as a new Panel B; add a counties column; include all five samples.
  → New `table1_combined.tex`: 2 panels (IRS + ACS College) × 6 rows × 9 cols. IRS uses 2018-19 vs 2021-22; ACS uses 2018-19 vs 2021-24. Synced to Overleaf.

* [DONE — `02_tables_figures.do`, 2026-05-07] *(Item 9)* Elasticity figures: clearer y-axis labels (e.g. "Migration Stock Elasticity"); define mathematically in the note.
  → Y-axis labels updated to "Migration Semi-Elasticity (β)", "Migration Stock Elasticity", "Migration Flow Elasticity" with PFA+SHS variants. Math definitions go in figure notes during Phase 3 tex pass.

* [DONE — `02_tables_figures.do`, 2026-05-07] *(Item 10)* Add Figure A3 counterparts for flow elasticities and semi-elasticities.
  → 4 new flow-elasticity distribution figures: `fig_speccurve_elast_flow_in.pdf`, `_out`, +SHS variants. Synced to Overleaf.

* [DONE — new `02_appendix_descriptives.do`, 2026-05-07] *(Item 11)* Re-think Table A1: one table per method (SDID, IRS county-to-county flow, ACS individual data).
  → Three method-specific tables synced to Overleaf:
    - `tableA1_sdid.tex`: 2 panels × 6 rows × 7 cols, time-pooled means by donor pool.
    - `tableA2_irs_flow.tex`: 2 panels (All / ACS-restricted) × 2 rows × 6 cols (adds unique-county count). Median n1 replaces "share with 0 movers" (IRS suppresses low-count flows).
    - `tableA3_acs.tex`: 2 panels (out / in samples) × 2 rows × 5 cols.
  → Old `tableA1_variables.tex` remains on disk; Phase 3 tex pass swaps `\input` lines to point at the three new files.

* [DONE — Phase 3 tex pass, 2026-05-10] *(Item 12)* Drop Table A2. Old `\input{tables/diagnostics_obs_counts}` is no longer in `Conway_Iselin_Rork_2026.tex`.

* [DONE — `02_spec_engine.do`, 2026-05-06] *(Item 13)* Check extraneous quotation marks in elasticity measures.
  → Dropped `string asis` from `cap()` and `cols()` in `elast_tex_open`. Tables now have clean `\caption{...}` and `\begin{tabular}{...}`.

* [DONE — Phase 3 tex pass, 2026-05-10] *(Item 14)* Table 2: footnote text after "each post year" was garbled; ensure last three columns are equally spaced and centered. Confirm whether the ATR used in elasticities includes the Metro tax — if not, add appendix table including Metro tax.
  → Math fix: rephrased to avoid `$h$` / `$H$` / `$T$` / `$s_\text{...}` patterns Stata's macro engine eats; used `char(96)+char(96)` for `` `` ``. Last 3 cols already centered with `ccc`.
  → ATR + Metro tax: SHS variant `tbl_elasticities_shs.tex` is included as Appendix Table A3 (`\input` at `Conway_Iselin_Rork_2026.tex:493`) and referenced from `sec:elasticities` as `\ref{tab:elasticities_shs}`.

* [DONE — `02_tables_figures.do`, 2026-05-07] *(Item 15)* Table A3: `\footnotesize` above title; replace flow elasticities with stock elasticities.
  → Stock-elasticity column replaces flow column in `tbl_elasticities_inout.tex` (PFA + SHS). Header changed to "Stock ε". Notes updated. `\footnotesize` placement uses `char(92)`.

* [DONE — `02_tables_figures.do`, 2026-05-07] *(Item 16)* Add appendix table with coefficients and SEs from preferred SDID estimates.
  → New `results/sdid/tab_sdid_preferred.tex` (synced to Overleaf). 4 specs × 3 directions with τ̂, SE, N counties.

* [DONE — Phase 3 tex pass, 2026-05-10] *(Item 17)* Conditional means regression model: `sec:appb_condmean` in Appendix B with `eq:condmean` derivation from main text.

* [DONE — Phase 3 tex pass, 2026-05-10] *(Item 18)* SDID equations in Appendix B: `sec:appb_sdid` with `eq:sdid` (weighted least-squares problem, unit + time weights). DiD baseline equations dropped per Item 1.

* [DONE — Phase 3 tex pass, 2026-05-10] *(Item 19)* Bootstrap procedure: `sec:appb_bootstrap` describes the donor-cluster bootstrap with deterministic per-rep seeds; donor-pool resampling rationale; treatment of fixed parameters.

* [DONE — Phase 3 tex pass, 2026-05-10] *(Item 20)* Appendix C "IRS Migration Data Quality": `sec:appendixc` with proper `\setcounter{figure}{0}` and `\renewcommand{\thefigure}{C\arabic{figure}}` block; three subsections (IRS time-series, YoY changes, ACS cross-validation) and 6 figures from `results/appx_irs_data/`.

* [DONE — Phase 3 tex pass, 2026-05-10] *(Item 21)*
  * [DONE] (a) `fig:strip_mult` block (lines 965-990) with AGI / N1 / N2 panels. Referenced from §5 intro after Table 1 discussion.
  * [DONE] (b) `tab:did_combined` block (lines 498-511) wrapping `\input{tables/tab_did_combined}`. Referenced from §5.3.
  * [DONE] (c) `tab:flow_regression` block now active in `Conway_Iselin_Rork_2026.tex` (table file `tab_flow_regression.tex` materialized in Overleaf). Caption + threeparttable wrapper added; referenced from §5.2.
  * [DONE] (d) `fig:condmean_educ_age` block with educ_1, educ_2, age_1, age_2 subfigures. Referenced from §5.3.
  * [DONE] (e) `fig:speccurves_n1`, `_n1_outstate`, `_n2`, `_n2_outstate` blocks (with `\ContinuedFloat` for 3-panel layouts). Referenced from §5.1.
  * [DONE] (f) `fig:sdid_influence_agi` and `_outstate` blocks. Referenced from `sec:appb_sdid`.

* [DONE — Phase 3 tex pass, 2026-05-10] *(Item 22)* Audit: every appendix figure / table has at least one `\ref` from main text or appendix prose.
  - 2026-05-10 re-audit (`sandbox/_check_orphans.R`):
    - 0 table orphans
    - 0 figure orphans (the 40 flagged are all subfigure labels of parent figures
      that ARE referenced — conventionally allowed)
    - 3 equation orphans remain (`eq:sdid`, `eq:semi_elast`, `eq:stock_elast`) —
      appendix display equations whose surrounding prose discusses them
      descriptively; acceptable.
  - All previously-flagged orphans (A3–A6, A7, A8, A15, A16, C4, C5) now have
    proper `\ref` from prose. See session log for placement details.
  - Re-check after any new appendix figure is added going forward.

## 2026-05-10 follow-ups (post-Phase-3)

These changes happened in the same session as the Phase 3 tex pass but address
issues that surfaced after the original 22-item audit was framed. Recorded
here so a future reader doesn't re-investigate.

* [DONE] **Table A2 column labels rewritten** (`tables/tableA2_irs_flow.tex`,
  upstream `code/stata/02_appendix_descriptives.do:396-397`).
  - Header now reads: "Number of county-flows / Median count of returns /
    Mean count of returns / Mean count of exemptions / Mean AGI (USD
    thousands)".
  - Replaces the prior cryptic abbreviations (`N flows`, `Median n1`, etc.).

* [DONE] **Table A6 width fix** (`tables/tab_did_combined.tex`, upstream
  `code/stata/02_did_analysis.do` esttab `prehead`/`postfoot`).
  - Switched from `\begin{tabular}{l*{4}{w{c}{3cm}}}` to
    `\begin{tabular*}{\textwidth}{@{\extracolsep{\fill}}l*{4}{c}@{}}` and
    matching `\end{tabular*}` close.
  - Table now spans the full text width with evenly distributed column
    spacing. No new packages.

* [DONE — design decision, NOT a code fix] **Stock ε column dropped from
  gross in/out tables** (`tables/tbl_elasticities_inout.tex`,
  `tables/tbl_elasticities_inout_shs.tex`, upstream
  `code/stata/02_tables_figures.do` blocks (c) and (d), helper
  `elast_inout_panel`, plus main-text sentence at
  `Conway_Iselin_Rork_2026.tex:149`).
  - **Why:** the prior column was all `--` because two upstream gates
    restrict stock-elasticity computation to `migration == "net"`:
    - `code/stata/02_post_spec.do:265-274` — only passes
      `eventtaus(...)` for `mig_i == "net"` specs; gross specs never
      hand the matrix to the engine.
    - `code/stata/02_spec_engine.do:848` — accumulator gate is
      `if "`migration'" == "net" & "`event_taus'" != ""` so even with
      eventtaus passed, `r(stock_common) = .` for gross specs.
  - The `event_tau2021…2024` wide columns ARE populated for all specs
    (line 189-190 reshape doesn't filter migration), so the data are
    available — the gates are deliberate. Math-wise the formula
    $\Delta\ln S_h = \ln(1 + \tau_h/100 \cdot s_{scale})$ works for any
    $\tau$, with sign conventions matching the existing $\beta$ column
    (out negative, in positive, net positive).
  - **Decision:** rather than relax the gates, the user chose to drop
    the column. "Stock elasticity for gross out-migration" doesn't have
    an established literature meaning, and the gross tables now report
    only $\hat{\tau}$ + Kleven semi-elasticity. Net tables (Table 2 and
    `tab:elasticities_shs`) still report stock ε.
  - Code change preserves the helper signature minus the `FLOWCIVAR`
    option; both gross-table writers now use the 5-column layout
    (`lll cc`).

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

- [~] **TODO-1.1: Bootstrap CIs for derived elasticities and revenue loss**
  - Pipeline complete and tested at V3 (100 reps, 2026-05-02). All target
    outputs flowing: percentile CIs for highlighted elasticity tables,
    stock elasticities, and revenue-loss tables; rendering toggled by
    `${show_bootstrap_cis}`; Excel `bootstrap_cis` sheet populated.
  - Remaining: rerun at V4 (500 reps) for the publication tables. See
    Section B7 final two items.
  - Bootstrap is the canonical inference path; delta-method `nlcom`
    fallback was not needed.

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

- [x] Collapse bootstrap draws to percentile intervals by spec. (2026-04-28)
- [x] Write `bootstrap_cis.dta`. (2026-04-28)
- [x] Merge bootstrap CIs into the current table-rendering workflow.
  (2026-04-28; see B6 helper `elast_tex_notes_inference` and Section C
  acceptance lines below.)

### B5. Update `02_tables_figures.do`

- [x] Add a `${show_bootstrap_cis}` flag. (2026-04-28)
- [x] Keep CI-off behavior bit-for-bit compatible with current point-estimate
  outputs. (2026-04-28; 5/5 baselines byte-identical, 6th had a stale
  before-snapshot but the new OFF output is content-clean.)
- [x] When CI-on, render `[lo, hi]` rows or equivalent CI strings in the
  highlighted tables. (2026-04-28; verified across all six elasticity
  tables.)

- [x] Keep the current Excel workbook contract stable: (2026-04-28)
  - `recalc_components` remains point-estimate inputs and outputs
  - `run_parameters` remains the shared denominator/scalar sheet
  - `preferred_net_stock` and `preferred_net_stock_shs` remain simple
    presentation sheets
  - `variable_guide` remains the workbook dictionary

- [x] Add bootstrap results in a new sheet instead of mutating the existing
  point-estimate sheets (2026-04-28). Sheet name: `bootstrap_cis`;
  placeholder row when flag is off, 24 spec rows when on.

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

- [x] V2.5 parity re-run — superseded by V3, which completed successfully
  (see below). The 2026-04-28 ~11:55 launch was interrupted or otherwise
  not preserved as a separate artifact, but V3 (100 reps) running cleanly
  over the same parallel path is sufficient evidence that the K=2 parallel
  pipeline works end-to-end.

- [x] Delete `.legacy` files (`run_bootstrap_parallel.sh.legacy`,
  `_bootstrap_worker.do.legacy`, `02_bootstrap_combine.do.legacy`) — confirmed
  2026-05-16 these were already removed in a prior session; nothing on disk
  or in git.

- [x] V3 (N=100 K=2 parallel) — completed 2026-05-02 (manifest:
  `results/bootstrap/bootstrap_draws_manifest.csv`, `reps=100`,
  `use_parallel=1`, `n_clusters=2`). `bootstrap_cis.dta` last refreshed
  2026-05-04. V3 numbers are what currently feed the elasticity/revenue
  tables and the abstract.

- [ ] V4 (N=500 K=2 parallel) — publication run. Required before final
  submission; replaces V3 numbers in Table 2, `tab:elasticities_shs`,
  Figure 10, the abstract, and §5.3 / §6 prose. ETA at observed
  ~22s/spec × 24 specs × 500 reps / (2 effective cores) ≈ 36–40 hr;
  plan a long weekend run.

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
  - [x] 20 reps for development (passed 2026-04-27 via bash launcher).
  - [x] 100 reps for stress testing (V3, completed 2026-05-02 via the
    parallel-migrated path; manifest at
    `results/bootstrap/bootstrap_draws_manifest.csv`).
  - [ ] 500 reps for publication tables (V4) — final blocker for release.
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

---

# Workstream 3: Pipeline hygiene

Source: critical-code-reviewer pass on the full pipeline, 2026-05-16. The
pipeline is ~23K lines (28 .do + 9 .R); review identified one real bug, a
handful of duplicated patterns, and one architectural gap (paper-artifact
tagging). Items below are grouped by priority — Section A is correctness,
Section B is the largest pending item, the rest are hygiene.

## Section A - Bugs surfaced during review (resolved)

- [x] **`02_tables_figures.do` missing `spec_narrow`** (commit 7a9460a,
  2026-05-16). Narrow donor pool was added to SDID + descriptives but not
  to the elasticity / revenue spec-curve renderers, so narrow rows landed
  as unlabeled points instead of labeled indicator rows. Added the
  indicator, label, and entries in `indic_county` / `indic_outstate` /
  `indic_instate`.

- [x] **Duplicate 22-FIPS list** across `02_sdid_analysis.do` and
  `02_diagnostics.do` (commit 7a9460a, 2026-05-16). Extracted to
  `resources/narrow_pool_fips.csv` + new `load_narrow_pool` helper in
  `01a_programs.do`. CSV is now the single source of truth.

- [x] **2012 panel extension covariate-standardization scope** (commit
  7a9460a, 2026-05-16). The in-time placebo work standardized covariates
  on the 2016+ subset only. Resolved by dropping the in-time placebo
  workstream and reverting `year < 2016` cut + original `egen std()`.

- [x] **SDID influence coefplot dropped narrow** (commit fee23c0,
  2026-05-17). Surfaced after the original review during follow-up audit
  of donor-pool plotting. `02_sdid_analysis.do:2040` `keep(...)` listed
  only levels 2-5 of `donor_pool`, so narrow's coefficient was estimated
  (`ib1.donor_pool` included level 6) but silently dropped from rendered
  figures A3 / A4. Added `6.donor_pool` to `keep()` and a `"Narrow" "Pool"`
  coeflabel. Influence PDFs regenerated 2026-05-17 14:30-14:35 via a
  one-off sandbox script (since deleted); Overleaf copies synced.

## Section B - Paper-artifact registry (largest pending item)

The pipeline copies ~95 artifacts to `${ol_fig}` / `${ol_tab}` when
`${overleaf} == 1`. The Overleaf paper actually includes ~25 (9 figures
+ 2 tables in the main body, 14 figures in the appendix). ~70 are dead
weight that get synced but never appear. No way today to ask "is this
artifact in the paper?" from the code.

- [x] **B1: Build `resources/paper_manifest.csv`** (commit 99655bf,
  2026-05-16). 95 rows parsed from `Conway_Iselin_Rork_2026.tex`: 2
  main-body tables + 8 appendix-A tables + 9 main figures (19 subpanels)
  + 18 appendix-A figures (57 subpanels) + 5 appendix-C figures (12
  subpanels). Columns: `artifact_basename, artifact_kind, paper_label,
  paper_number, location, source_script`. One row per
  (artifact, paper_label) pair so multi-panel figures expand cleanly.

- [ ] **B2: Add `project_save_overleaf` helper** to `01a_programs.do`.
  Gated on `${overleaf} == 1`; with optional `PAPERONLY` flag consults
  the manifest before copying. Replaces ~40 hand-rolled `if ${overleaf}
  == 1 { copy ... }` blocks across 9 files. Held until V4 bootstrap
  finishes (helper file is sourced by running pipeline).

- [x] **B3: Audit script** (commit 99655bf, 2026-05-16). Implemented as
  `code/R/audit_paper_artifacts.R` rather than the originally-planned
  Stata `.do` — R has much cleaner `.tex` parsing and gets the same job
  done in <100 lines. Reconciles manifest ↔ on-disk results/ ↔ live
  `.tex`; writes three CSVs and exits non-zero on MISSING (build break);
  soft-warns on DEAD (unregistered artifacts pushed to Overleaf). Run
  with `Rscript code/R/audit_paper_artifacts.R`. **Still to do:** wire
  into `00_multnomah.do` as the final step (deferred until V4 finishes,
  since the orchestrator is the running script).

## Section C - Dead-output cleanup

Decide per-artifact: include in paper (add to manifest), keep as
dev-sync (no `paperonly`), or stop generating. Audit candidates:

- [ ] `tables/table2.tex`, `tables/table1.tex` (legacy; replaced by
  `table1_combined.tex`)
- [ ] `tables/table_migration_county.tex`,
  `tables/table_migration_state.tex`
- [ ] `figures/fig_strip_*_all.png` and `figures/fig_strip_*_state_*.png`
  (paper uses `_mult` variants only)
- [ ] `tables/tableA1_{sdid,irs_flow,acs}.tex` — 5 tables tagged for
  Overleaf, paper inventory lists only 2 tables total. Confirm what
  Phase-3 tex pass actually wired in.
- [ ] `figures/fig_es_combined.png`, `figures/fig_es_agepost_*.png` (4)
- [ ] `tables/tbl_elasticities_*.tex` (6 files)
- [ ] `tables/tab_sdid_preferred.tex`
- [ ] Dynamic `tables/tab_sdid_*_*_*.tex` permutations (≥12)
- [ ] All `02_otherout_sdid.do` outputs (intentional or stop tagging?)
- [ ] All `02_narrow_sdid.do` outputs (script is now `//`-commented in
  orchestrator)

## Section D - File splits

Four files exceed 1300 lines. Each is plausibly splittable along
existing section boundaries; do AFTER the registry lands so output-path
changes can flow through both at once.

- [ ] **`02_sdid_analysis.do`** (2,180 lines) → estimate / event-study /
  spec-curve render
- [ ] **`02_tables_figures.do`** (1,908 lines) → elasticity-tables /
  elasticity-curves / sdid-overlays
- [ ] **`02_descriptives.do`** (1,852 lines) → flow-tables / strip-figures
  / table1
- [ ] **`02_revenue_microsim.do`** (1,316 lines) → parameters /
  counterfactuals / tables

## Section E - Hygiene / consistency

- [ ] Unify Overleaf gate comparison: `${overleaf} == 1` everywhere.
  Currently mixed string vs numeric in `02_tables_figures.do:980`,
  `02_did_analysis.do`, `02_flow_analysis.do`.
- [ ] Fix `n_clusters` default drift: `00_stata_config.do:56` defaults
  to 4, `00_multnomah.do:100` sets 2. Pick one.
- [ ] Resolve or delete two stale TODOs at top of
  `02_tables_figures.do:50-53` (revenue annualization check,
  dashed-line colors).
- [ ] Extract `project_standalone_init` helper for consistent sourcing
  of `00_stata_config.do` + `01a_programs.do` + `02_spec_engine.do`.
  Currently each `02_*` file does this inconsistently.
- [ ] Move `.log` files out of `code/stata/` into `${logs}`. Add stale
  ones (`_describe_sdid_results.log`, `02_tables_figures.log`) to
  `.gitignore` and delete from working tree.
- [ ] Consolidate `00_post_stata.R`, `00_download_data.R`,
  `00_multnomah.R` (13-24 lines each) into one `R/00_main.R` per the
  project's main-script naming convention.
- [ ] Add `__pllr*` (Stata `parallel` scratch) and `_describe_*.log`
  patterns to `.gitignore`; clean repo root of accumulated scratch
  files.
- [ ] Replace `fig_diagrams.R` Unicode checkmark hack (U+2714 at sz=13)
  with a font-independent rendering (`geom_segment` ✓ or vector path).

## Section F - Spec-curve consolidation (deferred)

`02_sdid_analysis.do:1500-1940` and `02_tables_figures.do:214-449`
both render spec curves with overlapping logic. `02_tables_figures.do`
factored its version into `elast_speccurve_plot`; `02_sdid_analysis.do`
did not. The recent narrow-pool addition required hand-bumping every
`yp*` index in both files.

- [ ] Move the SDID spec-curve renderer into `02_spec_engine.do` (or
  adapt to call `elast_speccurve_plot` with a schema adapter). Defer
  until after the file splits in Section D — the duplication becomes
  more visible once both blocks are in their own files.

## Section G - Parity gap (low priority)

- [ ] `02_otherout_sdid.do` builds its own donor pools from raw data
  (lines 158-231) and iterates a hardcoded 5-pool list (lines 315, 627,
  640, 818). `sample_narrow` is not extended to non-migration outcomes.
  Decision needed: extend, or document as intentional and stop tagging
  these tables for Overleaf.
