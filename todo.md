# TODO — Multnomah County Tax

Consolidated tracking doc. Combines deferred items from the 2026-04-18 Stata
review with the detailed `02_elasticities.do` TODO list (reviewed against the
`dylantmoore/stata-skill` reference files, April 2026).

---

## Section A — Pipeline-wide (from 2026-04-18 Stata review)

### Deferred refactors

- [ ] **Split `02_revenue.do` into inputs + analysis (C1)** — 1,200+ lines,
  scored 79/100. Candidate split: `02_revenue_inputs.do` (Section 0B SDID
  lookup, ACS load, tax-unit construction, TAXSIM) and
  `02_revenue_analysis.do` (PFA/Oregon revenue baseline, simulation, output).
  2-4 hour refactor; do when you're already touching revenue math.

- [ ] **Path-bootstrap deduplication (C2)** — 14 analysis .do files carry a
  ~10-line path-detection preamble that's now also in `00_stata_config.do`.
  Would need a thin `_find_config.do` sourced by each file to resolve the
  chicken-and-egg (need `${code}` to source config; config sets `${code}`).
  Medium cost, modest reward. Skip unless platform portability becomes a pain.

### Completed code-quality items

- [x] **`02_revenue.do` section numbering** — added a note at Section 2 header
  explaining Section 2B was merged in. (Commit `d6fab90`.)

- [x] **`01e_acs.do` exploratory diagnostics** — deleted `tab year`,
  `fre migrate1`, `tab migplac1`, `tab migcounty1`. (Commit `d6fab90`.)

- [x] **`01h_auxiliary.do:49-54`** — renamed `mf_*_med` to `mfcc_*_med` so
  output prefix matches input prefix in both mc/mfcc groups; updated the
  downstream `foreach varlist mc_* mfcc_*` loop. (Commit `d6fab90`.)

- [x] **`02_indiv_analysis.do`** — added `Requires: Stata 14+` to the header
  and a `version 14` directive for the `direxists()` dependency.
  (Commit `d6fab90`.)

- [x] **`02_indiv_analysis.do:263`** — already had
  `capture confirm variable _at1/_at2/_margin` guards with explicit error
  messages (lines 272-290). No change needed — done in an earlier pass.

- [x] **Resume-mode Mata cleanup** — actual location was
  `02_sdid_analysis.do:1140` and `02_otherout_sdid.do:799`, not
  `02_flow_analysis.do`. Added `capture mata: mata drop _done_set` before
  (re-)creation to clear stale state, and unconditionalized the scope-exit
  cleanup with `capture`. (Commit `d6fab90`.)

- [x] **`01d_covid.do:126`** — added `fileexists()` guard before
  `use "${data}JII Covid data.dta"`, matching the pattern in
  `01b_download.do`. (Commit `d6fab90`.)

### Deliberately not doing

- **C3 — Extract `build_sdid_sample` program.** Audited 2026-04-18. The three
  SDID files share only ~26% of their sample-construction code; a shared
  program would need ~12 parameters and would be longer than the current
  three copies. Revisit if a 4th SDID variant is added.

---

## Section B — `02_elasticities.do` (from Claude Opus 4.7 review, April 2026)

**Source file:** `code/stata/02_elasticities.do`
**Reviewed against:** `dylantmoore/stata-skill` reference files
(workflow-best-practices, programming, tables-reporting, graphics,
linear-regression).

The file is well above median craft for empirical economics: defensive
coding, working assertions, reusable LaTeX scaffolding, and a header that
ties formulas to code. The TODOs below are scoped accordingly — most are
quality-of-life and robustness; one is methodological and should block
release.

### Priority 1 — Methodological (blocks public release)

#### TODO-1.1: Fix the standard errors on derived elasticities

**Problem.** Lines 289–337 propagate SEs by simple division:

```stata
gen double flow_semi_se      = se / (delta_t * 100)
gen double flow_se_total     = (se / abs(pre_mean)) / abs(delta_ln_ntr_total)
gen double stock_se_att_taxbase_kleven = (se / 100) * scale_taxbase / abs(delta_ln_ntr_total)
```

This treats `delta_t`, `delta_ln_ntr_total`, `pre_mean`, `scale_taxbase`, and
`impacted_agi_share` as known constants. None is. They are estimates from
`02_revenue.do` carrying their own sampling variation, and `pre_mean` is
from the same panel that produced `tau` (so numerator and denominator are
correlated). The reported CIs (`±1.96 × se` at lines 339–355) are therefore
biased downward.

**Compounding gap.** The cumulative stock elasticities
(`stock_e_cum_*_kleven`, the *primary* numbers in Table 1) have **no SE at
all** because the pipeline does not export the joint event-study covariance
matrix. The current footnote (lines 574–575) discloses this for the
cumulative version but obscures that the SEs we *do* report are also
conservative-only-by-luck.

**Two paths.**
1. **Cheap:** After each SDID model in `02_sdid_analysis.do`, compute the
   elasticity transform via `nlcom` so the delta-method SE accounts for the
   joint variance of the SDID estimate and any same-sample quantities
   (notably `pre_mean`). Will not address revenue-parameter uncertainty but
   fixes the within-sample correlation, which is the bigger contributor.
2. **Right:** Bootstrap the whole pipeline. Resample counties, re-run
   `02_revenue.do` and `02_sdid_analysis.do` inside the bootstrap, recompute
   `flow_e_total` and `stock_e_cum_*_kleven` per replication, percentile-CI
   those. Gives correct CIs for cumulative stock elasticities — the main
   published numbers.

**Action.** Implement option 1 first as a defensible interim. Output a new
column in Table 1 with delta-method 95% CIs alongside the point estimate.
Update the table footnote to be accurate. Open a separate branch for the
bootstrap pipeline (option 2).

**Acceptance criteria.**
- New `*_ci_lo_dm`/`*_ci_hi_dm` variables generated via `nlcom` for at least
  `flow_semi_e`, `flow_e_total`, `stock_e_att_taxbase_kleven`.
- Table 1 (`tbl_elasticities.tex`) shows CIs on the cumulative stock
  elasticity column or has a footnote explicitly stating "no uncertainty
  quantification available; see Appendix X for bootstrap CIs."
- The current note at lines 574–575 is rewritten so it does not imply the
  in-table SEs are unbiased.

### Priority 2 — Robustness (within next sprint)

#### TODO-2.1: Defensive checks on `${overleaf}` and companions

**Problem.** Lines 715, 904, 919 read `${overleaf}` directly:

```stata
if ${overleaf} == 1 { ... }
```

If `${overleaf}` is empty, this expands to `if  == 1`, which is a syntax
error that halts execution *after* the figures and tables are written. Same
risk for `${ol_fig}` and `${ol_tab}` — if either is empty the `copy` command
writes to a nonsense path.

**Action.** Replace bare `if ${overleaf} == 1` with
`if "${overleaf}" == "1"` (string comparison is empty-safe). Add a startup
check near line 53 that verifies `${overleaf}`, `${ol_fig}`, `${ol_tab}` are
all set when `${overleaf} == "1"`:

```stata
if "${overleaf}" == "1" {
    foreach g in ol_fig ol_tab {
        if "${`g'}" == "" {
            dis as error "ERROR: \${overleaf}=1 but \${`g'} is unset."
            exit 198
        }
    }
}
```

**Acceptance criteria.** Setting `global overleaf ""` at the top of the file
produces a clean exit with a helpful error rather than a syntax fault deep
in Section 3.

#### TODO-2.2: Named locals for magic year boundaries

**Problem.** Line 364 hard-codes the common-support window:

```stata
gen byte post_common = inrange(event_year, 2021, 2022) & !missing(event_tau)
```

The variable name `_common` plus the magic numbers `2021` and `2022` will
silently drift when 2023 IRS data lands.

**Action.** Define near the top of Section 0 (around line 144):

```stata
local pfa_start_year   = 2021    // PFA tax took effect
local common_end_year  = 2022    // last year of IRS+ACS overlap
```

Replace the magic numbers at line 364 with
`inrange(event_year, `pfa_start_year', `common_end_year')`. Add a comment
in the header noting that "common-support" is defined relative to IRS-ACS
overlap.

#### TODO-2.3: Assert `scale_total` correctness

**Problem.** Lines 319–321 default `scale_total = 1` and only adjust for
`data_type == "ACS College"` or `"ACS College (Out-of-State)"`. A new data
slice would silently get `scale_total = 1` and produce wrong elasticities.

**Action.** Add immediately after the `replace` block at line 321:

```stata
assert scale_total == 1 ///
    if !inlist(data_type, "ACS College", "ACS College (Out-of-State)")
assert scale_total == college_agi_share ///
    if  inlist(data_type, "ACS College", "ACS College (Out-of-State)")
```

#### TODO-2.4: Cache `sdid_event_results.dta` in a tempfile **[urgent — introduced by commit `a4bc53d`]**

**Problem.** `sdid_event_results.dta` is now loaded twice — once at lines
257–263 (to extract `outstate` per spec) and again at lines 359–377 (to
build cumulative-tau aggregates). The second load was introduced by the
"read outstate from source" fix in commit `a4bc53d`; the pre-fix file only
loaded it once.

**Action.** Load it once into a tempfile near line 255, and `use` from the
tempfile in both subsequent `preserve` blocks. Keep the existing
`bysort … keep if _n == 1` and `collapse` operations unchanged.

**Acceptance criteria.** Profiling with `set rmsg on` shows no second `use
"${results}sdid/sdid_event_results.dta"` call.

#### TODO-2.5: Explicit row-ordering for tables

**Problem.** Line 536's `sort data_type sample` orders Table 1 rows
alphabetically. Renaming a `data_type` label would silently re-shuffle the
published table.

**Action.** After computing `data_type` (around line 247), add:

```stata
gen byte row_order = .
replace row_order = 1 if data_type == "IRS"
replace row_order = 2 if data_type == "IRS (Out-of-State)"
replace row_order = 3 if data_type == "ACS All"
replace row_order = 4 if data_type == "ACS All (Out-of-State)"
replace row_order = 5 if data_type == "ACS College"
replace row_order = 6 if data_type == "ACS College (Out-of-State)"
assert !missing(row_order)
```

Replace `sort data_type sample` with `sort row_order sample` at lines 536
and 621.

#### TODO-2.6: Promote `delta_t` range warning to a hard error

**Problem.** Line 178: `delta_t` outside `[0.001, 0.05]` only emits a
warning. A `delta_t` of 0.5 (50%) would print one line and proceed,
producing absurd elasticities.

**Action.** Convert to `error 459` with a specific message:

```stata
if delta_t < 0.001 | delta_t > 0.05 {
    dis as error "ERROR: avg_mt_rate = " %8.6f delta_t " — outside [0.001, 0.05]"
    dis as error "       Inspect TAXSIM v25 inputs in 02_revenue.do and verify"
    dis as error "       avg_mt_rate is on the [0,1] scale (not [0,100])."
    log close log_02elast
    error 459
}
```

Same treatment for the `avg_total_rate` check at line 182.

### Priority 3 — Style and idiom

#### TODO-3.1: Refactor Section 3 into a reusable program

**Problem.** Lines 779–838 build vertical-line overlays by string-concatenating
`twoway` syntax inside a `forvalues` loop, then duplicate that pattern three
times for panels (a)/(b)/(c) of the histogram. ~60% of Section 3 is
copy-paste.

**Action.** Lift the logic into a program modeled on the existing
`elast_tex_open` / `elast_tex_notes_open` helpers:

```stata
capture program drop elast_hist_panel
program define elast_hist_panel
    syntax, VAR(varname) NAME(string) XTITLE(string) ///
        [BINS(integer 20) FILLCOLOR(string) IRSCOLOR(string) ACSCOLOR(string)]

    /* ... build overlay locals from preferred==1 obs ... */
    /* ... emit twoway call with name(`name', replace) nodraw ... */
end
```

Then Section 3 collapses to three `elast_hist_panel` calls per migration
direction, plus a `graph combine`.

#### TODO-3.2: `str20` for formatted-string columns

**Problem.** Lines 518–523 and 589–598 use `gen str12` for formatted strings.
With values like `"(-1234.567)"` (11 chars) this is close to truncation;
adding one decimal place silently truncates.

**Action.** Bulk-replace `str12` → `str20` in those generations.

#### TODO-3.3: Drop stale scalars at the top of Section 0

**Problem.** Lines 128–143 pull every revenue parameter into Stata
`scalar`s. Scalars persist across `clear`, so a stale scalar from a prior
run could silently shadow what you intended to load.

**Action.** Add after `project_set_seed` (line 105):

```stata
foreach s in avg_mt_rate avg_state_rate baseline_pfa_revenue total_agi_2022 ///
    agi_total agi_impacted impacted_agi_share agi_college college_agi_share ///
    agi_college_impacted college_impacted_agi_share avg_mt_rate_college_impacted ///
    avg_total_rate avg_total_rate_pre avg_total_rate_college avg_total_rate_pre_college ///
    delta_t delta_ln_ntr delta_ln_ntr_total delta_ln_ntr_total_college ///
    ntr_post ntr_pre ntr_mid delta_ntr_arc {
    capture scalar drop `s'
}
```

#### TODO-3.4: Replace `subinstr`/`proper` sample labeling with a map

**Problem.** Line 542's `subinstr(sample[`i'], "sample_", "", .)` followed by
`proper("`smp'")` will mangle multi-word sample names. A sample named
`sample_pre_post` becomes `Pre_Post`, not `Pre-Post`.

**Action.** Define a label map near the top of Section 2, or encode `sample`
to a labeled numeric and use `decode` for display.

### Priority 4 — Practices to preserve

Not TODOs. Flagged so future refactors don't accidentally remove:

- Header docstring (lines 1–38) with formulas, primary vs diagnostic flags,
  FICA scope, `Called by` / `Requires` block.
- `project_assert_manifest` on every input (lines 123, 204, 214).
- `assert inlist(outcome_type, ...)` and `assert inlist(migration, ...)`
  after regex parse (lines 235–236).
- `isid` on both sides of the 1:1 merge before merging (line 380 and the
  `preserve` block at 381–384).
- `scale_total` / `scale_taxbase` separation (lines 319–323).
- The CI loop with `capture confirm variable` (lines 340–355).
- Three NTR denominators computed in parallel, clearly labeled.
- `compress` before `save` (line 464).

### Suggested commit ordering

Each commit should leave the file in a working, releasable state.

1. **Commit A:** TODO-2.4 first (it's a regression from recent work).
2. **Commit B:** TODO-2.1, 2.2, 2.3, 2.6 — defensive checks and named
   constants. No behavior change on the happy path; hardens failure paths.
3. **Commit C:** TODO-3.3, 3.2 — scalar hygiene and string-width fix. Pure
   cleanup.
4. **Commit D:** TODO-3.1 — Section 3 refactor. Larger diff; verify figures
   are byte-identical to pre-refactor PDFs as a regression test.
5. **Commit E:** TODO-2.5 — explicit row ordering. Confirms LaTeX output is
   unchanged for current data_type set.
6. **Commit F (separate branch, ideally with statistical reviewer):**
   TODO-1.1 option 1 — `nlcom` delta-method SEs and updated table footnote.
7. **Commit G (separate branch, longer-horizon):** TODO-1.1 option 2 — full
   bootstrap pipeline.
