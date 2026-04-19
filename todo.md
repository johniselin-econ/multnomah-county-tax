# TODO — Multnomah County Tax

Tracking items from the 2026-04-18 Stata review that were deferred.

## Deferred refactors

- [ ] **Split `02_revenue.do` into inputs + analysis (C1)** — 1,200+ lines, scored
  79/100. Candidate split: `02_revenue_inputs.do` (Section 0B SDID lookup,
  ACS load, tax-unit construction, TAXSIM) and `02_revenue_analysis.do`
  (PFA/Oregon revenue baseline, simulation, output). 2-4 hour refactor;
  do when you're already touching revenue math.

- [ ] **Path-bootstrap deduplication (C2)** — 14 analysis .do files carry a
  ~10-line path-detection preamble that's now also in `00_stata_config.do`.
  Would need a thin `_find_config.do` sourced by each file to resolve the
  chicken-and-egg (need `${code}` to source config; config sets `${code}`).
  Medium cost, modest reward. Skip unless platform portability becomes a pain.

## Smaller code-quality items

- [x] **`02_revenue.do` section numbering** — added a note at Section 2
  header explaining Section 2B was merged in.

- [x] **`01e_acs.do` exploratory diagnostics** — deleted `tab year`,
  `fre migrate1`, `tab migplac1`, `tab migcounty1`.

- [x] **`01h_auxiliary.do:49-54`** — renamed `mf_*_med` to `mfcc_*_med` so
  output prefix matches input prefix in both mc/mfcc groups; updated the
  downstream `foreach varlist mc_* mfcc_*` loop.

- [x] **`02_indiv_analysis.do`** — added `Requires: Stata 14+` to the header
  and a `version 14` directive for the `direxists()` dependency.

- [x] **`02_indiv_analysis.do:263`** — already had `capture confirm variable
  _at1/_at2/_margin` guards with explicit error messages (lines 272-290). No
  change needed — this was done in an earlier pass.

- [x] **Resume-mode Mata cleanup** (todo originally listed `02_flow_analysis.do`,
  but flow_analysis has no resume-mode Mata code; the actual pattern is in
  `02_sdid_analysis.do:1140` and `02_otherout_sdid.do:799`). Added
  `capture mata: mata drop _done_set` before (re-)creation to clear stale
  state from an aborted prior run, and unconditionalized the scope-exit
  cleanup (wrapped in `capture`) so it runs regardless of whether creation
  succeeded.

- [x] **`01d_covid.do:126`** — added `fileexists()` guard before `use "${data}JII
  Covid data.dta"` consistent with the pattern used in `01b_download.do`.

## Deliberately not doing

- **C3 — Extract `build_sdid_sample` program.** Audited 2026-04-18. The three
  SDID files share only ~26% of their sample-construction code; a shared
  program would need ~12 parameters and would be longer than the current
  three copies. Revisit if a 4th SDID variant is added.
