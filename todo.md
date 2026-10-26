# TODO — Multnomah County Tax

Open items only. History lives in git (`git log`) and the dated run manifests
under `results/_runs/`. Last pruned 2026-10-26, after the 500-rep publication
bootstrap, the Overleaf-mirror / results-hygiene refactor, and the laptop-side
October rerun follow-ups (merged in from `aba1ca7`).

Context for the paper items: the `October-revised` full run finished on the
cluster 2026-10-26 and its `results/overleaf_export/` was copied to the Overleaf
Dropbox folder the same day. The manuscript text was updated to the new numbers
except where noted below. Comparison archive for the September PFR submission:
`Documents/Multnomah (2026) - PFR archive`.

## Paper (blocking for submission)

- [ ] **Propagate the V4 (500-rep) bootstrap numbers into the paper.** The
  publication bootstrap finished 2026-10-26 (`results/bootstrap/`, manifest
  `reps=500`). Table 2 / `tab:elasticities_shs`, Figure 10, the abstract, and
  §5.3 / §6 prose still carry the V3 (100-rep) numbers.
- [ ] **Recompute the PPML placebo shares** for Section 5.2 (share of placebo
  out-migration coefficients / t-stats larger than Multnomah's; share of
  in-migration ones smaller). The text still says 5.1% (6.5%) and 15.7% (8.0%)
  from the pre-zero-flow run; flagged with a `% TODO` in the tex. Source: the
  placebo output of `02_flow_analysis.do` (`results/flows/`).
- [ ] **Recompute the revenue spec-curve summary** for Section 6 (PFA loss
  center and IQR, Oregon center; text still says $16M, $10-22M, $79M). Signed
  losses (`462a80a`) un-fold the distribution. Source: the revenue spec grid
  from `02_post_spec.do` / `02_tables_figures.do`. Flagged with a `% TODO`.
- [ ] **Re-sync the Overleaf folder** from `results/overleaf_export/` after the
  2026-10-26 table-note fixes (`31f1b6e`, `3eebbff`) were regenerated on the
  cluster: `tbl_elasticities_shs.tex` and `tbl_revenue_decomposition.tex` were
  hand-patched in Overleaf and the mirror now overwrites them. Prefer
  `PAPER_ONLY=1` so only the paper's 98 artifacts land there.
- [ ] **Run the audit against the `.tex`** on the laptop
  (`OVERLEAF_TEX=... Rscript code/R/audit_paper_artifacts.R`) and act on any
  DRIFT rows. On the cluster the audit only checks manifest -> disk -> mirror.
- [ ] **Decide on the figure font.** The regenerated DiD and PPML figures
  (`fig_es_*.png`, `fig_multnomah_both_*.png`) switched from serif to
  sans-serif between the September archive and this run; the SDID/spec-curve
  PDFs did not. Pick one scheme and re-export the odd ones.
- [ ] **Phase 4 visual review of the compiled PDF**: regenerated tables
  (Table 1 note now reads "IRS-389"; Table A2 gained unique-county columns;
  Table A3 rates are lower after the 2020 exclusion and `same_county`
  definition), figure legibility, dashed-line colors / labels in the
  spec-curve figures, and whether the revenue figures are annual (the last two
  were stale TODO notes in `02_tables_figures.do`).
- [ ] **Review the new by-age DiD pre-trend**: the 45-64 college group has a
  positive 2017 coefficient in `fig_es_age_out_migration.png`. Consider the
  `cat_educ#cat_age` alternative noted in commit `075dfc6` as a robustness check.
- [ ] **Merge `October-revised` into `main`** once the above is verified (main
  is a strict ancestor; fast-forward).

## Submission artifacts

- [ ] **Regenerate `manuscript_anon.tex`** from the main file after the TODO
  numbers are filled (the 2026-10-26 edits were applied to both copies in the
  Overleaf folder, but the archived submission version also rewires figures to
  the `_combined` multi-page PDFs).
- [ ] **Rebuild the `fig_*_combined.pdf` multi-page figures** used by the PFR
  submission (35 files in the archive). No script in the repo produces them;
  either script the assembly (PyMuPDF/qpdf from the manifest's per-label file
  lists) under `other/` or document the manual steps, then rebuild.
- [ ] **Co-author email**: draft at
  `drafts/2026-10-26_coauthor_email_results_update.md`; send after the two
  TODO numbers are recomputed (or send now and follow up).
- [ ] **Slide deck**: `Conway_Iselin_Rork_2026_slides.tex` at the Overleaf
  project root (set as main file in Overleaf to compile) and
  `drafts/Conway-Iselin-Rork-2026-10-26-slides.pdf`. Placebo shares and the
  revenue spec-curve summary are marked "[to recompute]" there too.

## Pipeline — hygiene (non-blocking)

- [ ] **Dev-only outputs still generated** (decided 2026-10-26: keep for now,
  they are excluded from Overleaf under `PAPER_ONLY=1`; the legacy
  `table1.tex` / `table2.tex` writers were removed). Revisit if run time
  matters: `table_migration_{county,state}.tex`, `fig_strip_*_all.png` and
  `fig_strip_*_state_*.png`, `fig_es_combined.png`, `fig_es_agepost_*.png`,
  the non-`_shs` `tbl_elasticities_*` variants, `tab_sdid_preferred.tex`, the
  per-spec `tab_sdid_*` permutations, and the `02_otherout_sdid.do` tables.
- [ ] **File splits** (each >1,300 lines): `02_sdid_analysis.do` → estimate /
  event-study / spec-curve render; `02_tables_figures.do` → elasticity tables
  / elasticity curves / SDID overlays; `02_descriptives.do` → flow tables /
  strip figures / Table 1; `02_revenue_microsim.do` → parameters /
  counterfactuals / tables. Not mechanical: Stata locals are file-scoped, so
  each cut needs a pass over the locals that cross it, and each split stage
  needs a full rerun to validate. Schedule after the submission.
- [ ] Spec-curve consolidation: move the SDID spec-curve renderer into
  `02_spec_engine.do` (or adapt it to `elast_speccurve_plot` with a schema
  adapter). Do after the file splits.
- [ ] Optional pipeline renumbering once the file splits land.

## Notes — laptop / server gotchas (recorded 2026-10-26)

- The laptop cannot ssh/scp to any YCRC host (no registered key); move
  artifacts via a zip download from Open OnDemand (what worked 2026-10-26) or
  a GitHub branch.
- Git Bash rewrites Stata's `/e` flag into the path `E:/`; launch batch Stata
  from PowerShell (`Start-Process StataMP-64.exe -ArgumentList '/e','do','file.do' -Wait`).
- Stata string escaping, verified by batch test: a literal `\$X\$` in a
  double-quoted `file write` string prints `$X$`; a local holding `\$`
  followed by a letter is re-expanded and prints `\`; a local holding `\\$`
  prints `\$`; `\$150K` is safe because a digit cannot start a global name.

## Closed 2026-10-26 (for the record; details in `git log`)

500-rep publication bootstrap run; `project_save_overleaf` / `overleaf_copy`
single mirror with `PAPER_ONLY`; audit wired into `full_run.sbatch`;
`clean_results` flag; serial event-study failures logged (`r(event_rc)` /
`r(event_requested)` from `fit_spec_sdid`); stale `did_elasticities.dta`
erased when its input is missing; other-outcome donor pools documented as
intentionally five; legacy Table 1 / Table 2 writers removed; locator
preamble confirmed identical in all 16 stage scripts (no
`project_standalone_init` possible before `globals.do`); `00_main.R
download|post` replaces the two R entry scripts; diagram check marks are
vector paths; results/ and data/ detritus purged.
