# TODO — Multnomah County Tax

Open items only. History lives in git (`git log`) and the dated run manifests
under `results/_runs/`. Last pruned 2026-10-26, after the 500-rep publication
bootstrap, the Overleaf-mirror / results-hygiene refactor, and the laptop-side
October rerun follow-ups (merged in from `aba1ca7`).

Context for the paper items: the `October-revised` full run finished on the
cluster 2026-10-26 and its `results/overleaf_export/` was copied to the Overleaf
Dropbox folder the same day. The manuscript text was updated to the new numbers
except where noted below. A second full run (Slurm job 30912814, 500-rep
bootstrap) finished 2026-10-29 00:14; its `results/overleaf_export/` and
`results/paper_numbers.csv` are now tracked in git (`git pull` on the laptop),
and the numbers below come from that run. Its "TAXSIM fell back" warning was a
false positive (the check matched the echoed source line; fixed in
`full_run.sbatch`) — TAXSIM ran normally. Comparison archive for the September PFR submission:
`Documents/Multnomah (2026) - PFR archive`.

## Paper (blocking for submission)

- [ ] **Prose fixes still open in the draft** (checked 2026-10-29 against
  `drafts/overleaf_2026-10-29/` and the 2026-10-29 run). Already current there:
  the PPML placebo shares (§5.2), the revenue spec-curve summary (§6), the DiD
  in-migration semi-elasticities (§5.3), and the abstract's ranges, which match
  Table 2 / Table 3 (§5.3 / §6 prose and Figure 10 not re-read line by line).
  Still stale:
  - "About double" (§5.1, line 138; key `sdid_agi_out_in_mean_tau_ratio`:
    mean |tau| out / in across the AGI SDID spec curve) — the run logs 3.82.
    On the 2026-10-26 results no reasonable definition gives ~2: in-state data
    types 3.82 (median 3.68), preferred in-state specs 2.72, all data types
    3.04, out-of-state data types 2.30. Either rewrite the prose ("nearly four
    times", and set `fmt(%3.1f)`) or point the key at the out-of-state specs if
    that is what the sentence means.
  - Intro (line 74) says the county revenue loss is "5 and 9 percent"; Table 3
    and the abstract give 5 and 10.
  - §6 (line 173) says the state baseline is "$2,910 million"; the run gives
    2,910.7 (Table 3), i.e. "2,911".
  - Appendix C.8 (line 1306) baselines $189M / $2,886M vs Table 3's $191.9M /
    $2,910.7M.
  - Slides: headline bullets (lines 121, 656, 685) carry the old revenue
    numbers ($76.7M where Table 3 now gives $76.6M); fixed by the generated
    slides table below.
- [ ] **Convert the prose to `\pn{}` macros** (decided 2026-10-29; not
  started — neither .tex uses `\pn{}` yet). Do it after the next full run, so
  the register carries the fixes below. The pipeline writes
  `tables/paper_numbers.tex` (one macro per register key). In both the paper
  and the slides: `\input{tables/paper_numbers}` in the preamble, then replace
  each registered number with `\pn{<key>}` (wrap in `$...$` where a negative
  needs a math minus). Then run
  `OVERLEAF_TEX=... OVERLEAF_SLIDES_TEX=... Rscript code/R/audit_paper_numbers.R`:
  fix every UNKNOWN_KEY; for each UNUSED key, convert the literal it lists or
  drop the key (e.g. the 33 `sdid_meta_*` keys if they are table-only; the
  `map_tax_rate_*` keys feed `map_code.R`, not the text). Upload both .tex
  files to the cluster (`drafts/`) so the register can be checked there.
- [ ] **Next full run — register fixes waiting on it.**
  - `sdid_n_specs_per_outcome` counts after the out-of-state drop (it logged
    288 = county + out-of-state geographies; the paper's 144 is right). Then
    `\pn{}` the "144" in §3 / §5.1 and the slides.
  - `revenue_baseline_state_musd` logs with `%9.0fc` ("2,911").
  - ACS county count: 389 was right all along (identified in every year
    2016-2024). The `acs_n_counties_*` keys counted the `county_fips == 0`
    "rest of state" buckets (48-49 per year) and logged 534 / 437 / 436; fixed
    in `02_diagnostics.do` and `build_acs_balanced_set`, they should read
    485 / 389 / 388. New keys `acs_n_counties_sdid_sample` (337) and
    `acs_n_counties_sdid_donors` (336) come from `02_appendix_descriptives.do`.
    Then rewrite the Appendix sentence (line 1198: "restricted to the 389
    counties identified in the public-use microdata") to say "identified in
    every year 2016-2024", add that 337 (336 donors + Multnomah) remain after
    the state drops, `\pn{}` both, and `\pn{}` the 389 in the slides (line 231).
  - `map_tax_rate_{or,or_metro,mult,mult_metro}` (02_revenue_microsim.do)
    now feed the tax-rate map; `map_code.R` stops on a missing key, so Stage 3
    cannot run until Stage 2 has logged them. Expect 9.90 / 10.90 / 12.15 /
    13.15.
- [ ] **Slides' revenue table: validate, then wire into the deck.**
  `02_tables_figures.do` (end of Section 4B) now writes
  `tbl_revenue_decomposition_slides.tex` (bare 7-column tabular, panel headers
  carry actual revenue) and registers it in `paper_manifest.csv`
  (`slides:revenue_table`). It also logs the prose revenue numbers:
  `revenue_{pfa,oregon}_loss_irs_all_{musd,pct}` (headline row),
  `revenue_{pfa,oregon}_loss_pref_{min,max}_{musd,pct}` (six-spec ranges), and
  `revenue_acs_college_scale` (0.68). Not yet run — validate with a standalone
  `02_tables_figures.do` or the next full run. Then in the deck: replace the
  hand-typed tabular in the "Revenue Decomposition" frame with
  `\input{tables/tbl_revenue_decomposition_slides}`, and `\pn{}` the bullets
  (lines 121, 656, 685), the range sentence, and the 0.68 in the source note.
  The six-spec range keys log whole millions (PFA 10-19, Oregon 45-81), so the
  slide's range sentence becomes "$10M to $19M".
- [ ] **Abstract: "$9-$19 million" -> "$10-$19 million"** (decided 2026-10-29:
  the abstract uses round numbers; the PFA minimum is 9.7). Edit in Overleaf,
  in both `Conway_Iselin_Rork_2026.tex` and `manuscript_anon.tex`; it becomes
  `\pn{revenue_pfa_loss_pref_min_musd}`--`\pn{revenue_pfa_loss_pref_max_musd}`
  with the macro conversion.
- [ ] **Re-sync the Overleaf folder** from `results/overleaf_export/` (now
  tracked: `git pull`, then rsync per `RUNBOOK.md`). The 2026-10-29 run carries
  the 2026-10-26 table-note fixes (`31f1b6e`, `3eebbff`): `tbl_elasticities_shs.tex` and `tbl_revenue_decomposition.tex` were
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

- The laptop cannot ssh/scp to any YCRC host (no registered key); paper
  artifacts now travel via git (tracked `results/overleaf_export/` +
  `results/paper_numbers.csv`). For anything else, a zip download from Open
  OnDemand (what worked 2026-10-26).
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
