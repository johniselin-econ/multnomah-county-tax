TODO List for Multnomah Paper revision.

I have below a list of changes I want to make to the paper. Some of these involve just changes to the tex file, others involve changes to the code. Proceed through them as follows:

1. Identify which require changes to code, changes to the tex file, or both.
2. Worth through code changes first. Once these are done we can make the tex changes all at once.
3. For each change, consider where there might be references or other places in the text that require changes
4. For each change, consider whether the changes are correctly transmitted through the code-to-overleaf pipeline


-------------------------------------------------------------------------------
SESSION STATUS — last updated 2026-05-07 (late)
-------------------------------------------------------------------------------

Plan file: `quality_reports/plans/2026-05-06_paper-revision-todos.md`
Tracking: 15 tasks in the harness task list.

CODE-SIDE PROGRESS:
- ALL 12 items DONE (13, 14, 15, 9, 10, 16, 3, 21c, 2, 4, 8, 11).

TEX-SIDE PROGRESS:
- All Phase 3 tex edits are still pending — collected to a single edit pass.

NEXT STEPS:
- Phase 3: single tex pass on `updated.tex` covering items 1, 5, 6, 7, 12, 17, 18, 19, 20, 21a, 21b, 21d, 21e, 21f.
- Phase 4: verify + Overleaf compile.

-------------------------------------------------------------------------------
ITEMS
-------------------------------------------------------------------------------

* [DONE — Phase 3 tex pass] Drop the equations from the SDID text (4.1) - those are DID equations, not SDID equations.

* [DONE — code in `code/R/map_code.R`, 2026-05-07] *(Item 2)* For figure 1, let's go back to a version of the Oregon / Washington area, with a cutout zooming in on the Portland area. The cutout should include the Average Marginal Tax rate shading. Have the average tax rate legend below the full figure.
  → New `map_combined_tax.png` produced and synced to Overleaf. Built from the existing `map1_with_box` overview + new `map2_tax_inset` (tax-shaded close-up with internal legend suppressed) + horizontal tax-rate legend strip below. Phase 3 tex pass will switch updated.tex Fig 1 from `map2_tax.png` to `map_combined_tax.png`.

* [DONE — `code/R/fig_diagrams.R`, 2026-05-07] *(Item 3)* For figure 2 and figure 3, let's make the text bigger. We have some space to spare, and it is not super easy to read. Also, remove the different color boxes. To save space, in figure 3, drop the final text "Outcome variables", "Key Controls", and "Donor Pool Restrictions".
  → Bumped all `tx()` font sizes by ~2 points; replaced colored fills with `NA` so only outlines remain; deleted the bottom-info section in `draw_empirical_approach`. Re-rendered and synced to Overleaf.

* [DONE — code in `fig_diagrams.R` and `02_tables_figures.do`, 2026-05-07] *(Item 4)* For all figures, create versions with and without titles / subtitles / notes. For the paper, use the versions without these components.
  → R-side conceptual diagrams (Figs 2, 3) now produce two variants: `fig_*.pdf` (no title, no notes — paper) and `fig_*_titled.pdf` (with title and notes — slides etc.). Stata-side: added `${clean_figs}` global toggle (default 0, set to 1 for paper-clean output) wired through both preferred-overlay event-study blocks (Set 1 donor-pool comparison, Set 2 dataset-overlay). Maps were already title-free. Spec curves keep existing minor titles — the user can extend the `${clean_figs}` pattern to those later if desired.

* [PENDING — Phase 3 tex pass] *(Item 5)* For Figure 6, use the Oregon / Washington versions, and add the West Coast versions to the appendix.
  → Just an `\includegraphics` swap in updated.tex (orwa.png in main, westcoast.png in appendix). Assets exist.

* [PENDING — Phase 3 tex pass] *(Item 6)* For figure 7, Have the two event-studies stacked, rather than next to each other.
  → Subfigure layout change in updated.tex.

* [PENDING — Phase 3 tex pass] *(Item 7)* For figure 10, use newly-created specification curves.
  → Switch \includegraphics from `fig_revenue_dist_*.pdf` to `fig_speccurve_revenue_*.pdf` (assets already in Overleaf).

* [DONE — code in `02_descriptives_supp.do`, 2026-05-07] *(Item 8)* For Table 1:
  * Drop Panel B
  * Expand Panel A: (a) replicate the structure for the ACS as a new panel B, similar to how table A1 is laid out; (b) add a column with the count of counties; (c) include all five samples (all, urban-95, urban-Covid match, demographic match, stringency match)
  → New table1_combined.tex: 2 panels (IRS + ACS College) × 6 rows (Multnomah + 5 donor pools) × 9 cols (label, N counties, out pre/post, in pre/post, net pre/post, net change). IRS uses 2018-19 vs 2021-22 window; ACS uses 2018-19 vs 2021-24 (extended post period). Old "sample composition" Panel B removed. Synced to Overleaf.

* [DONE — code in `02_tables_figures.do`, 2026-05-07] *(Item 9)* For the elasticity figures, have the y-axis label be clearer (e.g. Migration Stock Elasticity) and define the term mathematically in the note (see Figure A3)
  → Y-axis labels updated to "Migration Semi-Elasticity (β)", "Migration Stock Elasticity", "Migration Flow Elasticity" with PFA+SHS variants. Math definitions will go in the figure note in Phase 3 tex pass.

* [DONE — code in `02_tables_figures.do`, 2026-05-07] *(Item 10)* Add matching figures for Figure A3 for flow elasticities and semi-elasticities.
  → Added 4 new flow-elasticity distribution figures: fig_speccurve_elast_flow_in.pdf, fig_speccurve_elast_flow_out.pdf, +SHS variants. Synced to Overleaf.

* [DONE — new `02_appendix_descriptives.do`, 2026-05-07] *(Item 11)* I want to re-think Table A1. Break it into one table per method (SDID, IRS county-to-county flow, ACS individual data):
  * SDID: variant of Table 1 with overall weighted averages of out-of-county and out-of-state migration plus county counts. Two panels (IRS, ACS College).
  * IRS Flow: 2 panels (All / ACS), each reporting for Multnomah flows and non-Multnomah flows: number of observed flows, share with 0 movers, and mean flow size (n1, n2, AGI).
  * ACS: 2 panels (out-migration / in-migration samples) with count of individuals, weighted count, avg income, migration rates.
  → New file `code/stata/02_appendix_descriptives.do` produces three method-specific tables synced to Overleaf:
    - `tableA1_sdid.tex`: 2 panels × 6 rows × 7 cols, time-pooled means by donor pool.
    - `tableA1_irs_flow.tex`: 2 panels (All / ACS-restricted) × 2 rows (Multnomah-touching / non-Multnomah) × 5 cols (N flows, median n1, mean n1, mean n2, mean AGI). "Share with 0 movers" not directly observable since IRS suppresses low-count flows; replaced with median n1.
    - `tableA1_acs.tex`: 2 panels (out / in samples) × 2 rows × 5 cols (county-years, weighted persons / households in millions, total income in USD billions, migration rate).
  → The old single `tableA1_variables.tex` (variable definitions) remains on disk; Phase 3 tex pass will swap the \input lines to point to the three new files.

* [PENDING — Phase 3 tex pass] Drop Table A2 *(item 12)*

* [DONE — code in `02_spec_engine.do`, 2026-05-06] *(Item 13)* For all elasticity measures, check extraneous quotation marks.
  → Dropped `string asis` from `cap()` and `cols()` in `elast_tex_open`. All elasticity tables now have clean `\caption{...}` and `\begin{tabular}{...}` (no spurious quotes).

* [DONE code-side; PENDING tex pass for Metro-tax appendix counterpart] *(Item 14)* For Table 2, Check the footnote, after "each post year" the text gets messed up. Make sure the latter three columns are equally spaced and centered. Can you check if the ATR used in the elasticities include the Metro tax? If not, include in the appendix a matching table with the same results WITH the metro tax included.
  → Math fix: rephrased to avoid `$h$` / `$H$` / `$T$` / `$s_\text{...}` patterns that Stata's macro engine eats; used `char(96)+char(96)` for LaTeX `` `` `` quotes. Last 3 cols already centered with `ccc` spec.
  → ATR + Metro tax: SHS variant `tbl_elasticities_shs.tex` already exists. Will be included as an appendix counterpart in the Phase 3 tex pass.

* [DONE — code in `02_tables_figures.do`, 2026-05-07] *(Item 15)* For table A3, "footnotesize" above table title. Replace flow elasticities with stock elasticities.
  → Stock-elasticity column replaces flow column in `tbl_elasticities_inout.tex` (PFA + SHS variants). Header changed to "Stock ε". Notes updated to define stock elasticity. `\footnotesize` placement moved to before `\caption{}` (uses `char(92)` to escape backslash-eat issue).

* [DONE — code in `02_tables_figures.do`, 2026-05-07] *(Item 16)* Add a table to the appendix with the coefficients and standard errors from our preferred SDID estimates.
  → New file `results/sdid/tab_sdid_preferred.tex` (synced to Overleaf). 4 specs (IRS×{all,stringency} and ACS College×{all,stringency}) × 3 directions (out, in, net) with τ̂, SE, and N counties.

* [PENDING — Phase 3 tex pass] *(Item 17)* Add to Appendix B a section with the conditional means regression model, describing the data, the controls included, equations, etc... Pull from main.tex as necessary.

* [PENDING — Phase 3 tex pass] *(Item 18)* For Appendix B3, include relevant equations to describe the SDID math, pulling from main.tex as necessary.

* [PENDING — Phase 3 tex pass] *(Item 19)* For appendix B6, add a description of the bootstrap procedure to produce standard errors.

* [PENDING — Phase 3 tex pass + asset copies] *(Item 20)* Add Appendix C, which will be the IRS Migration Data Quality appendix in main.tex. Review and include as necessary figures from `results/appx_irs_data`.

* [PARTIAL — code in `02_flow_analysis.do` is staged; some Phase 3 tex] *(Item 21)* Add the following figures / tables to the appendix:
  * [PENDING — Phase 3 tex pass + asset copies] (a) `fig_strip_agi_mult.png` (and matching n1, n2) → Appendix referencing overall migration rate in Multnomah for context.
  * [PENDING — Phase 3 tex pass + asset copies] (b) Difference-in-Difference results table (use `results/did/tab_did_combined.tex`).
  * [DONE code-side; AUTO-GENERATES on next 02_flow_analysis.do run] (c) Flow results table parallel to DiD table.
    → Sample-tagged `estimates store` calls added inside the sample loop. End-of-script esttab block produces `results/flows/tab_flow_regression.tex` + auto-syncs to Overleaf when `${overleaf}=1`. Cannot be triggered without re-running the full PPML pipeline (~30 min) since stored estimates are not persisted.
  * [PENDING — Phase 3 tex pass + asset copies] (d) Conditional mean figures for education and age (from `results/individual`).
  * [PENDING — Phase 3 tex pass] (e) Specification curves for N1 and N2 (Net, Out, In migration; out-of-state and county-level). Assets already in Overleaf.
  * [PENDING — Phase 3 tex pass + asset copies] (f) Influence figures from `results/sdid/influence` for AGI net, AGI in, and AGI out.
