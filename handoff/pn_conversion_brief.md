# Brief: convert the paper's in-text numbers to `\pn{}` macros

Temporary handoff (2026-10-29). Delete `handoff/` once the conversion is done
and the audit is clean.

## Goal

The paper and slides currently type their in-text numbers by hand, so they go
stale after every pipeline run. The pipeline now writes
`tables/paper_numbers.tex`, which defines one value per key of the in-text
numbers register; `\pn{<key>}` prints it (an unknown key prints a bold **??**
and a LaTeX warning). Replace the hand-typed numbers with `\pn{}` calls so the
text updates itself on every run.

## Inputs

| File | What it is |
|---|---|
| `tables/paper_numbers.tex` (Overleaf; repo: `results/overleaf_export/tables/`) | The macro definitions: key → current formatted value. |
| `results/paper_numbers.csv` | The register: `key`, `formatted` value, `unit`, and `note`, which says what the number *means*. The note is how you match a key to a sentence. |
| `handoff/pn_conversion_worklist.csv` | One row per place a key's value appears as a literal number (`status = CANDIDATE`, with `doc`, `line_12_29`, and ~180 characters of `context`), plus one row per key that appears nowhere (`status = NOT_IN_TEXT`). |

Line numbers are from the 12:29 2026-10-29 Overleaf copies and will have
drifted: locate each candidate by its `context`, not its line number.

## Documents to edit

1. `Conway_Iselin_Rork_2026.tex` (the paper)
2. `Conway_Iselin_Rork_2026_slides.tex` (the deck)
3. `manuscript_anon.tex`: the anonymised copy of (1); apply the same edits.

## Rules

1. Add `\input{tables/paper_numbers}` to the preamble of all three documents.
2. Replace a literal with `\pn{key}` **only when the key's `note` describes what
   the sentence is saying**. Matches are by digits alone, so many CANDIDATE
   rows are coincidences. Known traps:
   - "144" (the two `revenue_*_loss_spec_n` keys) and "6.5" (two placebo-share
     keys) each match more than one key; pick by meaning or skip.
   - Values inside a `tabular` (table cells) stay as they are: tables are
     generated files, and the register covers prose only.
   - `_acs`, `_incl2020` and unsuffixed placebo keys share values; the
     unsuffixed key is the all-counties, excluding-2020 headline.
3. Negative values: write `$\pn{key}$` so LaTeX sets a true minus sign. Keep
   whatever math wrapper the literal already had (e.g. `$0.002$` →
   `$\pn{did_semi_elast_in_west}$`).
4. Keep units and symbols outside the macro: `\pn{x}\%`, `\$\pn{x}M`.
5. If a sentence quotes a number in a different rounding or form than the
   register's `formatted` value (e.g. the text says \$2,910.2M but
   `revenue_baseline_state_musd` is `2911`), do **not** convert it: list it
   for the author (the fix is a format change in the pipeline).
6. Do not change wording. A sentence whose claim no longer matches the number
   ("about double" vs. `sdid_agi_out_in_mean_tau_ratio` = 3.82) goes on the
   list for the author, unedited.
7. Do not invent keys. If a prose number has no key, leave it and list it.

## Deliverable

- The three edited `.tex` files, compiling without `paper_numbers` warnings.
- A short report for the author with:
  - every conversion made (key → document and section);
  - CANDIDATE rows skipped, and why (coincidence, table cell, rounding);
  - the NOT_IN_TEXT keys, sorted into: stale prose (the sentence exists but
    quotes an old value or different form; quote it), never quoted (a
    candidate to drop from the register), or unclear;
  - prose numbers that look computed but have no key.

## Check

From the repo root, with the edited files reachable:

```bash
OVERLEAF_TEX=<path>/Conway_Iselin_Rork_2026.tex \
OVERLEAF_SLIDES_TEX=<path>/Conway_Iselin_Rork_2026_slides.tex \
Rscript code/R/audit_paper_numbers.R
```

Target: zero `UNKNOWN_KEY`; every `UNUSED` key either converted or on the
author list. The report lands in `results/_runs/paper_numbers_audit_<date>.csv`.
