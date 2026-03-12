# Code Review Findings

Date: 2026-03-12

This document records the concrete code review findings identified during a static review of the repository, along with suggested fixes.

## 1. ACS yearly imports omit the `.csv` extension

- Severity: ~~P0~~ P2 (Stata's `import delimited` auto-appends `.csv`, so this works in practice)
- Status: **FIXED**
- Files:
  - `code/stata/01e_acs.do`

### Problem

The R downloader writes yearly ACS extracts as `acs_YYYY.csv`, but the Stata cleaning script imports `"${data}acs/acs_`y'"` without the `.csv` suffix. Stata's `import delimited` auto-appends `.csv` by default, so this is not a runtime failure, but the inconsistency is worth fixing for clarity.

### Fix applied

Added `.csv` extension to the import line in `01e_acs.do`.

## 2. ~~ACS household head variable uses `relate` instead of `related`~~

- Severity: ~~P0~~ **REJECTED**
- Status: **INVALID** — no fix needed

### Why invalid

The IPUMS variable is `RELATE` (not `RELATED`). The R extract requests `RELATED` as the column label, but after `rename_with(tolower)` the column name is `relate`. The Stata code `gen byte hh_head = (relate == 1)` is correct as written.

## 3. `parallel` is treated as optional, then forced back on

- Severity: P1
- Status: **FIXED**
- File:
  - `00_multnomah.do`

### Problem

The startup check correctly sets `global use_parallel = 0` when the `parallel` package is missing, but later the script unconditionally resets `global use_parallel = 1`, overriding the safety check.

### Fix applied

Changed line 123 from `global use_parallel = 1` to `if "${use_parallel}" == "" global use_parallel = 1`, so the early safety check is honored.

## 4. BEA filename fallback is not honored by the cleaning step

- Severity: P1
- Status: **FIXED**
- Files:
  - `code/stata/01c_demographics.do`

### Problem

The download step accepts either `CAINC1__ALL_AREAS_*.csv` or `CAINC1__ALL_STATES_*.csv`, but the cleaning step hardcoded `CAINC1__ALL_AREAS_1969_2024.csv`.

### Fix applied

Replaced the hardcoded import in `01c_demographics.do` with dynamic file resolution using the same AREAS-then-STATES fallback pattern as the downloader, with an informative error if neither is found.

## 5. API key parsing can silently return the wrong credential

- Severity: P2
- Status: **FIXED**
- File:
  - `code/R/utils.R`

### Problem

The API key helper used a fuzzy `grepl()` substring match and fell back to the first row if no match was found.

### Fix applied

Changed `grepl()` to exact `==` match and removed the silent first-row fallback. The existing `stop()` at line 47 already catches the `is.na(key)` case, so no match now errors immediately with a clear message.

## 6. `map_code.R` uses `unit()` without importing or qualifying it

- Severity: ~~P2~~ P3 (`tidyverse` loads `ggplot2` which imports `grid`, so `unit()` is available in practice)
- Status: **FIXED**
- File:
  - `code/R/map_code.R`

### Problem

6 instances of bare `unit()` without `library(grid)` or namespace qualification. Works in practice because `tidyverse` transitively loads `grid`, but fragile.

### Fix applied

Replaced all 6 `unit()` calls with `grid::unit()` to make the dependency explicit.

## Notes

- These findings came from static code review rather than a full pipeline run.
- I did not execute the end-to-end workflow because that would require external downloads, API keys, and Stata.
- The items above are prioritized around reproducibility and runtime correctness rather than style.

## 7. Variable name mismatch in `01h_auxiliary.do`

- Severity: P0
- Status: **FIXED**
- File:
  - `code/stata/01h_auxiliary.do`

### Problem

Line 25 keeps the variable `county_fips_code`, but line 29 tries to rename `county_fips` (without the `_code` suffix). This causes a "variable county_fips not found" error, halting the pipeline before DOL childcare data or property tax rates are built — blocking all downstream SDID analysis scripts.

### Fix applied

Changed `rename county_fips fips` to `rename county_fips_code fips` on line 29.

---

## 8. Parallel interruption handling is inconsistent across the parallelized do-files

- Severity: P1
- Status: **FIXED**
- Files:
  - `code/stata/02_otherout_sdid.do`
  - `code/stata/02_quarterly_sdid.do`

### Problem

`02_sdid_analysis.do` had checkpoint/resume behavior, but `02_otherout_sdid.do` and `02_quarterly_sdid.do` did not clear stale temp files before fresh runs and had no resume support. An interrupted parallel run could silently contaminate the next run's combined results.

### Fix applied

Both files now match the `02_sdid_analysis.do` pattern:

1. **`resume` global fallback** added (defaults to 0 for standalone runs)
2. **Clean-start in parallel mode**: temp directory is deleted and recreated when `resume == 0`, preventing stale file contamination
3. **Parallel worker resume**: skip logic checks for existing temp result files when `resume == 1`
4. **Sequential checkpoint**: Mata associative array loads completed spec keys from the results file; loop skips already-done specs
5. **Conditional results init**: existing results file is preserved when resuming (not overwritten with empty dataset)

`02_flow_analysis.do` was already safe (clean-start pattern) and left unchanged.

---

## Stylistic And Maintainability Nits

These items are lower priority than the runtime issues above. They are not the first things to fix, but they would make the repo easier to maintain and easier for collaborators to run.

### ~~7. Encoding artifacts appear in user-facing text and comments~~

- Status: **REJECTED** — Both `README.md` and `00_multnomah.R` are clean UTF-8 with no mojibake, no broken dashes, and no box-drawing characters. This finding was incorrect.

### 8. `map_code.R` is doing too many jobs in one script

- Severity: ~~P3~~ P4 (optional)
- File:
  - `code/R/map_code.R`

#### Assessment

The file is 1,074 lines and produces ~65 PNGs across 5 map families, but it already factors out 4 reusable helper functions (`create_flow_map`, `create_rate_change_map`, `create_hatch_pattern`, `create_directional_flow_map`). The map families share data loading, so splitting into separate files would add coordination overhead for modest benefit. Low priority unless the file continues to grow.

### 9. Repeated hard-coded layout constants make maps difficult to tune

- Severity: P3 — **Confirmed**
- File:
  - `code/R/map_code.R`

#### Assessment

Verified. Padding value `50000` appears 5 times, nudge coordinates are repeated 2–4 times each, and inset placement values (`0.63`, `0.15`, `0.36`, `0.70`) are scattered across multiple lines. Centralizing into a constants block at the top of the file is a clean improvement that would make visual tuning easier.

### 10. `map_code.R` imports more of the tidyverse than it appears to need

- Severity: ~~P3~~ P4 (optional)
- File:
  - `code/R/map_code.R`

#### Assessment

Verified that only `dplyr`, `ggplot2`, and minor `readr`/`stringr` usage exists. However, `library(tidyverse)` is a standard pattern in research R code and the unused sub-packages cause no runtime harm. Not worth the churn to change.

### 11. The descriptive-analysis script contains substantial duplication

- Severity: P3 — **Confirmed**
- File:
  - `code/stata/02_descriptives.do`

#### Assessment

Verified. The 1,539-line file has clear parallel IRS/ACS blocks (lines 45–95, 103–223), duplicated box-plot statistics computation (lines 1000–1026 repeated at 1271–1297), and repeated LaTeX table-writing boilerplate across Tables 1 and 2. Factoring the box-plot stats and LaTeX boilerplate into small Stata programs would reduce copy-paste drift.

### 12. `00_multnomah.R` mixes orchestration with package installation policy

- Severity: P3 — **Confirmed**
- File:
  - `00_multnomah.R`

#### Assessment

Verified. Lines 28–50 auto-install any missing packages from a hard-coded list with no opt-out flag. This is common and convenient in research pipelines, but adding an `auto_install_packages <- TRUE` flag at the top would make the behavior explicit and easy to disable in controlled environments.

### 13. Some comments are out of date or more narrative than actionable

- Severity: P3 — **Partially confirmed**
- Files:
  - `code/R/qwi_data.R`
  - `code/stata/02_descriptives.do`

#### Assessment

`qwi_data.R` lines 14–17 reference a "previous API-based approach" that is no longer the current method — this is genuinely stale. `02_descriptives.do` has several trivial comments that restate obvious code (`** Preserve`, `** Clear and restore`, `** Keep Multnomah`). However, `api_code.R` comments are mostly fine and should be removed from this list.
