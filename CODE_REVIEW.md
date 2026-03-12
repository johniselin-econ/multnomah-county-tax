# Code Review Findings

Date: 2026-03-12

This document records the concrete code review findings identified during a static review of the repository, along with suggested fixes.

## Runtime And Correctness Findings

## 1. ACS yearly imports omitted the `.csv` extension

- Severity: ~~P0~~ P2
- Status: **FIXED**
- File:
  - `code/stata/01e_acs.do`

### Problem

The R downloader writes yearly ACS extracts as `acs_YYYY.csv`, while the Stata cleaning script imported `"${data}acs/acs_`y'"` without the `.csv` suffix.

### Review update

Stata's `import delimited` auto-appends `.csv` in this usage, so this was more of a clarity issue than a true pipeline blocker.

### Fix applied

Added the explicit `.csv` extension to the import line in `01e_acs.do`.

## 2. ~~ACS household head variable uses `relate` instead of `related`~~

- Severity: ~~P0~~ **REJECTED**
- Status: **INVALID** - no fix needed

### Why invalid

The exported ACS files include both `relate` and `related`. The Stata code `gen byte hh_head = (relate == 1)` is using the correct household-relationship field for identifying the household reference person, so the original finding was invalid.

## 3. `parallel` was treated as optional, then forced back on

- Severity: P1
- Status: **FIXED**
- File:
  - `00_multnomah.do`

### Problem

The startup check correctly set `global use_parallel = 0` when the `parallel` package was missing, but later the script unconditionally reset `global use_parallel = 1`, overriding the safety check.

### Fix applied

Changed the later assignment to `if "${use_parallel}" == "" global use_parallel = 1`, so the early safety check is honored.

## 4. BEA filename fallback was not honored by the cleaning step

- Severity: P1
- Status: **FIXED**
- File:
  - `code/stata/01c_demographics.do`

### Problem

The download step accepted either `CAINC1__ALL_AREAS_*.csv` or `CAINC1__ALL_STATES_*.csv`, but the cleaning step hardcoded the `ALL_AREAS` filename.

### Fix applied

Replaced the hardcoded import in `01c_demographics.do` with dynamic file resolution using the same AREAS-then-STATES fallback pattern as the downloader, with an informative error if neither is found.

## 5. API key parsing could silently return the wrong credential

- Severity: P2
- Status: **FIXED**
- File:
  - `code/R/utils.R`

### Problem

The API key helper used a fuzzy `grepl()` substring match and fell back to the first row if no match was found.

### Fix applied

Changed the lookup to exact `==` matching and removed the silent fallback. Missing keys now fail fast with a clear error.

## 6. `map_code.R` used `unit()` without explicit qualification

- Severity: ~~P2~~ P3
- Status: **FIXED**
- File:
  - `code/R/map_code.R`

### Problem

`unit()` worked in practice because `tidyverse` transitively loaded `grid`, but the dependency was implicit and fragile.

### Fix applied

Replaced the bare `unit()` calls with `grid::unit()` to make the dependency explicit.

## 7. Variable name mismatch in `01h_auxiliary.do`

- Severity: P0
- Status: **FIXED**
- File:
  - `code/stata/01h_auxiliary.do`

### Problem

The script kept `county_fips_code`, then tried to rename `county_fips`. That would raise "variable county_fips not found" before the childcare and property-tax outputs were built.

### Fix applied

Changed `rename county_fips fips` to `rename county_fips_code fips`.

## 8. Parallel interruption handling was inconsistent across the parallelized do-files

- Severity: P1
- Status: **FIXED**
- Files:
  - `code/stata/02_otherout_sdid.do`
  - `code/stata/02_quarterly_sdid.do`

### Problem

`02_sdid_analysis.do` already had checkpoint/resume behavior, but `02_otherout_sdid.do` and `02_quarterly_sdid.do` previously allowed stale temp files to contaminate reruns after interrupted parallel execution.

### Fix applied

Both files now follow the `02_sdid_analysis.do` pattern:

1. Add a `resume` global fallback for standalone runs.
2. Clear and recreate temp-result directories on fresh parallel runs.
3. Skip existing worker outputs when `resume == 1`.
4. Reuse completed-spec tracking in sequential resume mode.
5. Preserve the existing results dataset when resuming.

`02_flow_analysis.do` was already restart-safe through a clean-start temp-directory pattern and did not need changes.

## Space And Speed Opportunities

These items are lower severity than the runtime blockers above, but they are strong candidates for improving rerun time, reducing disk churn, and lowering peak memory use.

## 9. `overwrite_csv` defaults to full refreshes of large raw data assets

- Severity: P1
- Status: **ALREADY RESOLVED**
- File:
  - `00_multnomah.R`

### Problem

The original finding stated `overwrite_csv <- TRUE` was the default. In the current code, `overwrite_csv <- FALSE` (line 71), so cached datasets are reused by default. No change needed.

## 10. `01h_auxiliary.do` reloads a multi-GB ACS microdata file for a narrow property-tax task

- Severity: P1
- Status: **FIXED**
- Files:
  - `code/stata/01e_acs.do`
  - `code/stata/01h_auxiliary.do`

### Problem

`01e_acs.do` saves `acs_migration_file.dta`, and `01h_auxiliary.do` reloaded that full person-level file just to compute property-tax-rate aggregates from a small subset of columns. In the current workspace, `data/working/acs_migration_file.dta` is about 4.3 GB.

### Fix applied

`01e_acs.do` now saves a slim companion file `acs_proptx_slim.dta` (8 columns: year, relate, fips_d, proptx99, valueh, qprotx99, qvalueh, hhwt) immediately after the full ACS save. `01h_auxiliary.do` loads that slim file instead, reducing I/O from ~4.3 GB to ~200-400 MB (~90% reduction).

## 11. Sequential SDID scripts repeatedly append and resave growing result files inside inner loops

- Severity: P1
- Status: **FIXED**
- Files:
  - `code/stata/02_sdid_analysis.do` (~3,600 specs)
  - `code/stata/02_otherout_sdid.do` (~80 specs)
  - `code/stata/02_quarterly_sdid.do` (~120 specs, QCEW + QWI phases)

### Problem

The sequential code paths used a `preserve/clear/set obs 1/gen .../append using/save/restore` pattern inside the innermost estimation loop. Each iteration re-read and re-wrote the growing results file, creating O(n^2) disk I/O.

### Fix applied

Replaced the preserve/append/restore pattern with Stata's `postfile`/`post`/`postclose` mechanism (O(1) per spec). A single postfile handle is opened before the loops, each completed spec posts one row, and the handle is closed after all loops finish. Resume mode writes to a temporary `_new.dta` file which is merged into the main results after `postclose`. A `capture postclose` guard before each `postfile` declaration prevents stale-handle errors from interrupted runs. Event study `preserve`/`restore` blocks (used for plotting) and the `eststo`/`estadd`/`esttab` pipeline are untouched.

## 12. IRS processing keeps year-specific shard files even when only the combined datasets are used downstream

- Severity: P2
- Status: **FIXED**
- Files:
  - `code/stata/01f_irs_migration.do`
  - `code/stata/01g_irs_agi.do`

### Problem

Both IRS scripts save year-by-year working datasets and then reopen and append them into a combined file. Those shards are helpful for debugging, but they add disk usage and extra I/O during normal runs.

### Fix applied

Replaced permanent year-specific `.dta` saves with `tempfile` declarations. Both files now pre-declare tempfiles before the year loop, save each year's data to the tempfile, and append from tempfiles in the combine step. No permanent shard files are left on disk.

## 13. `qwi_data.R` and `qcew_data.R` hold more data in memory than they need to

- Severity: P2
- Files:
  - `code/R/qwi_data.R`
  - `code/R/qcew_data.R`

### Problem

`qwi_data.R` builds a nationwide `combined` object across all states before splitting it back out to quarterly CSVs, and `qcew_data.R` processes full yearly datasets before writing quarter chunks. This raises peak memory use, especially for QWI.

### Suggested fix

Write quarter outputs incrementally as state/year chunks are processed, or flush completed chunks earlier instead of building a single large combined object first.

## 14. Property-tax CSV exports appear to be unused downstream

- Severity: P2
- File:
  - `code/stata/01h_auxiliary.do`

### Problem

`01h_auxiliary.do` exports `property_tax_rates_overall.csv` and `property_tax_rates_excl_allocated.csv`, but I could not find any downstream code reading those CSVs back in. The `.dta` outputs appear to be the real working artifacts.

### Suggested fix

Remove those CSV exports, or guard them behind an optional export flag if they are only meant for ad hoc inspection.

## 15. ACS yearly imports write tempfiles and then reread them immediately for append

- Severity: P3
- File:
  - `code/stata/01e_acs.do`

### Problem

The ACS loader imports each year to a tempfile in one loop, then appends those tempfiles in a second loop. That is safe, but it writes and rereads each yearly import once before the combined dataset is built.

### Suggested fix

Append incrementally inside the import loop, using the first year as the seed dataset, to cut temporary I/O roughly in half.

## First-Time User And README Assessment

These items focus on whether a new downloader could realistically run the repo and whether the current README is explicit enough about setup and expectations.

## 16. The README does not document the TAXSIM dependency behind the revenue pipeline

- Severity: P1
- Files:
  - `README.md`
  - `STATA_REQUIREMENTS.txt`
  - `code/stata/02_revenue.do`

### Problem

The revenue workflow runs `taxsimlocal35`, but the README and Stata requirements file do not explain how to install or configure local TAXSIM. The script does contain a simplified fallback if TAXSIM fails, but a first-time user would not know whether that fallback is acceptable for replication or just a convenience for partial runs.

### Suggested fix

Add a dedicated README note for TAXSIM setup, and explicitly state whether the paper's preferred revenue results require a working local TAXSIM installation or whether the fallback path is considered acceptable.

## 17. The minimum supported Stata version is not stated clearly enough for new users

- Severity: P1
- Files:
  - `README.md`
  - `STATA_REQUIREMENTS.txt`

### Problem

Multiple scripts declare or assume modern Stata functionality, but the setup docs do not clearly say which Stata version is required. A first-time downloader can install all listed packages and still run into avoidable compatibility issues if they are on an older Stata release.

### Suggested fix

Add a short prerequisites block near the top of the README that states the minimum supported Stata version and, ideally, the version the repo was last tested on.

## 18. The README does not set expectations for runtime, storage, and download volume on a first run

- Severity: P2
- File:
  - `README.md`

### Problem

The README explains the data sources and run order well, but it does not warn new users that a full first run downloads a large number of multi-year datasets and produces large working files. That can be a practical onboarding problem even when the code itself is correct.

### Suggested fix

Add a brief "Expected first run" note with rough guidance on download size, disk usage, and the fact that the first full run may take substantial time.

## 19. The README could do a better job separating required replication steps from optional extras

- Severity: P2
- File:
  - `README.md`

### Problem

The README documents the two-stage R-then-Stata workflow clearly, but it does not sharply separate core replication steps from optional features such as Overleaf sync, post-Stata map reruns, optional parallel execution, and the revenue-step TAXSIM dependency. A first-time downloader may not know which issues are blocking versus optional.

### Suggested fix

Add a short checklist that distinguishes:

1. Minimum required steps to reproduce the main analysis.
2. Optional enhancements and convenience features.
3. Known non-blocking skips or fallback behaviors.

## Stylistic And Maintainability Nits

These items are lower priority than the runtime issues above. They are not the first things to fix, but they would make the repo easier to maintain and easier for collaborators to run.

### ~~20. Encoding artifacts appear in user-facing text and comments~~

- Status: **REJECTED** - Both `README.md` and `00_multnomah.R` are clean UTF-8 with no mojibake, no broken dashes, and no box-drawing characters. This finding was incorrect.

### 21. `map_code.R` is doing too many jobs in one script

- Severity: ~~P3~~ P4
- File:
  - `code/R/map_code.R`

#### Assessment

The file is 1,074 lines and produces about 65 PNGs across 5 map families, but it already factors out reusable helper functions and the map families share data loading. Splitting it further is low priority unless the file keeps growing.

### 22. Repeated hard-coded layout constants make maps difficult to tune

- Severity: P3 - **Confirmed**
- File:
  - `code/R/map_code.R`

#### Assessment

Padding value `50000` appears repeatedly, nudge coordinates are duplicated, and inset placement values are scattered across the file. A small constants block near the top would make visual tuning easier.

### 23. `map_code.R` imports more of the tidyverse than it appears to need

- Severity: ~~P3~~ P4
- File:
  - `code/R/map_code.R`

#### Assessment

Only a subset of tidyverse packages appears to be used, but `library(tidyverse)` is a common research-code convention and changing it is not worth much churn here.

### 24. The descriptive-analysis script contains substantial duplication

- Severity: P3 - **Confirmed**
- File:
  - `code/stata/02_descriptives.do`

#### Assessment

The file contains parallel IRS/ACS blocks, duplicated box-plot statistics logic, and repeated LaTeX table-writing boilerplate. Small Stata helper programs would reduce copy-paste drift.

### 25. `00_multnomah.R` mixes orchestration with package installation policy

- Severity: P3 - **Confirmed**
- File:
  - `00_multnomah.R`

#### Assessment

The script auto-installs missing packages from a hard-coded list with no opt-out flag. That is convenient in a research workflow, but an explicit `auto_install_packages <- TRUE` flag would make the behavior easier to control.

### 26. Some comments are out of date or more narrative than actionable

- Severity: P3 - **Partially confirmed**
- Files:
  - `code/R/qwi_data.R`
  - `code/stata/02_descriptives.do`

#### Assessment

`qwi_data.R` still references a previous API-based approach in an introductory comment, and `02_descriptives.do` has several comments that restate obvious code. By contrast, `api_code.R` comments are mostly fine and do not belong in this nit.

## Notes

- These findings came from static code review rather than a full pipeline run.
- I did not execute the end-to-end workflow because that would require external downloads, API keys, and Stata.
- The items are prioritized around correctness first, then rerun cost and maintainability.
