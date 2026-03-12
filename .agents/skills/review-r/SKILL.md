---
name: review-r
description: Run the R code review protocol on R scripts. Checks code quality, reproducibility, Stata parity, and research standards. Produces a report without editing files.
disable-model-invocation: true
argument-hint: "[filename or 'all']"
---

# Review R Scripts

Run the comprehensive R code review protocol.

## Steps

1. **Identify scripts to review:**
   - If `$ARGUMENTS` is a specific `.R` or `.Rmd` filename: review that file only
   - If `$ARGUMENTS` is `all`: review all R scripts in `R/`

2. **For each script, launch the `r-reviewer` agent** with instructions to:
   - Follow the full protocol in the agent instructions
   - Read `.Codex/rules/r-code-conventions.md` for current standards
   - Check for corresponding Stata files in `code/` and verify parity
   - Save report to `quality_reports/[script_name]_r_review.md`

3. **After all reviews complete**, present a summary:
   - Total issues found per script
   - Breakdown by severity (Critical / High / Medium / Low)
   - Top 3 most critical issues

4. **IMPORTANT: Do NOT edit any R source files.**
   Only produce reports. Fixes are applied after user review.
