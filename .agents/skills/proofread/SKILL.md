---
name: proofread
description: Run the proofreading protocol on a document. Checks grammar, typos, consistency, and academic quality. Produces a report without editing files.
disable-model-invocation: true
argument-hint: "[filename]"
---

# Proofread Document

Run the comprehensive proofreading protocol.

## Steps

1. **Identify the file:** `$ARGUMENTS` should be a `.tex`, `.Rmd`, or `.md` file

2. **Launch the `proofreader` agent** with instructions to:
   - Review the file for grammar, typos, consistency, and academic quality
   - Check formatting specific to the file type (LaTeX, Rmd, etc.)
   - Save report to `quality_reports/[filename]_proofread.md`

3. **Present summary:**
   - Total issues found
   - Breakdown by category and severity
   - Top issues to address first

4. **IMPORTANT: Do NOT edit the source file.**
   Only produce the report. Fixes are applied after user review.
