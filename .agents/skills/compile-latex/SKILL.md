---
name: compile-latex
description: Compile a LaTeX file with pdflatex or xelatex (3-pass with bibtex). Reports errors and warnings.
disable-model-invocation: true
argument-hint: "[filename.tex]"
---

# Compile LaTeX

Compile a LaTeX document and report results.

## Steps

1. **Identify the file:** `$ARGUMENTS` should be a `.tex` file path

2. **Compile with 3-pass workflow:**
   ```
   pdflatex -interaction=nonstopmode $ARGUMENTS
   bibtex [basename]
   pdflatex -interaction=nonstopmode $ARGUMENTS
   pdflatex -interaction=nonstopmode $ARGUMENTS
   ```

3. **Check results:**
   - Report any compilation errors
   - List overfull/underfull hbox warnings
   - List undefined references or citations
   - Confirm PDF was generated

4. **Present summary** with pass/fail status and any issues found
