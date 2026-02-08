# CLAUDE.md — Multnomah County Tax & Migration

**Last Updated:** 2026-02-07
**Project:** Multnomah County Tax Effects on Migration
**Authors:** John Iselin
**Working Branch:** main

---

## Quick Reference: Available Skills & Agents

| Command | What It Does |
|---------|-------------|
| `/review-r [file]` | R code review: quality, reproducibility, Stata parity |
| `/review-stata [file]` | Stata code review: specifications, samples, R parity |
| `/compile-latex [file.tex]` | 3-pass LaTeX compilation with bibtex |
| `/proofread [file]` | Grammar/typo/consistency review and report |

**Agents:** `r-reviewer`, `stata-reviewer`, `proofreader`, `verifier`

**Rules:** See `.claude/rules/` for plan-first workflow, orchestrator protocol, quality gates, verification, code conventions, and replication protocol.

---

## Project Overview

This project analyzes the effect of the Multnomah County income tax (enacted 2020, effective 2021) on migration patterns by education level. The analysis uses ACS microdata to examine whether the tax disproportionately affected migration of college-educated residents.

The codebase has parallel Stata and R implementations. **Changes to analysis code should generally be applied to both languages.**

---

## Folder Structure

```
multnomah-county-tax/
├── CLAUDE.md                    # This file
├── .claude/                     # Claude Code configuration
│   ├── settings.json
│   ├── rules/                   # Workflow rules (auto-loaded)
│   ├── agents/                  # Review agents
│   └── skills/                  # Slash commands
├── code/                        # Stata do-files
│   ├── 00_multnomah.do          # Master file
│   ├── 01_data_prep.do
│   ├── 02_did_analysis.do       # Main DiD + event study regressions
│   └── ...
├── R/                           # R scripts (mirrors Stata)
├── data/                        # Data files
│   └── working/                 # Processed datasets
├── results/                     # Output tables and figures
│   └── did/                     # DiD regression outputs
├── logs/                        # Stata log files
└── quality_reports/             # Review reports and plans
    ├── plans/
    └── session_logs/
```

---

## Key Design Decisions

### Samples
- **Sample 1:** Multnomah County residents (out-migration analysis)
- **Sample 2:** CA/OR/WA residents excluding Multnomah (in-migration, West Coast)
- **Sample 3:** Lower 48 + DC excluding Multnomah (in-migration, national)
- Age restriction: 25+ (changed from 18+)
- Year 2020 excluded from all regressions
- Sample period: 2016-2024

### Treatment Definition
- **Post:** year > 2020
- **High education:** `educd >= 101` (college degree)
- **Treated:** high_ed x post

### Regression Structure
1. **Reg 1:** Overall DiD (treated = college x post)
2. **Reg 2:** Event study (year x college interactions, base = 2019)
3. **Reg 3:** DiD by age (college x post x age group)
4. **Reg 4:** Event study by age (year x college x age group)
5. **Reg 5:** DiD by age, no education (age x post, omit 45-64)
6. **Reg 6:** Event study by age, no education (year x age, omit 45-64)

### Age Groups
- cat_age == 1: 25-44
- cat_age == 2: 45-64 (reference group in Regs 5-6)
- cat_age == 3: 65+

### IRS Migration Data
- Type 1: non-movers
- Type 2: same-state
- Type 3: domestic
- Type 5: interstate (plotted separately in spec curves)

### Standard Errors
- Sample 1 (Multnomah residents): `vce(robust)` — single origin county
- Samples 2 & 3 (multiple origins): `vce(cluster fips_o)`

### Fixed Effects
- All regressions absorb: `year`, `fips_o` (samples 2-3), `cat_*` (age, sex, marital, children, youngest child, education)

---

## Working Philosophy

### Collaborative Partnership
- User drives the research direction and specification choices
- Claude proposes implementations and flags potential issues
- Changes require user approval before implementation
- Always re-read files before editing (user frequently edits externally)

### Plan-First Approach
For non-trivial tasks, enter plan mode first. Save plans to `quality_reports/plans/`. See `.claude/rules/plan-first-workflow.md`.

### Stata-R Parity
When modifying analysis code, check if the other language version needs updating. See `.claude/rules/replication-protocol.md`.

### Tables
- DiD tables use unweighted N (`estadd scalar N_unwtd`)
- Star levels: * 0.10, ** 0.05, *** 0.01
- Coefficient formatting: `%-9.3f`

---

## Upcoming Work

- Convert Word document into LaTeX paper and Beamer presentation
- The `/compile-latex` skill and `proofreader` agent are available for this workflow
