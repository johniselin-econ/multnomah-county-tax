/*******************************************************************************
    File Name:    code/utils/globals.do   (formerly 00_stata_config.do)
    Creator:      John Iselin
    Date Updated: May 31, 2026

    Purpose:      Single source of truth for project SETTINGS: path globals,
                  master seed, analysis parameters, run-control defaults, the
                  required-package check, output-directory creation, and
                  Overleaf resolution. Sourced by every entry script (the
                  orchestrator and standalone callees) via the locator preamble.
                  Loads all program definitions from programs.do at the end, so
                  sourcing this file yields paths + parameters + programs in one
                  step. This file defines NO programs itself.
*******************************************************************************/

** Bootstrap project root if the caller's locator preamble did not set it.
** Handles a working directory at the repo root, code/stata, or code/utils.
if "${dir}" == "" {
    local cwd = subinstr("`c(pwd)'", "\", "/", .)
    if regexm("`cwd'", "(.*)/code/(stata|utils)$") global dir = regexs(1)
    else global dir "`cwd'"
}

if "${code}" == ""    global code    "${dir}/code/stata/"
if "${utils}" == ""   global utils   "${dir}/code/utils/"
if "${rcode}" == ""   global rcode   "${dir}/code/R/"
if "${data}" == ""    global data    "${dir}/data/"
if "${results}" == "" global results "${dir}/results/"
if "${logs}" == ""    global logs    "${code}logs/"
if "${pr_name}" == "" global pr_name "multnomah"
if "${date}" == ""    global date "`: di %tdCY-N-D daily("$S_DATE", "DMY")'"

** Shared defaults for reproducibility and artifact signatures
if "${master_seed}" == ""            global master_seed = 56403
if "${bootstrap_seed_offset}" == ""  global bootstrap_seed_offset = 60000
if "${artifact_schema_version}" == "" global artifact_schema_version "2026-03-15"
if "${preferred_spec_version}" == ""  global preferred_spec_version "2026-03-main"

** ------------------------------------------------------------------
** Run-control defaults + parallel resolution. Entry scripts (the
** orchestrator's RUN-CONTROL FLAGS panel, or a standalone callee) may set
** these before sourcing this file; the lazy-sets below fill in anything
** left unset. Either way we resolve use_parallel against actual `parallel'
** availability here, in one place: an intent of 1 means "use it if
** installed", so a 1 with the package missing downgrades to 0.
** ------------------------------------------------------------------
capture which parallel
local has_parallel = (_rc == 0)
if "${use_parallel}" == "" global use_parallel = `has_parallel'
if ${use_parallel} == 1 & !`has_parallel' {
    di as txt "  Note: `parallel' package not installed. Downgrading use_parallel 1 -> 0."
    global use_parallel = 0
}
if "${n_clusters}" == "" global n_clusters = 4
if "${resume}" == ""     global resume = 0

** Event-study mode: "all" runs event studies for every SDID spec (needed for
** the full distribution of stock elasticity in 02_post_spec.do). "preferred"
** restricts to the 4 domestic baseline specs (sample_all × c=1 × exl=1 ×
** {irs_full_16_22, acs_16_24_col}) — much faster when you only need the
** main table's cumulative stock column. Orchestrator override in PROJECT
** GLOBALS — RUN-CONTROL FLAGS.
if "${event_study_mode}" == "" global event_study_mode "all"

if "${start_year_irs_data}" == ""     global start_year_irs_data     = 2012
if "${start_year_irs_analysis}" == "" global start_year_irs_analysis = 2016
if "${start_year_acs}" == ""          global start_year_acs          = 2012
if "${end_year_acs}" == ""            global end_year_acs            = 2024

if "${start_yy_irs_download}" == "" global start_yy_irs_download = 11
if "${end_yy_irs_migration}" == "" global end_yy_irs_migration = 21
if "${end_yy_irs_agi}" == ""       global end_yy_irs_agi       = 22
if "${start_yy_irs_county}" == ""  global start_yy_irs_county  = 12
if "${end_yy_irs_county}" == ""    global end_yy_irs_county    = 22

** ------------------------------------------------------------------
** Overleaf sync. Enabled iff a (gitignored) user_settings.do at the repo
** root defines ${oth_path}; that single switch drives ol_fig / ol_tab and
** the overleaf flag read by downstream graph export / esttab. No
** user_settings.do (or no oth_path) -> overleaf 0, no sync.
** ------------------------------------------------------------------
global ol_fig ""
global ol_tab ""
capture do "${dir}/user_settings.do"
if "${oth_path}" != "" {
    global ol_fig "${oth_path}figures/"
    global ol_tab "${oth_path}tables/"
    global overleaf = 1
}
else global overleaf = 0

** ------------------------------------------------------------------
** PFA policy & calibration constants (tax year 2022)
** Source: Multnomah Co. Ordinance 1269, 2023 Annual Report
** ------------------------------------------------------------------
if "${pfa_rate}" == ""             global pfa_rate             = 0.015
if "${pfa_thresh1_single}" == ""   global pfa_thresh1_single   = 125000
if "${pfa_thresh2_single}" == ""   global pfa_thresh2_single   = 250000
if "${pfa_thresh1_joint}" == ""    global pfa_thresh1_joint    = 200000
if "${pfa_thresh2_joint}" == ""    global pfa_thresh2_joint    = 400000
** Portland Metro Supportive Housing Services (SHS) — effective 2021.
** Flat 1% on income above the PFA tier-1 thresholds ($125K single / $200K joint).
** Used only as a sensitivity denominator for Kleven-style elasticities; SHS revenue
** accrues to Metro, not Multnomah, so it does not enter PFA baseline calcs.
if "${shs_rate}" == ""             global shs_rate             = 0.01
** Actual collections used to rescale simulation output.
** actual_pfa_revenue: Multnomah County PFA collections (county-level tax).
** statewide_oregon_revenue: Oregon STATEWIDE individual income tax collections.
**   The Multnomah-resident share is computed at runtime in 02_revenue_microsim.do
**   (statewide × Multnomah's IRS AGI share) and stored in actual_oregon_revenue,
**   so the table comparison is apples-to-apples with the simulated baseline
**   (which is also Multnomah-resident-only).
if "${actual_pfa_revenue}" == ""        global actual_pfa_revenue        = 187000000
if "${statewide_oregon_revenue}" == ""  global statewide_oregon_revenue  = 11772886000
** CPI-U inflation factor, 2019 annual -> 2022 annual (BLS series CUUR0000SA0)
if "${cpi_2019_to_2022}" == ""     global cpi_2019_to_2022     = 1.136

** Output directories (created here so standalone callees have them too).
foreach d in "" "tables" "figures" "sdid" "flows" "did" "individual" {
    capture mkdir "${results}`d'"
}
capture mkdir "${logs}"

** ------------------------------------------------------------------
** plotplainblind palette (RGB) — shared by SDID and elasticity spec curves
** so both renderers pull from one source of truth. Scoped as globals
** rather than locals so 02_sdid_analysis.do, 02_tables_figures.do, and
** future renderers can reference them without redeclaring.
** ------------------------------------------------------------------
if "${col_sig_notpref}" == ""   global col_sig_notpref   "0 114 178"     // sea (p7)        — sig, not preferred
if "${col_insig_notpref}" == "" global col_insig_notpref "86 180 233"    // sky (p3)        — insig, not preferred
if "${col_sig_pref}" == ""      global col_sig_pref      "213 94 0"      // vermillion (p6) — sig, preferred
if "${col_insig_pref}" == ""    global col_insig_pref    "230 159 0"     // orangebrown (p8)— insig, preferred
if "${col_zero}" == ""          global col_zero          "204 121 167"   // reddish (p5)    — zero line
if "${col_ref}" == ""           global col_ref           "153 153 153"   // gs10 (p2)       — reference lines

** ------------------------------------------------------------------
** Pre-flight: required packages (single authoritative list for the whole
** pipeline). Fails fast with an install hint rather than dying cryptically
** later on `command not found'. See STATA_REQUIREMENTS.txt for install lines.
** ------------------------------------------------------------------
local required_pkgs reghdfe ftools ppmlhdfe sdid sdid_event estout coefplot fre distinct
foreach pkg of local required_pkgs {
    capture which `pkg'
    if _rc {
        di as error "Missing required package: `pkg'"
        di as error "  See STATA_REQUIREMENTS.txt for install instructions."
        exit 199
    }
}
capture findfile scheme-plotplainblind.scheme
if _rc {
    di as error "Missing required package: blindschemes (scheme plotplainblind)"
    di as error "  See STATA_REQUIREMENTS.txt for install instructions."
    exit 199
}
set scheme plotplainblind

** Optional packages (soft-checked — callers have fallbacks): `parallel'
** auto-downgrades use_parallel above; taxsimlocal35 falls back to
** taxsim_fallback_calc in programs.do.
foreach pkg in parallel taxsimlocal35 {
    capture which `pkg'
    if _rc di as txt "  Note: optional package `pkg' not installed (soft dependency)."
}


** ------------------------------------------------------------------
** Load shared programs (data helpers + project_* manifest / seed /
** preferred-spec utilities). Defined in programs.do so this file holds
** settings only. Sourcing globals.do therefore yields paths,
** parameters, AND all programs in one step.
** ------------------------------------------------------------------
do "${utils}programs.do"
