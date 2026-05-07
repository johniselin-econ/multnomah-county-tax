/*******************************************************************************
    File Name:    00_stata_config.do
    Creator:      John Iselin
    Date Updated: March 15, 2026

    Purpose:      Shared Stata configuration helpers for standalone scripts and
                  orchestrated runs. Centralizes project globals, master seed,
                  preferred-spec rules, and lightweight manifest utilities used
                  to standardize major artifacts across reruns.
*******************************************************************************/

** Bootstrap project root if globals are not already defined
local cwd = subinstr("`c(pwd)'", "\", "/", .)
local suffix "/code/stata"

if "${dir}" == "" {
    if length("`cwd'") >= length("`suffix'") & ///
        substr("`cwd'", length("`cwd'") - length("`suffix'") + 1, .) == "`suffix'" {
        global dir = substr("`cwd'", 1, length("`cwd'") - length("`suffix'"))
    }
    else {
        global dir "`cwd'"
    }
}

if "${code}" == ""    global code    "${dir}/code/stata/"
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
** Run-control fallbacks. The orchestrator (00_multnomah.do PROJECT
** GLOBALS — RUN-CONTROL FLAGS section) sets each of these explicitly,
** so the lazy-sets here become no-ops in a full pipeline run. They
** stay as safety nets for callee scripts run standalone.
** ------------------------------------------------------------------
capture which parallel
local has_parallel = (_rc == 0)
if "${use_parallel}" == "" {
    if `has_parallel' {
        global use_parallel = 1
    }
    else {
        global use_parallel = 0
    }
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

** Overleaf sync (default off; orchestrator panel sets explicitly, then the
** profile.do block in 00_multnomah.do auto-promotes to 1 if oth_path defined)
if "${overleaf}" == "" global overleaf = 0

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

capture mkdir "${results}"
capture mkdir "${logs}"

set scheme plotplainblind

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
** Pre-flight: required SSC packages
** Fails fast with an install hint rather than letting the pipeline die
** cryptically later on `command not found`.
** ------------------------------------------------------------------
local required_ssc reghdfe ppmlhdfe coefplot estout distinct sdid
foreach pkg of local required_ssc {
    capture which `pkg'
    if _rc {
        di as error "Missing required package: `pkg'"
        di as error "  Install with: ssc install `pkg'"
        exit 199
    }
}
** Non-SSC packages (soft-checked — callers handle their own fallbacks):
**   parallel:      net install parallel, from(https://raw.github.com/gvegayon/parallel/stable/) replace
**   taxsimlocal35: ssc install taxsim35  (or see https://taxsim.nber.org/taxsim35/)

capture program drop project_set_seed
program define project_set_seed
    syntax , CONTEXT(string) [OFFSET(integer 0)]

    local seed = ${master_seed} + `offset'
    set seed `seed'
    di as txt "Seed set to `seed' (`context')"
end

capture program drop project_build_signature
program define project_build_signature, rclass
    syntax , ARTIFACT(string)

    local signature ///
        "schema=${artifact_schema_version}|artifact=`artifact'|pref=${preferred_spec_version}|seed=${master_seed}|irs_data=${start_year_irs_data}|irs_analysis=${start_year_irs_analysis}|acs_start=${start_year_acs}|acs_end=${end_year_acs}|irs_dl=${start_yy_irs_download}|irs_mig=${end_yy_irs_migration}|irs_agi=${end_yy_irs_agi}|irs_cty=${start_yy_irs_county}-to-${end_yy_irs_county}"

    return local signature "`signature'"
end

capture program drop project_export_run_manifest
program define project_export_run_manifest
    syntax

    local stamp = subinstr("`c(current_time)'", ":", "", .)

    project_build_signature, artifact("run_manifest")
    local signature "`r(signature)'|parallel=${use_parallel}|clusters=${n_clusters}|resume=${resume}|event=${event_study_mode}"

    preserve
    clear
    set obs 1

    gen str40 project_name = "${pr_name}"
    gen str120 project_dir = "${dir}"
    gen str244 config_signature = "`signature'"
    gen str20 run_date = "${date}"
    gen str20 run_time = "`c(current_time)'"
    gen double master_seed = real("${master_seed}")
    gen double use_parallel = real("${use_parallel}")
    gen double n_clusters = real("${n_clusters}")
    gen double resume = real("${resume}")
    gen str20 event_study_mode = "${event_study_mode}"
    gen double start_year_irs_data = real("${start_year_irs_data}")
    gen double start_year_irs_analysis = real("${start_year_irs_analysis}")
    gen double start_year_acs = real("${start_year_acs}")
    gen double end_year_acs = real("${end_year_acs}")
    gen double start_yy_irs_download = real("${start_yy_irs_download}")
    gen double end_yy_irs_migration = real("${end_yy_irs_migration}")
    gen double end_yy_irs_agi = real("${end_yy_irs_agi}")
    gen double start_yy_irs_county = real("${start_yy_irs_county}")
    gen double end_yy_irs_county = real("${end_yy_irs_county}")
    gen str40 preferred_spec_version = "${preferred_spec_version}"
    gen str40 artifact_schema_version = "${artifact_schema_version}"

    save "${results}run_manifest_${date}_`stamp'.dta", replace
    export delimited using "${results}run_manifest_${date}_`stamp'.csv", replace
    save "${results}run_manifest_latest.dta", replace
    export delimited using "${results}run_manifest_latest.csv", replace
    restore
end

capture program drop project_write_manifest
program define project_write_manifest
    syntax using/, ARTIFACT(string) SCRIPT(string) [UPSTREAM(string)]

    project_build_signature, artifact("`artifact'")
    local signature "`r(signature)'"
    local csv_path = subinstr(`"`using'"', ".dta", ".csv", .)

    preserve
    clear
    set obs 1

    gen str80 artifact = "`artifact'"
    gen str120 script_name = "`script'"
    gen str244 config_signature = "`signature'"
    gen str244 upstream_signature = "`upstream'"
    gen str20 created_date = "${date}"
    gen str20 created_time = "`c(current_time)'"
    gen double master_seed = real("${master_seed}")
    gen str40 preferred_spec_version = "${preferred_spec_version}"
    gen str40 artifact_schema_version = "${artifact_schema_version}"
    gen double start_year_irs_data = real("${start_year_irs_data}")
    gen double start_year_irs_analysis = real("${start_year_irs_analysis}")
    gen double start_year_acs = real("${start_year_acs}")
    gen double end_year_acs = real("${end_year_acs}")

    save `"`using'"', replace
    export delimited using `"`csv_path'"', replace
    restore
end

capture program drop project_assert_manifest
program define project_assert_manifest
    syntax using/, ARTIFACT(string)

    if !fileexists(`"`using'"') {
        di as error "ERROR: Required manifest not found: `using'"
        exit 601
    }

    project_build_signature, artifact("`artifact'")
    local expected "`r(signature)'"

    preserve
    use `"`using'"', clear
    qui levelsof artifact if _n == 1, local(actual_artifact) clean
    qui levelsof config_signature if _n == 1, local(actual_signature) clean
    restore

    if "`actual_artifact'" != "`artifact'" {
        di as error "ERROR: Manifest artifact mismatch for `using'"
        di as error "       Expected artifact: `artifact'"
        di as error "       Found artifact:    `actual_artifact'"
        exit 459
    }

    if "`actual_signature'" != "`expected'" {
        di as error "ERROR: Stale or incompatible artifact manifest detected for `artifact'."
        di as error "       Expected signature: `expected'"
        di as error "       Found signature:    `actual_signature'"
        exit 459
    }
end

capture program drop project_mark_preferred_main
program define project_mark_preferred_main
    capture drop preferred
    gen preferred = 0

    ** Highlighted main SDID specifications:
    ** IRS x {All Counties, Stringency Match}
    ** ACS College x {All Counties, Stringency Match}

    replace preferred = 1 if                                 ///
        data_type == "IRS" &                                 ///
        period_type == "16-22" &                             ///
        inlist(sample, "sample_all", "sample_stringency") &  ///
        controls == 1 &                                      ///
        exclusion == 1

    replace preferred = 1 if                                 ///
        data_type == "ACS College" &                         ///
        period_type == "16-24" &                             ///
        inlist(sample, "sample_all", "sample_stringency") &  ///
        controls == 1 &                                      ///
        exclusion == 1

    replace preferred = 1 if                                 ///
        data_type == "IRS (Out-of-State)" &                  ///
        period_type == "16-22" &                             ///
        inlist(sample, "sample_all", "sample_stringency") &  ///
        controls == 1 &                                      ///
        exclusion == 1

    replace preferred = 1 if                                 ///
        data_type == "ACS College (Out-of-State)" &          ///
        period_type == "16-24" &                             ///
        inlist(sample, "sample_all", "sample_stringency") &  ///
        controls == 1 &                                      ///
        exclusion == 1
end
