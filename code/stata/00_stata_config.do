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
if "${artifact_schema_version}" == "" global artifact_schema_version "2026-03-15"
if "${preferred_spec_version}" == ""  global preferred_spec_version "2026-03-main"

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
if "${n_clusters}" == "" global n_clusters = 6
if "${resume}" == ""     global resume = 0

if "${event_study_mode}" == "" global event_study_mode "preferred"

if "${start_year_irs_data}" == ""     global start_year_irs_data     = 2012
if "${start_year_irs_analysis}" == "" global start_year_irs_analysis = 2016
if "${start_year_acs}" == ""          global start_year_acs          = 2012
if "${end_year_acs}" == ""            global end_year_acs            = 2024

if "${start_yy_irs_download}" == "" global start_yy_irs_download = 11
if "${end_yy_irs_migration}" == "" global end_yy_irs_migration = 21
if "${end_yy_irs_agi}" == ""       global end_yy_irs_agi       = 22
if "${start_yy_irs_county}" == ""  global start_yy_irs_county  = 12
if "${end_yy_irs_county}" == ""    global end_yy_irs_county    = 22

** Overleaf sync (default off; set to 1 in profile.do with oth_path)
if "${overleaf}" == "" global overleaf = 0

capture mkdir "${results}"
capture mkdir "${logs}"

set scheme plotplainblind

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

    capture confirm file `"`using'"'
    if _rc != 0 {
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
