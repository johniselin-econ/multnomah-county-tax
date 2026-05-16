/*******************************************************************************
File Name:      02_intime_placebo.do
Creator:        John Iselin
Date Updated:   May 2026

Called by:      00_multnomah.do

Purpose:        In-time placebo test for the SDID. Re-estimates the
                treatment effect for AGI net migration as if PFA had
                taken effect in 2017, 2018, or 2019 instead of 2021.
                Estimation sample is the 2012-2019 window (excluding
                2020), giving each placebo treatment year at least
                five pre-treatment years. The actual PFA shock cannot
                contaminate the pseudo-post period because all
                post-2020 rows are dropped.

                Uses the `irs_intime` and `acs_intime` panel indicators
                from 02_sdid_analysis.do; canonical specs continue to
                use `irs_sample_1` / `acs_period_2` and are unaffected.

                A clean in-time placebo (estimates clustered near zero
                across pretend treatment years) supports the headline
                interpretation that the 2021 result reflects PFA rather
                than pre-existing differential trends.

Inputs:
- ${data}working/sdid_analysis_data.dta (built by 02_sdid_analysis.do;
  must include the 2012-2015 rows + `irs_intime` / `acs_intime`
  indicators introduced when the panel was extended back to 2012)
- ${results}sdid/sdid_results.dta       (for the actual-2021 reference line)

Outputs:
- ${results}sdid/intime/temp/results_*.dta             (per-cell point estimates)
- ${results}sdid/intime/sdid_results_intime.dta        (aggregated)
- ${results}sdid/intime/fig_intime_placebo_<dset>.{pdf,jpg}
- ${results}sdid/intime/fig_intime_placebo_combined.{pdf,jpg}
- ${ol_fig}fig_intime_placebo_combined.pdf             (if overleaf==1)

Authors: John Iselin
For more information, contact john.iselin@yale.edu
*******************************************************************************/

** Load shared project defaults and helper programs
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
if "${code}" == "" global code "${dir}/code/stata/"
do "${code}00_stata_config.do"
do "${code}01a_programs.do"
do "${code}02_spec_engine.do"

capture log close log_02_intime
log using "${logs}02_log_intime_placebo_${date}", replace text name(log_02_intime)
project_set_seed, context("02_intime_placebo.do") offset(90000)

capture mkdir "${results}sdid/intime"
capture mkdir "${results}sdid/intime/temp"

** Two underlying panels — the four preferred specs reduce to two
** county-level data sources. The `*_intime` samples extend back to 2012
** so each placebo year has at least five pre-treatment years.
local sampledatas "irs_intime acs_col_intime"

** Placebo years (treatment pretends to start at each)
local placebo_years "2017 2018 2019"

** All five donor pools — the in-time placebo is informative if any pool
** lights up under a pretend treatment year
local pools "sample_all sample_urban95 sample_urban75_covid sample_demog sample_stringency"

** Clean stale temp files (matches 02_bootstrap.do convention)
local _stale : dir "${results}sdid/intime/temp" files "results_*.dta"
local _n_stale : word count `_stale'
if `_n_stale' > 0 {
    dis as text "Clearing `_n_stale' stale per-cell file(s) from prior run"
    foreach f of local _stale {
        capture erase "${results}sdid/intime/temp/`f'"
    }
}

** ----------------------------------------------------------------------------
** Main loop: dataset x placebo_year x donor_pool x controls
** ----------------------------------------------------------------------------
foreach sd of local sampledatas {

    dis _n "{hline 70}"
    dis "Dataset: `sd'"
    dis "{hline 70}"

    load_spec_panel, sampledata("`sd'")
    local out_type   `"`r(out_type)'"'
    local covariates `"`r(covariates)'"'

    local outcome "agi_net_rate_`out_type'"
    capture confirm variable `outcome'
    if _rc != 0 {
        dis as error "  Outcome `outcome' not found for `sd' — skipping dataset"
        continue
    }

    foreach py of local placebo_years {

        dis _n "  Placebo year: `py'"

        foreach pool of local pools {
            forvalues c = 0/1 {

                ** Override Treated for the placebo year. The panel arrives
                ** with Treated = (multnomah & year > 2020); we redefine it
                ** here and restrict estimation to pre-2021.
                cap drop Treated_intime
                gen byte Treated_intime = multnomah == 1 & year >= `py'

                tempvar in_sample
                gen byte `in_sample' = `pool' == 1 & year <= 2020 & year != 2020

                ** Confirm we still have at least 2 pseudo-post years
                qui count if multnomah == 1 & Treated_intime == 1 & `in_sample' == 1
                if r(N) < 2 {
                    dis "    Skip `pool' c=`c' (py=`py'): only `=r(N)' pseudo-post years"
                    drop `in_sample' Treated_intime
                    continue
                }

                local covars ""
                if `c' == 1 local covars "covariates(`covariates')"

                ** Swap Treated_intime in for Treated in the sdid call. The
                ** package requires the treatment variable to be named so
                ** we pass Treated_intime directly.
                capture noisily sdid `outcome' fips year Treated_intime ///
                    if `in_sample' == 1, ///
                    vce(placebo) reps(100) `covars'

                if _rc != 0 {
                    sdid_log_failure, rc(`=_rc') script("02_intime_placebo") ///
                        tableid("`sd'_`py'_`pool'_`c'") outcome("`outcome'") ///
                        c(`c') exl(1) samp("`pool'") context("intime-placebo")
                    drop `in_sample' Treated_intime
                    continue
                }

                local tau = e(ATT)
                local se  = .
                capture local se = e(se)

                drop `in_sample' Treated_intime

                ** Persist one-row result
                preserve
                clear
                set obs 1
                gen str40  dataset      = "`sd'"
                gen int    placebo_year = `py'
                gen str40  donor_pool   = "`pool'"
                gen byte   controls     = `c'
                gen double tau          = `tau'
                gen double se           = `se'
                gen double ci_lower     = tau - 1.96 * se
                gen double ci_upper     = tau + 1.96 * se
                gen byte   significant  = abs(tau/se) > 1.96 if !missing(se)
                save "${results}sdid/intime/temp/results_`sd'_`py'_`pool'_`c'.dta", replace
                restore
            }
        }
    }
}

** ----------------------------------------------------------------------------
** Aggregate temp files (mirrors pattern in 02_sdid_analysis.do:999)
** ----------------------------------------------------------------------------
clear
local files : dir "${results}sdid/intime/temp" files "results_*.dta"
local n_files : word count `files'
if `n_files' == 0 {
    dis as error "No temp result files found — aborting"
    exit 459
}
foreach f of local files {
    append using "${results}sdid/intime/temp/`f'"
}
save "${results}sdid/intime/sdid_results_intime.dta", replace
dis _n "Aggregated `n_files' per-cell results to sdid_results_intime.dta"

** ----------------------------------------------------------------------------
** Plotting
** ----------------------------------------------------------------------------
local col_ref "213 94 0"    // vermillion — actual 2021 estimate reference
local col_dot "0 114 178"   // sea — placebo point estimates

** Pre-compute actual-2021 reference taus once. sdid_results.dta does not
** carry `preferred` or `migration` columns — those get added downstream by
** 02_post_spec.do. Filter on the underlying columns instead.
tempname ref_irs ref_acs
preserve
use "${results}sdid/sdid_results.dta", clear
keep if sample == "sample_stringency" & controls == 1 & exclusion == 1 ///
    & regexm(outcome, "^agi_net_rate_")
qui summ tau if sample_data == "irs_full_16_22"
scalar `ref_irs' = r(mean)
qui summ tau if sample_data == "acs_16_24_col"
scalar `ref_acs' = r(mean)
restore

foreach sd of local sampledatas {

    ** Map intime sample IDs to their canonical-sample reference taus.
    ** The reference comes from sdid_results.dta, which is keyed by the
    ** canonical sample names; the intime panel just shifts the year window.
    local sd_label ""
    if "`sd'" == "irs_intime" {
        local actual_tau = scalar(`ref_irs')
        local sd_label "IRS (2012-2019)"
    }
    if "`sd'" == "acs_col_intime" {
        local actual_tau = scalar(`ref_acs')
        local sd_label "ACS college (2012-2019)"
    }

    ** In-time placebo points
    preserve
    use "${results}sdid/intime/sdid_results_intime.dta", clear
    keep if dataset == "`sd'"

    ** Small horizontal jitter so the donor pools don't overlap exactly
    ** at each placebo year
    gen pool_idx = .
    replace pool_idx = -0.2 if donor_pool == "sample_all"
    replace pool_idx = -0.1 if donor_pool == "sample_urban95"
    replace pool_idx =  0   if donor_pool == "sample_urban75_covid"
    replace pool_idx =  0.1 if donor_pool == "sample_demog"
    replace pool_idx =  0.2 if donor_pool == "sample_stringency"
    gen pyear_jit = placebo_year + pool_idx

    local ref_opt ""
    if !missing(`actual_tau') {
        local ref_opt `"yline(`actual_tau', lc("`col_ref'") lp(solid) lwidth(medthick))"'
    }

    twoway ///
        (rcap ci_lower ci_upper pyear_jit, lcolor(gs10)) ///
        (scatter tau pyear_jit, msymbol(O) mcolor("`col_dot'") msize(small)), ///
        yline(0, lc(black) lp(dash)) ///
        `ref_opt' ///
        xlabel(2017 2018 2019) ///
        xtitle("Placebo treatment year", size(small)) ///
        ytitle("SDID estimate (pp)", size(small)) ///
        title("`sd_label'", size(small)) ///
        legend(off) ///
        graphregion(color(white)) plotregion(color(white)) ///
        name(intime_`sd', replace)

    graph export "${results}sdid/intime/fig_intime_placebo_`sd'.pdf", replace
    graph export "${results}sdid/intime/fig_intime_placebo_`sd'.jpg", as(jpg) quality(100) replace
    restore
}

** Combined figure
capture graph combine intime_irs_intime intime_acs_col_intime, ///
    cols(2) graphregion(color(white))
if _rc == 0 {
    graph export "${results}sdid/intime/fig_intime_placebo_combined.pdf", replace
    graph export "${results}sdid/intime/fig_intime_placebo_combined.jpg", as(jpg) quality(100) replace
    if ${overleaf} == 1 {
        graph export "${ol_fig}fig_intime_placebo_combined.pdf", replace
    }
    dis _n "Wrote: ${results}sdid/intime/fig_intime_placebo_combined.pdf"
}
else {
    dis as error "graph combine failed"
}

dis _n "{hline 70}"
dis "02_intime_placebo.do complete"
dis "{hline 70}"

capture log close log_02_intime
