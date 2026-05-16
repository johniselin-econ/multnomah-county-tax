/*******************************************************************************
File Name:      02_placebo_distribution.do
Creator:        John Iselin
Date Updated:   May 2026

Called by:      00_multnomah.do

Purpose:        Recover the donor-county placebo distribution from the
                Clarke `sdid` package for each of the four highlighted
                "preferred" specifications, and plot it as a histogram
                with Multnomah's estimated tau marked. Produces the
                appendix figure referenced as fig:placebo_distribution.

                Designed to be run AFTER 02_sdid_analysis.do (depends on
                sdid_results.dta to confirm which specs are preferred).

Inputs:
- ${data}working/sdid_analysis_data.dta (built by 02_sdid_analysis.do)
- ${results}sdid/sdid_results.dta       (built by 02_sdid_analysis.do)

Outputs:
- ${results}sdid/placebo_dist/fig_placebo_<dataset>.{pdf,jpg}  (4 panels)
- ${results}sdid/placebo_dist/fig_placebo_combined.{pdf,jpg}    (2x2 combined)
- ${ol_fig}fig_placebo_combined.pdf                            (if overleaf==1)

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
do "${code}02_spec_engine.do"

capture log close log_02_placebo
log using "${logs}02_log_placebo_distribution_${date}", replace text name(log_02_placebo)
project_set_seed, context("02_placebo_distribution.do") offset(80000)

** Output dir
capture mkdir "${results}sdid/placebo_dist"

** Colors (plotplainblind)
local col_mult     "213 94 0"      // vermillion — Multnomah estimate
local col_placebo  "0 114 178"     // sea — placebo distribution

** ----------------------------------------------------------------------------
** Four highlighted preferred specifications
**     stringency-matched donor pool, controls=1, exclude 2020,
**     across IRS-full, IRS-outstate, ACS-college, ACS-college-outstate
** ----------------------------------------------------------------------------
local sampledatas "irs_full_16_22 acs_16_24_col irs_outstate_full_16_22 acs_outstate_16_24_col"
local labels      `""IRS (county)" "ACS-college (county)" "IRS (out-of-state)" "ACS-college (out-of-state)""'

local n_specs : word count `sampledatas'

** ----------------------------------------------------------------------------
** Loop preferred specs: load panel, refit SDID with 500 reps, extract
** placebo distribution, plot
** ----------------------------------------------------------------------------
forvalues s = 1/`n_specs' {
    local sd  : word `s' of `sampledatas'
    local lab : word `s' of `labels'

    dis _n "{hline 70}"
    dis "Spec `s'/`n_specs': `sd'"
    dis "{hline 70}"

    ** Load the panel via the shared helper (sets out_type, covariates)
    load_spec_panel, sampledata("`sd'")
    local out_type   `"`r(out_type)'"'
    local covariates `"`r(covariates)'"'

    ** Match the preferred-spec outcome var: agi_net_rate_<out_type>
    local outcome "agi_net_rate_`out_type'"
    capture confirm variable `outcome'
    if _rc != 0 {
        dis as error "  Outcome `outcome' not found for `sd' — skipping"
        continue
    }

    ** Stringency-matched donor pool
    capture confirm variable sample_stringency
    if _rc != 0 {
        dis as error "  sample_stringency not found — skipping"
        continue
    }

    ** Apply exclusion: drop 2020 (matches preferred-spec exclusion=1)
    tempvar in_sample
    gen byte `in_sample' = sample_stringency == 1 & year != 2020

    ** ----- 1) Actual Multnomah estimate (no inference needed; just the tau)
    capture noisily sdid `outcome' fips year Treated if `in_sample' == 1, ///
        vce(noinference) covariates(`covariates')

    if _rc != 0 {
        dis as error "  sdid failed for `sd' actual fit (rc=`=_rc') — skipping"
        drop `in_sample'
        continue
    }

    local mult_tau = e(ATT)

    ** ----- 2) All-donor permutation placebo distribution
    ** Treat each non-Multnomah donor county as if it were the treated unit
    ** and record its SDID tau. The Clarke sdid package's vce(placebo)
    ** computes an SE from random placebo draws but does not expose the
    ** per-iteration taus; we re-implement the classical all-donor
    ** permutation here so the distribution is plottable.
    qui levelsof fips if multnomah == 0 & `in_sample' == 1, local(donor_fips) clean
    local n_donors : word count `donor_fips'
    dis "  `n_donors' donor counties in stringency pool — looping permutations"

    tempname placebo_mat
    matrix `placebo_mat' = J(`n_donors', 1, .)
    tempvar Treated_placebo
    gen byte `Treated_placebo' = 0

    local i = 0
    foreach d of local donor_fips {
        local ++i
        qui replace `Treated_placebo' = (fips == `d' & year > 2020)

        capture qui sdid `outcome' fips year `Treated_placebo' if `in_sample' == 1, ///
            vce(noinference) covariates(`covariates')
        if _rc == 0 {
            matrix `placebo_mat'[`i', 1] = e(ATT)
        }
        if mod(`i', 20) == 0 dis "    permutation `i'/`n_donors'"
    }

    drop `Treated_placebo' `in_sample'

    ** Pull placebo values into a fresh dataset for plotting
    preserve
    clear
    qui svmat double `placebo_mat', names(placebo_tau)
    qui rename placebo_tau1 placebo_tau
    qui drop if missing(placebo_tau)

    ** Two-sided placebo p-value: share of |placebo| >= |actual|
    qui count if abs(placebo_tau) >= abs(`mult_tau')
    local n_extreme = r(N)
    qui count
    local n_total = r(N)
    local pval = `n_extreme' / `n_total'

    dis "  Multnomah tau = " %6.3f `mult_tau' " | placebo p = " %5.3f `pval' ///
        " (`n_extreme'/`n_total')"

    ** Plot histogram + vertical line at Multnomah's tau
    local note_str "Multnomah {&tau}-hat = `: di %5.2f `mult_tau'' pp; placebo p = `: di %4.3f `pval''"

    twoway ///
        (histogram placebo_tau, bin(40) fcolor("`col_placebo'%50") lcolor("`col_placebo'")), ///
        xline(`mult_tau', lc("`col_mult'") lwidth(thick)) ///
        xtitle("Placebo treatment effect (percentage points)", size(small)) ///
        ytitle("Density", size(small)) ///
        title("`lab'", size(small)) ///
        note("`note_str'", size(vsmall)) ///
        legend(off) ///
        graphregion(color(white)) plotregion(color(white)) ///
        name(placebo_`s', replace)

    graph export "${results}sdid/placebo_dist/fig_placebo_`sd'.pdf", replace
    graph export "${results}sdid/placebo_dist/fig_placebo_`sd'.jpg", as(jpg) quality(100) replace

    ** Compute placebo SD (sd of donor-permutation taus, comparable to
    ** the sdid package's vce(placebo) SE)
    qui summ placebo_tau
    local placebo_sd = r(sd)

    ** Persist placebo distribution + metadata for downstream use
    qui gen str40  sample_data = "`sd'"
    qui gen double mult_tau    = `mult_tau'
    qui gen double placebo_sd  = `placebo_sd'
    qui gen double placebo_p   = `pval'
    qui save "${results}sdid/placebo_dist/temp_`sd'.dta", replace
    restore
}

** ----------------------------------------------------------------------------
** Combined 2x2 figure for the appendix
** ----------------------------------------------------------------------------
capture graph combine placebo_1 placebo_2 placebo_3 placebo_4, ///
    cols(2) imargin(2 2 2 2) graphregion(color(white))
if _rc == 0 {
    graph export "${results}sdid/placebo_dist/fig_placebo_combined.pdf", replace
    graph export "${results}sdid/placebo_dist/fig_placebo_combined.jpg", as(jpg) quality(100) replace
    if ${overleaf} == 1 {
        graph export "${ol_fig}fig_placebo_combined.pdf", replace
    }
    dis _n "Wrote: ${results}sdid/placebo_dist/fig_placebo_combined.pdf"
}
else {
    dis as error "graph combine failed — check that all 4 spec panels rendered"
}

dis _n "{hline 70}"
dis "02_placebo_distribution.do complete"
dis "{hline 70}"

capture log close log_02_placebo
