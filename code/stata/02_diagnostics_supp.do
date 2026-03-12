/*****************************************************************************
* File:        02_diagnostics_supp.do
* Purpose:     Supplemental observation counts for Other-Outcome SDID and
*              Quarterly SDID analyses
* Called by:   00_multnomah.do (optional, after 02_otherout_sdid and
*              02_quarterly_sdid have run)
* Outputs:     ${results}tables/diagnostics_obs_counts_supp.tex
*              ${results}tables/diagnostics_obs_counts_supp.xlsx
*              ${results}tables/diagnostics_obs_counts_supp.csv
*
* Note:        Companion to 02_diagnostics.do. Separated because these
*              sections depend on data prep that runs later in the pipeline.
******************************************************************************/

capture log close log_diag_supp
log using "${logs}02_log_diagnostics_supp_${pr_name}_${date}", replace text name(log_diag_supp)

** Initialize results dataset
clear
tempfile results
gen str40 approach = ""
gen str40 sample = ""
gen str40 data_source = ""
gen str20 unit = ""
gen long N_units = .
gen int N_years = .
gen long N_obs = .
save `results'


********************************************************************************
** SECTION 1: Other-Outcome SDID
********************************************************************************

use "${data}working/irs_county_all", clear

** Collapse across AGI brackets to county-year
collapse (sum) n1 mars1 mars2 mars4 n2 elderly agi n_total_inc a_total_inc n_wage a_wage, ///
    by(fips year state_fips state_abb county_fips county_name)

keep if inrange(year, ${start_year_irs_analysis}, 2022)
drop if county_fips == 0

** Merge demographics
merge m:1 state_fips county_fips using "${data}working/demographics_2020", gen(demo_merge)
keep if demo_merge == 3
drop demo_merge

** Merge BEA economics
merge 1:1 state_fips county_fips year using "${data}working/bea_economics", gen(econ_merge)
keep if econ_merge == 3
drop econ_merge

** Balanced panel
bysort fips: gen ct = _N
qui summ ct
keep if ct == `r(max)'
drop ct

** Log outcomes (drop if any are missing/zero)
gen ln_n1 = ln(n1)
gen ln_agi = ln(agi) if agi > 0
gen ln_total_inc = ln(a_total_inc) if a_total_inc > 0
gen ln_wage = ln(a_wage) if a_wage > 0

** Count for each outcome
foreach out in "ln_n1" "ln_agi" "ln_total_inc" "ln_wage" {

    preserve
    drop if missing(`out')

    qui distinct fips
    local n_units = r(ndistinct)
    qui distinct year
    local n_years = r(ndistinct)
    local n_obs = _N

    if "`out'" == "ln_n1" local lbl "Returns"
    if "`out'" == "ln_agi" local lbl "AGI"
    if "`out'" == "ln_total_inc" local lbl "Total Income"
    if "`out'" == "ln_wage" local lbl "Wages"

    clear
    set obs 1
    gen str40 approach = "Other-Outcome SDID"
    gen str40 sample = "`lbl'"
    gen str40 data_source = "IRS (county-level)"
    gen str20 unit = "county-year"
    gen long N_units = `n_units'
    gen int N_years = `n_years'
    gen long N_obs = `n_obs'
    append using `results'
    save `results', replace
    restore

} // END OUTCOME LOOP

clear


********************************************************************************
** SECTION 2: Quarterly SDID (QCEW + QWI)
********************************************************************************

** ---- QCEW (establishments, wages) ----
capture confirm file "${data}working/qcew_county_quarterly.dta"
if !_rc {

    use "${data}working/qcew_county_quarterly", clear

    drop if county_fips == 0

    ** Balanced panel
    bysort fips: gen ct = _N
    qui summ ct
    keep if ct == `r(max)'
    drop ct

    preserve
        qui distinct fips
        local n_units = r(ndistinct)
        qui distinct year_quarter
        local n_periods = r(ndistinct)
        local n_obs = _N

        clear
        set obs 1
        gen str40 approach = "Quarterly SDID"
        gen str40 sample = "Establishments + Wages"
        gen str40 data_source = "QCEW"
        gen str20 unit = "county-quarter"
        gen long N_units = `n_units'
        gen int N_years = `n_periods'
        gen long N_obs = `n_obs'
        append using `results'
        save `results', replace
    restore

    clear
}
else {
    dis as txt "  Note: QCEW data not found, skipping QCEW diagnostics."
}

** ---- QWI (employment, earnings) ----
capture confirm file "${data}working/qwi_county_quarterly.dta"
if !_rc {

    use "${data}working/qwi_county_quarterly", clear

    drop if county_fips == 0

    ** Balanced panel
    bysort fips: gen ct = _N
    qui summ ct
    keep if ct == `r(max)'
    drop ct

    preserve
        qui distinct fips
        local n_units = r(ndistinct)
        qui distinct year_quarter
        local n_periods = r(ndistinct)
        local n_obs = _N

        clear
        set obs 1
        gen str40 approach = "Quarterly SDID"
        gen str40 sample = "Employment + Earnings"
        gen str40 data_source = "QWI"
        gen str20 unit = "county-quarter"
        gen long N_units = `n_units'
        gen int N_years = `n_periods'
        gen long N_obs = `n_obs'
        append using `results'
        save `results', replace
    restore

    clear
}
else {
    dis as txt "  Note: QWI data not found, skipping QWI diagnostics."
}


********************************************************************************
** EXPORT RESULTS
********************************************************************************

use `results', clear

** Drop empty seed row
drop if approach == ""

** Sort
sort approach sample data_source

** Display
list, sep(0) noobs

** Ensure output directory exists
capture mkdir "${results}"
capture mkdir "${results}tables"

** Export to CSV
export delimited using "${results}tables/diagnostics_obs_counts_supp.csv", replace

** Export to Excel
export excel using "${results}tables/diagnostics_obs_counts_supp.xlsx", ///
    firstrow(variables) replace

** Export to LaTeX
tempname fh
file open `fh' using "${results}tables/diagnostics_obs_counts_supp.tex", write replace

file write `fh' "\begin{table}[htbp]" _n
file write `fh' "\centering" _n
file write `fh' "\caption{Observation Counts: Supplemental Analyses}" _n
file write `fh' "\label{tab:diagnostics_supp}" _n
file write `fh' "\begin{tabular}{llllrrr}" _n
file write `fh' "\toprule" _n
file write `fh' "Approach & Sample & Data & Unit & Units & Periods & Obs. \\" _n
file write `fh' "\midrule" _n

local N = _N
forvalues i = 1/`N' {

    local a = approach[`i']
    local s = sample[`i']
    local d = data_source[`i']
    local u = unit[`i']
    local nu = N_units[`i']
    local ny = N_years[`i']
    local no = N_obs[`i']

    file write `fh' "`a' & `s' & `d' & `u' & "
    file write `fh' %~12.0fc (`nu') " & " %~4.0f (`ny') " & " %~12.0fc (`no') " \\" _n
}

file write `fh' "\bottomrule" _n
file write `fh' "\end{tabular}" _n
file write `fh' "\end{table}" _n

file close `fh'

dis _n "Supplemental diagnostics table saved to:"
dis "  ${results}tables/diagnostics_obs_counts_supp.csv"
dis "  ${results}tables/diagnostics_obs_counts_supp.xlsx"
dis "  ${results}tables/diagnostics_obs_counts_supp.tex"

clear
log close log_diag_supp
