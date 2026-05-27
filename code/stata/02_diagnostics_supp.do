/*****************************************************************************
* File:        02_diagnostics_supp.do
* Purpose:     Supplemental observation counts for Other-Outcome SDID analysis
* Called by:   00_multnomah.do (optional, after 02_otherout_sdid has run)
* Outputs:     ${results}tables/diagnostics_obs_counts_supp.tex
*              ${results}tables/diagnostics_obs_counts_supp.xlsx
*              ${results}tables/diagnostics_obs_counts_supp.csv
*
* Note:        Companion to 02_diagnostics.do. Kept separate because it runs
*              after 02_otherout_sdid.do has produced its working datasets.
******************************************************************************/

** Load shared project defaults and helper programs
if "${code}" == "" {
    local _cwd = subinstr("`c(pwd)'", "\", "/", .)
    if regexm("`_cwd'", "(.*)/code/stata$") global code "`_cwd'/"
    else global code "`_cwd'/code/stata/"
}
do "${code}00_stata_config.do"

capture log close log_diag_supp
log using "${logs}02_log_diagnostics_supp_${pr_name}_${date}", replace text name(log_diag_supp)
project_set_seed, context("02_diagnostics_supp.do") offset(120)

** Initialize results dataset. Schema mirrors 02_diagnostics.do so the two
** diagnostics fragments share a column layout. Other-Outcome SDID is
** county-level, so n_orig / n_dest stay missing (printed as "--"), exactly like
** the SDID rows in the main table.
clear
tempfile results
gen str44 approach    = ""
gen str44 sample      = ""
gen str40 data_source = ""
gen str16 unit        = ""
gen long  n_counties  = .
gen long  n_orig      = .
gen long  n_dest      = .
gen int   n_years     = .
gen long  n_obs       = .
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
merge m:1 year fips using "${data}working/bea_economics", gen(econ_merge)
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
    gen str44 approach    = "Other-Outcome SDID"
    gen str44 sample      = "`lbl'"
    gen str40 data_source = "IRS (county-level)"
    gen str16 unit        = "county-year"
    gen long  n_counties  = `n_units'
    gen long  n_orig      = .
    gen long  n_dest      = .
    gen int   n_years     = `n_years'
    gen long  n_obs       = `n_obs'
    append using `results'
    save `results', replace
    restore

} // END OUTCOME LOOP

clear


********************************************************************************
** EXPORT RESULTS
********************************************************************************

use `results', clear

** Drop empty seed row
drop if approach == ""

** Order to match the main diagnostics table, then sort
order approach sample data_source unit n_counties n_orig n_dest n_years n_obs
sort approach sample data_source

** Display
dis _n "==== Supplemental county-count / observation audit ===="
list approach n_counties n_orig n_dest n_years n_obs, sep(0) noobs

** Ensure output directory exists
capture mkdir "${results}"
capture mkdir "${results}tables"

** Export to CSV
export delimited using "${results}tables/diagnostics_obs_counts_supp.csv", replace

** Export to Excel
export excel using "${results}tables/diagnostics_obs_counts_supp.xlsx", ///
    firstrow(variables) replace

** Export to LaTeX (tabular fragment — wrapper in main.tex)
tempname fh
file open `fh' using "${results}tables/diagnostics_obs_counts_supp.tex", write replace

file write `fh' "\begin{tabular}{lllrrrrr}" _n
file write `fh' "\toprule" _n
file write `fh' "Approach & Sample & Data & Counties & Origin & Dest. & Years & Obs. \\" _n
file write `fh' "\midrule" _n

local N = _N
forvalues i = 1/`N' {

    local a  = approach[`i']
    local s  = sample[`i']
    local d  = data_source[`i']
    local nc = n_counties[`i']
    local no = n_orig[`i']
    local nd = n_dest[`i']
    local ny = n_years[`i']
    local no_obs = n_obs[`i']
    local nc_t = cond(missing(`nc'), "--", strofreal(`nc', "%9.0fc"))
    local no_t = cond(missing(`no'), "--", strofreal(`no', "%9.0fc"))
    local nd_t = cond(missing(`nd'), "--", strofreal(`nd', "%9.0fc"))
    file write `fh' "`a' & `s' & `d' & `nc_t' & `no_t' & `nd_t' & "
    file write `fh' %4.0f (`ny') " & " %12.0fc (`no_obs') " \\" _n
}

file write `fh' "\bottomrule" _n
file write `fh' "\end{tabular}" _n

file close `fh'

** Overleaf copy
if ${overleaf} == 1 {
    copy "${results}tables/diagnostics_obs_counts_supp.tex" ///
        "${ol_tab}diagnostics_obs_counts_supp.tex", replace
}

dis _n "Supplemental diagnostics table saved to:"
dis "  ${results}tables/diagnostics_obs_counts_supp.csv"
dis "  ${results}tables/diagnostics_obs_counts_supp.xlsx"
dis "  ${results}tables/diagnostics_obs_counts_supp.tex"

clear
log close log_diag_supp
