/*****************************************************************************
* File:        02_diagnostics.do
* Purpose:     Observation count table by approach x specification x data source
*              Covers: SDID (main), Narrow SDID, Flows, DiD
* Called by:   00_multnomah.do (optional) or standalone after data cleaning
* Outputs:     ${results}tables/diagnostics_obs_counts.tex
*              ${results}tables/diagnostics_obs_counts.xlsx
*              ${results}tables/diagnostics_obs_counts.csv
*
* Note:        Replicates the sample-construction logic from each analysis
*              do-file without running any regressions. Counts only.
*              For Other-Outcome SDID and Quarterly SDID counts, see
*              02_diagnostics_supp.do (runs after those scripts).
******************************************************************************/

capture log close log_diag
log using "${logs}02_log_diagnostics_${pr_name}_${date}", replace text name(log_diag)

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
** SECTION 1: SDID (Main — All Counties)
********************************************************************************

** Load IRS county gross data
use "${data}working/irs_county_gross", clear

** Replicate sample construction from 02_sdid_analysis.do
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

** Drop counties with no base population
drop if (missing(n1_out_1) | n1_out_1 == 0) & year <= 2022

** Balanced panel requirement
bysort state_fips county_fips: gen ct = _N
keep if ct >= 7
drop ct

** Make FIPS for identification
make_fips state_fips county_fips, gen(fips)

** Count: IRS, all counties, 2016-2022
preserve
    qui distinct fips
    local n_units = r(ndistinct)
    qui distinct year
    local n_years = r(ndistinct)
    local n_obs = _N
    clear
    set obs 1
    gen str40 approach = "SDID"
    gen str40 sample = "All counties"
    gen str40 data_source = "IRS"
    gen str20 unit = "county-year"
    gen long N_units = `n_units'
    gen int N_years = `n_years'
    gen long N_obs = `n_obs'
    append using `results'
    save `results', replace
restore

** Urban sample (top 5% urbanization)
preserve
    qui summ percent_urban, det
    keep if percent_urban >= `r(p95)'
    bysort fips: gen ct2 = _N
    keep if ct2 >= 7
    drop ct2
    qui distinct fips
    local n_units = r(ndistinct)
    qui distinct year
    local n_years = r(ndistinct)
    local n_obs = _N
    clear
    set obs 1
    gen str40 approach = "SDID"
    gen str40 sample = "Urban (p95)"
    gen str40 data_source = "IRS"
    gen str20 unit = "county-year"
    gen long N_units = `n_units'
    gen int N_years = `n_years'
    gen long N_obs = `n_obs'
    append using `results'
    save `results', replace
restore

clear

** ---- SDID with ACS data ----

** Load ACS gross migration
use "${data}working/acs_county_gross_25plus", clear
keep if inrange(year, ${start_year_acs}, ${end_year_acs})
drop if county_fips == 0

** Balanced panel
bysort fips: gen ct = _N
qui summ ct
keep if ct == `r(max)'
drop ct

** Count: ACS, 2012-2024
preserve
    qui distinct fips
    local n_units = r(ndistinct)
    qui distinct year
    local n_years = r(ndistinct)
    local n_obs = _N
    clear
    set obs 1
    gen str40 approach = "SDID"
    gen str40 sample = "All counties"
    gen str40 data_source = "ACS"
    gen str20 unit = "county-year"
    gen long N_units = `n_units'
    gen int N_years = `n_years'
    gen long N_obs = `n_obs'
    append using `results'
    save `results', replace
restore

clear

********************************************************************************
** SECTION 2: Narrow SDID (22-county pool)
********************************************************************************

use "${data}working/irs_county_gross", clear

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

** Make FIPS
make_fips state_fips county_fips, gen(fips)

** Define the 22-county narrow pool
gen sample_narrow = 0
** Multnomah, OR
replace sample_narrow = 1 if fips == 41051
** Columbus, OH (Franklin County)
replace sample_narrow = 1 if fips == 39049
** Minneapolis, MN (Hennepin County)
replace sample_narrow = 1 if fips == 27053
** Philadelphia, PA
replace sample_narrow = 1 if fips == 42101
** Austin, TX (Travis County)
replace sample_narrow = 1 if fips == 48453
** Orlando, FL (Orange County)
replace sample_narrow = 1 if fips == 12095
** Tampa, FL (Hillsborough County)
replace sample_narrow = 1 if fips == 12057
** Salt Lake City, UT (Salt Lake County)
replace sample_narrow = 1 if fips == 49035
** Detroit, MI (Wayne County)
replace sample_narrow = 1 if fips == 26163
** Vancouver, WA (Clark County)
replace sample_narrow = 1 if fips == 53011
** Seattle, WA (King County)
replace sample_narrow = 1 if fips == 53033
** Baltimore, MD (Baltimore City)
replace sample_narrow = 1 if fips == 24510
** Milwaukee, WI
replace sample_narrow = 1 if fips == 55079
** St. Louis, MO (St. Louis City)
replace sample_narrow = 1 if fips == 29510
** Denver, CO
replace sample_narrow = 1 if fips == 08031
** Kansas City, MO (Jackson County)
replace sample_narrow = 1 if fips == 29095
** Indianapolis, IN (Marion County)
replace sample_narrow = 1 if fips == 18097
** Atlanta, GA (Fulton County)
replace sample_narrow = 1 if fips == 13121
** Las Vegas, NV (Clark County)
replace sample_narrow = 1 if fips == 32003
** Sacramento, CA
replace sample_narrow = 1 if fips == 06067
** San Antonio, TX (Bexar County)
replace sample_narrow = 1 if fips == 48029
** Boston, MA (Suffolk County)
replace sample_narrow = 1 if fips == 25025

keep if sample_narrow == 1

** Balanced panel
bysort fips: gen ct = _N
keep if ct >= 7
drop ct

** Count
qui distinct fips
local n_units = r(ndistinct)
qui distinct year
local n_years = r(ndistinct)
local n_obs = _N

clear
set obs 1
gen str40 approach = "Narrow SDID"
gen str40 sample = "22-county pool"
gen str40 data_source = "IRS"
gen str20 unit = "county-year"
gen long N_units = `n_units'
gen int N_years = `n_years'
gen long N_obs = `n_obs'
append using `results'
save `results', replace
clear


********************************************************************************
** SECTION 3: Flow Analysis
********************************************************************************

use "${data}working/irs_county_flow", clear

** Year restriction
keep if inrange(year, ${start_year_irs_analysis}, 2022)

** Drop Alaska and Hawaii flows
drop if inlist(state_fips_o, 2, 15)
drop if inlist(state_fips_d, 2, 15)

** Count: All flows
preserve
    egen flow_id = group(fips_d fips_o)
    qui distinct flow_id
    local n_units = r(ndistinct)
    qui distinct year
    local n_years = r(ndistinct)
    local n_obs = _N

    clear
    set obs 1
    gen str40 approach = "Flows"
    gen str40 sample = "All"
    gen str40 data_source = "IRS"
    gen str20 unit = "flow-year"
    gen long N_units = `n_units'
    gen int N_years = `n_years'
    gen long N_obs = `n_obs'
    append using `results'
    save `results', replace
restore

clear


********************************************************************************
** SECTION 4: DiD (Individual-Level ACS)
********************************************************************************

use "${data}working/acs_migration_file", clear

** Replicate sample construction from 02_did_analysis.do
drop if year == 2015
drop if year == 2020
keep if age >= 25
capture confirm variable qmigplc1
if !_rc drop if qmigplc1 == 4
drop if inlist(state_fips_o, 2, 15)
drop if inlist(state_fips_d, 2, 15)
capture confirm variable ftotinc
if !_rc drop if ftotinc < 0

** Define treatment variables
gen multnomah_o = (state_fips_o == 41 & county_fips_o == 51)
gen multnomah_d = (state_fips_d == 41 & county_fips_d == 51)

** ---- Sample 1: Out-migration from Multnomah ----
preserve
    keep if multnomah_o == 1

    local n_obs = _N
    qui distinct year
    local n_years = r(ndistinct)

    clear
    set obs 1
    gen str40 approach = "DiD"
    gen str40 sample = "Out-migration (Multnomah)"
    gen str40 data_source = "ACS (individual)"
    gen str20 unit = "person-year"
    gen long N_units = `n_obs'
    gen int N_years = `n_years'
    gen long N_obs = `n_obs'
    append using `results'
    save `results', replace
restore

** ---- Sample 2: In-migration, West Coast ----
preserve
    keep if multnomah_o != 1
    keep if inlist(state_fips_o, 6, 41, 53)

    local n_obs = _N
    qui distinct year
    local n_years = r(ndistinct)

    clear
    set obs 1
    gen str40 approach = "DiD"
    gen str40 sample = "In-migration (West Coast)"
    gen str40 data_source = "ACS (individual)"
    gen str20 unit = "person-year"
    gen long N_units = `n_obs'
    gen int N_years = `n_years'
    gen long N_obs = `n_obs'
    append using `results'
    save `results', replace
restore

** ---- Sample 3: In-migration, Lower 48 + DC ----
preserve
    keep if multnomah_o != 1

    local n_obs = _N
    qui distinct year
    local n_years = r(ndistinct)

    clear
    set obs 1
    gen str40 approach = "DiD"
    gen str40 sample = "In-migration (Lower 48)"
    gen str40 data_source = "ACS (individual)"
    gen str20 unit = "person-year"
    gen long N_units = `n_obs'
    gen int N_years = `n_years'
    gen long N_obs = `n_obs'
    append using `results'
    save `results', replace
restore

clear


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
export delimited using "${results}tables/diagnostics_obs_counts.csv", replace

** Export to Excel
export excel using "${results}tables/diagnostics_obs_counts.xlsx", ///
    firstrow(variables) replace

** Export to LaTeX
** Build a .tex file manually for full control of formatting
tempname fh
file open `fh' using "${results}tables/diagnostics_obs_counts.tex", write replace

file write `fh' "\begin{table}[htbp]" _n
file write `fh' "\centering" _n
file write `fh' "\caption{Observation Counts by Approach and Sample}" _n
file write `fh' "\label{tab:diagnostics}" _n
file write `fh' "\begin{tabular}{llllrrr}" _n
file write `fh' "\toprule" _n
file write `fh' "Approach & Sample & Data & Unit & Units & Years & Obs. \\" _n
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

dis _n "Diagnostics table saved to:"
dis "  ${results}tables/diagnostics_obs_counts.csv"
dis "  ${results}tables/diagnostics_obs_counts.xlsx"
dis "  ${results}tables/diagnostics_obs_counts.tex"

clear
log close log_diag
