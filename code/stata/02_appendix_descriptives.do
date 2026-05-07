/*******************************************************************************
File Name:      02_appendix_descriptives.do
Creator:        John Iselin (with Claude)
Date Created:   2026-05-07
Last Modified:  2026-05-07 (post-review cleanup: collapsed Section C duplication; renamed M_ACS->M_ACS_SAMP)

Purpose:        Build the three method-specific descriptive-statistics tables
                that replace the old single Appendix Table A1 (item 11 of the
                May 2026 paper revision TODO):

                  (A) SDID descriptives — Multnomah + 5 donor pools, both
                      IRS and ACS College, with overall (time-pooled)
                      county-level and out-of-state migration means and
                      county counts.

                  (B) IRS Flow descriptives — Multnomah-touching vs.
                      non-Multnomah flows for All / ACS-restricted samples,
                      reporting # observed flows, share with zero movers,
                      and mean flow size in n1, n2, AGI.

                  (C) ACS descriptives — out-migration and in-migration
                      samples, reporting county-year observations,
                      weighted person counts, and aggregate migration
                      rates by direction.

                These are appendix-only tables; they do not replace the new
                Table 1 produced by 02_descriptives_supp.do. Each table is
                emitted as a separate .tex file so updated.tex can \input
                them independently.

Inputs:         ${data}working/sdid_analysis_data.dta            (Table A)
                ${data}working/irs_county_flow.dta               (Table B)
                ${data}working/acs_county_gross_25plus.dta       (Table B ACS-county list; Table C)

Outputs:        ${results}tables/tableA1_sdid.tex
                ${results}tables/tableA1_irs_flow.tex
                ${results}tables/tableA1_acs.tex
                Same files copied to ${ol_tab} when ${overleaf}=1
*******************************************************************************/

** Load shared project defaults (path globals + Overleaf sync)
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

if "${ol_tab}" == "" {
    capture do "${dir}/profile.do"
    if "${oth_path}" != "" {
        global ol_fig "${oth_path}figures/"
        global ol_tab "${oth_path}tables/"
        global overleaf = 1
    }
}

capture log close log_02_appx
log using "${logs}02_log_appendix_descriptives_${date}", replace text name(log_02_appx)
capture mkdir "${results}tables"

dis ""
dis "=============================================="
dis "Appendix descriptives: 3 method-specific tables"
dis "=============================================="


********************************************************************************
** TABLE A1.A: SDID descriptives
********************************************************************************
**
** Two panels (IRS, ACS College) × 6 rows (Multnomah + 5 pools) × 7 numeric
** columns: N counties, county-level out / in / net (time-pooled means),
** out-of-state out / in / net (time-pooled means).
**
** Time-pooling: average over all in-sample years 2018-2022 (IRS) or
** 2018-2024 (ACS), excluding 2020. This gives an "overall" rate for each
** (pool × direction × scope) combo, where scope is county-level or
** out-of-state.

dis ""
dis "--- Table A1.A: SDID descriptives ---"

use "${data}working/sdid_analysis_data.dta", clear

capture confirm variable multnomah
if _rc {
    gen byte multnomah = (state_fips == 41 & county_fips == 51)
}

local pool_list "mult sample_all sample_urban95 sample_urban75_covid sample_demog sample_stringency"
local pool_label_mult                  "Multnomah"
local pool_label_sample_all            "All donor counties"
local pool_label_sample_urban95        "Urban top-5\%"
local pool_label_sample_urban75_covid  "Urban top-25\%, Covid match"
local pool_label_sample_demog          "Demographic match"
local pool_label_sample_stringency     "Stringency match"

local cond_mult                  "multnomah == 1"
local cond_sample_all            "sample_all == 1"
local cond_sample_urban95        "sample_urban95 == 1"
local cond_sample_urban75_covid  "sample_urban75_covid == 1"
local cond_sample_demog          "sample_demog == 1"
local cond_sample_stringency     "sample_stringency == 1"

** Build SDID descriptive matrices: 6 rows × 7 cols (N + 6 rate means)
tempname M_SDID_IRS M_SDID_ACS
matrix `M_SDID_IRS' = J(6, 7, .)
matrix `M_SDID_ACS' = J(6, 7, .)
matrix colnames `M_SDID_IRS' = N out_cty in_cty net_cty out_st in_st net_st
matrix colnames `M_SDID_ACS' = N out_cty in_cty net_cty out_st in_st net_st

** N counties from a single year snapshot
preserve
keep if year == 2019
local row = 1
foreach pool of local pool_list {
    qui count if `cond_`pool''
    matrix `M_SDID_IRS'[`row', 1] = r(N)
    matrix `M_SDID_ACS'[`row', 1] = r(N)
    local ++row
}
restore

** ---- IRS panel: time-pooled means 2018-2022 (excluding 2020) ----
preserve
keep if inrange(year, 2018, 2022) & year != 2020
local row = 1
foreach pool of local pool_list {
    local i = 0
    foreach scope in "irs" "irs_outstate" {
        foreach dir in "out" "in" "net" {
            local ++i
            local col = `i' + 1
            qui summ agi_`dir'_rate_`scope' if `cond_`pool''
            matrix `M_SDID_IRS'[`row', `col'] = r(mean)
        }
    }
    local ++row
}
restore

** ---- ACS College panel: time-pooled means 2018-2024 (excluding 2020) ----
preserve
keep if inrange(year, 2018, 2024) & year != 2020
local row = 1
foreach pool of local pool_list {
    local i = 0
    foreach scope in "acs2" "acs2_outstate" {
        foreach dir in "out" "in" "net" {
            local ++i
            local col = `i' + 1
            qui summ agi_`dir'_rate_`scope' if `cond_`pool''
            matrix `M_SDID_ACS'[`row', `col'] = r(mean)
        }
    }
    local ++row
}
restore

mat list `M_SDID_IRS'
mat list `M_SDID_ACS'

** Write tex
local _dests `""${results}tables/tableA1_sdid.tex""'
if ${overleaf} == 1 {
    local _dests `"`_dests' "${ol_tab}tableA1_sdid.tex""'
}

foreach _outfile of local _dests {

tempname fh
file open `fh' using "`_outfile'", write replace

file write `fh' "% Appendix Table A1.A: SDID descriptives" _n
file write `fh' "% Generated by 02_appendix_descriptives.do (item 11)" _n
file write `fh' `"\begin{table}[htbp]"' _n
file write `fh' `"\centering"' _n
file write `fh' `"\begin{threeparttable}"' _n
file write `fh' `"\footnotesize"' _n
file write `fh' `"\caption{SDID Sample: AGI Migration Rates by Comparison Group, Time-Pooled}"' _n
file write `fh' `"\label{tab:tableA1_sdid}"' _n
file write `fh' `"\setlength{\tabcolsep}{4pt}"' _n
file write `fh' `"\begin{tabular}{l r c c c c c c}"' _n
file write `fh' `"\toprule"' _n
file write `fh' `" & N & \multicolumn{3}{c}{County-level migration} & \multicolumn{3}{c}{Out-of-state migration} \\"' _n
file write `fh' `"\cmidrule(lr){3-5} \cmidrule(lr){6-8}"' _n
file write `fh' `" & counties & Out & In & Net & Out & In & Net \\"' _n
file write `fh' `"\midrule"' _n

** Helper: write one panel's 6 rows
foreach panel in IRS ACS {
    if "`panel'" == "IRS" {
        file write `fh' `"\multicolumn{8}{l}{\textit{Panel A: IRS (2018-2022, excl.\ 2020)}} \\"' _n
        local matname "`M_SDID_IRS'"
    }
    else {
        file write `fh' `"\midrule"' _n
        file write `fh' `"\addlinespace[0.4em]"' _n
        file write `fh' `"\multicolumn{8}{l}{\textit{Panel B: ACS College (2018-2024, excl.\ 2020)}} \\"' _n
        local matname "`M_SDID_ACS'"
    }
    file write `fh' `"\addlinespace"' _n
    forvalues r = 1/6 {
        local pool : word `r' of `pool_list'
        local lab  "`pool_label_`pool''"
        local nC : di %12.0fc `matname'[`r', 1]
        local nC = strtrim("`nC'")
        local cells ""
        forvalues c = 2/7 {
            local v : di %5.2f `matname'[`r', `c']
            local v = strtrim("`v'")
            local cells "`cells' & `v'"
        }
        file write `fh' `"`lab' & `nC'`cells' \\"' _n
        if `r' == 1 file write `fh' `"\addlinespace"' _n
    }
}

file write `fh' `"\bottomrule"' _n
file write `fh' `"\end{tabular}"' _n
file write `fh' `"\begin{tablenotes}[flushleft]"' _n
file write `fh' `"\small"' _n
file write `fh' `"\item \textit{Notes:} Time-pooled means of AGI migration rates (\% of base population) within each comparison group. Each rate is averaged over the in-sample years (2020 excluded). County-level migration counts moves to / from any other county; out-of-state migration restricts to moves crossing the Oregon state line. Means are simple county-level means (each county weighted equally), matching the SDID donor-pool construction."' _n
file write `fh' `"\item Source: IRS SOI county-to-county migration flows (Panel~A); ACS microdata, college-educated subsample (Panel~B). See Appendix~B for donor-pool construction."' _n
file write `fh' `"\end{tablenotes}"' _n
file write `fh' `"\end{threeparttable}"' _n
file write `fh' `"\end{table}"' _n
file close `fh'
dis "Wrote `_outfile'"
}


********************************************************************************
** TABLE A1.B: IRS Flow descriptives
********************************************************************************
**
** Two panels (All / ACS-restricted) × 2 rows (Multnomah-touching / Non-Multnomah)
** × 5 numeric columns: # observed flows, share with zero movers, mean n1,
** mean n2, mean AGI.

dis ""
dis "--- Table A1.B: IRS Flow descriptives ---"

** Build the ACS county list (~389 counties identified in public-use ACS).
** acs_county_sample.dta is a small reference (42 rows) — not the 389-county
** universe. The right source is the unique set of fips in
** acs_county_gross_25plus.dta, which is the county-year panel of all
** ACS-identified counties.
preserve
capture confirm file "${data}working/acs_county_gross_25plus.dta"
if _rc == 0 {
    use "${data}working/acs_county_gross_25plus.dta", clear
    keep fips
    gen byte acs_county = 1
    duplicates drop fips, force
    tempfile acs_counties
    save `acs_counties'
    local have_acs = 1
    qui count
    dis "ACS county list: " r(N) " counties from acs_county_gross_25plus.dta"
}
else {
    local have_acs = 0
    dis as error "Warning: acs_county_gross_25plus.dta not found -- Panel B will report N=0 in the ACS-restricted rows."
}
restore

** Load IRS flows; restrict to analysis years (2016-2022 by convention,
** matching the SDID main panel).
use "${data}working/irs_county_flow.dta", clear
keep if inrange(year, 2016, 2022)

** Drop suppressed totals (county_fips == 0 means state-aggregate row)
drop if county_fips_o == 0 | county_fips_d == 0

** Tag Multnomah-touching flows: origin OR destination = Multnomah (41051)
gen byte mult_flow = (state_fips_o == 41 & county_fips_o == 51) | ///
                     (state_fips_d == 41 & county_fips_d == 51)

** Tag ACS-restricted: BOTH origin and destination in ACS-389 sample.
** Build a left-join lookup on each endpoint by renaming the join key
** to match the data side (Stata's `merge ... using` requires matching
** key names; we can't rename in-flight via the merge command).
gen byte acs_o = 0
gen byte acs_d = 0
if `have_acs' == 1 {
    preserve
    use `acs_counties', clear
    rename fips fips_o
    rename acs_county acs_o
    tempfile a_o
    save `a_o'
    use `acs_counties', clear
    rename fips fips_d
    rename acs_county acs_d
    tempfile a_d
    save `a_d'
    restore

    drop acs_o acs_d
    merge m:1 fips_o using `a_o', keep(master match) nogen
    merge m:1 fips_d using `a_d', keep(master match) nogen
    replace acs_o = 0 if missing(acs_o)
    replace acs_d = 0 if missing(acs_d)
}
gen byte acs_flow = (acs_o == 1 & acs_d == 1)

** ---- Compute stats ----
** Note on "share with 0 movers": IRS SOI suppresses low-count flows
** (cells with fewer than ~20 movers are dropped from the public release),
** so the data does not include explicit zero-mover rows. The share of
** zero-mover county-pairs is therefore not directly observable from the
** released data. We report median n1 instead (a more interpretable
** summary that captures the bulk of the observed flow distribution).

tempname M_FLOW
matrix `M_FLOW' = J(4, 5, .)
matrix colnames `M_FLOW' = Nflows median_n1 mean_n1 mean_n2 mean_agi

** Helper local for the two condition chains (same logic across rows)
forvalues r = 1/4 {
    local mflow = cond(mod(`r', 2) == 1, 1, 0)        // odd → Multnomah, even → Non-Multnomah
    local acsf  = cond(`r' <= 2, 0, 1)                 // 1-2 → All, 3-4 → ACS
    if `acsf' == 0 local cond "mult_flow == `mflow'"
    else           local cond "mult_flow == `mflow' & acs_flow == 1"

    qui count if `cond'
    matrix `M_FLOW'[`r', 1] = r(N)
    qui summ n1 if `cond', de
    matrix `M_FLOW'[`r', 2] = r(p50)
    matrix `M_FLOW'[`r', 3] = r(mean)
    qui summ n2 if `cond'
    matrix `M_FLOW'[`r', 4] = r(mean)
    qui summ agi if `cond'
    matrix `M_FLOW'[`r', 5] = r(mean)
}

mat list `M_FLOW'

** Write tex
local _dests `""${results}tables/tableA1_irs_flow.tex""'
if ${overleaf} == 1 {
    local _dests `"`_dests' "${ol_tab}tableA1_irs_flow.tex""'
}
foreach _outfile of local _dests {
tempname fh
file open `fh' using "`_outfile'", write replace
file write `fh' "% Appendix Table A1.B: IRS Flow descriptives" _n
file write `fh' "% Generated by 02_appendix_descriptives.do (item 11)" _n
file write `fh' `"\begin{table}[htbp]"' _n
file write `fh' `"\centering"' _n
file write `fh' `"\begin{threeparttable}"' _n
file write `fh' `"\footnotesize"' _n
file write `fh' `"\caption{IRS County-to-County Flow Sample: Descriptive Statistics}"' _n
file write `fh' `"\label{tab:tableA1_irs_flow}"' _n
file write `fh' `"\setlength{\tabcolsep}{4pt}"' _n
file write `fh' `"\begin{tabular}{l r r r r r}"' _n
file write `fh' `"\toprule"' _n
file write `fh' `" & N flows & Median n1 & Mean n1 & Mean n2 & Mean AGI \\"' _n
file write `fh' `" & & (returns) & (returns) & (exemptions) & (USD thousands) \\"' _n
file write `fh' `"\midrule"' _n

local row_labels `""Multnomah-touching flows" "Non-Multnomah flows""'
foreach panel in All ACS {
    if "`panel'" == "All" {
        file write `fh' `"\multicolumn{6}{l}{\textit{Panel A: All flows}} \\"' _n
        local row_off 0
    }
    else {
        file write `fh' `"\midrule"' _n
        file write `fh' `"\addlinespace[0.4em]"' _n
        file write `fh' `"\multicolumn{6}{l}{\textit{Panel B: ACS-restricted (both endpoints in ACS-389 sample)}} \\"' _n
        local row_off 2
    }
    file write `fh' `"\addlinespace"' _n
    forvalues k = 1/2 {
        local r = `k' + `row_off'
        local lab : word `k' of `row_labels'
        local nf : di %14.0fc `M_FLOW'[`r', 1]
        local nf = strtrim("`nf'")
        local md : di %7.1f `M_FLOW'[`r', 2]
        local md = strtrim("`md'")
        local mn1 : di %7.1f `M_FLOW'[`r', 3]
        local mn1 = strtrim("`mn1'")
        local mn2 : di %7.1f `M_FLOW'[`r', 4]
        local mn2 = strtrim("`mn2'")
        local mag : di %9.1f `M_FLOW'[`r', 5]
        local mag = strtrim("`mag'")
        file write `fh' `"`lab' & `nf' & `md' & `mn1' & `mn2' & `mag' \\"' _n
    }
}

file write `fh' `"\bottomrule"' _n
file write `fh' `"\end{tabular}"' _n
file write `fh' `"\begin{tablenotes}[flushleft]"' _n
file write `fh' `"\small"' _n
file write `fh' `"\item \textit{Notes:} County-pair flow rows from IRS SOI 2016-2022, after dropping state-aggregate rows. A flow is Multnomah-touching if either origin or destination is Multnomah County. The ACS-restricted panel keeps only flows where both endpoints are among the county-identified counties in the public-use ACS (drawn from the unique fips set in the ACS county-year panel), matching the smaller-sample PPML specification used in the paper. Mean and median flow sizes are computed across all observed flow rows in the group; n1 is returns (households), n2 is exemptions (individuals), AGI is in thousands of dollars. The data drop county-pair-years with fewer than ~20 movers (the IRS suppression threshold), so explicit zero-mover rows do not appear."' _n
file write `fh' `"\item Source: IRS SOI county-to-county migration flows (2016-2022)."' _n
file write `fh' `"\end{tablenotes}"' _n
file write `fh' `"\end{threeparttable}"' _n
file write `fh' `"\end{table}"' _n
file close `fh'
dis "Wrote `_outfile'"
}


********************************************************************************
** TABLE A1.C: ACS descriptives
********************************************************************************
**
** Two panels (out-migration sample / in-migration sample) × 2 rows
** (Multnomah / non-Multnomah counties) × 4 numeric columns:
** N county-years, total persons (weighted), total dollars (weighted),
** total households (weighted).
**
** The acs_county_gross_25plus.dta file is already aggregated at the
** county-year level with weighted person/household/dollar counts. We
** sum the relevant flow categories to get aggregate ACS-sample sizes
** for the out- and in-migration estimating samples.
**
** Categories (as documented in the data prep):
**   _1 = non-movers (lived in same county at t-1 and t)
**   _2 = same-state movers
**   _3 = domestic movers (cross-county)
**   _4 = international movers
**   _5 = interstate movers
**
** Out-migration sample: those living in county i at t-1 (i.e., everyone
** in the county base, including movers OUT). The denominator for the
** out-migration RATE is _out_1 + _out_2 + ... totals.
**
** In-migration sample: those NOT living in county i at t-1 who could
** have moved IN. Approximated by the in-flow categories.

dis ""
dis "--- Table A1.C: ACS descriptives ---"

** Use the all-25+ panel for the rows (all-25+ matches the SDID ACS-all
** specifications; the college subsample has a parallel structure but
** is shown by the SDID descriptives table above).

use "${data}working/acs_county_gross_25plus.dta", clear
keep if inrange(year, 2016, 2024)

capture confirm variable multnomah
if _rc {
    gen byte multnomah = (state_fips == 41 & county_fips == 51)
}

** Build per-row totals and sum over county-years.
** persons_*_*: counts of individuals (weighted by perwt) by category;
** sum across categories 1..5 for the total in-county base. Movers cross
** county lines = categories 3 (domestic) + 5 (interstate).
foreach pre in out in {
    gen double persons_`pre'_base    = persons_`pre'_1 + persons_`pre'_2 + persons_`pre'_3 ///
        + persons_`pre'_4 + persons_`pre'_5
    gen double persons_`pre'_movers  = persons_`pre'_3 + persons_`pre'_5
    gen double households_`pre'_base = households_`pre'_1 + households_`pre'_2 + households_`pre'_3 ///
        + households_`pre'_4 + households_`pre'_5
    gen double dollars_`pre'_base    = dollars_`pre'_1 + dollars_`pre'_2 + dollars_`pre'_3 ///
        + dollars_`pre'_4 + dollars_`pre'_5
}

** Build the 4-row sample-size matrix.
** Rows: 1=Out/Multnomah, 2=Out/non-Multnomah, 3=In/Multnomah, 4=In/non-Multnomah.
** Cols: 1=N county-years, 2=persons, 3=households, 4=dollars, 5=migration rate (%).
** Renamed from M_ACS to avoid collision with the same name in 02_descriptives_supp.do.
tempname M_ACS_SAMP
matrix `M_ACS_SAMP' = J(4, 5, .)
matrix colnames `M_ACS_SAMP' = Nyears persons households dollars rate

local row = 0
foreach pre in out in {
    foreach mult in 1 0 {
        local ++row
        qui count if multnomah == `mult'
        matrix `M_ACS_SAMP'[`row', 1] = r(N)
        qui summ persons_`pre'_base if multnomah == `mult', meanonly
        local base_sum = r(sum)
        matrix `M_ACS_SAMP'[`row', 2] = `base_sum'
        qui summ households_`pre'_base if multnomah == `mult', meanonly
        matrix `M_ACS_SAMP'[`row', 3] = r(sum)
        qui summ dollars_`pre'_base if multnomah == `mult', meanonly
        matrix `M_ACS_SAMP'[`row', 4] = r(sum)
        qui summ persons_`pre'_movers if multnomah == `mult', meanonly
        matrix `M_ACS_SAMP'[`row', 5] = cond(`base_sum' > 0, 100 * r(sum) / `base_sum', .)
    }
}

mat list `M_ACS_SAMP'

** Write tex
local _dests `""${results}tables/tableA1_acs.tex""'
if ${overleaf} == 1 {
    local _dests `"`_dests' "${ol_tab}tableA1_acs.tex""'
}
foreach _outfile of local _dests {
tempname fh
file open `fh' using "`_outfile'", write replace
file write `fh' "% Appendix Table A1.C: ACS descriptives" _n
file write `fh' "% Generated by 02_appendix_descriptives.do (item 11)" _n
file write `fh' `"\begin{table}[htbp]"' _n
file write `fh' `"\centering"' _n
file write `fh' `"\begin{threeparttable}"' _n
file write `fh' `"\footnotesize"' _n
file write `fh' `"\caption{ACS Microdata: Out- and In-Migration Sample Descriptives}"' _n
file write `fh' `"\label{tab:tableA1_acs}"' _n
file write `fh' `"\setlength{\tabcolsep}{4pt}"' _n
file write `fh' `"\begin{tabular}{l r r r r r}"' _n
file write `fh' `"\toprule"' _n
file write `fh' `" & County- & Persons & Households & Total income & Migration \\"' _n
file write `fh' `" & years & (millions, wt.) & (millions, wt.) & (USD billions, wt.) & rate (\%) \\"' _n
file write `fh' `"\midrule"' _n

local row_labels `""Multnomah" "Non-Multnomah counties""'
foreach panel in OUT IN {
    if "`panel'" == "OUT" {
        ** Note: $t$ / $t-1$ would be eaten by Stata's macro engine
        ** (treated as global ${t}); rephrase to avoid math mode here.
        file write `fh' `"\multicolumn{6}{l}{\textit{Panel A: Out-migration sample (origin = the county in the prior year)}} \\"' _n
        local row_off 0
    }
    else {
        file write `fh' `"\midrule"' _n
        file write `fh' `"\addlinespace[0.4em]"' _n
        file write `fh' `"\multicolumn{6}{l}{\textit{Panel B: In-migration sample (destination = the county in the current year)}} \\"' _n
        local row_off 2
    }
    file write `fh' `"\addlinespace"' _n
    forvalues k = 1/2 {
        local r = `k' + `row_off'
        local lab : word `k' of `row_labels'
        local ny : di %12.0fc `M_ACS_SAMP'[`r', 1]
        local ny = strtrim("`ny'")
        ** Persons / households reported in millions, dollars in billions.
        local pp : di %12.1fc (`M_ACS_SAMP'[`r', 2] / 1e6)
        local pp = strtrim("`pp'")
        local hh : di %12.1fc (`M_ACS_SAMP'[`r', 3] / 1e6)
        local hh = strtrim("`hh'")
        local dd : di %12.1fc (`M_ACS_SAMP'[`r', 4] / 1e9)
        local dd = strtrim("`dd'")
        local rt : di %5.2f `M_ACS_SAMP'[`r', 5]
        local rt = strtrim("`rt'")
        file write `fh' `"`lab' & `ny' & `pp' & `hh' & `dd' & `rt' \\"' _n
    }
}

file write `fh' `"\bottomrule"' _n
file write `fh' `"\end{tabular}"' _n
file write `fh' `"\begin{tablenotes}[flushleft]"' _n
file write `fh' `"\small"' _n
file write `fh' `"\item \textit{Notes:} ACS sample sizes from the all-25+ subsample (the all-counties ACS specification used in the paper); the college-educated subsample has the same structure but smaller weighted counts. Persons / households / dollars are weighted by person and household weights respectively, summed over all in-sample county-years (2016-2024). Migration rate is the share of persons in the base who moved across county lines (categories 3 and 5), computed within each (sample, group) cell."' _n
file write `fh' `"\item Source: ACS microdata via IPUMS, aggregated to county-year by 02\_descriptives.do."' _n
file write `fh' `"\end{tablenotes}"' _n
file write `fh' `"\end{threeparttable}"' _n
file write `fh' `"\end{table}"' _n
file close `fh'
dis "Wrote `_outfile'"
}


dis ""
dis "=============================================="
dis "Appendix descriptives build complete."
dis "  Outputs:"
dis "    ${results}tables/tableA1_sdid.tex"
dis "    ${results}tables/tableA1_irs_flow.tex"
dis "    ${results}tables/tableA1_acs.tex"
dis "=============================================="

capture log close log_02_appx
