/*******************************************************************************
File Name:      02_descriptives_supp.do
Creator:        John Iselin (with Claude)
Date Created:   2026-05-06
Last Modified:  2026-05-07 (post-review cleanup: removed dead Appendix A1 builder; collapsed duplicate panel-write blocks)

Purpose:        Build Table 1 (combined) for the revised short-paper draft.

                NEW (item 8 of May 2026 paper revision TODO):
                Two-panel structure -- Panel A = IRS, Panel B = ACS College --
                each with 6 rows (Multnomah + the 5 donor pools defined in
                02_sdid_analysis.do) and 8 numeric columns (N counties,
                out-migration pre/post, in-migration pre/post, net pre/post,
                net change). The old "sample composition" Panel B has been
                dropped.

                Pools:
                  * sample_all            -- all donor counties (broad)
                  * sample_urban95        -- urban top 5%
                  * sample_urban75_covid  -- urban top 25% x Covid k-means
                  * sample_demog          -- demographic k-means
                  * sample_stringency     -- urban top 25% x stringency k-means

                The existing ${ol_tab}table1.tex (county-by-county
                characteristics) and ${ol_tab}table2.tex (Multnomah +
                neighbors migration rates) are NOT modified -- those are
                still produced by 02_descriptives.do for the long version.
                This file emits a parallel artifact `table1_combined.tex`
                that the new short paper (`updated.tex`) inputs in place
                of the old Table 1.

Called by:      Standalone (or 00_multnomah.do orchestrator block).
                Depends on `02_sdid_analysis.do` having produced
                `sdid_analysis_data.dta` (for the pool indicators and
                pre-computed migration rates).

Inputs:         ${data}working/sdid_analysis_data.dta

Outputs:        ${results}tables/table1_combined.tex
                ${results}tables/table1_combined.csv      (raw values for QA)
                ${ol_tab}table1_combined.tex              (if ${overleaf}==1)
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

** Source profile.do for Overleaf sync globals when run standalone (the
** orchestrator does this in 00_multnomah.do; replicate here so this script
** works either way).
if "${ol_tab}" == "" {
    capture do "${dir}/profile.do"
    if "${oth_path}" != "" {
        global ol_fig "${oth_path}figures/"
        global ol_tab "${oth_path}tables/"
        global overleaf = 1
    }
}

** Start log file
capture log close log_02_supp
log using "${logs}02_log_descriptives_supp_${date}", replace text name(log_02_supp)

** Ensure output dir exists
capture mkdir "${results}tables"

dis ""
dis "=============================================="
dis "Building new Table 1: Multnomah vs. comparison groups"
dis "=============================================="

********************************************************************************
** SECTION 1: Compute pre/post migration rates by donor pool, two data sources
********************************************************************************
**
** Two panels: A = IRS (uses agi_*_rate_irs columns), B = ACS College
** (uses agi_*_rate_acs2 columns). Each panel has 6 rows (Multnomah +
** 5 donor pools) and 8 numeric columns:
**     N counties, out_pre, out_post, in_pre, in_post, net_pre, net_post,
**     net_change (= net_post - net_pre).
**
** Pre/post windows:
**     IRS  : pre = 2018-2019, post = 2021-2022 (drop 2020).
**     ACS  : pre = 2018-2019, post = 2021-2024 (drop 2020) -- ACS has
**            an extra two years post-treatment.

use "${data}working/sdid_analysis_data.dta", clear

** Multnomah identifier (defensive)
capture confirm variable multnomah
if _rc {
    gen byte multnomah = (state_fips == 41 & county_fips == 51)
}

** Helper: build one panel matrix `mat_<suffix>` for a given outcome stem
** (`agi_<dir>_rate_<src>` where src in {irs, acs2}).
**
** Stata local arrays at multi-level loops get unwieldy quickly; using
** a tempname matrix per panel keeps the table-write loop straightforward.

** ---- Define pool list (consistent across panels) ----
** Order: Multnomah first, then 5 donor pools.
local pool_list "mult sample_all sample_urban95 sample_urban75_covid sample_demog sample_stringency"
local pool_label_mult                  "Multnomah"
local pool_label_sample_all            "All donor counties (mean)"
local pool_label_sample_urban95        "Urban top-5\% (mean)"
local pool_label_sample_urban75_covid  "Urban top-25\%, Covid match (mean)"
local pool_label_sample_demog          "Demographic match (mean)"
local pool_label_sample_stringency     "Stringency match (mean)"

** Map pool name â†’ row condition
local cond_mult                  "multnomah == 1"
local cond_sample_all            "sample_all == 1"
local cond_sample_urban95        "sample_urban95 == 1"
local cond_sample_urban75_covid  "sample_urban75_covid == 1"
local cond_sample_demog          "sample_demog == 1"
local cond_sample_stringency     "sample_stringency == 1"

** Build one matrix per data source.
**
** For IRS, the `period_post` indicator is straightforward (year in {2021, 2022}).
** For ACS we widen the post window through 2024 since the ACS columns
** are available there.

tempname M_IRS M_ACS
matrix `M_IRS' = J(6, 8, .)
matrix `M_ACS' = J(6, 8, .)
matrix colnames `M_IRS' = N out_pre out_post in_pre in_post net_pre net_post net_chg
matrix colnames `M_ACS' = N out_pre out_post in_pre in_post net_pre net_post net_chg
matrix rownames `M_IRS' = mult all urban95 urban_covid demog stringency
matrix rownames `M_ACS' = mult all urban95 urban_covid demog stringency

** Capture pool county counts using a single year-snapshot.
** (Pools are time-invariant; using year == 2019 gives a clean snapshot.)
preserve
keep if year == 2019
local row = 1
foreach pool of local pool_list {
    qui count if `cond_`pool''
    matrix `M_IRS'[`row', 1] = r(N)
    matrix `M_ACS'[`row', 1] = r(N)
    local ++row
}
restore

** ---- IRS panel: pre = 2018-2019, post = 2021-2022 ----
preserve
keep if inrange(year, 2018, 2022) & year != 2020
gen byte period_post = inlist(year, 2021, 2022)

local row = 1
foreach pool of local pool_list {
    foreach dir in "out" "in" "net" {
        local col_off = cond("`dir'" == "out", 1, cond("`dir'" == "in", 3, 5))
        foreach per in 0 1 {
            qui summ agi_`dir'_rate_irs if `cond_`pool'' & period_post == `per'
            local col = `col_off' + 1 + `per'
            matrix `M_IRS'[`row', `col'] = r(mean)
        }
    }
    matrix `M_IRS'[`row', 8] = `M_IRS'[`row', 7] - `M_IRS'[`row', 6]
    local ++row
}
restore

** ---- ACS panel: pre = 2018-2019, post = 2021-2024 ----
preserve
keep if inrange(year, 2018, 2024) & year != 2020
gen byte period_post = inrange(year, 2021, 2024)

local row = 1
foreach pool of local pool_list {
    foreach dir in "out" "in" "net" {
        local col_off = cond("`dir'" == "out", 1, cond("`dir'" == "in", 3, 5))
        foreach per in 0 1 {
            qui summ agi_`dir'_rate_acs2 if `cond_`pool'' & period_post == `per'
            local col = `col_off' + 1 + `per'
            matrix `M_ACS'[`row', `col'] = r(mean)
        }
    }
    matrix `M_ACS'[`row', 8] = `M_ACS'[`row', 7] - `M_ACS'[`row', 6]
    local ++row
}
restore

mat list `M_IRS'
mat list `M_ACS'

** ---- CSV export for QA ----
preserve
clear
set obs 12
gen str8  panel    = ""
gen str40 pool     = ""
gen long  N        = .
gen double out_pre = .
gen double out_post = .
gen double in_pre  = .
gen double in_post = .
gen double net_pre = .
gen double net_post = .
gen double net_chg = .
forvalues r = 1/6 {
    replace panel    = "IRS"  in `r'
    replace pool     = `"`pool_label_`: word `r' of `pool_list'''"' in `r'
    replace N        = `M_IRS'[`r', 1] in `r'
    replace out_pre  = `M_IRS'[`r', 2] in `r'
    replace out_post = `M_IRS'[`r', 3] in `r'
    replace in_pre   = `M_IRS'[`r', 4] in `r'
    replace in_post  = `M_IRS'[`r', 5] in `r'
    replace net_pre  = `M_IRS'[`r', 6] in `r'
    replace net_post = `M_IRS'[`r', 7] in `r'
    replace net_chg  = `M_IRS'[`r', 8] in `r'
}
forvalues r = 1/6 {
    local rr = `r' + 6
    replace panel    = "ACS"  in `rr'
    replace pool     = `"`pool_label_`: word `r' of `pool_list'''"' in `rr'
    replace N        = `M_ACS'[`r', 1] in `rr'
    replace out_pre  = `M_ACS'[`r', 2] in `rr'
    replace out_post = `M_ACS'[`r', 3] in `rr'
    replace in_pre   = `M_ACS'[`r', 4] in `rr'
    replace in_post  = `M_ACS'[`r', 5] in `rr'
    replace net_pre  = `M_ACS'[`r', 6] in `rr'
    replace net_post = `M_ACS'[`r', 7] in `rr'
    replace net_chg  = `M_ACS'[`r', 8] in `rr'
}
export delimited "${results}tables/table1_combined.csv", replace
restore


********************************************************************************
** SECTION 2: Write LaTeX table
********************************************************************************

local _dests `""${results}tables/table1_combined.tex""'
if ${overleaf} == 1 {
    local _dests `"`_dests' "${ol_tab}table1_combined.tex""'
}

foreach _outfile of local _dests {

tempname fh
file open `fh' using "`_outfile'", write replace

file write `fh' "% Table 1 (combined): Migration rates by donor pool (item 8)" _n
file write `fh' "% Generated by 02_descriptives_supp.do" _n
file write `fh' "% Requires: \usepackage{booktabs, threeparttable}" _n
file write `fh' `"\begin{table}[htbp]"' _n
file write `fh' `"\centering"' _n
file write `fh' `"\caption{AGI Migration Rates by Comparison Group: Multnomah vs.\ SDID Donor Pools}"' _n
file write `fh' `"\label{tab:multnomah_vs_groups}"' _n
file write `fh' `"\begin{threeparttable}"' _n
file write `fh' `"\footnotesize"' _n
file write `fh' `"\setlength{\tabcolsep}{3pt}"' _n

** Panel header -- shared header block written once, with both panels
** stacked into a single tabular environment for tidy column alignment.
file write `fh' `"\begin{tabular}{l r c c c c c c c}"' _n
file write `fh' `"\toprule"' _n
file write `fh' `" & N & \multicolumn{2}{c}{Out-migration} & \multicolumn{2}{c}{In-migration} & \multicolumn{2}{c}{Net in-migration} & Net \\"' _n
file write `fh' `"\cmidrule(lr){3-4} \cmidrule(lr){5-6} \cmidrule(lr){7-8}"' _n
file write `fh' `" & counties & Pre & Post & Pre & Post & Pre & Post & change (pp) \\"' _n
file write `fh' `"\midrule"' _n

** ---- Panels A (IRS, 2018--19 vs 2021--22) and B (ACS College, 2018--19 vs 2021--24) ----
** Cols: 1=N, 2=out_pre, 3=out_post, 4=in_pre, 5=in_post, 6=net_pre, 7=net_post, 8=net_chg
foreach panel in IRS ACS {
    if "`panel'" == "IRS" {
        local matname "`M_IRS'"
        local panel_hdr "Panel A: IRS (Pre = 2018--2019; Post = 2021--2022)"
    }
    else {
        local matname "`M_ACS'"
        local panel_hdr "Panel B: ACS College (Pre = 2018--2019; Post = 2021--2024)"
        file write `fh' `"\midrule"' _n
        file write `fh' `"\addlinespace[0.4em]"' _n
    }
    file write `fh' `"\multicolumn{9}{l}{\textit{`panel_hdr'}} \\"' _n
    file write `fh' `"\addlinespace"' _n

    forvalues r = 1/6 {
        local pool : word `r' of `pool_list'
        local lab  "`pool_label_`pool''"

        local nC : di %12.0fc `matname'[`r', 1]
        local nC = strtrim("`nC'")
        local cells ""
        forvalues c = 2/8 {
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

** Notes
file write `fh' `"\begin{tablenotes}[flushleft]"' _n
file write `fh' `"\small"' _n
file write `fh' `"\item \textit{Notes:} AGI in-, out-, and net-migration rates as a percentage of each county's base filing population, averaged over the indicated pre and post periods (2020 dropped). Means within each donor pool are simple county-level means (each county weighted equally), matching the SDID donor-pool construction. The 2018--2019 pre-period is shared across panels; the IRS post-period is 2021--2022 (the last year IRS county-to-county data is currently available), while the ACS post-period is extended through 2024."' _n
file write `fh' `"\item \textit{Donor pools.} The all-donor-counties pool is the broad SDID benchmark: all U.S.\ counties excluding Alaska, Hawaii, California, Washington, and non-Multnomah Oregon counties. Urban top-5\% restricts to counties in the top 5\% of urban-share. Urban-Covid match restricts to the urban top-25\% k-means cluster matched to Multnomah on Covid case and death trajectories. Demographic match k-means clusters on pre-treatment per-capita income, population, urban share, and age distribution. Stringency match restricts to the urban top-25\% k-means cluster matched on JII Covid policy stringency duration. See Appendix~B for full donor-pool construction details."' _n
file write `fh' `"\item Source: IRS SOI county-to-county migration flows (Panel~A); ACS microdata, college-educated subsample (Panel~B)."' _n
file write `fh' `"\end{tablenotes}"' _n
file write `fh' `"\end{threeparttable}"' _n
file write `fh' `"\end{table}"' _n

file close `fh'
dis "Wrote `_outfile'"

} // end foreach _outfile

dis ""
dis "=============================================="
dis "Table 1 (combined) build complete."
dis "  Output:    ${results}tables/table1_combined.tex"
if ${overleaf} == 1 {
    dis "  Overleaf:  ${ol_tab}table1_combined.tex"
}
dis "  CSV (QA):  ${results}tables/table1_combined.csv"
dis "=============================================="

** Note: the old Appendix Table A1 builder (variable definitions panel)
** has been moved to 02_appendix_descriptives.do, which produces three
** method-specific descriptive tables (SDID, IRS Flow, ACS) per item 11
** of the May 2026 paper revision TODO.

capture log close log_02_supp
