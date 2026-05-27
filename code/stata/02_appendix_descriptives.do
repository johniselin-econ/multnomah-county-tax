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

Called by:      00_multnomah.do (Stage 6: Appendix & Robustness)

Inputs:         ${data}working/sdid_analysis_data.dta            (Table A)
                ${data}working/irs_county_flow.dta               (Table B)
                ${data}working/acs_county_gross_25plus.dta       (Table B ACS-county list; Table C)

Outputs:        ${results}tables/tableA1_sdid.tex
                ${results}tables/tableA2_irs_flow.tex
                ${results}tables/tableA3_acs.tex
                Same files copied to ${ol_tab} when ${overleaf}=1
*******************************************************************************/

** Load shared project defaults (path globals + Overleaf sync)
if "${code}" == "" {
    local _cwd = subinstr("`c(pwd)'", "\", "/", .)
    if regexm("`_cwd'", "(.*)/code/stata$") global code "`_cwd'/"
    else global code "`_cwd'/code/stata/"
}
do "${code}00_stata_config.do"
** 01a_programs.do is normally sourced by 00_multnomah.do; source defensively
** so build_acs_balanced_set is available when run standalone.
do "${code}01a_programs.do"

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

local pool_list "mult sample_all sample_urban95 sample_urban75_covid sample_demog sample_stringency sample_narrow"
local pool_label_mult                  "Multnomah"
local pool_label_sample_all            "All donor counties"
local pool_label_sample_urban95        "Urban top-5\%"
local pool_label_sample_urban75_covid  "Urban top-25\%, Covid match"
local pool_label_sample_demog          "Demographic match"
local pool_label_sample_stringency     "Stringency match"
local pool_label_sample_narrow         "Narrow similar-cities pool"

** Donor-pool conditions exclude Multnomah so the per-pool N counts and
** means describe donors only (Multnomah is reported separately on its
** own row).
local cond_mult                  "multnomah == 1"
local cond_sample_all            "sample_all == 1            & multnomah == 0"
local cond_sample_urban95        "sample_urban95 == 1        & multnomah == 0"
local cond_sample_urban75_covid  "sample_urban75_covid == 1  & multnomah == 0"
local cond_sample_demog          "sample_demog == 1          & multnomah == 0"
local cond_sample_stringency     "sample_stringency == 1     & multnomah == 0"
local cond_sample_narrow         "sample_narrow == 1         & multnomah == 0"

** Build four SDID descriptive matrices: 7 rows × 7 cols (N + 6 rate means).
** Four panels: IRS (all counties), IRS-389 (IRS restricted to the
** balanced ACS panel via _balanced_acs), ACS all-25+ (acs1), ACS College
** (acs2). The ACS public-use file identifies about 389 counties of
** residence; ~337 (336 donors + Multnomah) satisfy our balanced-panel
** and state-drop restrictions and appear in Panels~B--D, matching the
** SDID estimation samples.
tempname M_SDID_IRS M_SDID_IRS_389 M_SDID_ACS1 M_SDID_ACS2
foreach m in `M_SDID_IRS' `M_SDID_IRS_389' `M_SDID_ACS1' `M_SDID_ACS2' {
    matrix `m' = J(7, 7, .)
    matrix colnames `m' = N out_cty in_cty net_cty out_st in_st net_st
    matrix rownames `m' = mult all urban95 urban_covid demog stringency narrow
}

** _balanced_acs = county-level flag: 1 iff the county is in the balanced
** ACS panel (same county set used by the IRS-389, ACS-all, and ACS-college
** SDID specifications via irs_sample_2 / acs_period_1 / acs_period_2,
** which share an identical ct_tmp == max balance + state-drop zero-out).
bysort fips: egen byte _balanced_acs = max(irs_sample_2 == 1)

** Per-panel observability flags from a single year snapshot.
preserve
keep if year == 2019
gen byte has_irs  = !missing(agi_out_rate_irs)
gen byte has_acs1 = !missing(agi_out_rate_acs1)
gen byte has_acs2 = !missing(agi_out_rate_acs2)

local row = 1
foreach pool of local pool_list {
    qui count if `cond_`pool'' & has_irs == 1
    matrix `M_SDID_IRS'[`row', 1] = r(N)
    qui count if `cond_`pool'' & has_irs == 1 & _balanced_acs == 1
    matrix `M_SDID_IRS_389'[`row', 1] = r(N)
    qui count if `cond_`pool'' & has_acs1 == 1 & _balanced_acs == 1
    matrix `M_SDID_ACS1'[`row', 1] = r(N)
    qui count if `cond_`pool'' & has_acs2 == 1 & _balanced_acs == 1
    matrix `M_SDID_ACS2'[`row', 1] = r(N)
    local ++row
}
restore

** ---- IRS panels (all-counties + ACS-restricted): time-pooled means
**      2018-2022 (excluding 2020). Fills M_SDID_IRS and M_SDID_IRS_389
**      in one pass; same IRS data, different county filter.
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
            qui summ agi_`dir'_rate_`scope' if `cond_`pool'' & _balanced_acs == 1
            matrix `M_SDID_IRS_389'[`row', `col'] = r(mean)
        }
    }
    local ++row
}
restore

** ---- ACS panels (all-25+ and college): time-pooled means 2018-2024 (excl. 2020) ----
foreach acs_pair in "M_SDID_ACS1 acs1" "M_SDID_ACS2 acs2" {
    local matname : word 1 of `acs_pair'
    local src     : word 2 of `acs_pair'

    preserve
    keep if inrange(year, 2018, 2024) & year != 2020
    local row = 1
    foreach pool of local pool_list {
        local i = 0
        foreach scope in "`src'" "`src'_outstate" {
            foreach dir in "out" "in" "net" {
                local ++i
                local col = `i' + 1
                qui summ agi_`dir'_rate_`scope' if `cond_`pool'' & _balanced_acs == 1
                matrix ``matname''[`row', `col'] = r(mean)
            }
        }
        local ++row
    }
    restore
}

mat list `M_SDID_IRS'
mat list `M_SDID_IRS_389'
mat list `M_SDID_ACS1'
mat list `M_SDID_ACS2'

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

** Four-panel write: A=IRS, B=IRS-389, C=ACS all-25+, D=ACS college.
** matname_<letter> stores the local-NAME (not resolved tempname) so that
** double-dereference ``matname'' works at use time.
local matname_A "M_SDID_IRS"
local matname_B "M_SDID_IRS_389"
local matname_C "M_SDID_ACS1"
local matname_D "M_SDID_ACS2"
local hdr_A "Panel A: IRS, all counties (2018-2022, excl.\ 2020)"
local hdr_B "Panel B: IRS, restricted to ACS-identified counties (2018-2022, excl.\ 2020)"
local hdr_C "Panel C: ACS, all 25+ (2018-2024, excl.\ 2020)"
local hdr_D "Panel D: ACS, college-educated (2018-2024, excl.\ 2020)"

foreach letter in A B C D {
    local matname  "`matname_`letter''"
    local panel_hdr "`hdr_`letter''"

    if "`letter'" != "A" {
        file write `fh' `"\midrule"' _n
        file write `fh' `"\addlinespace[0.4em]"' _n
    }
    file write `fh' `"\multicolumn{8}{l}{\textit{`panel_hdr'}} \\"' _n
    file write `fh' `"\addlinespace"' _n

    forvalues r = 1/7 {
        local pool : word `r' of `pool_list'
        local lab  "`pool_label_`pool''"
        local nC : di %12.0fc ``matname''[`r', 1]
        local nC = strtrim("`nC'")
        local cells ""
        forvalues c = 2/7 {
            local v : di %5.2f ``matname''[`r', `c']
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
file write `fh' `"\item \textit{Notes:} Time-pooled means of AGI migration rates (\% of county base population), averaged over in-sample years (2020 excluded). County-level migration is any county-to-county move; out-of-state migration restricts to interstate moves. Donor-pool means exclude Multnomah, which is reported separately. The narrow pool retains Sacramento and Seattle (Vancouver/Clark, WA is excluded to avoid commuter spillover). Panels~B--D restrict to the balanced ACS panel, matching the IRS-389, ACS-25+, and ACS-college SDID estimation samples. The ACS public-use file identifies 389 counties of residence; 337 (336 donors $+$ Multnomah) satisfy our balanced-panel and state-drop restrictions and appear in Panels~B--D. The narrow pool has a fixed 20-county list; ACS-restricted panels see 18 donors because 2 narrow donors are not in the balanced ACS panel. See Appendix~B for donor-pool construction and Appendix~\ref{sec:appb_college} for the college subsample."' _n
file write `fh' `"\item Source: IRS SOI county-to-county migration flows (Panels~A and~B); ACS microdata, all 25+ (Panel~C) and college-educated (Panel~D) subsamples."' _n
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

** Build the ACS county set used to restrict Panel B, mirroring the flow
** estimation (02_flow_analysis.do): counties observed in the ACS 25+ panel in
** EVERY analysis year 2016-2024 (the balanced ACS set, ~389 counties). This is
** the same county restriction the PPML specification uses.
preserve
capture confirm file "${data}working/acs_county_gross_25plus.dta"
if _rc == 0 {
    ** Shared single-source-of-truth builder (01a_programs.do); leaves the set
    ** in memory so the count below works, and saves it to the tempfile.
    tempfile acs_counties
    build_acs_balanced_set, saving(`acs_counties') flag(acs_county)
    local have_acs = 1
    qui count
    dis "ACS balanced county set (2016-2024): " r(N) " counties"
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

** Drop Alaska and Hawaii flows (mirrors 02_flow_analysis.do)
drop if inlist(state_fips_o, 2, 15)
drop if inlist(state_fips_d, 2, 15)

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

** ---- Distinct counties per row: overall (either endpoint), origin, dest ----
** Overall = distinct counties appearing as origin OR destination; computed on a
** reshaped copy (endpoints stacked) so the flow dataset is untouched. Origin and
** destination counts are computed directly on fips_o / fips_d.
tempname N_CTY
matrix `N_CTY' = J(4, 3, .)
matrix colnames `N_CTY' = either origin dest

** Overall (either endpoint)
preserve
    keep fips_o fips_d mult_flow acs_flow
    rename fips_o fips1
    rename fips_d fips2
    gen long _rid = _n
    reshape long fips, i(_rid) j(_end)
    forvalues r = 1/4 {
        local mflow = cond(mod(`r', 2) == 1, 1, 0)
        local acsf  = cond(`r' <= 2, 0, 1)
        if `acsf' == 0 local cond "mult_flow == `mflow'"
        else           local cond "mult_flow == `mflow' & acs_flow == 1"
        qui levelsof fips if `cond', local(_lv)
        local _n : word count `_lv'
        matrix `N_CTY'[`r', 1] = `_n'
    }
restore

** Origin and destination
forvalues r = 1/4 {
    local mflow = cond(mod(`r', 2) == 1, 1, 0)
    local acsf  = cond(`r' <= 2, 0, 1)
    if `acsf' == 0 local cond "mult_flow == `mflow'"
    else           local cond "mult_flow == `mflow' & acs_flow == 1"
    qui levelsof fips_o if `cond', local(_lo)
    local _n : word count `_lo'
    matrix `N_CTY'[`r', 2] = `_n'
    qui levelsof fips_d if `cond', local(_ld)
    local _n : word count `_ld'
    matrix `N_CTY'[`r', 3] = `_n'
}
mat list `N_CTY'

** Write tex
local _dests `""${results}tables/tableA2_irs_flow.tex""'
if ${overleaf} == 1 {
    local _dests `"`_dests' "${ol_tab}tableA2_irs_flow.tex""'
}
foreach _outfile of local _dests {
tempname fh
file open `fh' using "`_outfile'", write replace
file write `fh' "% Appendix Table A2: IRS Flow descriptives" _n
file write `fh' "% Generated by 02_appendix_descriptives.do (item 11)" _n
file write `fh' `"\begin{table}[htbp]"' _n
file write `fh' `"\centering"' _n
file write `fh' `"\begin{threeparttable}"' _n
file write `fh' `"\footnotesize"' _n
file write `fh' `"\caption{IRS County-to-County Flow Sample: Descriptive Statistics}"' _n
file write `fh' `"\label{tab:tableA2_irs_flow}"' _n
file write `fh' `"\setlength{\tabcolsep}{3pt}"' _n
file write `fh' `"\begin{tabular}{l r r r r r r r r}"' _n
file write `fh' `"\toprule"' _n
file write `fh' `" & Number of & \multicolumn{3}{c}{Unique counties} & Median count & Mean count & Mean count & Mean AGI \\"' _n
file write `fh' `"\cmidrule(lr){3-5}"' _n
file write `fh' `" & county-flows & Overall & Origin & Dest. & of returns & of returns & of exemptions & (USD thousands) \\"' _n
file write `fh' `"\midrule"' _n

local row_labels `""Multnomah-touching flows" "Non-Multnomah flows""'
foreach panel in All ACS {
    if "`panel'" == "All" {
        file write `fh' `"\multicolumn{9}{l}{\textit{Panel A: All flows}} \\"' _n
        local row_off 0
    }
    else {
        file write `fh' `"\midrule"' _n
        file write `fh' `"\addlinespace[0.4em]"' _n
        file write `fh' `"\multicolumn{9}{l}{\textit{Panel B: ACS-restricted (both endpoints in the balanced ACS county set)}} \\"' _n
        local row_off 2
    }
    file write `fh' `"\addlinespace"' _n
    forvalues k = 1/2 {
        local r = `k' + `row_off'
        local lab : word `k' of `row_labels'
        local nf : di %14.0fc `M_FLOW'[`r', 1]
        local nf = strtrim("`nf'")
        local nce : di %14.0fc `N_CTY'[`r', 1]
        local nce = strtrim("`nce'")
        local nco : di %14.0fc `N_CTY'[`r', 2]
        local nco = strtrim("`nco'")
        local ncd : di %14.0fc `N_CTY'[`r', 3]
        local ncd = strtrim("`ncd'")
        local md : di %7.1f `M_FLOW'[`r', 2]
        local md = strtrim("`md'")
        local mn1 : di %7.1f `M_FLOW'[`r', 3]
        local mn1 = strtrim("`mn1'")
        local mn2 : di %7.1f `M_FLOW'[`r', 4]
        local mn2 = strtrim("`mn2'")
        local mag : di %9.1f `M_FLOW'[`r', 5]
        local mag = strtrim("`mag'")
        file write `fh' `"`lab' & `nf' & `nce' & `nco' & `ncd' & `md' & `mn1' & `mn2' & `mag' \\"' _n
    }
}

file write `fh' `"\bottomrule"' _n
file write `fh' `"\end{tabular}"' _n
file write `fh' `"\begin{tablenotes}[flushleft]"' _n
file write `fh' `"\small"' _n
file write `fh' `"\item \textit{Notes:} County-pair flow rows from IRS SOI 2016-2022, after dropping state-aggregate rows and Alaska/Hawaii. A flow is Multnomah-touching if either origin or destination is Multnomah County. The ACS-restricted panel keeps only flows where both endpoints are among the counties observed in the ACS in every year 2016-2024 (the balanced ACS county set), matching the PPML specification used in the paper. The Unique-counties columns count distinct counties appearing as either endpoint (Overall), as the origin (Origin), or as the destination (Dest.) of a flow in each row. Mean and median flow sizes are computed across all observed flow rows in the group; returns are tax-filing households, exemptions are individuals (IRS variables \texttt{n1} and \texttt{n2}), and AGI is in thousands of dollars. The data drop county-pair-years with fewer than ~20 movers (the IRS suppression threshold), so explicit zero-mover rows do not appear."' _n
file write `fh' `"\item Source: IRS SOI county-to-county migration flows (2016-2022)."' _n
file write `fh' `"\end{tablenotes}"' _n
file write `fh' `"\end{threeparttable}"' _n
file write `fh' `"\end{table}"' _n
file close `fh'
dis "Wrote `_outfile'"
}


********************************************************************************
** Appendix Table A3: ACS sample descriptives, individual-level
********************************************************************************
**
** Restructured (May 2026): drop the in-migration panel (it was nearly identical
** to out-migration), transpose so Multnomah and Non-Multnomah are COLUMNS, and
** organize stats into three panels:
**
**   Panel A (Size)         counties, persons, households, total income
**   Panel B (Means+Medians) age, household income, county/state migration rates
**   Panel C (Tabulations)   shares of individuals by # children, education, age bin
**
** Switches input from acs_county_gross_25plus.dta (county-year aggregates) to
** acs_migration_file.dta (individual-level microdata) so we can compute means /
** medians / tabulations directly with person weights.

dis ""
dis "--- Table A1.C / A3: ACS individual-level descriptives ---"

use "${data}working/acs_migration_file.dta", clear

** Replicate the analytical-sample filters from 02_indiv_analysis.do so this
** descriptive table reflects the same universe used for paper estimates.
keep if inrange(year, 2016, 2024)
drop if qmigplc1 == 4                    // bad migration place
drop if inlist(state_fips_o, 2, 15)      // Alaska / Hawaii origin
drop if inlist(state_fips_d, 2, 15)      // Alaska / Hawaii destination
drop if ftotinc < 0                      // negative family income
drop if age < 25                         // 25+ subsample

** Multnomah origin flag (single column split)
gen byte mult = (state_fips_o == 41 & county_fips_o == 51)

** Migration indicators (using IPUMS migrate1: 1=non-mover, 2=same-state move,
** 3=different-state move, 4=abroad, 9=unknown).
gen byte cty_mover   = inlist(migrate1, 2, 3)
gen byte state_mover = (migrate1 == 3)

** Education collapsed to 4 categories (IPUMS educd):
**   <=64       HS or less (no schooling through HS grad / GED)
**   65..81     Some college or associate's
**   100..101   Bachelor's
**   >=110      Graduate (Master's, professional, doctorate)
gen byte educ_cat = .
replace educ_cat = 1 if educd <= 64
replace educ_cat = 2 if educd >= 65  & educd <= 81
replace educ_cat = 3 if educd >= 100 & educd <= 101
replace educ_cat = 4 if educd >= 110
label define lb_educ_cat 1 "HS or less" 2 "Some college / Assoc." 3 "Bachelor's" 4 "Graduate", replace
label values educ_cat lb_educ_cat

** Number of children categories (0, 1, 2, 3+)
gen byte nchild_cat = .
replace nchild_cat = 1 if nchild == 0
replace nchild_cat = 2 if nchild == 1
replace nchild_cat = 3 if nchild == 2
replace nchild_cat = 4 if nchild >= 3 & !missing(nchild)

** Age bins
gen byte age_cat = .
replace age_cat = 1 if inrange(age, 25, 34)
replace age_cat = 2 if inrange(age, 35, 44)
replace age_cat = 3 if inrange(age, 45, 54)
replace age_cat = 4 if inrange(age, 55, 64)
replace age_cat = 5 if age >= 65

** ---- Build a 23-row x 2-col results matrix ----
** Rows (in order):
**   Panel A (4): N counties, persons, households, total income
**   Panel B (6): mean age, median age, mean hh income, median hh income,
**                mean county migration rate, mean state migration rate
**   Panel C kids   (4): % nchild=0/1/2/3+
**   Panel C educ   (4): % HS/Some college/Bachelor's/Graduate
**   Panel C age    (5): % 25-34/35-44/45-54/55-64/65+
tempname M_ACS_SAMP
matrix `M_ACS_SAMP' = J(23, 2, .)

** Helper: column index 1 = Multnomah (mult==1), 2 = Non-Multnomah (mult==0)
local col_M 1
local col_N 2

** Panel A row 1: distinct counties of origin
foreach m in 1 0 {
    local c = cond(`m' == 1, `col_M', `col_N')
    qui distinct fips_o if mult == `m'
    matrix `M_ACS_SAMP'[1, `c'] = r(ndistinct)
}

** Panel A rows 2-4: weighted persons, households, total income
foreach m in 1 0 {
    local c = cond(`m' == 1, `col_M', `col_N')
    qui summ perwt if mult == `m', meanonly
    matrix `M_ACS_SAMP'[2, `c'] = r(sum)
    qui summ hhwt if mult == `m' & hh_head == 1, meanonly
    matrix `M_ACS_SAMP'[3, `c'] = r(sum)
    tempvar hhinc_w
    gen double `hhinc_w' = hhwt * ftotinc if mult == `m' & hh_head == 1
    qui summ `hhinc_w', meanonly
    matrix `M_ACS_SAMP'[4, `c'] = r(sum)
    drop `hhinc_w'
}

** Panel B rows 5-10: means and medians (age, hh income), migration rates
foreach m in 1 0 {
    local c = cond(`m' == 1, `col_M', `col_N')
    qui summ age [aw = perwt] if mult == `m', detail
    matrix `M_ACS_SAMP'[5, `c'] = r(mean)
    matrix `M_ACS_SAMP'[6, `c'] = r(p50)

    qui summ ftotinc [aw = hhwt] if mult == `m' & hh_head == 1, detail
    matrix `M_ACS_SAMP'[7, `c'] = r(mean)
    matrix `M_ACS_SAMP'[8, `c'] = r(p50)

    qui summ cty_mover [aw = perwt] if mult == `m', meanonly
    matrix `M_ACS_SAMP'[9, `c'] = 100 * r(mean)
    qui summ state_mover [aw = perwt] if mult == `m', meanonly
    matrix `M_ACS_SAMP'[10, `c'] = 100 * r(mean)
}

** Panel C: weighted shares of categorical variables (rows 11-23)
foreach m in 1 0 {
    local c = cond(`m' == 1, `col_M', `col_N')
    qui summ perwt if mult == `m', meanonly
    local denom = r(sum)

    ** Number of children: rows 11-14 for k=1..4
    forvalues k = 1/4 {
        qui summ perwt if mult == `m' & nchild_cat == `k', meanonly
        matrix `M_ACS_SAMP'[10 + `k', `c'] = 100 * r(sum) / `denom'
    }
    ** Education: rows 15-18 for k=1..4
    forvalues k = 1/4 {
        qui summ perwt if mult == `m' & educ_cat == `k', meanonly
        matrix `M_ACS_SAMP'[14 + `k', `c'] = 100 * r(sum) / `denom'
    }
    ** Age bins: rows 19-23 for k=1..5
    forvalues k = 1/5 {
        qui summ perwt if mult == `m' & age_cat == `k', meanonly
        matrix `M_ACS_SAMP'[18 + `k', `c'] = 100 * r(sum) / `denom'
    }
}

mat list `M_ACS_SAMP'

** ---- Write LaTeX table ----
local _dests `""${results}tables/tableA3_acs.tex""'
if ${overleaf} == 1 {
    local _dests `"`_dests' "${ol_tab}tableA3_acs.tex""'
}
foreach _outfile of local _dests {
tempname fh
file open `fh' using "`_outfile'", write replace

file write `fh' "% Appendix Table A3: ACS individual-level descriptives" _n
file write `fh' "% Generated by 02_appendix_descriptives.do" _n
file write `fh' `"\begin{table}[htbp]"' _n
file write `fh' `"\centering"' _n
file write `fh' `"\begin{threeparttable}"' _n
file write `fh' `"\footnotesize"' _n
file write `fh' `"\caption{ACS Microdata: Sample Descriptives}"' _n
file write `fh' `"\label{tab:tableA3_acs}"' _n
file write `fh' `"\setlength{\tabcolsep}{6pt}"' _n
file write `fh' `"\begin{tabular}{l r r}"' _n
file write `fh' `"\toprule"' _n
file write `fh' `" & Multnomah & Non-Multnomah counties \\"' _n
file write `fh' `"\midrule"' _n

** ---- PANEL A: Size ----
file write `fh' `"\multicolumn{3}{l}{\textit{Panel A: Sample size}} \\"' _n
file write `fh' `"\addlinespace"' _n

** Row 1: Counties (integer)
local v1 : di %12.0fc `M_ACS_SAMP'[1, 1]
local v1 = strtrim("`v1'")
local v2 : di %12.0fc `M_ACS_SAMP'[1, 2]
local v2 = strtrim("`v2'")
file write `fh' `"\quad Number of counties & `v1' & `v2' \\"' _n

** Row 2: Persons (millions, weighted) — display as M with 1 decimal
local v1 : di %12.1fc (`M_ACS_SAMP'[2, 1] / 1e6)
local v1 = strtrim("`v1'")
local v2 : di %12.1fc (`M_ACS_SAMP'[2, 2] / 1e6)
local v2 = strtrim("`v2'")
file write `fh' `"\quad Persons (millions, weighted) & `v1' & `v2' \\"' _n

** Row 3: Households (millions, weighted)
local v1 : di %12.1fc (`M_ACS_SAMP'[3, 1] / 1e6)
local v1 = strtrim("`v1'")
local v2 : di %12.1fc (`M_ACS_SAMP'[3, 2] / 1e6)
local v2 = strtrim("`v2'")
file write `fh' `"\quad Households (millions, weighted) & `v1' & `v2' \\"' _n

** Row 4: Total income (USD billions, weighted)
local v1 : di %12.1fc (`M_ACS_SAMP'[4, 1] / 1e9)
local v1 = strtrim("`v1'")
local v2 : di %12.1fc (`M_ACS_SAMP'[4, 2] / 1e9)
local v2 = strtrim("`v2'")
file write `fh' `"\quad Total income (USD billions, weighted) & `v1' & `v2' \\"' _n

** ---- PANEL B: Means + Medians ----
file write `fh' `"\midrule"' _n
file write `fh' `"\addlinespace[0.4em]"' _n
file write `fh' `"\multicolumn{3}{l}{\textit{Panel B: Means and medians}} \\"' _n
file write `fh' `"\addlinespace"' _n

local labs `""Mean age" "Median age" "Mean household income (USD)" "Median household income (USD)" "County migration rate (\%)" "State migration rate (\%)""'
forvalues r = 5/10 {
    local k = `r' - 4
    local lab : word `k' of `labs'
    if `r' == 5 | `r' == 6 {
        local v1 : di %5.1f `M_ACS_SAMP'[`r', 1]
        local v2 : di %5.1f `M_ACS_SAMP'[`r', 2]
    }
    else if `r' == 7 | `r' == 8 {
        local v1 : di %12.0fc `M_ACS_SAMP'[`r', 1]
        local v2 : di %12.0fc `M_ACS_SAMP'[`r', 2]
    }
    else {
        local v1 : di %5.2f `M_ACS_SAMP'[`r', 1]
        local v2 : di %5.2f `M_ACS_SAMP'[`r', 2]
    }
    local v1 = strtrim("`v1'")
    local v2 = strtrim("`v2'")
    file write `fh' `"\quad `lab' & `v1' & `v2' \\"' _n
}

** ---- PANEL C: Tabulations ----
file write `fh' `"\midrule"' _n
file write `fh' `"\addlinespace[0.4em]"' _n
file write `fh' `"\multicolumn{3}{l}{\textit{Panel C: Distribution of individuals (\%, weighted)}} \\"' _n
file write `fh' `"\addlinespace"' _n

** Children (rows 11-14)
file write `fh' `"\quad \textit{Number of children:} & & \\"' _n
local kid_labs `""0" "1" "2" "3+""'
forvalues k = 1/4 {
    local lab : word `k' of `kid_labs'
    local r = 10 + `k'
    local v1 : di %5.1f `M_ACS_SAMP'[`r', 1]
    local v2 : di %5.1f `M_ACS_SAMP'[`r', 2]
    local v1 = strtrim("`v1'")
    local v2 = strtrim("`v2'")
    file write `fh' `"\quad\quad `lab' & `v1' & `v2' \\"' _n
}

** Education (rows 15-18)
file write `fh' `"\addlinespace[0.3em]"' _n
file write `fh' `"\quad \textit{Education:} & & \\"' _n
local educ_labs `""HS or less" "Some college / Associate's" "Bachelor's" "Graduate""'
forvalues k = 1/4 {
    local lab : word `k' of `educ_labs'
    local r = 14 + `k'
    local v1 : di %5.1f `M_ACS_SAMP'[`r', 1]
    local v2 : di %5.1f `M_ACS_SAMP'[`r', 2]
    local v1 = strtrim("`v1'")
    local v2 = strtrim("`v2'")
    file write `fh' `"\quad\quad `lab' & `v1' & `v2' \\"' _n
}

** Age bins (rows 19-23)
file write `fh' `"\addlinespace[0.3em]"' _n
file write `fh' `"\quad \textit{Age bin:} & & \\"' _n
local age_labs `""25--34" "35--44" "45--54" "55--64" "65+""'
forvalues k = 1/5 {
    local lab : word `k' of `age_labs'
    local r = 18 + `k'
    local v1 : di %5.1f `M_ACS_SAMP'[`r', 1]
    local v2 : di %5.1f `M_ACS_SAMP'[`r', 2]
    local v1 = strtrim("`v1'")
    local v2 = strtrim("`v2'")
    file write `fh' `"\quad\quad `lab' & `v1' & `v2' \\"' _n
}

file write `fh' `"\bottomrule"' _n
file write `fh' `"\end{tabular}"' _n
file write `fh' `"\begin{tablenotes}[flushleft]"' _n
file write `fh' `"\small"' _n
file write `fh' `"\item \textit{Notes:} Sample is ACS individuals aged 25+ in the analytical universe (2016-2024, dropping AK / HI and negative family income). The Multnomah column restricts to individuals whose prior-year county of residence was Multnomah; the Non-Multnomah column is everyone else in the sample. Person- and household-level weights (\texttt{perwt}, \texttt{hhwt}) are applied throughout. Total income sums household-level family income (\texttt{ftotinc}) over household-head records. Migration rates use IPUMS \texttt{migrate1}: a county move is any inter- or intra-state move; a state move is a different state of residence in the prior year. Panel C reports weighted percentages of individuals within each column."' _n
file write `fh' `"\item Source: ACS microdata via IPUMS (2016-2024)."' _n
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
dis "    ${results}tables/tableA2_irs_flow.tex"
dis "    ${results}tables/tableA3_acs.tex"
dis "=============================================="

capture log close log_02_appx
