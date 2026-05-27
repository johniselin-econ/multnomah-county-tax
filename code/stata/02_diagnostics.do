/*****************************************************************************
* File:        02_diagnostics.do
* Purpose:     County-count / observation AUDIT of the actual estimation
*              samples. For each approach it reports the number of counties
*              (residence / either-endpoint, origin, destination), years, and
*              observations actually used:
*                - SDID   : counted directly from sdid_analysis_data.dta, so the
*                           audit cannot drift from the estimation (matches
*                           Table 1 / Appendix Table A1, which load the same file).
*                - Flows  : same SAMPLE RESTRICTIONS as 02_flow_analysis.do
*                           (ACS set balanced 2016-2024, AK/HI dropped, both
*                           endpoints in ACS), reported with AND without 2020.
*                           NOTE: counts the OBSERVED flow sample. n_obs is
*                           observed flow-years; it is NOT the PPML estimation
*                           N, which adds tsfill,full zero-rows, restricts to
*                           movers, and inner-joins covariates. The county
*                           counts are unaffected by those steps.
*                - DiD    : mirrors 02_did_analysis.do (ACS individuals 25+,
*                           2016-2024 excl. 2020, AK/HI + neg-income + qmigplc1
*                           dropped).
*              Also writes the SDID sample-construction FUNNEL
*              (IRS -> +ACS -> +demographics -> +BEA -> balance -> state drops),
*              validated against the estimation file.
*
*              County definition throughout: residence (= destination) county.
*
* Called by:   00_multnomah.do (after the analysis scripts) or standalone.
* Outputs:     ${results}tables/diagnostics_obs_counts.{tex,csv,xlsx}
*              ${results}tables/sdid_sample_funnel.{tex,csv}
*
* Note:        Counts only -- no regressions.
******************************************************************************/

** Load shared project defaults and helper programs
if "${code}" == "" {
	local _cwd = subinstr("`c(pwd)'", "\", "/", .)
	if regexm("`_cwd'", "(.*)/code/stata$") global code "`_cwd'/"
	else global code "`_cwd'/code/stata/"
}
do "${code}00_stata_config.do"
** 01a_programs.do is normally sourced by 00_multnomah.do; source defensively
** so 02_diagnostics.do can be invoked standalone (needs load_narrow_pool).
do "${code}01a_programs.do"

capture log close log_diag
log using "${logs}02_log_diagnostics_${pr_name}_${date}", replace text name(log_diag)
project_set_seed, context("02_diagnostics.do") offset(90)

********************************************************************************
** Helper programs
********************************************************************************

** distinct_count: store r(nd) = # distinct values of `varlist' (if `if').
** `distinct' does not modify the data, so no preserve is needed (and using
** one here would clash with an enclosing preserve in the calling code).
capture program drop diag_distinct
program define diag_distinct, rclass
	syntax varname [if]
	qui distinct `varlist' `if'
	return scalar nd = r(ndistinct)
end

** distinct_either: store r(nd) = # distinct counties appearing as origin OR
** destination among the kept observations.
capture program drop diag_distinct_either
program define diag_distinct_either, rclass
	syntax [if]
	preserve
		if "`if'" != "" qui keep `if'
		keep fips_o fips_d
		rename fips_o _f1
		rename fips_d _f2
		gen long _rid = _n
		qui reshape long _f, i(_rid) j(_end)
		qui distinct _f
		return scalar nd = r(ndistinct)
	restore
end

********************************************************************************
** Initialize results dataset
********************************************************************************
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

** post_row: append one audit row. Pass counts as locals before calling.
**   `approach' `sample' `data_source' `unit' `nc' `norig' `ndest' `nyr' `nobs'
capture program drop diag_post
program define diag_post
	syntax , APProach(string) SAMple(string) DATasource(string) UNIT(string) ///
	         NYears(integer) NObs(real) RESultsfile(string) ///
	         [ NCounties(string) NORig(string) NDest(string) ]
	preserve
		clear
		set obs 1
		gen str44 approach    = "`approach'"
		gen str44 sample      = "`sample'"
		gen str40 data_source = "`datasource'"
		gen str16 unit        = "`unit'"
		gen long  n_counties  = cond("`ncounties'" == "", ., real("`ncounties'"))
		gen long  n_orig      = cond("`norig'" == "", ., real("`norig'"))
		gen long  n_dest      = cond("`ndest'" == "", ., real("`ndest'"))
		gen int   n_years     = `nyears'
		gen long  n_obs       = `nobs'
		append using "`resultsfile'"
		save "`resultsfile'", replace
	restore
end

** Persist the results tempfile path to a real file so the helper can reopen it
tempfile resfile
save "`resfile'", replace emptyok


********************************************************************************
** SECTION 1: SDID -- counted from the prepared estimation file
**            (sdid_analysis_data.dta). These are the AUTHORITATIVE counts and
**            match Table 1 / Appendix Table A1 by construction.
********************************************************************************

capture confirm file "${data}working/sdid_analysis_data.dta"
if _rc != 0 {
	dis as error "WARNING: sdid_analysis_data.dta not found -- skipping SDID audit."
	dis as error "         Run 02_sdid_analysis.do first."
}
else {
	use "${data}working/sdid_analysis_data.dta", clear

	** County-level membership flags (a county is "in" a sample if any of its
	** years carries the indicator). These mirror the SDID spec selectors.
	bysort fips: egen byte in_irs1 = max(irs_sample_1 == 1)   // IRS, all counties
	bysort fips: egen byte in_irs2 = max(irs_sample_2 == 1)   // IRS, ACS-restricted
	bysort fips: egen byte in_acs1 = max(acs_period_1 == 1)   // ACS, 2016-2022
	bysort fips: egen byte in_acs2 = max(acs_period_2 == 1)   // ACS, 2016-2024

	** "All donor counties" pool = sample_all (excludes narrow-only keepers);
	** Multnomah is the treated unit and is added to each county set.
	** Loop over the four headline SDID samples.
	**   k : label | county-flag | year-indicator | data tag
	local lab1 "SDID donors+treated: IRS, all counties"
	local lab2 "SDID donors+treated: IRS, ACS-identified"
	local lab3 "SDID donors+treated: ACS 25+ (2016-2024)"
	local lab4 "SDID donors+treated: ACS college (2016-2024)"
	local flg1 in_irs1
	local flg2 in_irs2
	local flg3 in_acs2
	local flg4 in_acs2
	local ind1 irs_sample_1
	local ind2 irs_sample_2
	local ind3 acs_period_2
	local ind4 acs_period_2
	local dat1 "IRS"
	local dat2 "IRS (ACS counties)"
	local dat3 "ACS 25+"
	local dat4 "ACS college"

	forvalues k = 1/4 {
		preserve
			** donors + Multnomah, restricted to counties in this sample
			keep if (sample_all == 1 | multnomah == 1) & `flg`k'' == 1
			diag_distinct fips
			local nc = r(nd)
			diag_distinct fips if multnomah == 0
			local nc_don = r(nd)
			qui count if `ind`k'' == 1 & (sample_all == 1 | multnomah == 1)
			local nobs = r(N)
			qui distinct year if `ind`k'' == 1
			local nyr = r(ndistinct)
		restore
		diag_post, approach("`lab`k''") sample("All donors + Multnomah") ///
			datasource("`dat`k''") unit("county-year") ///
			ncounties("`nc'") nyears(`nyr') nobs(`nobs') resultsfile("`resfile'")
		dis "`lab`k'': `nc' counties (`nc_don' donors + Multnomah), `nobs' county-years"
	}

	** Narrow pool (20 similar cities + Multnomah), ACS-restricted variant.
	** County count, obs, and years all keyed on acs_period_2 so this single row
	** describes one sample (the ACS-balanced 2016-2024 narrow pool); previously
	** the county count used in_irs2 (the 2016-2022 IRS-ACS window), which did not
	** match the acs_period_2 obs/year counts in the same row.
	preserve
		keep if (sample_narrow == 1) & in_acs2 == 1
		diag_distinct fips
		local nc = r(nd)
		qui count if acs_period_2 == 1 & sample_narrow == 1
		local nobs = r(N)
		qui distinct year if acs_period_2 == 1
		local nyr = r(ndistinct)
	restore
	diag_post, approach("Narrow SDID: 20 cities + Multnomah") ///
		sample("Narrow pool (ACS-balanced)") datasource("IRS/ACS") ///
		unit("county-year") ncounties("`nc'") nyears(`nyr') nobs(`nobs') ///
		resultsfile("`resfile'")
}


********************************************************************************
** SECTION 2: SDID sample-construction FUNNEL
**            Replicates 02_sdid_analysis.do's construction and counts distinct
**            residence counties (fips) after each step. The final step is
**            validated against the from-file count above.
********************************************************************************

tempfile funnel
clear
gen int    step  = .
gen str60  label = ""
gen long   n_cty = .
save `funnel', emptyok

capture program drop funnel_post
program define funnel_post
	syntax , STep(integer) LABel(string) NCty(integer) FUNnelfile(string)
	preserve
		clear
		set obs 1
		gen int   step  = `step'
		gen str60 label = "`label'"
		gen long  n_cty = `ncty'
		append using "`funnelfile'"
		save "`funnelfile'", replace
	restore
end

** ACS reference sets (built once for the funnel) --------------------------
** acs_identified: counties appearing in the ACS 25+ panel (any year).
use "${data}working/acs_county_gross_25plus", clear
keep fips
duplicates drop
gen byte acs_identified = 1
tempfile acs_id
save `acs_id'

** acs_balanced: the county set the SDID estimation calls acs_period_2. The
** estimation (02_sdid_analysis.do:190-200) sets acs_period_2 by counting, per
** county, the years matched to the ACS 25+ panel AMONG the rows that survive the
** BEA merge, then keeping counties matched in every such year (ct == max). A
** county's 2023-2024 ACS rows therefore only count if it ALSO has BEA coverage
** those years, so a county balanced in the RAW ACS panel but missing 2023-2024
** BEA is NOT in acs_period_2. Reproduce that ACS-and-BEA-every-year rule here
** (demographics is time-invariant -> applied at Step 3; the covid/proptax/age
** merges keep non-matches, so they do not gate the panel) so the funnel endpoint
** reconciles with the estimation by construction, rather than approximating it
** with the raw-ACS balance (build_acs_balanced_set, used for the flow sample).
** If BEA covers every ACS county-year this is identical to the raw-ACS balance.
use "${data}working/acs_county_gross_25plus", clear
keep year fips
keep if inrange(year, 2016, 2024)
merge 1:1 year fips using "${data}working/bea_economics", keep(match) gen(_mbea)
bysort fips: gen _ny = _N
qui summ _ny
keep if _ny == r(max)              // ACS-and-BEA-matched every year = balanced
keep fips
duplicates drop
gen byte acs_balanced = 1
tempfile acs_bal
save `acs_bal'

** Step 1: IRS county-to-county file, analysis years, drop "other counties"
use "${data}working/irs_county_gross", clear
capture confirm variable fips
if _rc make_fips state_fips county_fips, gen(fips)
keep if inrange(year, 2016, 2022)
drop if county_fips == 0
diag_distinct fips
funnel_post, step(1) label("IRS county-to-county file (2016-2022)") ///
	ncty(`r(nd)') funnelfile(`funnel')

** Step 2: + matched to the ACS 25+ panel (county appears in ACS)
merge m:1 fips using `acs_id', keep(master match) gen(_macs)
diag_distinct fips if _macs == 3
funnel_post, step(2) label("+ matched to ACS 25+ panel") ///
	ncty(`r(nd)') funnelfile(`funnel')
keep if _macs == 3
drop _macs acs_identified

** Step 3: + matched to demographics_2020
merge m:1 fips using "${data}working/demographics_2020", gen(demo_merge) ///
	keep(master match)
keep if demo_merge == 3
diag_distinct fips
funnel_post, step(3) label("+ matched to demographics") ///
	ncty(`r(nd)') funnelfile(`funnel')

** Step 4: + matched to BEA economics
merge m:1 year fips using "${data}working/bea_economics", gen(econ_merge) ///
	keep(master match)
keep if econ_merge == 3
diag_distinct fips
funnel_post, step(4) label("+ matched to BEA economics") ///
	ncty(`r(nd)') funnelfile(`funnel')

** Step 5: + non-zero IRS base population + IRS-balanced (all 7 years 2016-2022)
drop if (missing(n1_out_1) | n1_out_1 == 0) & year <= 2022
bysort fips: egen ct_irs = total(inrange(year, 2016, 2022))
keep if ct_irs == 7
drop ct_irs
diag_distinct fips
funnel_post, step(5) label("+ non-zero base, IRS-balanced (2016-2022)") ///
	ncty(`r(nd)') funnelfile(`funnel')

** Step 6: + ACS-balanced = acs_period_2. acs_bal (built above) reproduces the
**         estimation's rule: ACS-25+ AND BEA-matched in every analysis year
**         2016-2024, so the 2023-2024 ACS rows are gated by BEA coverage exactly
**         as in 02_sdid_analysis.do. This is the SDID ACS estimation county set.
merge m:1 fips using `acs_bal', keep(master match) gen(_mbal)
keep if _mbal == 3
drop _mbal acs_balanced
diag_distinct fips
funnel_post, step(6) label("+ ACS-balanced panel (acs_period_2, 2016-2024)") ///
	ncty(`r(nd)') funnelfile(`funnel')

** Step 7: + state drops (AK, HI, CA, WA, non-Multnomah OR). Dropping ALL of
**         CA/WA/OR (except Multnomah) matches the SDID's sample_all donor pool,
**         which sets narrow-pool keepers (Sacramento/Seattle) to sample_all==0.
gen byte multnomah = state_fips == 41 & county_fips == 51
drop if inlist(state_fips, 2, 15)                       // Alaska, Hawaii
drop if inlist(state_fips, 6, 53) & multnomah == 0      // California, Washington
drop if state_fips == 41 & multnomah == 0               // non-Multnomah Oregon
diag_distinct fips
local funnel_final = r(nd)
funnel_post, step(7) label("+ state drops (= SDID ACS estimation sample)") ///
	ncty(`r(nd)') funnelfile(`funnel')

** Validate the funnel endpoint against the from-file count (Section 1, k=3).
capture confirm file "${data}working/sdid_analysis_data.dta"
if _rc == 0 {
	preserve
		use "${data}working/sdid_analysis_data.dta", clear
		bysort fips: egen byte in_acs2 = max(acs_period_2 == 1)
		keep if (sample_all == 1 | multnomah == 1) & in_acs2 == 1
		diag_distinct fips
		local truth = r(nd)
	restore
	dis ""
	dis "Funnel endpoint = `funnel_final' counties; estimation file = `truth'."
	if `funnel_final' != `truth' {
		dis as error "NOTE: funnel reconstruction (`funnel_final') differs from the"
		dis as error "      estimation file (`truth'). The from-file count is authoritative."
		dis as error "      The reconstruction now applies the estimation's ACS+BEA balance"
		dis as error "      rule, so any residual gap is a county in acs_period_2 but absent"
		dis as error "      from irs_county_gross (ACS-only) -- check before trusting Step 6."
	}
	else {
		dis "Funnel reconstruction matches the estimation file exactly."
	}
}


********************************************************************************
** SECTION 3: Flow analysis -- applies the same SAMPLE RESTRICTIONS as
**            02_flow_analysis.do (ACS set balanced 2016-2024; AK/HI dropped).
**            Reported overall and with origin/destination county counts, WITH
**            and WITHOUT 2020.
**            n_obs here is the count of OBSERVED flow-years (the reported IRS
**            rows). It deliberately does NOT reproduce the PPML estimation N:
**            the estimation xtset/tsfill,full zero-fills every county-pair-year,
**            keeps only movers (fips_o != fips_d), and inner-joins ids/bls/bea
**            coverage -- none of which changes the distinct-county counts that
**            are this audit's purpose.
********************************************************************************

** ACS county set: balanced over 2016-2024, via the shared helper
** (01a_programs.do) -- the same set the flow estimation and appendix
** descriptives use.
tempfile flow_acs_fips
build_acs_balanced_set, saving(`flow_acs_fips')

** IRS flows, analysis window, drop AK/HI both endpoints
use "${data}working/irs_county_flow", clear
keep if inrange(year, 2016, 2022)
drop if inlist(state_fips_o, 2, 15)
drop if inlist(state_fips_d, 2, 15)

** Tag flows whose BOTH endpoints are in the balanced ACS county set
gen fips = fips_o
merge m:1 fips using `flow_acs_fips', keep(master match) gen(_m_o)
drop fips
gen fips = fips_d
merge m:1 fips using `flow_acs_fips', keep(master match) gen(_m_d)
drop fips
gen byte acs_flow = (_m_o == 3 & _m_d == 3)

** Save the full (incl-2020) flow data; reload per version so the count helpers
** (which use preserve internally) are never called inside an enclosing preserve.
tempfile flowfull
save `flowfull'

** Loop: with 2020 (incl) and without 2020 (excl = primary)
foreach excl in 1 0 {
	use `flowfull', clear
	if `excl' == 1 qui drop if year == 2020
	local ylab = cond(`excl' == 1, "excl. 2020", "incl. 2020")

	** ---- All counties (non-AK/HI) ----
	diag_distinct_either
	local n_either = r(nd)
	diag_distinct fips_o
	local n_o = r(nd)
	diag_distinct fips_d
	local n_d = r(nd)
	qui count
	local nobs = r(N)
	qui distinct year
	local nyr = r(ndistinct)
	diag_post, approach("Flows: all counties (`ylab')") sample("All flows") ///
		datasource("IRS") unit("flow-year") ncounties("`n_either'") ///
		norig("`n_o'") ndest("`n_d'") nyears(`nyr') nobs(`nobs') ///
		resultsfile("`resfile'")

	** ---- ACS-restricted (both endpoints in ACS set) ----
	diag_distinct_either if acs_flow == 1
	local n_either = r(nd)
	diag_distinct fips_o if acs_flow == 1
	local n_o = r(nd)
	diag_distinct fips_d if acs_flow == 1
	local n_d = r(nd)
	qui count if acs_flow == 1
	local nobs = r(N)
	qui distinct year if acs_flow == 1
	local nyr = r(ndistinct)
	diag_post, approach("Flows: ACS-restricted (`ylab')") ///
		sample("Both endpoints in ACS set") datasource("IRS") unit("flow-year") ///
		ncounties("`n_either'") norig("`n_o'") ndest("`n_d'") ///
		nyears(`nyr') nobs(`nobs') resultsfile("`resfile'")
}


********************************************************************************
** SECTION 4: DiD -- mirrors 02_did_analysis.do (ACS individuals 25+,
**            2016-2024 excl. 2020, AK/HI + neg-income + qmigplc1==4 dropped).
**            Counties = distinct ORIGIN counties; obs = person-years.
********************************************************************************

use "${data}working/acs_migration_file", clear
drop if year < 2016
drop if year == 2020
keep if age >= 25
capture confirm variable qmigplc1
if !_rc drop if qmigplc1 == 4
drop if inlist(state_fips_o, 2, 15)
drop if inlist(state_fips_d, 2, 15)
capture confirm variable ftotinc
if !_rc drop if ftotinc < 0

gen byte multnomah_o = (state_fips_o == 41 & county_fips_o == 51)

** Sample 1: out-migration from Multnomah (origin = Multnomah)
preserve
	keep if multnomah_o == 1
	diag_distinct fips_o
	local nc = r(nd)
	qui count
	local nobs = r(N)
	qui distinct year
	local nyr = r(ndistinct)
restore
diag_post, approach("DiD: out-migration (Multnomah)") sample("Origin = Multnomah") ///
	datasource("ACS individual") unit("person-year") ncounties("`nc'") ///
	nyears(`nyr') nobs(`nobs') resultsfile("`resfile'")

** Sample 2: in-migration, West Coast origins (CA/OR/WA, non-Multnomah)
preserve
	keep if multnomah_o != 1 & inlist(state_fips_o, 6, 41, 53)
	diag_distinct fips_o
	local nc = r(nd)
	qui count
	local nobs = r(N)
	qui distinct year
	local nyr = r(ndistinct)
restore
diag_post, approach("DiD: in-migration (West Coast)") sample("Origins in CA/OR/WA") ///
	datasource("ACS individual") unit("person-year") ncounties("`nc'") ///
	nyears(`nyr') nobs(`nobs') resultsfile("`resfile'")

** Sample 3: in-migration, all non-Multnomah origins (Lower 48 + DC)
preserve
	keep if multnomah_o != 1
	diag_distinct fips_o
	local nc = r(nd)
	qui count
	local nobs = r(N)
	qui distinct year
	local nyr = r(ndistinct)
restore
diag_post, approach("DiD: in-migration (Lower 48)") sample("All non-Multnomah origins") ///
	datasource("ACS individual") unit("person-year") ncounties("`nc'") ///
	nyears(`nyr') nobs(`nobs') resultsfile("`resfile'")


********************************************************************************
** EXPORT: audit table
********************************************************************************

use "`resfile'", clear
drop if approach == ""
order approach sample data_source unit n_counties n_orig n_dest n_years n_obs

dis _n "==== County-count / observation audit ===="
list approach n_counties n_orig n_dest n_years n_obs, sep(0) noobs

capture mkdir "${results}"
capture mkdir "${results}tables"

export delimited using "${results}tables/diagnostics_obs_counts.csv", replace
export excel using "${results}tables/diagnostics_obs_counts.xlsx", ///
	firstrow(variables) replace

** LaTeX tabular fragment
tempname fh
file open `fh' using "${results}tables/diagnostics_obs_counts.tex", write replace
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

if ${overleaf} == 1 {
	copy "${results}tables/diagnostics_obs_counts.tex" ///
		"${ol_tab}diagnostics_obs_counts.tex", replace
}


********************************************************************************
** EXPORT: SDID sample-construction funnel
********************************************************************************

use `funnel', clear
drop if missing(step)
sort step

dis _n "==== SDID sample-construction funnel (residence counties) ===="
list step label n_cty, sep(0) noobs

export delimited using "${results}tables/sdid_sample_funnel.csv", replace

tempname fh2
file open `fh2' using "${results}tables/sdid_sample_funnel.tex", write replace
file write `fh2' "\begin{tabular}{clr}" _n
file write `fh2' "\toprule" _n
file write `fh2' "Step & Sample restriction & Counties \\" _n
file write `fh2' "\midrule" _n
local N = _N
forvalues i = 1/`N' {
	local st = step[`i']
	local lb = label[`i']
	local nc = n_cty[`i']
	file write `fh2' "`st' & `lb' & " %9.0fc (`nc') " \\" _n
}
file write `fh2' "\bottomrule" _n
file write `fh2' "\end{tabular}" _n
file close `fh2'

if ${overleaf} == 1 {
	copy "${results}tables/sdid_sample_funnel.tex" ///
		"${ol_tab}sdid_sample_funnel.tex", replace
}

dis _n "Diagnostics written to:"
dis "  ${results}tables/diagnostics_obs_counts.{tex,csv,xlsx}"
dis "  ${results}tables/sdid_sample_funnel.{tex,csv}"

clear
log close log_diag
