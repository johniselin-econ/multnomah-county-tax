/*******************************************************************************
File Name: 		02_did_placebo.do
Creator: 		John Iselin
Date Update:	November 10, 2026

Called by: 00_multnomah.do (after 02_sdid_analysis.do, which writes the
           Policy-match pool to resources/donor_pools.csv and builds
           data/working/sdid_analysis_data.dta)

Purpose: Placebo runs of the main individual-level DiD (College x Post,
         02_did_analysis.do). Each Policy-match donor county is treated as if
         it were Multnomah, if it passes two pre-2020 screens:
           (a) parallel college / non-college pre-trends within the county:
               its own out-migration event study's 2016-2018 College x Year
               coefficients are jointly insignificant
               (p > ${did_placebo_pretest_p});
           (b) migration trends like Multnomah's: its 2016-2019 IRS AGI out-
               and in-migration rate slopes are among the
               ${did_placebo_irs_keep} share of donors closest to Multnomah's.
         Multnomah's estimate is then ranked against the screened placebos
         (one-sided permutation p-value: larger out-migration, smaller
         in-migration). In-migration from the West Coast has no
         analogue for a non-Oregon county and is not run.

Outputs: results/did/did_placebo_results.csv
         results/did/did_placebo_es.csv (College x Year coefficients, every
           ACS-identified county; read by code/R/donor_onepagers.R)
         results/did/tab_did_placebo.tex
         results/did/fig_did_placebo.pdf
         results/did/fig_did_placebo_es.pdf (event studies vs placebos)

Authors: John Iselin

For more information, contact john.iselin@yale.edu

*******************************************************************************/


** Load shared project defaults and helper programs
if "${dir}" == "" {
    local _cwd = subinstr("`c(pwd)'", "\", "/", .)
    if regexm("`_cwd'", "(.*)/code/(stata|utils)$") global dir = regexs(1)
    else global dir "`_cwd'"
}
do "${dir}/code/utils/globals.do"

** Start log file
capture log close log_02
log using "${logs}02_log_did_placebo_${date}", replace text name(log_02)
project_set_seed, context("02_did_placebo.do") offset(65)

** plotplainblind palette (RGB), as in 02_did_analysis.do
local col_mult  "213 94 0"     // vermillion - Multnomah
local col_pass  "0 114 178"    // sea - screened placebos
local col_fail  "153 153 153"  // gs10 - placebos failing a screen
local col_ref   "153 153 153"  // reference lines

** Multnomah FIPS and the placebo outcomes (est name, outcome, sample, label)
local mult_fips 41051
local outcomes "out out_state in_48"

********************************************************************************
** STEP 1: CANDIDATE COUNTIES (Policy-match donors)
********************************************************************************

import delimited using "${dir}/resources/donor_pools.csv", clear varnames(1)
keep if sample_policy == 1 & fips != `mult_fips'
keep fips
qui count
if r(N) != ${policy_pool_n} {
	di as error "02_did_placebo: expected ${policy_pool_n} Policy-match donors, found `r(N)'"
	exit 459
}
levelsof fips, local(donor_fips)

********************************************************************************
** STEP 2: SCREEN (b) -- IRS PRE-2020 MIGRATION TRENDS
********************************************************************************
** Slopes of the IRS AGI out- and in-migration rates over 2016-2019, the same
** rates the SDID uses (02_sdid_analysis.do), then each donor's Euclidean
** distance from Multnomah's pair of slopes.

use fips state_fips state_name county_name year irs_sample_1 sample_policy ///
	multnomah agi_out_rate_irs agi_in_rate_irs ///
	using "${data}working/sdid_analysis_data", clear
keep if inrange(year, 2016, 2019) & irs_sample_1 == 1 & (sample_policy == 1 | multnomah == 1)

bysort fips: assert _N == 4		// balanced 2016-2019 IRS panel

foreach v in out in {
	bysort fips: egen _mx = mean(year)
	bysort fips: egen _my = mean(agi_`v'_rate_irs)
	gen _cxy = (year - _mx) * (agi_`v'_rate_irs - _my)
	gen _vxx = (year - _mx)^2
	bysort fips: egen _sxy = total(_cxy)
	bysort fips: egen _sxx = total(_vxx)
	gen irs_slope_`v' = _sxy / _sxx
	drop _mx _my _cxy _vxx _sxy _sxx
}

bysort fips (year): keep if _n == 1
keep fips state_fips state_name county_name multnomah irs_slope_out irs_slope_in

foreach v in out in {
	qui summ irs_slope_`v' if multnomah == 1
	assert r(N) == 1
	local mult_slope_`v' = r(mean)
}
gen irs_dist = sqrt((irs_slope_out - `mult_slope_out')^2 + (irs_slope_in - `mult_slope_in')^2)

local keep_pct = 100 * ${did_placebo_irs_keep}
_pctile irs_dist if multnomah == 0, p(`keep_pct')
gen pass_irs = irs_dist <= r(r1) if multnomah == 0
label var irs_slope_out "IRS AGI out-rate slope, 2016-2019 (pp/yr)"
label var irs_slope_in  "IRS AGI in-rate slope, 2016-2019 (pp/yr)"
label var irs_dist      "Distance from Multnomah's IRS slopes"
label var pass_irs      "Passes IRS trend screen"

tempfile irs_screen
save `irs_screen'

********************************************************************************
** STEP 3: PLACEBO DiDs (and screen (a))
********************************************************************************

did_load_acs, tag("02_did_placebo")

** Year x College interactions for the pre-trend test (base 2019, 2020 out)
foreach y in 2016 2017 2018 2021 2022 2023 2024 {
	gen x_high_ed_`y' = high_ed * (year == `y')
}

tempname pf
tempfile placebo_post
postfile `pf' double fips str12 outcome double(b se n_unwtd pre_p) byte acs_ok ///
	using `placebo_post', replace

** Event-study coefficients (College x Year, base 2019) for the figure
tempname pes
tempfile placebo_es
postfile `pes' double fips str12 outcome int year double(b se) using `placebo_es', replace

foreach c in `mult_fips' `donor_fips' {

	did_define_focal, fips(`c')

	** A county the ACS never identifies as an origin cannot be run
	qui count if sample_1 == 1
	if r(N) == 0 {
		di as text "02_did_placebo: county `c' is not ACS-identified; skipped"
		foreach o of local outcomes {
			post `pf' (`c') ("`o'") (.) (.) (.) (.) (0)
		}
		continue
	}

	** Screen (a): own out-migration event study, 2016-2018 pre-coefficients
	qui reghdfe out_1 x_high_ed_* if sample_1 == 1 [pw = perwt], 	///
		vce(robust) absorb(year cat_*)
	qui test x_high_ed_2016 x_high_ed_2017 x_high_ed_2018
	local pre_p = r(p)
	foreach y in 2016 2017 2018 2021 2022 2023 2024 {
		post `pes' (`c') ("out") (`y') (_b[x_high_ed_`y']) (_se[x_high_ed_`y'])
	}

	** Out-of-state event study (figure only; not part of the screen)
	qui reghdfe out_3 x_high_ed_* if sample_1 == 1 [pw = perwt], 	///
		vce(robust) absorb(year cat_*)
	foreach y in 2016 2017 2018 2021 2022 2023 2024 {
		post `pes' (`c') ("out_state") (`y') (_b[x_high_ed_`y']) (_se[x_high_ed_`y'])
	}

	** Main DiD, as 02_did_analysis.do Regression 1, for this focal county
	foreach o of local outcomes {
		if "`o'" == "out" {
			qui reghdfe out_1 treated if sample_1 == 1 [pw = perwt], 	///
				vce(robust) absorb(year cat_*)
		}
		else if "`o'" == "out_state" {
			qui reghdfe out_3 treated if sample_1 == 1 [pw = perwt], 	///
				vce(robust) absorb(year cat_*)
		}
		else if "`o'" == "in_48" {
			qui reghdfe out_2 treated if sample_3 == 1 [pw = perwt], 	///
				vce(cluster fips_o) absorb(year fips_o cat_*)
		}
		qui count if e(sample)
		post `pf' (`c') ("`o'") (_b[treated]) (_se[treated]) (r(N)) (`pre_p') (1)
		di as text "  `c' `o': b = " %7.3f _b[treated] "  se = " %6.3f _se[treated] "  pre-trend p = " %5.3f `pre_p'
	}
}
postclose `pf'
postclose `pes'


********************************************************************************
** STEP 4: CHECK THE MULTNOMAH ROW AGAINST THE MAIN DiD
********************************************************************************
** The focal-county code path must reproduce 02_did_analysis.do Regression 1
** (results/did/did_coefficients.dta), or the placebos are not comparable.

use "${results}did/did_coefficients.dta", clear
keep if inlist(outcome, "did_out", "did_out_state", "did_in_48")
gen str12 outcome_short = substr(outcome, 5, .)
keep outcome_short b se
rename (outcome_short b se) (outcome b_main se_main)
tempfile main_coefs
save `main_coefs'

use `placebo_post', clear
merge m:1 outcome using `main_coefs', keep(master match)
assert _merge == 3 if fips == `mult_fips'
assert reldif(b, b_main) < 1e-6 & reldif(se, se_main) < 1e-6 if fips == `mult_fips'
drop _merge b_main se_main

********************************************************************************
** STEP 5: SCREENS AND PERMUTATION P-VALUES
********************************************************************************

reshape wide b se n_unwtd, i(fips) j(outcome) string
merge 1:1 fips using `irs_screen', keep(master match)
assert _merge == 3
drop _merge

gen pass_pretrend = pre_p > ${did_placebo_pretest_p} if acs_ok == 1 & fips != `mult_fips'
gen in_screen = acs_ok == 1 & pass_pretrend == 1 & pass_irs == 1 if fips != `mult_fips'
** NJ and NY raised top income-tax rates on millionaires in 2020-2021
gen nj_ny = inlist(state_fips, 34, 36)
label var pass_pretrend "Passes within-county pre-trend screen"
label var in_screen     "Placebo used (ACS-identified, passes both screens)"
label var nj_ny         "New Jersey or New York county"

qui count if acs_ok == 1 & fips != `mult_fips'
local n_candidates = r(N)
qui count if in_screen == 1
local n_screened = r(N)
qui count if in_screen == 1 & nj_ny == 0
local n_screened_nonjny = r(N)
if `n_screened' == 0 {
	di as error "02_did_placebo: no donor county passes both screens"
	exit 459
}

project_log_number, key(did_placebo_n_candidates) value(`n_candidates') fmt(%3.0f) unit(counties) ///
	source(02_did_placebo.do) note("Policy-match donors identified as ACS origin counties (placebo candidates)")
project_log_number, key(did_placebo_n_screened) value(`n_screened') fmt(%3.0f) unit(counties) ///
	source(02_did_placebo.do) note("Placebo counties passing both pre-2020 screens")

** One-sided permutation p-value in the hypothesized direction of the tax:
** share of placebos (plus Multnomah) with an estimate at least as large as
** Multnomah's for out-migration and out-of-state moves (college graduates
** leaving), at least as negative for in-migration (college graduates not
** arriving). With N placebos the smallest attainable value is 1/(N+1).
foreach o of local outcomes {
	qui summ b`o' if fips == `mult_fips'
	local b_mult = r(mean)
	local tail = cond("`o'" == "in_48", "<=", ">=")
	foreach set in screened all nonjny {
		if "`set'" == "screened" local cond "in_screen == 1"
		if "`set'" == "all"      local cond "acs_ok == 1 & fips != `mult_fips'"
		if "`set'" == "nonjny"   local cond "in_screen == 1 & nj_ny == 0"
		qui count if `cond'
		local n_set = r(N)
		qui count if `cond' & b`o' `tail' `b_mult'
		local p_perm = (1 + r(N)) / (1 + `n_set')
		local p_`o'_`set' = `p_perm'
		project_log_number, key(did_placebo_perm_p_`o'_`set') value(`p_perm') fmt(%4.2f) unit(p) ///
			source(02_did_placebo.do) note("One-sided permutation p-value, `o' DiD, Multnomah vs `set' placebos (`n_set' counties)")
	}
}

** Results file (all counties, flags, estimates)
order fips state_name county_name acs_ok pre_p pass_pretrend irs_slope_out irs_slope_in ///
	irs_dist pass_irs in_screen nj_ny b* se* n_unwtd*
gsort -multnomah -in_screen county_name
export delimited using "${results}did/did_placebo_results.csv", replace

** Event-study coefficients, all ACS-identified counties (base 2019, 2020 out)
preserve
	use `placebo_es', clear
	sort fips outcome year
	export delimited using "${results}did/did_placebo_es.csv", replace
restore

********************************************************************************
** STEP 6: TABLE AND FIGURE
********************************************************************************

** Table: Multnomah, screened placebos, then counties failing a screen
gen str80 cty = subinstr(county_name, " County", "", 1) + ", " + state_name
replace cty = subinstr(cty, "&", "\&", .)
gen byte grp = cond(fips == `mult_fips', 1, cond(in_screen == 1, 2, 3))
sort grp county_name

tempname fh
file open `fh' using "${results}did/tab_did_placebo.tex", write text replace
file write `fh' "\begin{tabular*}{\textwidth}{@{\extracolsep{\fill}}lccccc@{}}" _n
file write `fh' "\toprule" _n
file write `fh' " & Pre-trend & IRS trend & \shortstack{Out-\\migration} & \shortstack{Out-of-\\state} & \shortstack{In-migration\\(Lower 48 + DC)} \\" _n
file write `fh' " & \$p\$-value & distance & & & \\" _n
file write `fh' "\midrule" _n
local heads `" "" "\multicolumn{6}{l}{\textit{Placebo counties passing both screens}}\\[0.3em]" "\multicolumn{6}{l}{\textit{Counties failing a screen}}\\[0.3em]" "'
forvalues g = 1/3 {
	if `g' > 1 {
		local h : word `g' of `heads'
		file write `fh' "\addlinespace" _n "`h'" _n
	}
	forvalues i = 1/`=_N' {
		if grp[`i'] != `g' continue
		local row = cty[`i']
		if acs_ok[`i'] == 0 {
			file write `fh' "`row' & \multicolumn{5}{c}{\textit{not identified in the ACS}} \\" _n
			continue
		}
		local pp : di %4.2f pre_p[`i']
		local dd : di %4.2f irs_dist[`i']
		if `g' == 1 local dd "--"
		local cells ""
		foreach o of local outcomes {
			local bb : di %6.3f b`o'[`i']
			local ss : di %5.3f se`o'[`i']
			local cells "`cells' & `=strtrim("`bb'")' (`=strtrim("`ss'")')"
		}
		file write `fh' "`row' & `pp' & `dd'`cells' \\" _n
	}
}
file write `fh' "\midrule" _n
foreach set in screened all {
	if "`set'" == "screened" local lab "One-sided permutation \textit{p}-value, screened placebos"
	if "`set'" == "all"      local lab "One-sided permutation \textit{p}-value, all placebos"
	local cells ""
	foreach o of local outcomes {
		local cells "`cells' & `: di %4.2f `p_`o'_`set'''"
	}
	file write `fh' "`lab' & & `cells' \\" _n
}
file write `fh' "\bottomrule" _n "\end{tabular*}" _n
file close `fh'
project_save_overleaf "${results}did/tab_did_placebo.tex"

** Figure: placebo estimates by outcome, Multnomah marked
local graphs ""
local k = 0
foreach o of local outcomes {
	local ++k
	if "`o'" == "out"       local ttl "Out-migration"
	if "`o'" == "out_state" local ttl "Out-of-state"
	if "`o'" == "in_48"     local ttl "In-migration (Lower 48 + DC)"
	local leg `"legend(off)"'
	if `k' == 2 local leg `"legend(order(6 "Multnomah" 4 "Screened placebo" 2 "Fails a screen") rows(1) pos(6))"'
	preserve
		keep if acs_ok == 1
		sort b`o'
		gen rank = _n
		gen lo = b`o' - 1.96 * se`o'
		gen hi = b`o' + 1.96 * se`o'
		twoway (rcap lo hi rank if grp == 3, horizontal lc("`col_fail'")) ///
			(scatter rank b`o' if grp == 3, mc("`col_fail'") ms(Oh)) ///
			(rcap lo hi rank if grp == 2, horizontal lc("`col_pass'")) ///
			(scatter rank b`o' if grp == 2, mc("`col_pass'") ms(O)) ///
			(rcap lo hi rank if grp == 1, horizontal lc("`col_mult'")) ///
			(scatter rank b`o' if grp == 1, mc("`col_mult'") ms(D)), ///
			xline(0, lc("`col_ref'") lp(dash)) ylabel(none) ytitle("") ///
			xtitle("College × Post (pp)") title("`ttl'", size(medium)) ///
			`leg' ///
			graphregion(color(white)) plotregion(color(white)) name(g`k', replace)
	restore
	local graphs "`graphs' g`k'"
}
graph combine `graphs', rows(1) graphregion(color(white)) xsize(12) ysize(5)
graph export "${results}did/fig_did_placebo.pdf", replace
project_save_overleaf "${results}did/fig_did_placebo.pdf"

** Figure: event studies, Multnomah against the screened placebos
keep fips grp
tempfile groups
save `groups'
use `placebo_es', clear
merge m:1 fips using `groups', keep(match) nogen
keep if grp <= 2								// Multnomah and screened placebos
** Base year 2019 = 0 for every county and outcome
preserve
	keep fips outcome grp
	duplicates drop
	gen year = 2019
	gen b = 0
	gen se = 0
	tempfile base
	save `base'
restore
append using `base'
gen lo = b - 1.96 * se if grp == 1
gen hi = b + 1.96 * se if grp == 1
sort fips outcome year

qui levelsof fips if grp == 2, local(screened_fips)
local graphs ""
local k = 0
foreach o in out out_state {
	local ++k
	if "`o'" == "out"       local ttl "Out-migration"
	if "`o'" == "out_state" local ttl "Out-of-state"
	local lines ""
	foreach f of local screened_fips {
		local lines `"`lines' (line b year if fips == `f' & outcome == "`o'" & year < 2020, lc("`col_pass'%50") lw(thin))"'
		local lines `"`lines' (line b year if fips == `f' & outcome == "`o'" & year > 2020, lc("`col_pass'%50") lw(thin))"'
	}
	local leg `"legend(off)"'
	if `k' == 2 local leg `"legend(order(1 "Screened placebo" `=2*`: word count `screened_fips'' + 2' "Multnomah") rows(1) pos(6))"'
	twoway `lines' ///
		(rcap lo hi year if grp == 1 & outcome == "`o'", lc("`col_mult'")) ///
		(connected b year if grp == 1 & outcome == "`o'" & year < 2020, lc("`col_mult'") mc("`col_mult'") ms(D)) ///
		(connected b year if grp == 1 & outcome == "`o'" & year > 2020, lc("`col_mult'") mc("`col_mult'") ms(D)), ///
		yline(0, lc("`col_ref'") lp(dash)) xline(2020.5, lc(black) lp(solid)) ///
		xlabel(2016(1)2024) xtitle("Year") ytitle("College × Year (pp)") ///
		title("`ttl'", size(medium)) `leg' ///
		graphregion(color(white)) plotregion(color(white)) name(e`k', replace)
	local graphs "`graphs' e`k'"
}
graph combine `graphs', rows(1) graphregion(color(white)) xsize(10) ysize(4.5)
graph export "${results}did/fig_did_placebo_es.pdf", replace
project_save_overleaf "${results}did/fig_did_placebo_es.pdf"

********************************************************************************
** CLEANUP
********************************************************************************

clear
log close log_02
