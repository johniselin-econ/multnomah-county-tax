/*******************************************************************************
File Name: 		02_did_analysis.do
Creator: 		John Iselin
Date Update:	January 11, 2026

Called by: 00_multnomah.do

Purpose: Perform DiD analysis on ACS data examining the effect of the
         Multnomah County tax on migration patterns by education level.
         Main text: College x Post DiD and event study (tab_did_main.tex,
         fig_es_*_migration.png). DiD appendix: heterogeneity by age, own
         children, marital status, and WFH potential
         (tab_did_heterogeneity.tex) and the by-age event studies.
         Placebo counties: 02_did_placebo.do.

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
log using "${logs}02_log_did_${date}", replace text name(log_02)
project_set_seed, context("02_did_analysis.do") offset(60)


** plotplainblind palette (RGB) — consistent across all figures
local col_out   "0 114 178"    // sea (p7) — out-migration
local col_in    "213 94 0"     // vermillion (p6) — in-migration
local col_green "86 180 233"   // sky blue - ordered age palette middle tone
local col_state "204 121 167"  // reddish (p5) — out-of-state
local col_mult  "0 68 136"     // dark blue - ordered age palette oldest group
local col_ref   "153 153 153"  // gs10 (p2) — reference lines
local col_age1  "158 202 225"  // light blue - age 25-44
local col_age2  "`col_green'"  // medium blue - age 45-64
local col_age3  "`col_mult'"   // dark blue - age 65+


********************************************************************************
** LOAD AND PREPARE DATA
********************************************************************************

** Load the ACS migration file with the DiD sample restrictions, treatment,
** and covariate cells (programs.do: did_load_acs, create_covariates)
did_load_acs, tag("02_did_analysis")

********************************************************************************
** DEFINE SAMPLES AND OUTCOMES
********************************************************************************

** Multnomah as the focal county: sample_1, sample_3, out_1-out_3
** (programs.do: did_define_focal)
did_define_focal, fips(41051)

** West Coast in-migration sample (Multnomah-specific; globals.do)
project_flag_states state_fips_o, states(${indiv_inmig_origin_states}) generate(inmig_origin)
gen sample_2 = (focal_o != 1) & 				/// Not Multnomah in origin
				year != 2020 &						///
			   inmig_origin == 1  					// West Coast in-migration
drop inmig_origin
label var sample_2 "In-migration Sample (West Coast)"
********************************************************************************
** REGRESSION 1: DIFFERENCE-IN-DIFFERENCES
********************************************************************************

** Store estimates for coefficient plot
estimates clear

** Out-migration regression (Sample 1: Multnomah residents)
reghdfe out_1 treated if sample_1 == 1 [pw = perwt], 	///
	vce(robust) absorb(year cat_*)
qui count if e(sample)
estadd scalar N_unwtd = r(N)
estimates store did_out

** In-migration regression (Sample 2: CA/OR/WA residents)
reghdfe out_2 treated if sample_2 == 1 [pw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_*)
qui count if e(sample)
estadd scalar N_unwtd = r(N)
estimates store did_in_west

** In-migration regression (Sample 3: Lower 48 + DC residents)
reghdfe out_2 treated if sample_3 == 1 [pw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_*)
qui count if e(sample)
estadd scalar N_unwtd = r(N)
estimates store did_in_48

** Out-of-state migration regression (Sample 1: Multnomah residents)
reghdfe out_3 treated if sample_1 == 1 [pw = perwt], 	///
	vce(robust) absorb(year cat_*)
qui count if e(sample)
estadd scalar N_unwtd = r(N)
estimates store did_out_state

********************************************************************************
** MAIN TABLE: DiD RESULTS (College Degree x Post, Multnomah)
********************************************************************************

esttab did_out did_out_state did_in_west did_in_48 						///
	using "${results}did/tab_did_main.tex",								///
	replace 															///
	starlevel("*" 0.10 "**" 0.05 "***" 0.01)							///
	b(%-9.3f) se(%-9.3f) 												///
	keep(treated) 														///
	coeflabels(treated  "College Degree $\times$ Post")					///
	mtitle(	"\shortstack{Out-\\migration}" 								///
			"\shortstack{Out-of-\\state}"								///
			"\shortstack{In-migration\\(West Coast)}" 					///
			"\shortstack{In-migration\\(Lower 48 + DC)}")				///
	prehead("\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" 				///
			"\begin{tabular*}{\textwidth}{@{\extracolsep{\fill}}l*{4}{c}@{}}" 	///
			"\toprule")													///
	posthead("\midrule")												///
	prefoot("\midrule") 												///
	stats(N_unwtd, fmt(%12.0fc) labels("Observations"))					///
	postfoot("\bottomrule" 												///
			 "\end{tabular*}")

project_save_overleaf "${results}did/tab_did_main.tex"
********************************************************************************
** EXPORT DiD COEFFICIENTS FOR ELASTICITY CALCULATION (consumed in Stage 4)
********************************************************************************
** The four College x Post coefficients (in percentage points -- ACS migration
** outcomes are scaled x100) feed the DiD-implied semi-elasticities computed in
** 02_post_spec.do, where the microsimulation-derived net-of-tax denominator
** (delta_ln_ntr_total_college_shs) is available. We export here because these
** regression estimates live only in this script's memory.
tempname didpost
postfile `didpost' str20 outcome double(b se n) ///
	using "${results}did/did_coefficients.dta", replace
foreach est in did_out did_out_state did_in_west did_in_48 {
	estimates restore `est'
	post `didpost' ("`est'") (_b[treated]) (_se[treated]) (e(N))
}
postclose `didpost'
dis as txt "Saved DiD College x Post coefficients to ${results}did/did_coefficients.dta"

********************************************************************************
** REGRESSION 2: EVENT STUDY
********************************************************************************

** Create year × education interactions (base year = 2019, 2020 excluded)
foreach y in 2016 2017 2018 2021 2022 2023 2024 {
	gen x_high_ed_`y' = high_ed * (year == `y')
}

** Out-migration event study
reghdfe out_1 x_high_ed_* if sample_1 == 1 [pw = perwt], 	///
	vce(robust) absorb(year cat_*)
estimates store es_out

** In-migration event study
reghdfe out_2 x_high_ed_* if sample_2 == 1 [pw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_*)
estimates store es_in_west

** In-migration event study
reghdfe out_2 x_high_ed_* if sample_3 == 1 [pw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_*)
estimates store es_in_48

** Out-of-state migration event study
reghdfe out_3 x_high_ed_* if sample_1 == 1 [pw = perwt], 	///
	vce(robust) absorb(year cat_*)
estimates store es_out_state

********************************************************************************
** EVENT STUDY PLOTS
********************************************************************************

** Extract coefficients and create event study dataset
preserve
	clear
	set obs 9
	gen year = 2015 + _n

	** Out-migration coefficients
	gen out_coef = .
	gen out_ci_lo = .
	gen out_ci_hi = .

	** In-migration coefficients
	gen in_west_coef = .
	gen in_west_ci_lo = .
	gen in_west_ci_hi = .
	
	** In-migration coefficients
	gen in_48_coef = .
	gen in_48_ci_lo = .
	gen in_48_ci_hi = .

	** Out-of-state migration coefficients
	gen out_state_coef = .
	gen out_state_ci_lo = .
	gen out_state_ci_hi = .

	** Fill in coefficients (2020 = base year = 0)
	** `capture` wraps each _b[] lookup because not every year is estimable
	** in every spec (e.g. out-of-sample years or dropped categories); a
	** missing coefficient should leave the cell at its initialized value.
	foreach y in 2016 2017 2018 2021 2022 2023 2024 {

		** Out-migration
		estimates restore es_out
		capture {
			replace out_coef = _b[x_high_ed_`y'] if year == `y'
			replace out_ci_lo = _b[x_high_ed_`y'] - 1.96 * _se[x_high_ed_`y'] if year == `y'
			replace out_ci_hi = _b[x_high_ed_`y'] + 1.96 * _se[x_high_ed_`y'] if year == `y'
		}

		** In-migration
		estimates restore es_in_west
		capture {
			replace in_west_coef = _b[x_high_ed_`y'] if year == `y'
			replace in_west_ci_lo = _b[x_high_ed_`y'] - 1.96 * _se[x_high_ed_`y'] if year == `y'
			replace in_west_ci_hi = _b[x_high_ed_`y'] + 1.96 * _se[x_high_ed_`y'] if year == `y'
		}
		
		** In-migration
		estimates restore es_in_48
		capture {
			replace in_48_coef = _b[x_high_ed_`y'] if year == `y'
			replace in_48_ci_lo = _b[x_high_ed_`y'] - 1.96 * _se[x_high_ed_`y'] if year == `y'
			replace in_48_ci_hi = _b[x_high_ed_`y'] + 1.96 * _se[x_high_ed_`y'] if year == `y'
		}

		** Out-of-state migration
		estimates restore es_out_state
		capture {
			replace out_state_coef = _b[x_high_ed_`y'] if year == `y'
			replace out_state_ci_lo = _b[x_high_ed_`y'] - 1.96 * _se[x_high_ed_`y'] if year == `y'
			replace out_state_ci_hi = _b[x_high_ed_`y'] + 1.96 * _se[x_high_ed_`y'] if year == `y'
		}

	}

	** Base year (2019) = 0; 2020 excluded (no coefficient)
	foreach v in out_coef out_ci_lo out_ci_hi in_west_coef in_west_ci_lo in_west_ci_hi in_48_coef in_48_ci_lo in_48_ci_hi out_state_coef out_state_ci_lo out_state_ci_hi {
		replace `v' = 0 if year == 2019
	}

	** Register the post-PFA coefficient range quoted in Section 5.3 (paper_numbers.csv)
	qui summ out_coef if year >= 2021
	local es_min = r(min)
	local es_max = r(max)
	project_log_number, key(did_es_out_college_post_min) value(`es_min') fmt(%4.1f) unit(pp) ///
		source(02_did_analysis.do) note("Smallest College x Year coefficient, out-migration event study, 2021-2024 (Figure 8 Panel A)")
	project_log_number, key(did_es_out_college_post_max) value(`es_max') fmt(%4.1f) unit(pp) ///
		source(02_did_analysis.do) note("Largest College x Year coefficient, out-migration event study, 2021-2024 (Figure 8 Panel A)")

	** Offset years slightly for visibility
	gen year_out = year - 0.2
	gen year_out_state = year - 0.07
	gen year_in_west = year + 0.07
	gen year_in_48 = year + 0.2

	** Plot 1: Out-migration event study
	twoway 	(rcap out_ci_lo out_ci_hi year, lc("`col_out'")) 				///
			(scatter out_coef year, mc("`col_out'") ms(O)),		///
		yline(0, lc("`col_ref'") lp(dash)) 								///
		xline(2020.5, lc(black) lp(solid))							///
		xlabel(2016(1)2024) 										///
		ytitle("Effect on Out-Migration (pp)") 						///
		xtitle("Year")												///
		legend(off) 												///
		graphregion(color(white)) plotregion(color(white))

	graph export "${results}did/fig_es_out_migration.png", replace
	project_save_overleaf "${results}did/fig_es_out_migration.png"

	** Plot 2: In-migration event study (West Coast)
	twoway 	(rcap in_west_ci_lo in_west_ci_hi year, lc("`col_in'")) 	///
			(scatter in_west_coef year, mc("`col_in'") ms(O)),	///
		yline(0, lc("`col_ref'") lp(dash)) 								///
		xline(2020.5, lc(black) lp(solid))							///
		xlabel(2016(1)2024) 										///
		ytitle("Effect on In-Migration (pp)") 						///
		xtitle("Year")												///
		legend(off) 												///
		graphregion(color(white)) plotregion(color(white))

	graph export "${results}did/fig_es_in_migration_west.png", replace
	project_save_overleaf "${results}did/fig_es_in_migration_west.png"

	** Plot 3: In-migration event study (Lower 48 + DC)
	twoway 	(rcap in_48_ci_lo in_48_ci_hi year, lc("`col_green'")) 	///
			(scatter in_48_coef year, mc("`col_green'") ms(O)),	///
		yline(0, lc("`col_ref'") lp(dash)) 								///
		xline(2020.5, lc(black) lp(solid))							///
		xlabel(2016(1)2024) 										///
		ytitle("Effect on In-Migration (pp)") 						///
		xtitle("Year")												///
		legend(off) 												///
		graphregion(color(white)) plotregion(color(white))

	graph export "${results}did/fig_es_in_migration_48.png", replace
	project_save_overleaf "${results}did/fig_es_in_migration_48.png"

	** Plot 4: Out-of-state migration event study
	twoway 	(rcap out_state_ci_lo out_state_ci_hi year, lc("`col_state'")) 		///
			(scatter out_state_coef year, mc("`col_state'") ms(D)),				///
		yline(0, lc("`col_ref'") lp(dash)) 										///
		xline(2020.5, lc(black) lp(solid))									///
		xlabel(2016(1)2024) 												///
		ytitle("Effect on Out-of-State Migration (pp)") 					///
		xtitle("Year")														///
		legend(off) 														///
		graphregion(color(white)) plotregion(color(white))

	graph export "${results}did/fig_es_out_state_migration.png", replace
	project_save_overleaf "${results}did/fig_es_out_state_migration.png"

	** Plot 5: Combined event study
	twoway 	(rcap out_ci_lo out_ci_hi year_out, lc("`col_out'")) 					///
			(scatter out_coef year_out, mc("`col_out'") ms(O)) 			///
			(rcap out_state_ci_lo out_state_ci_hi year_out_state, lc("`col_state'")) 	///
			(scatter out_state_coef year_out_state, mc("`col_state'") ms(D))	///
			(rcap in_west_ci_lo in_west_ci_hi year_in_west, lc("`col_in'")) 	///
			(scatter in_west_coef year_in_west, mc("`col_in'") ms(T))	///
			(rcap in_48_ci_lo in_48_ci_hi year_in_48, lc("`col_green'")) 	///
			(scatter in_48_coef year_in_48, mc("`col_green'") ms(S)),	///
		yline(0, lc("`col_ref'") lp(dash)) 										///
		xline(2020.5, lc(black) lp(solid))									///
		xlabel(2016(1)2024) 												///
		ytitle("Effect on Migration Rate (pp)") 							///
		xtitle("Year")														///
		legend(order(2 "Out-migration" 4 "Out-of-state" 					///
			6 "In-migration (West Coast)" 									///
			8 "In-migration (Lower 48 + DC)") pos(6) rows(1)) 				///
		graphregion(color(white)) plotregion(color(white))

	graph export "${results}did/fig_es_combined.png", replace
	project_save_overleaf "${results}did/fig_es_combined.png"

restore

********************************************************************************
** REGRESSION 3: TREATMENT HETEROGENEITY (DiD APPENDIX)
********************************************************************************
** College x Post x group triple differences, one dimension at a time: age,
** own children in the household, marital status, and work-from-home (WFH)
** potential. Each adds year x group and college x group fixed effects to the
** main specification, so treated_<stub>_<g> is the College x Post effect for
** group g. p_equal tests equal effects across the dimension's groups.
** Group variables avoid the cat_ prefix: absorb(cat_*) would pick them up and
** change the main specification.

** Own children in the household (IPUMS NCHILD: own children present)
gen het_kids = nchild > 0
label define lb_het_kids 0 "No own children" 1 "Own children in household", replace
label values het_kids lb_het_kids

** Married, spouse present (cat_married category 1)
gen het_married = marst == 1
label define lb_het_married 0 "Not married" 1 "Married", replace
label values het_married lb_het_married

** WFH potential: Dingel-Neiman teleworkability of the respondent's occupation
** (resources/wfh_teleworkable_occ.csv; ${wfh_share_cutoff} in globals.do)
**   1 teleworkable, 2 not teleworkable, 3 no occupation (IPUMS OCC 0 = not
**   applicable; 9920 = unemployed, no work in the last 5 years)
confirm variable occ
gen occ_vintage = cond(year <= 2017, 2010, 2018)	// ACS 2018+ uses 2018 Census codes
preserve
	import delimited using "${wfh_xwalk_file}", clear varnames(1) case(lower)
	keep occ_vintage occ teleworkable_share
	tempfile wfh_xwalk
	save `wfh_xwalk'
restore
merge m:1 occ_vintage occ using `wfh_xwalk', keep(master match) gen(wfh_merge)
gen no_occ = inlist(occ, 0, 9920)
** Every reported occupation must be in the crosswalk (fail loudly otherwise)
qui count if no_occ == 0 & wfh_merge != 3
local n_unmatched = r(N)
if `n_unmatched' > 0 {
	tab occ year if no_occ == 0 & wfh_merge != 3
	di as error "02_did_analysis: `n_unmatched' person-years with an occupation missing from ${wfh_xwalk_file}"
	exit 459
}
gen het_wfh = cond(no_occ == 1, 3, cond(teleworkable_share >= ${wfh_share_cutoff}, 1, 2))
label define lb_het_wfh 1 "Teleworkable" 2 "Not teleworkable" 3 "No occupation", replace
label values het_wfh lb_het_wfh
drop wfh_merge no_occ

capture program drop did_het_triple
program define did_het_triple
	syntax varname(numeric), STUB(name)

	local grp `varlist'
	qui levelsof `grp' if sample_1 == 1 | sample_3 == 1, local(groups)
	local texpr ""
	foreach g of local groups {
		gen treated_`stub'_`g' = treated * (`grp' == `g')
		local texpr = cond("`texpr'" == "", "treated_`stub'_`g'", "`texpr' = treated_`stub'_`g'")
	}
	local fe_het "year#`grp' high_ed#`grp'"

	** Out-migration (Sample 1: Multnomah residents)
	reghdfe out_1 treated_`stub'_* if sample_1 == 1 [pw = perwt], 	///
		vce(robust) absorb(year cat_* `fe_het')
	qui count if e(sample)
	estadd scalar N_unwtd = r(N)
	qui test `texpr'
	estadd scalar p_equal = r(p)
	estimates store did_`stub'_out

	** In-migration (Sample 2: CA/OR/WA residents)
	reghdfe out_2 treated_`stub'_* if sample_2 == 1 [pw = perwt], 	///
		vce(cluster fips_o) absorb(year fips_o cat_* `fe_het')
	qui count if e(sample)
	estadd scalar N_unwtd = r(N)
	qui test `texpr'
	estadd scalar p_equal = r(p)
	estimates store did_`stub'_in_west

	** In-migration (Sample 3: Lower 48 + DC residents)
	reghdfe out_2 treated_`stub'_* if sample_3 == 1 [pw = perwt], 	///
		vce(cluster fips_o) absorb(year fips_o cat_* `fe_het')
	qui count if e(sample)
	estadd scalar N_unwtd = r(N)
	qui test `texpr'
	estadd scalar p_equal = r(p)
	estimates store did_`stub'_in_48

	** Out-of-state migration (Sample 1: Multnomah residents)
	reghdfe out_3 treated_`stub'_* if sample_1 == 1 [pw = perwt], 	///
		vce(robust) absorb(year cat_* `fe_het')
	qui count if e(sample)
	estadd scalar N_unwtd = r(N)
	qui test `texpr'
	estadd scalar p_equal = r(p)
	estimates store did_`stub'_out_state
end

did_het_triple cat_age,     stub(age)
did_het_triple het_kids,    stub(kids)
did_het_triple het_married, stub(married)
did_het_triple het_wfh,     stub(wfh)

********************************************************************************
** APPENDIX TABLE: DiD HETEROGENEITY (Panels A-D in single file)
********************************************************************************

local het_cols `" "\shortstack{Out-\\migration}" "\shortstack{Out-of-\\state}" "\shortstack{In-migration\\(West Coast)}" "\shortstack{In-migration\\(Lower 48 + DC)}" "'

** Panel A: Age (creates file with full header)
esttab did_age_out did_age_out_state did_age_in_west did_age_in_48 		///
	using "${results}did/tab_did_heterogeneity.tex",					///
	replace 															///
	starlevel("*" 0.10 "**" 0.05 "***" 0.01)							///
	b(%-9.3f) se(%-9.3f) 												///
	keep(treated_age_*) 												///
	order(treated_age_1 treated_age_2 treated_age_3)					///
	coeflabels(	treated_age_1  "College $\times$ Post $\times$ Age 25--44"	///
				treated_age_2  "College $\times$ Post $\times$ Age 45--64"	///
				treated_age_3  "College $\times$ Post $\times$ Age 65+")	///
	mtitle(`het_cols')													///
	prehead("\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" 				///
			"\begin{tabular*}{\textwidth}{@{\extracolsep{\fill}}l*{4}{c}@{}}" 	///
			"\toprule")													///
	posthead("\midrule" 												///
			 "\multicolumn{5}{l}{\textit{Panel A: Age}}\\[0.3em]") 		///
	prefoot("") 														///
	stats(p_equal, fmt(%9.3f) labels("p-value, equal effects"))		///
	postfoot("\midrule") 												///
	noobs

** Panel B: Own children in the household
esttab did_kids_out did_kids_out_state did_kids_in_west did_kids_in_48 	///
	using "${results}did/tab_did_heterogeneity.tex",					///
	append 																///
	starlevel("*" 0.10 "**" 0.05 "***" 0.01)							///
	b(%-9.3f) se(%-9.3f) 												///
	keep(treated_kids_*) 												///
	order(treated_kids_0 treated_kids_1)								///
	coeflabels(	treated_kids_0  "College $\times$ Post $\times$ No own children"	///
				treated_kids_1  "College $\times$ Post $\times$ Own children")	///
	nomtitles nonumbers 												///
	prehead("\multicolumn{5}{l}{\textit{Panel B: Own children in household}}\\[0.3em]") ///
	posthead("") 														///
	prefoot("") 														///
	stats(p_equal, fmt(%9.3f) labels("p-value, equal effects"))		///
	postfoot("\midrule") 												///
	noobs

** Panel C: Marital status
esttab did_married_out did_married_out_state did_married_in_west did_married_in_48 	///
	using "${results}did/tab_did_heterogeneity.tex",					///
	append 																///
	starlevel("*" 0.10 "**" 0.05 "***" 0.01)							///
	b(%-9.3f) se(%-9.3f) 												///
	keep(treated_married_*) 											///
	order(treated_married_0 treated_married_1)							///
	coeflabels(	treated_married_0  "College $\times$ Post $\times$ Not married"	///
				treated_married_1  "College $\times$ Post $\times$ Married")	///
	nomtitles nonumbers 												///
	prehead("\multicolumn{5}{l}{\textit{Panel C: Marital status}}\\[0.3em]") ///
	posthead("") 														///
	prefoot("") 														///
	stats(p_equal, fmt(%9.3f) labels("p-value, equal effects"))		///
	postfoot("\midrule") 												///
	noobs

** Panel D: WFH potential (appends and closes table)
esttab did_wfh_out did_wfh_out_state did_wfh_in_west did_wfh_in_48 		///
	using "${results}did/tab_did_heterogeneity.tex",					///
	append 																///
	starlevel("*" 0.10 "**" 0.05 "***" 0.01)							///
	b(%-9.3f) se(%-9.3f) 												///
	keep(treated_wfh_*) 												///
	order(treated_wfh_1 treated_wfh_2 treated_wfh_3)					///
	coeflabels(	treated_wfh_1  "College $\times$ Post $\times$ Teleworkable"	///
				treated_wfh_2  "College $\times$ Post $\times$ Not teleworkable"	///
				treated_wfh_3  "College $\times$ Post $\times$ No occupation")	///
	nomtitles nonumbers 												///
	prehead("\multicolumn{5}{l}{\textit{Panel D: Work-from-home potential}}\\[0.3em]") ///
	posthead("") 														///
	prefoot("") 														///
	stats(p_equal N_unwtd, fmt(%9.3f %12.0fc) 							///
		labels("p-value, equal effects" "Observations"))				///
	postfoot("\bottomrule" 												///
			 "\end{tabular*}")

project_save_overleaf "${results}did/tab_did_heterogeneity.tex"
********************************************************************************
** REGRESSION 4: EVENT STUDY BY AGE
********************************************************************************

** Create year × education × age interactions (base year = 2019, 2020 excluded)
foreach y in 2016 2017 2018 2021 2022 2023 2024 {
	forvalues a = 1/3 {
		gen x_he_`y'_age`a' = high_ed * (year == `y') * (cat_age == `a')
	}
}

** Out-migration event study by age
reghdfe out_1 x_he_*_age* if sample_1 == 1 [pw = perwt], 	///
	vce(robust) absorb(year cat_* year#cat_age high_ed#cat_age)
estimates store es_age_out

** In-migration event study by age (West Coast)
reghdfe out_2 x_he_*_age* if sample_2 == 1 [pw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_* year#cat_age high_ed#cat_age)
estimates store es_age_in_west

** In-migration event study by age (Lower 48 + DC)
reghdfe out_2 x_he_*_age* if sample_3 == 1 [pw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_* year#cat_age high_ed#cat_age)
estimates store es_age_in_48

** Out-of-state migration event study by age
reghdfe out_3 x_he_*_age* if sample_1 == 1 [pw = perwt], 	///
	vce(robust) absorb(year cat_* year#cat_age high_ed#cat_age)
estimates store es_age_out_state

********************************************************************************
** EVENT STUDY BY AGE PLOTS
********************************************************************************


preserve
	clear
	set obs 9
	gen year = 2015 + _n

	** Create coefficient variables for each age group and sample
	forvalues a = 1/3 {
		foreach s in "out" "out_state" "in_west" "in_48" {
			gen `s'_coef_age`a' = .
			gen `s'_ci_lo_age`a' = .
			gen `s'_ci_hi_age`a' = .
		}
	}

	** Fill in coefficients
	foreach y in 2016 2017 2018 2021 2022 2023 2024 {
		forvalues a = 1/3 {

			** Out-migration
			estimates restore es_age_out
			capture {
				replace out_coef_age`a' = _b[x_he_`y'_age`a'] if year == `y'
				replace out_ci_lo_age`a' = _b[x_he_`y'_age`a'] - 1.96 * _se[x_he_`y'_age`a'] if year == `y'
				replace out_ci_hi_age`a' = _b[x_he_`y'_age`a'] + 1.96 * _se[x_he_`y'_age`a'] if year == `y'
			}

			** Out-of-state migration
			estimates restore es_age_out_state
			capture {
				replace out_state_coef_age`a' = _b[x_he_`y'_age`a'] if year == `y'
				replace out_state_ci_lo_age`a' = _b[x_he_`y'_age`a'] - 1.96 * _se[x_he_`y'_age`a'] if year == `y'
				replace out_state_ci_hi_age`a' = _b[x_he_`y'_age`a'] + 1.96 * _se[x_he_`y'_age`a'] if year == `y'
			}

			** In-migration (West Coast)
			estimates restore es_age_in_west
			capture {
				replace in_west_coef_age`a' = _b[x_he_`y'_age`a'] if year == `y'
				replace in_west_ci_lo_age`a' = _b[x_he_`y'_age`a'] - 1.96 * _se[x_he_`y'_age`a'] if year == `y'
				replace in_west_ci_hi_age`a' = _b[x_he_`y'_age`a'] + 1.96 * _se[x_he_`y'_age`a'] if year == `y'
			}

			** In-migration (Lower 48)
			estimates restore es_age_in_48
			capture {
				replace in_48_coef_age`a' = _b[x_he_`y'_age`a'] if year == `y'
				replace in_48_ci_lo_age`a' = _b[x_he_`y'_age`a'] - 1.96 * _se[x_he_`y'_age`a'] if year == `y'
				replace in_48_ci_hi_age`a' = _b[x_he_`y'_age`a'] + 1.96 * _se[x_he_`y'_age`a'] if year == `y'
			}

		}
	}

	** Base year (2019) = 0; 2020 excluded (no coefficient)
	forvalues a = 1/3 {
		foreach s in "out" "out_state" "in_west" "in_48" {
			foreach v in coef ci_lo ci_hi {
				replace `s'_`v'_age`a' = 0 if year == 2019
			}
		}
	}

	** Register the 45-64 post-2022 coefficient range quoted in Section 5.3 (paper_numbers.csv)
	qui summ out_coef_age2 if year >= 2022
	local es_min = r(min)
	local es_max = r(max)
	project_log_number, key(did_es_out_college_age45_64_post22_min) value(`es_min') fmt(%4.1f) unit(pp) ///
		source(02_did_analysis.do) note("Smallest College x Year coefficient for ages 45-64, out-migration by age, 2022-2024 (DiD appendix, by-age event study)")
	project_log_number, key(did_es_out_college_age45_64_post22_max) value(`es_max') fmt(%4.1f) unit(pp) ///
		source(02_did_analysis.do) note("Largest College x Year coefficient for ages 45-64, out-migration by age, 2022-2024 (DiD appendix, by-age event study)")

	** Offset years slightly for visibility across age groups
	gen year_age1 = year - 0.15
	gen year_age2 = year
	gen year_age3 = year + 0.15

	** Plot 1: Out-migration event study by age
	twoway 	(rcap out_ci_lo_age1 out_ci_hi_age1 year_age1, lc("`col_age1'")) 				///
			(scatter out_coef_age1 year_age1, mc("`col_age1'") ms(O)) 			///
			(rcap out_ci_lo_age2 out_ci_hi_age2 year_age2, lc("`col_age2'")) 			///
			(scatter out_coef_age2 year_age2, mc("`col_age2'") ms(T)) 	///
			(rcap out_ci_lo_age3 out_ci_hi_age3 year_age3, lc("`col_age3'")) 				///
			(scatter out_coef_age3 year_age3, mc("`col_age3'") ms(S)),		///
		yline(0, lc("`col_ref'") lp(dash)) 													///
		xline(2020.5, lc(black) lp(solid))												///
		xlabel(2016(1)2024) 															///
		ytitle("Effect on Out-Migration (pp)") 											///
		xtitle("Year")																	///
		legend(order(2 "25-44" 4 "45-64" 6 "65+") pos(6) rows(1)) 						///
		graphregion(color(white)) plotregion(color(white))

	graph export "${results}did/fig_es_age_out_migration.png", replace
	project_save_overleaf "${results}did/fig_es_age_out_migration.png"

	** Plot 2: In-migration (West Coast) event study by age
	twoway 	(rcap in_west_ci_lo_age1 in_west_ci_hi_age1 year_age1, lc("`col_age1'")) 		///
			(scatter in_west_coef_age1 year_age1, mc("`col_age1'") ms(O)) 		///
			(rcap in_west_ci_lo_age2 in_west_ci_hi_age2 year_age2, lc("`col_age2'")) 	///
			(scatter in_west_coef_age2 year_age2, mc("`col_age2'") ms(T)) ///
			(rcap in_west_ci_lo_age3 in_west_ci_hi_age3 year_age3, lc("`col_age3'")) 		///
			(scatter in_west_coef_age3 year_age3, mc("`col_age3'") ms(S)),	///
		yline(0, lc("`col_ref'") lp(dash)) 													///
		xline(2020.5, lc(black) lp(solid))												///
		xlabel(2016(1)2024) 															///
		ytitle("Effect on In-Migration (pp)") 											///
		xtitle("Year")																	///
		legend(order(2 "25-44" 4 "45-64" 6 "65+") pos(6) rows(1)) 						///
		graphregion(color(white)) plotregion(color(white))

	graph export "${results}did/fig_es_age_in_migration_west.png", replace
	project_save_overleaf "${results}did/fig_es_age_in_migration_west.png"

	** Plot 3: In-migration (Lower 48 + DC) event study by age
	twoway 	(rcap in_48_ci_lo_age1 in_48_ci_hi_age1 year_age1, lc("`col_age1'")) 			///
			(scatter in_48_coef_age1 year_age1, mc("`col_age1'") ms(O)) 		///
			(rcap in_48_ci_lo_age2 in_48_ci_hi_age2 year_age2, lc("`col_age2'")) 		///
			(scatter in_48_coef_age2 year_age2, mc("`col_age2'") ms(T)) ///
			(rcap in_48_ci_lo_age3 in_48_ci_hi_age3 year_age3, lc("`col_age3'")) 			///
			(scatter in_48_coef_age3 year_age3, mc("`col_age3'") ms(S)),	///
		yline(0, lc("`col_ref'") lp(dash)) 													///
		xline(2020.5, lc(black) lp(solid))												///
		xlabel(2016(1)2024) 															///
		ytitle("Effect on In-Migration (pp)") 											///
		xtitle("Year")																	///
		legend(order(2 "25-44" 4 "45-64" 6 "65+") pos(6) rows(1)) 						///
		graphregion(color(white)) plotregion(color(white))

	graph export "${results}did/fig_es_age_in_migration_48.png", replace
	project_save_overleaf "${results}did/fig_es_age_in_migration_48.png"

	** Plot 4: Out-of-state migration event study by age
	twoway 	(rcap out_state_ci_lo_age1 out_state_ci_hi_age1 year_age1, lc("`col_age1'")) 			///
			(scatter out_state_coef_age1 year_age1, mc("`col_age1'") ms(O)) 		///
			(rcap out_state_ci_lo_age2 out_state_ci_hi_age2 year_age2, lc("`col_age2'")) 		///
			(scatter out_state_coef_age2 year_age2, mc("`col_age2'") ms(T)) ///
			(rcap out_state_ci_lo_age3 out_state_ci_hi_age3 year_age3, lc("`col_age3'")) 			///
			(scatter out_state_coef_age3 year_age3, mc("`col_age3'") ms(S)),	///
		yline(0, lc("`col_ref'") lp(dash)) 													///
		xline(2020.5, lc(black) lp(solid))												///
		xlabel(2016(1)2024) 															///
		ytitle("Effect on Out-of-State Migration (pp)") 								///
		xtitle("Year")																	///
		legend(order(2 "25-44" 4 "45-64" 6 "65+") pos(6) rows(1)) 						///
		graphregion(color(white)) plotregion(color(white))

	graph export "${results}did/fig_es_age_out_state_migration.png", replace
	project_save_overleaf "${results}did/fig_es_age_out_state_migration.png"

restore

********************************************************************************
** REGRESSION 5: DIFFERENCE-IN-DIFFERENCES BY AGE (NO EDUCATION)
********************************************************************************

** Data from Regressions 1-4 are still in memory (the age event-study
** plots run inside preserve/restore).

** Create age × post interactions (omit middle age group: 45-64)
gen age_post_1 = (cat_age == 1) * post
gen age_post_3 = (cat_age == 3) * post

label var age_post_1 "Age 25-44 × Post"
label var age_post_3 "Age 65+ × Post"

** Store estimates

** Out-migration regression (Sample 1: Multnomah residents)
reghdfe out_1 age_post_1 age_post_3 if sample_1 == 1 [pw = perwt], 	///
	vce(robust) absorb(year cat_*)
qui count if e(sample)
estadd scalar N_unwtd = r(N)
estimates store did_agep_out

** In-migration regression (Sample 2: CA/OR/WA residents)
reghdfe out_2 age_post_1 age_post_3 if sample_2 == 1 [pw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_*)
qui count if e(sample)
estadd scalar N_unwtd = r(N)
estimates store did_agep_in_west

** In-migration regression (Sample 3: Lower 48 + DC residents)
reghdfe out_2 age_post_1 age_post_3 if sample_3 == 1 [pw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_*)
qui count if e(sample)
estadd scalar N_unwtd = r(N)
estimates store did_agep_in_48

** Out-of-state migration regression (Sample 1: Multnomah residents)
reghdfe out_3 age_post_1 age_post_3 if sample_1 == 1 [pw = perwt], 	///
	vce(robust) absorb(year cat_*)
qui count if e(sample)
estadd scalar N_unwtd = r(N)
estimates store did_agep_out_state

********************************************************************************
** TABLE: DiD BY AGE (NO EDUCATION) (individual panel -- kept for reference)
********************************************************************************

** Framing note for manuscript/slides:
** - Treat Panel C as the main age heterogeneity result; it is the closest match
**   to the substantive question of whether the college-proxy treatment effect
**   differs by age.
** - Reframe Panel B as a differential post-period age-pattern/composition check,
**   not as a treatment proxy result, or move it to an appendix.
** - If the text makes a heterogeneity claim, add explicit cross-age tests (for
**   example, 25-44 vs. 45-64 and 45-64 vs. 65+) or clarify that Panel C reports
**   age-specific treatment effects rather than tested differences between ages.

esttab did_agep_out did_agep_out_state did_agep_in_west did_agep_in_48 	///
	using "${results}did/tab_did_by_age_post.tex",						///
	starlevel("*" 0.10 "**" 0.05 "***" 0.01)							///
	b(%-9.3f) se(%-9.3f) replace 										///
	keep(age_post_1 age_post_3) 										///
	order(age_post_1 age_post_3)										///
	coeflabels(	age_post_1  "Age 25-44 $\times$ Post"					///
				age_post_3  "Age 65+ $\times$ Post")					///
	mtitle(	"Out-migration" 											///
			"Out-of-state"												///
			"In-migration (West Coast)" 								///
			"In-migration (Lower 48 + DC)")								///
	stats(N_unwtd, fmt(%12.0fc) labels("Observations"))

********************************************************************************
** REGRESSION 6: EVENT STUDY BY AGE (NO EDUCATION)
********************************************************************************

** Create year × age interactions (base year = 2019, 2020 excluded, omit age 2)
foreach y in 2016 2017 2018 2021 2022 2023 2024 {
	gen x_age1_`y' = (cat_age == 1) * (year == `y')
	gen x_age3_`y' = (cat_age == 3) * (year == `y')
}

** Out-migration event study by age
reghdfe out_1 x_age1_* x_age3_* if sample_1 == 1 [pw = perwt], 	///
	vce(robust) absorb(year cat_*)
estimates store es_agep_out

** In-migration event study by age (West Coast)
reghdfe out_2 x_age1_* x_age3_* if sample_2 == 1 [pw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_*)
estimates store es_agep_in_west

** In-migration event study by age (Lower 48 + DC)
reghdfe out_2 x_age1_* x_age3_* if sample_3 == 1 [pw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_*)
estimates store es_agep_in_48

** Out-of-state migration event study by age
reghdfe out_3 x_age1_* x_age3_* if sample_1 == 1 [pw = perwt], 	///
	vce(robust) absorb(year cat_*)
estimates store es_agep_out_state

********************************************************************************
** EVENT STUDY BY AGE (NO EDUCATION) PLOTS
********************************************************************************

preserve
	clear
	set obs 9
	gen year = 2015 + _n

	** Create coefficient variables for age groups 1 and 3, each sample
	foreach a in 1 3 {
		foreach s in "out" "out_state" "in_west" "in_48" {
			gen `s'_coef_age`a' = .
			gen `s'_ci_lo_age`a' = .
			gen `s'_ci_hi_age`a' = .
		}
	}

	** Fill in coefficients
	foreach y in 2016 2017 2018 2021 2022 2023 2024 {
		foreach a in 1 3 {

			** Out-migration
			estimates restore es_agep_out
			capture {
				replace out_coef_age`a' = _b[x_age`a'_`y'] if year == `y'
				replace out_ci_lo_age`a' = _b[x_age`a'_`y'] - 1.96 * _se[x_age`a'_`y'] if year == `y'
				replace out_ci_hi_age`a' = _b[x_age`a'_`y'] + 1.96 * _se[x_age`a'_`y'] if year == `y'
			}

			** Out-of-state migration
			estimates restore es_agep_out_state
			capture {
				replace out_state_coef_age`a' = _b[x_age`a'_`y'] if year == `y'
				replace out_state_ci_lo_age`a' = _b[x_age`a'_`y'] - 1.96 * _se[x_age`a'_`y'] if year == `y'
				replace out_state_ci_hi_age`a' = _b[x_age`a'_`y'] + 1.96 * _se[x_age`a'_`y'] if year == `y'
			}

			** In-migration (West Coast)
			estimates restore es_agep_in_west
			capture {
				replace in_west_coef_age`a' = _b[x_age`a'_`y'] if year == `y'
				replace in_west_ci_lo_age`a' = _b[x_age`a'_`y'] - 1.96 * _se[x_age`a'_`y'] if year == `y'
				replace in_west_ci_hi_age`a' = _b[x_age`a'_`y'] + 1.96 * _se[x_age`a'_`y'] if year == `y'
			}

			** In-migration (Lower 48)
			estimates restore es_agep_in_48
			capture {
				replace in_48_coef_age`a' = _b[x_age`a'_`y'] if year == `y'
				replace in_48_ci_lo_age`a' = _b[x_age`a'_`y'] - 1.96 * _se[x_age`a'_`y'] if year == `y'
				replace in_48_ci_hi_age`a' = _b[x_age`a'_`y'] + 1.96 * _se[x_age`a'_`y'] if year == `y'
			}

		}
	}

	** Base year (2019) = 0; 2020 excluded (no coefficient)
	foreach a in 1 3 {
		foreach s in "out" "out_state" "in_west" "in_48" {
			foreach v in coef ci_lo ci_hi {
				replace `s'_`v'_age`a' = 0 if year == 2019
			}
		}
	}

	** Offset years slightly for visibility
	gen year_age1 = year - 0.1
	gen year_age3 = year + 0.1

	** Plot 1: Out-migration event study by age (no education)
	twoway 	(rcap out_ci_lo_age1 out_ci_hi_age1 year_age1, lc("`col_age1'")) 				///
			(scatter out_coef_age1 year_age1, mc("`col_age1'") ms(O)) 						///
			(rcap out_ci_lo_age3 out_ci_hi_age3 year_age3, lc("`col_age3'")) 				///
			(scatter out_coef_age3 year_age3, mc("`col_age3'") ms(S)),						///
		yline(0, lc("`col_ref'") lp(dash)) 													///
		xline(2020.5, lc(black) lp(solid))												///
		xlabel(2016(1)2024) 															///
		ytitle("Effect on Out-Migration (pp)") 											///
		xtitle("Year")																	///
		legend(order(2 "25-44" 4 "65+") pos(6) rows(1)) 								///
		graphregion(color(white)) plotregion(color(white))

	graph export "${results}did/fig_es_agepost_out_migration.png", replace
	project_save_overleaf "${results}did/fig_es_agepost_out_migration.png"

	** Plot 2: In-migration (West Coast) event study by age (no education)
	twoway 	(rcap in_west_ci_lo_age1 in_west_ci_hi_age1 year_age1, lc("`col_age1'")) 		///
			(scatter in_west_coef_age1 year_age1, mc("`col_age1'") ms(O)) 					///
			(rcap in_west_ci_lo_age3 in_west_ci_hi_age3 year_age3, lc("`col_age3'")) 		///
			(scatter in_west_coef_age3 year_age3, mc("`col_age3'") ms(S)),					///
		yline(0, lc("`col_ref'") lp(dash)) 													///
		xline(2020.5, lc(black) lp(solid))												///
		xlabel(2016(1)2024) 															///
		ytitle("Effect on In-Migration (pp)") 											///
		xtitle("Year")																	///
		legend(order(2 "25-44" 4 "65+") pos(6) rows(1)) 								///
		graphregion(color(white)) plotregion(color(white))

	graph export "${results}did/fig_es_agepost_in_migration_west.png", replace
	project_save_overleaf "${results}did/fig_es_agepost_in_migration_west.png"

	** Plot 3: In-migration (Lower 48 + DC) event study by age (no education)
	twoway 	(rcap in_48_ci_lo_age1 in_48_ci_hi_age1 year_age1, lc("`col_age1'")) 			///
			(scatter in_48_coef_age1 year_age1, mc("`col_age1'") ms(O)) 						///
			(rcap in_48_ci_lo_age3 in_48_ci_hi_age3 year_age3, lc("`col_age3'")) 			///
			(scatter in_48_coef_age3 year_age3, mc("`col_age3'") ms(S)),					///
		yline(0, lc("`col_ref'") lp(dash)) 													///
		xline(2020.5, lc(black) lp(solid))												///
		xlabel(2016(1)2024) 															///
		ytitle("Effect on In-Migration (pp)") 											///
		xtitle("Year")																	///
		legend(order(2 "25-44" 4 "65+") pos(6) rows(1)) 								///
		graphregion(color(white)) plotregion(color(white))

	graph export "${results}did/fig_es_agepost_in_migration_48.png", replace
	project_save_overleaf "${results}did/fig_es_agepost_in_migration_48.png"

	** Plot 4: Out-of-state migration event study by age (no education)
	twoway 	(rcap out_state_ci_lo_age1 out_state_ci_hi_age1 year_age1, lc("`col_age1'")) 	///
			(scatter out_state_coef_age1 year_age1, mc("`col_age1'") ms(O)) 					///
			(rcap out_state_ci_lo_age3 out_state_ci_hi_age3 year_age3, lc("`col_age3'")) 	///
			(scatter out_state_coef_age3 year_age3, mc("`col_age3'") ms(S)),				///
		yline(0, lc("`col_ref'") lp(dash)) 													///
		xline(2020.5, lc(black) lp(solid))												///
		xlabel(2016(1)2024) 															///
		ytitle("Effect on Out-of-State Migration (pp)") 								///
		xtitle("Year")																	///
		legend(order(2 "25-44" 4 "65+") pos(6) rows(1)) 								///
		graphregion(color(white)) plotregion(color(white))

	graph export "${results}did/fig_es_agepost_out_state_migration.png", replace
	project_save_overleaf "${results}did/fig_es_agepost_out_state_migration.png"

restore


********************************************************************************
** CLEANUP
********************************************************************************

** Close log
clear
log close log_02
