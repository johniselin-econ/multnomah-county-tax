/*******************************************************************************
File Name: 		02_did_analysis.do
Creator: 		John Iselin
Date Update:	January 11, 2026

Called by: 00_multnomah.do

Purpose: Perform DiD analysis on ACS data examining the effect of the
         Multnomah County tax on migration patterns by education level.

Authors: John Iselin

For more information, contact john.iselin@yale.edu

*******************************************************************************/


** Load shared project defaults and helper programs
if "${code}" == "" {
	local _cwd = subinstr("`c(pwd)'", "\", "/", .)
	if regexm("`_cwd'", "(.*)/code/stata$") global code "`_cwd'/"
	else global code "`_cwd'/code/stata/"
}
do "${code}00_stata_config.do"

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

** Load ACS migration data
use "${data}working/acs_migration_file", replace

** Sample restrictions
drop if year < 2016 					// Sample: 2016-2024 (ACS file starts 2012)
drop if qmigplc1 == 4					// Error in migration place
drop if inlist(state_fips_o, 2, 15)   	// Alaska and Hawaii
drop if inlist(state_fips_d, 2, 15)   	// Alaska and Hawaii
drop if age < 25 						// Dropping under 25
drop if ftotinc < 0 					// Negative Family Income 	

********************************************************************************
** DEFINE SAMPLES AND TREATMENT
********************************************************************************

** Multnomah county indicators
gen multnomah_o = (state_fips_o == 41 & county_fips_o == 51)
gen multnomah_d = (state_fips_d == 41 & county_fips_d == 51)

** Analysis samples
gen sample_1 = (multnomah_o == 1) & year != 2020    // Multnomah in origin (out-migration)
gen sample_2 = (multnomah_o != 1) & 				///
				year != 2020 &						/// Not Multnomah in origin
			   inlist(state_fips_o, 6, 41, 53)  	// CA, OR, WA (in-migration)
gen sample_3 = (multnomah_o != 1) & 				/// Not Multnomah in origin
				year != 2020						// 

label var sample_1 "Out-migration Sample"
label var sample_2 "In-migration Sample (West Coast)"
label var sample_3 "In-migration Sample (Lower 48 + DC)"

** Outcome variables (scaled to percentages)
gen out_1 = (same_county == 0) * 100
label var out_1 "Moved out of Multnomah (%)"

gen out_2 = (multnomah_d == 1) * 100
label var out_2 "Moved to Multnomah (%)"

gen out_3 = (state_fips_o != state_fips_d) * 100
label var out_3 "Moved out of Oregon (%)"

** Treatment variables
gen post = year > 2020
label var post "Post-2020"

gen high_ed = educd >= 101
label var high_ed "College Degree"

gen treated = high_ed * post
label var treated "College Degree × Post"

********************************************************************************
** COVARIATE CATEGORIES (program definition + first call)
********************************************************************************

** Define reusable program for categorical covariate creation
capture program drop create_covariates
program define create_covariates

	** Age categories
	recode age ///
		(25/44   = 1 "25-44") ///
		(45/64   = 2 "45-64") ///
		(65/max  = 3 "65+"), ///
		gen(cat_age)
	label var cat_age "Age categories"

	** Sex
	gen cat_sex = (sex == 2)
	label var cat_sex "Female indicator"
	label define lb_cat_sex 0 "Male" 1 "Female", replace
	label values cat_sex lb_cat_sex

	** Marital status
	recode marst ///
		(1       = 1 "Married") ///
		(2/3     = 2 "Separated") ///
		(4/5     = 3 "Divorced / Widowed") ///
		(6       = 4 "Single"), ///
		gen(cat_married)
	label var cat_married "Marriage categories"

	** Number of children
	recode nchild ///
		(0       = 0 "0 Children") ///
		(1       = 1 "1 Child") ///
		(2       = 2 "2 Children") ///
		(3/max   = 3 "3+ Children"), ///
		gen(cat_child)
	label var cat_child "Number of Children"

	** Age of youngest child
	recode yngch ///
		(99      = 0 "No Children") ///
		(0/4     = 1 "0-4") ///
		(5/12    = 2 "5-12") ///
		(13/17   = 3 "13-17")	///
		(18/max  = 4 "18+"), ///
		gen(cat_yngch)
	label var cat_yngch "Age of youngest child"

	** Education categories
	recode educd ///
		(min/61  = 1 "Less than HS") ///
		(62/71   = 2 "HS Diploma") ///
		(80/100  = 3 "Some College") ///
		(101/max = 4 "College Degree"), ///
		gen(cat_educ)
	label var cat_educ "Education categories"

end

create_covariates

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
** TABLE: DiD RESULTS (individual panel -- kept for reference)
** Combined table output moved to after all DiD regressions below
********************************************************************************

esttab did_out did_out_state did_in_west did_in_48 						///
	using "${results}did/tab_did_overall.tex",							///
	starlevel("*" 0.10 "**" 0.05 "***" 0.01)							///
	b(%-9.3f) se(%-9.3f) replace 										///
	keep(treated) 														///
	coeflabels(treated  "College Degree $\times$ Post")				///
	mtitle(	"Out-migration" 											///
			"Out-of-state"												///
			"In-migration (West Coast)" 								///
			"In-migration (Lower 48 + DC)")								///
	stats(N_unwtd, fmt(%12.0fc) labels("Observations"))

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
	if ${overleaf} == 1 {
		graph export "${ol_fig}fig_es_out_migration.png", replace
	}

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
	if ${overleaf} == 1 {
		graph export "${ol_fig}fig_es_in_migration_west.png", replace
	}

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
	if ${overleaf} == 1 {
		graph export "${ol_fig}fig_es_in_migration_48.png", replace
	}

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
	if ${overleaf} == 1 {
		graph export "${ol_fig}fig_es_out_state_migration.png", replace
	}

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
	if ${overleaf} == 1 {
		graph export "${ol_fig}fig_es_combined.png", replace
	}

restore

********************************************************************************
** REGRESSION 3: DIFFERENCE-IN-DIFFERENCES BY AGE
********************************************************************************

** Create age × treatment interactions
gen treated_age_1 = treated * (cat_age == 1)
gen treated_age_2 = treated * (cat_age == 2)
gen treated_age_3 = treated * (cat_age == 3)

label var treated_age_1 "College x Post x Age 25-44"
label var treated_age_2 "College x Post x Age 45-64"
label var treated_age_3 "College x Post x Age 65+"

** Store estimates

** Out-migration regression by age (Sample 1: Multnomah residents)
reghdfe out_1 treated_age_* if sample_1 == 1 [pw = perwt], 	///
	vce(robust) absorb(year cat_*)
qui count if e(sample)
estadd scalar N_unwtd = r(N)
estimates store did_age_out

** In-migration regression by age (Sample 2: CA/OR/WA residents)
reghdfe out_2 treated_age_* if sample_2 == 1 [pw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_*)
qui count if e(sample)
estadd scalar N_unwtd = r(N)
estimates store did_age_in_west

** In-migration regression by age (Sample 3: Lower 48 + DC residents)
reghdfe out_2 treated_age_* if sample_3 == 1 [pw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_*)
qui count if e(sample)
estadd scalar N_unwtd = r(N)
estimates store did_age_in_48

** Out-of-state migration regression by age (Sample 1: Multnomah residents)
reghdfe out_3 treated_age_* if sample_1 == 1 [pw = perwt], 	///
	vce(robust) absorb(year cat_*)
qui count if e(sample)
estadd scalar N_unwtd = r(N)
estimates store did_age_out_state

********************************************************************************
** TABLE: DiD BY AGE (individual panel -- kept for reference)
** Combined table output moved to after all DiD regressions below
********************************************************************************

esttab did_age_out did_age_out_state did_age_in_west did_age_in_48 		///
	using "${results}did/tab_did_by_age.tex",							///
	starlevel("*" 0.10 "**" 0.05 "***" 0.01)							///
	b(%-9.3f) se(%-9.3f) replace 										///
	keep(treated_age_*) 												///
	order(treated_age_1 treated_age_2 treated_age_3)					///
	coeflabels(	treated_age_1  "College $\times$ Post $\times$ Age 25-44"	///
				treated_age_2  "College $\times$ Post $\times$ Age 45-64"	///
				treated_age_3  "College $\times$ Post $\times$ Age 65+")	///
	mtitle(	"Out-migration" 											///
			"Out-of-state"												///
			"In-migration (West Coast)" 								///
			"In-migration (Lower 48 + DC)")								///
	stats(N_unwtd, fmt(%12.0fc) labels("Observations"))

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
	vce(robust) absorb(year cat_*)
estimates store es_age_out

** In-migration event study by age (West Coast)
reghdfe out_2 x_he_*_age* if sample_2 == 1 [pw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_*)
estimates store es_age_in_west

** In-migration event study by age (Lower 48 + DC)
reghdfe out_2 x_he_*_age* if sample_3 == 1 [pw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_*)
estimates store es_age_in_48

** Out-of-state migration event study by age
reghdfe out_3 x_he_*_age* if sample_1 == 1 [pw = perwt], 	///
	vce(robust) absorb(year cat_*)
estimates store es_age_out_state

********************************************************************************
** EVENT STUDY BY AGE PLOTS
********************************************************************************


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
	if ${overleaf} == 1 {
		graph export "${ol_fig}fig_es_age_out_migration.png", replace
	}

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
	if ${overleaf} == 1 {
		graph export "${ol_fig}fig_es_age_in_migration_west.png", replace
	}

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
	if ${overleaf} == 1 {
		graph export "${ol_fig}fig_es_age_in_migration_48.png", replace
	}

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
	if ${overleaf} == 1 {
		graph export "${ol_fig}fig_es_age_out_state_migration.png", replace
	}


********************************************************************************
** REGRESSION 5: DIFFERENCE-IN-DIFFERENCES BY AGE (NO EDUCATION)
********************************************************************************

** Restore original data
use "${data}working/acs_migration_file", replace

** Sample restrictions
drop if year < 2016					// Sample: 2016-2024 (ACS file starts 2012)
drop if qmigplc1 == 4
drop if inlist(state_fips_o, 2, 15)
drop if inlist(state_fips_d, 2, 15)
drop if age < 25
drop if ftotinc < 0

** Multnomah county indicators
gen multnomah_o = (state_fips_o == 41 & county_fips_o == 51)
gen multnomah_d = (state_fips_d == 41 & county_fips_d == 51)

** Analysis samples
gen sample_1 = (multnomah_o == 1) & year != 2020
gen sample_2 = (multnomah_o != 1) & 				///
				year != 2020 &						///
			   inlist(state_fips_o, 6, 41, 53)
gen sample_3 = (multnomah_o != 1) & 				///
				year != 2020

** Outcome variables
gen out_1 = (same_county == 0) * 100
gen out_2 = (multnomah_d == 1) * 100
gen out_3 = (state_fips_o != state_fips_d) * 100

** Treatment variables
gen post = year > 2020

** Covariate categories (reuse program from above)
create_covariates

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
** COMBINED TABLE: DiD RESULTS (Panels A, B, C in single file)
** Produces tab_did_combined.tex for use in main.tex Table A3
********************************************************************************

** Panel A: College Degree × Post (creates file with full header)
esttab did_out did_out_state did_in_west did_in_48 						///
	using "${results}did/tab_did_combined.tex",							///
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
			"\begin{tabular*}{\textwidth}{@{\extracolsep{\fill}}l*{4}{c}@{}}" 						///
			"\toprule")												///
	posthead("\midrule" 													///
			 "\multicolumn{5}{l}{\textit{Panel A: College Degree as Treatment Proxy}}\\[0.3em]") ///
	prefoot("") 														///
	postfoot("\midrule") 													///
	noobs

** Panel B: Age Group × Post (appends to combined file)
esttab did_agep_out did_agep_out_state did_agep_in_west did_agep_in_48 	///
	using "${results}did/tab_did_combined.tex",							///
	append 																///
	starlevel("*" 0.10 "**" 0.05 "***" 0.01)							///
	b(%-9.3f) se(%-9.3f) 												///
	keep(age_post_1 age_post_3) 										///
	order(age_post_1 age_post_3)										///
	coeflabels(	age_post_1  "Age 25-44 $\times$ Post"					///
				age_post_3  "Age 65+ $\times$ Post")					///
	nomtitles nonumbers 												///
	prehead("\multicolumn{5}{l}{\textit{Panel B: Age Group as Treatment Proxy (Reference: 45--64)}}\\[0.3em]") ///
	posthead("") 														///
	prefoot("") 														///
	postfoot("\midrule") 													///
	noobs

** Panel C: Treatment heterogeniety by age (appends and closes table)
esttab did_age_out did_age_out_state did_age_in_west did_age_in_48 		///
	using "${results}did/tab_did_combined.tex",							///
	append 																///
	starlevel("*" 0.10 "**" 0.05 "***" 0.01)							///
	b(%-9.3f) se(%-9.3f) 												///
	keep(treated_age_*) 												///
	order(treated_age_1 treated_age_2 treated_age_3)					///
	coeflabels(	treated_age_1  "College $\times$ Post $\times$ Age 25-44"	///
				treated_age_2  "College $\times$ Post $\times$ Age 45-64"	///
				treated_age_3  "College $\times$ Post $\times$ Age 65+")	///
	nomtitles nonumbers 												///
	prehead("\multicolumn{5}{l}{\textit{Panel C: Treatment heterogeneity by age (College $\times$ Age $\times$ Post)}}\\[0.3em]") ///
	posthead("") 														///
	prefoot("\midrule") 													///
	stats(N_unwtd, fmt(%12.0fc) labels("Observations"))					///
	postfoot("\bottomrule" 											///
			 "\end{tabular*}")

if ${overleaf} == 1 {


	** Panel A: College Degree × Post (creates file with full header)
	esttab did_out did_out_state did_in_west did_in_48 						///
		using "${ol_tab}tab_did_combined.tex",							///
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
				"\begin{tabular*}{\textwidth}{@{\extracolsep{\fill}}l*{4}{c}@{}}" 						///
				"\toprule")												///
		posthead("\midrule" 													///
				 "\multicolumn{5}{l}{\textit{Panel A: College Degree as Treatment Proxy}}\\[0.3em]") ///
		prefoot("") 														///
		postfoot("\midrule") 													///
		noobs

	** Panel B: Age Group × Post (appends to combined file)
	esttab did_agep_out did_agep_out_state did_agep_in_west did_agep_in_48 	///
		using "${ol_tab}tab_did_combined.tex",							///
		append 																///
		starlevel("*" 0.10 "**" 0.05 "***" 0.01)							///
		b(%-9.3f) se(%-9.3f) 												///
		keep(age_post_1 age_post_3) 										///
		order(age_post_1 age_post_3)										///
		coeflabels(	age_post_1  "Age 25-44 $\times$ Post"					///
					age_post_3  "Age 65+ $\times$ Post")					///
		nomtitles nonumbers 												///
		prehead("\multicolumn{5}{l}{\textit{Panel B: Age Group as Treatment Proxy (Reference: 45--64)}}\\[0.3em]") ///
		posthead("") 														///
		prefoot("") 														///
		postfoot("\midrule") 													///
		noobs

	** Panel C: Treatment heterogeniety by age (appends and closes table)
	esttab did_age_out did_age_out_state did_age_in_west did_age_in_48 		///
		using "${ol_tab}tab_did_combined.tex",							///
		append 																///
		starlevel("*" 0.10 "**" 0.05 "***" 0.01)							///
		b(%-9.3f) se(%-9.3f) 												///
		keep(treated_age_*) 												///
		order(treated_age_1 treated_age_2 treated_age_3)					///
		coeflabels(	treated_age_1  "College $\times$ Post $\times$ Age 25-44"	///
					treated_age_2  "College $\times$ Post $\times$ Age 45-64"	///
					treated_age_3  "College $\times$ Post $\times$ Age 65+")	///
		nomtitles nonumbers 												///
		prehead("\multicolumn{5}{l}{\textit{Panel C: Treatment heterogeneity by age (College $\times$ Age $\times$ Post)}}\\[0.3em]") ///
		posthead("") 														///
		prefoot("\midrule") 													///
		stats(N_unwtd, fmt(%12.0fc) labels("Observations"))					///
		postfoot("\bottomrule" 											///
				 "\end{tabular*}")

	}			 
			 
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
	if ${overleaf} == 1 {
		graph export "${ol_fig}fig_es_agepost_out_migration.png", replace
	}

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
	if ${overleaf} == 1 {
		graph export "${ol_fig}fig_es_agepost_in_migration_west.png", replace
	}

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
	if ${overleaf} == 1 {
		graph export "${ol_fig}fig_es_agepost_in_migration_48.png", replace
	}

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
	if ${overleaf} == 1 {
		graph export "${ol_fig}fig_es_agepost_out_state_migration.png", replace
	}

restore


********************************************************************************
** CLEANUP
********************************************************************************

** Close log
clear
log close log_02
