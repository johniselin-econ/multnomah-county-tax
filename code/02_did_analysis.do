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


** Start log file
capture log close log_02
log using "${logs}02_log_did_${date}", replace text name(log_02)


********************************************************************************
** LOAD AND PREPARE DATA
********************************************************************************

** Load ACS migration data
use "${data}working/acs_migration_file", replace

** Sample restrictions
drop if year == 2015 					// Sample: 2016-2024
drop if qmigplc1 == 4					// Error in migration place
drop if inlist(state_fips_o, 2, 15)   	// Alaska and Hawaii
drop if inlist(state_fips_d, 2, 15)   	// Alaska and Hawaii

********************************************************************************
** DEFINE SAMPLES AND TREATMENT
********************************************************************************

** Multnomah county indicators
gen multnomah_o = (state_fips_o == 41 & county_fips_o == 51)
gen multnomah_d = (state_fips_d == 41 & county_fips_d == 51)

** Analysis samples
gen sample_1 = (multnomah_o == 1)       // Multnomah in origin (out-migration)
gen sample_2 = (multnomah_o != 1) & 	/// Not Multnomah in origin
			   inlist(state_fips_o, 6, 41, 53)  // CA, OR, WA (in-migration)

label var sample_1 "Out-migration Sample"
label var sample_2 "In-migration Sample"

** Outcome variables (scaled to percentages)
gen out_1 = (same_county == 0) * 100
label var out_1 "Moved out of Multnomah (%)"

gen out_2 = (multnomah_d == 1) * 100
label var out_2 "Moved to Multnomah (%)"

** Treatment variables
gen post = year > 2020
label var post "Post-2020"

gen high_ed = educd >= 101
label var high_ed "College Degree"

gen treated = high_ed * post
label var treated "College Degree × Post"

********************************************************************************
** COVARIATE CATEGORIES
********************************************************************************

** Age categories
recode age ///
    (18/24   = 1 "18-24") ///
    (25/44   = 2 "25-44") ///
    (45/64   = 3 "45-64") ///
    (65/max  = 4 "65+"), ///
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

********************************************************************************
** REGRESSION 1: DIFFERENCE-IN-DIFFERENCES
********************************************************************************

** Store estimates for coefficient plot
estimates clear

** Out-migration regression (Sample 1: Multnomah residents)
reghdfe out_1 treated if sample_1 == 1 [fw = perwt], 	///
	vce(robust) absorb(year cat_*)
estimates store did_out

** In-migration regression (Sample 2: CA/OR/WA residents)
reghdfe out_2 treated if sample_2 == 1 [fw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_*)
estimates store did_in

********************************************************************************
** COEFFICIENT PLOT: DiD RESULTS
********************************************************************************

coefplot 	(did_out, label("Out-migration from Multnomah") 	///
				mc(navy) ciopts(lc(navy))) 						///
			(did_in, label("In-migration to Multnomah") 		///
				mc(maroon) ciopts(lc(maroon))), 				///
	keep(treated) 												///
	ciopts(recast(rcap)) 										///
	yline(0, lc(gs10) lp(dash)) 								///
	xline(0, lc(gs10) lp(dash)) 								///
	coeflabels(treated = "College Degree × Post") 				///
	ytitle("") 													///
	xtitle("Effect on Migration Rate (pp)")						///
	legend(pos(6) rows(1)) 										///
	title("Effect of Multnomah County Tax on Migration")		///
	subtitle("Difference-in-Differences Estimates")				///
	graphregion(color(white))

graph export "${results}did/fig_did_coefplot.png", replace

********************************************************************************
** REGRESSION 2: EVENT STUDY
********************************************************************************

** Create year × education interactions (base year = 2020)
forvalues y = 2016/2024 {
	gen x_high_ed_`y' = high_ed * (year == `y')
}

** Drop base year (2020)
drop x_high_ed_2020

** Out-migration event study
reghdfe out_1 x_high_ed_* if sample_1 == 1 [fw = perwt], 	///
	vce(robust) absorb(year cat_*)
estimates store es_out

** In-migration event study
reghdfe out_2 x_high_ed_* if sample_2 == 1 [fw = perwt], 	///
	vce(cluster fips_o) absorb(year fips_o cat_*)
estimates store es_in

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
	gen in_coef = .
	gen in_ci_lo = .
	gen in_ci_hi = .

	** Fill in coefficients (2020 = base year = 0)
	foreach y in 2016 2017 2018 2019 2021 2022 2023 2024 {

		** Out-migration
		estimates restore es_out
		capture {
			replace out_coef = _b[x_high_ed_`y'] if year == `y'
			replace out_ci_lo = _b[x_high_ed_`y'] - 1.96 * _se[x_high_ed_`y'] if year == `y'
			replace out_ci_hi = _b[x_high_ed_`y'] + 1.96 * _se[x_high_ed_`y'] if year == `y'
		}

		** In-migration
		estimates restore es_in
		capture {
			replace in_coef = _b[x_high_ed_`y'] if year == `y'
			replace in_ci_lo = _b[x_high_ed_`y'] - 1.96 * _se[x_high_ed_`y'] if year == `y'
			replace in_ci_hi = _b[x_high_ed_`y'] + 1.96 * _se[x_high_ed_`y'] if year == `y'
		}
	}

	** Base year (2020) = 0
	foreach v in out_coef out_ci_lo out_ci_hi in_coef in_ci_lo in_ci_hi {
		replace `v' = 0 if year == 2020
	}

	** Offset years slightly for visibility
	gen year_out = year - 0.1
	gen year_in = year + 0.1

	** Plot 1: Out-migration event study
	twoway 	(rcap out_ci_lo out_ci_hi year, lc(navy)) 				///
			(connected out_coef year, mc(navy) lc(navy) ms(O)),		///
		yline(0, lc(gs10) lp(dash)) 								///
		xline(2020.5, lc(black) lp(solid))							///
		xlabel(2016(1)2024) 										///
		ytitle("Effect on Out-Migration (pp)") 						///
		xtitle("Year")												///
		legend(off) 												///
		title("Out-Migration from Multnomah County")				///
		subtitle("College Degree vs. No College Degree")			///
		note("Base year: 2020. Vertical line indicates tax implementation.")	///
		graphregion(color(white))

	graph export "${results}did/fig_es_out_migration.png", replace

	** Plot 2: In-migration event study
	twoway 	(rcap in_ci_lo in_ci_hi year, lc(maroon)) 				///
			(connected in_coef year, mc(maroon) lc(maroon) ms(O)),	///
		yline(0, lc(gs10) lp(dash)) 								///
		xline(2020.5, lc(black) lp(solid))							///
		xlabel(2016(1)2024) 										///
		ytitle("Effect on In-Migration (pp)") 						///
		xtitle("Year")												///
		legend(off) 												///
		title("In-Migration to Multnomah County")					///
		subtitle("College Degree vs. No College Degree")			///
		note("Base year: 2020. Vertical line indicates tax implementation.")	///
		graphregion(color(white))

	graph export "${results}did/fig_es_in_migration.png", replace

	** Plot 3: Combined event study
	twoway 	(rcap out_ci_lo out_ci_hi year_out, lc(navy)) 					///
			(connected out_coef year_out, mc(navy) lc(navy) ms(O)) 			///
			(rcap in_ci_lo in_ci_hi year_in, lc(maroon)) 					///
			(connected in_coef year_in, mc(maroon) lc(maroon) ms(T)),		///
		yline(0, lc(gs10) lp(dash)) 										///
		xline(2020.5, lc(black) lp(solid))									///
		xlabel(2016(1)2024) 												///
		ytitle("Effect on Migration Rate (pp)") 							///
		xtitle("Year")														///
		legend(order(2 "Out-migration" 4 "In-migration") 					///
			pos(6) rows(1)) 												///
		title("Migration Effects: Multnomah County Tax")					///
		subtitle("College Degree vs. No College Degree")					///
		note("Base year: 2020. Vertical line indicates tax implementation.")	///
		graphregion(color(white))

	graph export "${results}did/fig_es_combined.png", replace

restore

********************************************************************************
** CLEANUP
********************************************************************************

** Close log
clear
log close log_02
