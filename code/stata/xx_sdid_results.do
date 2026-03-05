/*******************************************************************************
File Name: 		xx_sdid_results.do
Creator: 		John Iselin
Date Update:	March 2026

Called by: 00_multnomah.do

Purpose: Analyze SDID specification curve results to determine how each
         researcher decision (donor pool, data source, covariates, etc.)
         influences the estimated treatment effect.

         Treats the SDID results as a dataset and runs OLS regressions with
         tau (treatment effect) as the dependent variable and specification
         choice indicators as regressors. Produces coefplots for each
         outcome type (returns, exemptions, AGI) x migration direction
         (out, in, net).

Inputs:
- ${results}sdid/sdid_results.dta

Outputs:
- ${results}sdid/fig_sdid_influence_*.pdf/jpg: Coefplots (9 total)

Authors: John Iselin

For more information, contact john.iselin@yale.edu

*******************************************************************************/


** Start log file
capture log close log_xx
log using "${logs}xx_log_sdid_results_${date}", replace text name(log_xx)

********************************************************************************
** CONFIGURATION
********************************************************************************

** plotplainblind palette (RGB)
local col_main    "0 114 178"     // sea (p7)
local col_zero    "153 153 153"   // gs10 (p2)

********************************************************************************
** LOAD AND PREPARE RESULTS
********************************************************************************

use "${results}sdid/sdid_results.dta", clear

** Parse outcome type from variable name
gen outcome_type = ""
replace outcome_type = "n1"  if strpos(outcome, "n1_") > 0
replace outcome_type = "n2"  if strpos(outcome, "n2_") > 0
replace outcome_type = "agi" if strpos(outcome, "agi_") > 0

** Parse migration direction
gen migration = ""
replace migration = "net" if strpos(outcome, "_net_") > 0
replace migration = "in"  if strpos(outcome, "_in_") > 0
replace migration = "out" if strpos(outcome, "_out_") > 0

********************************************************************************
** CREATE SPECIFICATION CHOICE VARIABLES
********************************************************************************

** 1. Donor pool (5 levels, base = All Counties)
gen donor_pool = .
replace donor_pool = 1 if sample == "sample_all"
replace donor_pool = 2 if sample == "sample_urban95"
replace donor_pool = 3 if sample == "sample_urban75_covid"
replace donor_pool = 4 if sample == "sample_demog"
replace donor_pool = 5 if sample == "sample_stringency"
label define donor_pool_lbl 1 "All Counties" 2 "Urban 95%" 			///
	3 "COVID Match" 4 "Demog. Match" 5 "Stringency"
label values donor_pool donor_pool_lbl

** 2. Data source (4 levels, base = IRS Full)
gen data_src = .
replace data_src = 1 if strpos(sample_data, "_full_") > 0
replace data_src = 2 if strpos(sample_data, "_389_") > 0
replace data_src = 3 if strpos(sample_data, "acs_") > 0 			///
	& strpos(sample_data, "_all") > 0
replace data_src = 4 if strpos(sample_data, "acs_") > 0 			///
	& strpos(sample_data, "_col") > 0
label define data_src_lbl 1 "IRS (Full)" 2 "IRS (ACS Counties)" 	///
	3 "ACS (All 25+)" 4 "ACS (College)"
label values data_src data_src_lbl

** 3. Out-of-state movers (binary)
gen outstate = strpos(sample_data, "outstate") > 0
label var outstate "Out-of-State Movers"

** 4. Extended period (binary: 16-24 vs 16-22)
gen period_1624 = strpos(sample_data, "16_24") > 0
label var period_1624 "Extended Period (16-24)"

** 5. Label existing binary variables
label var controls "With Covariates"
label var exclusion "Exclude 2020"

** Verify no missing specification choices
assert !missing(donor_pool)
assert !missing(data_src)

** Summary of specification choices
dis _n "Specification choice summary:"
tab donor_pool
tab data_src
tab outstate
tab period_1624
tab controls
tab exclusion

********************************************************************************
** META-REGRESSIONS AND COEFPLOTS
********************************************************************************

foreach otype in "n1" "n2" "agi" {
	foreach migr in "out" "in" "net" {

		** Labels for titles
		if "`otype'" == "n1"  local otype_label "Returns"
		else if "`otype'" == "n2"  local otype_label "Exemptions"
		else if "`otype'" == "agi" local otype_label "AGI"

		if "`migr'" == "net" local migr_label "Net Migration"
		else if "`migr'" == "in"  local migr_label "In-Migration"
		else if "`migr'" == "out" local migr_label "Out-Migration"

		** Preserve and subset
		preserve
		keep if outcome_type == "`otype'" & migration == "`migr'"

		** Check sufficient observations
		qui count
		local n_specs = r(N)
		if `n_specs' < 10 {
			dis "Skipping `otype' `migr': only `n_specs' specifications."
			restore
			continue
		}

		dis _n "========================================================"
		dis "`otype_label': `migr_label' (`n_specs' specifications)"
		dis "========================================================"

		** Run meta-regression
		** LHS: SDID treatment effect (tau)
		** RHS: Indicators for each specification decision
		** Base categories: All Counties (donor pool), IRS Full (data source)
		reg tau 	ib1.donor_pool 		///
					ib1.data_src 		///
					outstate 			///
					period_1624 		///
					controls 			///
					exclusion, 			///
					robust

		** Store regression stats for subtitle
		local r2 : di %4.3f e(r2)
		local n  : di %4.0f e(N)

		** Coefplot
		coefplot, drop(_cons) noomitted 							///
			xline(0, lc("`col_zero'") lp(dash)) 					///
			headings( 												///
				2.donor_pool = "{bf:Donor Pool (vs. All Counties)}" 	///
				2.data_src = "{bf:Data Source (vs. IRS Full)}" 		///
				outstate = "{bf:Other Specification Choices}" 		///
			) 														///
			coeflabels( 											///
				2.donor_pool = "Urban 95%" 							///
				3.donor_pool = "COVID Match" 						///
				4.donor_pool = "Demog. Match" 						///
				5.donor_pool = "Stringency Match" 					///
				2.data_src   = "IRS (ACS Counties)" 				///
				3.data_src   = "ACS (All 25+)" 						///
				4.data_src   = "ACS (College)" 						///
				outstate     = "Out-of-State Movers" 				///
				period_1624  = "Extended Period (16-24)" 			///
				controls     = "With Covariates" 					///
				exclusion    = "Exclude 2020" 						///
			) 														///
			msymbol(D) mcolor("`col_main'") 						///
			ciopts(lcolor("`col_main'")) 							///
			graphregion(color(white)) 								///
			title("`otype_label': `migr_label'", size(medium)) 		///
			subtitle("N = `n' specifications, R-sq = `r2'", 		///
				size(small)) 										///
			xtitle("Effect on SDID Estimate (pp)", size(small)) 	///
			note("OLS with robust SEs. Each observation is one SDID specification." , size(vsmall))

		** Export
		graph export "${results}sdid/fig_sdid_influence_`otype'_`migr'.pdf", replace
		graph export "${results}sdid/fig_sdid_influence_`otype'_`migr'.jpg", ///
			as(jpg) quality(100) replace
		** Overleaf copy
		if ${overleaf} == 1 {
			graph export 											///
				"${ol_fig}fig_sdid_influence_`otype'_`migr'.pdf", replace
		}

		restore

	} // END MIGRATION LOOP
} // END OUTCOME TYPE LOOP

********************************************************************************
** FINISH
********************************************************************************

dis ""
dis "=============================================="
dis "SDID RESULTS ANALYSIS COMPLETE"
dis "=============================================="
dis "Coefplots saved to:"
dis "  - ${results}sdid/fig_sdid_influence_*.pdf"
dis "=============================================="

** Close log
clear
log close log_xx
