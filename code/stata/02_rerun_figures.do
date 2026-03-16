/*******************************************************************************
File Name:    02_rerun_figures.do
Creator:      John Iselin
Date Updated: March 2026

Purpose: Re-run ONLY the specification curve and influence figure sections
         from 02_sdid_analysis.do, without re-estimating SDID results.
         Requires sdid_results.dta to already exist.

Called by: standalone (run from code/stata/ or project root)
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

** Verify sdid_results.dta exists
capture confirm file "${results}sdid/sdid_results.dta"
if _rc != 0 {
	di as error "ERROR: ${results}sdid/sdid_results.dta not found."
	di as error "       Run 02_sdid_analysis.do first to generate results."
	exit 601
}

** Start log
capture log close log_02_rerun
log using "${logs}02_log_rerun_figures_${date}", replace text name(log_02_rerun)

dis ""
dis "=============================================="
dis "RE-RUNNING SPEC CURVES + INFLUENCE FIGURES"
dis "=============================================="

********************************************************************************
** PALETTE (must match 02_sdid_analysis.do)
********************************************************************************

local col_sig_notpref  "0 114 178"    // sea (p7)
local col_insig_notpref "86 180 233"  // sky (p3)
local col_sig_pref     "213 94 0"     // vermillion (p6)
local col_insig_pref   "230 159 0"    // orangebrown (p8)
local col_zero         "204 121 167"  // reddish (p5)
local col_ref          "153 153 153"  // gs10 (p2)

********************************************************************************
** SPECIFICATION CURVE ANALYSIS
********************************************************************************

capture mkdir "${results}sdid/spec_curves"

use "${results}sdid/sdid_results.dta", clear

** Parse outcome variable names to extract components
gen outcome_type = ""
replace outcome_type = "n1" if strpos(outcome, "n1_") > 0
replace outcome_type = "n2" if strpos(outcome, "n2_") > 0
replace outcome_type = "agi" if strpos(outcome, "agi_") > 0

gen migration = ""
replace migration = "net" if strpos(outcome, "_net_") > 0
replace migration = "in" if strpos(outcome, "_in_") > 0
replace migration = "out" if strpos(outcome, "_out_") > 0

gen data_type = ""
replace data_type = "IRS" if strpos(outcome, "_irs") > 0 & strpos(outcome, "_irs_outstate") == 0
replace data_type = "IRS (Out-of-State)" if strpos(outcome, "_irs_outstate") > 0
replace data_type = "IRS (389)" if strpos(sample_data, "irs_389") > 0 & strpos(outcome, "_irs_outstate") == 0
replace data_type = "IRS (389, Out-of-State)" if strpos(sample_data, "irs_389") > 0 & strpos(outcome, "_irs_outstate") > 0
replace data_type = "ACS All (Out-of-State)" if strpos(outcome, "_acs1_outstate") > 0
replace data_type = "ACS College (Out-of-State)" if strpos(outcome, "_acs2_outstate") > 0
replace data_type = "ACS All" if strpos(outcome, "_acs1") > 0 & strpos(outcome, "_acs1_outstate") == 0
replace data_type = "ACS College" if strpos(outcome, "_acs2") > 0 & strpos(outcome, "_acs2_outstate") == 0

gen period_type = ""
replace period_type = "16-22" if strpos(outcome, "_irs") > 0
replace period_type = "16-22" if strpos(sample_data, "16_22") > 0
replace period_type = "16-24" if strpos(sample_data, "16_24") > 0

** Create specification indicators for bottom panel
gen spec_all = sample == "sample_all"
gen spec_urban95 = sample == "sample_urban95"
gen spec_covid = sample == "sample_urban75_covid"
gen spec_demog = sample == "sample_demog"
gen spec_stringency = sample == "sample_stringency"
gen spec_16_22 = period_type == "16-22"
gen spec_16_24 = period_type == "16-24"
gen spec_covars = controls == 1
gen spec_excl2020 = exclusion == 1
gen spec_irs = data_type == "IRS"
gen spec_irs_outstate = data_type == "IRS (Out-of-State)"
gen spec_irs_389 = data_type == "IRS (389)"
gen spec_irs_outstate_389 = data_type == "IRS (389, Out-of-State)"
gen spec_acs_all = data_type == "ACS All"
gen spec_acs_col = data_type == "ACS College"
gen spec_acs_all_outstate = data_type == "ACS All (Out-of-State)"
gen spec_acs_col_outstate = data_type == "ACS College (Out-of-State)"

** Calculate statistical significance (p < 0.05)
replace significant = pval < 0.05 if missing(significant)

** Mark preferred specifications
project_mark_preferred_main

** Display count of preferred specifications
dis "Number of preferred specifications: "
count if preferred == 1

********************************************************************************
** CREATE SPECIFICATION CURVE PLOTS
********************************************************************************

foreach otype in "n1" "n2" "agi" {
	foreach migr in "net" "in" "out" {
		foreach pset in "main" "outstate" {

		preserve

		keep if outcome_type == "`otype'" & migration == "`migr'"

		if "`pset'" == "main" {
			drop if inlist(data_type, "IRS (Out-of-State)", "IRS (389, Out-of-State)", ///
								     "ACS All (Out-of-State)", "ACS College (Out-of-State)")
		}
		else if "`pset'" == "outstate" {
			keep if inlist(data_type, "IRS (Out-of-State)", "IRS (389, Out-of-State)", ///
								     "ACS All (Out-of-State)", "ACS College (Out-of-State)")
		}

		qui count
		if r(N) == 0 {
			restore
			continue
		}

		sort tau
		gen spec_rank = _n
		local n_specs = _N

		if "`otype'" == "n1" local otype_label "Returns/Households"
		else if "`otype'" == "n2" local otype_label "Exemptions/Persons"
		else if "`otype'" == "agi" local otype_label "AGI/Income"

		if "`migr'" == "net" local migr_label "Net Migration"
		else if "`migr'" == "in" local migr_label "In-Migration"
		else if "`migr'" == "out" local migr_label "Out-Migration"

		if "`pset'" == "outstate" local pset_title " (Out-of-State)"
		else local pset_title ""

		** Four-category coloring
		gen tau_sig_notpref = tau if significant == 1 & preferred == 0
		gen ci_lo_sig_notpref = ci_lower if significant == 1 & preferred == 0
		gen ci_hi_sig_notpref = ci_upper if significant == 1 & preferred == 0

		gen tau_insig_notpref = tau if significant == 0 & preferred == 0
		gen ci_lo_insig_notpref = ci_lower if significant == 0 & preferred == 0
		gen ci_hi_insig_notpref = ci_upper if significant == 0 & preferred == 0

		gen tau_sig_pref = tau if significant == 1 & preferred == 1
		gen ci_lo_sig_pref = ci_lower if significant == 1 & preferred == 1
		gen ci_hi_sig_pref = ci_upper if significant == 1 & preferred == 1

		gen tau_insig_pref = tau if significant == 0 & preferred == 1
		gen ci_lo_insig_pref = ci_lower if significant == 0 & preferred == 1
		gen ci_hi_insig_pref = ci_upper if significant == 0 & preferred == 1

		qui count if significant == 1 & preferred == 0
		local n_sig_notpref = r(N)
		qui count if significant == 0 & preferred == 0
		local n_insig_notpref = r(N)
		qui count if significant == 1 & preferred == 1
		local n_sig_pref = r(N)
		qui count if significant == 0 & preferred == 1
		local n_insig_pref = r(N)

		** Compute dynamic placement for indicator zone below coefficients
		qui su ci_lower
		local ci_min = r(min)
		qui su ci_upper
		local ci_max = r(max)

		local sep_y = floor(`ci_min') - 1.5
		local ind_top = floor(`ci_min') - 3
		local tick_lo = floor(`ci_min')
		local tick_hi = ceil(`ci_max')

		if "`pset'" == "main" {

		** 13 indicator rows
		local yp1  = `ind_top'
		local yp2  = `ind_top' - 1
		local yp3  = `ind_top' - 2
		local yp4  = `ind_top' - 3
		local yp5  = `ind_top' - 4
		local yp6  = `ind_top' - 5
		local yp7  = `ind_top' - 6
		local yp8  = `ind_top' - 7
		local yp9  = `ind_top' - 8
		local yp10 = `ind_top' - 9
		local yp11 = `ind_top' - 10
		local yp12 = `ind_top' - 11
		local yp13 = `ind_top' - 12

		gen y_all        = `yp1'  if spec_all == 1
		gen y_urban      = `yp2'  if spec_urban95 == 1
		gen y_covid      = `yp3'  if spec_covid == 1
		gen y_demog      = `yp4'  if spec_demog == 1
		gen y_stringency = `yp5'  if spec_stringency == 1
		gen y_covars     = `yp6'  if spec_covars == 1
		gen y_excl       = `yp7'  if spec_excl2020 == 1
		gen y_irs        = `yp8'  if spec_irs == 1
		gen y_irs_389    = `yp9'  if spec_irs_389 == 1
		gen y_acs_all    = `yp10' if spec_acs_all == 1
		gen y_acs_col    = `yp11' if spec_acs_col == 1
		gen y_16_22      = `yp12' if spec_16_22 == 1
		gen y_16_24      = `yp13' if spec_16_24 == 1

		twoway 	(rcap ci_lo_sig_notpref ci_hi_sig_notpref spec_rank, 		///
					lc("`col_sig_notpref'") lw(vthin)) 						///
				(rcap ci_lo_insig_notpref ci_hi_insig_notpref spec_rank, 	///
					lc("`col_insig_notpref'") lw(vthin)) 					///
				(rcap ci_lo_sig_pref ci_hi_sig_pref spec_rank, 				///
					lc("`col_sig_pref'") lw(thin)) 							///
				(rcap ci_lo_insig_pref ci_hi_insig_pref spec_rank, 			///
					lc("`col_insig_pref'") lw(thin)) 						///
				(scatter tau_sig_notpref spec_rank, 						///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter tau_insig_notpref spec_rank, 						///
					mc("`col_insig_notpref'") ms(O) msize(vsmall)) 		///
				(scatter tau_sig_pref spec_rank, 							///
					mc("`col_sig_pref'") ms(D) msize(small)) 				///
				(scatter tau_insig_pref spec_rank, 							///
					mc("`col_insig_pref'") ms(D) msize(small)) 			///
				(scatter y_all spec_rank, 									///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_urban spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_covid spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_demog spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_stringency spec_rank, 							///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_covars spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_excl spec_rank, 									///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_irs spec_rank, 									///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_irs_389 spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_acs_all spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_acs_col spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_16_22 spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_16_24 spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)), 			///
			yline(`sep_y', lc(gs12) lp(solid) lw(vthin)) 					///
			yline(0, lc("`col_zero'") lp(dash)) 							///
			ylabel(`tick_lo'(1)`tick_hi', labsize(vsmall) nogrid) 			///
			ylabel(`yp1'  "All Counties" 									///
				   `yp2'  "Urban (Top 5%)" 									///
				   `yp3'  "COVID Match" 									///
				   `yp4'  "Demographic Match" 								///
				   `yp5'  "Stringency Match" 								///
				   `yp6'  "Covariates" 										///
				   `yp7'  "Excl. 2020" 										///
				   `yp8'  "IRS (all counties)" 								///
				   `yp9'  "IRS (ACS counties)" 								///
				   `yp10' "ACS All" 										///
				   `yp11' "ACS College" 									///
				   `yp12' "16-22" 											///
				   `yp13' "16-24", 											///
				labsize(vsmall) angle(0) notick nogrid add) 				///
			legend(order(5 "Sig. (p<0.05)" 6 "Insig." 						///
						 7 "Sig., Preferred" 8 "Insig., Preferred") 		///
				   rows(1) pos(6) size(vsmall)) 							///
			ytitle("Treatment Effect (pp)", size(vsmall)) 					///
			xtitle("Specification (ranked by effect size)", size(vsmall)) 	///
			title("`otype_label': `migr_label'`pset_title'", size(medium)) 	///
			xlabel(none) 													///
			xscale(range(0.5 `=`n_specs'+0.5')) 							///
			graphregion(color(white)) 										///
			name(speccurve_`otype'_`migr', replace)

		}

		else if "`pset'" == "outstate" {

		** 11 indicator rows
		local yp1  = `ind_top'
		local yp2  = `ind_top' - 1
		local yp3  = `ind_top' - 2
		local yp4  = `ind_top' - 3
		local yp5  = `ind_top' - 4
		local yp6  = `ind_top' - 5
		local yp7  = `ind_top' - 6
		local yp8  = `ind_top' - 7
		local yp9  = `ind_top' - 8
		local yp10 = `ind_top' - 9
		local yp11 = `ind_top' - 10

		gen y_all              = `yp1'  if spec_all == 1
		gen y_urban            = `yp2'  if spec_urban95 == 1
		gen y_covid            = `yp3'  if spec_covid == 1
		gen y_demog            = `yp4'  if spec_demog == 1
		gen y_stringency       = `yp5'  if spec_stringency == 1
		gen y_covars           = `yp6'  if spec_covars == 1
		gen y_excl             = `yp7'  if spec_excl2020 == 1
		gen y_irs_outstate     = `yp8'  if spec_irs_outstate == 1
		gen y_irs_outstate_389 = `yp9'  if spec_irs_outstate_389 == 1
		gen y_acs_all_outstate = `yp10' if spec_acs_all_outstate == 1
		gen y_acs_col_outstate = `yp11' if spec_acs_col_outstate == 1

		twoway 	(rcap ci_lo_sig_notpref ci_hi_sig_notpref spec_rank, 		///
					lc("`col_sig_notpref'") lw(vthin)) 						///
				(rcap ci_lo_insig_notpref ci_hi_insig_notpref spec_rank, 	///
					lc("`col_insig_notpref'") lw(vthin)) 					///
				(rcap ci_lo_sig_pref ci_hi_sig_pref spec_rank, 				///
					lc("`col_sig_pref'") lw(thin)) 							///
				(rcap ci_lo_insig_pref ci_hi_insig_pref spec_rank, 			///
					lc("`col_insig_pref'") lw(thin)) 						///
				(scatter tau_sig_notpref spec_rank, 						///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter tau_insig_notpref spec_rank, 						///
					mc("`col_insig_notpref'") ms(O) msize(vsmall)) 		///
				(scatter tau_sig_pref spec_rank, 							///
					mc("`col_sig_pref'") ms(D) msize(small)) 				///
				(scatter tau_insig_pref spec_rank, 							///
					mc("`col_insig_pref'") ms(D) msize(small)) 			///
				(scatter y_all spec_rank, 									///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_urban spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_covid spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_demog spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_stringency spec_rank, 							///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_covars spec_rank, 								///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_excl spec_rank, 									///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_irs_outstate spec_rank, 							///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_irs_outstate_389 spec_rank, 						///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_acs_all_outstate spec_rank, 						///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)) 			///
				(scatter y_acs_col_outstate spec_rank, 						///
					mc("`col_sig_notpref'") ms(O) msize(vsmall)), 			///
			yline(`sep_y', lc(gs12) lp(solid) lw(vthin)) 					///
			yline(0, lc("`col_zero'") lp(dash)) 							///
			ylabel(`tick_lo'(1)`tick_hi', labsize(vsmall) nogrid) 			///
			ylabel(`yp1'  "All Counties" 									///
				   `yp2'  "Urban (Top 5%)" 									///
				   `yp3'  "COVID Match" 									///
				   `yp4'  "Demographic Match" 								///
				   `yp5'  "Stringency Match" 								///
				   `yp6'  "Covariates" 										///
				   `yp7'  "Excl. 2020" 										///
				   `yp8'  "IRS Out-of-State (all counties)" 				///
				   `yp9'  "IRS Out-of-State (ACS counties)" 				///
				   `yp10' "ACS All (Out-of-State)" 							///
				   `yp11' "ACS College (Out-of-State)", 					///
				labsize(vsmall) angle(0) notick nogrid add) 				///
			legend(order(5 "Sig. (p<0.05)" 6 "Insig." 						///
						 7 "Sig., Preferred" 8 "Insig., Preferred") 		///
				   rows(1) pos(6) size(vsmall)) 							///
			ytitle("Treatment Effect (pp)", size(vsmall)) 					///
			xtitle("Specification (ranked by effect size)", size(vsmall)) 	///
			title("`otype_label': `migr_label'`pset_title'", size(medium)) 	///
			xlabel(none) 													///
			xscale(range(0.5 `=`n_specs'+0.5')) 							///
			graphregion(color(white)) 										///
			name(speccurve_`otype'_`migr', replace)

		}

		if "`pset'" == "outstate" local fsuffix "_outstate"
		else local fsuffix ""

		graph export "${results}sdid/spec_curves/fig_speccurve_`otype'_`migr'`fsuffix'.pdf", replace
		graph export "${results}sdid/spec_curves/fig_speccurve_`otype'_`migr'`fsuffix'.jpg", as(jpg) quality(100) replace
		if ${overleaf} == 1 {
			graph export "${ol_fig}fig_speccurve_`otype'_`migr'`fsuffix'.pdf", replace
		}

		graph drop speccurve_`otype'_`migr'

		restore

		}
	}
}

********************************************************************************
** META-REGRESSION: SPECIFICATION INFLUENCE ANALYSIS
********************************************************************************

local col_pool    "0 114 178"
local col_data    "213 94 0"
local col_other   "0 158 115"
local col_zero    "153 153 153"

capture mkdir "${results}sdid/influence"

use "${results}sdid/sdid_results.dta", clear

gen outcome_type = ""
replace outcome_type = "n1"  if strpos(outcome, "n1_") > 0
replace outcome_type = "n2"  if strpos(outcome, "n2_") > 0
replace outcome_type = "agi" if strpos(outcome, "agi_") > 0

gen migration = ""
replace migration = "net" if strpos(outcome, "_net_") > 0
replace migration = "in"  if strpos(outcome, "_in_") > 0
replace migration = "out" if strpos(outcome, "_out_") > 0

gen donor_pool = .
replace donor_pool = 1 if sample == "sample_all"
replace donor_pool = 2 if sample == "sample_urban95"
replace donor_pool = 3 if sample == "sample_urban75_covid"
replace donor_pool = 4 if sample == "sample_demog"
replace donor_pool = 5 if sample == "sample_stringency"
label define donor_pool_lbl 1 "All Counties" 2 "Urban 95%" 			///
	3 "COVID Match" 4 "Demog. Match" 5 "Stringency"
label values donor_pool donor_pool_lbl

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

gen outstate = strpos(sample_data, "outstate") > 0
label var outstate "Out-of-State Movers"

gen period_1624 = strpos(sample_data, "16_24") > 0
label var period_1624 "Extended Period (16-24)"

label var controls "With Covariates"
label var exclusion "Exclude 2020"

assert !missing(donor_pool)
assert !missing(data_src)

foreach geo in 0 1 {

	if `geo' == 0 {
		local geo_label "Out-of-County"
		local geo_suffix ""
	}
	else {
		local geo_label "Out-of-State"
		local geo_suffix "_outstate"
	}

foreach otype in "n1" "n2" "agi" {
	foreach migr in "out" "in" "net" {

		if "`otype'" == "n1"  local otype_label "Returns"
		else if "`otype'" == "n2"  local otype_label "Exemptions"
		else if "`otype'" == "agi" local otype_label "AGI"

		if "`migr'" == "net" local migr_label "Net Migration"
		else if "`migr'" == "in"  local migr_label "In-Migration"
		else if "`migr'" == "out" local migr_label "Out-Migration"

		preserve
		keep if outcome_type == "`otype'" & migration == "`migr'" ///
			& outstate == `geo'

		qui count
		local n_specs = r(N)
		if `n_specs' < 10 {
			dis "Skipping `otype' `migr' (`geo_label'): only `n_specs' specifications."
			restore
			continue
		}

		reg tau 	ib1.donor_pool 		///
					ib1.data_src 		///
					period_1624 		///
					controls 			///
					exclusion, 			///
					robust
		estimates store meta_full

		local r2 : di %4.3f e(r2)
		local n  : di %4.0f e(N)

		** Panel 1: donor-pool choices
		coefplot meta_full, drop(_cons) noomitted 					///
			keep(2.donor_pool 3.donor_pool 4.donor_pool 5.donor_pool) ///
			xline(0, lc("`col_zero'") lp(dash)) 					///
			coeflabels( 											///
				2.donor_pool = `""Urban" "(Top 5%)""' 				///
				3.donor_pool = `""COVID" "Match""' 					///
				4.donor_pool = `""Demographic" "Match""' 			///
				5.donor_pool = `""Stringency" "Match""' 			///
			) 														///
			msymbol(D) mcolor("`col_pool'") 						///
			ciopts(lcolor("`col_pool'")) 							///
			graphregion(color(white)) plotregion(color(white)) 		///
			title("Donor Pool", size(small)) 						///
			subtitle("", size(vsmall)) 								///
			xtitle("Effect on SDID estimate (pp)", size(vsmall)) 	///
			legend(off)												///
			name(inf_pool_`otype'_`migr'_`geo', replace)

		** Panel 2: data-source choices
		coefplot meta_full, drop(_cons) noomitted 					///
			keep(2.data_src 3.data_src 4.data_src) 				///
			xline(0, lc("`col_zero'") lp(dash)) 					///
			coeflabels( 											///
				2.data_src = `""IRS" "(ACS sample)""' 				///
				3.data_src = `""ACS" "(All 25+)""' 					///
				4.data_src = `""ACS" "(College)""' 					///
			) 														///
			msymbol(D) mcolor("`col_data'") 						///
			ciopts(lcolor("`col_data'")) 							///
			graphregion(color(white)) plotregion(color(white)) 		///
			title("Data Source", size(small)) 						///
			subtitle("", size(vsmall)) 								///
			xtitle("Effect on SDID estimate (pp)", size(vsmall)) 	///
			legend(off)												///
			name(inf_data_`otype'_`migr'_`geo', replace)

		** Panel 3: other specification choices
		coefplot meta_full, drop(_cons) noomitted 					///
			keep(period_1624 controls exclusion) 					///
			xline(0, lc("`col_zero'") lp(dash)) 					///
			coeflabels( 											///
				period_1624 = `""Extended Period" "(16-24)""' 		///
				controls    = "Covariates" 							///
				exclusion   = `""Exclude" "2020""' 					///
			) 														///
			msymbol(D) mcolor("`col_other'") 						///
			ciopts(lcolor("`col_other'")) 							///
			graphregion(color(white)) plotregion(color(white)) 		///
			title("Other Choices", size(small)) 					///
			subtitle("", size(vsmall)) 								///
			xtitle("Effect on SDID estimate (pp)", size(vsmall)) 	///
			legend(off)												///
			name(inf_other_`otype'_`migr'_`geo', replace)

		graph combine 												///
			inf_pool_`otype'_`migr'_`geo' 							///
			inf_data_`otype'_`migr'_`geo' 							///
			inf_other_`otype'_`migr'_`geo', 						///
			cols(3) xcommon imargin(2 2 2 2) 						///
			title("`otype_label': `migr_label'", size(medium)) 		///
			subtitle("`geo_label' | N = `n', R-sq = `r2'", size(small)) ///
			graphregion(color(white))

		graph export "${results}sdid/influence/fig_sdid_influence_`otype'_`migr'`geo_suffix'.pdf", replace
		graph export "${results}sdid/influence/fig_sdid_influence_`otype'_`migr'`geo_suffix'.jpg", ///
			as(jpg) quality(100) replace

		if ${overleaf} == 1 {
			graph export 											///
				"${ol_fig}fig_sdid_influence_`otype'_`migr'`geo_suffix'.pdf", replace
		}

		restore

	}
}
}

********************************************************************************
** FINISH
********************************************************************************

dis ""
dis "=============================================="
dis "FIGURE RE-RUN COMPLETE"
dis "=============================================="
dis "Outputs updated:"
dis "  - ${results}sdid/spec_curves/fig_speccurve_*.pdf/jpg"
dis "  - ${results}sdid/influence/fig_sdid_influence_*.pdf/jpg"
dis "=============================================="

clear
log close log_02_rerun
