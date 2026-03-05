/*******************************************************************************
File Name: 		02_elasticities.do
Creator: 		John Iselin
Date Update:	February 2026

Purpose: 	Calculate flow and stock elasticities of migration with respect to the
			Preschool for All (PFA) income tax from SDID treatment effect estimates.

			Formulas:
			  Flow semi-elasticity:  ε_semi = τ / (Δt × 100)
			  Flow elasticity:       ε = (τ / pre_mean) / Δln(1−t)
			  Stock semi-elasticity: ε_semi × T
			  Stock elasticity:      ε × T

			where τ = SDID ATT (pp), Δt = avg effective PFA rate,
			Δln(1−t) = ln(1 − avg_mt_rate), and T = post-treatment years.

Called by: 	00_multnomah.do
Requires:	${data}working/revenue_parameters.dta (from 02_revenue.do)
			${results}sdid/sdid_results.dta (from 02_sdid_analysis.do)

Authors: John Iselin

For more information, contact john.iselin@yale.edu
*******************************************************************************/

********************************************************************************
** SECTION 0: Setup & Parameters
********************************************************************************

** Start log file
capture log close log_02elast
log using "${logs}02_log_elasticities_${date}", name(log_02elast) replace text

** Create output directory
capture mkdir "${results}elasticities"

dis ""
dis "=============================================="
dis "Section 0: Load parameters"
dis "=============================================="

** Load revenue parameters exported by 02_revenue.do
capture confirm file "${data}working/revenue_parameters.dta"
if _rc != 0 {
	dis as error "ERROR: revenue_parameters.dta not found."
	dis as error "       Run 02_revenue.do first."
	log close log_02elast
	error 601
}

preserve
use "${data}working/revenue_parameters.dta", clear
scalar avg_mt_rate = avg_mt_rate[1]
scalar avg_state_rate = avg_state_rate[1]
scalar baseline_pfa_revenue = baseline_pfa_revenue[1]
scalar total_agi_2022 = total_agi_2022[1]
restore

** Compute tax change terms
** PFA was new in 2021, so Δt = avg_mt_rate (change from 0)
scalar delta_t = avg_mt_rate
scalar delta_ln_ntr = ln(1 - avg_mt_rate)		// Δln(1−t), negative for tax increase

** Post-treatment horizon (T)
** IRS data: 2016-2022, treatment starts 2021 → T = 2 (2021, 2022)
** ACS data: 2016-2024, treatment starts 2021 → T = 4 (2021, 2022, 2023, 2024)
local T_irs = 2
local T_acs = 4

dis ""
dis "Revenue parameters:"
dis "  avg_mt_rate     = " %10.6f avg_mt_rate
dis "  avg_state_rate  = " %10.6f avg_state_rate
dis "  Δt              = " %10.6f delta_t
dis "  Δln(1−t)        = " %10.6f delta_ln_ntr
dis "  T_irs           = `T_irs'"
dis "  T_acs           = `T_acs'"

** Sanity check: avg_mt_rate should be in a reasonable range
if delta_t < 0.001 | delta_t > 0.05 {
	dis as error "WARNING: avg_mt_rate = " %8.6f delta_t " — outside expected range [0.001, 0.05]"
	dis as error "         Elasticities may be very large or small. Verify 02_revenue.do output."
}

********************************************************************************
** SECTION 1: Load and Filter SDID Results
********************************************************************************

dis ""
dis "=============================================="
dis "Section 1: Load and filter SDID results"
dis "=============================================="

** Check that SDID results exist
capture confirm file "${results}sdid/sdid_results.dta"
if _rc != 0 {
	dis as error "ERROR: sdid_results.dta not found."
	dis as error "       Run 02_sdid_analysis.do first."
	log close log_02elast
	error 601
}

use "${results}sdid/sdid_results.dta", clear

dis "Total specifications loaded: " _N

** ---- Parse outcome variable to extract components ----
** (Mirrors 02_sdid_analysis.do specification curve section)

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

** ---- Define preferred specifications ----
** (Matches 02_sdid_analysis.do lines 1440-1462 exactly)

gen preferred = 0

** IRS FULL SAMPLE
replace preferred = 1 if 									///
	data_type == "IRS" & 									///
	period_type == "16-22" &								///
	inlist(sample, "sample_all", "sample_urban95_covid") &	///
	controls == 1 &											///
	exclusion == 1 											//

** ACS COLLEGE SAMPLE
replace preferred = 1 if 									///
	data_type == "ACS College" & 							///
	period_type == "16-24" &								///
	inlist(sample, "sample_all", "sample_urban95_covid") &	///
	controls == 1 &											///
	exclusion == 1 											//

** ACS COLLEGE OUT-OF-STATE SAMPLE
replace preferred = 1 if 									///
	data_type == "ACS College (Out-of-State)" & 			///
	period_type == "16-24" &								///
	inlist(sample, "sample_all", "sample_urban95_covid") &	///
	controls == 1 &											///
	exclusion == 1 											//

dis ""
dis "Preferred specifications: "
count if preferred == 1
local n_preferred = r(N)

if `n_preferred' == 0 {
	dis as error "ERROR: No preferred specifications found. Check filter criteria."
	log close log_02elast
	error 2000
}

** Keep only preferred specs
keep if preferred == 1

** ---- Assign post-treatment horizon (T) ----
gen byte T = .
replace T = `T_irs' if strpos(data_type, "IRS") > 0
replace T = `T_acs' if strpos(data_type, "ACS") > 0
label var T "Post-treatment years (IRS=2, ACS=4)"

** Check for missing pre_mean
count if missing(pre_mean) | pre_mean == 0
if r(N) > 0 {
	dis "WARNING: " r(N) " preferred specs have missing or zero pre_mean."
	dis "         These will have missing net-of-tax elasticities."
	list sample_data outcome migration pre_mean if missing(pre_mean) | pre_mean == 0
}

** Display preferred specs
dis ""
dis "Preferred specifications for elasticity calculation:"
list data_type sample outcome_type migration tau se pre_mean T, sep(0)

********************************************************************************
** SECTION 2: Compute Elasticities
********************************************************************************

dis ""
dis "=============================================="
dis "Section 2: Compute elasticities"
dis "=============================================="

** ---- Flow semi-elasticity ----
** ε_flow_semi = τ / (Δt × 100)
** Units: pp of migration rate per pp of tax rate
gen double flow_semi_e = tau / (delta_t * 100)
gen double flow_semi_se = se / (delta_t * 100)
gen double flow_semi_ci_lo = flow_semi_e - 1.96 * flow_semi_se
gen double flow_semi_ci_hi = flow_semi_e + 1.96 * flow_semi_se

** ---- Flow elasticity (net-of-tax) ----
** ε_flow = (τ / pre_mean) / Δln(1−t)
** Unitless: % change in migration rate per % change in net-of-tax rate
gen double flow_e = (tau / pre_mean) / delta_ln_ntr if !missing(pre_mean) & pre_mean != 0
gen double flow_se = (se / abs(pre_mean)) / abs(delta_ln_ntr) if !missing(pre_mean) & pre_mean != 0
gen double flow_ci_lo = flow_e - 1.96 * flow_se
gen double flow_ci_hi = flow_e + 1.96 * flow_se

** ---- Stock semi-elasticity ----
** ε_stock_semi = (τ × T) / (Δt × 100)
gen double stock_semi_e = tau * T / (delta_t * 100)
gen double stock_semi_se = se * T / (delta_t * 100)
gen double stock_semi_ci_lo = stock_semi_e - 1.96 * stock_semi_se
gen double stock_semi_ci_hi = stock_semi_e + 1.96 * stock_semi_se

** ---- Stock elasticity (net-of-tax) ----
** ε_stock = (τ × T / pre_mean) / Δln(1−t)
gen double stock_e = (tau * T / pre_mean) / delta_ln_ntr if !missing(pre_mean) & pre_mean != 0
gen double stock_se = (se * T / abs(pre_mean)) / abs(delta_ln_ntr) if !missing(pre_mean) & pre_mean != 0
gen double stock_ci_lo = stock_e - 1.96 * stock_se
gen double stock_ci_hi = stock_e + 1.96 * stock_se

** Labels
label var flow_semi_e "Flow semi-elasticity (pp mig rate per pp tax rate)"
label var flow_semi_se "SE: flow semi-elasticity"
label var flow_semi_ci_lo "95% CI lower: flow semi-elasticity"
label var flow_semi_ci_hi "95% CI upper: flow semi-elasticity"
label var flow_e "Flow elasticity (net-of-tax)"
label var flow_se "SE: flow elasticity"
label var flow_ci_lo "95% CI lower: flow elasticity"
label var flow_ci_hi "95% CI upper: flow elasticity"
label var stock_semi_e "Stock semi-elasticity (cumulative pp per pp tax rate)"
label var stock_semi_se "SE: stock semi-elasticity"
label var stock_semi_ci_lo "95% CI lower: stock semi-elasticity"
label var stock_semi_ci_hi "95% CI upper: stock semi-elasticity"
label var stock_e "Stock elasticity (net-of-tax, cumulative)"
label var stock_se "SE: stock elasticity"
label var stock_ci_lo "95% CI lower: stock elasticity"
label var stock_ci_hi "95% CI upper: stock elasticity"

** ---- Sign check ----
** For out-migration with a tax increase: τ > 0 (more out-migration) and
** Δln(1−t) < 0 → flow_e should be negative (out-migration rises when
** net-of-tax falls). For in-migration: τ < 0 and Δln(1−t) < 0 → flow_e
** should be positive.
dis ""
dis "Sign check (expect flow_e < 0 for out-migration, > 0 for in-migration):"
list data_type migration tau flow_e if !missing(flow_e), sep(0)

********************************************************************************
** SECTION 3: Output Tables
********************************************************************************

dis ""
dis "=============================================="
dis "Section 3: Output tables"
dis "=============================================="

** ---- Create readable labels ----
gen str80 row_label = ""
replace row_label = data_type + ", " + sample + ": "
replace row_label = row_label + proper(outcome_type) + " " + proper(migration)

** Outcome type labels for table
gen str30 otype_label = ""
replace otype_label = "Returns" if outcome_type == "n1"
replace otype_label = "Exemptions" if outcome_type == "n2"
replace otype_label = "AGI" if outcome_type == "agi"

** Migration labels for table
gen str20 migr_label = ""
replace migr_label = "Net" if migration == "net"
replace migr_label = "In" if migration == "in"
replace migr_label = "Out" if migration == "out"

** ---- Export to Excel ----
preserve

** Keep display variables
keep data_type sample otype_label migr_label T ///
	tau se pre_mean ///
	flow_semi_e flow_semi_se ///
	flow_e flow_se ///
	stock_semi_e stock_semi_se ///
	stock_e stock_se ///
	flow_semi_ci_lo flow_semi_ci_hi ///
	flow_ci_lo flow_ci_hi ///
	stock_semi_ci_lo stock_semi_ci_hi ///
	stock_ci_lo stock_ci_hi

order data_type sample otype_label migr_label T ///
	tau se pre_mean ///
	flow_semi_e flow_semi_se ///
	flow_e flow_se ///
	stock_semi_e stock_semi_se ///
	stock_e stock_se

export excel using "${results}elasticities/tbl_elasticities.xlsx", ///
	firstrow(variables) replace

restore

** ---- Export LaTeX table ----
** Format: one row per spec, columns for τ and the four elasticity types
** Focus on net migration for the main table

preserve

** Build formatted string columns for LaTeX
gen str12 tau_str = string(tau, "%9.3f")
gen str12 se_str = "(" + string(se, "%9.3f") + ")"
gen str12 fsemi_str = string(flow_semi_e, "%9.3f")
gen str12 fsemi_se_str = "(" + string(flow_semi_se, "%9.3f") + ")"
gen str12 fe_str = string(flow_e, "%9.3f") if !missing(flow_e)
gen str12 fe_se_str = "(" + string(flow_se, "%9.3f") + ")" if !missing(flow_se)
gen str12 ssemi_str = string(stock_semi_e, "%9.3f")
gen str12 ssemi_se_str = "(" + string(stock_semi_se, "%9.3f") + ")"
gen str12 ste_str = string(stock_e, "%9.3f") if !missing(stock_e)
gen str12 ste_se_str = "(" + string(stock_se, "%9.3f") + ")" if !missing(stock_e)

** Write LaTeX table manually
tempname fh
file open `fh' using "${results}elasticities/tbl_elasticities.tex", write replace

file write `fh' "\begin{table}[htbp]" _n
file write `fh' "\centering" _n
file write `fh' "\begin{threeparttable}" _n
file write `fh' "\caption{Elasticities of Migration with Respect to PFA Tax}" _n
file write `fh' "\label{tab:elasticities}" _n
file write `fh' "\begin{adjustbox}{max width=\textwidth}" _n
file write `fh' "\begin{tabular}{llll ccccc}" _n
file write `fh' "\toprule" _n
file write `fh' " & & & & & \multicolumn{2}{c}{Flow} & \multicolumn{2}{c}{Stock} \\" _n
file write `fh' "\cmidrule(lr){6-7} \cmidrule(lr){8-9}" _n
file write `fh' "Data & Sample & Outcome & Direction & $\hat{\tau}$ (pp) & Semi-$\varepsilon$ & $\varepsilon$ & Semi-$\varepsilon$ & $\varepsilon$ \\" _n
file write `fh' "\midrule" _n

** Sort for table output
sort data_type sample outcome_type migration
local N = _N
local prev_dt = ""

forvalues i = 1/`N' {
	local dt = data_type[`i']
	local smp = sample[`i']
	local ot = otype_label[`i']
	local mg = migr_label[`i']
	local t_val = tau_str[`i']
	local se_val = se_str[`i']
	local fs = fsemi_str[`i']
	local fs_se = fsemi_se_str[`i']
	local fe = fe_str[`i']
	local fe_se = fe_se_str[`i']
	local ss = ssemi_str[`i']
	local ss_se = ssemi_se_str[`i']
	local ste = ste_str[`i']
	local ste_se = ste_se_str[`i']

	** Handle missing elasticities
	if "`fe'" == "" local fe "--"
	if "`fe_se'" == "" local fe_se ""
	if "`ste'" == "" local ste "--"
	if "`ste_se'" == "" local ste_se ""

	** Clean up sample name for display
	local smp_clean = subinstr("`smp'", "sample_", "", .)

	** Add spacing between data-type groups
	if "`prev_dt'" != "" & "`prev_dt'" != "`dt'" {
		file write `fh' "\addlinespace" _n
	}
	local prev_dt "`dt'"

	** Point estimates row
	file write `fh' "`dt' & `smp_clean' & `ot' & `mg' & `t_val' & `fs' & `fe' & `ss' & `ste' \\" _n
	** Standard errors row
	file write `fh' " & & & & `se_val' & `fs_se' & `fe_se' & `ss_se' & `ste_se' \\" _n
}

file write `fh' "\bottomrule" _n
file write `fh' "\end{tabular}" _n
file write `fh' "\end{adjustbox}" _n
file write `fh' "\begin{tablenotes}" _n
file write `fh' "\small" _n
file write `fh' "\item \textit{Notes:} " _n
file write `fh' "Semi-elasticity: pp change in migration rate per pp of tax rate. " _n
file write `fh' "Elasticity: \% change in migration rate per \% change in net-of-tax rate (1$-$t). " _n
file write `fh' "Stock elasticities accumulate the annual flow effect over $T$ post-treatment years " _n
file write `fh' "(IRS: $T=2$, ACS: $T=4$). " _n
file write `fh' "Average effective PFA rate: " string(delta_t * 100, "%5.3f") "\%. " _n
file write `fh' "Standard errors in parentheses, derived from SDID bootstrap SEs." _n
file write `fh' "\end{tablenotes}" _n
file write `fh' "\end{threeparttable}" _n
file write `fh' "\end{table}" _n

file close `fh'

restore

** ---- Copy to Overleaf if enabled ----
if ${overleaf} == 1 {
	copy "${results}elasticities/tbl_elasticities.tex" ///
		"${ol_tab}tbl_elasticities.tex", replace
}

dis "LaTeX table exported to: ${results}elasticities/tbl_elasticities.tex"
dis "Excel table exported to: ${results}elasticities/tbl_elasticities.xlsx"

********************************************************************************
** SECTION 4: Display Summary & Save Results
********************************************************************************

dis ""
dis "=============================================="
dis "Section 4: Summary"
dis "=============================================="

dis ""
dis "=================================================================="
dis "ELASTICITY SUMMARY — PREFERRED SPECIFICATIONS"
dis "=================================================================="
dis ""
dis "Average effective PFA rate (Δt):  " %8.4f delta_t " (" %5.3f delta_t * 100 "%)"
dis "Δln(1−t):                         " %8.6f delta_ln_ntr
dis ""
dis "------- Flow Elasticities -------"

** Display key results in log
list data_type sample outcome_type migration tau se ///
	flow_semi_e flow_e stock_semi_e stock_e, ///
	sep(0) abbreviate(20)

dis ""
dis "=================================================================="

** ---- Save results dataset ----
save "${results}elasticities/elasticity_results.dta", replace

dis ""
dis "=============================================="
dis "02_elasticities.do complete."
dis "Output files:"
dis "  ${results}elasticities/tbl_elasticities.tex"
dis "  ${results}elasticities/tbl_elasticities.xlsx"
dis "  ${results}elasticities/elasticity_results.dta"
dis "=============================================="

capture log close log_02elast
