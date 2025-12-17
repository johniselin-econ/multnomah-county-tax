/*******************************************************************************
File Name: 		02_indiv_analysis.do
Creator: 		John Iselin
Date Update:	November 29, 2025

Called by: 00_multnomah.do

Purpose: Preform individual-level migration analysis. 

Authors: John Iselin 

For more information, contact john.iselin@yale.edu

*******************************************************************************/

** Start log file 
capture log close log_02
log using "${logs}02_log_individual_${date}", replace text name(log_02)

** Run Program 
capture program drop hdfe_catyear_plot
program define hdfe_catyear_plot
    version 16.0
    syntax varname(numeric) [if] [in] [fw aw pw iw] , ///
        CAT(varname) YEAR(varname) ABSORB(string) ///
        [ WVAR(varname) WTYPE(string) ///
          VCE(string) ///
          TITLE(string) XTITLE(string) YTITLE(string) ///
          NOCI ///
          BASEYEAR(integer 0) ///
          SAVING(string asis) REPLACE ///
          DEBUG DEBUGDETAIL ]

    // ============================================================
    // Configuration: project root for relative paths
    // ============================================================
    local __project_root "C:/Users/ji252/Documents/GitHub/multnomah-county-tax/"

    // ---------------------------
    // Debug helpers (SAFE)
    // ---------------------------
    local __dbg 0
    local __dbgdetail 0
    if "`debug'" != "" local __dbg 1
    if "`debugdetail'" != "" {
        local __dbg 1
        local __dbgdetail 1
    }

    if `__dbg' {
        di as txt "------------------------------------------------------------"
        di as txt "hdfe_catyear_plot DEBUG START"
        di as txt "Outcome   : `varlist'"
        di as txt "CAT()     : `cat'"
        di as txt "YEAR()    : `year'"
        di as txt "ABSORB()  : `absorb'"
        di as txt "Weights   : weight=`weight' exp=`exp' wvar=`wvar' wtype=`wtype'"
        di as txt "VCE()     : `vce'"
        di as txt "BASEYEAR(): `baseyear'   NOCI: `noci'"
        di as txt "SAVING()  : "`saving'"   REPLACE: `replace'"
        di as txt "IF/IN     : `if' `in'"
        di as txt "------------------------------------------------------------"
    }

    // Requirements
    capture which reghdfe
    if _rc {
        di as error "reghdfe not found. Install with: ssc install reghdfe, replace"
        exit 198
    }

    // Mark sample
    if `__dbg' di as txt "[Step 1] Marking estimation sample..."
    marksample touse, strok
    quietly replace `touse' = `touse' & !missing(`varlist', `cat', `year')

    quietly count if `touse'
    if `__dbg' di as txt "  N (after filters): " %12.0gc r(N)
    if r(N) == 0 {
        di as error "No observations available after applying if/in/sample filters."
        exit 2000
    }

    // Weights (default fw=perwt unless user passed weights)
    if `__dbg' di as txt "[Step 2] Resolving weights..."
    local wgt ""
    if "`weight'" != "" {
        local wgt "[`weight'=`exp']"
        if `__dbg' di as txt "  Using caller-provided weights: `wgt'"
    }
    else {
        if "`wvar'" == "" local wvar "perwt"
        if "`wtype'" == "" local wtype "fw"
        capture confirm variable `wvar'
        if _rc {
            di as error "Default weight variable `wvar' not found. Either create it or pass weights explicitly."
            exit 111
        }
        local wgt "[`wtype'=`wvar']"
        if `__dbg' di as txt "  Using default weights: `wgt'"
    }

    // Ensure cat/year numeric for factor vars
    if `__dbg' di as txt "[Step 3] Harmonizing CAT/YEAR types..."
    tempvar __cat __year
    local cattype : type `cat'
    if substr("`cattype'",1,3) == "str" {
        quietly encode `cat' if `touse', gen(`__cat')
        local catv `__cat'
        if `__dbg' di as txt "  Encoded string CAT -> `catv'"
    }
    else {
        local catv `cat'
        if `__dbg' di as txt "  CAT already numeric -> `catv'"
    }

    local yeartype : type `year'
    if substr("`yeartype'",1,3) == "str" {
        quietly destring `year' if `touse', gen(`__year') force
        capture assert !missing(`__year') if `touse'
        if _rc {
            di as error "Year variable is string and could not be cleanly converted to numeric."
            exit 459
        }
        local yearv `__year'
        if `__dbg' di as txt "  Destring YEAR -> `yearv'"
    }
    else {
        local yearv `year'
        if `__dbg' di as txt "  YEAR already numeric -> `yearv'"
    }

    // Guard: category variable should not also be absorbed
    if `__dbg' di as txt "[Step 4] Validating absorb()..."
    if regexm(" `absorb' ", " `catv' ") {
        di as error "Category variable `catv' appears in absorb(): `absorb'. Remove it from absorb() for this run."
        exit 198
    }

    // Levels + label (capture label BEFORE we change datasets)
    if `__dbg' di as txt "[Step 5] Collecting year and category levels..."
    quietly levelsof `yearv' if `touse', local(yrs)
    quietly levelsof `catv'  if `touse', local(cats)
    local vlab : value label `catv'

    if `__dbg' {
        di as txt "  Years: `yrs'"
        di as txt "  Cats : `cats'"
        di as txt "  CAT value label: `vlab'"
    }

    if "`yrs'" == "" | "`cats'" == "" {
        di as error "No year or category levels found in estimation sample."
        exit 2000
    }

    // Base year sanity check (only if baseyear != 0)
    if `baseyear' != 0 {
        if `__dbg' di as txt "[Step 5b] Checking baseyear()..."
        local found 0
        foreach y of local yrs {
            if `y' == `baseyear' local found 1
        }
        if `found' == 0 {
            di as error "baseyear(`baseyear') not found in estimation sample years: `yrs'"
            exit 459
        }
        if `__dbg' di as txt "  baseyear OK: `baseyear'"
    }

    // Regression
    if `__dbg' di as txt "[Step 6] Running reghdfe..."
    if "`vce'" == "" local vce "robust"
    if `__dbg' {
        di as txt "  Command:"
        di as txt "    reghdfe `varlist' i.`catv'#i.`yearv' if `touse' `wgt', absorb(`absorb') vce(`vce') nocons"
    }

    quietly reghdfe `varlist' i.`catv'#i.`yearv' if `touse' `wgt', ///
        absorb(`absorb') vce(`vce') nocons

    // Existing coefficient names
    if `__dbg' di as txt "[Step 7] Extracting coefficients..."
    local bnames : colnames e(b)
    if `__dbg' {
        local nb : word count `bnames'
        di as txt "  # coefficients in e(b): `nb'"
    }
    if `__dbgdetail' {
        di as txt "  First up to 30 coef names:"
        local shown 0
        foreach bn of local bnames {
            local ++shown
            di as txt "    `bn'"
            if `shown' >= 30 continue, break
        }
    }

    // Critical value for CI (fallback to normal if df_r missing)
    tempname crit
    capture scalar `crit' = invttail(e(df_r), 0.025)
    if _rc scalar `crit' = invnormal(0.975)
    if `__dbg' di as txt "  CI critical value used: " %9.4f scalar(`crit')

    // Post to temp dataset
    if `__dbg' di as txt "[Step 8] Posting cat×year coefficients to temp dataset..."
    tempfile coefdata
    tempname posth
    postfile `posth' int cat_level int year double b se ll ul using `coefdata', replace

    local posted 0
    foreach bn of local bnames {
        // Accept base markers: 0b.cat#2015b.year etc.
        if regexm("`bn'", "^([0-9]+)b?\.`catv'#([0-9]+)b?\.`yearv'$") {
            local c = real(regexs(1))
            local y = real(regexs(2))

            scalar __b  = _b[`bn']
            scalar __se = _se[`bn']
            scalar __ll = __b - scalar(`crit')*__se
            scalar __ul = __b + scalar(`crit')*__se

            post `posth' (`c') (`y') (scalar(__b)) (scalar(__se)) (scalar(__ll)) (scalar(__ul))
            local ++posted
        }
    }
    postclose `posth'

    if `__dbg' di as txt "  Rows posted: `posted'"
    if `posted' == 0 {
        di as error "No cat×year coefficients matched the expected naming pattern. Plot cannot be produced."
        exit 459
    }

    preserve
        use `coefdata', clear



        // Apply value label to cat_level (if available)
        if "`vlab'" != "" {
            label values cat_level `vlab'
            if `__dbg' di as txt "  Applied value label to cat_level: `vlab'"
        }

		if `__dbgdetail' {
            di as txt "[DebugDetail] First 8 rows of coefficient dataset:"
            list in 1/8, abbrev(24)
        }
		
        // Optional normalization to baseyear (within-category)
        if `baseyear' != 0 {
            if `__dbg' di as txt "[Step 9] Normalizing to baseyear(`baseyear')..."
            bysort cat_level: egen base_b = max(cond(year==`baseyear', b, .))
            quietly count if missing(base_b)
            if r(N) > 0 {
                di as error "Some categories have no observations in baseyear(`baseyear'). Cannot normalize."
                if `__dbgdetail' {
                    di as txt "Categories missing baseyear:"
                    bysort cat_level: gen __hasbase = !missing(base_b)
                    tab cat_level if __hasbase==0
                    drop __hasbase
                }
                exit 459
            }
            replace b  = b  - base_b
            replace ll = ll - base_b
            replace ul = ul - base_b
            drop base_b
        }

      // Build twoway spec + legend mapping + plotplainblind palette
		if `__dbg' di as txt "[Step 10] Building plot command..."
		local plots ""
		local leg_order ""
		local leg_labels ""
		local pnum 0

		// Legend rows: 1 by default, 2 if more than 4 categories
		local ncat : word count `cats'
		local legrows 1
		if `ncat' > 4 local legrows 2

		// plotplainblind palette
		local collist ""
		if `ncat' == 1 local collist "black"
		else if `ncat' == 2 local collist "black gs10"
		else local collist "sky turquoise orangebrown reddish vermillion sea ananas"

		local ncols : word count `collist'
		local ci 0

		foreach c of local cats {
			quietly count if cat_level == `c'
			if r(N) == 0 continue

			// Legend label: value labels if available (store as PLAIN text)
			local serieslab ""
			if "`vlab'" != "" {
				local tmp : label `vlab' `c'
				if "`tmp'" != "" local serieslab "`tmp'"
				else             local serieslab "`c'"
			}
			else {
				local serieslab "cat=`c'"
			}
			// remove embedded quotes just in case
			local serieslab : subinstr local serieslab `"""' "", all


			// Pick color (cycle if > #colors)
			local ++ci
			local idx = mod(`ci' - 1, `ncols') + 1
			local col : word `idx' of `collist'

			// CI bars (exclude from legend), same color @ 50% opacity
			if "`noci'" == "" {
				local ++pnum
				local plots `plots' ///
					(rcap ll ul year if cat_level==`c', sort ///
						lcolor("`col'%50") legend(off))
			}

			// Connected series (IN legend): line + markers same color
			local ++pnum
			local plots `plots' ///
				(connected b year if cat_level==`c', sort ///
					lcolor("`col'") mcolor("`col'"))

			// Legend: only connected plots; label safely quoted here
			local leg_order  `leg_order' `pnum'
			local leg_labels `leg_labels' label(`pnum' `"`serieslab'"')
		}

		if `"`plots'"' == `""' {
			di as error "No series were built for plotting (plotspec empty)."
			exit 2000
		}

		if `__dbgdetail' {
			di as txt "  legend order: `leg_order'"
			di as txt "  legend labels: `leg_labels'"
			di as txt "  legend rows: `legrows'"
			di as txt "  palette used: `collist'"
		}


		// X axis labels: use actual year values present in coef dataset
		levelsof year, local(xyrs)
		if `__dbg' di as txt "  X-axis years used: `xyrs'"

		// ---------------------------
		// Titles as option-strings (safe quoting)
		// ---------------------------
		local xtitle_input ""
		local ytitle_input ""
		local title_input  ""

		// xtitle
		if `"`xtitle'"' == `""' {
			local xtitle_input "xtitle(Year)"
		}
		else {
			local xtitle_input "xtitle(`xtitle')"
		}

		// ytitle
		if `"`ytitle'"' == `""' {
			if `baseyear' != 0 local ytitle_input `"ytitle(`"`varlist' (relative to `baseyear')"')"'
			else local ytitle_input `"ytitle(`"`varlist' (adjusted mean)"')"'
		}
		else {
			local ytitle_input "ytitle(`ytitle')"
		}

		// title (optional)
		if `"`title'"' != `""' {
			local title_input "title(`title')"
		}


		if `__dbg' {
			di as txt "  xtitle_input: `xtitle_input'"
			di as txt "  ytitle_input: `ytitle_input'"
			di as txt "  title_input : `title_input'"
		}
		
		twoway `plots', ///
			`xtitle_input' ///
			`ytitle_input' ///
			`title_input' ///
			xlabel(`xyrs', angle(45)) ///
			legend(order(`leg_order') `leg_labels' rows(`legrows') position(6)) ///
			yline(0)



        // ---------------------------
        // Export figure (PDF) - robust path handling
        // ---------------------------
       if "`saving'" != "" {
    local outpath `"`saving'"'
    local outpath : subinstr local outpath `"""' "", all
    local outpath : subinstr local outpath "\" "/" , all

    // prepend project root if relative
    if !regexm("`outpath'", "^[A-Za-z]:/") & substr("`outpath'",1,1) != "/" {
        local outpath "`__project_root'`outpath'"
    }

    if !regexm("`outpath'", "\.pdf$") local outpath "`outpath'.pdf"

    local p = strrpos("`outpath'", "/")
    if `p' > 0 {
        local outdir = substr("`outpath'", 1, `p'-1)
        if !direxists("`outdir'") {
            capture mkdir "`outdir'"
            if _rc & !direxists("`outdir'") {
                di as error "Output directory does not exist and could not be created: `outdir'"
                exit 601
            }
        }
    }

    if "`replace'" != "" graph export "`outpath'", as(pdf) replace
    else                graph export "`outpath'", as(pdf)
}


    restore

    if `__dbg' {
        di as txt "hdfe_catyear_plot DEBUG END"
        di as txt "------------------------------------------------------------"
    }
end



** Load ACS Data 
use "${data}working/acs_migration_file", replace 

** Define sample 
drop if qmigplc1 == 4 					// FAILED EDIT OF MIGPLACE 
drop if inlist(state_fips_o, 2, 15) 	// ALASKA AND HAWAII 

** Define indicator for being in Multnomah County in origin/destination year
gen multnomah_o = state_fips_o == 41 & county_fips_o == 51
gen multnomah_d = state_fips_d == 41 & county_fips_d == 51

** Define samples 
gen sample_1 = multnomah_o == 1 				// Multnomah in origin-year 
gen sample_2 = multnomah_o != 1 				// Not in Multnomah
label var sample_1 "Out-migration Sample"
label var sample_1 "In-migration Sample"

** Define moving indicator 
gen out_1 = same_county == 0 
label var out_1 "Moved out of Multnomah"
gen out_2 = multnomah_d == 1 
label var out_2 "Moved to Multnomah"

** Define variables of interest

** Age 
recode age 	(18/24 = 1 "18-24") 	///
			(25/44 = 2 "25-44")		///
			(45/64 = 3 "45-64") 	///
			(65/max = 4 "65+"), 	///
	gen(cat_age)
label var cat_age "Age categories"

** Sex  
gen cat_sex = sex == 2 
label var cat_sex "Female indicator"
label define lb_cat_sex 0 "Male" 1 "Female"
label values cat_sex lb_cat_sex 

** Marital Status 		
recode marst	(1 = 1 "Married")				///
				(2/3 = 2 "Seperated")			///
				(4/5 = 3 "Divourced / Widowed")	///
				(6 = 4 "Single"), 				///
	gen(cat_married)
label var cat_married "Marriage categories"

** Kids at home 
recode nchild 	(0 = 0 "0 Children")					///
				(1 = 1 "1 Child")						///
				(2 = 2 "2 Children")					///
				(3/max = 3 "3+ Children"), 				///
	gen(cat_child)
label var cat_child "Number of Children"

** Education 
recode educd 	(min/61 = 1 "Less than HS") 	///
				(62/71 = 2 "HS Diploma")		///
				(80/100 = 3 "Some College") 	///
				(101/max = 4 "College Degree"),	///
	gen(cat_educ)
label var cat_educ "Education categories"

** Earnings / Income 
gen tmp1 = cpi99 if year == 2023
egen tmp2 = mean(tmp1)
gen cpi =  cpi99 / tmp2 
drop tmp1 tmp2 

** Loop over income variables 
foreach var of varlist inctot incwage incearn ftotinc {
	
	** Update CPI 
	gen real_`var' = round(`var' * cpi) 
	
	** Categorical variables 
	recode real_`var'	(min/-1 = 1 "Negative income") 		///
						(0 = 2 "$0")						///
						(1/24999 = 3 "$1-$25K") 			///
						(25000/49999 = 4 "$25K-$50K") 		///
						(50000/99999 = 5 "$50K-$100K") 		///
						(100000/199999 = 6 "$100K-$200K") 	///
						(200000/max = 7 "$200K+") , 		///
			gen(cat_`var')
			
	** Tab
	tab cat_`var', m 
	
}

** Label variable 
label var cat_inctot "Total personal income categories (real 2023 USD)"
label var cat_incwage "Total wage income categories (real 2023 USD)"
label var cat_incearn "Total earned income categories (real 2023 USD)"
label var cat_ftotinc "Total family income categories (real 2023 USD)"

** Local categorical variables
local catvars "cat_sex cat_married cat_age cat_child cat_educ cat_ftotinc"

** Loop over out- and in-migration 
forvalues i = 1/2 {
	
	if `i' == 1 local ytitle_txt "Out-migration rate (%)"
	if `i' == 2 local ytitle_txt "In-migration rate (%)"

	** Loop over categories
	foreach cat of local catvars {
		
		** FEs 
		local othercats : list catvars - cat		

		** Run Regressions
		hdfe_catyear_plot out_`i' if sample_`i' == 1, 			///
			cat(`cat') 											///
			year(year) 											///
			absorb(state_fips_o county_fips_o `othercats')	 	///
			wvar(perwt) wtype(fw) 								///
			ytitle("`ytitle_txt'")								///
			saving("${results}fig_`cat'_`i'") replace debug debugdetail
		
	} // END CAT LOOP
	
} // END SAMPLE LOOP 
	

** Close log
clear 
log close log_02
