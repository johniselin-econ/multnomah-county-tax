/*******************************************************************************
File Name:       02_indiv_analysis.do
Creator:         John Iselin
Date Update:     November 29, 2025

Called by:       00_multnomah.do

Purpose:         Perform individual-level migration analysis.

Authors:         John Iselin

For more information, contact john.iselin@yale.edu
*******************************************************************************/

** ---------------------------------------------------------------------------
** Start log file
** ---------------------------------------------------------------------------
capture log close log_02
log using "${logs}02_log_individual_${date}", replace text name(log_02)


** ---------------------------------------------------------------------------
** Program: hdfe_catyear_plot
**
** - Runs:      reghdfe y i.cat#i.year, absorb(...) nocons
** - Posts:     cell means and CIs for each cat×year coefficient
** - Plots:     one connected series per category + optional CI caps
** - Exports:   PDF via graph export
**
** Notes:
** - Legend uses category VALUE LABELS. We store those labels immediately after
**   levelsof (before moving into the posted coefficient dataset) to avoid any
**   issues from dataset swaps.
** - CI and line use the same color; CI uses 50% opacity.
** ---------------------------------------------------------------------------
** ---------------------------------------------------------------------------
** Program: hdfe_catyear_plot
**
** Purpose:
**   1) Run reghdfe with a CAT × YEAR interaction and absorbed fixed effects.
**   2) Use margins (rather than reghdfe coefficients) to compute conditional means:
**        margins i.CAT, over(YEAR)
**   3) Plot margins by YEAR with one series per CAT (CI optional).
**
** Notes:
**   - baseyear(0) => plot levels (conditional means).
**   - baseyear(#) => plot within-CAT differences relative to that year.
** ---------------------------------------------------------------------------
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
          SAVING(string) REPLACE ///
          DEBUG DEBUGDETAIL ]

    ** -----------------------------------------------------------------------
    ** Configuration: project root for relative paths
    ** -----------------------------------------------------------------------
    local __project_root "C:/Users/ji252/Documents/GitHub/multnomah-county-tax/"

    ** -----------------------------------------------------------------------
    ** Debug flags
    ** -----------------------------------------------------------------------
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
        di as txt "SAVING()  : `saving'   REPLACE: `replace'"
        di as txt "IF/IN     : `if' `in'"
        di as txt "------------------------------------------------------------"
    }

    ** -----------------------------------------------------------------------
    ** Requirements
    ** -----------------------------------------------------------------------
    capture which reghdfe
    if _rc {
        di as error "reghdfe not found. Install with: ssc install reghdfe, replace"
        exit 198
    }

    ** -----------------------------------------------------------------------
    ** Mark sample
    ** -----------------------------------------------------------------------
    if `__dbg' di as txt "[Step 1] Marking estimation sample..."
    marksample touse, strok
    quietly replace `touse' = `touse' & !missing(`varlist', `cat', `year')

    quietly count if `touse'
    if `__dbg' di as txt "  N (after filters): " %12.0gc r(N)
    if r(N) == 0 {
        di as error "No observations available after applying if/in filters."
        exit 2000
    }

    ** -----------------------------------------------------------------------
    ** Weights (default fw=perwt unless caller provided weights)
    ** -----------------------------------------------------------------------
    if `__dbg' di as txt "[Step 2] Resolving weights..."
    local wgt ""
    if "`weight'" != "" {
        local wgt "[`weight'=`exp']"
        if `__dbg' di as txt "  Using caller-provided weights: `wgt'"
    }
    else {
        if "`wvar'"  == "" local wvar  "perwt"
        if "`wtype'" == "" local wtype "fw"
        capture confirm variable `wvar'
        if _rc {
            di as error "Default weight variable `wvar' not found. Either create it or pass weights explicitly."
            exit 111
        }
        local wgt "[`wtype'=`wvar']"
        if `__dbg' di as txt "  Using default weights: `wgt'"
    }

    ** -----------------------------------------------------------------------
    ** Ensure CAT/YEAR numeric for factor variables
    ** -----------------------------------------------------------------------
    if `__dbg' di as txt "[Step 3] Harmonizing CAT/YEAR types..."
    tempvar __cat __year

    local cattype : type `cat'
    if substr("`cattype'", 1, 3) == "str" {
        quietly encode `cat' if `touse', gen(`__cat')
        local catv `__cat'
        if `__dbg' di as txt "  Encoded string CAT -> `catv'"
    }
    else {
        local catv `cat'
        if `__dbg' di as txt "  CAT already numeric -> `catv'"
    }

    local yeartype : type `year'
    if substr("`yeartype'", 1, 3) == "str" {
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

    ** -----------------------------------------------------------------------
    ** Guard: CAT should not also be absorbed
    ** -----------------------------------------------------------------------
    if `__dbg' di as txt "[Step 4] Validating absorb()..."
    if regexm(" `absorb' ", " `catv' ") {
        di as error "Category variable `catv' appears in absorb(): `absorb'. Remove it from absorb() for this run."
        exit 198
    }

    ** -----------------------------------------------------------------------
    ** Levels and labels (save labels NOW, before switching datasets)
    ** -----------------------------------------------------------------------
    if `__dbg' di as txt "[Step 5] Collecting year and category levels..."
    quietly levelsof `yearv' if `touse', local(yrs)
    quietly levelsof `catv'  if `touse', local(cats)
    local vlab : value label `catv'

    if "`yrs'" == "" | "`cats'" == "" {
        di as error "No year or category levels found in estimation sample."
        exit 2000
    }

    if `__dbg' {
        di as txt "  Years: `yrs'"
        di as txt "  Cats : `cats'"
        di as txt "  CAT value label: `vlab'"
    }

    ** Save category label text into locals (avoids label-loss during preserve/use)
    foreach c of local cats {
        local __key "`c'"
        local __key : subinstr local __key "-" "m", all

        local __lab "`c'"
        if "`vlab'" != "" {
            local __tmp : label `vlab' `c'
            if "`__tmp'" != "" local __lab "`__tmp'"
        }
        local __lab : subinstr local __lab `"""' "", all
        local __lab_`__key' "`__lab'"
    }   // END CAT LABEL SAVE LOOP

    ** -----------------------------------------------------------------------
    ** Base year sanity check (only if baseyear != 0)
    ** -----------------------------------------------------------------------
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
    }   // END BASEYEAR CHECK

    ** -----------------------------------------------------------------------
    ** Regression (supports margins over CAT × YEAR)
    ** -----------------------------------------------------------------------
    if `__dbg' di as txt "[Step 6] Running reghdfe..."
    if "`vce'" == "" local vce "robust"
    if `__dbg' {
        di as txt "  Command:"
        di as txt "    reghdfe `varlist' i.`catv'##i.`yearv' if `touse' `wgt', absorb(`absorb') vce(`vce')"
    }

    quietly reghdfe `varlist' i.`catv'##i.`yearv' if `touse' `wgt', ///
        absorb(`absorb') vce(`vce')

    ** -----------------------------------------------------------------------
    ** Margins: conditional means by CAT over YEAR (save to dataset)
    ** -----------------------------------------------------------------------
    if `__dbg' di as txt "[Step 7] Computing margins and saving results..."
    tempfile __margdata
    quietly margins, at(`catv'=(`cats') `yearv'=(`yrs')) saving(`__margdata', replace) post

    ** -----------------------------------------------------------------------
    ** Plot using the saved margins dataset
    ** -----------------------------------------------------------------------
    preserve

        use `__margdata', clear

        ** Identify CAT and YEAR columns in margins output
        ** We compute margins using at(): at(cat levels × year levels). The saved dataset
        ** therefore stores the grid in _at1 (cat) and _at2 (year) in the order supplied.
        capture confirm variable _at1
        if _rc {
            di as error "margins output missing _at1. Ensure margins is called with: margins, at(`catv'=(`cats') `yearv'=(`yrs')) saving(...)"
            exit 459
        }
        capture confirm variable _at2
        if _rc {
            di as error "margins output missing _at2. Ensure margins is called with: margins, at(`catv'=(`cats') `yearv'=(`yrs')) saving(...)"
            exit 459
        }

        ** Standardize variable names
        rename _at1 cat_level
        rename _at2 year

        capture confirm variable _margin
        if _rc {
            di as error "margins output missing _margin. Cannot proceed."
            exit 459
        }
        rename _margin b

        capture confirm variable _se_margin
        if !_rc rename _se_margin se
        else {
            capture confirm variable _se
            if !_rc rename _se se
        }

        capture confirm variable _ci_lb
        if !_rc rename _ci_lb ll
        capture confirm variable _ci_ub
        if !_rc rename _ci_ub ul
if `__dbgdetail' {
            di as txt "[DebugDetail] First 8 rows of margins dataset:"
            list in 1/8, abbrev(24)
        }

        ** Ensure year is numeric and sorted
        capture confirm numeric variable year
        if _rc {
            di as error "Year in margins dataset is not numeric; cannot plot on x-axis."
            exit 459
        }

        ** Optional normalization to baseyear (within-category)
        if `baseyear' != 0 {
            if `__dbg' di as txt "[Step 8] Normalizing margins to baseyear(`baseyear')..."
            bysort cat_level: egen base_b = max(cond(year==`baseyear', b, .))
            quietly count if missing(base_b)
            if r(N) > 0 {
                di as error "Some categories have no observations in baseyear(`baseyear'). Cannot normalize."
                exit 459
            }
            replace b  = b  - base_b
            capture confirm variable ll
            if !_rc replace ll = ll - base_b
            capture confirm variable ul
            if !_rc replace ul = ul - base_b
            drop base_b
        }   // END BASEYEAR NORMALIZATION

        ** X axis labels (years present in margins results)
        levelsof year, local(xyrs)
        if `__dbg' di as txt "  X-axis years used: `xyrs'"

        ** -------------------------------------------------------------------
        ** Plot styling: use plotplainblind palette if available (scheme-level)
        ** -------------------------------------------------------------------
        local __oldscheme "`c(scheme)'"
        capture set scheme plotplainblind
        if _rc {
            if `__dbg' di as txt "  plotplainblind scheme not available; using current scheme: `__oldscheme'"
        }
        else {
            if `__dbg' di as txt "  Using scheme: plotplainblind (was `__oldscheme')"
        }

        ** -------------------------------------------------------------------
        ** Build twoway spec + legend mapping (one label per series)
        **   - CI and line share the same pstyle; CI uses 50% opacity.
        ** -------------------------------------------------------------------
        if `__dbg' di as txt "[Step 9] Building plot command..."
        local plots ""
        local leg_order ""
        local leg_labels ""
        local pnum 0

        local cats_n : word count `cats'
        local legrows 1
        if `cats_n' > 4 local legrows 2

        local i 0
        foreach c of local cats {
            quietly count if cat_level == `c'
            if r(N) == 0 continue

            local ++i
            local pstyle "p`i'"

            ** Retrieve pre-saved label text (safe key)
            local __key "`c'"
            local __key : subinstr local __key "-" "m", all
            local serieslab "`__lab_`__key''"

            ** CI (excluded from legend)
            if "`noci'" == "" {
                local ++pnum
                local plots `plots' ///
                    (rcap ll ul year if cat_level==`c', sort ///
                        pstyle(`pstyle') lcolor(%50))
            }

            ** Line (in legend)
            local ++pnum
            local plots `plots' ///
                (connected b year if cat_level==`c', sort ///
                    pstyle(`pstyle') lp(solid) msym(O) )

            local leg_order  `leg_order' `pnum'
            local leg_labels `leg_labels' label(`pnum' `serieslab')
        }   // END CATEGORY PLOT LOOP

        if `"`plots'"' == `""' {
            di as error "No series were built for plotting (plotspec empty)."
            exit 2000
        }

        if `__dbgdetail' {
            di as txt "  legend order: `leg_order'"
            di as txt "  legend labels: `leg_labels'"
            di as txt "  legend rows: `legrows'"
        }

        ** -------------------------------------------------------------------
        ** Titles (safe quoting)
        ** -------------------------------------------------------------------
        local xtitle_input ""
        local ytitle_input ""
        local title_input  ""

        if `"`xtitle'"' == `""' local xtitle_input "xtitle(Year)"
        else                    local xtitle_input `"xtitle(`"`xtitle'"')"'

        if `"`ytitle'"' == `""' {
            if `baseyear' != 0 local ytitle_input `"ytitle(`"`varlist' (relative to `baseyear')"')"'
            else              local ytitle_input `"ytitle(`"`varlist' (adjusted mean)"')"'
        }
        else {
            local ytitle_input `"ytitle(`"`ytitle'"')"'
        }

        if `"`title'"' != `""' local title_input `"title(`"`title'"')"'

		** ------------------------------------------------------------
		** Y-axis: round "nice" ticks (e.g., 0,2,4,6,8,10)
		** Uses CI bounds to set max, then rounds to a nice step.
		** ------------------------------------------------------------

		quietly summarize ll, meanonly
		local __ymin = r(min)

		quietly summarize ul, meanonly
		local __ymax = r(max)

		** Always include 0 on axis
		if `__ymin' > 0 local __ymin = 0
		if `__ymax' < 0 local __ymax = 0

		** If plotting levels (baseyear==0), anchor at 0
		if `baseyear' == 0 local __ymin = 0

		** Add a little headroom (5%)
		local __ymax = `__ymax' * 1.05

		** Decide on a "nice" step aiming for ~5 intervals
		local __span = `__ymax' - `__ymin'
		if `__span' <= 0 local __span = 1

		local __rawstep = `__span'/5

		** Snap step to {1,2,5} * 10^k
		local __k = floor(log10(`__rawstep'))
		local __base = 10^`__k'
		local __mant = `__rawstep'/`__base'

		local __m = 1
		if `__mant' > 1  local __m = 2
		if `__mant' > 2  local __m = 5
		if `__mant' > 5  local __m = 10

		local __ystep = `__m' * `__base'

		** Round max up to nearest multiple of step; min down similarly
		local __yhigh = ceil(`__ymax'/`__ystep') * `__ystep'
		local __ylow  = floor(`__ymin'/`__ystep') * `__ystep'

		** For levels, ensure bottom is exactly 0
		if `baseyear' == 0 local __ylow = 0

		local __yaxis_opts `"yscale(range(`__ylow' `__yhigh')) ylabel(`__ylow'(`__ystep')`__yhigh')"'

        twoway `plots', ///
            `xtitle_input' ///
            `ytitle_input' ///
            `title_input' ///
            xlabel(`xyrs', angle(45)) ///
			`__yaxis_opts' ///           
			legend(order(`leg_order') `leg_labels' rows(`legrows') position(6)) 

        ** Restore scheme (if we successfully switched)
        capture set scheme `__oldscheme'

        ** -------------------------------------------------------------------
        ** Export figure (PDF) - robust path handling
        ** -------------------------------------------------------------------
        if "`saving'" != "" {

            local outpath `"`saving'"'
            local outpath : subinstr local outpath `"""' "", all
            local outpath : subinstr local outpath "\" "/" , all

            ** Prepend project root if relative
            if !regexm("`outpath'", "^[A-Za-z]:/") & substr("`outpath'", 1, 1) != "/" {
                local outpath "`__project_root'`outpath'"
            }

            ** Ensure .pdf extension
            if !regexm("`outpath'", "\.pdf$") local outpath "`outpath'.pdf"

            ** Ensure output directory exists
            local p = strrpos("`outpath'", "/")
            if `p' > 0 {
                local outdir = substr("`outpath'", 1, `p' - 1)
                if !direxists("`outdir'") {
                    if `__dbg' di as txt "  Creating output directory: `outdir'"
                    capture mkdir "`outdir'"
                    if _rc & !direxists("`outdir'") {
                        di as error "Output directory does not exist and could not be created: `outdir'"
                        exit 601
                    }
                }
            }

            if `__dbg' di as txt "[Step 10] Exporting graph to: `outpath'"

            if "`replace'" != "" graph export "`outpath'", as(pdf) replace
            else                graph export "`outpath'", as(pdf)

        }   // END PDF EXPORT

    restore    // END PRESERVE BLOCK

    if `__dbg' {
        di as txt "hdfe_catyear_plot DEBUG END"
        di as txt "------------------------------------------------------------"
    }

end

** ---------------------------------------------------------------------------
** Load ACS migration data
** ---------------------------------------------------------------------------
use "${data}working/acs_migration_file", replace

** Sample restrictions
drop if year == 2015 					// Sample: 2016-2024
drop if qmigplc1 == 4					// Error in migration place 
drop if inlist(state_fips_o, 2, 15)   	// Alaska and Hawaii
drop if inlist(state_fips_d, 2, 15)   	// Alaska and Hawaii
drop if ftotinc < 0 					// Negative Family Income 	
drop if age < 25 						// Dropping under 25

** ---------------------------------------------------------------------------
** Multnomah indicators and samples
** ---------------------------------------------------------------------------
gen multnomah_o = (state_fips_o == 41 & county_fips_o == 51)
gen multnomah_d = (state_fips_d == 41 & county_fips_d == 51)

gen sample_1 = (multnomah_o == 1)      // Multnomah in origin-year
gen sample_2 = (multnomah_o != 1)      // Not Multnomah in origin-year
label var sample_1 "Out-migration Sample"
label var sample_2 "In-migration Sample"

gen out_1 = (same_county == 0)
replace out_1 = out_1 * 100
label var out_1 "Moved out of Multnomah"

gen out_2 = (multnomah_d == 1)
replace out_2 = out_2 * 100 
label var out_2 "Moved to Multnomah"


** ---------------------------------------------------------------------------
** Covariate categories
** ---------------------------------------------------------------------------

** Age
recode age ///
    (25/44   = 1 "25-44") ///
    (45/64   = 2 "45-64") ///
    (65/max  = 3 "65+"), ///
    gen(cat_age)
label var cat_age "Age categories"
tab year cat_age, m 

** Sex
gen cat_sex = (sex == 2)
label var cat_sex "Female indicator"
label define lb_cat_sex 0 "Male" 1 "Female", replace
label values cat_sex lb_cat_sex
tab year cat_sex, m 

** Marital status
recode marst ///
    (1       = 1 "Married") ///
    (2/3     = 2 "Separated") ///
    (4/5     = 3 "Divorced / Widowed") ///
    (6       = 4 "Single"), ///
    gen(cat_married)
label var cat_married "Marriage categories"
tab year cat_married, m 

** Kids at home
recode nchild ///
    (0       = 0 "0 Children") ///
    (1       = 1 "1 Child") ///
    (2       = 2 "2 Children") ///
    (3/max   = 3 "3+ Children"), ///
    gen(cat_child)
label var cat_child "Number of Children"
tab year cat_child, m 
 
** Age of youngest kid at home
recode yngch ///
    (99      = 0 "0 Children") ///
    (0/4     = 1 "0-4") ///
    (5/12    = 2 "5-12") ///
    (13/17   = 3 "13-17")	///
	(18/max   = 4 "18+"), ///
    gen(cat_yngch)
label var cat_yngch "Age of youngest child"
tab year cat_yngch, m 

** Education
recode educd ///
    (min/61  = 1 "Less than HS") ///
    (62/71   = 2 "HS Diploma") ///
    (80/100  = 3 "Some College") ///
    (101/max = 4 "College Degree"), ///
    gen(cat_educ)
label var cat_educ "Education categories"
tab year cat_educ, m 


** ---------------------------------------------------------------------------
** Real income (2024 USD) and income categories
** ---------------------------------------------------------------------------
gen tmp1 = cpi99 if year == ${end_year_acs}
egen tmp2 = mean(tmp1)
gen cpi = cpi99 / tmp2
drop tmp1 tmp2

foreach var of varlist inctot incwage incearn ftotinc {

    gen real_`var' = round(`var' * cpi)

    recode real_`var' ///
        (min/-1        = 1 "Negative income") ///
        (0             = 2 "$0") ///
        (1/24999       = 3 "$1-$25K") ///
        (25000/49999   = 4 "$25K-$50K") ///
        (50000/99999   = 5 "$50K-$100K") ///
        (100000/199999 = 6 "$100K-$200K") ///
        (200000/max    = 7 "$200K+"), ///
        gen(cat_`var')

    tab cat_`var', missing

} // END INCOME LOOP

label var cat_inctot  "Total personal income categories (real ${end_year_acs} USD)"
label var cat_incwage "Total wage income categories (real ${end_year_acs} USD)"
label var cat_incearn "Total earned income categories (real ${end_year_acs} USD)"
label var cat_ftotinc "Total family income categories (real ${end_year_acs} USD)"


** ---------------------------------------------------------------------------
** Analysis loops
** ---------------------------------------------------------------------------

** Categorical variables to iterate over
local catvars "cat_yngch cat_sex cat_married cat_age cat_child cat_educ cat_ftotinc"

forvalues i = 1/2 {

    if `i' == 1 local ytitle_txt "Out-migration rate (%)"
    if `i' == 2 local ytitle_txt "In-migration rate (%)"

    foreach cat of local catvars {

        ** Absorb all other categories (but not the focal one)
        local othercats : list catvars - cat

        ** Run regression and plot
        hdfe_catyear_plot out_`i' if sample_`i' == 1, ///
            cat(`cat') ///
            year(year) ///
            absorb(state_fips_o county_fips_o `othercats') ///
            wvar(perwt) wtype(fw) ///
			xtitle("ACS Survey Year (t=2)") ///
            ytitle("`ytitle_txt'") ///
            saving("${results}individual/fig_`cat'_`i'") ///
            replace
			
    } // END CAT LOOP

} // END SAMPLE LOOP


** ---------------------------------------------------------------------------
** Close log
** ---------------------------------------------------------------------------
clear
log close log_02
