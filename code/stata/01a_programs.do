/*****************************************************************************
* File:        01a_programs.do
* Purpose:     Define value labels and reusable programs for data cleaning
* Called by:   01_clean_data.do
* Outputs:     In-memory label definitions and programs:
*                lb_move_type, lb_agi (labels)
*                make_fips, unsuppress, acs_make_gross_migration (programs)
******************************************************************************/

//--------------------------------------------------
// Value Labels
//--------------------------------------------------

** Migration move types (mirrors IRS SOI classification)
label define lb_move_type 	0 "ERROR"				///
							1 "Non-movers"			///
							2 "All movers"			///
							3 "Domestic movers"		///
							4 "Within-state movers"	///
							5 "Inter-state movers"	///
							6 "Foreign movers", modify

** AGI income brackets (IRS SOI county data)
label define lb_agi 		1 "Under $1"			///
							2 "$1 under $10K"		///
							3 "$10K under $25K"		///
							4 "$25K under $50K"		///
							5 "$50K under $75K"		///
							6 "$75K under $100K"	///
							7 "$100K under $200K"	///
							8 "$200K or more", modify


//--------------------------------------------------
// Programs
//--------------------------------------------------

** Make FIPS code from state and county fips codes
capture program drop make_fips
program define make_fips
    syntax varlist(min=2 max=2 numeric), GEN(name)

    quietly {
        tempvar s c

        local v1 : word 1 of `varlist'
        local v2 : word 2 of `varlist'

        gen `s' = string(`v1', "%02.0f")
        gen `c' = string(`v2', "%03.0f")

        gen `gen' = real(`s' + `c')
    }
end

** Reclassify suppressed values as 0

capture program drop unsuppress
program define unsuppress
    syntax varlist

    foreach v of varlist `varlist' {
        replace `v' = 0 if `v' == -1
    }
end


** Safe default for parallel processing
** Avoids crash when running analysis do-files standalone (not via 00_multnomah.do)
capture program drop setup_parallel
program define setup_parallel
    if "${use_parallel}" == "" global use_parallel 0
    if "${n_clusters}" == ""   global n_clusters 1
    if ${use_parallel} == 1 {
        parallel initialize ${n_clusters}, force
    }
end


** Create a gross-migration file via ACS
**
** This program aggregates individual ACS microdata into county-year gross
** migration totals (in/out/net) by mover type. Move-type indices mirror the
** IRS convention:
**   1 = Non-movers (same county)
**   2 = All movers (different county) = type 4 + type 5
**   3 = Domestic movers (same as 2; foreign already dropped upstream)
**   4 = Within-state movers (different county, same state)
**   5 = Inter-state movers (different state)
**
** Note on type 2/3: Because foreign-born movers are excluded before this
** program runs (migplac1 > 56 and migrate1 == 4 dropped in 01e_acs.do),
** "all movers" and "domestic movers" are identical. We compute both so that
** the ACS and IRS datasets share the same variable naming structure.

capture program drop acs_make_gross_migration
program define acs_make_gross_migration
    version 16.0

    syntax using/ [if] [in], SAVING(string) [REPLACE] ///
        [ IDSFILE(string) ///
          YEARVAR(name) ORIGFIPS(name) DESTFIPS(name) ///
          PERSONWT(name) HHWT(name) HHPERWT(name) HEADVAR(name) INCOME(name) SAMPLE(string)]

    // Defaults consistent with 01_clean_data.do
    if "`idsfile'"  == "" local idsfile  "${data}working/ids"
    if "`yearvar'"  == "" local yearvar  year
    if "`origfips'" == "" local origfips fips_o
    if "`destfips'" == "" local destfips fips_d
	if "`personwt'" == "" local personwt perwt
    if "`hhperwt'"  == "" local hhperwt  hh_perwt
    if "`hhwt'"     == "" local hhwt     hhwt
    if "`headvar'"  == "" local headvar  hh_head
    if "`income'"   == "" local income   inctot

    // Load microdata (optionally subset via if/in)
    use "`using'" `if' `in', clear

	if "`sample'" != "" keep if `sample'

    // Basic checks
    foreach v in `yearvar' `origfips' `destfips' `personwt' `hhwt' `headvar' `income' {
        capture confirm variable `v'
        if _rc {
            di as err "Required variable `v' not found in `using'."
            exit 198
        }
    }

    // Keep only valid year/origin/destination
    drop if missing(`yearvar') | missing(`origfips') | missing(`destfips')

    // Income: treat missing as 0 (keep negatives as reported)
    replace `income' = 0 if missing(`income')

    // Build weighted components at the person level
    gen double persons_wt = `hhperwt' if `headvar' == 1
    gen double dollars_wt = `income' * `personwt'
    gen double households_wt = `hhwt' if `headvar' == 1
    replace households_wt = 0 if missing(households_wt)

    // Collapse to origin-destination-year flow first
    keep `yearvar' `origfips' `destfips' persons_wt dollars_wt households_wt
    collapse (sum) persons=persons_wt dollars=dollars_wt households=households_wt, ///
        by(`yearvar' `origfips' `destfips')

    // Derive state/county components for mover-type logic
    gen int state_o  = floor(`origfips'/1000)
    gen int state_d  = floor(`destfips'/1000)

    gen byte same_county = (`origfips' == `destfips')
    gen byte same_state  = (state_o == state_d)
    gen byte within_state_mover = same_state & !same_county
    gen byte inter_state_mover  = !same_state

    // -----------------------
    // IN-MIGRATION (by destination county)
    // -----------------------
    preserve
        gen long fips = `destfips'
        gen int state_fips  = floor(fips/1000)
        gen int county_fips = mod(fips, 1000)

        // type 1/4/5 components
        foreach m in persons households dollars {
            gen double `m'_1 = `m' if same_county
            gen double `m'_4 = `m' if within_state_mover
            gen double `m'_5 = `m' if inter_state_mover
        }

        collapse (sum) persons_1 persons_4 persons_5 ///
                       households_1 households_4 households_5 ///
                       dollars_1 dollars_4 dollars_5, ///
                by(`yearvar' fips state_fips county_fips)

        // build 2 and 3
        gen double persons_2    = persons_4 + persons_5
        gen double persons_3    = persons_2
        gen double households_2 = households_4 + households_5
        gen double households_3 = households_2
        gen double dollars_2    = dollars_4 + dollars_5
        gen double dollars_3    = dollars_2

        // rename to *_in_*
        foreach t in 1 2 3 4 5 {
            rename persons_`t'    persons_in_`t'
            rename households_`t' households_in_`t'
            rename dollars_`t'    dollars_in_`t'
        }

        tempfile __in
        save `__in', replace
    restore

    // -----------------------
    // OUT-MIGRATION (by origin county)
    // -----------------------
    preserve
        gen long fips = `origfips'
        gen int state_fips  = floor(fips/1000)
        gen int county_fips = mod(fips, 1000)

        foreach m in persons households dollars {
            gen double `m'_1 = `m' if same_county
            gen double `m'_4 = `m' if within_state_mover
            gen double `m'_5 = `m' if inter_state_mover
        }

        collapse (sum) persons_1 persons_4 persons_5 ///
                       households_1 households_4 households_5 ///
                       dollars_1 dollars_4 dollars_5, ///
                by(`yearvar' fips state_fips county_fips)

        gen double persons_2    = persons_4 + persons_5
        gen double persons_3    = persons_2
        gen double households_2 = households_4 + households_5
        gen double households_3 = households_2
        gen double dollars_2    = dollars_4 + dollars_5
        gen double dollars_3    = dollars_2

        foreach t in 1 2 3 4 5 {
            rename persons_`t'    persons_out_`t'
            rename households_`t' households_out_`t'
            rename dollars_`t'    dollars_out_`t'
        }

        tempfile __out
        save `__out', replace
    restore

    // -----------------------
    // Merge in/out; compute net
    // -----------------------
    use `__in', clear
    merge 1:1 `yearvar' fips state_fips county_fips using `__out', nogen

    // Replace missings with 0 prior to net calcs (counties can be only in or only out)
    foreach m in persons households dollars {
        foreach t in 1 2 3 4 5 {
            replace `m'_in_`t'  = 0 if missing(`m'_in_`t')
            replace `m'_out_`t' = 0 if missing(`m'_out_`t')
        }
    }

    // Net = in - out (types 2/3/4/5 are the meaningful migration nets; 1 will be ~0 by construction)
    foreach m in persons households dollars {
        foreach t in 2 3 4 5 {
            gen double `m'_net_`t' = `m'_in_`t' - `m'_out_`t'
        }
    }

    // Merge names
    merge m:1 state_fips county_fips using "`idsfile'", keep(master match) nogen

    // Labels
    label var fips "County FIPS (state*1000 + county)"
    label var state_fips "State FIPS"
    label var county_fips "County FIPS"

    label var persons_in_2  "Persons, in-migration, all movers"
    label var persons_out_2 "Persons, out-migration, all movers"
    label var persons_net_2 "Persons, net migration, all movers"

    label var households_in_2  "Households, in-migration, all movers (HH heads)"
    label var households_out_2 "Households, out-migration, all movers (HH heads)"
    label var households_net_2 "Households, net migration, all movers (HH heads)"

    label var dollars_in_2  "Dollars, in-migration, all movers (INCTOT*PERWT)"
    label var dollars_out_2 "Dollars, out-migration, all movers (INCTOT*PERWT)"
    label var dollars_net_2 "Dollars, net migration, all movers (INCTOT*PERWT)"

    order `yearvar' fips state_fips county_fips state_name county_name, first
    sort `yearvar' state_fips county_fips
    compress

    // Save
    save "`saving'", `replace'
	clear

end
