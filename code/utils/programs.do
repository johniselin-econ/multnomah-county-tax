/*****************************************************************************
* File:        code/utils/programs.do  (formerly 01a_programs.do)
* Purpose:     Define value labels and reusable programs for data cleaning
* Loaded by:   code/utils/globals.do (sourced by every entry script)
* Outputs:     In-memory label definitions and programs:
*                lb_move_type, lb_agi (labels)
*                make_fips, unsuppress, setup_parallel,
*                acs_make_gross_migration, label_irs_migration_vars,
*                sdid_log_failure, sdid_consolidate_failures (programs)
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
**
** The auto-cap below is what would have averted the 2026-04-28 V3 incident:
** Stata MP licensed for N-cores consumes that many cores per instance, so K
** concurrent workers demand N*K cores. K=4 with a 4-core MP license on an
** 8-core machine oversubscribes 2:1 and degrades each rep ~15-20x.
**
** c(processors_max) reports the license's per-instance core cap. The machine's
** core count is read from the OS:
**   Windows: NUMBER_OF_PROCESSORS env var (always set; reports LOGICAL cores
**            on machines with hyperthreading, so on a 4-physical/8-logical box
**            this is 8 — the cap will then permit K=2 workers requesting 4
**            cores each = 8 logical demanded, mild oversubscription of physical
**            cores. K=4 still gets capped to K=2 even in that case, which is
**            the load-bearing protection.)
**   Linux:   `nproc` command (also logical cores by default)
**   macOS:   `sysctl -n hw.ncpu` (logical cores)
** If detection fails the cap is skipped with a warning.
**
** floor(machine / per_mp) is the number of MP workers that can run concurrently
** without (logical-core) oversubscription.
capture program drop setup_parallel
program define setup_parallel
    if "${use_parallel}" == "" global use_parallel 0
    if "${n_clusters}" == ""   global n_clusters 1
    if ${use_parallel} == 1 {
        local _per_mp_cores = c(processors_max)
        local _physical ""

        ** Windows: env var always present
        if "`c(os)'" == "Windows" {
            local _physical : env NUMBER_OF_PROCESSORS
        }
        else {
            ** Linux / macOS: shell out
            tempfile _ncpu_out
            if "`c(os)'" == "MacOSX" {
                capture qui shell sysctl -n hw.ncpu > "`_ncpu_out'"
            }
            else {
                capture qui shell nproc > "`_ncpu_out'"
            }
            if _rc == 0 {
                tempname _fh
                capture file open `_fh' using "`_ncpu_out'", read text
                if _rc == 0 {
                    file read `_fh' _line
                    local _physical = trim("`_line'")
                    file close `_fh'
                }
            }
        }

        if "`_physical'" == "" {
            dis as text "setup_parallel: unable to detect physical cores on " ///
                "this OS (`c(os)'); using n_clusters=${n_clusters} without cap"
        }
        else {
            local _max_safe = max(1, floor(`_physical' / `_per_mp_cores'))
            if ${n_clusters} > `_max_safe' {
                dis as text "setup_parallel: capping n_clusters from ${n_clusters} to " ///
                    "`_max_safe' (machine has `_physical' physical cores; Stata MP " ///
                    "license uses `_per_mp_cores' per instance — running more would " ///
                    "oversubscribe CPU and slow each worker)"
                global n_clusters = `_max_safe'
            }
        }
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

    syntax [using/] [if] [in], SAVING(string) [REPLACE] ///
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

    local __restore = 0
    tempfile __src

    // Load microdata (optionally subset via if/in), or operate on current data
    if `"`using'"' != "" {
        use "`using'" `if' `in', clear
    }
    else {
        local __restore = 1
        save `__src', replace
        if `"`if'"' != "" | `"`in'"' != "" {
            keep `if' `in'
        }
    }

	if "`sample'" != "" keep if `sample'

    // Basic checks
    foreach v in `yearvar' `origfips' `destfips' `personwt' `hhwt' `headvar' `income' {
        capture confirm variable `v'
        if _rc {
            di as err "Required variable `v' not found."
            if `__restore' use `__src', clear
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
    if `__restore' use `__src', clear
	else clear

end


** Apply labels to reshaped IRS migration variables
**
** After 01f_irs_migration.do reshapes gross_in / gross_out by move_type,
** variables exist as <prefix>_<direction>_<n> for prefix in {n1, n2, agi},
** direction in {in, out, net}, and n an IRS move-type code. This program
** applies "<Series>, <direction>-migration, <move-type>" labels, using the
** move-type strings from the lb_move_type value label (defined above).
**
** Usage:
**   label_irs_migration_vars, direction(in)                 // types 1-6
**   label_irs_migration_vars, direction(out)                // types 1-6
**   label_irs_migration_vars, direction(net) first(2)       // types 2-6

capture program drop label_irs_migration_vars
program define label_irs_migration_vars
    syntax, DIRection(string) [First(integer 1) Last(integer 6)]

    if !inlist("`direction'", "in", "out", "net") {
        di as err "direction() must be one of: in, out, net"
        exit 198
    }

    local dir_label "`direction'-migration"

    foreach v in n1 n2 agi {
        if      "`v'" == "n1"  local vtext "Returns"
        else if "`v'" == "n2"  local vtext "Exemptions"
        else if "`v'" == "agi" local vtext "AGI"

        forvalues n = `first'/`last' {
            local mtype : label lb_move_type `n'
            label var `v'_`direction'_`n' "`vtext', `dir_label', `mtype'"
        }
    }
end


//------------------------------------------------------------------------------
// project_report_merge — report match rates after a merge
//
// Call immediately after `merge ..., gen(<name>) keep(master match)`. Reads
// the _merge indicator, prints matched/master-only/using-only counts, and (by
// default) drops the indicator. Fails loudly if match rate falls below
// `required(#)` — useful for catching silent data loss.
//
// Usage:
//   merge m:1 fips using "X.dta", gen(x_mrg) keep(master match)
//   project_report_merge, gen(x_mrg) tag("X")
//
// Options:
//   gen(name)        — name of the merge indicator (required)
//   tag(string)      — label shown in the report
//   required(real)   — minimum match rate (master rows matched / master rows);
//                      aborts with error if below
//   keep_merge       — do not drop the merge indicator after reporting
//------------------------------------------------------------------------------
capture program drop project_report_merge
program define project_report_merge
    syntax, gen(name) [tag(string) required(real -1) keep_merge]
    qui count if `gen' == 3
    local matched = r(N)
    qui count if `gen' == 1
    local master_only = r(N)
    qui count if `gen' == 2
    local using_only = r(N)
    local total_master = `matched' + `master_only'
    if `total_master' == 0 local total_master = 1
    local rate = 100 * `matched' / `total_master'
    if "`tag'" == "" local tag "merge"
    di as text "  `tag': " %9.0fc `matched' " matched / " ///
        %9.0fc `matched' + `master_only' " master (" %4.1f `rate' "%)" ///
        "  [" %9.0fc `master_only' " master-only, " ///
        %9.0fc `using_only' " using-only]"
    if `required' > 0 & `rate' < `required' {
        di as error "  `tag': match rate `rate'% below required `required'%"
        exit 459
    }
    if "`keep_merge'" == "" drop `gen'
end


//------------------------------------------------------------------------------
// taxsim_fallback_calc — approximate federal/state/FICA tax when TAXSIM is
// unavailable. Generates (or replaces) fiitax, siitax, fica, taxable_income.
//
// Uses Oregon's 2022 single-filer progressive schedule and standard
// 2022 federal deductions ($12,950 single / $25,900 joint). Federal income
// tax is set to 0 as a placeholder — this fallback is not a substitute for
// TAXSIM, only a diagnostic when TAXSIM fails.
//
// Usage: taxsim_fallback_calc, agi(agi_proxy) mstat(mstat) ///
//            pwages(pwages) swages(swages)
//------------------------------------------------------------------------------
capture program drop taxsim_fallback_calc
program define taxsim_fallback_calc
    syntax, agi(varname numeric) mstat(varname numeric) ///
        pwages(varname numeric) swages(varname numeric)

    ** Federal-like taxable income (std deduction by filing status)
    capture drop taxable_income
    gen double taxable_income = max(`agi' - cond(`mstat' == 2, 25900, 12950), 0)

    ** Oregon 2022 single-filer brackets
    capture drop siitax
    gen double siitax = 0.05  * min(taxable_income, 3750) ///
        + 0.07  * max(min(taxable_income, 9450)   - 3750, 0) ///
        + 0.09  * max(min(taxable_income, 125000) - 9450, 0) ///
        + 0.099 * max(taxable_income - 125000, 0)

    ** Placeholder — fallback does not attempt federal liability
    capture drop fiitax
    gen double fiitax = 0

    ** FICA (employee share): 6.2% OASDI capped at $147k + 1.45% HI uncapped
    capture drop fica
    gen double fica = 0.062  * min(`pwages' + `swages', 147000) ///
        + 0.0145 * (`pwages' + `swages')
end


** ---------------------------------------------------------------------
** SDID failure logging
** ---------------------------------------------------------------------
** Record each SDID skip-handler hit with its rc so reruns can distinguish
** legitimate failures from crash-masked cells. Per-PID CSVs avoid contention
** between parallel workers; consolidate step merges them into one summary.

capture program drop sdid_log_failure
program define sdid_log_failure
    syntax, RC(integer)       ///
            SCRIPT(string)    ///
            TABLEID(string)   ///
            OUTCOME(string)   ///
            C(integer)        ///
            EXL(integer)      ///
            [SAMP(string)     ///
             CONTEXT(string)]

    capture mkdir "${logs}sdid_failures"

    local pid ""
    capture local pid = c(processid)
    if _rc != 0 | "`pid'" == "" {
        capture local pid = c(pid)
    }
    if _rc != 0 | "`pid'" == "" {
        local pid = subinstr("`c(current_time)'", ":", "", .)
    }
    local fpath "${logs}sdid_failures/failures_pid`pid'.csv"

    local rc_text ""
    if `rc' == 503  local rc_text "conformability error"
    if `rc' == 603  local rc_text "file could not be opened"
    if `rc' == 700  local rc_text "no room to add more observations"
    if `rc' == 900  local rc_text "no room to add more variables"
    if `rc' == 910  local rc_text "op. sys. refuses to provide memory"
    if `rc' == 950  local rc_text "op. sys. refuses to provide memory"
    if `rc' == 2000 local rc_text "no observations"

    capture confirm file "`fpath'"
    local need_header = (_rc != 0)

    tempname fh
    file open `fh' using "`fpath'", write text append
    if `need_header' {
        file write `fh' "timestamp,script,table_id,outcome,c,exl,samp,rc,rc_text,context" _n
    }

    local ts "`c(current_date)' `c(current_time)'"
    file write `fh' `""`ts'","`script'","`tableid'","`outcome'",`c',`exl',"`samp'",`rc',"`rc_text'","`context'""' _n
    file close `fh'

    dis as txt "  [sdid_log_failure] pid=`pid' rc=`rc' `rc_text' | script=`script' table=`tableid' out=`outcome' c=`c' exl=`exl' samp=`samp'"
end


capture program drop sdid_consolidate_failures
program define sdid_consolidate_failures
    syntax [, QUIET]

    local logdir "${logs}sdid_failures"
    local files ""
    capture local files : dir "`logdir'" files "failures_pid*.csv"
    if `"`files'"' == "" {
        if "`quiet'" == "" dis as txt "No SDID failure files; nothing to consolidate."
        exit 0
    }

    local outpath "${logs}sdid_failures_${pr_name}_${date}.csv"
    tempname outh inh
    file open `outh' using "`outpath'", write text replace
    file write `outh' "timestamp,script,table_id,outcome,c,exl,samp,rc,rc_text,context" _n

    local n_rows = 0
    foreach f of local files {
        file open `inh' using "`logdir'/`f'", read
        file read `inh' line
        file read `inh' line
        while r(eof) == 0 {
            file write `outh' `"`line'"' _n
            local ++n_rows
            file read `inh' line
        }
        file close `inh'
    }
    file close `outh'

    dis as text "sdid_consolidate_failures: wrote `n_rows' row(s) to `outpath'"
end


** Build the narrow donor-pool indicator from resources/narrow_pool_fips.csv.
** The CSV is the single source of truth for the 22-county Metroverse pool;
** edit it (not Stata code) to add or drop a county. Requires `fips` in memory.
capture program drop load_narrow_pool
program define load_narrow_pool
    syntax , [PATH(string)]

    if "`path'" == "" local path "${dir}/resources/narrow_pool_fips.csv"

    capture confirm variable fips
    if _rc {
        di as error "load_narrow_pool: variable `fips' not in memory"
        exit 111
    }
    capture confirm file "`path'"
    if _rc {
        di as error "load_narrow_pool: cannot find `path'"
        exit 601
    }

    preserve
    import delimited using "`path'", clear varnames(1) numericcols(1)
    levelsof fips, local(narrow_fips) clean
    restore

    capture drop sample_narrow
    gen byte sample_narrow = 0
    foreach f of local narrow_fips {
        qui replace sample_narrow = 1 if fips == `f'
    }
    label var sample_narrow "Narrow pool: 20 Metroverse similar cities + Multnomah"
end


** ------------------------------------------------------------------
** project_parse_outcome_components
**
** Single source of truth for parsing an SDID-style results dataset's
** `outcome` + `sample_data` columns into the canonical spec-metadata
** columns. Previously open-coded in several places (02_post_spec.do,
** 02_sdid_analysis.do — main + influence section),
** producing latent drift hazards (the regex bug at 02_post_spec.do:118
** that misclassified outstate IRS-389 rows survived because the parsing
** wasn't unified with the strpos pattern at 02_sdid_analysis.do:1512).
**
** Generates:
**   outcome_type   "n1" | "n2" | "agi"
**   migration      "net" | "in" | "out"
**   data_type      one of: IRS, IRS (Out-of-State), IRS (389),
**                  IRS (389, Out-of-State), ACS All, ACS All (Out-of-State),
**                  ACS College, ACS College (Out-of-State)
**   period_type    "16-22" | "16-24"  (narrow's "full" sample_data also → 16-22)
**   outstate       0 | 1
**
** With option INDICATORS: also generates the spec_* indicator family used
** by spec-curve indicator panels (spec_all / urban95 / covid / demog /
** stringency / narrow / covars / excl2020 / irs / irs_389 / irs_outstate /
** irs_outstate_389 / acs_all / acs_all_outstate / acs_col / acs_col_outstate
** / 16_22 / 16_24).
**
** All generated variables are `capture drop`-ped first, so the helper is
** idempotent and safe to call after manual replaces.
** ------------------------------------------------------------------
capture program drop project_parse_outcome_components
program define project_parse_outcome_components
    syntax [, INDICATORS]

    foreach v in outcome_type migration data_type period_type outstate {
        capture drop `v'
    }

    ** outcome family
    gen str8 outcome_type = ""
    replace outcome_type = "n1"  if regexm(outcome, "^n1_")
    replace outcome_type = "n2"  if regexm(outcome, "^n2_")
    replace outcome_type = "agi" if regexm(outcome, "^agi_")

    ** migration direction
    gen str4 migration = ""
    replace migration = "net" if regexm(outcome, "_net_")
    replace migration = "in"  if regexm(outcome, "_in_")
    replace migration = "out" if regexm(outcome, "_out_")

    ** data source label. Order matters: the unrestricted-IRS branch fires
    ** first, then sample_data-keyed IRS (389) variants override it.
    gen str40 data_type = ""
    replace data_type = "IRS" if regexm(outcome, "_irs(_|$)") & !regexm(outcome, "_irs_outstate")
    replace data_type = "IRS (Out-of-State)" if regexm(outcome, "_irs_outstate")
    replace data_type = "IRS (389)" if regexm(sample_data, "irs_389") & !regexm(outcome, "_irs_outstate")
    replace data_type = "IRS (389, Out-of-State)" if regexm(sample_data, "irs_outstate_389") & regexm(outcome, "_irs_outstate")
    replace data_type = "ACS All (Out-of-State)" if regexm(outcome, "_acs1_outstate")
    replace data_type = "ACS College (Out-of-State)" if regexm(outcome, "_acs2_outstate")
    replace data_type = "ACS All" if regexm(outcome, "_acs1(_|$)") & !regexm(outcome, "_acs1_outstate")
    replace data_type = "ACS College" if regexm(outcome, "_acs2(_|$)") & !regexm(outcome, "_acs2_outstate")

    ** period type. IRS data is always 16-22 (no later vintage in the
    ** SOI). Narrow's sample_data uses "full" instead of "16_22" for the
    ** unrestricted IRS panel; treat both as 16-22.
    gen str5 period_type = ""
    replace period_type = "16-22" if regexm(outcome, "_irs(_|$)")
    replace period_type = "16-22" if regexm(sample_data, "16_22") | regexm(sample_data, "full")
    replace period_type = "16-24" if regexm(sample_data, "16_24")

    ** outstate flag — outcome-based with sample_data fallback.
    gen byte outstate = regexm(outcome, "_outstate") | regexm(sample_data, "outstate")

    if "`indicators'" != "" {
        foreach s in spec_all spec_urban95 spec_covid spec_demog            ///
                     spec_stringency spec_narrow spec_covars spec_excl2020  ///
                     spec_irs spec_irs_389 spec_irs_outstate                ///
                     spec_irs_outstate_389 spec_acs_all spec_acs_all_outstate ///
                     spec_acs_col spec_acs_col_outstate spec_16_22 spec_16_24 {
            capture drop `s'
        }
        gen byte spec_all              = sample == "sample_all"
        gen byte spec_urban95          = sample == "sample_urban95"
        gen byte spec_covid            = sample == "sample_urban75_covid"
        gen byte spec_demog            = sample == "sample_demog"
        gen byte spec_stringency       = sample == "sample_stringency"
        gen byte spec_narrow           = sample == "sample_narrow"
        gen byte spec_covars           = controls == 1
        gen byte spec_excl2020         = exclusion == 1
        gen byte spec_irs              = data_type == "IRS"
        gen byte spec_irs_389          = data_type == "IRS (389)"
        gen byte spec_irs_outstate     = data_type == "IRS (Out-of-State)"
        gen byte spec_irs_outstate_389 = data_type == "IRS (389, Out-of-State)"
        gen byte spec_acs_all          = data_type == "ACS All"
        gen byte spec_acs_all_outstate = data_type == "ACS All (Out-of-State)"
        gen byte spec_acs_col          = data_type == "ACS College"
        gen byte spec_acs_col_outstate = data_type == "ACS College (Out-of-State)"
        gen byte spec_16_22            = period_type == "16-22"
        gen byte spec_16_24            = period_type == "16-24"
    }
end


** ------------------------------------------------------------------
** build_acs_balanced_set
**
** Single source of truth for the "balanced ACS county set": counties observed
** in the ACS 25+ gross-migration panel in EVERY analysis year 2016-2024 (the
** ~389-county set). This is the county restriction the SDID ACS specification
** (acs_period_2) and the PPML flow specification use, so the flow estimation
** (02_flow_analysis.do), the appendix flow descriptives (Table A2,
** 02_appendix_descriptives.do), and the diagnostics audit (02_diagnostics.do)
** all draw the set from here rather than re-deriving it. It was previously
** open-coded in four places that had begun to drift (`_ct` vs `ct`, `qui summ`
** vs `tab ct`); centralizing it guarantees the audit can't diverge from the
** estimation sample over a counting detail.
**
** Builds the set in memory (one row per balanced fips) AND writes it to the
** path passed in saving(). The data in memory is REPLACED, so call this inside
** a preserve, or immediately before reloading the working file.
**
** Options:
**   saving(string)  REQUIRED. Destination path/tempfile for the fips set.
**   flag(name)      Optional. Adds `gen byte <name> = 1` so the set can be
**                   tagged after a merge (e.g. flag(acs_county)).
**   source(string)  Input panel. Default ${data}working/acs_county_gross_25plus.
** ------------------------------------------------------------------
capture program drop build_acs_balanced_set
program define build_acs_balanced_set
    syntax , SAVing(string) [ FLAG(name) SOURce(string) ]

    if "`source'" == "" local source "${data}working/acs_county_gross_25plus"

    capture confirm file "`source'.dta"
    if _rc {
        di as error "build_acs_balanced_set: cannot find `source'.dta"
        exit 601
    }

    use "`source'", clear
    keep year fips
    keep if inrange(year, 2016, 2024)        // ACS analysis window (SDID/DiD)
    bysort fips: gen _ct = _N
    qui summ _ct
    keep if _ct == r(max)                      // present every year = balanced
    keep fips
    duplicates drop
    if "`flag'" != "" gen byte `flag' = 1
    qui save "`saving'", replace
end


** ==================================================================
** Project manifest / seed / preferred-spec programs
** (relocated from globals.do — the config file holds settings only)
** ==================================================================
capture program drop project_set_seed
program define project_set_seed
    syntax , CONTEXT(string) [OFFSET(integer 0)]

    local seed = ${master_seed} + `offset'
    set seed `seed'
    di as txt "Seed set to `seed' (`context')"
end

capture program drop project_build_signature
program define project_build_signature, rclass
    syntax , ARTIFACT(string)

    local signature ///
        "schema=${artifact_schema_version}|artifact=`artifact'|pref=${preferred_spec_version}|seed=${master_seed}|irs_data=${start_year_irs_data}|irs_analysis=${start_year_irs_analysis}|acs_start=${start_year_acs}|acs_end=${end_year_acs}|irs_dl=${start_yy_irs_download}|irs_mig=${end_yy_irs_migration}|irs_agi=${end_yy_irs_agi}|irs_cty=${start_yy_irs_county}-to-${end_yy_irs_county}"

    return local signature "`signature'"
end

capture program drop project_export_run_manifest
program define project_export_run_manifest
    syntax

    local stamp = subinstr("`c(current_time)'", ":", "", .)

    project_build_signature, artifact("run_manifest")
    local signature "`r(signature)'|parallel=${use_parallel}|clusters=${n_clusters}|resume=${resume}|event=${event_study_mode}"

    preserve
    clear
    set obs 1

    gen str40 project_name = "${pr_name}"
    gen str120 project_dir = "${dir}"
    gen str244 config_signature = "`signature'"
    gen str20 run_date = "${date}"
    gen str20 run_time = "`c(current_time)'"
    gen double master_seed = real("${master_seed}")
    gen double use_parallel = real("${use_parallel}")
    gen double n_clusters = real("${n_clusters}")
    gen double resume = real("${resume}")
    gen str20 event_study_mode = "${event_study_mode}"
    gen double start_year_irs_data = real("${start_year_irs_data}")
    gen double start_year_irs_analysis = real("${start_year_irs_analysis}")
    gen double start_year_acs = real("${start_year_acs}")
    gen double end_year_acs = real("${end_year_acs}")
    gen double start_yy_irs_download = real("${start_yy_irs_download}")
    gen double end_yy_irs_migration = real("${end_yy_irs_migration}")
    gen double end_yy_irs_agi = real("${end_yy_irs_agi}")
    gen double start_yy_irs_county = real("${start_yy_irs_county}")
    gen double end_yy_irs_county = real("${end_yy_irs_county}")
    gen str40 preferred_spec_version = "${preferred_spec_version}"
    gen str40 artifact_schema_version = "${artifact_schema_version}"

    ** Dated snapshots archive under _runs/; run_manifest_latest stays at the
    ** results root (the reproducibility-signature check reads _latest).
    capture mkdir "${results}_runs"
    save "${results}_runs/run_manifest_${date}_`stamp'.dta", replace
    export delimited using "${results}_runs/run_manifest_${date}_`stamp'.csv", replace
    save "${results}run_manifest_latest.dta", replace
    export delimited using "${results}run_manifest_latest.csv", replace
    restore
end

capture program drop project_write_manifest
program define project_write_manifest
    syntax using/, ARTIFACT(string) SCRIPT(string) [UPSTREAM(string)]

    project_build_signature, artifact("`artifact'")
    local signature "`r(signature)'"
    local csv_path = subinstr(`"`using'"', ".dta", ".csv", .)

    preserve
    clear
    set obs 1

    gen str80 artifact = "`artifact'"
    gen str120 script_name = "`script'"
    gen str244 config_signature = "`signature'"
    gen str244 upstream_signature = "`upstream'"
    gen str20 created_date = "${date}"
    gen str20 created_time = "`c(current_time)'"
    gen double master_seed = real("${master_seed}")
    gen str40 preferred_spec_version = "${preferred_spec_version}"
    gen str40 artifact_schema_version = "${artifact_schema_version}"
    gen double start_year_irs_data = real("${start_year_irs_data}")
    gen double start_year_irs_analysis = real("${start_year_irs_analysis}")
    gen double start_year_acs = real("${start_year_acs}")
    gen double end_year_acs = real("${end_year_acs}")

    save `"`using'"', replace
    export delimited using `"`csv_path'"', replace
    restore
end

capture program drop project_assert_manifest
program define project_assert_manifest
    syntax using/, ARTIFACT(string)

    if !fileexists(`"`using'"') {
        di as error "ERROR: Required manifest not found: `using'"
        exit 601
    }

    project_build_signature, artifact("`artifact'")
    local expected "`r(signature)'"

    preserve
    use `"`using'"', clear
    qui levelsof artifact if _n == 1, local(actual_artifact) clean
    qui levelsof config_signature if _n == 1, local(actual_signature) clean
    restore

    if "`actual_artifact'" != "`artifact'" {
        di as error "ERROR: Manifest artifact mismatch for `using'"
        di as error "       Expected artifact: `artifact'"
        di as error "       Found artifact:    `actual_artifact'"
        exit 459
    }

    if "`actual_signature'" != "`expected'" {
        di as error "ERROR: Stale or incompatible artifact manifest detected for `artifact'."
        di as error "       Expected signature: `expected'"
        di as error "       Found signature:    `actual_signature'"
        exit 459
    }
end

capture program drop project_mark_preferred_main
program define project_mark_preferred_main
    capture drop preferred
    gen preferred = 0

    ** Highlighted main SDID specifications:
    ** IRS x {All Counties, Stringency Match, Narrow Pool}
    ** ACS College x {All Counties, Stringency Match, Narrow Pool}

    replace preferred = 1 if                                                    ///
        data_type == "IRS" &                                                    ///
        period_type == "16-22" &                                                ///
        inlist(sample, "sample_all", "sample_stringency", "sample_narrow") &    ///
        controls == 1 &                                                         ///
        exclusion == 1

    replace preferred = 1 if                                                    ///
        data_type == "ACS College" &                                            ///
        period_type == "16-24" &                                                ///
        inlist(sample, "sample_all", "sample_stringency", "sample_narrow") &    ///
        controls == 1 &                                                         ///
        exclusion == 1

    replace preferred = 1 if                                                    ///
        data_type == "IRS (Out-of-State)" &                                     ///
        period_type == "16-22" &                                                ///
        inlist(sample, "sample_all", "sample_stringency", "sample_narrow") &    ///
        controls == 1 &                                                         ///
        exclusion == 1

    replace preferred = 1 if                                                    ///
        data_type == "ACS College (Out-of-State)" &                             ///
        period_type == "16-24" &                                                ///
        inlist(sample, "sample_all", "sample_stringency", "sample_narrow") &    ///
        controls == 1 &                                                         ///
        exclusion == 1
end
