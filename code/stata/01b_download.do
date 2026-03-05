/*****************************************************************************
* File:        01b_download.do
* Purpose:     Auto-download public data and verify manual downloads
* Called by:   01_clean_data.do
* Outputs:     Populated data/ subdirectories:
*                data/irs/          — IRS SOI migration + county AGI CSVs
*                data/demographic/  — BEA CAINC1, BLS, DOL files
*                data/covid/        — NYTimes COVID CSV
*                data/working/      — Census age shares (created by R)
******************************************************************************/

** Ensure expected directory structure exists
capture mkdir "${data}"
capture mkdir "${data}working"
capture mkdir "${data}demographic"
capture mkdir "${data}demographic/CAINC1"
capture mkdir "${data}demographic/nhgis0031_csv"
capture mkdir "${data}demographic/dol"
capture mkdir "${data}demographic/bls"
capture mkdir "${data}irs"
capture mkdir "${data}covid"

* ----------------------------
* IRS SOI: county-to-county migration files
* ----------------------------
local irs_base "https://www.irs.gov/pub/irs-soi"

forvalues yy = $start_yy_irs_download/$end_yy_irs_migration {
    local zz = `yy' + 1
    local fn_out "countyoutflow`yy'`zz'.csv"
    local fn_in  "countyinflow`yy'`zz'.csv"

    capture confirm file "${data}irs/`fn_out'"
    if _rc {
        di as txt "Downloading (IRS SOI) `fn_out' ..."
        copy "`irs_base'/`fn_out'" "${data}irs/`fn_out'", replace
    }

    capture confirm file "${data}irs/`fn_in'"
    if _rc {
        di as txt "Downloading (IRS SOI) `fn_in' ..."
        copy "`irs_base'/`fn_in'" "${data}irs/`fn_in'", replace
    }
}

* ----------------------------
* IRS SOI: county income (AGI) files
* ----------------------------
forvalues yy = $start_yy_irs_download/$end_yy_irs_agi {
    ** 2012 uses a different filename convention (12cyallagi vs YYincyallagi)
    if `yy' == 12 {
        local fn_inc "12cyallagi.csv"
    }
    else {
        local fn_inc "`yy'incyallagi.csv"
    }

    capture confirm file "${data}irs/`fn_inc'"
    if _rc {
        di as txt "Downloading (IRS SOI) `fn_inc' ..."
        copy "`irs_base'/`fn_inc'" "${data}irs/`fn_inc'", replace
    }
}

* ----------------------------
* BEA Regional: CAINC1.zip
* ----------------------------
local bea_dir "${data}demographic/CAINC1"
local bea_url "https://apps.bea.gov/regional/zip/CAINC1.zip"
local bea_zip "`bea_dir'/CAINC1.zip"

* If we don't already have a CAINC1 "_ALL_AREAS" file, download + unzip the ZIP.
local bea_files : dir "`bea_dir'" files "CAINC1__ALL_AREAS_*.csv"
if "`bea_files'"=="" {
    local bea_files : dir "`bea_dir'" files "CAINC1__ALL_STATES_*.csv"
}

if "`bea_files'"=="" {
    di as txt "Downloading (BEA) CAINC1.zip ..."
    copy "`bea_url'" "`bea_zip'", replace

    local curdir "`c(pwd)'"
    cd "`bea_dir'"
    unzipfile "CAINC1.zip", replace
    cd "`curdir'"

    capture erase "`bea_zip'"
}

* ----------------------------
* IRS SOI: state-level migration files
* ----------------------------
forvalues yy = $start_yy_irs_download/$end_yy_irs_migration {
    local zz = `yy' + 1
    local fn_sout "stateoutflow`yy'`zz'.csv"
    local fn_sin  "stateinflow`yy'`zz'.csv"

    capture confirm file "${data}irs/`fn_sout'"
    if _rc {
        di as txt "Downloading (IRS SOI) `fn_sout' ..."
        copy "`irs_base'/`fn_sout'" "${data}irs/`fn_sout'", replace
    }

    capture confirm file "${data}irs/`fn_sin'"
    if _rc {
        di as txt "Downloading (IRS SOI) `fn_sin' ..."
        copy "`irs_base'/`fn_sin'" "${data}irs/`fn_sin'", replace
    }
}

* ----------------------------
* NYTimes COVID Data
* ----------------------------
local covid_dir "${data}covid"
local covid_url "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

* If we don't already have a COVID file, download.
local covid_file : dir "`covid_dir'" files "covid_nyt.csv"
if "`covid_file'"=="" {
    di as txt "Downloading (COVID)  ..."
    copy "`covid_url'" "`covid_dir'/covid_nyt.csv", replace
}

* ----------------------------
* Verify manual downloads (DOL + BLS)
* ----------------------------

local dol_dir "${data}demographic/dol/NDCP2022.xlsx"
local bls_dir "${data}demographic/bls/la.data.64.County"

capture confirm file `dol_dir'

if _rc != 0 {
    display "Error: The file `dol_dir' was not found."
    display "Execution of the do-file is stopping."
    exit
}

capture confirm file `bls_dir'

if _rc != 0 {
    display "Error: The file `bls_dir' was not found."
    display "Execution of the do-file is stopping."
    exit
}

* ----------------------------
* Census B01001: County Age Shares (created by 00_multnomah.R)
* ----------------------------
capture confirm file "${data}working/age_shares_county.csv"
if _rc {
    di as err "ERROR: age_shares_county.csv not found."
    di as err "Run 00_multnomah.R first to download Census age share data."
    exit 601
}

** Import and save as Stata dataset
capture confirm file "${data}working/age_shares_county.dta"
if _rc {
    import delimited "${data}working/age_shares_county.csv", clear
    label var fips "County FIPS code"
    label var share_under_24 "Share of population under age 24 (ACS 2015-2019)"
    label var share_over_65 "Share of population age 65+ (ACS 2015-2019)"
    save "${data}working/age_shares_county", replace
    clear
}
