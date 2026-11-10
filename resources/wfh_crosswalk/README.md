# WFH (work-from-home) crosswalk inputs

Source files for `code/R/build_wfh_crosswalk.R`, which writes
`resources/wfh_teleworkable_occ.csv`: the Dingel & Neiman (2020) teleworkable
share for each ACS public-use occupation code (IPUMS `OCC`), by code vintage.
Used by the WFH heterogeneity panel in `code/stata/02_did_analysis.do`.

| File | What it is | Source |
|---|---|---|
| `dn_occupations_workathome.csv` | Dingel & Neiman teleworkable flag (0/1) by O*NET-SOC 2010 code | `occ_onet_scores/output/occupations_workathome.csv` in github.com/jdingel/DingelNeiman-workathome (master, commit 3ebab4f6, 2025-10-15) |
| `census_pums_occ_2013.xlsx` | ACS PUMS occupation code list, 2010 Census codes (ACS 2013-2017) -> SOC 2010 | https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/2013-acs-1-year-and-2014-sipp-pums-occupation-code-list2.xlsx |
| `census_pums_occ_2018.xlsx` | ACS PUMS occupation code list, 2018 Census codes (ACS 2018+) -> SOC 2018 (sheet `ACS`) | https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/2018-ACS-PUMS-and-2018-SIPP-Public-Use-Occupation-Code-List.xlsx |
| `bls_soc_2010_to_2018.xlsx` | BLS SOC 2010 -> SOC 2018 crosswalk | https://www.bls.gov/soc/2018/soc_2010_to_2018_crosswalk.xlsx |

Downloaded 2026-11-10.

## Construction (see the script header for detail)

1. SOC 2010 share = unweighted mean of the D&N binary over each SOC code's
   O*NET-SOC detail codes. D&N weight by O*NET respondent counts, which they
   do not publish; this affects only SOC codes with mixed detail codes.
2. SOC 2018 share = mean of the SOC 2010 shares that map to it (BLS file).
3. An ACS occupation code's share = mean over the SOC codes it combines
   (the Census list's "Combines:" rows, X wildcards, and broad/minor group
   codes expanded as prefixes).
4. SOC codes O*NET does not rate ("All Other" residuals, legislators) take the
   mean of rated codes sharing the first four SOC digits, D&N's rule (they
   weight by OES employment; this is unweighted).
5. Military codes (55-xxxx) are set to 0. Code 9920 (unemployed, no work in
   the last 5 years) is not in the file; the DiD treats it as "no occupation".

D&N do not dichotomize; `${wfh_share_cutoff}` (globals.do, 0.5) does.
