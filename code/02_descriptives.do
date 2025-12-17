/*******************************************************************************
File Name: 		02_descriptives.do
Creator: 		John Iselin
Date Update:	December 17, 2025

Called by: 00_multnomah.do

Purpose: Preform descriptive analysis

Authors: John Iselin 

For more information, contact john.iselin@yale.edu

*******************************************************************************/

** Start log file 
capture log close log_02
log using "${logs}02_log_descriptives_${date}", replace text name(log_02)

** Determine set of common in- and out-migration counties for Multnomah
use ${data}working/irs_county_flow.dta, clear 

** Merge with ACS county data 
merge 1:1 year fips_o fips_d using ${data}working/acs_county_flow


** Close log
clear 
log close log_02
