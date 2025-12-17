
# =============================================================================
# Author: John Iselin
# Date:   August 17th, 2025
# File:   01_data_prep_ipums.R
#
# Load ACS Data VIA IPUMS
# Load CPS Data VIA IPUMS

# =============================================================================

# -----------------------------
# Define, Submit, and read ACS Extract Request
# -----------------------------

## PREFORM THIS PROCESS ITERATIVELY OVER YEARS 
years <- start_year_data:end_year_data
for (y in years) {
  
  # File path for the raw and processed RDS version
  rds_file_raw <- file.path(dir_data_raw, paste0("acs_", y, ".rds"))
  rds_file_int <- file.path(dir_data_int, paste0("acs_", y, ".rds"))
  
  if (!file.exists(rds_file_raw) | overwrite_ipums == 1) {
    
    message("Downloading ACS data for ", y, "via IPUMS...")
    
    # Define extract name 
    extract_name <- paste0("ACS  microdata for CalEITC, Year: ", y)
  
    ## Specified vars
    gq <- var_spec("GQ", case_selections = c("1", "2"))
    workedyr <- var_spec("WORKEDYR", data_quality_flags = TRUE) 
    empstat <- var_spec("EMPSTAT", data_quality_flags = TRUE) 
    empstatd <- var_spec("EMPSTATD", data_quality_flags = TRUE) 
    classwkr <- var_spec("CLASSWKR", data_quality_flags = TRUE) 
    wkswork2 <- var_spec("WKSWORK2", data_quality_flags = TRUE) 
    uhrswork <- var_spec("UHRSWORK", data_quality_flags = TRUE) 
    labforce <- var_spec("LABFORCE", data_quality_flags = TRUE) 
    inctot <- var_spec("INCTOT", data_quality_flags = TRUE) 
    incwage <- var_spec("INCWAGE", data_quality_flags = TRUE) 
    incbus00 <- var_spec("INCBUS00", data_quality_flags = TRUE) 
    incearn <- var_spec("INCEARN", data_quality_flags = TRUE) 
    incinvst <- var_spec("INCINVST", data_quality_flags = TRUE) 
    incwelfr <- var_spec("INCWELFR", data_quality_flags = TRUE) 
    incsupp <- var_spec("INCSUPP", data_quality_flags = TRUE) 
    incother <-  var_spec("INCOTHER", data_quality_flags = TRUE)
    
    ## Extract data 
    acs_data <- define_extract_micro(
      collection = "usa",
      description = extract_name,
      samples = paste0("us", y, "a"),  
      variables = list(
        "YEAR", 
        "SAMPLE", 
        "SERIAL", 
        "HHWT", 
        "PERWT", 
        "CLUSTER", 
        "STRATA",    
        "CPI99", 
        "STATEFIP", 
        "COUNTYFIP",   
        "FOODSTMP", 
        "PERNUM", 
        "MOMLOC", 
        "POPLOC", 
        "SPLOC", 
        "MOMLOC2", 
        "POPLOC2", 
        "RELATED",    
        "NCHILD", 
        "YNGCH",  
        "AGE", 
        "SEX",  
        "RACE", 
        "HISPAN", 
        "MARST", 
        "CITIZEN", 
        "SCHOOL", 
        "EDUCD", 
        gq,
        workedyr, 
        empstat,
        empstatd, 
        classwkr, 
        wkswork2,
        uhrswork,
        labforce,
        inctot, 
        incwage,
        incbus00,
        incearn,
        incinvst,
        incwelfr,
        incsupp,
        incother 
      )
    ) %>%
      submit_extract() %>%
      wait_for_extract() %>%
      download_extract(download_dir = dir_data_raw, overwrite = TRUE) %>%  
      read_ipums_micro() %>%
      rename_with(tolower)
    
    # Save file
    saveRDS(acs_data, rds_file_raw)
    
    # Remove from memory
    rm( gq,
        workedyr, 
        empstat,
        empstatd, 
        classwkr, 
        wkswork2,
        uhrswork,
        labforce,
        inctot, 
        incwage,
        incbus00,
        incearn,
        incinvst,
        incwelfr,
        incsupp,
        incother )
    
    gc()
    
    # DF move 
    df <- acs_data
    rm(acs_data)
    
  }  else {
  message("ACS file for ", y, " already exists — skipping download from IPUMS.")
    
  ## LOAD Data
  df <- readRDS(rds_file_raw) 
    
  } # END IF-ELSE-STATEMENT

  ## Convert to data.table
  setDT(df)
  
  # ----------------------
  # PREP FOR QC ASSIGNMENT
  # ----------------------  
  
  message("Step 1: Run pre-QC-assignment code.")
  
  # Make household ID
  df[, hh_id := .GRP, by = .(year, serial)]
  
  # Drop unnecessary variables
  df[, c("sample","cbserial","serial","relate") := NULL]

    # Rename related 
  setnames(df, "related", "relate")
  
  # Define basic age flags 
  df[, `:=`(
    child = as.integer(age <= 17),
    adult = as.integer(age >= 18),
    elder = as.integer(age >= 65)
  )]
  
  # Tax unit ID (smaller of two person locations in HH)
  df[, unit_id := fifelse(marst == 1 & sploc != 0, pmin(pernum, sploc), pernum)]
  
  # Count of individuals per married couple
  df[, unit_ct := .N, by = .(hh_id, unit_id)]
  
  # QC tests
  df[, `:=`(
    age_test     = fifelse(age < 19 | (age < 24 & school == 2), 1L, 0L),
    citizen_test = fifelse(citizen != 3, 1L, 0L),
    joint_test   = fifelse(!(marst %in% c(1, 2, 3)), 1L, 0L)
  )]
  df[, qc := fifelse(age_test == 1 & citizen_test == 1 & joint_test == 1, 1L, 0L)]
  
  # Relationship flags
  df[, `:=`(
    hoh        = as.integer(relate == 101),
    sibling    = as.integer(relate == 701),
    foster     = as.integer(relate == 1242),
    grandchild = as.integer(relate == 901)
  )]
  
  # -----------------
  # RUN QC ASSIGNMENT
  # -----------------  
  
  message("Step 2: Run QC-assignment code.")
  
  # Run QC assignment program (keep as is, just feed a data.table)
  df_qc <- qc_assignment_vec(df)
  
  # Set output of QC program to be a data.table
  setDT(df_qc) 
  
  # Replace data frame 
  df <- copy(df_qc)   # replace df
  
  # Clean 
  rm(df_qc); gc()
  
  # ----------------
  # RUN HH COMP CODE
  # ----------------  
  
  message("Step 3: Create household composition variables.")
  
  # Count of kids, parents, etc... at the individual-level.
  df[, `:=`(
    qc_ct        = pmin(qc_ct, 3L),
    kid_ct       = pmin(nchild, 3L),
    qc_present   = as.integer(qc_ct > 0),
    kid_present  = as.integer(nchild > 0),
    mom_present  = as.integer(momloc  != 0),
    dad_present  = as.integer(poploc  != 0),
    mom2_present = as.integer(momloc2 != 0),
    dad2_present = as.integer(poploc2 != 0))]
  
  # Same, but needs to be in separate line
  df[, `:=`(
    parent_ct    = mom_present + dad_present + mom2_present + dad2_present)]
  
  # Count of kids, parents, etc... at the hh-level.
  df[, `:=`(
    hh_person_ct = .N,
    hh_child_ct  = sum(child, na.rm=TRUE),
    hh_qc_ct     = sum(qc, na.rm=TRUE),
    hh_adult_ct  = sum(adult, na.rm=TRUE),
    hh_elder_ct  = sum(elder, na.rm=TRUE),
    hh_child_present = as.integer(sum(child, na.rm=TRUE) > 0),
    hh_qc_present    = as.integer(sum(qc, na.rm=TRUE) > 0)
  ), by = hh_id]
  
  # Drop unneeded vars
  drop_cols <- c("momloc","poploc","momloc2","poploc2","sploc")
  df[, (drop_cols) := NULL]  
      
  # Minimum age of QC and Kids  
      
  # Compressed brackets of minimum age among QC
  df[, minage_qc_compr := fifelse(
    is.na(min_qc_age), 0L,
    fifelse(min_qc_age >= 1 & min_qc_age <= 5, 1L,
            fifelse(min_qc_age >= 6 & min_qc_age <= 12, 2L,
                    fifelse(min_qc_age >= 13 & min_qc_age <= 23, 3L, 0L))))
  ]
  
  # Bracketed minimum age among QC
  df[, minage_qc := fifelse(
    is.na(min_qc_age), 0L,
    fifelse(min_qc_age >= 0 & min_qc_age <= 1, 1L,
            fifelse(min_qc_age >= 2 & min_qc_age <= 3, 2L,
                    fifelse(min_qc_age >= 4 & min_qc_age <= 6, 3L,
                            fifelse(min_qc_age >= 7 & min_qc_age <= 9, 4L,
                                    fifelse(min_qc_age >= 10 & min_qc_age <= 13, 5L,
                                            fifelse(min_qc_age >= 14 & min_qc_age <= 17, 6L,
                                                    fifelse(min_qc_age >= 18 & min_qc_age <= 24, 7L, 0L))))))))
  ]
  
  # Bracketed minimum age among own children
  df[, minage_kid := fifelse(
    kid_present == 0, 0L,
    fifelse(yngch >= 0 & yngch <= 1, 1L,
            fifelse(yngch >= 2 & yngch <= 3, 2L,
                    fifelse(yngch >= 4 & yngch <= 6, 3L,
                            fifelse(yngch >= 7 & yngch <= 9, 4L,
                                    fifelse(yngch >= 10 & yngch <= 13, 5L,
                                            fifelse(yngch >= 14 & yngch <= 17, 6L,
                                                    fifelse(yngch >= 18 & yngch <= 24, 7L, 0L))))))))
  ]
  
  # Drop unneeded columns
  df[, c("yngch", "nchild") := NULL]
    
  # Restrict sample: drop if under 18
  df <- df[age >= 18]    

  # --------------------
  # Demographic variables 
  # --------------------  
  
  message("Step 4. Assign demographic variables")
  
  # Age samples 
  df[, age_sample_20_50 := as.integer(between(age, 20, 50))]
  df[, age_sample_20_49 := as.integer(between(age, 20, 49))]
  df[, age_sample_25_54 := as.integer(between(age, 25, 54))]
  
  # Educational attainment 
  df[, education := fcase(
    educd <= 61, 1L,
    educd %in% 62:64, 2L,
    educd %in% 65:80, 3L,
    educd > 80, 4L,
    default = NA_integer_
  )]
  
  # Currently in school 
  df[, in_school := as.integer(school == 2)]
  
  # 5-year age brackets (20–54)
  df[, age_bracket := cut(
    age,
    breaks = c(seq(20, 55, by = 5),Inf),   # 20, 25, 30, ..., 55
    right = FALSE,                  # intervals are [20,25), [25,30), etc.
    labels = c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55+")
  )]
  
  # 10-year age brackets (20–49)
  df[, age_bin := cut(
    age,
    breaks = c(seq(20, 50, by = 10), Inf),  # 20, 30, 40, 50, Inf
    labels = c("20-29", "30-39", "40-49", "50+"),
    right = FALSE
  )]
  
  # 10-year age brackets (25–54)
  df[, age_grps_prime :=  cut(
    age,
    breaks = c(seq(25, 55, by = 10), Inf),  # 20, 30, 40, 50, Inf
    labels = c("25-34", "35-44", "45-54", "55+"),
    right = FALSE
  )]
   
  # Marital status: married = marst 1 or 2
  df[, married := as.integer(marst %in% c(1,2))]
  
  # Marital status: married filing separately (MFS) = marst 3
  df[, mfs := as.integer(marst == 3)]
  
  # Hispanic 
  df[, hispanic := as.integer(hispan != 0)]
  
  # Race 
  df[, race_group := fcase(
    race == 1, 1L,            # White
    race == 2, 2L,            # Black
    race %in% c(4,5,6), 3L,   # Asian
    default = 4L              # Other
  )]
      
  # Race x Hispanic categories
  df[, race_hisp := fcase(
    hispan != 0, 1L,             # Hispanic
    hispan == 0 & race == 1, 2L, # Non-Hisp White
    hispan == 0 & race == 2, 3L, # Non-Hisp Black
    default = 4L              # Other
  )]

      
  # Male/female indicator (ACS sex: 1=male, 2=female)
  df[, female := as.integer(sex == 2)]
  
  # Drop columns
  df[, c("hispan","hispand","sex","educ","educd",
         "relate","race","raced","citizen","school",
         "marst") := NULL]
  
  # --------------------
  # Employment variables 
  # --------------------  
  
  message("Step 5. Assign employment variables")
  gc()
  
  # Employed last year
  df[, employed_y := as.integer(workedyr == 3)]
  df[, employed_y_reported := as.integer(qworkedy == 0)]
  
  # Employed last week
  df[, employed_w := as.integer(empstat == 1)]
  df[, employed_w_reported := as.integer(qempstat == 0)]
  
  # In the labor force last week
  df[, labor_force_w := as.integer(empstat %in% c(1,2))]
  df[, labor_force_reported := as.integer(qempstat == 0)]
  
  # Hours worked last year
  df[, hours_worked_y := fifelse(
    employed_y == 1 & !is.na(uhrswork) & uhrswork > 0 & uhrswork < 99,
    as.numeric(uhrswork), NA_real_
  )]
  df[, hours_worked_y_reported := as.integer(quhrswor == 0)]

  # Weeks worked last-year (bin mid-points)
  df[, weeks_worked_y := fcase(
    wkswork2 == 1, 7,   # midpoint 1-13
    wkswork2 == 2, 20,  # etc.
    wkswork2 == 3, 33,
    wkswork2 == 4, 44,
    wkswork2 == 5, 48.5,
    wkswork2 == 6, 51,
    employed_y == 0, 0,
    default = 0
  )]
  df[, weeks_worked_y_reported := as.integer(qwkswork2 == 0)]
  df[, wkswork2 := as.character(wkswork2)]  
  
  ## Worked part-time last year 
  df[, part_time_y := as.integer(employed_y & !is.na(hours_worked_y) & hours_worked_y < 35)]
  df[, part_time_y_reported := as.integer(quhrswor == 0 & qworkedy == 0)]
  
  ## Worked full-time last year 
  df[, full_time_y := as.integer(employed_y & !part_time_y)]
  df[, full_time_y_reported := as.integer(quhrswor == 0 & qworkedy == 0)]
  
  # Self-employed last week 
  df[, self_employed_w := as.integer(classwkr == 1)]
  df[, self_employed_w_reported := as.integer(qclasswk == 0)]
  
  # Self-employed last year 
  df[, se_income_y := fcase(
    is.na(incbus00), as.numeric(0),
    incbus00 == 999999, as.numeric(0),
    default = as.numeric(incbus00)
  )]
  df[, self_employed_y := as.integer(se_income_y != 0)]
  df[, self_employed_pos_y := as.integer(se_income_y > 0)]
  df[, self_employed_y_reported := as.integer(qincbus == 0)]
  
  # Armed services 
  df[, armed_services := as.integer(empstatd %in% c(14, 15))]
  
  # Clean up
  df[, c("workedyr","empstat","classwkr","uhrswork","labforce",
         "quhrswor","qwkswork2","qclasswk","qempstat","qworkedy") := NULL]    

  # ------------------
  # Earnings variables 
  # ------------------  
  
  message("Step 6. Assign earnings variables")
  gc()

  # Total Income 
  df[, inctot_real := inctot * cpi99 / 0.652]
  df[, inctot_nom  := inctot]
  
  # Earned Income 
  df[, incearn_real := incearn * cpi99 / 0.652]
  df[, incearn_nom  := incearn]
  df[, incearn_reported := (qincwage == 0 & qincbus == 0)]
  
  # Wage Income
  df[, incwage_real := incwage * cpi99 / 0.652]
  df[, incwage_nom  := incwage]
  df[, incwage_reported := (qincwage == 0)]
  
  # Welfare Income 
  df[, incwel_real := fifelse(is.na(incwelfr) | incwelfr==999999, 
                              as.numeric(0), 
                              as.numeric(incwelfr * cpi99 / 0.652))]
  df[, incwel_nom  := fifelse(is.na(incwelfr) | incwelfr==999999, 
                              as.numeric(0), 
                              as.numeric(incwelfr))]
  df[, incwel_reported := (qincwelf == 0)]
  
  # Household-level sums
  df_hh <- df[inctot_nom > 0 & !is.na(inctot_nom),
               .(
                 inctot_hh_real   = sum(inctot_real),
                 inctot_hh_nom    = sum(inctot_nom),
                 incearn_hh_real  = sum(incearn_real),
                 incearn_hh_nom   = sum(incearn_nom),
                 incearn_hh_rep   = min(incearn_reported, na.rm=TRUE),
                 incwage_hh_real  = sum(incwage_real),
                 incwage_hh_nom   = sum(incwage_nom),
                 incwage_hh_rep   = min(incwage_reported, na.rm=TRUE)
               ), by = hh_id]
  
  df <- merge(df, df_hh, by="hh_id", all.x=TRUE)
  rm(df_hh); gc()
  
  # ---------------------------------------------
  # Merge with Unemployment and Minimum Wage Data  
  # ---------------------------------------------
  
  message("Step 7. Merge with unemployment and minimum wage Data")
  gc()  
  
  # State Unemployment 
  st_unemp <- readRDS(file.path(dir_data_int, "bls_state_unemployment_annual.rds"))
  setDT(st_unemp)
  st_unemp <- st_unemp[year == y, .(state_fips = as.integer(state_fips), 
                                    state_unemp=value)]
  
  
  # County unemployment 
  ct_unemp <- readRDS(file.path(dir_data_int, "bls_county_unemployment_annual.rds"))
  setDT(ct_unemp)
  ct_unemp   <- ct_unemp[year == y,
                         .(state_fips=as.integer(state_fips),
                           county_fips=as.integer(county_fips),
                           fips=as.integer(fips),
                           county_unemp=value)]
  
  # State Minimum wages 
  st_minwage <- readRDS(file.path(dir_data_int, "VKZ_state_minwage_annual.rds"))
  setDT(st_minwage)
  st_minwage <- st_minwage[year == y, .(state_fips=as.integer(state_fips), state_minwage)]
  
  # Merge keys
  df[, state_fips := as.integer(statefip)]
  df[, county_fips := as.integer(countyfip)]
  
  # Merge State Unemployment
  df <- merge(df, st_unemp,   by="state_fips", all.x=TRUE)
  
  # Check merge
  df[, .(
    total_rows = .N,
    merged_values = sum(!is.na(state_unemp)),
    missing_values = sum(is.na(state_unemp))
  )]
  
  # Merge State minimum wage 
  df <- merge(df, st_minwage, by="state_fips", all.x=TRUE)
  
  # Check merge
  df[, .(
    total_rows = .N,
    merged_values = sum(!is.na(state_minwage)),
    missing_values = sum(is.na(state_minwage))
  )]
  
  # Merge county unemployment rates 
  df <- merge(df, ct_unemp,   by=c("state_fips","county_fips"), all.x=TRUE)
  
  # Check merge
  df[, .(
    total_rows = .N,
    merged_values = sum(!is.na(county_unemp)),
    missing_values = sum(is.na(county_unemp))
  )]
  
  # -----------------------------------------
  # Fill in missing county unemployment rates 
  # -----------------------------------------
  
  message("Step 7b. Fill in missing county unemployment rates ")
  gc()  
  
  # Get list of observed counties 
  df_wcounty <- unique(df[!is.na(county_unemp),  .(state_fips, county_fips)])
  
  # Keep only unmatched counties and get state average 
  ct_unemp_unmatched <- ct_unemp[!df_wcounty, on = c("state_fips","county_fips")] 
  state_unmatched_avg <- ct_unemp_unmatched[, .(average = mean(county_unemp, na.rm = TRUE)), by = state_fips]
  
  # Merge with data 
  df <- merge(df, state_unmatched_avg,   by="state_fips", all.x=TRUE)
  
  # Update value 
  df[, county_unemp := fifelse(is.na(county_unemp), average, county_unemp)]
  
  rm(state_unmatched_avg, ct_unemp_unmatched, df_wcounty)
  
  df[, c("statefip", "countyfip", "average") := NULL]
  
  message("Step 8: State treatment assignment")
  
  # Initialize variable
  df[, state_status := 1L]  # default: no EITC change
  
  # Treated state: California
  df[state_fips == 6L, state_status := 2L]
  
  # States with EITC changes (set to 0)
  eitc_change_states <- c(
    8, 9, 15, 17, 19, 20, 22, 23, 24, 25,
    26, 27, 30, 34, 35, 39, 41, 44, 45, 50, 55
  )
  df[state_fips %in% eitc_change_states, state_status := 0L]
  
  # Excluded states (Alaska and DC)
  excluded_states <- c(2, 11)
  df[state_fips %in% excluded_states, state_status := -1L]
  
  # Restrict sample: drop if assigned as a QC 
  df <- df[qc == 0]    
  
  ## SAVE
  message("Year ", y, " done. Saving.")
  
  saveRDS(df, rds_file_int)
    
  # Drop working files 
  rm(df, st_unemp, st_minwage, ct_unemp)
  gc()
    
  
} # END YEAR LOOP 

