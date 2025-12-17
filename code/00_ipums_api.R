# =============================================================================
# Author: John Iselin
# Date:   September 1st, 2025
# File:   00_caleitc.R
#
# Project: Analysis of the impact of CalEITC on labor supply.
# =============================================================================

## CLEAR ALL 
rm(list = ls())
gc()

## Packages to install
# install.packages('gtools')
# install.packages('fwildclusterboot', repos ='https://s3alfisc.r-universe.dev')

# --- Libraries ---
library(here)       # project-root relative paths
library(dplyr)      # data wrangling
library(data.table)
library(ggplot2)    # plotting
library(readr)      # data import
library(stringr)    # string helpers
library(ipumsr)     # download data 
library(fixest)     # fixed effects regressions 
library(fwildclusterboot) # Wild Cluster Bootstrap (install.packages('fwildclusterboot', repos ='https://s3alfisc.r-universe.dev'))
library(devtools)
library(blsR)
library(tidycensus)
library(tidyr)
library(purrr)
library(usincometaxes)
library(ggplot2)
library(scales)
library(ggthemes)
library(viridis) # install.packages("viridis")
library(data.table)
library(curl)
library(readxl) # install.packages('readxl')
#library(furrr)
library(modelsummary)
library(progressr)
handlers(global = TRUE)
library(glue)
library(tibble)
library(broom)
library(knitr)
library(openxlsx)
library(specr)

# --- Project Metadata ---
project_name <- "caleitc"
date_run     <- format(Sys.Date(), "%Y-%m-%d")

# --- File Paths (relative to Rproj root) ---
dir_code       <- here("code")
dir_data.      <- here("data")
dir_data_raw   <- here("data", "raw")
dir_data_int   <- here("data", "interim")
dir_data_fin   <- here("data", "final")
dir_results <- here("results")
dir_logs    <- here("code", "logs")

# --- Overleaf location ---
dir_olink <- "C:/Users/ji252/Dropbox/Apps/Overleaf/Iselin (2024)"
dir_ol_fig  <- paste(dir_olink,'figures', sep = "/") 
dir_ol_tab  <- paste(dir_olink,'tables', sep = "/") 

# --- Logging ---
logfile <- file.path(dir_logs, paste0(project_name, "_", date_run, ".log"))
sink(logfile, split = TRUE)  # send console output to both screen + log file

# --- Random Seed for Reproducibility ---
set.seed(56403)

# --- Get IPUMS and BLS API codes  --- 
if (file.exists("api_codes.txt") == TRUE) {
  
  ## Load values
  api_codes <- read.delim("api_codes.txt", sep = ",")
  
  ## Store values
  ipums_key <- str_trim(api_codes[1, 2])
  bls_key <- str_trim(api_codes[2, 2])
  
  # --- IPUMS KEY ---
  set_ipums_api_key( ipums_key, save = TRUE, overwrite = TRUE)
  
  # --- BLS KEY --- 
  bls_set_key(bls_key)
  
} else stop("STOP, NO API KEYS")

# --- Parameters ---
start_year       <- 2012
end_year         <- 2017
start_year_data  <- 2006
end_year_data    <- 2019

overleaf <- FALSE   # save results into Overleaf folder as well
debug    <- FALSE  # debugging mode (optional hooks below)

acs_prep <- FALSE  # ACS prep option (skip unless needed, slow)
ipums_opt <- TRUE  # IPUMS prep option

## Overwrite IPUMS files (1 = yes, 0 = no)
overwrite_ipums <- 0

## Overwrite BLS files (1 = yes, 0 = no)
overwrite_bls <- 0

# --- Fonts / Plotting Theme ---
#theme_set(theme_minimal(base_family = "Times New Roman"))

# COLORS 

col1 <- "#000000"
col2 <- "#C0392B"
col2 <- "#87CEEB"
col3 <- "#2E8B57"


stata2r_colors <- c(
  black        = "#000000",      # black
  orangebrown   = "#D2691E",      # orange brown / chocolate
  sky           = "#87CEEB",      # sky blue
  turquoise     = "#40E0D0",      # turquoise
  reddish       = "#C0392B",      # reddish (brick / red-brown)
  vermillion     = "#E34234",     # vermillion
  sea           = "#2E8B57",      # sea green / medium sea green
  ananas        = "#FFD700",       # “ananas” (pineapple) ~ golden yellow
  gs10          = "#D4D4D4"      # light gray "#4C4C4C",
)

tmp_stata2r_colors <- c(
  black        = "#000000",      # black
  gs10          = "#D4D4D4",      # light gray "#4C4C4C",
  sky           = "#87CEEB",      # sky blue
  turquoise     = "#40E0D0",      # turquoise
  orangebrown   = "#D2691E",      # orange brown / chocolate
  reddish       = "#C0392B",      # reddish (brick / red-brown)
  vermillion     = "#E34234",     # vermillion
  sea           = "#2E8B57",      # sea green / medium sea green
  ananas        = "#FFD700"       # “ananas” (pineapple) ~ golden yellow
)

# Set colors for number of QC 
col_qc0 <- "#f0f9e8"
col_qc1 <- "#bae4bc"
col_qc2 <- "#7bccc4"
col_qc3 <- "#2b8cbe"

# Run all files in util 
source(here("code","utils", "qc_assignment.R"))   
source(here("code","utils", "regression_commands.R"))   

# =============================================================================
# (01) DATA CREATION
# =============================================================================

source(here("code", "01_data_prep_other.R"))   # Download all non-IPUMS Data
source(here("code", "01_data_prep_ipums.R"))   # ACS + CPS Datasets Download

# =============================================================================
# (02) FIGURES and TABLES
# =============================================================================

# Figure 1: EITC Plot 
source(here("code", "02_fig_1"))   

# Load and clean EITC schedule data
source(here("code", "02_eitc_param_prep.R"))

# Calculation of Marginal Value of Public Funds (MVPF)
source(here("code", "02_MVPF.R"))

# --- Close Log ---
sink()