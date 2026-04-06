# =========================================================
# Title:    
# Author:   
# Date:     
# Purpose:  
#
# Description:
#   Briefly describe what this script does, why it exists,
#   and where it fits in the workflow.
#
# Inputs:
#   - data/raw/...
#   - data/external/...
#
# Outputs:
#   - data/processed/...
#   - figures/...
#   - tables/...
#
# Upstream / Downstream Workflow:
#   Run after:   
#   Run before:  
#
# Notes:
#   - List important assumptions
#   - Required columns: ...
# =========================================================

# ---- Libraries ----
library(tidyverse)   # data import, wrangling, plotting
library(lubridate)   # dates and times
library(here)        # reproducible file paths

# Optional (uncomment if needed)
# library(sf)         # spatial data / mapping
# library(data.table) # fast processing for large datasets

# ---- Reproducibility (optional but often recommended) ----
# If using renv for package management:
# renv::activate()   # ensures project-specific library is used
# renv::snapshot()   # (run manually when updating packages)

# ---- Global options ----
options(stringsAsFactors = FALSE) # character vs factor conversion for older R
options(dplyr.summarise.inform = FALSE) # suppress messages on grouping commands
theme_set(theme_minimal()) # sets defaults for grids, lines, etc. in ggplot2

# ---- File paths ----
# here("data", "raw", "file.csv") builds a path relative to the project root:
#   "data"        = top-level folder
#   "raw"         = subfolder inside "data"
#   "file.csv"    = file name
# This avoids hardcoding absolute paths like C:/Users/...

input_file  <- here("data", "raw", "input_file.csv")       # main input dataset
output_file <- here("data", "processed", "output_file.csv") # cleaned/processed data
figure_file <- here("figures", "figure_name.png")           # output plot

# Optional additional inputs
# lookup_file <- here("data", "raw", "site_lookup.csv")      # lookup/reference table
# shape_file  <- here("data", "external", "boundary.shp")    # spatial boundary file

# ---- Parameters / constants ----
# start_date <- as.Date("2025-01-01")   # analysis start date
# end_date   <- as.Date("2025-12-31")   # analysis end date
# edit this section as appropriate - add more

timezone_local <- "America/New_York"  # local timezone

# ---- Expected columns ----
# input_file should contain:
# device_id, datetime, pm25, latitude, longitude

# ---- Sanity checks for codes that take a long time to run ----
# stopifnot(file.exists(input_file))
# stopifnot(file.exists(lookup_file))

# ---- Read data ----
input_file <- read_csv(input_file.csv)

# Quick inspection
glimpse(input_file)
summary(input_file)




# ---- Reproducibility info ----
sessionInfo()

# Optional: save session info to file
# writeLines(capture.output(sessionInfo()),
#            here("output", "session_info.txt"))