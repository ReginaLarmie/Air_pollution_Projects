# =========================================================
# Title: Ingesting_sorting_CinC_originalCSVdata.R    
# Author: ASC   
# Date: 3/23/26    
# Purpose/Description: Inputs the original CSV data that Chris Nurre emailed  
#   in May 2025 with CLEANinCLE data from Nov. 2024 - May 2025.  These data
#   had been slightly filtered to remove data that appeared erroneous. We do
#   not yet have the criteria used for filtering. Subsets only the PM2.5 data
#   from this CSV. Then exports all the unique lat/long/sensor # combos into
#   a CSV that will be used offline to assign site_number values for each site
#   that include sensor ID and deployment number concatenated for each sensor
#   used at a particular site.  
#
# Inputs:
#   - CLEANinCLE Raw Air Quality Data 2024-11 - 2025-05 CSV.csv
#
# Outputs:
#   - pm2.5_data_presitekey.csv
#   - site_key_for_editing.csv  # a file with all unique combinations of lat/long/sensor #
#
# Upstream / Downstream Workflow:
#   Run after:  no code required prior to this 
#   Run before:  next step will be code to insert and populate site_number column
#
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

input_file  <- "Albert code and output/data/raw/CLEANinCLE Raw Air Quality Data 2024-11 - 2025-05 CSV.csv"
                 # main input dataset
# output_file <- here("data", "processed", "output_file.csv") # cleaned/processed data
# figure_file <- here("figures", "figure_name.png")           # output plot

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

# ---- Read, filter/sort, and export data ----
pm2.5_data_presitekey <- read_csv(
  input_file, col_select = c(location_name, latitude, longitude, device_number, 
                             device_eui, time, measure_name, value)
) %>%                    # creates the tibble that will be 
                         # processed in R from the input CSV file.
  filter(measure_name == "mc_pm2_5") %>%
  arrange(latitude, longitude, time) # sorts tibble by lat, long, then time

write_csv(pm2.5_data_presitekey, "Albert code and output/data/processed/pm2.5_data_presitekey.csv")

# ---- Export unique lat/long/device_number combos
site_key_for_editing <- pm2.5_data_presitekey %>%
  distinct(latitude, longitude, device_number) %>%
  arrange(latitude, longitude, device_number)

write_csv(site_key_for_editing, "Albert code and output/data/processed/site_key_for_editing.csv")

# Quick inspection
# glimpse(main_data_tibble)
# summary(main_data_tibble)




# ---- Reproducibility info ----
sessionInfo()

# Optional: save session info to file
# writeLines(capture.output(sessionInfo()),
#            here("output", "session_info.txt"))