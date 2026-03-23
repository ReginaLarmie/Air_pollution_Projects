# =========================================================
# Title: CinC_originalCSVdata_DailyMeasurementCounts.R    
# Author: ASC   
# Date: 3/23/26    
# Purpose/Description: Inputs the processed CSV data (original emailed by 
#   Chris Nurre emailed in May 2025 with CLEANinCLE data from 11/24 - 5/25).  
#   These data were sorted to inlcude only PM2.5 data, sorted by sensor # and
#   location.
#
#   In this file, sensor ID, lat/long, and date info are used to pair each data
#   point with a site ID number.  This number is created using the same approach
#   used for the full API CinC data set from 11/24 - 12/25. Sensor ID numbers are
#   considered to be sensor_number.deployment_number (which we don't work up
#   explicitly in code).  Then site number is a string of all Sensor ID numbers
#   that were used in sequence at a single site location.  These Sensor ID numbers
#   are concatenated together using "_" as a separator. The resultant Site ID
#   numbers grow over time as additional sensors are deployed in the same position.
#   Ultimate, we need to develop a fixed naming position for sensors, but this
#   current system is useful for explicitly identifying each sensor that is used
#   for a single location.  
#
#   The Site ID numbers were developed in an offline CSV that is input here,
#   and the Site ID numbers are then assigned to the sensor records in the input
#   CSV data file.
#
#   Then we use the routine for counting the number of data points/day/site
#   that was developed for the full API PM2.5 data set in another R code.  These
#   data are then plotted.
#
# Inputs:
#   - pm2.5_data_presitekey.csv
#   - site_key_May2025_sitenumberadded.csv
#
# Outputs:
#   - #
#
# Upstream / Downstream Workflow:
#   Run after:  Inputs are CSV files that were generated when running:
#                 Ingesting_sorting_CinC_originalCSVdata.R and then an edited
#                 CSV file called site_key_May2025_sitenumberadded
#
#   Run before:  
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

input_file  <- "Albert code and output/data/processed/pm2.5_data_presitekey.csv"
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

# ---- Read data ----
site_lookup <- read_csv("Albert code and output/data/processed/site_key_May2025_sitenumberadded.csv")
pm2.5_data <- read_csv(
  input_file
)                  

# ---- Add site_number column to pm25_data  
pm2.5_data <- pm2.5_data %>%
left_join(site_lookup, by = c("latitude", "longitude", "device_number"))  



# Quick inspection
glimpse(pm2.5_data)
# summary(main_data_tibble)




# ---- Reproducibility info ----
sessionInfo()

# Optional: save session info to file
# writeLines(capture.output(sessionInfo()),
#            here("output", "session_info.txt"))