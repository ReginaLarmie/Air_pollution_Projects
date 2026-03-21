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
start_date <- as.Date("2025-01-01")   # analysis start date
end_date   <- as.Date("2025-12-31")   # analysis end date

pm25_threshold <- 35                  # EPA 24-hr standard (µg/m³)
timezone_local <- "America/New_York"  # local timezone

# ---- Expected columns ----
# input_file should contain:
# device_id, datetime, pm25, latitude, longitude

# ---- Sanity checks ----
stopifnot(file.exists(input_file))
# stopifnot(file.exists(lookup_file))

# ---- Read data ----
df <- read_csv(input_file)

# Quick inspection
glimpse(df)
summary(df)

# ---- Data cleaning / preprocessing ----
# df_clean <- df %>%
#   mutate(
#     datetime = ymd_hms(datetime, tz = timezone_local),
#     date = as.Date(datetime)
#   ) %>%
#   filter(date >= start_date, date <= end_date)

# ---- Analysis / transformation ----
# df_summary <- df_clean %>%
#   group_by(device_id, date) %>%
#   summarise(
#     mean_pm25 = mean(pm25, na.rm = TRUE),
#     max_pm25  = max(pm25, na.rm = TRUE),
#     n_obs     = sum(!is.na(pm25))
#   ) %>%
#   ungroup()

# ---- Spatial conversion (if needed) ----
# df_sf <- df_clean %>%
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# ---- Plotting (if needed) ----
# p <- df_summary %>%
#   ggplot(aes(x = date, y = mean_pm25, color = device_id)) +
#   geom_line() +
#   labs(
#     title = "Daily Mean PM2.5 by Device",
#     x = "Date",
#     y = expression(PM[2.5]~(mu*g/m^3))
#   )
#
# print(p)
# ggsave(filename = figure_file, plot = p, width = 8, height = 5, dpi = 300)

# ---- Write outputs ----
# write_csv(df_summary, output_file)

# ---- Final checks / diagnostics ----
# glimpse(df_summary)
# count(df_clean, device_id)
# skimr::skim(df_clean)   # optional package

# ---- Reproducibility info ----
sessionInfo()

# Optional: save session info to file
# writeLines(capture.output(sessionInfo()),
#            here("output", "session_info.txt"))