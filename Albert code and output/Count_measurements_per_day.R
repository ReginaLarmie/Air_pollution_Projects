# =========================================================
# Title: Count_measurements_per_day.R    
# Author: ASC  
# Date: 2026_0320    
# Purpose: Create a table and graph illustrating the number of CinC measurements
#          recorded and passing QC for each day in a monitoring interval. This
#          can help us assess days that have anomalously low numbers of 
#          measurements--could mean something faulty with measurements or 
#          data transmission and archiving or high rate of erratic values
#
# Description:
#   This takes an input file of data (a) compiled from all CinC sensors, 
#   then (b) arranged so that data from sensors deployed sequentially at same 
#   site are binned making the data site specific, and then (c) QC applied 
#   to screen out PM2.5 values that are > 250 ug/m3.
#
#   Then the code simply counts the number of measurements per day and plots
#   these values for each site. If a measurement is recorded every half hour
#   then we would see 48 data points per day.
#
# Inputs:
#   - CinC_final_table_v2.csv
#
# Outputs:
#   - CinC_final_table_v2-DataPerDay.csv
#   - figures/...
#   - tables/...
#
# Upstream / Downstream Workflow:
#   Run after: after Regina has run her chunks of code that generate the above
#               input file.
#   Run before:  
#
# Notes:

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
#   "Albert code and output"  = top-level folder
##   "raw"         = subfolder inside "data"
#   "CinC_final_table_v2.csv" = data file
# This avoids hardcoding absolute paths like C:/Users/...

input_file  <- here("Albert code and output", "CinC_final_table_v2.csv")       
                    # creats a string with path/filename for main input dataset
# output_file <- CinC_final_table_v2-DataPerDay.csv # processed data
# figure_file <- here("figures", "figure_name.png")           # output plot


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
CinC_DailyMeasurementCts <- read_csv(input_file) # creates the 
#                 tibble that will be processed in R from the input CSV file.
#
#  Column headers: Sensor ID, Site ID, Lat, Long, Date, Time, PM2.5
# Quick inspection
print(glimpse(CinC_DailyMeasurementCts))
print(summary(CinC_DailyMeasurementCts))




# ---- Reproducibility info ----
sessionInfo()

# Optional: save session info to file
# writeLines(capture.output(sessionInfo()),
#            here("output", "session_info.txt"))


# ---- Create daily measurement count table ----

# Optional but recommended: rename columns to simpler names
CinC_DailyMeasurementCts_clean <- CinC_DailyMeasurementCts %>%
  rename(
    site_id = `Site ID`,
    date    = Date
  )

# Make sure the date column is stored as a Date object
CinC_DailyMeasurementCts_clean <- CinC_DailyMeasurementCts_clean %>%
  mutate(date = as.Date(date))

# Create vector of all dates in desired range
date_seq <- seq.Date(
  from = as.Date("2024-11-01"),
  to   = as.Date("2025-12-31"),
  by   = "day"
)

# Extract unique site IDs from the input table
site_ids <- CinC_DailyMeasurementCts_clean %>%
  distinct(site_id)

# Create complete site x date grid
site_date_grid <- expand_grid(
  site_ids,
  date = date_seq
)

# Count number of measurements for each site and date in the original table
daily_counts <- CinC_DailyMeasurementCts_clean %>%
  group_by(site_id, date) %>%  # treats all site_id/date combos as being in the
                               # same group and keeps track of which group
                               # each row is in
  summarise(n_measurements = n()) %>%  # counts the number of rows in each
                                       # group and puts this into a new
                                       # n_measurements column in group_by
                                       # so group_by has side_id, date, n
  ungroup()

# Join counts onto the full site x date grid
# Missing site/date combinations will have NA, which we replace with 0
site_date_counts <- site_date_grid %>%
  left_join(daily_counts, by = c("site_id", "date")) %>%
  mutate(n_measurements = replace_na(n_measurements, 0)) %>%
  arrange(site_id, date)

output_file <- here("Albert code and output", "CinC_final_table_v2-DataPerDay.csv")

write_csv(site_date_counts, output_file)

## Sort site_date_counts so that sites will be in correct order, e.g., site
# IDs in original order 1.1, 12.1_5.2, 13.1_33.2, 20.2, 35.2, 5.1_19.3_24.2
# will be ordered 1.1, 5.1_19.3_24.2, 12.1_5.2, 13.1_33.2, 20.2, 35.2

site_levels <- site_date_counts %>%
  distinct(site_id) %>%
  mutate(
    site_id_leading = str_extract(site_id, "^[^_]+"),
    site_id_sort = as.numeric(site_id_leading)
  ) %>%
  arrange(site_id_sort, site_id) %>%
  pull(site_id)

site_date_counts <- site_date_counts %>%
  mutate(site_id = factor(site_id, levels = site_levels))

## Plotting routine

library(grid)

# Monthly breaks
x_breaks <- seq.Date(
  from = as.Date("2024-11-01"),
  to   = as.Date("2025-12-01"),
  by   = "1 month"
)

# Every other month labeled
x_labels <- c("Nov", "", "Jan", "", "Mar", "", "May", "", "Jul", "", "Sep", "", "Nov", "")

p <- site_date_counts %>%
  ggplot(aes(x = date, y = n_measurements)) +
  geom_point(size = 0.6) +
  geom_hline(yintercept = 48, linetype = "dashed", color = "lightblue") +
  facet_grid(
    rows = vars(site_id),
    switch = "y",
    axes = "all_x"
  ) +
  labs(
    title = "Daily Number of PM2.5 Measurements by Site",
    x = NULL,
    y = "Number of measurements per day"
  ) +
  scale_x_date(
    breaks = x_breaks,
    labels = x_labels,
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    breaks = seq(0, 60, by = 10),
    minor_breaks = NULL
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),   # ← centers the title
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, size = 8),
    strip.background = element_blank(),
    panel.spacing.y = unit(0.6, "lines"),   # increased spacing
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line()
  )

# defining height of the PDF file that is printout of plots 
n_sites <- n_distinct(site_date_counts$site_id)

height_per_site <- 0.9   # adjust this (0.7–1.2 works well)
pdf_height <- n_sites * height_per_site + 2

# Save only (no print)
ggsave(
  filename = file.path(dirname(rstudioapi::getActiveDocumentContext()$path),
                       "DailyMeasurementCounts_bySite.pdf"),
  plot = p,
  device = cairo_pdf,
  width = 11,
  height = pdf_height,
  units = "in"
)