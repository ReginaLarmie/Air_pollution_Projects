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
pm25_data <- read_csv(
  input_file
)                  

# ---- Add site_number column to pm25_data  
pm25_data <- pm25_data %>%
left_join(site_lookup, by = c("latitude", "longitude", "device_number"))  
pm25_data <- pm25_data %>%
  relocate(site_number) # shifts site_number to be the first column


# Assembling a table with Counts per Site for each day
#
library(dplyr)
library(tidyr)

# --------------------------------------------
# STEP 1: Create a correctly ordered list of site IDs
# --------------------------------------------
ordered_sites <- pm25_data %>%
  # Keep only one copy of each site_number
  distinct(site_number) %>%
  
  # Create temporary columns for sorting:
  #   major = number before the decimal
  #   minor = number after the decimal
  # Examples:
  #   "2.1"  -> major = 2,  minor = 1
  #   "11.2" -> major = 11, minor = 2
  mutate(
    major = as.numeric(sub("\\..*", "", site_number)),
    minor = as.numeric(sub(".*\\.", "", site_number))
  ) %>%
  
  # Sort numerically by the major part, then the minor part
  # This gives the desired order:
  #   2.1 before 11.1
  arrange(major, minor) %>%
  
  # Keep only the ordered site_number column as a vector
  pull(site_number)


# --------------------------------------------
# STEP 2: Count observed PM2.5 measurements per site per day
# --------------------------------------------
observed_counts <- pm25_data %>%
  # Create a new date column by removing the time-of-day
  # Example: "2025-05-09 19:34:08" -> "2025-05-09"
  mutate(
    date = as.Date(time),
    
    # Convert site_number to a factor using the desired site order
    # This preserves the correct numeric-aware ordering later
    site_number = factor(site_number, levels = ordered_sites)
  ) %>%
  
  # Count number of rows for each site on each date
  count(site_number, date, name = "n_measurements")


# --------------------------------------------
# STEP 3: Create a full grid of all site × date combinations
# --------------------------------------------
full_grid <- expand_grid(
  # Use the ordered site list and convert it to a factor
  # so the custom site ordering is preserved
  site_number = factor(ordered_sites, levels = ordered_sites),
  
  # Create every date in the target study period
  date = seq(as.Date("2024-11-01"), as.Date("2025-05-23"), by = "day")
)


# --------------------------------------------
# STEP 4: Join observed counts onto the full grid
#          and fill in missing combinations with zero
# --------------------------------------------
daily_counts <- full_grid %>%
  # Join the observed daily counts onto the complete site/date framework
  # This keeps every site/date combination, even if no data were observed
  left_join(observed_counts, by = c("site_number", "date")) %>%
  
  # Replace missing values with 0
  # Missing values occur when a site had no PM2.5 rows on a given date
  mutate(n_measurements = replace_na(n_measurements, 0)) %>%
  
  # Sort first by site_number using the factor order defined above,
  # then sort chronologically by date within each site
  arrange(site_number, date)


# --------------------------------------------
# OPTIONAL STEP 5: Convert site_number back to character
# --------------------------------------------
# This is optional. The factor version is useful for preserving the
# custom order, but sometimes you may prefer site_number to be plain text.
#  daily_counts <- daily_counts %>%
#  mutate(site_number = as.character(site_number))


# --------------------------------------------
# RESULT:
# daily_counts contains:
# - one row for every site on every day from 2024-11-01 to 2025-05-23
# - n_measurements = observed number of PM2.5 rows for that site/day
# - n_measurements = 0 when no PM2.5 data were recorded that day
# - site_number is ordered the way you intended (e.g., 2.1 before 11.1)
# --------------------------------------------

# Quick inspection
# glimpse(pm25_data)
# summary(main_data_tibble)

# ------------------------------
## Plotting routine
# ------------------------------

# Monthly breaks
x_breaks <- seq.Date(
  from = as.Date("2024-11-01"),
  to   = as.Date("2025-06-01"),
  by   = "1 month"
)

# Every other month labeled
x_labels <- c("Nov '24", "Dec", "Jan '25", "Feb", "Mar", "Apr", "May", "Jun")

p <- daily_counts %>%
  ggplot(aes(x = date, y = n_measurements)) +
  geom_point(size = 0.6) +
  geom_hline(yintercept = 48, linetype = "dashed", color = "lightblue") +
  facet_grid(
    rows = vars(site_number),
    switch = "y",
    axes = "all_x"
  ) +
  labs(
    title = "Daily Number of PM2.5 Measurements by Site (original CSV data)",
    x = NULL,
    y = "Number of measurements per day"
  ) +
  scale_x_date(
    breaks = x_breaks,
    labels = x_labels,
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 10),
    minor_breaks = NULL,
    expand = c(0, 0)
  ) +
#  coord_cartesian(clip = "on"
#  ) +
  theme(
    plot.title = element_text(hjust = 0.5),   # ← centers the title
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, size = 8),
    strip.background = element_blank(),
    panel.spacing.y = unit(1.0, "lines"),   # increased spacing
#    panel.border = element_rect(color = NA),
#    panel.background = element_rect(fill = "white", color = NA),

    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line()
  )

# defining height of the PDF file that is printout of plots 
n_sites <- n_distinct(daily_counts$site_number)

height_per_site <- 1.2   # adjust this (0.7–1.2 works well)
pdf_height <- n_sites * height_per_site + 2

# Save only (no print)
ggsave(
  filename = file.path(dirname(rstudioapi::getActiveDocumentContext()$path),
                       "DailyMeasurementCounts_bySite_CSVdata.pdf"),
  plot = p,
  device = cairo_pdf,
  width = 11,
  height = pdf_height,
  units = "in"
)

# ------------------------------
# Subset the three high-count sites -- sites that had some daily counts > 60
# ------------------------------
high_count_sites <- c("24.1", "28.1", "30.1_17.1")

daily_counts_high <- daily_counts %>%
  filter(site_number %in% high_count_sites)


# ------------------------------
# Plot only these three sites with autoscaled y-axes
# ------------------------------
p_high <- daily_counts_high %>%
  ggplot(aes(x = date, y = n_measurements)) +
  geom_point(size = 0.6) +
  geom_hline(yintercept = 48, linetype = "dashed", color = "lightblue") +
  facet_grid(
    rows = vars(site_number),
    switch = "y",
    axes = "all_x",
    scales = "free_y"
  ) +
  labs(
    title = "Daily Number of PM2.5 Measurements by Site (sites with counts > 60)",
    x = NULL,
    y = "Number of measurements per day"
  ) +
  scale_x_date(
    breaks = x_breaks,
    labels = x_labels,
    minor_breaks = NULL
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, size = 8),
    strip.background = element_blank(),
    panel.spacing.y = unit(3, "lines"),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.major.y = element_line(color = "grey80")
  )

ggsave(
  filename = file.path(dirname(rstudioapi::getActiveDocumentContext()$path),
                       "DailyMeasurementCounts_highCountSites_CSVdata.pdf"),
  plot = p_high,
  device = cairo_pdf,
  width = 11,
  height = 6,
  units = "in"
)




# ---- Reproducibility info ----
sessionInfo()

# Optional: save session info to file
# writeLines(capture.output(sessionInfo()),
#            here("output", "session_info.txt"))