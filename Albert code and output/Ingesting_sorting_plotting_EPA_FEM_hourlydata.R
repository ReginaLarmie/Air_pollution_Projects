# =========================================================
# Title:  Ingesting_sorting_EPA_FEM_hourlydata.R  
# Author: ASC   
# Date:   3/23/2026  
# Purpose/Desription: Ingesting EPA PM2.5 FEM hourly. Plotting monthly time
# series in order to pick individual weeks for plotting.
#
#
# Inputs:
#   - data/raw/EPA_FEM_hourly_PM25.csv
#
# Outputs:
#   - data/processed/...
#   - figures/...
#   - tables/...
#
# Upstream / Downstream Workflow:
#   Run after: not applic   
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

input_file  <- "Albert code and output/data/raw/EPA_FEM_hourly_PM25.csv"
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

# ---- Read and organize data ----
EPA_FEM_hourlydata <- read_csv(
  input_file,
  col_select = c(state_code, county_code, site_number, latitude, longitude, 
                 date_local, time_local, sample_measurement)
  )

# rename PM2.5 data column
EPA_FEM_hourlydata <- EPA_FEM_hourlydata %>%
  rename(PM25 = sample_measurement)

# create aqs_site_id column and make it the first column; create datetime_local
# column and position it to the left of PM25
EPA_FEM_hourlydata <- EPA_FEM_hourlydata %>%
  mutate(
    state_code  = str_pad(as.character(state_code),  2, pad = "0"),
    county_code = str_pad(as.character(county_code), 3, pad = "0"),
    site_number = str_pad(as.character(site_number), 4, pad = "0"),
    aqs_site_id = str_c(state_code, county_code, site_number, sep = "-"),
    datetime_local = ymd_hms(str_c(date_local, time_local, sep = " "))
  ) %>%
  relocate(aqs_site_id, .before = 1) %>%
  relocate(datetime_local, .before = PM25)


#------------------------------------------------------------
# 2. Restrict to the 14-month interval of interest
#------------------------------------------------------------
# This creates tibble named plot_data and restricts the datetime_local to
#   Nov 1, 2024 through Dec 31, 2025 (just in case there are any problems
#   with out-of-date-range data in the original dataset)
# The upper bound is set to Jan 1, 2026 so all of Dec 2025 is included.
# month_start column is created for easier grouping and plotting later,
# and it is basically in datetime format, but rounded down to the beginning
# of each month
#------------------------------------------------------------

plot_data <- EPA_FEM_hourlydata %>%
  filter(
    datetime_local >= ymd_hms("2024-11-01 00:00:00"),
    datetime_local <  ymd_hms("2026-01-01 00:00:00")
  ) %>%
  mutate(
    month_start = as.Date(floor_date(datetime_local, unit = "month"))
  )

#------------------------------------------------------------
# 3. Create an ordered vector of the 14 months
#------------------------------------------------------------
# This ensures that:
# - every site uses the same month ordering
# - missing months still appear as empty panels if desired
#------------------------------------------------------------

month_sequence <- seq(
  from = as.Date("2024-11-01"),
  to   = as.Date("2025-12-01"),
  by   = "1 month"
)


#------------------------------------------------------------
# 4. Turn month_start into a factor with fixed ordered levels
#------------------------------------------------------------
# This forces facet panels to appear in calendar order rather than
# alphabetical or order-of-appearance order.
#------------------------------------------------------------

plot_data <- plot_data %>%
  mutate(
    month_facet = factor(month_start, levels = month_sequence)
  )

#------------------------------------------------------------
# 5. Define a plotting function for one site and one page
#------------------------------------------------------------
# Arguments:
#   data_site   = tibble for one EPA site only
#   site_id     = site identifier string
#   facet_months = which 7 months should appear on this page
#   page_num    = page number for labeling
#
# This function returns one ggplot object with 7 monthly panels.
#------------------------------------------------------------

make_site_page_plot <- function(data_site, site_id, facet_months, page_num) {
  
  # Keep only the months assigned to this page
  page_data <- data_site %>%
    filter(month_start %in% facet_months)
  
  # Make a small lookup table so empty months still get a panel
  # if a site has no data in one of the requested months
  page_data <- page_data %>%
    mutate(
      month_facet = factor(month_start, levels = facet_months)
    )
  
  ggplot(page_data, aes(x = datetime_local, y = PM25)) +
    geom_line(linewidth = 0.25) +
    geom_point(size = 0.25, alpha = 0.5) +
    
    # Add the facet layout: 7 monthly panels on one page
    # ncol = 1 gives a vertical stack of 7 plots
    facet_wrap(
      ~ month_facet,
      ncol = 1,
      scales = "free_x",
      drop = FALSE
    ) +
    
    # Make x-axis labels readable within each monthly panel
    scale_x_datetime(
      date_breaks = "7 days",
      date_labels = "%b %d",
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    
    # Add a little vertical padding
    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.08))
    ) +
    
    # Titles and axis labels
    labs(
      title = paste("Hourly PM2.5 by Month"),
      subtitle = paste("AQS Site", site_id, "| Page", page_num, "of 2"),
      x = "Date",
      y = expression(PM[2.5]~"(" * mu * "g/m"^3 * ")")
    ) +
    
    # Use a clean black-and-white theme for PDF export
    theme_bw() +
    
    theme(
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10),
      strip.text = element_text(size = 9, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      axis.title.x = element_text(size = 9),
      axis.title.y = element_text(size = 9),
      panel.grid.minor = element_blank(),
      panel.spacing.y = unit(0.6, "lines")
    )
}

#------------------------------------------------------------
# 6. Split the 14 months into two groups of 7
#------------------------------------------------------------
# Page 1: Nov 2024 through May 2025
# Page 2: Jun 2025 through Dec 2025
#------------------------------------------------------------

months_page_1 <- month_sequence[1:7]
months_page_2 <- month_sequence[8:14]

#------------------------------------------------------------
# 7. Split the full data set into one tibble per site
#------------------------------------------------------------

site_list <- split(plot_data, plot_data$aqs_site_id)

#------------------------------------------------------------
# 8. Loop over sites and write one 2-page PDF per site
#------------------------------------------------------------
# Each PDF will be standard US letter:
#   width = 8.5 inches
#   height = 11 inches
#------------------------------------------------------------

for (site_id in names(site_list)) {
  
  data_site <- site_list[[site_id]]
  
  # Build the two page plots
  p1 <- make_site_page_plot(
    data_site   = data_site,
    site_id     = site_id,
    facet_months = months_page_1,
    page_num    = 1
  )
  
  p2 <- make_site_page_plot(
    data_site   = data_site,
    site_id     = site_id,
    facet_months = months_page_2,
    page_num    = 2
  )
  
  # Open a PDF device for this site
  pdf(
    file   = paste0("EPA_PM25_", site_id, "_monthly_timeseries.pdf"),
    width  = 8.5,
    height = 11,
    onefile = TRUE
  )
  
  # Print page 1 and page 2 into the same PDF
  print(p1)
  print(p2)
  
  # Close the PDF device
  dev.off()
}
  
# Quick inspection
glimpse(input_file)
summary(input_file)




# ---- Reproducibility info ----
sessionInfo()

# Optional: save session info to file
# writeLines(capture.output(sessionInfo()),
#            here("output", "session_info.txt"))