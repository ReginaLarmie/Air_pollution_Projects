# =========================================================
# Title: Plotting_CinC_API_PM25_1weekTimeSeries.R    
# Author: ASC   
# Date: 3/23/2026     
# Purpose:  Look for evidence of diurnal patterns in PM2.5 by plotting EPA
#           FEM hourly PM2.5 data for 1 week spans.
#
#
# Upstream / Downstream Workflow:
#   Run after: CinC data should have been saved in an rds file by running the    
#              the Ingesting_sorting_plotting_CinC_API_PM25data.R code.
#   Run before:  
#
# =========================================================

# ---- Libraries ----
library(tidyverse)   # data import, wrangling, plotting
library(lubridate)   # dates and times
library(here)        # reproducible file paths

#------------------------------------------------------------
# 1. User inputs
#------------------------------------------------------------
# Specify the EPA site ID and the Sunday that begins the week.
# Use the same dashed site format you created earlier.
#------------------------------------------------------------

site_to_plot <- "7.1_4.1"
week_start_date <- as.Date("2025-09-07")   # must be a Sunday

#------------------------------------------------------------
# 2. Define input and output paths
#------------------------------------------------------------

input_file <- here("Albert code and output", "data", "processed", "CinC_API_halfhourlydata.rds")
output_dir <- here("Albert code and output", "output", "weekly_CinC_plots")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

#------------------------------------------------------------
# 3. Read the processed tibble
#------------------------------------------------------------

plot_data <- readRDS(input_file) %>%
  mutate(
    datetime_local_endshifted = datetime_local + minutes(30)
  ) %>%
  select(-date, -time)


#------------------------------------------------------------
# 4. Optional safety checks
#------------------------------------------------------------
# Check that the requested site exists and that the start date is Sunday.
# In lubridate, with week_start = 7, Sunday is day 1.
#------------------------------------------------------------

if (!(site_to_plot %in% unique(plot_data$site_id))) {
  stop("Requested site_to_plot was not found in plot_data.")
}

if (wday(week_start_date, week_start = 7) != 1) {
  stop("week_start_date is not a Sunday.")
}

#------------------------------------------------------------
# 5. Define the weekly time window
#------------------------------------------------------------
# This creates a 7-day window:
#   Sunday 00:00:00 through next Sunday 00:00:00
#------------------------------------------------------------

week_start_dt <- as.POSIXct(week_start_date, tz = "America/New_York")
week_end_dt   <- week_start_dt + days(7)

#------------------------------------------------------------
# 6. Filter to the requested site and week
#------------------------------------------------------------

week_data <- plot_data %>%
  filter(
    site_id == site_to_plot,
    datetime_local >= week_start_dt,
    datetime_local <  week_end_dt
  ) %>%
  filter(!is.na(datetime_local), !is.na(PM25), is.finite(PM25))

#------------------------------------------------------------
# 7. Stop if no data were found
#------------------------------------------------------------

if (nrow(week_data) == 0) {
  stop("No data found for the requested site and week.")
}

#------------------------------------------------------------
# 8. Build the plot
#------------------------------------------------------------

p <- ggplot(week_data, aes(x = datetime_local, y = PM25)) +
  geom_line(linewidth = 0.35) +
  geom_point(size = 0.7, alpha = 0.7) +
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%a\n%b %d",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.08))
  ) +
  labs(
    title = paste("Hourly PM2.5 for Week Beginning", format(week_start_date, "%b %d, %Y")),
    subtitle = paste("CLEANinCLE Site", site_to_plot),
    x = "Date",
    y = expression(PM[2.5]~"(" * mu * "g/m"^3 * ")")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

#------------------------------------------------------------
# 9. Save the plot as a PNG
#------------------------------------------------------------

output_file <- file.path(
  output_dir,
  paste0(
    "CinC_PM25_weekly_",
    site_to_plot,
    "_",
    format(week_start_date, "%Y-%m-%d"),
    ".png"
  )
)

ggsave(
  filename = output_file,
  plot = p,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

#------------------------------------------------------------
# 10. Optional: print the plot in the Plots pane
#------------------------------------------------------------

print(p)



# ---- Reproducibility info ----
sessionInfo()

# Optional: save session info to file
# writeLines(capture.output(sessionInfo()),
#            here("output", "session_info.txt"))