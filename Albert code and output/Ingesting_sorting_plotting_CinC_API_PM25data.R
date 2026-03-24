# =========================================================
# Title:  Ingesting_sorting_CinC_API_PM25data.R
# Author: ASC
# Date:   3/24/2026
# Purpose/Description:
#   Plot CinC PM2.5 data as monthly time series using the same
#   2-page layout/style as the EPA FEM script:
#   - 14 months total
#   - 7 month panels per page
#   - portrait PDF
#   - one full-width monthly panel per row
#   - empty months retained
# =========================================================

# ---- Libraries ----
library(tidyverse)
library(lubridate)

# ---- Global options ----
options(stringsAsFactors = FALSE)
options(dplyr.summarise.inform = FALSE)
theme_set(theme_minimal())

# ---- User inputs ----
output_dir <- "C:/R_Projects/Air_pollution_Projects/Albert code and output/output/monthly_CinC_plots"

sites_to_plot <- c("9.1_17.2", "7.1_4.1")

# Start with object already in Environment:
cinc_data <- CinC_final_table_v2

# ---- Rename columns ----
cinc_data <- cinc_data %>%
  rename(
    sensor_id = `Sensor ID`,
    site_id   = `Site ID`,
    lat       = Lat,
    long      = Long,
    date      = Date,
    time      = Time,
    PM25      = `PM2.5`
  )

# ---- Parse datetime ----
cinc_data <- cinc_data %>%
  mutate(
    date = as.Date(date),
    datetime_local = ymd_hms(paste(date, time), quiet = TRUE)
  )

# If time happens to be HH:MM instead of HH:MM:SS in some rows
cinc_data <- cinc_data %>%
  mutate(
    datetime_local = if_else(
      is.na(datetime_local),
      ymd_hm(paste(date, time), quiet = TRUE),
      datetime_local
    )
  )

# ---- Restrict to date interval of interest ----
plot_data <- cinc_data %>%
  filter(
    site_id %in% sites_to_plot,
    datetime_local >= ymd_hms("2024-11-01 00:00:00"),
    datetime_local <  ymd_hms("2026-01-01 00:00:00")
  ) %>%
  mutate(
    month_start = as.Date(floor_date(datetime_local, unit = "month"))
  ) %>%
  arrange(site_id, datetime_local)

# ---- Create ordered 14-month sequence ----
month_sequence <- seq(
  from = as.Date("2024-11-01"),
  to   = as.Date("2025-12-01"),
  by   = "1 month"
)

# ---- Turn month_start into fixed ordered factor ----
plot_data <- plot_data %>%
  mutate(
    month_facet = factor(month_start, levels = month_sequence)
  )

# ---- Save plot_data file as an rds file for future week-long plotting
saveRDS(
  plot_data,
  file.path("Albert code and output", "data", "processed", "CinC_API_hourlydata.rds")
)

# ---- Define plotting function for one site and one page ----
make_site_page_plot <- function(data_site, site_id, facet_months, page_num) {
  
  page_data <- data_site %>%
    filter(month_start %in% facet_months) %>%
    mutate(
      month_facet = factor(month_start, levels = facet_months)
    )
  
  # empty-month label locations
  all_months_df <- tibble(
    month_start = facet_months,
    month_facet = factor(facet_months, levels = facet_months)
  )
  
  months_with_data <- unique(page_data$month_start)
  
  empty_months_df <- all_months_df %>%
    filter(!(month_start %in% months_with_data)) %>%
    mutate(
      label_x = as.POSIXct(month_start + 14),
      label_y = 5
    )
  
  ggplot(page_data, aes(x = datetime_local, y = PM25)) +
    geom_line(linewidth = 0.25) +
    geom_point(size = 0.25, alpha = 0.5) +
    
    geom_text(
      data = empty_months_df,
      aes(x = label_x, y = label_y, label = "No data for this month"),
      inherit.aes = FALSE,
      size = 3,
      color = "gray35"
    ) +
    
    facet_wrap(
      ~ month_facet,
      ncol = 1,
      scales = "free_x",
      drop = FALSE
    ) +
    
    scale_x_datetime(
      date_breaks = "7 days",
      date_labels = "%b %d",
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    
    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.08))
    ) +
    
    labs(
      title = "CinC PM2.5 by Month",
      subtitle = paste("CinC Site", site_id, "| Page", page_num, "of 2"),
      x = "Date",
      y = expression(PM[2.5]~"(" * mu * "g/m"^3 * ")")
    ) +
    
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

# ---- Split 14 months into two groups of 7 ----
months_page_1 <- month_sequence[1:7]
months_page_2 <- month_sequence[8:14]

# ---- Split into one tibble per site ----
site_list <- split(plot_data, plot_data$site_id)

# ---- Write one 2-page PDF per site ----
for (site_id in names(site_list)) {
  
  data_site <- site_list[[site_id]]
  
  p1 <- make_site_page_plot(
    data_site    = data_site,
    site_id      = site_id,
    facet_months = months_page_1,
    page_num     = 1
  )
  
  p2 <- make_site_page_plot(
    data_site    = data_site,
    site_id      = site_id,
    facet_months = months_page_2,
    page_num     = 2
  )
  
  pdf(
    file    = file.path(output_dir, paste0("CinC_PM25_", site_id, "_monthly_timeseries.pdf")),
    width   = 8.5,
    height  = 11,
    onefile = TRUE
  )
  
  print(p1)
  print(p2)
  
  dev.off()
}