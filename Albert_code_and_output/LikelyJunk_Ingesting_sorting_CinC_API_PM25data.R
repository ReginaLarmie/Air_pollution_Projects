# =========================
# CinC monthly PM2.5 plots
# =========================

library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)

#--------------------------------------------------
# 1. Start from the object already in your Environment
#--------------------------------------------------

cinc_data <- CinC_final_table_v2
output_dir <- here("Albert code and output", "output", "monthly_CinC_plots")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

#--------------------------------------------------
# 2. Clean up column names slightly for easier coding
#    (keeps structure essentially the same)
#--------------------------------------------------

cinc_data <- cinc_data %>%
  rename(
    sensor_id = `Sensor ID`,
    site_id   = `Site ID`,
    lat       = Lat,
    long      = Long,
    date      = Date,
    time      = Time,
    pm25      = `PM2.5`
  )

#--------------------------------------------------
# 3. Parse date/time and create a datetime column
#--------------------------------------------------
# This assumes:
#   date looks like "2024-11-01" or similar
#   time looks like "13:30:00" or "13:30"


cinc_data <- cinc_data %>%
  mutate(
    date = as.Date(date),
    datetime = ymd_hms(paste(date, time), quiet = TRUE)
  )

# If some rows did not parse because time is HH:MM rather than HH:MM:SS,
# fill those in with ymd_hm:
cinc_data <- cinc_data %>%
  mutate(
    datetime = if_else(
      is.na(datetime),
      ymd_hm(paste(date, time), quiet = TRUE),
      datetime
    )
  )

#--------------------------------------------------
# 4. Filter to the two CinC sites of interest
#--------------------------------------------------

sites_to_plot <- c("9.1_17.2", "7.1_4.1")

cinc_subset <- cinc_data %>%
  filter(site_id %in% sites_to_plot) %>%
  filter(!is.na(datetime), !is.na(pm25)) %>%
  arrange(site_id, datetime)

#--------------------------------------------------
# 5. Add monthly grouping variables
#--------------------------------------------------

cinc_subset <- cinc_subset %>%
  mutate(
    month_start = floor_date(datetime, unit = "month"),
    month_label = format(month_start, "%b %Y"),
    day_in_month = day(datetime)
  )

#--------------------------------------------------
# 6. Function to make one site’s monthly panels
#--------------------------------------------------
# This gives one panel per month, with x-axis spanning the month.
# Months with no data are simply absent.

plot_monthly_site_timeseries <- function(data, site_value,
                                         y_limits = NULL,
                                         output_dir) {
  
  site_df <- data %>%
    filter(site_id == site_value) %>%
    arrange(datetime)
  
  if (nrow(site_df) == 0) {
    message("No data found for site ", site_value)
    return(NULL)
  }
  
  # Make month label an ordered factor so facets stay chronological
  # Define full monthly sequence for entire dataset range
  full_month_seq <- seq(
    floor_date(min(data$datetime, na.rm = TRUE), "month"),
    floor_date(max(data$datetime, na.rm = TRUE), "month"),
    by = "1 month"
  )
  
  month_levels <- format(full_month_seq, "%b %Y")
  
  site_df <- site_df %>%
    mutate(month_label = factor(month_label, levels = month_levels))
  
  # Create a dataframe of months with NO data for this site
  empty_months <- setdiff(month_levels, as.character(unique(site_df$month_label)))
  
  if (length(empty_months) > 0) {
    empty_month_starts <- full_month_seq[format(full_month_seq, "%b %Y") %in% empty_months]
    
    empty_df <- data.frame(
      month_label = factor(format(empty_month_starts, "%b %Y"), levels = month_levels),
      x = empty_month_starts + days(15),
      y = max(site_df$pm25, na.rm = TRUE) * 0.5
    )
  } else {
    empty_df <- data.frame(
      month_label = factor(character(0), levels = month_levels),
      x = as.POSIXct(character(0)),
      y = numeric(0)
    )
  }
  
  p <- ggplot(site_df, aes(x = datetime, y = pm25)) +
    geom_line(linewidth = 0.3) +
    geom_point(size = 0.25) +
    geom_text(
      data = empty_df,
      aes(x = x, y = y, label = "No data for this month"),
      inherit.aes = FALSE,
      hjust = 0.5,
      vjust = 0.5,
      size = 3.5
    ) +
    facet_wrap(~ month_label, ncol = 3, scales = "fixed", drop = FALSE) +
    labs(
      title = paste("CinC PM2.5 Monthly Time Series:", site_value),
      x = NULL,
      y = expression(PM[2.5]~(mu*g/m^3))
    ) +
    scale_x_datetime(
      date_labels = "%d",
      date_breaks = "7 days",
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.spacing.x = unit(0.8, "lines"),
      panel.spacing.y = unit(1.0, "lines")
    )
  
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  
  print(p)
  
  # Save to PDF
  file_out <- file.path(
    output_dir,
    paste0("CinC_PM25_monthly_timeseries_", str_replace_all(site_value, "[^A-Za-z0-9]+", "_"), ".pdf")
  )
  
  ggsave(
    filename = file_out,
    plot = p,
    width = 14,
    height = 10,
    units = "in"
  )
  
  return(p)
}

#--------------------------------------------------
# 7. Make the plots
#--------------------------------------------------

p1 <- plot_monthly_site_timeseries(cinc_subset, "9.1_17.2", output_dir = output_dir)
p2 <- plot_monthly_site_timeseries(cinc_subset, "7.1_4.1", output_dir = output_dir)