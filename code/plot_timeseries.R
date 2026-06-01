# In this file, descriptive plots of the time series are generated.

# load utility functions
source("code/data_utils.R")

# FIGURE 1

# load time series:
# t1 <- load_combined_series('sari')
# t2 <- load_combined_series('are')
# t3 <- load_combined_series('influenza')
# t4 <- load_combined_series('rsv')

# names of indicators:
indicators <- c("sari", "are", "influenza", "rsv")

# load time series for all indicators:
ts <- indicators %>%
  set_names() %>%
  map_dfr(function(indicator) {
    #target <- paste0(SOURCE_DICT[[indicator]], "-", indicator, "-DE")
    
    load_combined_series(indicator) %>%
      #select(date, value = all_of(target)) %>%
      mutate(indicator = indicator) %>% 
      filter(age_group == "DE")
  })

# Compute common date range across all indicators
date_range_common <- ts %>%
  group_by(indicator) %>%
  summarise(start = min(date), end = max(date), .groups = "drop") %>%
  summarise(start = max(start), end = min(end))

# Filter to the common date range
ts <- ts %>%
  filter(date >= date_range_common$start, date <= '2025-08-31') #date <= date_range_common$end)

ts <- ts %>%
  mutate(
    indicator = factor(
      indicator,
      levels = c("are", "sari", "influenza", "rsv")
    )
  )
# add an indicator to grey out incomplete rsv values
ts$grey_out <- ts$date <= as.Date("2023-07-01") & ts$indicator == "rsv"


# Pivot to wide format (one column per indicator)
# ts_wide <- ts %>%
#   pivot_wider(names_from = indicator, values_from = value)

# define theme for plotting
custom_theme <- theme(
  plot.title = element_text(size = 11),
  strip.text = element_text(size = 10),
  legend.title = element_text(size = 9),
  legend.text = element_text(size = 8),
  axis.title = element_text(size = 10),
  axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5),
  axis.text.y = element_text(size = 8)
)

# define labels per time series / facet:
facet_labels <- c(
  "sari" = "SARI",
  "are" = "ARI",
  "influenza" = "Seasonal influenza (SurvStat)",
  "rsv" = "RSV (SurvStat)"
)

# define dates to highlight as the evaluation period
highlight_areas <- tibble(
  xmin = as.Date(c("2024-10-17")),
  xmax = as.Date(c("2025-03-27")),
  period = c("Evaluation period")
)

# do the plotting
plot <- ggplot() +
  
  # Highlight periods using geom_rect
  geom_rect(
    data = highlight_areas,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period),
    alpha = 0.2
  ) +
  
  # Time series line
  geom_line(
    data = ts,
    aes(x = date, y = value, colour = grey_out),
    size = 0.6
  ) +
  scale_color_manual(values = c("TRUE" = "grey", "FALSE" = "black"), guide = "none") +
  # One facet per target
  facet_wrap(~indicator, scales = "free", labeller = as_labeller(facet_labels)) +
  expand_limits(y = 0) + 
  # Axis labels and legend title
  labs(
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  
  # Define manual colors for shaded periods
  scale_fill_manual(
    values = c("Evaluation period" = "blue"),
    limits = c("Evaluation period")
  ) +
  
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  
  # Additional theme settings
  theme_bw() +
  custom_theme +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 7, angle = 0),
    axis.text.y = element_text(size = 7),
    legend.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    legend.key = element_blank(),
    legend.key.size = unit(5, "pt"),
    panel.grid.minor.x = element_line(size = 0.25, colour = "lightgrey"),
    panel.grid.major.x = element_line(size = 0.25, colour = "lightgrey"),
    panel.grid.minor.y = element_line(size = 0.25, colour = "lightgrey"),
    panel.grid.major.y = element_line(size = 0.25, colour = "lightgrey")
  ) +
  
  coord_cartesian(expand = TRUE)

plot

# save:
ggsave("figures/timeseries.pdf", width = 190.5, height = 110, unit = "mm", device = "pdf")


####################################################
### a plot on the peak timing (not used in the manuscript)

ts_sari <- ts %>% filter(indicator == "sari")
ts_sari$season <- NA

ts_sari$season[ts_sari$week >= 40] <- paste0(ts_sari$year[ts_sari$week >= 40], "/", ts_sari$year[ts_sari$week >= 40] + 1)
ts_sari$season[ts_sari$week < 40] <- paste0(ts_sari$year[ts_sari$week < 40] - 1, "/", ts_sari$year[ts_sari$week < 40])

ts_sari$season_week <- NA
ts_sari$season_week[ts_sari$week >= 40] <- ts_sari$week[ts_sari$week >= 40] - 40
ts_sari$season_week[ts_sari$week < 40] <- ts_sari$week[ts_sari$week < 40] - 40 + 52
ts

ts_sari_max <- ts_sari %>% group_by(season) %>% summarise(max = max(value), peak_week = season_week[which.max(value)])

ts_sari_last <- ts_sari %>% filter(season == "2024/2025")

ggplot() +
  geom_line(data = ts_sari, aes(x = week, y = value, group = year))

ggplot() +
  geom_line(data = ts_sari, aes(x = season_week, y = value, group = season, colour = season)) +
  geom_point(data = ts_sari_max, aes(x = peak_week, y = max))



####################################################
### PLot showing data revisions

### Overview plot in old style (not used in manuscript):

# five dates to show:
dates <- c("2023-09-03", "2023-10-01", "2023-11-05", "2023-12-03", "2024-01-07")

# get all necessary data into one data.frame:
df_all <- map_dfr(dates, function(d) {
  cat(d, "\n")
  
  df_temp <- load_combined_series('sari', as_of=d, drop_incomplete = FALSE) %>%
    # select("date", "icosari-sari-DE") %>%
    filter(date >= as.Date("2023-07-01"),
           age_group == "DE") %>%
    mutate(data_version = d)
  
  return(df_temp)
})

# colors for different data versions:
version_colors <- c(
  "2023-09-03" = "#1f77b4",  # Blue
  "2023-10-01" = "#ff7f0e",  # Orange
  "2023-11-05" = "#2ca02c",  # Green
  "2023-12-03" = "#d62728",  # Red
  "2024-01-07" = "#000000"   # Black
)

df_all <- df_all %>%
  mutate(data_version = as.character(data_version))

# Build plot
plot <- ggplot(df_all, aes(x = date, y = value, color = data_version)) +
  geom_line(size = 1) +
  labs(
    x = NULL,
    y = "SARI incidence",
    color = "Data as of"
  ) +
  scale_color_manual(
    values = version_colors,
    limits = names(version_colors)
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.key = element_blank()
  )

plot

##########################################
### Revisions for all indicators - FIGURE 2

# vector of dates for which to show revisions:
dates <- format(seq(as.Date("2023-11-22"), as.Date("2025-04-24"), by = "1 week"), "%Y-%m-%d")
# remove Christmas:
# dates <- dates[!dates %in% c("2023-12-27", "2024-01-03", "2024-12-25", "2025-01-01")]

# the three data sources to show:
data_sources <- c("sari", "are", "influenza")
# axis labels
ylabs <- c("sari" = "SARI incidence",
           "are" = "ARI incidence",
           "rsv" = "SurvStat RSV incidence",
           "influenza" = "SurvStat influenza incidence")

# create plot for each data source
for(ds in data_sources){
  cat("Starting ", ds, "...\n")
  df_all <- map_dfr(dates, function(d) {
    cat(d, "\n")
    
    # load necessary data and organize a little
    df_temp <- load_combined_series(ds, as_of=d, drop_incomplete = FALSE) %>%  # CHANGE INDICATOR HERE SARI/ARE/RSV/INFLUENZA
      filter(date >= min(as.Date(dates)) - 35,
             age_group == "DE") %>%
      mutate(data_version = d)
    
    return(df_temp)
  })
  
  df_all <- df_all %>%
    mutate(data_version = as.character(data_version))
  
  last_version <- max(df_all$data_version)
  
  df_all <- df_all %>%
    mutate(
      color_group = ifelse(data_version == last_version, "Final data", "Initial report")
    )
  
  # build plot
  plot <- ggplot(df_all, aes(x = date, y = value, group = data_version, color = color_group)) +
    geom_line() +
    scale_x_date(
      expand = c(0, 0),
      limits = c(as.Date(dates[1]) - 21, as.Date(tail(dates, 1)) + 7),
      minor_breaks = seq(as.Date(dates[1]) - 21, as.Date(tail(dates, 1)) + 7, by = "4 weeks"),
      breaks = seq(as.Date(dates[1]) - 21, as.Date(tail(dates, 1)) + 7, by = "12 weeks"),
      date_labels = "%Y-%m-%d"
    ) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      x = NULL,
      y = ylabs[ds],
      color = NULL  # Legend title
    ) +
    scale_color_manual(
      values = c("Initial report" = "#D55E00", "Final data" = "black") # "springgreen4"
    ) +
    theme_bw() +
    theme(
      legend.position = c(0.5, 0.98),
      legend.justification = c("right", "top"),
      legend.background = element_rect(fill = alpha("white", 0.8)),
      legend.key = element_blank()
    )
  
  plot
  
  # store:
  file_name <- paste0("figures/revisions_", ds, ".pdf")
  ggsave(file_name, width = 140, height = 110, unit = "mm", device = "pdf")
  
}