source("code/data_utils.R")

t1 <- load_combined_series('sari')
t2 <- load_combined_series('are')
t3 <- load_combined_series('influenza')
t4 <- load_combined_series('rsv')


indicators <- c("sari", "are", "influenza", "rsv")

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
  filter(date >= date_range_common$start, date <= date_range_common$end)

# Pivot to wide format (one column per indicator)
# ts_wide <- ts %>%
#   pivot_wider(names_from = indicator, values_from = value)

custom_theme <- theme(
  plot.title = element_text(size = 11),
  strip.text = element_text(size = 10),
  legend.title = element_text(size = 9),
  legend.text = element_text(size = 8),
  axis.title = element_text(size = 10),
  axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5),
  axis.text.y = element_text(size = 8)
)

facet_labels <- c(
  "sari" = "SARI",
  "are" = "ARE",
  "influenza" = "Influenza",
  "rsv" = "RSV"
)

highlight_areas <- tibble(
  xmin = as.Date(c("2023-11-16", "2024-10-17")),
  xmax = as.Date(c("2024-09-12", "2025-03-27")),
  period = c("Retrospective", "Prospective")
)

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
    aes(x = date, y = value),
    size = 0.6
  ) +
  
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
    values = c("Retrospective" = "green", "Prospective" = "blue"),
    limits = c("Retrospective", "Prospective")
  ) +
  
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  
  # Theme settings
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

ggsave("figures/timeseries.pdf", width = 190.5, height = 110, unit = "mm", device = "pdf")


### Revisions


dates <- c("2023-09-03", "2023-10-01", "2023-11-05", "2023-12-03", "2024-01-07")

df_all <- map_dfr(dates, function(d) {
  cat(d, "\n")
  
  df_temp <- load_combined_series('sari', as_of=d, drop_incomplete = FALSE) %>%
    # select("date", "icosari-sari-DE") %>%
    filter(date >= as.Date("2023-07-01"),
           age_group == "DE") %>%
    mutate(data_version = d)
  
  return(df_temp)
})

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
  # scale_x_date(
  #   breaks = as.Date(c("2023-08-01", "2023-12-01"))
  # ) +
  #scale_y_continuous(limits = c(0, NA)) +
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


### Revisions all

dates <- format(seq(as.Date("2023-01-01"), as.Date("2024-04-28"), by = "4 weeks"), "%Y-%m-%d")

df_all <- map_dfr(dates, function(d) {
  cat(d, "\n")
  
  df_temp <- load_combined_series('sari', as_of=d, drop_incomplete = FALSE) %>%
    # select("date", "icosari-sari-DE") %>%
    filter(date >= as.Date("2022-11-06"),
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


plot <- ggplot(df_all, aes(x = date, y = value, group = data_version, color = color_group)) +
  geom_line() +
  scale_x_date(
    expand = c(0, 0),
    limits = as.Date(c("2022-11-06", "2024-04-21")),
    minor_breaks = seq(as.Date("2022-11-06"), as.Date("2024-04-28"), by = "4 weeks"),
    breaks = seq(as.Date("2022-11-06"), as.Date("2024-04-28"), by = "12 weeks")[-1],
    date_labels = "%Y-%m-%d"
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    x = NULL,
    y = "SARI incidence",
    color = NULL  # Legend title
  ) +
  scale_color_manual(
    values = c("Initial report" = "deepskyblue3", "Final data" = "black") # "springgreen4"
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = alpha("white", 0.8)),
    legend.key = element_blank()
  )

plot

ggsave("figures/revisions.pdf", width = 190.5, height = 110, unit = "mm", device = "pdf")


### Multiple indicators


indicators <- c("sari", "are", "influenza", "rsv")
dates <- c("2024-09-03", "2024-10-01", "2024-11-05", "2024-12-03", "2025-01-07")

base_colors <- c(
  "#1f77b4",  # Blue
  "#ff7f0e",  # Orange
  "#2ca02c",  # Green
  "#d62728",  # Red
  "#000000"   # Black
)

version_colors <- setNames(base_colors, dates)

# Load all indicators across all data versions into a single tidy df
df_all <- cross_df(list(indicator = indicators, date_version = dates)) %>%
  pmap_dfr(function(indicator, date_version) {
    target <- paste0(SOURCE_DICT[[indicator]], "-", indicator, "-DE")
    
    cat(indicator, date_version, "\n")
    
    load_combined_series(indicator, as_of = date_version, drop_incomplete = FALSE) %>%
      select(date, value = all_of(target)) %>%
      filter(date >= as.Date("2024-07-01")) %>%
      mutate(
        indicator = indicator,
        data_version = as.character(date_version)
      )
  })


ggplot(df_all, aes(x = date, y = value, color = data_version)) +
  geom_line(size = 1) +
  facet_wrap(~ indicator, scales = "free_y", labeller = as_labeller(facet_labels)) +
  labs(
    x = NULL,
    y = "Incidence",
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

ggsave("figures/revisions.pdf", width = 190.5, height = 110, unit = "mm", device = "pdf")





are <- load_combined_series('are', as_of="2023-09-03", drop_incomplete = FALSE)

load_target_series('are', as_of="2023-09-03")

r1 <- load_rt('sari')
r2 <- load_rt('are')
target <- target_as_of(r2, date="2023-09-03")



dates <- format(seq(as.Date("2023-09-17"), as.Date("2024-08-28"), by = "4 weeks"), "%Y-%m-%d")

df_all <- map_dfr(dates, function(d) {
  cat(d, "\n")
  
  df_temp <- load_combined_series('sari', as_of=d, drop_incomplete = FALSE) %>%
    # select("date", "icosari-sari-DE") %>%
    filter(date >= as.Date("2022-11-06"),
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


plot <- ggplot(df_all, aes(x = date, y = value, group = data_version, color = color_group)) +
  geom_line() +
  scale_x_date(
    expand = c(0, 0),
    limits = as.Date(c("2023-09-15", "2024-08-21")),
    minor_breaks = seq(as.Date("2022-11-06"), as.Date("2024-08-28"), by = "4 weeks"),
    breaks = seq(as.Date("2022-11-06"), as.Date("2024-08-28"), by = "8 weeks")[-1],
    date_labels = "%Y-%m-%d"
  ) +
  scale_y_continuous(limits = c(400000, 2200000)) +
  labs(
    x = NULL,
    y = "ARI",
    color = NULL  # Legend title
  ) +
  scale_color_manual(
    values = c("Initial report" = "deepskyblue3", "Final data" = "black") # "springgreen4"
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = alpha("white", 0.8)),
    legend.key = element_blank()
  )

plot

ggsave("figures/revisions_are.png", width = 190.5, height = 110, unit = "mm", device = "png")


#### Individual frames

# Stelle sicher, dass data_version als Date vorliegt
df_all <- df_all %>%
  mutate(data_version = as.Date(data_version))

# Alle As-of-Daten sortiert
versions <- sort(unique(df_all$data_version))

# Ausgabe-Ordner
out_dir <- "figures/revisions_sari_frames"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Fixe Skalen (an deine bisherigen Limits angepasst)
x_limits <- as.Date(c("2023-09-15", "2024-08-21"))
x_minor  <- seq(as.Date("2022-11-06"), as.Date("2024-08-28"), by = "4 weeks")
x_major  <- seq(as.Date("2022-11-06"), as.Date("2024-08-28"), by = "8 weeks")[-1]
#y_limits <- c(400000, 2200000)
y_limits <- c(NA, NA)

# Schleife über Frames
walk(seq_along(versions), function(i) {
  current_ver <- versions[i]
  
  # Alle Versionen bis einschließlich current_ver
  df_until_now <- df_all %>%
    filter(data_version <= current_ver)
  
  # Aufteilen: ältere (blau) vs. aktuelle (schwarz)
  df_blue <- df_until_now %>% filter(data_version <  current_ver)
  df_black <- df_until_now %>% filter(data_version == current_ver)
  
  p <- ggplot() +
    # zuerst alle bisherigen (blau), damit die aktuelle (schwarz) oben liegt
    geom_line(
      data = df_blue,
      aes(x = date, y = value, group = data_version, color = "Initial report"),
      linewidth = 0.5, alpha = 0.9
    ) +
    geom_line(
      data = df_black,
      aes(x = date, y = value, group = data_version, color = "Final data"),
      linewidth = 0.8
    ) +
    scale_x_date(
      expand = c(0, 0),
      limits = x_limits,
      minor_breaks = x_minor,
      breaks = x_major,
      date_labels = "%Y-%m-%d"
    ) +
    scale_y_continuous(limits = y_limits) +
    labs(
      x = NULL,
      y = "SARI",
      color = NULL,
      title = paste0("As of: ", format(current_ver, "%Y-%m-%d"))
    ) +
    scale_color_manual(
      values = c("Initial report" = "deepskyblue3", "Final data" = "black")
    ) +
    theme_bw() +
    theme(
      legend.position = c(0.98, 0.98),
      legend.justification = c("right", "top"),
      legend.background = element_rect(fill = alpha("white", 0.8)),
      legend.key = element_blank(),
      plot.title = element_text(hjust = 1, face = "bold")
    )
  
  # Dateiname mit laufender Nummer und Datum
  file_name <- file.path(
    out_dir,
    sprintf("revisions_sari_%02d.png", i)
  )
  
  ggsave(file_name, plot = p, width = 190.5, height = 110, units = "mm", dpi = 300)
})
