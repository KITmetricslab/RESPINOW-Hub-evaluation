source("code/data_utils.R")

t1 <- load_combined_series('sari')
t2 <- load_combined_series('are')
t3 <- load_combined_series('influenza')
t4 <- load_combined_series('rsv')


indicators <- c("sari", "are", "influenza", "rsv")

ts <- indicators %>%
  set_names() %>%
  map_dfr(function(indicator) {
    target <- paste0(SOURCE_DICT[[indicator]], "-", indicator, "-DE")
    
    load_combined_series(indicator) %>%
      select(date, value = all_of(target)) %>%
      mutate(indicator = indicator)
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
