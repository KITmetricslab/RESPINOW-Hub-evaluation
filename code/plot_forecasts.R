source("code/data_utils.R")
Sys.setlocale("LC_ALL", "C")

# FIGURE 4

custom_theme <- theme(
  plot.title = element_text(size = 11),
  strip.text = element_text(size = 10),
  legend.title = element_text(size = 9),
  legend.text = element_text(size = 8),
  axis.title = element_text(size = 10),
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8)
)

facet_labels <- c(
  "sari" = "SARI",
  "are" = "ARI",
  "influenza" = "Influenza",
  "rsv" = "RSV"
)

indicators <- c("sari", "are", "influenza", "rsv")
dates <- c("2024-10-17", "2024-12-05", "2025-02-06", "2025-03-27", "2025-05-11")
age_groups <- c("00+", "00-04", "15-34", "80+")

df_all <- cross_df(list(indicator = indicators, date_version = dates)) %>%
  pmap_dfr(function(indicator, date_version) {
    target <- paste0(SOURCE_DICT[[indicator]], "-", indicator, "-DE")
    
    cat(indicator, date_version, "\n")
    
    load_combined_series(indicator, as_of = date_version, drop_incomplete = FALSE, wide = FALSE) %>%
      #select(date, value = all_of(target)) %>%
      filter(date >= as.Date("2024-07-01")) %>%
      mutate(
        indicator = indicator,
        data_version = as.character(date_version)
      )
  }) %>% 
  mutate(age_group = str_replace(age_group, "DE", "00\\+"))

truth_sari <- df_all %>% 
  filter(indicator == "sari",
         age_group %in% age_groups)




#### MODELS x AGE GROUPS


df <- load_submissions(include_target = FALSE, include_median = FALSE)

df_wide <- df  %>%
  filter(
    # location == !!location,
    # age_group == !!age_group,
    type == "quantile",
    quantile %in% QUANTILES,
    forecast_date %in% as.Date(dates),
    age_group %in% age_groups
  ) %>%
  pivot_wider(
    names_from = quantile,
    values_from = value,
    names_prefix = "quantile_"
  )

df_sari <- df_wide %>% 
  filter(disease == "sari")

df_nowcast <- df_sari %>% 
  filter(model == "KIT-simple_nowcast") %>% 
  select(-model)

df_forecast <- df_sari %>% 
  filter(model %in% MODELS_FORECAST[["sari"]])


# Vertical lines
vline_data <- tibble(date = as.Date(dates[-length(dates)]))

# Manual alpha values
alphas <- c("50%" = 0.7, "95%" = 0.4)

ggplot(truth_sari) +
  facet_grid(rows = vars(age_group), cols = vars(model), scales = "free") +
  
  # Forecast date vertical lines
  geom_vline(
    data = vline_data,
    aes(xintercept = date, linetype = "Forecast date"),
    color = "black"
  ) +
  
  # Nowcast ribbons
  geom_ribbon(
    data = df_nowcast,
    aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975,
        group = forecast_date, alpha = "95%", fill = "Nowcast")
  ) +
  geom_ribbon(
    data = df_nowcast,
    aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75,
        group = forecast_date, alpha = "50%", fill = "Nowcast")
  ) +
  geom_line(
    data = df_nowcast,
    aes(x = target_end_date, y = quantile_0.5, group = forecast_date),
    color = "#009ACD", linetype = "solid"
  ) +
  
  # Forecast ribbons
  geom_ribbon(
    data = df_forecast,
    aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975,
        group = forecast_date, alpha = "95%", fill = "Forecast")
  ) +
  geom_ribbon(
    data = df_forecast,
    aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75,
        group = forecast_date, alpha = "50%", fill = "Forecast")
  ) +
  geom_line(
    data = df_forecast,
    aes(x = target_end_date, y = quantile_0.5, group = forecast_date),
    color = "seagreen", linetype = "solid"
  ) +
  
  # Actual data lines
  geom_line(aes(x = date, y = value, group = data_version, color = "as of forecast date"), size = 1) +
  
  # Add special line for final version 
  geom_line(
    data = truth_sari %>% filter(data_version == as.Date("2025-05-11")),
    aes(x = date, y = value, color = "final"),
    size = 1
  ) +
  
  labs(x = NULL, y = "SARI hospitalizations", linetype = NULL) +
  
  scale_color_manual(
    name = "Data version",
    values = c("as of forecast date" = "#D55E00", "final" = "black"),
    guide = guide_legend(order = 2, nrow = 2, direction = "vertical")
  ) +
  scale_fill_manual(
    name = " ",
    values = c("Forecast" = "seagreen", "Nowcast" = "#009ACD"),
    guide = guide_legend(order = 3, nrow = 2)
  ) +
  scale_linetype_manual(
    name = " ",
    values = c("Forecast date" = "dotted"),
    guide = guide_legend(order = 1)
  ) +
  scale_alpha_manual(
    name = "Forecasts with \nprediction intervals:",
    values = alphas,
    guide = "none"
  ) +
  # scale_x_date(
  #   breaks = as.Date(c("2024-01-01", "2024-08-01")),
  #   date_minor_breaks = "1 month"
  # ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.spacing = unit(20, "pt"),
    legend.key = element_blank(),
    legend.key.size = unit(5, "pt"),
    strip.text = element_text(size = 8, margin = margin(2, 2, 2, 2)),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 7),
    axis.ticks = element_line(color = "black"), # linewidth = 0.25),
    #panel.grid.major = element_line(linewidth = 0.15),
    #panel.grid.minor = element_line(linewidth = 0.1)
  )



### ENSEMBLE x INDICATOR


# =========================
# SETTINGS
# =========================
model      <- "KIT-EnsembleComplete"
age_group  <- "00+"
indicators <- c("sari", "are")

facet_labels <- c(
  "sari"      = "SARI",
  "are"       = "ARI",
  "influenza" = "Influenza",
  "rsv"       = "RSV"
)

custom_theme <- theme(
  plot.title   = element_text(size = 11),
  strip.text   = element_text(size = 10),
  legend.title = element_text(size = 9),
  legend.text  = element_text(size = 8),
  axis.title   = element_text(size = 10),
  axis.text.x  = element_text(size = 8),
  axis.text.y  = element_text(size = 8)
)

dates0 <- as.Date(c("2024-10-17", "2024-12-05", "2025-02-06", "2025-03-27", "2025-05-11"))
dates1 <- dates0 + 7

# =========================
# TRUTH (load both windows)
# =========================
truth_spec <- bind_rows(
  crossing(indicator = indicators, date_version = as.character(dates0)) %>%
    mutate(window = "as-is"),
  crossing(indicator = indicators, date_version = as.character(dates1)) %>%
    mutate(window = "shifted +1w")
)

truth_all <- pmap_dfr(truth_spec, function(indicator, date_version, window) {
  load_combined_series(indicator, as_of = date_version, drop_incomplete = FALSE, wide = FALSE) %>%
    filter(date >= as.Date("2024-07-01"),
           location == "DE") %>%
    mutate(indicator = indicator,
           data_version = as.character(date_version),
           window = window)
}) %>%
  # normalize and then keep only 00+
  mutate(age_group = str_replace(age_group, "DE", "00\\+")) %>%
  filter(indicator %in% indicators, age_group == !!age_group)

truth_final <- truth_all %>%
  mutate(dv = as.Date(data_version)) %>%
  group_by(indicator, window) %>%
  filter(dv == max(dv)) %>%
  ungroup() %>%
  select(-dv)

# =========================
# FORECASTS / NOWCASTS
# =========================
df <- load_submissions(include_target = FALSE, include_median = FALSE)

make_wide <- function(dates_vec, window_label) {
  df %>%
    filter(
      location == "DE",
      type == "quantile",
      quantile %in% QUANTILES,
      forecast_date %in% as.Date(dates_vec),
      age_group == !!age_group,
      disease %in% indicators
    ) %>%
    pivot_wider(
      names_from   = quantile,
      values_from  = value,
      names_prefix = "quantile_"
    ) %>%
    transmute(
      indicator = disease,   # align with truth facets
      window    = window_label,
      forecast_date,
      target_end_date,
      model,
      across(starts_with("quantile_"), identity)
    )
}

pred_all <- bind_rows(
  make_wide(dates0, "as-is"),
  make_wide(dates1, "shifted +1w")
)

nowcast <- pred_all %>% filter(model == "KIT-simple_nowcast") %>% select(-model)
forecast <- pred_all %>% filter(model == !!model)

# vertical lines (per window)
vlines <- bind_rows(
  tibble(date = dates0[-length(dates0)], window = "as-is"),
  tibble(date = dates1[-length(dates1)], window = "shifted +1w")
)

alphas <- c("50%" = 0.7, "95%" = 0.4)

# =========================
# PLOT
# =========================
ggplot(truth_all) +
  facet_grid(
    rows = vars(indicator),
    cols = vars(window),
    scales = "free_y",
    labeller = labeller(indicator = facet_labels)
  ) +
  
  geom_vline(
    data = vlines,
    aes(xintercept = date, linetype = "Forecast date"),
    color = "black",
    linewidth=0.5
  ) +
  
  # nowcast
  geom_ribbon(
    data = nowcast,
    aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975,
        group = forecast_date, alpha = "95%", fill = "Nowcast")
  ) +
  geom_ribbon(
    data = nowcast,
    aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75,
        group = forecast_date, alpha = "50%", fill = "Nowcast")
  ) +
  geom_line(
    data = nowcast,
    aes(x = target_end_date, y = quantile_0.5, group = forecast_date),
    color = "#009ACD"
  ) +
  
  # forecast (only KIT-MeanEnsemble)
  geom_ribbon(
    data = forecast,
    aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975,
        group = forecast_date, alpha = "95%", fill = "Forecast")
  ) +
  geom_ribbon(
    data = forecast,
    aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75,
        group = forecast_date, alpha = "50%", fill = "Forecast")
  ) +
  geom_line(
    data = forecast,
    aes(x = target_end_date, y = quantile_0.5, group = forecast_date),
    color = "seagreen"
  ) +
  
  # observed as-of
  geom_line(
    aes(x = date, y = value, group = data_version, color = "as of forecast date"),
    linewidth = 0.4
  ) +
  
  # observed final (per window)
  geom_line(
    data = truth_final,
    aes(x = date, y = value, color = "final"),
    linewidth = 0.4
  ) +
  
  scale_color_manual(
    name = "Data version",
    values = c("as of forecast date" = "#D55E00", "final" = "black")
  ) +
  scale_fill_manual(
    name = " ",
    values = c("Forecast" = "seagreen", "Nowcast" = "#009ACD")
  ) +
  scale_linetype_manual(
    name = " ",
    values = c("Forecast date" = "dotted")
  ) +
  scale_alpha_manual(values = alphas, guide = "none") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = "Incidence", linetype = NULL) +
  theme_bw() +
  custom_theme +
  theme(legend.position = "right",
        strip.background.x = element_blank(),
        strip.text.x = element_blank())

ggsave(
  "figures/ensemble_forecasts.pdf",
  width = 190.5,
  height = 110,
  unit = "mm",
  device = "pdf"
)
