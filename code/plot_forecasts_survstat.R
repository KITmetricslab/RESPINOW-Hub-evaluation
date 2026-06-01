# This file plots forecasts for SurvStat-influenza and RSV

source("code/data_utils.R")
Sys.setlocale("LC_ALL", "C")

# FIGURE 5

ind <- "rsv" # change to "rsv" for RSV

# handle themes:
custom_theme <- theme(
  plot.title   = element_text(size = 11),
  strip.text   = element_text(size = 10),
  legend.title = element_text(size = 9),
  legend.text  = element_text(size = 8),
  axis.title   = element_text(size = 10),
  axis.text.x  = element_text(size = 8),
  axis.text.y  = element_text(size = 8)
)

# labels:
facet_labels <- c(
  "influenza" = "Influenza",
  "rsv" = "RSV"
)

window_labels <- c(
  "window0" = "",
  "window1" = ""
)

# indicators, dates and age groups and model (this is HZI in both cases):
indicators <- c("influenza", "rsv")
dates <- c("2024-10-17", "2024-12-05", "2025-02-06", "2025-03-27", "2025-05-11")
# dates per facet:
dates0 <- as.Date(c("2024-10-24", "2024-11-21", "2025-01-09", "2025-01-23", "2025-03-27", "2025-05-29"))
dates1 <- as.Date(c("2024-10-31", "2024-11-28", "2025-01-16", "2025-01-30", "2025-05-29"))

age_group <- c("00+")
model      <- "HZI-ODEmodel"

# collect all truth data versions in one data.frame:
# df_all <- cross_df(list(indicator = indicators, date_version = dates)) %>%
#   pmap_dfr(function(indicator, date_version) {
#     target <- paste0(SOURCE_DICT[[indicator]], "-", indicator, "-DE")
#     
#     cat(indicator, date_version, "\n")
#     
#     load_combined_series(indicator, as_of = date_version, drop_incomplete = FALSE, wide = FALSE) %>%
#       #select(date, value = all_of(target)) %>%
#       filter(date >= as.Date("2024-07-01")) %>%
#       mutate(
#         indicator = indicator,
#         data_version = as.character(date_version)
#       )
#   }) %>% 
#   mutate(age_group = str_replace(age_group, "DE", "00\\+"))

# snapshots:
truth_spec <- bind_rows(
  crossing(indicator = indicators, date_version = as.character(dates0)) %>%
    mutate(window = "window0"),
  crossing(indicator = indicators, date_version = as.character(dates1)) %>%
    mutate(window = "window1")
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
# restrict to indicator:
truth_all_ind <- truth_all %>% filter(indicator == ind)

# final truth:
truth_final_ind <- read_csv(paste0("https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Hub/refs/heads/main/data/survstat/",
                                   ind, "/target-survstat-", 
                                   ind, 
                                   ".csv"))
truth_final_ind <- truth_final_ind %>% filter(location == "DE" & 
                                                age_group == "00+" & 
                                                date >= as.Date("2024-10-01") & 
                                                date <= as.Date("2025-06-30"))


# Get forecasts and nowcasts:
df <- load_submissions(include_target = FALSE, include_median = FALSE)

# helper function to switch to wide format:
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

# make wide for both windows:
pred_all <- bind_rows(
  make_wide(dates0, "window0"),
  make_wide(dates1, "window1")
)

# select nowcasts and forecasts:
nowcast <- pred_all %>% filter(model == "KIT-simple_nowcast") %>% select(-model)
nowcast_ind <- nowcast %>% filter(indicator == ind)

forecast <- pred_all %>% filter(model == !!model)
forecast_ind <- forecast %>% filter(indicator == ind)

# plotting:

# vertical lines (per window)
vlines <- bind_rows(
  tibble(date = dates0[-length(dates0)], window = "window0"),
  tibble(date = dates1[-length(dates1)], window = "window1")
)

alphas <- c("50%" = 0.7, "95%" = 0.4)

# labels, titles and limits:
ylabs <- c("sari" = "SARI incidence",
           "are" = "ARI incidence",
           "rsv" = "SurvStat RSV incidence",
           "influenza" = "SurvStat influenza incidence")
titles <- c("sari" = "SARI",
            "are" = "ARI",
            "rsv" = "SurvStat RSV",
            "influenza" = "SurvStat influenza")
ylims <- list(influenza = c(0, 60000),
              rsv = c(0, 10000))

# plot:
ggplot(truth_all_ind) +
  facet_grid(
    rows = vars(window),
    cols = vars(indicator),
    scales = "free_y",
    labeller = labeller(indicator = facet_labels, window = window_labels)
  ) +
  # scale_y_continuous(limits = c(0, NA)) +
  geom_vline(
    data = vlines,
    aes(xintercept = date, linetype = "Forecast date"),
    color = "black",
    linewidth=0.5
  ) +
  # forecast (only KIT-MeanEnsemble)
  geom_ribbon(
    data = forecast_ind,
    aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975,
        group = forecast_date, alpha = "95%", fill = "Forecast")
  ) +
  geom_ribbon(
    data = forecast_ind,
    aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75,
        group = forecast_date, alpha = "50%", fill = "Forecast")
  ) +
  geom_line(
    data = forecast_ind,
    aes(x = target_end_date, y = quantile_0.5, group = forecast_date),
    color = "seagreen"
  ) +
  
  # # observed as-of
  # geom_line(
  #   aes(x = date, y = value, group = data_version, color = "as of forecast date"),
  #   linewidth = 0.4
  # ) +
  
  # observed final (per window)
  geom_line(
    data = truth_final_ind,
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
  scale_y_continuous(labels = scales::comma, limits = ylims[[ind]]) +
  labs(x = NULL, y = ylabs[ind], linetype = NULL) +
  theme_bw() +
  custom_theme +
  theme(legend.position = "right",
        strip.background.x = element_blank(),
        strip.text.x = element_blank()) +
  ggtitle(titles[ind])

# write out:
ggsave(
  paste0("figures/ensemble_forecasts_", paste(ind, collapse = "_"),".pdf"),
  width = 140.5,
  height = 110,
  unit = "mm",
  device = "pdf"
)
