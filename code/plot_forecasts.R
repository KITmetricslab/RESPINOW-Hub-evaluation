# This file plots forecasts for SARI and ARI

#####################################
# Global settings

source("code/data_utils.R")
Sys.setlocale("LC_ALL", "C")

# some plot settings:
custom_theme <- theme(
  plot.title = element_text(size = 11),
  strip.text = element_text(size = 10),
  legend.title = element_text(size = 9),
  legend.text = element_text(size = 8),
  axis.title = element_text(size = 10),
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8)
)
# Manual alpha values for transparent plotting:
alphas <- c("50%" = 0.7, "95%" = 0.3)

# handle names of data sources
indicators <- c("sari", "are")
data_sources <- c("sari" = "icosari", "are" = "agi")

# select which indicator to plot (change to "sari" for SARI plots)
ind <- "sari"
# switch this to create supplementary figures with different dates
main <- TRUE

# load submissions:
submissions <- load_submissions(include_target = FALSE, include_median = FALSE)

truth_final_ind <- read_csv(paste0("data/target-", 
                                   data_sources[ind], "-", 
                                   ind, ".csv"))
truth_final_ind <- truth_final_ind %>% filter(location == "DE" & 
                                                age_group == "00+" & 
                                                date >= as.Date("2024-07-01") & 
                                                date <= as.Date("2025-06-30"))

# christmas break period to highlight
christmas_break <- as.Date(c("2024-12-24", "2025-01-08"))


######################################
# Ensemble forecasts for age group 00+ (selected dates; FIG 4)

############
# preparation
# nowcasts to show with different models:
mapping_nowcasts <- c("KIT-EnsembleComplete" = "KIT-EnsembleNowcastComplete",
                      "KIT-hhh4" = "KIT-simple_nowcast",
                      "KIT-LightGBM" = "KIT-simple_nowcast",
                      "KIT-TSMixer" = "KIT-simple_nowcast",
                      "MPIDS-PS_embedding" = NA
)


# a helper function to get wide format, to be applied per window
# note: uses global variables inside, only for use in following line
make_wide <- function(dates_vec, window_label) {
  submissions %>%
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

# run through forecasting models and generate plots:
models_to_plot <- c("KIT-EnsembleComplete")
for(model_forecast in models_to_plot){
  
  model_nowcast <- mapping_nowcasts[model_forecast]
  age_group  <- "00+"
  
  if(main){
    # date vectors per panel:
    dates0 <- as.Date(c("2024-10-17", "2024-12-05", "2025-02-06", "2025-03-27", "2025-05-11"))
    dates0 <- seq(from = as.Date("2024-10-17"), by = 7*7, length.out = 5)
    dates1 <- dates0 + 14
    dates2 <- dates0 + 42
  }else{
    # alternative dates for supplementary figure:
    dates0 <- as.Date(c("2024-10-24", "2024-11-21", "2025-01-30", "2025-02-27"))
    dates1 <- as.Date(c("2024-11-07", "2024-12-12", "2025-02-13", "2025-03-20"))
    dates2 <- as.Date(c("2024-11-14", "2025-01-09", "2025-02-20"))
  }

  
  # alternative dates for supplementar panel
  
  # labels for panels - empty as they don't have meaningful names
  window_labels <- c(
    "window0" = "",
    "window1" = "",
    "window2" = ""
  )
  
  # truth (used in all windows)
  truth_spec <- bind_rows(
    crossing(indicator = indicators, date_version = as.character(dates0)) %>%
      mutate(window = "window0"),
    crossing(indicator = indicators, date_version = as.character(dates1)) %>%
      mutate(window = "window1"),
    crossing(indicator = indicators, date_version = as.character(dates2)) %>%
      mutate(window = "window2")
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
  
  # submissions
  
  # collect relevant data across windows:
  pred_all <- bind_rows(
    make_wide(dates0, "window0"),
    make_wide(dates1, "window1"),
    make_wide(dates2, "window2")
  )
  
  # split into nowcasts (blue), forecasts (green) and transition between (grey)
  # removing nowcast horizon -3 for better graphical display
  nowcast <- pred_all %>% filter(model == model_nowcast) %>% select(-model) %>% filter(difftime(forecast_date, target_end_date) < 20)
  forecast <- pred_all %>% filter(model == !!model_forecast)
  transition <- bind_rows(filter(nowcast, difftime(forecast_date, target_end_date) == 4),
                          filter(forecast, difftime(forecast_date, target_end_date) == - 3))
  
  ############
  # plot
  
  # vertical lines (per window)
  vlines <- bind_rows(
    tibble(date = dates0[-length(dates0)], window = "window0"),
    tibble(date = dates1[-length(dates1)], window = "window1"),
    tibble(date = dates2[-length(dates2)], window = "window2")
  )
  
  # preparing labelling:
  ylabs <- c("sari" = "SARI incidence",
             "are" = "ARI incidence")
  titles <- c("sari" = "SARI",
              "are" = "ARI")
  
  truth_all_ind <- truth_all %>% filter(indicator == ind)
  nowcast_ind <- nowcast %>% filter(indicator == ind)
  transition_ind <- transition %>% filter(indicator == ind)
  forecast_ind <- forecast %>% filter(indicator == ind)
  
  # create plot:
  # determine ylim:
  yl <- c(0, 1.02*max(c(forecast_ind$quantile_0.975,
                        truth_final_ind$value)))
  
  p <- ggplot(truth_all_ind) +
    facet_grid(
      rows = vars(window),
      cols = vars(indicator),
      scales = "free_y",
      labeller = labeller(window = window_labels)
    ) +
    annotate("rect", xmin = christmas_break[1], 
             xmax = christmas_break[2],
             ymin = yl[1], ymax = yl[2], colour = NA, 
             fill = "lightgrey", alpha = 0.5) +
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
    geom_point(
      data = forecast_ind,
      aes(x = target_end_date, y = quantile_0.5, group = forecast_date),
      color = "seagreen", pch = 21, fill = "white", size = 0.6
    )
  
  # add nowcasts only if a nowcast model is specified for that forecast model:
  if(!is.na(model_nowcast)){
    p <- p  + # nowcast
      geom_ribbon(
        data = nowcast_ind,
        aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975,
            group = forecast_date, alpha = "95%", fill = "Nowcast")
      ) +
      geom_ribbon(
        data = nowcast_ind,
        aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75,
            group = forecast_date, alpha = "50%", fill = "Nowcast")
      ) +
      geom_line(
        data = nowcast_ind,
        aes(x = target_end_date, y = quantile_0.5, group = forecast_date),
        color = "#009ACD"
      ) +
      # transition
      geom_ribbon(
        data = transition_ind,
        aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975,
            group = forecast_date, alpha = "95%", fill = "Transition")
      ) +
      geom_ribbon(
        data = transition_ind,
        aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75,
            group = forecast_date, alpha = "50%", fill = "Transition")
      ) +
      geom_line(
        data = transition_ind,
        aes(x = target_end_date, y = quantile_0.5, group = forecast_date),
        color = "grey" # "#179393"
      )
  }
  
  p <- p +   # observed as-of
    geom_line(
      aes(x = date, y = value, group = data_version, color = "as of forecast date"),
      linewidth = 0.4
    ) +
    geom_point(
      aes(x = date, y = value, group = data_version, color = "as of forecast date"),
      pch = 16, size = 0.6
    ) +
    # observed final (per window)
    geom_line(
      data = truth_final_ind,
      aes(x = date, y = value, color = "final"),
      linewidth = 0.4
    ) +
    geom_point(
      data = truth_final_ind,
      aes(x = date, y = value, color = "final"),
      pch = 16, size = 0.6
    ) +
    
    scale_color_manual(
      name = "Data version",
      values = c("as of forecast date" = "#D55E00", "final" = "black")
    ) +
    scale_fill_manual(
      name = "Task",
      values = c(Forecast = "seagreen", Nowcast = "#009ACD"), # #179393
      breaks = c("Nowcast", "Transition", "Forecast"),
      labels = c("Nowcast" = "Nowcast", "Transition" = "(Transition)", "Forecast" = "Forecast")
    ) +
    scale_linetype_manual(
      name = " ",
      values = c("Forecast date" = "dotted")
    ) +
    scale_alpha_manual(values = alphas, 
                       name = "Prediction intervals") +
    scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    labs(x = NULL, y = ylabs[ind], linetype = NULL) +
    theme_bw() +
    theme(strip.text.y = element_blank()) +
    custom_theme +
    theme(legend.position = "right",
          strip.background.x = element_blank(),
          strip.text.x = element_blank()) +
    ggtitle(titles[ind]) +
    guides(linetype = guide_legend(order = 1),
           col = guide_legend(order = 2),
           alpha = guide_legend(order = 3))
  
  p
  
  # write out:
  ggsave(
    paste0("figures/forecasts_", model_forecast, "_", ind, if(!main) "_suppl", ".pdf"),
    width = 140.5,
    height = 110,
    unit = "mm",
    device = "pdf"
  )
}



#####################################
# nowcasts for all models, horizon 1wk (FIG 6)

######################
# preparation
age_group  <- "00+"

all_dates <- seq(from = as.Date("2024-10-17"), to = as.Date("2025-03-27"), by = 7)
all_dates <- all_dates[!all_dates %in% as.Date(c("2024-12-26", "2025-01-02"))]

# make wide using custom function from above:
pred_all <- bind_rows(
  make_wide(all_dates, "window")
)

# generate a data frame containing forecasts from all models plus the transition from
# simple_nowcast where appropriate
nowcasts_0wk <- pred_all %>% filter(model %in% MODELS_NOWCAST[[ind]] & difftime(forecast_date, target_end_date) == 4 &
                                      indicator == ind)

# get the frozen time series:
frozen <- truth_all_ind %>% filter(difftime(date, as.Date(data_version)) >= -5)
nrow(frozen)

rt <- read_csv(paste0("data/reporting_triangle-", data_sources[ind], "-", 
                      ind, 
                      ".csv"))
rt_ind <- rt %>% filter(location == "DE" & 
                          age_group == "00+" & 
                          date >= as.Date("2024-07-01") & 
                          date <= as.Date("2025-06-30"))

filter(truth_all_ind, age_group == "00+")

#########################
# plot

ylabs <- c("sari" = "SARI incidence",
           "are" = "ARI incidence")
titles <- c("sari" = "SARI",
            "are" = "ARI")

nowcasts_0wk$before_christmas <- nowcasts_0wk$forecast_date <= as.Date("2024-12-24")

# determine ylim:
yl <- c(0, 1.05*max(c(nowcasts_0wk$quantile_0.975,
                      truth_final_ind$value)))

ggplot(truth_all_ind) +
  facet_wrap(facets = vars(model), labeller = labeller (model = MODEL_LABELS)) +
  # highlight christmas break
  annotate("rect", xmin = christmas_break[1], 
           xmax = christmas_break[2],
           ymin = yl[1], ymax = yl[2], colour = NA, 
           fill = "lightgrey", alpha = 0.5) +
  geom_line(
    data = nowcasts_0wk,
    aes(x = target_end_date, y = quantile_0.5, color = "Nowcast", 
        colour = "Nowcast", group = before_christmas)
  ) +
  geom_ribbon( # same-week nowcasts 95%
    data = nowcasts_0wk,
    aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975,
        alpha = "95%", fill = "Nowcast", group = before_christmas)
  ) +
  geom_ribbon( # same-week nowcasts 50%
    data = nowcasts_0wk,
    aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75,
        alpha = "50%", fill = "Nowcast", group = before_christmas)
  ) +
  # observed final (per window)
  geom_line(
    data = truth_final_ind,
    aes(x = date, y = value, color = "final"),
    linewidth = 0.4
  ) +
  geom_point(
    data = truth_final_ind,
    aes(x = date, y = value, color = "final"),
    size = 0.6
  ) +
  geom_line( # snapshots
    data = rt_ind,
    aes(x = date, y = value_0w, color = "as of nowcast date"),
    linewidth = 0.4
  ) +
  geom_point( # snapshots
    data = rt_ind,
    aes(x = date, y = value_0w, color = "as of nowcast date"),
    size = 0.6
  ) +
  scale_color_manual(
    name = "Data version",
    values = c("as of nowcast date" = "#D55E00", "final" = "black")
  ) +
  scale_fill_manual(
    name = " ",
    values = c(Nowcast = "#009ACD"), # #179393
  ) +
  scale_alpha_manual(values = alphas, guide = "none") +
  scale_linetype_manual(
    name = " ",
    values = c("Forecast date" = "dotted")
  ) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(x = NULL, y = ylabs[ind], linetype = NULL) +
  theme_bw() +
  custom_theme +
  theme(legend.position = "right",
        strip.background.x = element_blank()) +
  #        strip.text.x = element_blank()) +
  scale_alpha_manual(values = alphas, 
                     name = "Prediction intervals") +
  ggtitle(titles[ind])  +
  guides(linetype = guide_legend(order = 1),
         col = guide_legend(order = 2),
         alpha = guide_legend(order = 3))

# write out:
ggsave(
  paste0("figures/nowcasts_0wk_", ind, ".pdf"),
  width = 190.5,
  height = 110,
  unit = "mm",
  device = "pdf"
)



######################################
# Point forecasts of all models for age group 00+ (all dates)

######################
# preparation
age_group  <- "00+"

all_dates <- seq(from = as.Date("2024-10-17"), to = as.Date("2025-03-27"), by = 7)
all_dates <- all_dates[!all_dates %in% as.Date(c("2024-12-26", "2025-01-02"))]

# make wide using custom function from above:
pred_all <- bind_rows(
  make_wide(all_dates, "window")
)


# generate a data frame containing forecasts from all models plus the transition from
# simple_nowcast where appropriate

# place holders, to be filled
point_forecasts <- point_transition <- NULL
# nowcasts
point_nowcast <- pred_all %>% filter(model == "KIT-simple_nowcast")
# the forecast models to address:
forecast_models <- c("KIT-TSMixer", "KIT-LightGBM", "KIT-hhh4", "MPIDS-PS_embedding",
                     "KIT-EnsembleComplete", "baseline", "KIT-persistence", "KIT-hhh4_christmas",
                     "respicast-ensemble")

for(model in forecast_models){
  point_forecasts_mod <- pred_all %>% filter(model == !!model)
  # forecast <- bind_rows(forecast, nowcast)
  
  if(model %in% c("KIT-LightGBM", "KIT-TSMixer", "KIT-hhh4", "KIT-persistence")){
    point_nowcasts_mod <- bind_rows(filter(point_nowcast, difftime(forecast_date, target_end_date) == c(4)),
                                      filter(point_forecasts_mod, difftime(forecast_date, target_end_date) == - 3))
    point_nowcasts_mod$model <- model
  }else{
    point_nowcasts_mod <- NULL
  }
  
  if(is.null(point_forecasts)){
    point_forecasts <- point_forecasts_mod
    point_transition <- point_nowcasts_mod
  }else{
    point_forecasts <- bind_rows(point_forecasts, point_forecasts_mod)
    point_transition <- bind_rows(point_transition, point_nowcasts_mod)
  }
}


#########################
# plot

# vertical lines (per window)
vlines <- bind_rows(
  tibble(date = all_dates[-length(all_dates)], window = "window")
)
# labels and titles:
ylabs <- c("sari" = "SARI incidence",
           "are" = "ARI incidence")
titles <- c("sari" = "SARI",
            "are" = "ARI")

# get relevant forecasts and transitions from snapshot data:
point_forecasts_ind <- point_forecasts %>% filter(indicator == ind)
point_nowcasts_ind <- point_transition %>% filter(indicator == ind)

point_forecasts_ind$model <- factor(point_forecasts_ind$model,
                                    levels = MODEL_ORDER[MODEL_ORDER %in% unique(point_forecasts_ind$model)],
                                    ordered = TRUE)
point_nowcasts_ind$model <- factor(point_nowcasts_ind$model,
                                     levels = MODEL_ORDER[MODEL_ORDER %in% unique(point_nowcasts_ind$model)],
                                     ordered = TRUE)

# plot:

# determine ylim:
yl <- c(0, 1.02*max(c(point_forecasts_ind$quantile_0.5,
                      truth_final_ind$value)))

ggplot(point_forecasts_ind) +
  facet_wrap(facets = vars(model), labeller = labeller (model = MODEL_LABELS)) +
  annotate("rect", xmin = christmas_break[1], 
           xmax = christmas_break[2],
           ymin = yl[1], ymax = yl[2], colour = NA, 
           fill = "lightgrey", alpha = 0.5) +
  geom_line(
    data = point_forecasts_ind,
    aes(x = target_end_date, y = quantile_0.5, group = forecast_date, color = "predictive median")
  ) +
  geom_point(
    data = point_forecasts_ind,
    aes(x = target_end_date, y = quantile_0.5, group = forecast_date, 
        color = "predictive median"), size = 0.6, shape = 21, fill = "white"
  ) +
  # geom_line(
  #  data = point_nowcasts_ind,
  #  aes(x = target_end_date, y = quantile_0.5,
  #      group = forecast_date, color = "nowcast median")
  # ) +
  # observed final (per window)
  geom_line(
    data = truth_final_ind,
    aes(x = date, y = value, color = "final data"),
    linewidth = 0.4
  ) +
  geom_point(
    data = truth_final_ind,
    aes(x = date, y = value, color = "final data"),
    size = 0.6
  ) +
  scale_color_manual(
    name = "",
    values = c("predictive median" = "seagreen", 
               # "nowcast median" = "#7cd9f7", 
               "final data" = "black")
  ) +
  
  scale_linetype_manual(
    name = " ",
    values = c("Forecast date" = "dotted")
  ) +
  scale_alpha_manual(values = alphas, guide = "none") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(x = NULL, y = ylabs[ind], linetype = NULL) +
  theme_bw() +
  custom_theme +
  theme(legend.position = "right",
        strip.background.x = element_blank()) +
  #        strip.text.x = element_blank()) +
  ggtitle(titles[ind])

ggsave(
  paste0("figures/point_forecasts_", ind, ".pdf"),
  width = 250,
  height = 150,
  unit = "mm",
  device = "pdf"
)
