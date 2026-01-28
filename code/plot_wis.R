library(tidyverse)
library(ggh4x) # for facet_nested
library(patchwork) # to combine individual plots

source("code/config.R")

NOWCAST_MODELS <- c("KIT-simple_nowcast", "KIT-epinowcast", "RIVM-GAM", "RKI-Pilot_01",
                    "KIT-EnsembleNowcastRealtime", "KIT-EnsembleNowcastComplete")

load_scores <- function(
    diseases = c("sari", "are"),
    by_age = FALSE,
    by_horizon = FALSE
) {
  df <- read_csv("data/scores.csv", show_col_types = FALSE) %>%
    mutate(
      level = factor(
        level,
        levels = c("national", "age", "states"),
        ordered = TRUE
      )
    )
  
  # Ensure diseases is a character vector
  if (is.character(diseases) && length(diseases) == 1) {
    diseases <- c(diseases)
  }
  
  df <- df %>%
    filter(disease %in% diseases)
  
  group_cols <- c("disease", "level", "model")
  if (by_age) {
    group_cols <- c(group_cols, "age_group")
  }
  if (by_horizon) {
    group_cols <- c(group_cols, "horizon")
  }
  
  df_summary <- df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      across(
        c(spread, overprediction, underprediction, wis, c50, c95),
        mean,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  return(df_summary)
}


custom_theme <- theme(
  plot.title = element_text(size = 11),
  strip.text = element_text(size = 10),
  legend.title = element_text(size = 9),
  legend.text = element_text(size = 8),
  axis.title = element_text(size = 10),
  axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5),
  axis.text.y = element_text(size = 8)
)

MODEL_ORDER <- c(
  "KIT-EnsembleNowcast",
  "KIT-EnsembleNowcastRealtime",
  "KIT-EnsembleNowcastComplete",
  "KIT-simple_nowcast",
  "KIT-epinowcast",
  "RIVM-GAM",
  "RKI-Pilot_01",
  "KIT-MeanEnsemble",
  "KIT-Ensemble",
  "KIT-EnsembleRealtime",
  "KIT-EnsembleComplete",
  "KIT-LightGBM",
  "KIT-TSMixer",
  "KIT-hhh4",
  "MPIDS-PS_embedding",
  "HZI-ODEmodel",
  "baseline"
)

MODEL_COLORS <- c(
  # Ensembles (forecast) - green family
  "KIT-MeanEnsemble"        = "#009E73",  # keep
  "KIT-Ensemble"            = "#3dd193",  # keep
  "KIT-EnsembleRealtime"    = "#1B9E77",  # new (deeper green, realtime)
  "KIT-EnsembleComplete"    = "#66E0B8",  # new (lighter green, complete)
  
  # Ensembles (nowcast) - blue family
  "KIT-EnsembleNowcast"         = "#004f9e",  # keep
  "KIT-EnsembleNowcastRealtime" = "#2B6CB0",  # new (medium blue, realtime)
  "KIT-EnsembleNowcastComplete" = "#63B3ED",  # new (light blue, complete)
  
  # Member models (keep your old choices)
  "KIT-simple_nowcast"     = "#56B4E9",
  "KIT-epinowcast"         = "#8a8889",
  "RIVM-GAM"               = "#80471C",
  "RKI-Pilot_01"           = "#88dd38ff",
  "KIT-LightGBM"           = "#B30000",
  "KIT-TSMixer"            = "#E69F00",
  "KIT-hhh4"               = "#3C4AAD",
  "MPIDS-PS_embedding"     = "#CC79A7",
  "HZI-ODEmodel"           = "#6A3D9A", 
  
  # Baseline
  "baseline"               = "#000000"
)


plot_total_scores <- function(df_long, models = NULL) {
  if (!is.null(models)) {
    df_long <- df_long %>% filter(model %in% models)
  }
  
  # Set factor levels for model ordering
  model_order <- MODEL_ORDER[MODEL_ORDER %in% unique(df_long$model)]
  df_long <- df_long %>%
    mutate(model = factor(model, levels = model_order, ordered = TRUE))
  
  # Relabel level for facets
  level_labels <- c(
    "national" = "National level",
    "states" = "State level",
    "age" = "Age groups"
  )
  
  # Separate WIS and its components for plotting
  df_wis <- df_long %>% filter(metric == "wis")
  df_components <- df_long %>% filter(metric != "wis")
  
  p <- ggplot() +
    geom_bar(
      data = df_wis,
      aes(x = model, y = value, color = model),
      fill = "white",
      stat = "identity",
      width = 0.7,
      show.legend = FALSE
    ) +
    geom_bar(
      data = df_components,
      aes(x = model, y = value, fill = model, alpha = metric, color = model),
      stat = "identity",
      width = 0.7,
      size = 0.1,
      show.legend = TRUE
    ) +
    facet_nested(
      ~kind + level,
      scales = "free",
      space = "free_x",
      labeller = labeller(
        level = as_labeller(level_labels),
        kind  = function(x) rep("", length(x))   # <- hide inner strip text
      ),
      #independent="y",
      strip = strip_nested(
        background_x = c(element_blank(),
                         element_rect()),
        by_layer_x=TRUE),
      drop=TRUE,
    ) +
    scale_color_manual(values = MODEL_COLORS, guide = "none") +
    scale_fill_manual(values = MODEL_COLORS, guide = "none") +
    #scale_x_discrete(drop = TRUE) +
    scale_alpha_discrete(
      labels = c(
        "overprediction" = "Overprediction",
        "spread" = "Spread",
        "underprediction" = "Underprediction"
      ),
      guide = guide_legend(reverse = FALSE)
    ) +
    labs(
      x = NULL,
      y = "WIS",
      color = "Model",
      alpha = "Decomposition of WIS:",
      title = NULL
    ) +
    theme_bw() +
    custom_theme + # Assumes you’ve defined this elsewhere
    theme(
      legend.position = "right",
    )
  
  return(p)
}



# disease  = "are"
# level = "age"
# 
# df_scores <- load_scores(diseases = disease, by_horizon = FALSE) %>% 
#   filter(level == !!level) %>% 
#   mutate(kind = factor(ifelse(model %in% NOWCAST_MODELS, "Nowcast", "Forecast"), 
#                        levels = c("Nowcast", "Forecast")))
# 
# df_scores_long <- df_scores %>%
#   pivot_longer(
#     cols = c(wis, underprediction, spread, overprediction),
#     names_to = "metric",
#     values_to = "value"
#   )
# 
# p <- plot_total_scores(df_scores_long)
# p
# 
# g <- plot_total_scores(df_scores_long)
# g
# 
# p + g + plot_layout(guides = "collect")
# 
# ggsave(
#   paste0("figures/wis_", disease, ".pdf"),
#   plot = p,
#   width = 190.5,
#   height = 110,
#   unit = "mm",
#   device = "pdf"
# )

plot_wis <- function(disease, export = TRUE) {
  
  df <- load_scores(diseases = disease, by_horizon = FALSE) %>%
    mutate(
      kind = factor(
        ifelse(model %in% NOWCAST_MODELS, "Nowcast", "Forecast"),
        levels = c("Nowcast", "Forecast")
      )
    ) %>%
    pivot_longer(
      c(wis, underprediction, spread, overprediction),
      names_to = "metric",
      values_to = "value"
    )
  
  p_nat <- df %>%
    filter(level == "national") %>%
    plot_total_scores()
  
  p_age <- df %>%
    filter(level == "age") %>%
    plot_total_scores()
  
  p <- p_nat + p_age + patchwork::plot_layout(guides = "collect")
  
  if (export) {
    ggsave(
      paste0("figures/wis_", disease, ".pdf"),
      plot = p,
      width = 190.5,
      height = 110,
      units = "mm"
    )
  }
  
  p
}


plot_wis("are")
plot_wis("sari")
plot_wis("rsv")
plot_wis("influenza")

