library(tidyverse)
source("code/config.R")

load_scores <- function(diseases = c("sari", "are"), by_age = FALSE, by_horizon = FALSE) {
  df <- read_csv("data/scores.csv", show_col_types = FALSE) %>%
    mutate(
      level = factor(level, levels = c("national", "age", "states"), ordered = TRUE)
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
      across(c(spread, overprediction, underprediction, wis, c50, c95), mean, na.rm = TRUE),
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

MODEL_ORDER <- c("KIT-simple_nowcast", "KIT-epinowcast", "RIVM-GAM", "KIT-MeanEnsemble", 
                 "KIT-LightGBM", "KIT-TSMixer", "KIT-hhh4", "MPIDS-PS_embedding", "baseline")

MODEL_COLORS <- c(
  "KIT-MeanEnsemble"    = "#009E73",
  "KIT-LightGBM"        = "#B30000",
  "KIT-TSMixer"         = "#E69F00",
  "KIT-hhh4"            = "#3C4AAD",
  "KIT-simple_nowcast"  = "#56B4E9",
  "RIVM-GAM"            = "#80471C",
  "MPIDS-PS_embedding"  = "#CC79A7",
  "baseline"          = "#000000",
  "Persistence"         = "#80471C"
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
    facet_wrap(~level, scales = "free_y", labeller = as_labeller(level_labels)) +
    scale_color_manual(values = MODEL_COLORS, guide = "none") +
    scale_fill_manual(values = MODEL_COLORS, guide = "none") +
    scale_alpha_discrete(
      labels = c("overprediction" = "Overprediction", 
                 "spread" = "Spread", 
                 "underprediction" = "Underprediction"),
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
    custom_theme +  # Assumes you’ve defined this elsewhere
    theme(
      legend.position = "right",
      #legend.title.position = "top"
    )
  
  return(p)
}

df_scores <- load_scores(diseases = "are", by_horizon = FALSE)

df_scores_long <- df_scores %>%
  pivot_longer(
    cols = c(wis, underprediction, spread, overprediction),
    names_to = "metric",
    values_to = "value"
  )

p <- plot_total_scores(df_scores_long)
p

ggsave("figures/wis.pdf", width = 190.5, height = 110, unit = "mm", device = "pdf")

plot_coverage <- function(df_wide, models = NULL) {
  if (!is.null(models)) {
    df_wide <- df_wide %>% filter(model %in% models)
  }
  
  model_order <- MODEL_ORDER[MODEL_ORDER %in% unique(df_wide$model)]
  df_wide <- df_wide %>%
    mutate(model = factor(model, levels = model_order, ordered = TRUE))
  
  alphas <- c("50%" = 0.7, "95%" = 0.4)
  
  level_labels <- c(
    "national" = "National level",
    "states" = "State level",
    "age" = "Age groups"
  )
  
  p <- ggplot(df_wide, aes(x = model)) +
    facet_wrap(~level, nrow = 1, scales = "fixed", labeller = as_labeller(level_labels)) +
    expand_limits(y = 1) +
    
    # Bars for 95% and 50% coverage
    geom_col(aes(y = c95), width = 0.7, fill = "white", show.legend = FALSE) +
    geom_col(aes(y = c95, fill = model, alpha = "95%"), width = 0.7, show.legend = FALSE) +
    geom_col(aes(y = c50, fill = model, alpha = "50%"), width = 0.7, show.legend = FALSE) +
    
    # Horizontal reference lines
    geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
    
    # Custom Y-axis label formatting
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
    
    # Labels and scales
    labs(
      x = NULL,
      y = "Empirical coverage",
      color = "Model",
      alpha = "Prediction interval: "
    ) +
    scale_fill_manual(values = MODEL_COLORS) +
    scale_alpha_manual(values = alphas, guide = guide_legend(reverse = FALSE)) +
    
    # Theme
    theme_bw() +
    custom_theme +
    theme(
      legend.position = "right",
      panel.spacing = unit(0.25, "lines")
    )
  
  return(p)
}

plot_coverage(df_scores)

ggsave("figures/coverage.pdf", width = 190.5, height = 110, unit = "mm", device = "pdf")


### WIS by horizon

df_scores <- load_scores(diseases = "are", by_horizon = TRUE)

df_scores_long <- df_scores %>%
  pivot_longer(
    cols = c(wis, underprediction, spread, overprediction),
    names_to = "metric",
    values_to = "value"
  )

level <- "national"

scores <- df_scores_long %>% filter(level == !!level)

scores <- df_scores_long

model_order <- MODEL_ORDER[MODEL_ORDER %in% unique(scores$model)]
scores <- scores %>%
  mutate(model = factor(model, levels = model_order, ordered = TRUE))

# Separate data for WIS and components
scores_wis <- scores %>% filter(metric == "wis")
scores_components <- scores %>% filter(metric != "wis")


p <- ggplot() +
  geom_bar(
    data = scores_wis,
    aes(x = model, y = value, color = model),
    fill = "white",
    stat = "identity",
    width = 0.7,
    show.legend = FALSE
  ) +
  geom_bar(
    data = scores_components,
    aes(x = model, y = value, fill = model, alpha = metric),
    stat = "identity",
    width = 0.7,
    size = 0.1,
    show.legend = TRUE
  ) +
  scale_color_manual(values = MODEL_COLORS, guide = "none") +
  scale_fill_manual(values = MODEL_COLORS, guide = "none") +
  scale_alpha_discrete(
    labels = c("overprediction" = "Overprediction",
               "spread" = "Spread",
               "underprediction" = "Underprediction"),
    guide = guide_legend(reverse = FALSE)
  ) +
  labs(
    x = NULL,
    y = "WIS",
    color = "Model",
    alpha = "Decomposition of WIS:",
    title = NULL #toupper(first(scores_wis$disease)) 
  ) +
  facet_grid(level~horizon, scales = "free", labeller = labeller(level = LEVEL_LABELS)) +
  theme_bw() +
  custom_theme +
  theme(
    legend.position = "bottom"
    # legend.title.position = "left",  # Not valid in ggplot2
    # axis.text.x = element_text(size = 7, angle = 90, hjust = 0.5, vjust = 0.5),
    # axis.text.y = element_text(size = 7)
  )

p

ggsave("figures/wis_by_horizon.pdf", width = 190.5, height = 110, unit = "mm", device = "pdf")


### By age group

df_sari <- load_scores(diseases = "sari", by_age = TRUE) %>% 
  filter(age_group != '00+')

scores <- df_sari %>%
  pivot_longer(
    cols = c(wis, underprediction, spread, overprediction),
    names_to = "metric",
    values_to = "value"
  )

plot_wis_by_age <- function(scores){
  model_order <- MODEL_ORDER[MODEL_ORDER %in% unique(scores$model)]
  scores <- scores %>%
    mutate(model = factor(model, levels = model_order, ordered = TRUE))
  
  # Separate data for WIS and components
  scores_wis <- scores %>% filter(metric == "wis")
  scores_components <- scores %>% filter(metric != "wis")
  
  
  p <- ggplot() +
    geom_bar(
      data = scores_wis,
      aes(x = model, y = value, color = model),
      fill = "white",
      stat = "identity",
      width = 0.7,
      show.legend = FALSE
    ) +
    geom_bar(
      data = scores_components,
      aes(x = model, y = value, fill = model, alpha = metric),
      stat = "identity",
      width = 0.7,
      size = 0.1,
      show.legend = TRUE
    ) +
    scale_color_manual(values = MODEL_COLORS, guide = "none") +
    scale_fill_manual(values = MODEL_COLORS, guide = "none") +
    scale_alpha_discrete(
      labels = c("overprediction" = "Overprediction",
                 "spread" = "Spread",
                 "underprediction" = "Underprediction"),
      guide = guide_legend(reverse = FALSE)
    ) +
    labs(
      x = NULL,
      y = "WIS",
      color = "Model",
      alpha = "Decomposition of WIS:",
      title = NULL #toupper(first(scores_wis$disease)) 
    ) +
    facet_wrap("age_group", scales = "free_y") +
    theme_bw() +
    custom_theme +
    theme(
      legend.position = "bottom"
      # axis.text.x = element_text(size = 7, angle = 90, hjust = 0.5, vjust = 0.5),
      # axis.text.y = element_text(size = 7)
    )  
}



p <- plot_wis_by_age(scores)
p

ggsave("figures/wis_by_age.pdf", width = 160, height = 120, unit = "mm", device = "pdf")

