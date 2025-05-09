load_scores <- function(diseases = c("sari", "are"), by_horizon = FALSE) {
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
  axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
  axis.text.y = element_text(size = 8)
)

MODEL_ORDER <- c("KIT-simple_nowcast", "KIT-epinowcast", "RIVM-GAM", "KIT-MeanEnsemble", "KIT-LightGBM", "KIT-TSMixer", "KIT-hhh4", "MPIDS-PS_embedding")

MODEL_COLORS <- c(
  "KIT-MeanEnsemble"    = "#009E73",
  "KIT-LightGBM"        = "#B30000",
  "KIT-TSMixer"         = "#E69F00",
  "KIT-hhh4"            = "#3C4AAD",
  "KIT-simple_nowcast"  = "#56B4E9",
  "RIVM-GAM"            = "#80471C",
  "MPIDS-PS_embedding"  = "#CC79A7",
  "Historical"          = "#000000",
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

df_sari <- load_scores(diseases = "sari", by_horizon = FALSE)

df_sari_long <- df_sari %>%
  pivot_longer(
    cols = c(wis, underprediction, spread, overprediction),
    names_to = "metric",
    values_to = "value"
  )

plot_total_scores(df_sari_long)
