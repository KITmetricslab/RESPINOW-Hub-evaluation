library(tidyverse)
library(ggh4x) # for facet_nested, used to separate nowcasts and forecasts
library(patchwork) # to combine plots, align them nicely and combine legends/axis titles

source("code/config.R")
source("code/data_utils.R")

custom_theme <- theme(
  plot.title = element_text(size = 11),
  strip.text = element_text(size = 10),
  legend.title = element_text(size = 9),
  legend.text = element_text(size = 8),
  axis.title = element_text(size = 10),
  axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5),
  axis.text.y = element_text(size = 8),
  axis.line = element_line(linewidth = 0.2),
  axis.ticks = element_line(linewidth = 0.2),
  panel.grid.major = element_line(linewidth = 0.2),
  panel.grid.minor = element_blank(),
  strip.background = element_rect(linewidth = 0.2),
  panel.border = element_rect(linewidth = 0.2)
)


plot_coverage_by_level <- function(df_wide, models = NULL) {
  if (!is.null(models)) {
    df_wide <- df_wide %>% filter(model %in% models)
  }
  
  model_order <- MODEL_ORDER[MODEL_ORDER %in% unique(df_wide$model)]
  df_wide <- df_wide %>%
    mutate(model = factor(model, levels = model_order, ordered = TRUE)) 
  
  alphas <- c("50%" = 0.7, "95%" = 0.4)
  
  p <- ggplot(df_wide, aes(x = model)) +
    geom_col(aes(y = c95), width = 0.7, fill = "white", show.legend = FALSE) +
    geom_col(aes(y = c95, fill = model, alpha = "95%"), width = 0.7, show.legend = FALSE) +
    geom_col(aes(y = c50, fill = model, alpha = "50%"), width = 0.7, show.legend = TRUE) +
    geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed", linewidth=0.35) +
    facet_nested(
      ~ kind + level,
      scales = "free",
      space = "free_x",
      labeller = labeller(
        level = as_labeller(LEVEL_LABELS),
        kind  = function(x) rep("", length(x))
      ),
      strip = strip_nested(
        background_x = c(element_blank(), element_rect()),
        by_layer_x = TRUE
      ),
      drop = TRUE
    ) +
    scale_fill_manual(values = MODEL_COLORS, guide = "none") +
    scale_x_discrete(labels = MODEL_LABELS, drop = TRUE) +
    scale_alpha_manual(values = alphas, guide = guide_legend(reverse = FALSE)) +
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
    labs(
      x = NULL,
      y = "Empirical coverage",
      alpha = "Prediction interval: ",
      title = NULL
    ) +
    theme_bw() +
    custom_theme +
    theme(legend.position = "right")
  
  p
}

plot_coverage <- function(disease, export = TRUE) {
  
  df <- load_scores(diseases = disease, by_horizon = FALSE) %>%
    mutate(
      kind = factor(
        ifelse(model %in% NOWCAST_MODELS, "Nowcast", "Forecast"),
        levels = c("Nowcast", "Forecast")
      )
    )
  
  p_nat <- df %>%
    filter(level == "national") %>%
    plot_coverage_by_level()
  
  p_age <- df %>%
    filter(level == "age") %>%
    plot_coverage_by_level()
  
  p <- p_nat + p_age + patchwork::plot_layout(guides = "collect")
  
  if (export) {
    ggsave(
      paste0("figures/coverage_", disease, ".pdf"),
      plot = p,
      width = 190.5,
      height = 110,
      units = "mm"
    )
  }
  
  p
}

plot_coverage("sari")
plot_coverage("are")
plot_coverage("influenza")
plot_coverage("rsv")

