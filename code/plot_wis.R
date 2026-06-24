# This file generates summary plots for WIS scores.

library(tidyverse)
library(ggh4x) # for facet_nested, used to separate nowcasts and forecasts
library(patchwork) # to combine plots, align them nicely and combine legends/axis titles

# get utility functions
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
  axis.line  = element_blank(),
  axis.ticks = element_line(linewidth = 0.2),
  panel.grid.major = element_line(linewidth = 0.2),
  panel.grid.minor = element_blank(),
  strip.background = element_rect(linewidth = 0.2),
  panel.border = element_rect(linewidth = 0.2)
)

labels_ind <- c("are" = "ARI",
                "sari" = "SARI")


# custom plotting functions. Note: these automatically write out plots to PDFs.

# summary plot for a given level (national level or age groups):
plot_wis_by_level <- function(df_long, models = NULL, label = "") {
  if (!is.null(models)) {
    df_long <- df_long %>% filter(model %in% models)
  }
  
  # Set factor levels for model ordering
  model_order <- MODEL_ORDER[MODEL_ORDER %in% unique(df_long$model)]
  df_long <- df_long %>%
    mutate(model = factor(model, levels = model_order, ordered = TRUE))
  
  # Relabel level for facets
  level_labels <- c(
    "national" = paste(label, "- total"),
    "states" = paste(label, "- by state"),
    "age" = paste(label, "- age-stratified")
  )
  
  # Separate WIS and its components for plotting
  df_wis <- df_long %>% filter(metric == "wis")
  df_ae <- df_long %>% filter(metric == "ae")
  df_ae$metric_point <- "ae"
  df_components <- df_long %>% filter(metric %in% c("underprediction", "spread", "overprediction"))
  
  # plot:
  p <- ggplot() +
    geom_bar(
      data = df_wis,
      aes(x = model, y = value, color = model),
      fill = "white",
      stat = "identity",
      width = 0.7,
      linewidth = 0.2,
      show.legend = FALSE
    ) +
    geom_bar(
      data = df_components,
      aes(x = model, y = value, fill = model, alpha = metric, color = model),
      stat = "identity",
      width = 0.7,
      linewidth = 0.2,
      show.legend = TRUE
    ) +
    geom_point(
      data = df_ae,
      aes(x = model, y = value, colour = model, shape = metric_point)
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
        background_x = list(element_blank(),
                         element_rect()),
        by_layer_x=TRUE),
      drop=TRUE,
    ) +
    scale_color_manual(values = MODEL_COLORS, guide = "none") +
    scale_fill_manual(values = MODEL_COLORS, guide = "none") +
    scale_x_discrete(labels = MODEL_LABELS) +
    #scale_x_discrete(drop = TRUE) +
    scale_shape(
      labels = c("ae" = "Absolute error"), 
    ) +
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
      title = NULL,
      shape = NULL
    ) +
    theme_bw() +
    custom_theme + # Assumes you’ve defined this elsewhere
    theme(
      legend.position = "right",
    )
  
  return(p)
}

# wrapper around plot_wis_by_level which loads stuf and saves output:
plot_wis <- function(disease, export = TRUE, models = NULL) {
  
  if(is.null(models)){
    models <- c(MODELS_NOWCAST[[disease]], MODELS_FORECAST[[disease]])
  }
  
  # get scores:
  df <- load_scores(diseases = disease, by_horizon = FALSE) %>%
    mutate(
      kind = factor(
        ifelse(model %in% NOWCAST_MODELS, "Nowcast", "Forecast"),
        levels = c("Nowcast", "Forecast")
      )
    ) %>%
    pivot_longer(
      c(wis, underprediction, spread, overprediction, ae),
      names_to = "metric",
      values_to = "value"
    )
  # restrict to models:
  df <- filter(df, model %in% models)
  # national:
  p_nat <- df %>%
    filter(level == "national") %>%
    plot_wis_by_level(label = labels_ind[disease])
  # age groups:
  p_age <- df %>%
    filter(level == "age") %>%
    plot_wis_by_level(label = labels_ind[disease])
  # stitch together:
  p <- p_nat + p_age + patchwork::plot_layout(guides = "collect")
  # export:
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

# apply to each target:
plot_wis("are")
plot_wis("sari")
plot_wis("rsv")
plot_wis("influenza")

# # get scores:
# scores <- read.csv("data/scores.csv")
# scores <- subset(scores, disease == "are" & age_group == "00+" & location == "DE" &
#                    model %in% c("KIT-EnsembleComplete", "respicast-ensemble"))


### WIS by horizon

# plotting function:
plot_wis_by_horizon <- function(df_scores_long, add_ae = FALSE){
  
  model_order <- MODEL_ORDER[MODEL_ORDER %in% unique(df_scores_long$model)]
  df_scores_long <- df_scores_long %>%
    mutate(model = factor(model, levels = model_order, ordered = TRUE))
  
  # Separate data for WIS and components
  scores_wis <- df_scores_long %>% filter(metric == "wis")
  scores_ae <- df_scores_long %>% filter(metric == "ae")
  scores_components <- df_scores_long %>% filter(metric %in% c("underprediction", "spread", "overprediction"))
  
  
  p <- ggplot() +
    geom_bar(
      data = scores_wis,
      aes(x = model, y = value, color = model),
      fill = "white",
      stat = "identity",
      width = 0.7,
      linewidth = 0.2,
      show.legend = FALSE
    ) +
    geom_bar(
      data = scores_components,
      aes(x = model, y = value, fill = model, alpha = metric),
      stat = "identity",
      width = 0.7,
      linewidth = 0.2,
      show.legend = TRUE
    ) +
    scale_color_manual(values = MODEL_COLORS, guide = "none") +
    scale_fill_manual(values = MODEL_COLORS, guide = "none") +
    scale_x_discrete(labels = MODEL_LABELS) +
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
      title = NULL #toupper(first(scores_wis$disease))
    ) +
    facet_grid(
      level ~ horizon,
      scales = "free",
      space = "free_x",
      labeller = labeller(level = LEVEL_LABELS)
    ) +
    theme_bw() +
    custom_theme +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 9)
    )
  
  if(add_ae){
    p <- p +
      geom_point(
      data = scores_ae,
      aes(x = model, y = value),
    )
  }
  
  return(p)
}

# wrapper around plot_wis_by_horizon which loads stuff and saves output:
plot_wis_by_horizon_disease <- function(disease, export = TRUE, add_ae = FALSE, models = NULL, label = "") {
  
  if(is.null(models)){
    models <- c(MODELS_NOWCAST[[disease]], MODELS_FORECAST[[disease]])
  }
  
  df_scores_long <- load_scores(diseases = disease, by_horizon = TRUE) %>%
    filter(level != "states") %>% 
    pivot_longer(
      cols = c(wis, underprediction, spread, overprediction, ae),
      names_to = "metric",
      values_to = "value"
    )
  
  df_scores_long <- df_scores_long %>% filter(model %in% models)
  p <- plot_wis_by_horizon(df_scores_long, add_ae = add_ae)
  
  if (export) {
    ggsave(
      paste0("figures/wis_by_horizon_", disease, label, ".pdf"),
      plot = p,
      width = 190.5,
      height = 110,
      units = "mm"
    )
  }
  
  p
}

# apply to four indicators:
plot_wis_by_horizon_disease("sari")
plot_wis_by_horizon_disease("are")
plot_wis_by_horizon_disease("influenza")
plot_wis_by_horizon_disease("rsv")

# special plot comparing hhh4 and hhh4-christmas
plot_wis_by_horizon_disease("are", models = c("KIT-hhh4", "KIT-hhh4_christmas"), label = "christmas")



### By age group (not exported)

# plotting function:
plot_wis_by_age <- function(disease, export = TRUE, models = NULL) {
  
  if(is.null(models)){
    models <- c(MODELS_NOWCAST[[disease]], MODELS_FORECAST[[disease]])
  }
  
  df_long <- load_scores(diseases = disease, by_age = TRUE) %>%
    filter(age_group != "00+") %>%
    mutate(
      kind = factor(
        ifelse(model %in% NOWCAST_MODELS, "Nowcast", "Forecast"),
        levels = c("Nowcast", "Forecast")
      )
    ) %>%
    pivot_longer(
      cols = c(wis, underprediction, spread, overprediction),
      names_to = "metric",
      values_to = "value"
    )
  
  df_long <- filter(df_long, model %in% models)
  
  model_order <- MODEL_ORDER[MODEL_ORDER %in% unique(df_long$model)]
  df_long <- df_long %>%
    mutate(model = factor(model, levels = model_order, ordered = TRUE))
  
  age_groups <- unique(df_long$age_group)
  
  plots <- lapply(age_groups, function(ag) {
    
    d <- df_long %>% filter(age_group == ag)
    
    d_wis <- d %>% filter(metric == "wis")
    d_comp <- d %>% filter(metric != "wis")
    
    ggplot() +
      geom_bar(
        data = d_wis,
        aes(x = model, y = value, color = model),
        fill = "white",
        stat = "identity",
        width = 0.7,
        linewidth = 0.2,
        show.legend = FALSE
      ) +
      geom_bar(
        data = d_comp,
        aes(x = model, y = value, fill = model, alpha = metric, color = model),
        stat = "identity",
        width = 0.7,
        linewidth = 0.2,
        show.legend = TRUE
      ) +
      facet_nested(
        ~ kind + age_group,
        scales = "free",
        space = "free_x",
        remove_labels = TRUE,
        labeller = labeller(
          age_group = function(x) x,
          kind = function(x) rep("", length(x))  # hide inner strip text
        ),
        strip = strip_nested(
          background_x = list(element_blank(), element_rect()),
          by_layer_x = TRUE
        ),
        drop = TRUE
      ) +
      scale_color_manual(values = MODEL_COLORS, labels = MODEL_LABELS, guide = "none") +
      scale_fill_manual(values = MODEL_COLORS, labels = MODEL_LABELS, guide = "none") +
      scale_x_discrete(labels = MODEL_LABELS) +
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
        alpha = "Decomposition of WIS:",
        title = NULL
      ) +
      theme_bw() +
      custom_theme +
      theme(
        plot.margin = margin(t = -20, r = 4, b = -20, l = 2), # to remove some whitespace from the top!
        strip.text = element_text(size = 9)
      )
  })
  
  
  if (length(plots) == 5) {
    
    # layout:
    # row1: p1 p2 legend
    # row2: p3 p4 p5 (so x labels are in the same row otherwise weird white space)
    p <- wrap_plots(
      c(plots[[1]], plots[[2]], guide_area(),
        plots[[3]], plots[[4]], plots[[5]]),
      ncol = 3
    ) +
      plot_layout(
        guides = "collect",
        axes = "collect_x",
        axis_titles = "collect"
      ) &
      theme(legend.position = "right")
    
    
  } else {
    
    p <- wrap_plots(
      plots,
      ncol = 3,
      guides = "collect",
      axes = "collect_x",
      axis_titles = "collect"
    ) &
      theme(legend.position = "bottom")
  }
  
  
  if (export) {
    ggsave(
      paste0("figures/wis_by_age_", disease, ".pdf"),
      plot = p,
      width = 190.5,
      height = 140,
      units = "mm",
      device = "pdf"
    )
  }
  
  p
}

plot_wis_by_age("sari")
plot_wis_by_age("are")
plot_wis_by_age("rsv")
plot_wis_by_age("influenza")

