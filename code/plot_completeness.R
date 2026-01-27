library(tidyverse)
Sys.setlocale("LC_TIME", "C")

df <- read_csv("https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Hub/refs/heads/main/respinow_viz/plot_data/other/list_commit_dates.csv")

df <- df %>% 
  filter(
    between(forecast_date, as.Date("2024-10-17"), as.Date("2025-03-27")),
    !forecast_date %in% c(as.Date("2024-12-26"), as.Date("2025-01-02"))
  ) %>%
  mutate(diff = parse_number(diff),
         retrospective = diff > 7)


all_models <- unique(df$model)
all_dates <- unique(df$forecast_date)
all_indicators <- unique(df$indicator)

all_models <- all_models[all_models != "KIT-MeanEnsemble"]

all_combinations <- expand.grid(
  model = all_models,
  forecast_date = all_dates,
  indicator = all_indicators,
  stringsAsFactors = FALSE
)

# Join and assign status
df_full <- all_combinations %>%
  left_join(df, by = c("model", "forecast_date", "indicator")) %>%
  mutate(
    status = case_when(
      is.na(retrospective) ~ "Missing",
      retrospective == TRUE ~ "Retrospective",
      retrospective == FALSE ~ "Prospective"
    )
  )

df_full <- df_full %>%
  mutate(
    status = case_when(
      model %in% c("MPIDS-PS_embedding", "KIT-hhh4") & indicator == "are" ~ "Prospective",
      model == "KIT-simple_nowcast" & indicator == "rsv"                  ~ "Prospective",
      model %in% c("RIVM-GAM", "KIT-epinowcast") & forecast_date == as.Date("2024-11-21") & 
        indicator == "influenza"   ~ "Data unavailable",
      TRUE ~ status
    )
  )


df_full <- df_full %>%
  group_by(model, indicator) %>%
  filter(any(status != "Missing"),
         indicator != 'pneumococcal') %>%
  filter(!(model == "MPIDS-PS_embedding" & indicator %in% c("rsv", "influenza")
  )) %>% 
  ungroup()

# save for selection of models for ensemble computation
write_csv(
  df_full,
  "code/ensemble/model_availability.csv"
)

df_full <- df_full %>%
  mutate(
    indicator = factor(
      recode(
        indicator,
        "are"        = "ARI",
        "sari"       = "SARI",
        "influenza"  = "Influenza",
        "rsv"        = "RSV"
      ),
      levels = c("ARI", "SARI", "Influenza", "RSV")
    )
  )


# Plot with faceting by indicator
ggplot(df_full, aes(x = forecast_date, y = model, fill = status)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_manual(values = c(
    "Prospective" = "forestgreen",
    "Retrospective" = "gold",
    "Missing" = "red",
    "Data unavailable" = "gray60"
  ),
  breaks = c(
    "Prospective",
    "Retrospective",
    "Missing",
    "Data unavailable"
  )) +
  facet_grid(indicator ~ ., scales = "free_y", space = "free_y") +
  theme_bw() +
  labs(
    #title = "Submission Completeness by Indicator",
    x = "Forecast Date",
    y = NULL,
    fill = NULL
  )

ggsave("figures/completeness.pdf", width = 190.5, height = 110, unit = "mm", device = "pdf")
