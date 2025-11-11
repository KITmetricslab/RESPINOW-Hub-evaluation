source("code/data_utils.R")
source("code/plot_functions.R")

df_scores <- load_scores(diseases = "sari", by_horizon = FALSE)

df_scores_long <- df_scores %>%
  pivot_longer(
    cols = c(wis, underprediction, spread, overprediction),
    names_to = "metric",
    values_to = "value"
  )

# WIS
p <- plot_total_scores(df_scores_long)
p
save_plot(p, "wis.pdf")


# Coverage
p <- plot_coverage(df_scores)
p
save_plot(p, "coverage.pdf")


# WIS by horizon
df_scores <- load_scores(diseases = "sari", by_horizon = TRUE)

df_scores_long <- df_scores %>%
  pivot_longer(
    cols = c(wis, underprediction, spread, overprediction),
    names_to = "metric",
    values_to = "value"
  )

p <- plot_wis_by_horizon(df_scores_long)
p
save_plot(p, "wis_by_horizon.pdf")


# WIS by age
df_scores <- load_scores(diseases = "sari", by_age = TRUE) %>%
  filter(age_group != '00+')

df_scores_long <- df_scores %>%
  pivot_longer(
    cols = c(wis, underprediction, spread, overprediction),
    names_to = "metric",
    values_to = "value"
  )
p <- plot_wis_by_age(df_scores_long)
p
save_plot(p, "wis_by_age.pdf")


# All plots per disease indicator

run_all_plots("sari")
run_all_plots("are")

