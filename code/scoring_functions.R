# Quantile score
qs <- function(q, y, alpha) {
  2 * (as.numeric(y < q) - alpha) * (q - y)
}


# Compute squared error, absolute error or quantile score based on "type"
score <- function(prediction, observation, type, quantile) {
  if (type == "mean") {
    return((prediction - observation)^2)
  } else if (type == "median") {
    return(abs(prediction - observation))
  } else if (type == "quantile") {
    return(qs(prediction, observation, quantile))
  }
}


# Compute scores for each row in a dataframe
# compute_scores <- function(df) {
#   df <- df %>%
#     rowwise() %>%
#     mutate(score = score(value, truth, type, quantile),
#            score = round(score, digits = 5)) %>% 
#     select(-c(pathogen, value, truth))
# }


# Compute WIS decomposition
compute_wis <- function(df) {
  df_median <- df %>%
    filter(type == "quantile", quantile == 0.5) %>%
    rename(med = value) %>%
    select(-any_of(c("quantile", "target")))
  
  df <- df %>%
    filter(type == "quantile") %>%
    left_join(df_median, 
              by = c("location", "age_group", "forecast_date", 
                     "target_end_date", "horizon", "type", "source", "disease", 
                     "model", "level", "year", "week"))
  
  df <- df %>%
    rowwise() %>%
    mutate(
      wis = score(value, target, type, quantile),
      spread = score(value, med, type, quantile),
      overprediction = ifelse(med > target, wis - spread, 0),
      underprediction = ifelse(med < target, wis - spread, 0)
    )
  
  df <- df %>%
    group_by(source, disease, level, location, age_group, horizon, model) %>%
    summarize(
      spread = mean(spread),
      overprediction = mean(overprediction),
      underprediction = mean(underprediction),
      wis = mean(wis),
      .groups = "drop"
    )
  
  return(df)
}

compute_coverage <- function(df) {
  df_wide <- df %>%
    filter(type == "quantile") %>%
    pivot_wider(
      id_cols = c(source, disease, level, location, age_group, forecast_date, target_end_date, horizon,
                  type, model, year, week, target),
      names_from = quantile,
      values_from = value,
      names_prefix = "quantile_"
    )
  
  df_wide <- df_wide %>%
    mutate(
      c50 = target >= quantile_0.25 & target <= quantile_0.75,
      c95 = target >= quantile_0.025 & target <= quantile_0.975
    )
  
  coverage_df <- df_wide %>%
    group_by(source, disease, model, level, location, age_group, horizon) %>%
    summarise(
      c50 = mean(c50, na.rm = TRUE),
      c95 = mean(c95, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(coverage_df)
}