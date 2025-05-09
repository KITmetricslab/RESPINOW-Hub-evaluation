source("code/data_utils.R")
source("code/scoring_functions.R")

df <- load_submissions()

df_wis <- compute_wis(df)

df_coverage <- compute_coverage(df)

df_scores <- left_join(df_wis, df_coverage, by = c("source", "disease", "model", "level", "location", "age_group", "horizon"))

write_csv(df_scores, "data/scores.csv")
