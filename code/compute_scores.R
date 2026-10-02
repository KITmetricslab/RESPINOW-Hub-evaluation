# This file computes WIS values and coverage fractions.

# load utility and scoring functions.
source("code/data_utils.R")
source("code/scoring_functions.R")
library(surveillance)

# get all submissions:
df <- load_submissions()

# compute WIS summary:
df_wis <- compute_wis(df)

# check balancedness:
table(df_wis$model, df_wis$disease)
nrow(df_wis)

# compute WIS summary for log-transformed target:
df_wis_log <- compute_wis(df, log = TRUE)
df_wis_log$n <- NULL
nrow(df_wis_log)

# join in one:
df_scores <- left_join(
  df_wis,
  df_wis_log,
  by = c(
    "source",
    "disease",
    "model",
    "level",
    "location",
    "age_group",
    "horizon",
    "forecast_date"
  )
)

nrow(df_wis)

# compute coverage:
df_coverage <- compute_coverage(df)
nrow(df_coverage)

# # add (note: coverage info will be stored in several places)
# df_scores <- left_join(
#   df_wis,
#   df_coverage,
#   by = c(
#     "source",
#     "disease",
#     "model",
#     "level",
#     "location",
#     "age_group",
#     "horizon"
#   )
# )
# 
# nrow(df_wis)

# write out:
write_csv(df_scores, "data/scores.csv")
write_csv(df_coverage, "data/coverage.csv")



