# This file computes WIS values and coverage fractions.

# load utility and scoring functions.
source("code/data_utils.R")
source("code/scoring_functions.R")

# get all submissions:
df <- load_submissions()

# compute WIS summary:
df_wis <- compute_wis(df)

# compute coverage:
df_coverage <- compute_coverage(df)

# join in one:
df_scores <- left_join(
  df_wis,
  df_coverage,
  by = c(
    "source",
    "disease",
    "model",
    "level",
    "location",
    "age_group",
    "horizon"
  )
)

# write out:
write_csv(df_scores, "data/scores.csv")



