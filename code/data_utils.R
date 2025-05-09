source("code/config.R")
library(tidyverse)

load_targets <- function() {
  names(SOURCE_DICT) %>%
    map_dfr(~{
      disease <- .x
      source <- SOURCE_DICT[[disease]]
      url <- str_glue("https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Hub/main/data/{source}/{disease}/target-{source}-{disease}.csv")
      
      read_csv(url, show_col_types = FALSE) %>%
        rename(target = value) %>%
        mutate(source = source, disease = disease)
    })
}

add_target <- function(df) {
  df_target <- load_targets()
  
  df_combined <- df %>%
    left_join(df_target, by = c("source", "disease", "location", "age_group", "target_end_date" = "date"))
  
  return(df_combined)
}

add_median <- function(df) {
  df_median <- df %>%
    filter(quantile == 0.5) %>%
    mutate(type = "median")
  
  bind_rows(df, df_median)
}

load_submissions <- function(include_target = TRUE, include_median = TRUE) {
  df <- read_csv("data/submissions.csv", show_col_types = FALSE)
  
  if (include_target) {
    df <- add_target(df)
  }
  
  if (include_median) {
    df <- add_median(df)
  }
  
  return(df)
}
