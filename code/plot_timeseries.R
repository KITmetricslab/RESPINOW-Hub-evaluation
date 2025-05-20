library(tidyverse)
source("code/config.R")

load_latest_series <- function(indicator = "sari", wide=TRUE) {
  source <- SOURCE_DICT[[indicator]]
  
  url <- glue::glue(
    "https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Hub/refs/heads/main/data/{source}/{indicator}/latest_data-{source}-{indicator}.csv"
  )
  
  ts <- read_csv(url, show_col_types = FALSE) %>%
    filter(location == "DE") %>%
    mutate(date = as.Date(date),
           age_group = str_replace(age_group, "00\\+", "DE"))
  
  # Keep only time period common to all age groups
  date_range_common <- ts %>%
    group_by(age_group) %>%
    summarise(start = min(date), end = max(date), .groups = "drop") %>%
    summarise(start = max(start), end = min(end))
  
  ts_trimmed <- ts %>%
    filter(date >= date_range_common$start, date <= date_range_common$end)
  
  # ts_trimmed %>%
  #   summarise(across(everything(), ~ sum(is.na(.))))
  
  ts_filled <- ts_trimmed %>%
    complete(
      age_group,
      date = seq(min(date), max(date), by = "7 days"),
      fill = list(value = 0)
    )
  
  if (wide){
    # Reshape to wide format
    ts_wide <- ts_filled %>%
      pivot_wider(
        names_from = age_group,
        values_from = value,
        names_prefix = paste0(source, "-", indicator, "-")
      ) %>%
      select(!any_of(c("year", "week", "location")))
    
    return(ts_wide)
  } else {
    return(ts_filled)
  }
}

ts1 <- load_latest_series('sari', wide=FALSE)
ts2 <- load_latest_series('are')
ts3 <- load_latest_series('influenza')
ts4 <- load_latest_series('rsv')
