library(tidyverse)
source("code/config.R")
source("code/data_utils.R")

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


load_rt <- function(indicator = "sari", preprocessed = FALSE) {
  source <- SOURCE_DICT[[indicator]]
  
  suffix <- if (preprocessed) "-preprocessed" else ""
  url <- glue::glue(
    "https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Hub/refs/heads/main/data/{source}/{indicator}/reporting_triangle-{source}-{indicator}{suffix}.csv"
  )
  
  rt <- read_csv(url, show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    select(1:which(names(.) == "value_4w"))
  
  return(rt)
}

r1 <- load_rt('sari')
r2 <- load_rt('are')
r3 <- load_rt('influenza')
r4 <- load_rt('rsv')


set_last_n_values_to_na <- function(group) {
  n <- nrow(group)
  for (i in 1:4) {
    col <- paste0("value_", i, "w")
    if (col %in% names(group) && n >= i) {
      group[(n - i + 1):n, col] <- NA
    }
  }
  return(group)
}

target_as_of <- function(rt, date) {
  date <- as.Date(date)
  
  rt_temp <- rt %>%
    filter(date <= !!date) %>%
    group_by(location, age_group) %>%
    group_modify(~ set_last_n_values_to_na(.x)) %>%
    ungroup() %>%
    mutate(
      value = rowSums(select(., value_0w, value_1w, value_2w, value_3w, value_4w), na.rm = TRUE),
      value = as.integer(value)
    ) %>%
    select(location, age_group, year, week, date, value)
  
  return(rt_temp)
}

r11 <- target_as_of(r1, "2025-04-27")


load_target_series <- function(indicator = "sari", as_of = NULL, age_group = NULL) {
  source <- SOURCE_DICT[[indicator]]
  
  if (is.null(as_of)) {
    url <- glue::glue(
      "https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Hub/main/data/{source}/{indicator}/target-{source}-{indicator}.csv"
    )
    target <- read_csv(url, show_col_types = FALSE)
  } else {
    rt <- load_rt(indicator)
    target <- target_as_of(rt, as_of)
  }
  
  # Filter for location == "DE"
  target <- target %>%
    filter(location == "DE") %>%
    mutate(age_group = str_replace(age_group, "00\\+", "DE"))
  
  # Optional age group filtering
  if (!is.null(age_group)) {
    target <- target %>% filter(age_group == age_group)
  }
  
  
  # Keep only common period across age groups
  date_range_common <- target %>%
    group_by(age_group) %>%
    summarise(start = min(date), end = max(date), .groups = "drop") %>%
    summarise(start = max(start), end = min(end))
  
  target <- target %>%
    filter(date >= date_range_common$start, date <= date_range_common$end)
  
  # Fill missing weeks with 0 and ensure weekly frequency
  target <- target %>%
    mutate(date = as.Date(date)) %>%
    complete(age_group, date = seq(min(date), max(date), by = "7 days"), fill = list(value = 0))
  
  # Pivot to wide format
  target_wide <- target %>%
    pivot_wider(
      names_from = age_group,
      values_from = value,
      names_prefix = paste0(source, "-", indicator, "-")
    ) %>%
    select(!any_of(c("year", "week", "location")))
  
  # Rename "00+" to "DE" if applicable
  if (is.null(age_group) || age_group == "00+") {
    target_wide <- target_wide %>%
      rename_with(~ str_replace(., paste0(source, "-", indicator, "-00\\+"), paste0(source, "-", indicator, "-DE")))
  }
  
  return(target_wide)
}

t1 <- load_target_series('sari')
t2 <- load_target_series('are')
t3 <- load_target_series('influenza')
t4 <- load_target_series('rsv')


load_combined_series <- function(indicator = "sari", as_of = NULL, drop_incomplete = TRUE) {
  source <- SOURCE_DICT[[indicator]]
  
  ts_target <- load_target_series(indicator, as_of)       # wide format, one column per age_group
  ts_latest <- load_latest_series(indicator, wide = TRUE) # same structure
  
  # Determine cutoff date from ts_target
  cutoff_date <- min(ts_target$date)
  
  # Truncate ts_latest to only include dates before cutoff_date
  ts_latest_truncated <- ts_latest %>%
    filter(date < cutoff_date)
  
  # Combine the two parts
  ts_combined <- bind_rows(ts_latest_truncated, ts_target)
  
  # Optionally drop the last 4 rows (incomplete weeks)
  if (drop_incomplete) {
    ts_combined <- ts_combined %>%
      slice_head(n = nrow(.) - 4)
  }
  
  return(ts_combined)
}

t1 <- load_combined_series('sari')
t2 <- load_combined_series('are')
t3 <- load_combined_series('influenza')
t4 <- load_combined_series('rsv')
