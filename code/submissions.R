library(tidyverse)

list_submissions <- function() {
  
  root_dir <- "submissions"
  
  file_paths <- list.files(path = root_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  
  data_list <- lapply(file_paths, function(path) {
    tryCatch({
      parts <- str_split(path, .Platform$file.sep)[[1]]
      
      data.frame(
        source = parts[2],
        disease = parts[3],
        model = parts[4],
        filename = parts[5],
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      message(glue::glue("Skipping malformed path: {path}"))
      NULL
    })
  })
  
  df <- bind_rows(data_list) %>%
    mutate(date = as.Date(substr(filename, 1, 10))) %>%
    filter(
      between(date, as.Date("2024-10-17"), as.Date("2025-03-27")),
      !date %in% c(as.Date("2024-12-26"), as.Date("2025-01-02")),
      !str_detect(model, "_old"),
      !(model == "MPIDS-PS_embedding" & source == "survstat")
    )
}

df <- list_submissions()

df %>%
  group_by(source, disease, model) %>%
  summarise(n_dates = n_distinct(date), .groups = "drop")


combine_submissions <- function() {
  d <- list_submissions()
  df <- data.frame()
  
  for (i in seq_len(nrow(d))) {
    row <- d[i, ]
    
    file_path <- file.path("submissions", row$source, row$disease, row$model, row$filename)
    
    df_temp <- tryCatch(
      read_csv(file_path, show_col_types = FALSE, progress = FALSE),
      error = function(e) {
        message(sprintf("Failed to load %s: %s", file_path, e$message))
        return(NULL)
      }
    )
    
    if (!is.null(df_temp)) {
      df_temp <- df_temp %>%
        mutate(
          source = row$source,
          disease = row$disease,
          model = row$model,
        )
      df <- bind_rows(df, df_temp)
    }
  }
  
  return(df)
}

determine_level <- function(df) {
  df %>%
    mutate(level = case_when(
      location == "DE" & age_group == "00+"  ~ "national",
      location != "DE"                       ~ "states",
      location == "DE" & age_group != "00+"  ~ "age",
      TRUE                                   ~ "unknown"
    ))
}

df <- combine_submissions()
df <- determine_level(df)

write_csv(df, "data/submissions.csv")
