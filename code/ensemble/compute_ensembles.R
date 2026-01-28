library(dplyr)
library(readr)
library(purrr)

# --- fixed nowcast member set (by model name) -----------------------------
NOWCAST_MODELS <- c("KIT-simple_nowcast", "KIT-epinowcast", "RIVM-GAM", "RKI-Pilot_01")

FORECAST_EXCLUDE <- c(NOWCAST_MODELS, "baseline")

# --- helpers --------------------------------------------------------------
read_member_quantiles <- function(source, indicator, model, filename) {
  read_csv(
    file.path("submissions", source, indicator, model, filename),
    show_col_types = FALSE
  ) %>%
    filter(type == "quantile")
}

compute_ensemble <- function(model_availability_slice) {
  df <- pmap_dfr(
    model_availability_slice %>% select(source, indicator, model, filename),
    function(source, indicator, model, filename) {
      tryCatch(
        read_member_quantiles(source, indicator, model, filename),
        error = function(e) {
          message("  - skip ", model, ": ", e$message)
          NULL
        }
      )
    }
  )
  
  if (nrow(df) == 0) {
    message("  -> ensemble skipped (no files loaded for this date)")
    return(NULL)
  }
  
  df %>%
    group_by(location, age_group, forecast_date, target_end_date, horizon, type, quantile) %>%
    summarize(value = mean(value), .groups = "drop")
}

write_ensemble <- function(df, source, indicator, forecast_date, model_name) {
  out_dir <- file.path("submissions", source, indicator, model_name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  write_csv(
    df,
    file.path(out_dir, paste0(forecast_date, "-", source, "-", indicator, "-", model_name, ".csv"))
  )
}

compute_ensembles <- function() {
  model_availability <- read_csv("code/ensemble/model_availability.csv", show_col_types = FALSE) 
  
  si_keys <- model_availability %>%
    distinct(source, indicator) %>%
    arrange(source, indicator)
  
  for (i in seq_len(nrow(si_keys))) {
    src <- si_keys$source[i]
    ind <- si_keys$indicator[i]
    
    message("\n", src, " / ", ind)
    
    df_si <- model_availability %>%
      filter(source == src, indicator == ind)
    
    # Define the 4 sets once (across all dates)
    fc_all <- df_si %>% filter(!model %in% FORECAST_EXCLUDE)
    fc_rt  <- fc_all %>% filter(status == "Prospective")
    
    nc_all <- df_si %>% filter(model %in% NOWCAST_MODELS)
    nc_rt  <- nc_all %>% filter(status == "Prospective")
    
    ok_fc_all <- n_distinct(fc_all$model) >= 2
    ok_fc_rt  <- n_distinct(fc_rt$model)  >= 2
    ok_nc_all <- n_distinct(nc_all$model) >= 2
    ok_nc_rt  <- n_distinct(nc_rt$model)  >= 2
    
    # Print eligibility once
    if (!ok_fc_all) message("  -> KIT-EnsembleComplete skipped (at least 2 forecast models required)")
    if (!ok_fc_rt)  message("  -> KIT-EnsembleRealtime skipped (at least 2 prospective forecast models required)")
    if (!ok_nc_all) message("  -> KIT-EnsembleNowcastComplete skipped (at least 2 nowcast models required)")
    if (!ok_nc_rt)  message("  -> KIT-EnsembleNowcastRealtime skipped (at least 2 prospective nowcast models required)")
    
    # If nothing can be computed, don’t loop dates
    if (!(ok_fc_all || ok_fc_rt || ok_nc_all || ok_nc_rt)) next
    
    for (fd_date in as.list(sort(unique(df_si$forecast_date)))) {
      message("  ", fd_date)
      
      df_fd <- df_si %>% filter(forecast_date == fd_date)
      
      if (ok_fc_all) {
        ens <- compute_ensemble(df_fd %>% filter(!model %in% FORECAST_EXCLUDE))
        if (!is.null(ens)) write_ensemble(ens, src, ind, fd_date, "KIT-EnsembleComplete")
      }
      
      if (ok_fc_rt) {
        ens <- compute_ensemble(df_fd %>% filter(!model %in% FORECAST_EXCLUDE, status == "Prospective"))
        if (!is.null(ens)) write_ensemble(ens, src, ind, fd_date, "KIT-EnsembleRealtime")
      }
      
      if (ok_nc_all) {
        ens <- compute_ensemble(df_fd %>% filter(model %in% NOWCAST_MODELS))
        if (!is.null(ens)) write_ensemble(ens, src, ind, fd_date, "KIT-EnsembleNowcastComplete")
      }
      
      if (ok_nc_rt) {
        ens <- compute_ensemble(df_fd %>% filter(model %in% NOWCAST_MODELS, status == "Prospective"))
        if (!is.null(ens)) write_ensemble(ens, src, ind, fd_date, "KIT-EnsembleNowcastRealtime")
      }
    }
  }
  
  invisible(NULL)
}

compute_ensembles()
