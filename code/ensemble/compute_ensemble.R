source("code/config.R")
source("code/ensemble/ensemble_functions.R")

indicators <- c("sari", "are", "influenza", "rsv")

members_forecast <- list(
  sari = c('KIT-hhh4', 'KIT-LightGBM', 'KIT-TSMixer', 'MPIDS-PS_embedding'),
  are = c('KIT-hhh4', 'KIT-LightGBM', 'KIT-TSMixer', 'MPIDS-PS_embedding'),
  influenza = c('HZI-ODEmodel'),
  rsv = c('HZI-ODEmodel')
)

members_nowcast <- list(
  sari = c('KIT-simple_nowcast', 'KIT-epinowcast', 'RIVM-GAM'),
  are = c('KIT-simple_nowcast'),
  influenza = c('KIT-simple_nowcast', 'KIT-epinowcast', 'RIVM-GAM'),
  rsv = c('KIT-simple_nowcast')
)

compute_ensembles <- function(indicator, type = "forecast") {
  source <- SOURCE_DICT[[indicator]]
  
  if (type == "forecast") {
    models <- members_forecast[[indicator]]
    model_name <- "KIT-Ensemble"
  } else if (type == "nowcast") {
    models <- members_nowcast[[indicator]]
    model_name <- "KIT-EnsembleNowcast"
  } else {
    stop("`type` must be either 'forecast' or 'nowcast'")
  }
  
  for (forecast_date in FORECAST_DATES) {
    tryCatch({
      print(forecast_date)
      df_ensemble <- compute_ensemble(indicator, models, forecast_date)
      
      dir_path <- paste0("submissions/", source, "/", indicator, "/", model_name, "/")
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      
      ensemble_path <- paste0(dir_path, forecast_date, "-", source, "-", indicator, "-", model_name, ".csv")
      write_csv(df_ensemble, ensemble_path)
    }, error = function(e) {
      message("Error on ", forecast_date, ": ", e$message)
      # continue to next forecast_date
    })
  }
}


compute_ensembles("sari", "forecast")
compute_ensembles("sari", "nowcast")

compute_ensembles("are", "forecast")
compute_ensembles("are", "nowcast")

compute_ensembles("influenza", "nowcast")

