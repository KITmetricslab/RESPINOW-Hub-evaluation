SOURCE_DICT <- c(
  sari = "icosari",
  are = "agi",
  influenza = "survstat",
  rsv = "survstat"
)

QUANTILES = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)

FORECAST_DATES = c("2024-10-17", "2024-10-24", "2024-10-31", "2024-11-07", "2024-11-14", 
                   "2024-11-21", "2024-11-28", "2024-12-05", "2024-12-12", "2024-12-19", 
                   "2025-01-09", "2025-01-16", "2025-01-23", "2025-01-30", "2025-02-06", 
                   "2025-02-13", "2025-02-20", "2025-02-27", "2025-03-06", "2025-03-13", 
                   "2025-03-20", "2025-03-27")

LEVEL_LABELS <- c(
  "national" = "National level",
  "states" = "State level",
  "age" = "Age groups"
)

MEMBERS_FORECAST <- list(
  sari = c('KIT-hhh4', 'KIT-LightGBM', 'KIT-TSMixer', 'MPIDS-PS_embedding'),
  are = c('KIT-hhh4', 'KIT-LightGBM', 'KIT-TSMixer', 'MPIDS-PS_embedding'),
  influenza = c('HZI-ODEmodel'),
  rsv = c('HZI-ODEmodel')
)

MEMBERS_NOWCAST <- list(
  sari = c('KIT-simple_nowcast', 'KIT-epinowcast', 'RIVM-GAM'),
  are = c('KIT-simple_nowcast'),
  influenza = c('KIT-simple_nowcast', 'KIT-epinowcast', 'RIVM-GAM'),
  rsv = c('KIT-simple_nowcast')
)

MODELS_FORECAST <- list(
  sari = c('KIT-hhh4', 'KIT-LightGBM', 'KIT-TSMixer', 'MPIDS-PS_embedding', 'KIT-MeanEnsemble', 'KIT-Ensemble', 'baseline'),
  are = c('KIT-hhh4', 'KIT-LightGBM', 'KIT-TSMixer', 'MPIDS-PS_embedding'),
  influenza = c('HZI-ODEmodel'),
  rsv = c('HZI-ODEmodel')
)

MODELS_NOWCAST <- list(
  sari = c('KIT-simple_nowcast', 'KIT-epinowcast', 'RIVM-GAM'),
  are = c('KIT-simple_nowcast'),
  influenza = c('KIT-simple_nowcast', 'KIT-epinowcast', 'RIVM-GAM'),
  rsv = c('KIT-simple_nowcast')
)
