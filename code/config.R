# This file sets up dictionaries for relevant dates, models, colours etc.

SOURCE_DICT <- c(
  sari = "icosari",
  are = "agi",
  influenza = "survstat",
  rsv = "survstat"
)

QUANTILES = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)

FORECAST_DATES = c(
  "2024-10-17",
  "2024-10-24",
  "2024-10-31",
  "2024-11-07",
  "2024-11-14",
  "2024-11-21",
  "2024-11-28",
  "2024-12-05",
  "2024-12-12",
  "2024-12-19",
  "2025-01-09",
  "2025-01-16",
  "2025-01-23",
  "2025-01-30",
  "2025-02-06",
  "2025-02-13",
  "2025-02-20",
  "2025-02-27",
  "2025-03-06",
  "2025-03-13",
  "2025-03-20",
  "2025-03-27"
)

LEVEL_LABELS <- c(
  "national" = "Total",
  "states" = "State level",
  "age" = "By age group"
)

NOWCAST_MODELS <- c("KIT-simple_nowcast", "KIT-epinowcast", "RIVM-GAM", "RKI-Pilot_01",
                    "KIT-EnsembleNowcastRealtime", "KIT-EnsembleNowcastComplete")

# SETTINGS FOR PLOTS

options(scipen = 999) # turn off scientific notation

MODEL_ORDER <- c(
  "KIT-EnsembleNowcast",
  "KIT-EnsembleNowcastRealtime",
  "KIT-EnsembleNowcastComplete",
  "KIT-simple_nowcast",
  "KIT-epinowcast",
  "RIVM-GAM",
  "RKI-Pilot_01",
  "KIT-MeanEnsemble",
  "KIT-Ensemble",
  "KIT-EnsembleRealtime",
  "KIT-EnsembleComplete",
  "KIT-LightGBM",
  "KIT-TSMixer",
  "KIT-hhh4",
  "MPIDS-PS_embedding",
  "HZI-ODEmodel",
  "baseline",
  "KIT-persistence",
  "KIT-hhh4_christmas",
  "respicast-ensemble"
)

MODEL_COLORS <- c(
  # Ensembles (forecast) - green family
  "KIT-MeanEnsemble"        = "#009E73",  # keep
  "KIT-Ensemble"            = "#3dd193",  # keep
  "KIT-EnsembleRealtime"    = "#1B9E77",  # new (deeper green, realtime)
  "KIT-EnsembleComplete"    = "#66E0B8",  # new (lighter green, complete)
  
  # Ensembles (nowcast) - blue family
  "KIT-EnsembleNowcast"         = "#004f9e",  # keep
  "KIT-EnsembleNowcastRealtime" = "#2B6CB0",  # new (medium blue, realtime)
  "KIT-EnsembleNowcastComplete" = "#63B3ED",  # new (light blue, complete)
  
  # Member models (keep your old choices)
  "KIT-simple_nowcast"     = "#56B4E9",
  "KIT-epinowcast"         = "#8a8889",
  "RIVM-GAM"               = "#80471C",
  "RKI-Pilot_01"           = "#88dd38ff",
  "KIT-LightGBM"           = "#B30000",
  "KIT-TSMixer"            = "#E69F00",
  "KIT-hhh4"               = "#3C4AAD",
  "MPIDS-PS_embedding"     = "#CC79A7",
  "HZI-ODEmodel"           = "#6A3D9A", 
  
  # Baseline
  "baseline"               = "#000000",
  "KIT-persistence"        = "#382e11",
  "respicast-ensemble"     = "#6A3D9A"
)

MODEL_LABELS <- c(
  # Nowcasts
  "KIT-EnsembleNowcastRealtime"  = "EnsembleRealtime",
  "KIT-EnsembleNowcastComplete"  = "EnsembleComplete",
  "KIT-simple_nowcast"           = "SimpleNowcast",
  "KIT-epinowcast"               = "EpiNowcast",
  
  # Forecasts (collapsed)
  "KIT-EnsembleRealtime"         = "EnsembleRealtime",
  "KIT-EnsembleComplete"         = "EnsembleComplete",
  
  # Other models
  "KIT-LightGBM"                 = "LightGBM",
  "KIT-TSMixer"                  = "TSMixer",
  "KIT-hhh4"                     = "hhh4",
  "KIT-hhh4_christmas"           = "hhh4_christmas",
  "MPIDS-PS_embedding"           = "PS-Embedding",
  "RIVM-GAM"                     = "RIVM-GAM",
  "RKI-Pilot_01"                 = "RKI-Pilot",
  "HZI-ODEmodel"                 = "ODEmodel",
  
  # Baselines
  "baseline"                     = "Historical",
  "KIT-persistence"              = "Persistence",
  "respicast-ensemble"           = "Respicast"
)


## MAYBE OUTDATED, NOT SURE WHERE IT'S USED

MEMBERS_FORECAST <- list(
  sari = c('KIT-hhh4', 'KIT-LightGBM', 'KIT-TSMixer', 'MPIDS-PS_embedding'),
  are = c('KIT-hhh4', 'KIT-LightGBM', 'KIT-TSMixer', 'MPIDS-PS_embedding'),
  influenza = c('HZI-ODEmodel'),
  rsv = c('HZI-ODEmodel')
)

MEMBERS_NOWCAST <- list(
  sari = c('KIT-simple_nowcast', 'KIT-epinowcast', 'RIVM-GAM', 'RKI-Pilot_01'),
  are = c('KIT-simple_nowcast'),
  influenza = c('KIT-simple_nowcast', 'KIT-epinowcast', 'RIVM-GAM'),
  rsv = c('KIT-simple_nowcast')
)

MODELS_FORECAST <- list(
  sari = c(
    'KIT-hhh4',
    'KIT-LightGBM',
    'KIT-TSMixer',
    'MPIDS-PS_embedding',
    "KIT-EnsembleRealtime",
    'KIT-EnsembleComplete',
    'KIT-persistence',
    'baseline'
  ),
  are = c('KIT-hhh4',
          'KIT-LightGBM',
          'KIT-TSMixer',
          'MPIDS-PS_embedding',
          'KIT-EnsembleComplete',
          'KIT-persistence',
          'baseline',
          'respicast-ensemble'),
  influenza = c('HZI-ODEmodel'),
  rsv = c('HZI-ODEmodel')
)

MODELS_NOWCAST <- list(
  sari = c('KIT-simple_nowcast', 'KIT-epinowcast', 'RIVM-GAM', 'RKI-Pilot_01'),
  are = c('KIT-simple_nowcast'),
  influenza = c('KIT-simple_nowcast', 'KIT-epinowcast', 'RIVM-GAM'),
  rsv = c('KIT-simple_nowcast')
)
