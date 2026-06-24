# RESPINOW-Hub-evaluation

This repository contains the evaluation pipeline for nowcasts and forecasts submitted to the RESPINOW Hub.

## Workflow

1. **Load & preprocess submissions**  
   All submitted nowcasts and forecasts are loaded and preprocessed using  
   `code/submissions.R`.  
   The processed submissions are stored in  
   `data/submissions.csv`.
   
2. **Visualize data and forecasts**
   The data are plotted with
   `code/plot_timeseries.R`
   Forecasts for SARI and ARI are plotted with
   `code/plot_forecasts.R`
   while the SurvStat targets are covered by
   `code/plot_forecasts_survstat.R`.
   Results are stored in `figures`.

2. **Compute evaluation metrics**  
   Evaluation metrics are computed from the processed submissions with  
   `code/compute_scores.R`.  
   The resulting scores are written to  
   `data/scores.csv`.

3. **Visualize results**  
   The computed scores and coverage fractions can be visualized using  
   `code/plot_wis.R` and `code/plot_coverage.R`.
   Results are stored in `figures`.
   
The code files use global settings and functions defined in the following R files:
   - `scoring_functions.R` implements WIS decomposition and coverage
   - `config.R` defines some dictionaries, model categories and colours for plotting
   - `data_utils` contains functions to load data (time series and forecasts)