# RESPINOW-Hub-evaluation

This repository contains the evaluation pipeline for nowcasts and forecasts submitted to the RESPINOW Hub.

## Workflow

1. **Load & preprocess submissions**  
   All submitted nowcasts and forecasts are loaded and preprocessed using  
   `code/submissions.R`.  
   The processed submissions are stored in  
   `data/submissions.csv`.

2. **Compute evaluation metrics**  
   Evaluation metrics are computed from the processed submissions with  
   `code/compute_scores.R`.  
   The resulting scores are written to  
   `data/scores.csv`.

3. **Visualize results**  
   The computed scores can be visualized using  
   `code/plot_wis.R` and `code/plot_coverage.R`.
