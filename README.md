# NRL Match Prediction Model

### Tools: R, tidymodels, tidyverse, xgboost, ELO modelling, nrlR
### Problem: Predict EPL match outcomes (H/D/A)  
### Metric: Multinomial Log Loss
### Best result: Log Loss of 0.58 (binary classification)

## Overview
This project builds a predictive model using:
- player level granularity
- rolling averages
- home/away team stats
- differential features
- ELO modelling
- model tuning with tidymodels

## Methodology
Data is first ingested and cleaned with the "nrlR_original_fetch.R" script. 
The cleaned data is then used for feature engineering in the "feature_engineering_nrlR.R" script. 
Finally, the "modelling_nrlR.R" script builds the model data frame, performs the data split and cross-validation, and constructs a modelling pipeline to tune a Gradient Boosted Decision Tree classification model.
The "pipeline_nrlR.R" script is the weekly pipeline that is run after every team list announcement on Tuesday during the season. It updates the historical data with previous week results, fetches fixtures and lineups, and predicts the new weeks outcomes.

## Data
Data was sourced from the nrlR package

## Results
- Best log loss: 0.58
- ELOs, ladder positions, and field position were the strongest predictors in the model.

## Future Improvements
- Employ a Bayesian outer model with this model output used as a covariate. This will allow for posterior distribution estimates rather than singular point estimates.
- Build the pipeline using Azure.
