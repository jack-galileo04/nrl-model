# 🏉 NRL Match Prediction Model

A reproducible end-to-end data pipeline using R, Excel, and Power BI for predicting National Rugby League (NRL) match outcomes using statistical modelling and machine learning.

---

## 🔍 Overview

This project builds a fully automated pipeline to:

- Ingest historical/upcoming NRL data, and real-time Australian head-to-head odds
- Engineer match-level and player-level features
- Train predictive models for upcoming games
- Generate predictions, odds comparisons, and betting signals
- Track model performance over time

The pipeline is built using the `{targets}` package to ensure reproducibility, scalability, and efficiency.

---

## ⚙️ How It Works


This project follows a structured pipeline:

1. **Data ingestion**
   - Historical match, team, and player data
   - Live bookmaker odds source from "The Odds API"
   - Upcoming lineups and fixtures

2. **Data processing**
   - Cleaning and standardisation
   - Updating historical datasets after each round
   - Maintaining and updating player/team keys

3. **Feature engineering**
   - Team-level features (elo model, rolling form match statistics, performance trends, ladder details, match context, etc)
   - Player-level features (lineups, aggregated rolling form player statistics, position weightings)
   - No bookmaker odds are used as features

4. **Modelling**
   - Predict match outcome
   - Generate win probabilities and predictions
   - Compare model outputs with bookmaker odds

6. **Output generation**
   - Predictions log (historical predictions made by model)
   - Odds log (historical odds fetched by pipeline)
   - Bets log (selected bets using the positive EV method

7. **Reporting**
   - Predictions log excel workbook (to easily update with Bet365 closing lines)
   - Import into Power BI for reporting performance KPIs and trends (closing line value, accuracy, logloss, and fictional rate of return)
   - View the report: [Report](https://app.powerbi.com/view?r=eyJrIjoiZTU3ZTc0NzAtN2JmNC00ZWRlLTk2ZjgtZGMyY2I2ODFjZDQ3IiwidCI6IjNhYTEyYWIxLWQyNGEtNGI0Yy04YjI0LTk5ZWI3ODE2YzJjZSJ9&pageName=662e1d2e2b358634c3d6)

Note: The report was built before the model was completely finalised; hence, some inaccurate predictions are shown. This is intentional, as the report will remain despite continuous improvement in the models.

---

## 🧠 Modelling Approach (tidymodels)

The model combines:

- **Feature engineering from historical match data**
- Model selection using different **Statistical / machine learning modelling** methods
- Ultimately, landing on an XgBoost Binary Classification model using the `{tidymodels}` framework, tuned via expanding window cross-validation folds.
- Rating systems like **Elo**, which dynamically update team strength based on match outcomes [1](https://www.iesrj.com/upload/10-Joel%20Carbone.pdf)  

Predictions are based only on **pre-game information**, avoiding data leakage and ensuring realistic forecasts [2](https://www.kruzey.com.au/nrl-tips/nrl-betting-model/)

---

## 🔄 Pipeline (targets)

The workflow is managed using `{targets}`, which:

- Tracks dependencies between steps
- Only reruns steps when inputs change
- Enables scalable and reproducible pipelines

This ensures the outputs always align with the latest code and data, improving reliability and trust in results [3](https://docs.ropensci.org/targets/articles/overview.html)  

---

## 📂 Project Structure
.
├── run_pipeline.R          # helper script for managing the pipeline

├── _targets.R              # pipeline definition

├── R/                      # functions (data, features, modelling)

├── Data/

│   ├── 01_Raw/             # Raw data before feature engineering

│   ├── 02_Features/        # Features data set (useful for modelling sandbox)

│   └── 03_Outputs/         # Reporting outputs

├── Outputs                 # Pipeline outputs (model parameters, api tokens, model experiments log)

├── .gitignore

├── Prediction Log.xlsx

├── NRL Model Performance Report.pbix

└── README.md

---

## Results

- Best logloss (probability calibration) on hold out split was 0.64 (betting using home advantage as a baseline for binary classification yields approximately a 0.68-0.69)
- Current closing line value (indicator of betting edge if positive) is statistically significant according to a right-tailed t-test and boot strapping at 95% confidence

---


## ⚠️ Limitations

- Sports prediction is inherently uncertain due to randomness and unobservable factors
- Model performance greatly depends on data quality and feature design
- Betting outcomes depend on market efficiency and bookmaker pricing

---

## 🔐 Security

- API keys are stored using environment variables

---

## 🛠 Tech Stack

R:
- {targets} (pipeline orchestration)
- {tidyverse} (data wrangling)
- {tidymodels} (modelling)
- {httr} / {jsonlite} (API access)

Power BI:
- Power Query
- DAX

Excel:
- Power Query

---

## 📌 Future Improvements

- Implement more safeguards
- Move storage to a database

- Improve feature engineering (e.g. travel, weather, venue record, variances)
- Improve modelling framework (historical odds calibration, Bayesian hierarchical models, ensemble stacking)
- Enhance model explainability (move to penalised logistic regression, incorporate model selection and visualisations into pipeline)

- Expand to more rugby league competitions

---

## 🙌 Acknowledgements
NRL data sources, nrlR package, Odds API, and the open-source R ecosystem






