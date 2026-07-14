# 🏉 NRL Match Prediction Model

A reproducible end-to-end data pipeline using R, Excel, SQL, and Power BI for predicting National Rugby League (NRL) match outcomes using statistical modelling and machine learning.

---

## 🔍 Overview

This project builds a fully automated pipeline to:

- Ingest historical/upcoming NRL data, and real-time Australian head-to-head odds
- Engineer features with player-level granularity
- Train predictive models for upcoming games
- Generate predictions, odds comparisons, and betting signals
- Track model performance over time

The pipeline is built using the `{targets}` package to ensure reproducibility, scalability, and efficiency. Data is stored using SQL Server Management Studio.

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
   - Player-level features (lineups, aggregated rolling form player statistics, position weightings, opponent adjustments)
   - Rolling mean and volatility aggregations, differentials, and interactions
   - No bookmaker odds are used as features (this separates my model from bookies, and avoids the predictive signal being dominated by one or two odds features)

4. **Modelling**
   - Predict match outcome
   - Generate win probabilities and predictions
   - Compare model outputs with bookmaker odds

6. **Output generation**
   - Predictions log (historical predictions made by model)
   - Odds log (historical odds fetched by pipeline)
   - Bets log (selected bets using the positive EV method

7. **Reporting**
   - Predictions log Excel workbook (to easily update manually with closing lines, and easy to observe)
   - Import into Power BI for reporting performance KPIs and trends (closing line value, accuracy, logloss, fictional rate of return, and diagnostics)
   - View the report: [Report](https://app.powerbi.com/view?r=eyJrIjoiZTU3ZTc0NzAtN2JmNC00ZWRlLTk2ZjgtZGMyY2I2ODFjZDQ3IiwidCI6IjNhYTEyYWIxLWQyNGEtNGI0Yy04YjI0LTk5ZWI3ODE2YzJjZSJ9&pageName=662e1d2e2b358634c3d6)

Note: The report intentionally reflects all past predictions, including those that use older/inferior models, despite continuous improvement in the models.

---

## 🧠 Modelling Approach (tidymodels)

The model combines:

- **Feature engineering from historical match data**
- Model selection using different **Statistical / machine learning modelling** methods (random forests, penalised regression, XGBoost, calibration and stacking techniques)
- Ultimately, landed on an Elastic Net Binary Classification model using the `{tidymodels}` framework, tuned via expanding window cross-validation folds.
- Rating systems like **Elo**, which dynamically update team strength based on match outcomes, acting like a latent prior estimate of team strength [1](https://www.iesrj.com/upload/10-Joel%20Carbone.pdf)  

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

├── Research Workbooks/     # workbooks for each stage of analysis to sandbox ideas (data cleaning/ingestion, feature engineering, modelling)

├── Queries/                # sql queries for tables and schema in database

├── Outputs                 # Pipeline outputs (model parameters, feature engineering parameters, etc)

├── .gitignore

├── Prediction Log.xlsx

├── NRL Model Performance Report.pbix

└── README.md

---

## Results

- Best logloss (probability calibration) on hold-out dataset was 0.64 (home advantage baseline is approximately 0.68-0.69, bookmaker odds approximately 0.64-0.65)
- Current closing line value (indicator of betting edge if positive) is statistically significant according to a right-tailed t-test and bootstrapping at 95% confidence

As discussed in the "Modelling Approach", the random forest, XGBoost, and general linear models (GLMs) all showed negligible differences. This is likely because of the rich historical data and hyper-optimised feature engineering, which together represent a large portion of the available predictive signal. Hence, predictions are probably linear combinations of these variables, which may be why complex ensemble methods don't demonstrate stronger performance. NRL match outcomes are inherently noisy, and if we also consider the bookmakers' similar hold-out performance, this may be close to the performance ceiling for log loss.

---


## ⚠️ Limitations

- Sports prediction is inherently uncertain due to randomness and unobservable factors
- Model performance greatly depends on data quality and feature design
- Betting outcomes depend on market efficiency and bookmaker pricing

---

## 🔐 Security

- API keys and database info are stored using environment variables

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

SQL:
- Basic functions
- Basic database design

---

## 📌 Future Improvements
- Expand to more rugby league competitions

---

## 🙌 Acknowledgements
NRL data sources, nrlR package, Odds API, and the open-source R ecosystem






