## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(tidymodels)
library(vip)

library(lubridate)
library(here)
library(readxl)

library(rsample)

n <- 5 # Number of matches for form

ladder_df <- read_csv(here("Data/Raw/Ladder Data.csv"), col_select = -1)
player_df <- read_csv(here("Data/Raw/Player Data.csv"), col_select = -1)
team_df <- read_csv(here("Data/Raw/Team Data.csv"), col_select = -1)

player_df <- player_df |> 
  mutate(utc_start = as_date(utc_start))
team_df <- team_df |> 
  mutate(utc_start = as_date(utc_start))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
model_data <- player_forms_df |> 
  group_by(match_id, team_name) |> 
  summarise(
    date = first(utc_start),
    squad_id = first(squad_id),
    comp_id = first(competition_id),
    season = first(season),
    round = first(round),
    team_location = first(team_location),
    across(m_per_run:ineff_per_t, ~ mean(.x, na.rm = TRUE))
    ) |> 
  ungroup() |> 
  left_join(team_recovery_elo_df |> select(-utc_start, -team_location, -competition_id, -round), by = c("match_id", "team_name")) |> 
  select(result, everything()) |> 
  pivot_wider(
    id_cols = c(match_id, date),
    names_from = team_location,
    values_from = c(result, team_name, m_per_run:longelo),
    names_sep = "_"
  ) |> 
  mutate(
    result = factor(case_when(
      result_home > result_away ~ "H",
      result_away > result_home ~ "A",
      T ~ "A" # Not multiclass
    ))
    ) |> 
  mutate(
    days_rest_home = ifelse(is.na(days_rest_home), 0, days_rest_home),
    days_rest_away = ifelse(is.na(days_rest_away), 0, days_rest_away)
  ) |> 
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) |> 
  arrange(date) |> 
  select(-pr_home_away, -shortpr_home_away, -longpr_home_away,
         -elo_away, -elo_home, -shortelo_away, -shortelo_home, -longelo_away, -longelo_home) |> 
  rename(prhome = pr_home_home, shortprhome = shortpr_home_home, longprhome = longpr_home_home,
         away_result = result_away, home_result = result_home, away_team = team_name_away, home_team = team_name_home) |> 
  mutate(
    across(ends_with("_home"),
           ~ .-get(str_replace(cur_column(), "_home$", "_away")),
           .names = "{str_remove(.col,'_home$')}_diff"
           )
  ) |> 
  select(-ends_with("_home"), -ends_with("_away"))


## ----Data Split---------------------------------------------------------------------------------------------------------------------------------------------------
cutoff <- as.Date("2025-02-01")

train_data <- model_data |>  filter(date < cutoff)
test_data  <- model_data |>  filter(date >= cutoff)

N <- nrow(train_data)
number_folds <- 10
assess <- 150
skip <- 30
initial <- -(number_folds*skip+assess-N)

folds <- rolling_origin(
  data = train_data,
  initial = initial,
  assess = assess,
  skip = skip,
  cumulative = TRUE
)

metrics <- yardstick::metric_set(mn_log_loss, accuracy, f_meas)


## ----Model Recipes------------------------------------------------------------------------------------------------------------------------------------------------

xgb_rec <- recipe(result ~ ., data = train_data) |> 
  update_role(match_id, new_role = "ID") |> 
  step_rm(home_result, away_result,
          home_team, away_team,
          date) |> 
  step_zv(all_predictors()) |> 
  step_novel(all_nominal_predictors()) |> 
  step_string2factor(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_naomit(all_predictors()) # Fix Later


## ----Spec---------------------------------------------------------------------------------------------------------------------------------------------------------
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(), loss_reduction = tune(),
  sample_size = tune(), mtry = tune(),
  learn_rate = tune()
) |> 
  set_engine("xgboost",
             params = list(
               nthread = 4,
               objective = "binary:logistic",
               eval_metric = "logloss"
             )) |> 
  set_mode("classification")


## ----Workflow-----------------------------------------------------------------------------------------------------------------------------------------------------

xgb_wf <- workflow() |> 
  add_model(xgb_spec) |> 
  add_recipe(xgb_rec)

params <- xgb_wf |> 
  extract_parameter_set_dials() |> 
  finalize(train_data)

grid <- grid_space_filling(
  params,
  size = 20
)


## ----xgb Tune-----------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)

xgb_tune_res <- tune_grid(
  xgb_wf,
  resamples = folds,
  grid = grid,
  control = control_grid(verbose = TRUE, save_pred = TRUE),
  metrics = metrics
)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
xgb_best_logloss <- xgb_tune_res |> select_best(metric = "mn_log_loss")
xgb_logloss <- (xgb_tune_res |> show_best(metric = "mn_log_loss", n = 1))$mean

log_experiment(algorithm = "xgb",
               feature_set = "include lineup forms, ladders, elo, rest days, field position, possession, all differentials",
               resample_scheme = "10 cv folds",
               logloss = xgb_logloss,
               notes = "")

saveRDS(xgb_tune_res, here("Outputs/Models/xgb_tune_res.rds"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
xgb_tune_res |> collect_metrics() |> 
  arrange(.metric, desc(mean))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
xgb_tune_res |> collect_metrics() |> 
  filter(.metric == "mn_log_loss") |> 
  pivot_longer(
    cols = mtry:sample_size,
    values_to = "value",
    names_to = "parameter"
  ) |> 
  ggplot(aes(value, mean)) +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err), colour = "red", width = 2) +
  geom_point(size = 2) +
  facet_wrap(~parameter)

# No real pattern here


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(234)

final_wf <- finalize_workflow(xgb_wf, xgb_best_logloss)
final_fit <- fit(final_wf, data = train_data)

predict(final_fit, new_data = test_data, type = "prob") |> 
  bind_cols(test_data) |> 
  select(result, .pred_A, .pred_H) |> 
  mutate(
    result = case_when(
      result == "H" ~ 1,
      result == "A" ~ 0,
      T ~ 0
    )
  ) |> 
  summarise(
    logloss = -1/n()*sum(result*log(.pred_H) + (1-result)*log(.pred_A))
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
vip(final_fit, num_features = 20L)
# Lagged scores, and lagged possessions.


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
nrl <- read_excel(here("Data/Raw/nrl.xlsx")) |> janitor::clean_names()


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
nrl |> 
  select(home_odds_close, away_odds_close, home_score, away_score) |> 
  drop_na() |> 
  mutate(
    result = case_when(
      home_score > away_score ~ 1,
      home_score < away_score ~ 0, 
      T ~ 0
    ),
    home_pr_vig = 1/home_odds_close,
    away_pr_vig = 1/away_odds_close
  ) |> 
  mutate(
    home_pr = home_pr_vig/(home_pr_vig + away_pr_vig),
    away_pr = away_pr_vig/(home_pr_vig + away_pr_vig)
  ) |> 
  summarise(
    logloss = -1/n()*sum(result*log(home_pr) + (1-result)*log(away_pr))
  )
# Closing lines generally 0.6 logloss?


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Final Model

saveRDS(xgb_best_logloss, file = here("Outputs/best_logloss_params.rds"))  # Tuned parameters
saveRDS(final_wf, file = here("Outputs/modelling_pipeline_worklfow.rds")) # Final model workflow
fetch_lineups()
fetch_ladder_nrl()
fetch_team_stats_championdata()


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::purl("modelling_nrlR.Rmd", output = "nrlR_original_fetch.R")

