#################### modelling functions #################### 
define_glm_model_specification <- function(model_parameters) {
  
  elastic_spec <- logistic_reg(
    penalty = model_parameters$penalty,
    mixture = model_parameters$mixture
  ) |> 
    set_engine("glmnet") |> 
    set_mode("classification")
  
  return(elastic_spec)
}

define_glm_data_PreprocessingRecipe <- function(train_data) {
  
  recipe(result ~ ., data = train_data) |> 
    update_role(match_id, new_role = "ID") |> 
    step_rm(
      home_team, away_team,
      date, round
    ) |> 
    step_novel(all_nominal_predictors()) |> 
    step_dummy(all_nominal_predictors()) |> 
    step_interact(terms = ~
                    elo_diff:outcome_mean_diff +
                    elo_diff:ladder_points_diff_diff +
                    tries_per_game_mean_diff:tries_conceded_per_game_mean_diff +
                    metres_per_run_mean_diff:metres_conceded_per_run_mean_diff +
                    lb_per_run_mean_diff:lb_conceded_per_run_mean_diff
    ) |> 
    step_zv(all_predictors()) |> 
    step_normalize(all_numeric_predictors()) |> 
    step_impute_median(all_numeric_predictors())
}

split_data <- function(features_data) {
  
  cutoff <- floor_date(Sys.Date(), unit = "weeks", week_start = 2)
  
  message("Date class:", class(features_data$date))
  
  model_data <- features_data
  
  message("Range of dates: ")
  print(range(features_data$date))
  
  train_data <- model_data |>  filter(date < cutoff) |> 
    drop_na(result)
  
  pred_data  <- model_data |>  filter(date >= cutoff)
  
  message("Number of training target NAs: ")
  print(sum(is.na(train_data$result)))
  
  message("Number of Predictions: ")
  print(nrow(pred_data))
  
  list(
    train_data = train_data,
    pred_data = pred_data
  )
}

fit_model <- function(data_split) {
  
  train_data <- data_split$train_data
  
  model_parameters <- readRDS(here("Outputs/elastic_model_parameters.rds"))
  
  model_specification <- define_glm_model_specification(model_parameters)
  
  preprocessing_recipe <- define_glm_data_PreprocessingRecipe(train_data)
  
  prep <- prep(preprocessing_recipe)
  message("Recipe prep successful")
  
  bake <- bake(prep, new_data = train_data)
  message("Recipe bake successful, rows: ")
  print(nrow(bake))
  
  modelling_workflow <- workflow() |> 
    add_model(model_specification) |> 
    add_recipe(preprocessing_recipe)
  
  set.seed(234)
  
  model_fit <- fit(modelling_workflow, data = train_data)
  
  message("Model fit was successful")
  
  model_fit
  
}














