#################### modelling functions #################### 

define_XgBoost_model_specification <- function(model_parameters) {
  boost_tree(
    trees = 1000,
    tree_depth = model_parameters$tree_depth,
    min_n = model_parameters$min_n,
    loss_reduction = model_parameters$loss_reduction,
    sample_size = model_parameters$sample_size,
    mtry = model_parameters$mtry,
    learn_rate = model_parameters$learn_rate
  ) |> 
    set_engine(
      "xgboost",
      eval_metric = "logloss",
      nthread = 4
    ) |> 
    set_mode("classification")
}

define_XgBoost_Data_PreprocessingRecipe <- function(train_data) {
  
  recipe(result ~ ., data = train_data) |> 
    step_rm(
      match_id,
      home_result, away_result, # target variable proxies
      home_team, away_team, # Noisy, 17 factors
      date, round) |> # Using season stage for this
    step_zv(all_predictors()) |> # Remove predictors that are constant "no predictive value"
    step_novel(all_nominal_predictors()) |> # assigns unseen factor levels "new"
    step_dummy(all_nominal_predictors(), sparse = "no") # dummies factor variables
  
}

fit_model_and_make_predictions <- function(features_data) {
  
  cutoff <- floor_date(Sys.Date(), unit = "weeks", week_start = 2)
  
  message("Date class:", class(features_data$date))
  
  model_data <- features_data
  
  message("Number of target NAs: ", sum(is.na(model_data$result)))
  message("Range of dates: ")
  print(range(features_data$date))
  
  train_data <- model_data |>  filter(date < cutoff) |> 
    drop_na(result)
  pred_data  <- model_data |>  filter(date >= cutoff)
  
  message("Number of Predictions: ", nrow(pred_data))
  
  model_parameters <- readRDS(here("Outputs/xgb_model_parameters.rds"))
  
  model_specification <- define_XgBoost_model_specification(model_parameters)
  
  preprocessing_recipe <- define_XgBoost_Data_PreprocessingRecipe(train_data)
  
  prep <- prep(preprocessing_recipe)
  message("Recipe prep successful")
  
  bake <- bake(prep, new_data = pred_data)
  message("Recipe bake successful, rows: ", nrow(bake))
  
  modelling_workflow <- workflow() |> 
    add_model(model_specification) |> 
    add_recipe(preprocessing_recipe)
  
  set.seed(234)
  
  model_fit <- fit(modelling_workflow, data = train_data)
  
  message("Model fit was successful")
  
  model_fit |> 
    predict(new_data = pred_data, type = "prob") |> 
    bind_cols(pred_data) |> 
    select(
      date,
      match_id,
      home_team,
      away_team,
      home_prediction = .pred_H,
      away_prediction = .pred_A
    ) |> 
    mutate(prediction = ifelse(home_prediction > away_prediction, home_team, away_team))
  
}


build_XgBoost_model_metadata <- function(params) {
  
  model_parameters <- readRDS(here("Outputs/model_parameters.rds"))
  
  model_filename <- paste0("Models/", "model_metadata_", params$season, "_round", params$round_number, ".rds")
  
  model_metadata <- list(
    model = "xgboost",
    parameters = model_parameters,
    metric = "logloss",
    trained_on = Sys.Date(),
    feature_version = "v1",
    notes = paste0("season ",params$season, " round ", params$round_number, " predictions")
  )
  
  write_rds(model_metadata, here(model_filename))
  
}














