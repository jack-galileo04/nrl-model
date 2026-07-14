
make_predictions <- function(data_split, model_fit) {
  
  pred_data <- data_split$pred_data
  
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
    mutate(prediction = ifelse(home_prediction > away_prediction, home_team, away_team)) |> 
    distinct(match_id, .keep_all = TRUE)
  
}

build_model_metadata <- function(params) {
  
  model_parameters <- readRDS(here("Outputs/elastic_model_parameters.rds"))
  
  model_filename <- paste0("Models/", "model_metadata_", params$season, "_round", params$round_number, ".rds")
  
  model_metadata <- list(
    model = "elastic_net",
    parameters = model_parameters,
    metric = "logloss",
    trained_on = Sys.Date(),
    feature_version = "v2",
    notes = paste0("season ",params$season, " round ", params$round_number, " predictions")
  )
  
  write_rds(model_metadata, here(model_filename))
  
}