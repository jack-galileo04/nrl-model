#################### reading parameters, set exogenous to the pipeline #################### 

get_nrl_round <- function(date = Sys.Date()) {
  season_start <- as.Date("2026-03-03")  # adjust each season
  round <- as.integer((date - season_start) / 7) + 1
  return(round)
}

read_params <- function(){
  
  list(
    season = lubridate::year(Sys.Date()),
    round_number = get_nrl_round(),
    week_start = 2,
    
    lambda = 0.94, # Degree of recency bias in rolling form variables
    elo_k = 23, # Degree of recency bias in elo model
    
    special_round = "", # "nrl-team-lists-magic-round"
    
    position_minutes_weights = tibble::tribble(
      ~position, ~w,
      "Fullback", 1,
      "Wing", 1,
      "Centre", 1,
      "Five-Eighth", 1,
      "Halfback", 1,
      "Prop", 0.6,
      "Hooker", 0.8,
      "Second Row", 0.75,
      "Lock", 0.75,
      "Interchange", 0.4
    ), # weighting stats by minutes played (approximate)
    
    position_feature_weights = readRDS(here::here("Outputs/feature_position_weights.rds")),
    
    comp = 12999
  )
}


