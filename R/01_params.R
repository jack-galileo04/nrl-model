#################### reading parameters, set exogenous to the pipeline #################### 

read_params <- function(){
  
  list(
    season = 2026,
    round_number = 13,
    week_start = 2,
    
    lambda = 0.94, # Degree of recency bias in rolling form variables
    elo_k = 23, # Degree of recency bias in elo model
    
    special_round = "", # "nrl-team-lists-magic-round"
    
    position_minutes_weights = tribble(
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
    
    comp = 12999
  )
}

