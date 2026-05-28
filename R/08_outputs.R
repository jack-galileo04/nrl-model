#################### get new output log entries functions #################### 

pull_new_odds_log_data <- function(upcoming_predictions, odds_raw, historical_odds_log) {
  
  upcoming_predictions |> 
    right_join(
      odds_raw,
      by = c("home_team", "away_team")
    ) |> 
    select(colnames(historical_odds_log)) |> 
    drop_na(match_id)
  
}

pull_new_bets_log_data <- function(upcoming_predictions, odds_raw, historical_bets_log) {
  
  upcoming_predictions |> 
    right_join(
      odds_raw,
      by = c("home_team", "away_team")
    ) |> 
    group_by(match_id) |> 
    mutate(
      best_home_odds = max(home_team_odds),
      best_away_odds = max(away_team_odds),
      home_ev = home_prediction * best_home_odds - 1,
      away_ev = away_prediction * best_away_odds - 1,
      ev_bet = ifelse(home_ev > away_ev, home_team, away_team),
      odd_diff = ifelse(home_ev >= away_ev, best_home_odds - home_team_odds, best_away_odds - away_team_odds)
    ) |> 
    arrange(odd_diff) |> 
    select(colnames(historical_bets_log)) |> 
    ungroup() |> 
    drop_na(match_id)
  
}


#################### update output logs #################### 

update_predictions_log <- function(historical_predictions_log, upcoming_predictions) {
  
  bind_rows(
    historical_predictions_log,
    upcoming_predictions
  ) |> 
    distinct(date, away_team, home_team, .keep_all = TRUE) # removing duplicates
  
}

update_odds_log <- function(historical_odds_log, new_odds_log_data) {
  
  bind_rows(
    historical_odds_log,
    new_odds_log_data) |> 
    distinct(match_id, bookmaker, .keep_all = TRUE)
  
}

update_bets_log <- function(historical_bets_log, new_bets_log) {
  
  bind_rows(
    historical_bets_log,
    new_bets_log) |> 
    distinct(match_id, .keep_all = TRUE) # removing duplicates
  
}



