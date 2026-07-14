#################### get new output log entries functions #################### 

pull_new_odds_log_data <- function(upcoming_predictions, odds_raw) {
  
  upcoming_predictions |> 
    right_join(
      odds_raw,
      by = c("home_team", "away_team")
    ) |> 
    drop_na(match_id) |> 
    distinct(match_id, bookmaker, .keep_all = TRUE)
  
}

pull_new_bets_log_data <- function(upcoming_predictions, odds_raw) {
  
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
    slice_min(order_by = odd_diff, n = 1, with_ties = FALSE) |> 
    ungroup() |> 
    drop_na(match_id) |> 
    distinct(match_id, .keep_all = TRUE)
  
}



