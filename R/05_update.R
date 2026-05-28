#################### update historical data #################### 

update_historical_ladder <- function(historical_ladder, PreviousRound_ladder_clean) {
  
  bind_rows(
    historical_ladder,
    PreviousRound_ladder_clean
  ) |> 
    distinct(team, season, round, .keep_all = TRUE) # removing duplicates
  
}

update_historical_player <- function(historical_player, PreviousRound_player_clean) {
  
  bind_rows(
    historical_player,
    PreviousRound_player_clean
  ) |> 
    distinct(player_id, match_id, .keep_all = TRUE) # removing duplicates
  
}

update_historical_team <- function(historical_team, PreviousRound_team_clean) {
  
  bind_rows(
    historical_team,
    PreviousRound_team_clean
  ) |> 
    distinct(team_name, match_id, .keep_all = TRUE) # removing duplicates
  
}

update_player_key <- function(UpcomingRound_lineups_raw, player_key) {
  # bit of code duplication with 04_clean
  
  UpcomingRound_lineup_NaiveMatch <- build_UpcomingRound_lineup_NaiveMatch(UpcomingRound_lineups_raw, player_key)
  
  UpcomingRound_lineup_UnMatched <- pull_UpcomingRound_lineup_UnMatched(UpcomingRound_lineup_NaiveMatch)
  
  UpcomingRound_lineup_FuzzyMatch <- build_UpcomingRound_lineup_FuzzyMatch(UpcomingRound_lineup_UnMatched, player_key)
  
  UpcomingRound_lineup_NewPlayers <- pull_UpcomingRound_lineup_NewPlayers(UpcomingRound_lineup_FuzzyMatch)
  
  updated_player_key <- add_NewPlayers_player_key(UpcomingRound_lineup_NewPlayers, player_key)
  
  updated_player_key |> 
    distinct(player_id, .keep_all = TRUE) # removing duplicates
  
}









