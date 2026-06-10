#################### targets for pipeline ####################

library(targets)

tar_source() # loads modules in R/ folder

tar_option_set(
  packages = c(
    "nrlR", "elo", "fuzzyjoin",
    "tidyverse", "tidymodels",
    "here", "DBI", "odbc",
    "httr", "jsonlite", "validate"
  )
)

# Pipeline (change the csv reading to file targets, and read into data frames donw stream)

list(
  
  # get exogenous parameters
  tar_target(params, read_params()),
  
  # add database connection later
  
  # load historical data
  tar_target(historical_ladder, read_csv(here("Data/01_Raw/Ladder Data.csv")),
             cue = tar_cue(mode = "always")),
  tar_target(historical_player, read_csv(here("Data/01_Raw/Player Data.csv")),
             cue = tar_cue(mode = "always")),
  tar_target(historical_team, read_csv(here("Data/01_Raw/Team Data.csv")),
             cue = tar_cue(mode = "always")),
  tar_target(player_key, read_csv(here("Data/01_Raw/Player Key.csv")),
             cue = tar_cue(mode = "always")),
  
  # fetch previous round data
  tar_target(PreviousRound_ladder_raw, fetch_PreviousRound_ladder_raw(params),
             cue = tar_cue(mode = "always")), # do not cache API forever
  
  tar_target(PreviousRound_player_raw, fetch_PreviousRound_player_raw(params),
             cue = tar_cue(mode = "always")), # do not cache API forever
  
  tar_target(PreviousRound_team_raw, fetch_PreviousRound_team_raw(params),
             cue = tar_cue(mode = "always")), # do not cache API forever
  
  # fetch upcoming odds
  tar_target(odds_raw, odds_API_fetch(),
             cue = tar_cue(mode = "always")),
  
  # fetch upcoming round data
  tar_target(UpcomingRound_ladder_raw, fetch_UpcomingRound_ladder_raw(params),
             cue = tar_cue(mode = "always")), # do not cache API forever
  
  tar_target(UpcomingRound_lineups_raw, fetch_UpcomingRound_lineups_raw(params), #########################################################
             cue = tar_cue(mode = "always")), # do not cache API forever
  
  tar_target(UpcomingRound_fixtures_raw, fetch_UpcomingRound_fixtures_raw(params), 
             cue = tar_cue(mode = "always")), # do not cache API forever
  
  # clean previous round data
  tar_target(PreviousRound_ladder_clean, clean_PreviousRound_ladder(PreviousRound_ladder_raw, historical_ladder, params)),
  tar_target(PreviousRound_player_clean, clean_PreviousRound_player(PreviousRound_player_raw, historical_player, params)),
  tar_target(PreviousRound_team_clean, clean_PreviousRound_team(PreviousRound_team_raw, historical_team, params)),
  
  # clean upcoming round data
  tar_target(UpcomingRound_ladder_clean, clean_UpcomingRound_ladder(UpcomingRound_ladder_raw, historical_ladder, params)),
  tar_target(UpcomingRound_fixtures_clean_long, clean_UpcomingRound_fixtures(UpcomingRound_fixtures_raw, params)),
  tar_target(UpcomingRound_lineup_clean, clean_UpcomingRound_lineup(UpcomingRound_lineups_raw, UpcomingRound_fixtures_clean_long, player_key)),
  
  # update historical data
  tar_target(historical_ladder_updated, update_historical_ladder(historical_ladder, PreviousRound_ladder_clean)),
  tar_target(historical_player_updated, update_historical_player(historical_player, PreviousRound_player_clean)),
  tar_target(historical_team_updated, update_historical_team(historical_team, PreviousRound_team_clean)),
  tar_target(player_key_updated, update_player_key(UpcomingRound_lineups_raw, player_key)),
  
  # export updated historical data
  tar_target(historical_ladder_file, 
             {
               path <- here("Data/01_Raw/Ladder Data.csv")
               write_csv(historical_ladder_updated, path)
               path
             }, 
             format = "file"),
  
  tar_target(historical_player_file, 
             {
               path <- here("Data/01_Raw/Player Data.csv")
               write_csv(historical_player_updated, path)
               path
             },
             
             format = "file"),
  tar_target(historical_team_file, 
             {
               path <- here("Data/01_Raw/Team Data.csv")
               write_csv(historical_team_updated, path)
               path
             },
             
             format = "file"),
  tar_target(player_key_file, 
             {
               path <- here("Data/01_Raw/Player Key.csv")
               write_csv(player_key_updated, path)
               path
             },
             format = "file"),
  
  # build features data
  tar_target(TeamLevel_features, build_TeamLevel_features(historical_team_updated, historical_ladder_updated, UpcomingRound_fixtures_clean_long, UpcomingRound_ladder_clean, params)),
  tar_target(PlayerLevel_features, build_player_features(historical_player_updated, UpcomingRound_lineup_clean, params)),
  tar_target(features_data, build_features_data(PlayerLevel_features, TeamLevel_features)),
  
  # update features data
  tar_target(features_data_file,
             {
               path <- here("Data/02_Features/feature_engineered_df.csv")
               write_csv(features_data, path)
               path
             },
             format = "file"),
  
  # fit model and predict on upcoming round data
  tar_target(upcoming_predictions, fit_model_and_make_predictions(features_data)),
  
  tar_target(model_metadata, 
             build_XgBoost_model_metadata(params)),
  
  # load output logs
  tar_target(historical_predictions_log, read_csv(here("Data/03_Outputs/prediction_log.csv")),
             cue = tar_cue(mode = "always")),
  tar_target(historical_odds_log, read_csv(here("Data/03_Outputs/odds_log.csv")),
             cue = tar_cue(mode = "always")),
  tar_target(historical_bets_log, read_csv(here("Data/03_Outputs/bets_log.csv")),
             cue = tar_cue(mode = "always")),
  
  # Get new output log entries
  tar_target(new_odds_log_data, pull_new_odds_log_data(upcoming_predictions, odds_raw, historical_odds_log)),
  tar_target(new_bets_log_data, pull_new_bets_log_data(upcoming_predictions, odds_raw, historical_bets_log)),
  
  # update outputs
  tar_target(predictions_log_updated, update_predictions_log(historical_predictions_log, upcoming_predictions)),
  tar_target(odds_log_updated, update_odds_log(historical_odds_log, new_odds_log_data)),
  tar_target(bets_log_updated, update_bets_log(historical_bets_log, new_bets_log_data)),
  
  # export updated outputs
  tar_target(predictions_log_updated_file,
             {
               path <- here("Data/03_Outputs/prediction_log.csv")
               write_csv(predictions_log_updated, path)
               path
             },
             format = "file"),
  
  tar_target(odds_log_updated_file,
             {
               path <- here("Data/03_Outputs/odds_log.csv")
               write_csv(odds_log_updated, path)
               path
             },
             format = "file"),
  
  tar_target(bets_log_updated_file,
             {
               path <- here("Data/03_Outputs/bets_log.csv")
               write_csv(bets_log_updated, path)
               path
             },
             format = "file")
  
)
