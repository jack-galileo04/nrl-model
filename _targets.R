#################### targets for pipeline ####################

message("DB_SERVER = ", Sys.getenv("DB_SERVER"))
message("DB_NAME = ", Sys.getenv("DB_NAME"))


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
  tar_target(params, read_params(),
             cue = tar_cue(mode = "always")),
  
  # read historical data
  tar_target(historical_ladder_data, db_read(schema = "clean", table = "Ladder Data")),
  tar_target(historical_player_data, db_read(schema = "clean", table = "Player Data")),
  tar_target(historical_team_data, db_read(schema = "clean", table = "Team Data")),
  tar_target(player_key, db_read(schema = "clean", table = "Player Key")),
  
  # fetch previous round data
  tar_target(PreviousRound_ladder, fetch_PreviousRound_ladder(params)),
  tar_target(PreviousRound_player, fetch_PreviousRound_player(params)),
  tar_target(PreviousRound_team, fetch_PreviousRound_team(params)),
  
  # update historical data
  tar_target(updated_ladder, 
             historical_ladder_data |> 
               bind_rows(
                 PreviousRound_ladder |> select(all_of(colnames(historical_ladder_data)))
               ) |> 
               distinct(team, season, round, .keep_all = TRUE) 
  ),
  tar_target(updated_player, 
             historical_player_data |> 
               bind_rows(
                 PreviousRound_player |> select(all_of(colnames(historical_player_data)))
               ) |> 
               distinct(player_id, match_id, .keep_all = TRUE) 
  ),
  tar_target(updated_team, 
             historical_team_data |> 
               bind_rows(
                 PreviousRound_team |> select(all_of(colnames(historical_team_data)))
               ) |> 
               distinct(team_name, match_id, .keep_all = TRUE)
  ),
  tar_target(player_key_updated, 
             update_player_key(UpcomingRound_lineups_raw, player_key)
             ),
  
  # write historical data
  tar_target(ladder_db, db_write(
    schema = "clean", 
    table = "Ladder Data", 
    df = updated_ladder
    )
  ),
    tar_target(player_db, db_write(
    schema = "clean", 
    table = "Player Data", 
    df = updated_player
    )
  ),
  tar_target(team_db, db_write(
    schema = "clean", 
    table = "Team Data", 
    df = updated_team
    )
  ),
  tar_target(player_key_db, db_write(
    schema = "clean", 
    table = "Player Key", 
    df = player_key_updated
    )
  ),
  
  # fetch upcoming round data
  tar_target(UpcomingRound_ladder_raw, fetch_UpcomingRound_ladder_raw(params),
             cue = tar_cue(mode = "always")), # do not cache API forever
  
  tar_target(UpcomingRound_lineups_raw, fetch_UpcomingRound_lineups_raw(params), 
             cue = tar_cue(mode = "always")), # do not cache API forever
  
  tar_target(UpcomingRound_fixtures_raw, fetch_UpcomingRound_fixtures_raw(params), 
             cue = tar_cue(mode = "always")), # do not cache API forever
  
  # clean upcoming round data
  tar_target(UpcomingRound_ladder_clean, 
             clean_UpcomingRound_ladder(UpcomingRound_ladder_raw, params)
  ),
  tar_target(UpcomingRound_fixtures_clean_long, 
             clean_UpcomingRound_fixtures(UpcomingRound_fixtures_raw, params)
  ),
  tar_target(UpcomingRound_lineup_clean, 
             clean_UpcomingRound_lineups(UpcomingRound_lineups_raw, UpcomingRound_fixtures_clean_long, player_key)
  ),
  
  # build features data
  tar_target(model_data, build_model_data(
    historical_player_updated = historical_player_data,
    historical_team_updated = historical_team_data,
    historical_ladder_updated = historical_ladder_data,
    UpcomingRound_lineups = UpcomingRound_lineup_clean,
    params = params
  )),
  
  # update features data
  tar_target(feature_db, db_write(
    schema = "feat", 
    table = "Model Data", 
    df = model_data
      )
    ),
  
  # data split
  tar_target(data_split, split_data(model_data)),
  
  # fit model
  tar_target(model_fit, fit_model(data_split)),
  
  # make predictions
  tar_target(upcoming_predictions, make_predictions(data_split, model_fit)),
  
  tar_target(model_metadata, 
             build_model_metadata(params)),
  
  # fetch upcoming odds
  tar_target(odds_raw, odds_API_fetch(),
             cue = tar_cue(mode = "always")),
  
  # Build upcoming log outputs
  tar_target(upcoming_odds_log_data, pull_new_odds_log_data(upcoming_predictions, odds_raw)),
  tar_target(upcoming_bets_log_data, pull_new_bets_log_data(upcoming_predictions, odds_raw)),
  
  # update outputs
  tar_target(prediction_log_db, db_append(
    schema = "out", 
    table = "Prediction Log", 
    df = upcoming_predictions
    )
  ),
  tar_target(odds_log_db, db_append(
    schema = "out", 
    table = "Odds Log", 
    df = upcoming_odds_log_data
    )
  ),
  tar_target(bets_log_db, db_append(
    schema = "out", 
    table = "Betting Log", 
    df = upcoming_bets_log_data
    )
  )
)
