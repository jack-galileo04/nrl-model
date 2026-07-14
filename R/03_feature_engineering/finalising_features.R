build_TeamLevel_features <- function(basic_features) {
  TeamLevel_features <- basic_features |> 
    filter(n() < 2, .by = c(match_id,team_location)) |>
    mutate(
      across(ladder_points_diff:elo_short_vs_long, ~lag(.x)),
      .by = team_name
    ) |> 
    pivot_wider(
      id_cols = c(match_id, utc_start, round, season_stage),
      names_from = team_location,
      values_from = c(result, team_name:last_col()),
      names_sep = "_"
    ) |> 
    mutate(
      prhome = 1 / (1 + 10^( (elo_away - elo_home) / 400) ),
      result = case_when(
        result_home == 1 ~ "H",
        result_away == 1 ~ "A",
        T ~ NA
      )
    )  |> # ok to impute this statistics like this, NAs are typically zeroes or didn't actually get the stat
    mutate(across(
      where(is.numeric),
      ~ ifelse(is.finite(.), ., NA_real_)
    )) |> # removes infinities for model in case
    select(-result_home, -result_away) |> 
    mutate(
      across(c(ends_with("_mean_home") | ends_with("_vol_home"), ladder_points_diff_home, elo_home, elo_short_vs_long_home),
             ~ .-get(str_replace(cur_column(), "_home$", "_away")),
             .names = "{str_remove(.col,'_home$')}_diff"
      )
    ) |> 
    select(
      -c(ends_with("_mean_home"), 
         ends_with("_vol_home"), 
         ends_with("_mean_away"), 
         ends_with("_vol_away")),
      -elo_away,
      -elo_home,
      -elo_short_vs_long_away,
      -elo_short_vs_long_home
    ) |> 
    select(match_id, date = utc_start,
           round,result, home_team = team_name_home, away_team = team_name_away, everything()
    )
  
  return(TeamLevel_features)
}

select_final_features <- function(TeamLevel_features) {
  final_features <- readRDS(here("Outputs/final_features_list.rds"))
  
  features_data <- TeamLevel_features |> 
    select(
      match_id:season_stage,
      all_of(final_features)
    )
  
  return(features_data)
}

build_model_data <- function(historical_player_updated, historical_team_updated, historical_ladder_updated, UpcomingRound_lineups, params) {
  
  joined <- join_player_and_team_data(
    historical_player_updated, 
    historical_team_updated,
    historical_ladder_updated,
    UpcomingRound_lineups
  )
  message("Data Join Successful")
  
  normalised_stats <- build_normalised_stats(
    joined_player_and_team_data = joined, 
    params
  )
  message("Stat Normalisation Successful")
  
  summarised_stats <- build_summarised_stats(
    normalised_stats_data = normalised_stats, 
    params
  )
  message("Stat Summarisation Successful")
  
  opponent_adjusted_stats <- build_opponent_adjusted_stats(
    summarised_stats = summarised_stats, 
    params
  )
  message("Built Opponent Adjusted Stats")
  
  stats_features <- build_stats_features(
    opponent_adjusted_stats = opponent_adjusted_stats,
    params
  )
  message("Built Stat Based Features")
  
  TeamContext_features <- build_TeamContext_features(
    stats_features = stats_features,
    params
  )
  message("Built Context Based Features")
  
  basic_features <- build_elo_features(
    TeamContext_features = TeamContext_features,
    params
  )
  message("Built Elo Features")
  
  TeamLevel_features <- build_TeamLevel_features(
    basic_features = basic_features
  )
  message("Feature Space Successful")
  
  model_data <- select_final_features(TeamLevel_features)
  message("Selected Final Features")
  
  return(model_data)
} # wrapper function