#################### feature calculation functions #################### 

compute_elo <- function(df, k) {
  
  elo.run(
    result ~ team_home + team_away,
    data = df,
    k = k
  ) |> 
    as_tibble() |> 
    bind_cols(df |> select(utc_start)) |> # select date from elo input
    arrange(desc(utc_start)) |> 
    select(team_home = team.A, team_away = team.B, elo_home = elo.A, elo_away = elo.B, utc_start) |> 
    pivot_longer(
      cols = team_home:elo_away,
      names_to = c(".value", "team_location"),
      names_sep = "_"
    ) |> 
    arrange(utc_start) |> 
    group_by(team) |> 
    mutate(elo = lag(elo),
           elo = replace_na(elo, 1500)) |> 
    ungroup()
}

# input vector x, and degree of weighting for rolling forms lambda. Get rolling forms output.
rolling_mean <- function(x, lambda) {
  purrr::accumulate(replace_na(x,0), 
                        ~ lambda*.x + (1-lambda)*.y, 
                    .init = 1
                    )[-1]
      
}

rolling_var <- function(x, lambda) {
  
  mu <- accumulate(
    x,
    ~ lambda * .x + (1-lambda) * .y,
    .init = 1
  )[-1]
  
    sqrt(
      purrr::accumulate(
        (x-mu)^2,
        ~lambda * .x + (1-lambda) * .y,
        .init = 0
      )[-1]
    )
}

#################### team level features #################### 

build_TeamContext_features <- function(historical_team_updated, historical_ladder_updated, UpcomingRound_fixtures_clean_long, UpcomingRound_ladder_clean) {
  
  historical_team_updated |> 
    bind_rows(UpcomingRound_fixtures_clean_long |> rename(team_name = team)) |> 
    group_by(competition_id, team_name) |> 
    arrange(team_name, utc_start) |> 
    mutate(
      days_rest = as.double(utc_start - lag(utc_start)),
      days_rest = ifelse(days_rest > 50, NA, days_rest),
      team_name = str_extract(team_name, "\\w+$"),
      season_stage = factor(case_when(
        round < 12 ~ "Early",
        round >= 12 & round <= 19 ~ "Mid",
        round > 19 ~ "Late"
      ), 
      levels = c("Early", "Mid", "Late"))
    ) |> 
    ungroup() |> 
    left_join(
      historical_ladder_updated |> 
        bind_rows(UpcomingRound_ladder_clean) |> 
        rename(team_name = team), 
      by = c("team_name", "season", "round")
    ) |> 
    select(-comp) |> 
    rename(
      ladder_points_for = points_for, 
      ladder_points_against = points_against, 
      ladder_points_diff = points_diff
    )
  
}

build_elo_input <- function(TeamContext_features) {
  
  TeamContext_features |> 
    select(match_id, utc_start, team_name, team_location, score) |> 
    rename(team = team_name) |> 
    pivot_wider(
      id_cols = c(match_id, utc_start),
      names_from = team_location,
      values_from = c(team, score)
    ) |> 
    mutate(
      result = case_when(
        score_home > score_away ~ 1,
        score_home < score_away ~ 0,
        score_home == score_away ~ 0.5,
        T ~ 0
      )) |> 
    select(match_id, utc_start, team_home, team_away, score_home, score_away, everything()) |>
    arrange(utc_start)
  
}

build_elo_features <- function(elo_input, params) {
  
  elo_model <- compute_elo(elo_input, k = params$elo_k)
  
  recent_elo_model <- compute_elo(elo_input, k = 10) |> 
    rename(shortelo = elo)
  
  long_elo_model <- compute_elo(elo_input, k = 40) |> 
    rename(longelo = elo)
  
  elo_input |> 
    select(utc_start, match_id, team_home, team_away) |>
    pivot_longer(
      cols = team_home:team_away,
      names_to = c(".value", "team_location"),
      names_sep = "_"
    ) |> 
    left_join(elo_model) |> 
    left_join(recent_elo_model) |> 
    left_join(long_elo_model)
}

combine_TeamLevel_features <- function(TeamContext_features, elo_features, params) {
  
  TeamContext_features |> 
    left_join(
      elo_features |> 
        select(match_id, team, elo, shortelo, longelo) |> 
        rename(team_name = team), 
      by = c("match_id", "team_name")
    ) |> 
    mutate(result = score) |> 
    arrange(utc_start) |> 
    group_by(team_name) |> 
    mutate(across(
      score:goal_line_dropouts,
      ~ rolling_lambda(., params$lambda) # uses rolling forms, lagged so that future stats are not leaked
    )) |> 
    mutate(across(
      ladder_points:ladder_position, # ensures future ladder is not leaked
      ~ lag(.x, n = 1)
    )) |> 
    ungroup()
  
}

build_TeamLevel_features <- function(historical_team_updated, historical_ladder_updated, UpcomingRound_fixtures_clean_long, UpcomingRound_ladder_clean, params) {
  
  TeamContext_features <- build_TeamContext_features(historical_team_updated, historical_ladder_updated, UpcomingRound_fixtures_clean_long, UpcomingRound_ladder_clean)
  
  elo_input <- build_elo_input(TeamContext_features)
  
  elo_features <- build_elo_features(elo_input, params)
  
  TeamLevel_features <- combine_TeamLevel_features(TeamContext_features, elo_features, params)
  
  message("build_TeamLevel_features:")
  print(class(TeamLevel_features$utc_start))
  
  return(TeamLevel_features)
}


#################### player level features #################### 

build_player_statistics <- function(historical_player_updated, UpcomingRound_lineup_clean, params) {
  
  historical_player_updated |> 
    bind_rows(UpcomingRound_lineup_clean) |> 
    unite("name", firstname:surname, sep = "_") |> # combine into one variable
    mutate(
      possessions = ifelse(possessions == 0, NA, possessions), # helps with division (per possession), immaterial to model as well
      goals_per_kick = (conversions+penalty_goals) / (conversion_attempts+penalty_goal_attempts)
    ) |> 
    select(
      -conversions, 
      -conversion_attempts, 
      -penalty_goals, 
      -penalty_goal_attempts,
      -runs_hitup,
      -runs_normal, 
      -runs_hitup_metres, 
      -runs_normal_metres, 
      -tackleds, 
      -handling_errors, 
      -post_contact_metres
    ) |> # lots of noisy stats that are similar to other stats
    left_join(
      params$position_minutes_weights, # estimate on minutes played in game based on position (not in data)
      by = "position"
    ) |> 
    mutate(
      m_per_run=run_metres/runs,
      tb_per_run=tackle_breaks/runs,
      lb_per_run=line_breaks/runs,
      offs_per_run=offloads/runs, # Attacking Runs
      
      ta_per_touch=try_assists/possessions,
      lba_per_touch=line_break_assists/possessions,
      kick_per_touch=kicks_general_play/possessions,
      pass_per_touch=passes/possessions,
      km_per_kick=ifelse(kicks_general_play ==0, NA, kick_metres/kicks_general_play), # Attacking Play-making
      
      tries_per_game=tries,
      err_per_touch=errors/possessions,
      points_per_game=points, # Attacking Overall
      
      tackles_time=tackles/w,
      saves_per_game=try_saves, # Defensive Work
      
      miss_per_t=missed_tackles/tackles,
      pen_per_t=penalties_conceded/tackles,
      ineff_per_t=tackles_ineffective/tackles # Defensive Discipline
    ) |> 
    select(-(points:penalties_conceded)) |> # We have scaled versions of these
    group_by(player_id) |> 
    arrange(player_id, utc_start) |> 
    select(
      utc_start, 
      name, 
      player_id, 
      team_name, 
      position, 
      match_id, 
      competition_id, 
      season, 
      round,
      team_location, 
      w,
      everything()
    ) |> 
    mutate(across(
      goals_per_kick:ineff_per_t,
      ~ rolling_lambda(., params$lambda) # uses rolling forms, lagged so that future stats are not leaked
    )) |> 
    ungroup()
  
}

combine_player_features <- function(player_statistics) {
  
  player_statistics |> 
    group_by(match_id, team_name) |> 
    summarise(
      utc_start = first(utc_start),
      comp_id = first(competition_id),
      season = first(season),
      round = first(round),
      team_location = first(team_location),
      across(goals_per_kick:ineff_per_t, ~ mean(.x, na.rm = TRUE)) # Averaging statistics across team, may change to weighted averages later
    ) |> 
    ungroup() |> 
    mutate(team_name = str_extract(team_name, "\\w+$")) |> 
    mutate(across(
      where(is.numeric),
      ~ifelse(.x == Inf, NA, .x)
      ))
  
}

build_player_features <- function(historical_player_updated, UpcomingRound_lineup_clean, params) {
  
  player_statistics <- build_player_statistics(historical_player_updated, UpcomingRound_lineup_clean, params)
  
  PlayerLevel_features <- combine_player_features(player_statistics)
  
  message("build_player_features:")
  print(class(PlayerLevel_features$utc_start))
  
  return(PlayerLevel_features)
  
}

#################### combine features ####################

build_features_data <- function(PlayerLevel_features, TeamLevel_features) {
  
PlayerLevel_features |> 
    left_join(
      TeamLevel_features |> 
        select(-utc_start, -team_location, -competition_id, -round, -season), 
      by = c("match_id", "team_name")) |> 
    select(result, match_id, utc_start, round, season_stage, everything()) |> 
    pivot_wider(
      id_cols = c(match_id, utc_start, round, season_stage),
      names_from = team_location,
      values_from = c(result, team_name, goals_per_kick:longelo),
      names_sep = "_"
    ) |> 
    mutate(
      result = factor(case_when(
        result_home > result_away ~ "H",
        result_away > result_home ~ "A",
        T ~ NA))
    ) |> # making target variable (binary classification)
    select(result, everything()) |> # making sure not included in across functions
    rename(date = utc_start) |> # clearer name
    rename(
      home_team = team_name_home, 
      away_team = team_name_away,
      away_result = result_away, 
      home_result = result_home
    ) |> # cleaning names and also avoiding below function
    mutate(
      across(ends_with("_home"),
             ~ .-get(str_replace(cur_column(), "_home$", "_away")),
             .names = "{str_remove(.col,'_home$')}_diff"
      )
    ) |> # This function takes the difference between _home and _away variables, creating _diff variables.
    mutate(
      prhome = 1 / (1 + 10^( (elo_away - elo_home) / 400) ),
      shortprhome = 1 / (1 + 10^( (shortelo_away - shortelo_home) / 400) ),
      longprhome = 1 / (1 + 10^( (longelo_away - longelo_home) / 400) )
    ) |> # pr of winning based on elo ratings
    mutate(across(
      goals_per_kick_away:ladder_position_diff,
      ~ replace_na(.x, 0)
    )) |> # ok to impute this statistics like this, NAs are typically zeroes or didn't actually get the stat
    mutate(across(
      where(is.numeric),
      ~ ifelse(is.finite(.), ., NA_real_)
    )) # figure out cause of this, only 1 or 2 in a few columns
  
}

