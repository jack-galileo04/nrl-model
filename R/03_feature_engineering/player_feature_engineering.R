join_player_and_team_data <- function(historical_player_updated, historical_team_updated, historical_ladder_updated, UpcomingRound_lineups) {
  join_player_and_team_data <- historical_player_updated |> 
    unite("name", firstname:surname, sep = "_") |> # combine into one variable
    select(-competition_id, -season) |> 
    left_join(
      historical_team_updated |> 
        select(match_id, team_name, team_location,
               score:goal_line_dropouts),
      by = c("match_id", "team_name", "team_location")
    ) |> 
    mutate(
      season = year(utc_start),
      team_name = str_extract(team_name, "\\w+$")
    ) |> 
    left_join(
      historical_ladder_updated |> 
        rename(team_name = team) |> 
        mutate(team_name = case_when(
          team_name == "Wests Tigers" ~ "Tigers",
          team_name == "Sea Eagles" ~ "Eagles",
          T ~ team_name
        )) |> 
        slice_head(n = 1, by = c("team_name", "season", "round")) |> 
        select(team_name, season, round, ladder_points_diff = points_diff), 
      by = c("team_name", "season", "round")
    ) |> 
    select(player_id:team_location, ladder_points_diff, score, everything()) |> 
    arrange(utc_start, team_name, position)
  
  joined_data <- join_player_and_team_data |> 
    bind_rows(
      UpcomingRound_lineups |> 
        unite("name", firstname:surname, sep = "_") |> 
        select(any_of(colnames(join_player_and_team_data)))
    ) |> 
    distinct(player_id, match_id, .keep_all = TRUE) # no duplicate players
  
  return(joined_data)
}

build_normalised_stats <- function(joined_player_and_team_data, params) {
  joined_player_and_team_data |>   
    mutate(
      goal_attempts = conversion_attempts + penalty_goal_attempts,
      goals = conversions + penalty_goals
    ) |> # fixes goal kicking (features are only used for percentage)
    mutate(
      goal_attempts = ifelse(goal_attempts == 0, NA, goal_attempts),
      possessions = ifelse(possessions == 0, NA, possessions), 
      kicks_general_play = ifelse(kicks_general_play < 3, NA, kicks_general_play), 
      runs = ifelse(runs == 0, NA, runs), 
      runs_hitup = ifelse(runs_hitup == 0, NA, runs_hitup), 
      tackles = ifelse(tackles == 0, NA, tackles)
    ) |> # cleans normalisation variables (no diving by zero)
    mutate(
      post_contact_metres = ifelse(year(utc_start) <= 2020, NA, post_contact_metres),
      set_restarts = ifelse(year(utc_start) <= 2020, NA, set_restarts),
      across(time_in_own_half:time_in_opp20, ~ .x / 60)
    ) |> 
    select(
      -conversion_attempts, -penalty_goal_attempts, -conversions, - penalty_goals,
      -tackleds, -handling_errors, -runs_normal, -runs_normal_metres,
    ) |> # removes variables not used
    group_by(match_id, team_name) |> 
    mutate(
      team_lb = sum(line_breaks),
      team_tries = sum(tries),
      team_metres = sum(run_metres),
      team_pcm = sum(post_contact_metres),
      team_errors = sum(errors),
      team_penalties = sum(penalties_conceded),
      team_runs = sum(runs),
      team_possessions = sum(possessions)
    ) |> 
    group_by(match_id) |> 
    mutate(
      opponent_lb = rev(team_lb),
      opponent_tries = rev(team_tries),
      opponent_run_metres = rev(team_metres),
      opponent_pcm = rev(team_pcm),
      opponent_possessions = rev(team_possessions),
      opponent_runs = rev(team_runs),
      penalties = rev(team_penalties),
      opponent_errors = rev(team_errors),
      score_against = rev(score),
      outcome = case_when(
        score > score_against ~ 1,
        score < score_against ~ 0,
        T ~ 0.5
      ),
      forced_dropouts = rev(goal_line_dropouts)
    ) |> 
    ungroup() |> 
    left_join(
      params$position_minutes_weights, # estimate on minutes played in game based on position (not in data)
      by = "position"
    ) |> 
    transmute(
      player_id, name,team_name,position,utc_start,match_id,round,team_location,
      ladder_points_diff,
      score,
      score_against,
      outcome,
      completion_rate_percentage, 
      possession_percentage, 
      time_in_own_half, 
      time_in_opp_half, 
      time_in_own20,time_in_opp20,
      
      set_restarts_per_game = set_restarts,
      dropouts_per_game = goal_line_dropouts,
      lb_conceded_per_run = opponent_lb / opponent_runs,
      tries_conceded_per_game = opponent_tries,
      metres_conceded_per_run = opponent_run_metres / opponent_runs,
      pcm_conceded_per_run = opponent_pcm / opponent_runs,
      penalties_per_game = penalties,
      opponent_errors_per_game = opponent_errors,
      
      penalties_conceded_per_game = penalties_conceded,
      tries_per_game = tries,
      try_saves_per_game  = try_saves,
      errors_per_game = errors,
      forced_dropouts_per_game = forced_dropouts,
      
      pos_per_min = possessions * w,
      runs_per_min = runs * w,
      
      goals_per_attempt = goals / goal_attempts,
      
      km_per_kick = kick_metres / kicks_general_play,
      
      lba_per_touch = line_break_assists / possessions,
      ta_per_touch = try_assists / possessions,
      kicks_per_touch = kicks_general_play / possessions, 
      
      miss_per_tackle = missed_tackles / tackles,
      ineff_per_tackle = tackles_ineffective / tackles,
      
      pcm_per_run = post_contact_metres / runs,
      tb_per_run = tackle_breaks / runs,
      off_per_run = offloads / runs,
      lb_per_run = line_breaks / runs,
      metres_per_run = run_metres / runs
    ) |> 
    group_by(player_id) |> 
    mutate(
      appearances = row_number()
    ) |> 
    ungroup() |> 
    mutate(
      round = floor(match_id / 100),
      result = outcome
    ) |> 
    group_by(round, position) |> 
    mutate(
      across(score:appearances, ~scale(.x))
    ) |> 
    ungroup()
}

build_summarised_stats <- function(normalised_stats_data, params) {
  position_summarised_stats <- normalised_stats_data |> 
    summarise(
      across(c(utc_start, round, team_location), ~first(.x)),
      result = first(result),
      ladder_points_diff = first(ladder_points_diff),
      across(score:appearances, ~ mean(.x, na.rm = TRUE)),
      .by = c(match_id, position, team_name)
    )
  
  position_weights <- params$position_feature_weights
  
  summarised_stats <- position_summarised_stats |> 
    pivot_longer(
      score:appearances,
      names_to = "feature",
      values_to = "value"
    ) |> 
    left_join(position_weights, by = c("feature", "position")) |> 
    mutate(
      weight = replace_na(weight, 1 / nrow(params$position_minutes_weights)),
      weighted_value = value * weight
    ) |> 
    summarise(
      value = sum(weighted_value, na.rm = TRUE),
      .by = c(match_id, team_name, utc_start, round, team_location, result, ladder_points_diff, feature)
    ) |> 
    pivot_wider(
      names_from = feature,
      values_from = value
    )
  
  return(summarised_stats)
}