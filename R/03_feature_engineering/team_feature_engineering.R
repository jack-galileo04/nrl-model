build_opponent_adjusted_stats <- function(summarised_stats, params) {
  team_historical_avgs <- summarised_stats |> 
    group_by(team_name) |> 
    arrange(utc_start, .by_group = TRUE) |> 
    transmute(
      match_id,
      avg_metres_conceded_per_run = rolling_mean(metres_conceded_per_run, params$lambda) |> lag(),
      avg_metres_per_run = rolling_mean(metres_per_run, params$lambda) |> lag(),
      avg_lb_conceded_per_run = rolling_mean(lb_conceded_per_run, params$lambda) |> lag(),
      avg_lb_per_run = rolling_mean(lb_per_run, params$lambda) |> lag(),
      avg_tries_conceded_per_game = rolling_mean(tries_conceded_per_game, params$lambda) |> lag(),
      avg_tries_per_game = rolling_mean(tries_per_game, params$lambda) |> lag()
    )
  
  opponent_adjusted_stats <- summarised_stats |> 
    group_by(match_id) |> 
    mutate(opponent = rev(team_name)) |> 
    left_join(
      team_historical_avgs |> rename(opponent = team_name),
      by = c("match_id", "opponent")
    ) |> 
    mutate(
      across(avg_metres_conceded_per_run:avg_tries_per_game, ~replace_na(.x, 0)),
      adj_metres_per_run = metres_per_run - avg_metres_conceded_per_run,
      adj_metres_conceded_per_run = metres_conceded_per_run - avg_metres_per_run,
      adj_lb_per_run = lb_per_run - avg_lb_conceded_per_run,
      adj_lb_conceded_per_run = lb_conceded_per_run - avg_lb_per_run,
      adj_tries_per_game = tries_per_game - avg_tries_conceded_per_game,
      adj_tries_conceded_per_game = tries_conceded_per_game - avg_tries_per_game
    ) |> 
    select(
      match_id:appearances,
      adj_metres_per_run:adj_tries_conceded_per_game
    )
  
  return(opponent_adjusted_stats)
}

build_stats_features <- function(opponent_adjusted_stats, params) {
  
  stats_features <- opponent_adjusted_stats |> 
    group_by(team_name) |> 
    arrange(utc_start) |> 
    mutate(
      across(
        score:adj_tries_conceded_per_game, 
        list(
          mean = ~rolling_mean(.x, params$lambda),
          vol = ~rolling_var(.x, params$lambda)
        ),
        .names = "{.col}_{.fn}"
      )
    ) |> 
    select(-(score:adj_tries_conceded_per_game)) |> 
    ungroup()
  
  return(stats_features)
}

build_TeamContext_features <- function(stats_features, params) {
  TeamContext_features <- stats_features |> 
    group_by(team_name) |> 
    arrange(utc_start) |> 
    mutate(
      round = round %% 100,
      days_rest = as.double(utc_start - lag(utc_start)),
      days_rest = ifelse(days_rest > 50, NA, days_rest),
      season_stage = factor(case_when(
        round < 12 ~ "Early",
        round >= 12 & round <= 19 ~ "Mid",
        round > 19 ~ "Late"), 
        levels = c("Early", "Mid", "Late")
      ),
      form = rolling_mean(result, params$lambda),
    ) |> 
    ungroup()
  
  return(TeamContext_features)
} 

compute_elo <- function(df, k) {
  elo.run(
    result ~ team_home + team_away,
    data = df,
    k = k
  ) |> 
    as_tibble() |> 
    bind_cols(
      df |> 
        drop_na(team_home, team_away) |> 
        select(utc_start),
    ) |> # select date from elo input
    arrange(desc(utc_start)) |> 
    select(team_home = team.A, team_away = team.B, elo_home = elo.A, elo_away = elo.B, utc_start) |> 
    pivot_longer(
      cols = team_home:elo_away,
      names_to = c(".value", "team_location"),
      names_sep = "_"
    ) |> 
    arrange(utc_start) |> 
    group_by(team) |> 
    mutate(elo = replace_na(elo, 1500)) |> 
    ungroup()
}

build_elo_features <- function(TeamContext_features, params) {
  elo_input <- TeamContext_features |> 
    select(match_id, utc_start, team_name, team_location, result) |> 
    rename(team = team_name) |> 
    filter(n() < 2, .by = c(match_id, team)) |> 
    pivot_wider(
      names_from = team_location,
      values_from = c(team, result)
    ) |>    
    transmute(
      match_id, utc_start, team_home, team_away,
      result = case_when(
        result_home == 1 ~ 1,
        result_home == 0 ~ 0,
        result_home == 0.5 ~ 0.5
      )
    ) |> 
    select(match_id, utc_start, team_home, team_away, result, everything()) |>
    arrange(utc_start)
  
  elo_model <- compute_elo(elo_input, k = params$elo_k)
  
  recent_elo_model <- compute_elo(elo_input, k = 5) |> 
    rename(shortelo = elo)
  
  long_elo_model <- compute_elo(elo_input, k = 40) |> 
    rename(longelo = elo)
  
  elo_features <- elo_input |> 
    select(utc_start, match_id, team_home, team_away) |>
    pivot_longer(
      cols = team_home:team_away,
      names_to = c(".value", "team_location"),
      names_sep = "_"
    ) |> 
    left_join(elo_model) |> 
    left_join(recent_elo_model) |> 
    left_join(long_elo_model) |> 
    mutate(
      elo_short_vs_long = shortelo - longelo
    ) |> 
    select(-c(shortelo, longelo))
  
  TeamContext_features |> 
    left_join(
      elo_features |> 
        select(match_id, team, elo, elo_short_vs_long) |> 
        rename(team_name = team), 
      by = c("match_id", "team_name")
    ) |> 
    select(match_id, utc_start, round, season_stage, result, team_location, team_name, everything())
}