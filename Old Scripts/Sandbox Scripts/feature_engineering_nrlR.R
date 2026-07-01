## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

r_files <- list.files(
  path = here::here("R"),
  pattern = "\\.R$",
  full.names = TRUE
)

invisible(lapply(r_files, source))


## ---------------------------------------------------------------------------------------------------------------------------------------------------
params <- read_params()

historical_ladder_updated <- read_csv(here("Data/01_Clean/Ladder Data.csv"))
historical_team_updated <- read_csv(here("Data/01_Clean/Team Data.csv"))
historical_player_updated <- read_csv(here("Data/01_Clean/Player Data.csv"))


## ----team functions---------------------------------------------------------------------------------------------------------------------------------
TeamContext_features <- historical_team_updated |> 
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
    historical_ladder_updated |> rename(team_name = team), 
    by = c("team_name", "season", "round")
    ) |> 
  select(-comp) |> 
  rename(
    ladder_points_for = points_for, 
    ladder_points_against = points_against, 
    ladder_points_diff = points_diff
    )
  
elo_input <- TeamContext_features |> 
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


## ---------------------------------------------------------------------------------------------------------------------------------------------------
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
    left_join(long_elo_model)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
TeamLevel_features <- TeamContext_features |> 
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


## ----player functions-------------------------------------------------------------------------------------------------------------------------------

player_statistics <- historical_player_updated |> 
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
  
PlayerLevel_features <-  player_statistics |> 
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
  mutate(across(everything(), ~ifelse(.x == Inf, NA, .x)))


## ----features functions-----------------------------------------------------------------------------------------------------------------------------

features_data <-  PlayerLevel_features |> 
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
        T ~ "NA"))
        ) |> # making target variable (binary classification)
    rename(
      date = utc_start, # clearer name
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


## ---------------------------------------------------------------------------------------------------------------------------------------------------
dbWriteTable(
  con,
  Id(schema = "feat", table = "team_feature_engineered_df"),
  TeamLevel_features,
  overwrite = TRUE
)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
knitr::purl("feature_engineering_nrlR.Rmd", output = "feature_engineering_nrlR.R")

