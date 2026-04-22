## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(here)
library(readxl)
library(tidyverse)
library(fuzzyjoin)
library(tidymodels)
library(vip)
library(rsample)

# Initialisation

# Current Round Info
season <- 2026
round <- 8
round_number <- 8
num_games <- 8 # Temporary

comps <- fetch_cd_comps() |> 
  filter(str_detect(name, "NRL Finals") | str_detect(name, "NRL Premiership")) |> 
  slice_tail(n = -4)
comp_ids <- comps$id

comp <- as.numeric(comps[comps$season == season,][,"id"])

# Lineups Info
lineup_tuesday <- gsub("-", "/", floor_date(Sys.Date(), unit = "weeks", week_start = 2))
lineup_url <- paste0("https://www.nrl.com/news/", lineup_tuesday, "/nrl-team-lists-round-", round, "/")

# Number of matches for form
n <- 5 

# Rolling weighted average parameter
lambda <- 0.94

# Elo Parameter
k <- 22

# Position Based Involvements - game time minutes
position_minutes_weights <- tribble(
  ~position, ~w,
  "Fullback", 1,
  "Wing", 1,
  "Centre", 1,
  "Five-Eighth", 1,
  "Halfback", 1,
  "Prop", 0.6,
  "Hooker", 0.8,
  "Second Row", 0.75,
  "Lock", 0.75,
  "Interchange", 0.4
)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Historical Data

ladder_df <- read_csv(here("Data/Raw/Ladder Data.csv"), col_select = -1) |> 
  mutate(team = ifelse(team == "Wests Tigers", "Tigers", team),
         team = ifelse(team == "Sea Eagles", "Eagles", team))
player_df <- read_csv(here("Data/Raw/Player Data.csv"), col_select = -1) |> 
  mutate(utc_start = as_date(utc_start))
team_df <- read_csv(here("Data/Raw/Team Data.csv"), col_select = -1) |> 
  mutate(utc_start = as_date(utc_start))

player_key <- read_csv(here("Data/Raw/Player Key.csv"), col_select = -1)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Fetch Previous Round Data

prev_ladder_df <- fetch_ladder_nrl(season = season, round_number = round-1) |> 
      select(team, ladder_points = points, points_for, points_against, points_diff) |> 
      mutate(season = season, round = round-1, ladder_position = row_number()) |> 
  mutate(team = ifelse(team == "Wests Tigers", "Tigers", team),
         team = ifelse(team == "Sea Eagles", "Eagles", team))

prev_player_df <- fetch_player_stats(
  season = season, 
  round = round-1,
  league = "nrl", 
  source = "championdata", 
  comp = comp) |> 
  mutate(season = season) |> 
  select(
    season,
    playerId, firstname, surname, team_name, squadId,position,jumperNumber,
    utc_start, match_id, competition_id, round, team_location,
    points, tries, conversionAttempts, conversions, penaltyGoalAttempts,penaltyGoals,
    tryAssists,lineBreakAssists,passes,kickMetres,kicksGeneralPlay,
    possessions,runs,runsHitup,runsNormal,runMetres,runsHitupMetres,runsNormalMetres,postContactMetres,
    offloads,lineBreaks,tackleBreaks,tackleds,
    tackles,tacklesIneffective,missedTackles,trySaves,
    handlingErrors,errors,penaltiesConceded
  ) |> 
  unnest(competition_id) |> 
  rename(competition_id = id) |> 
  janitor::clean_names() |> 
  mutate(utc_start = as_date(utc_start))

prev_team_df <- fetch_team_stats_championdata(
  round = round-1,
  comp = comp) |> 
  select(
    competition_id,match_id,round,team_name,team_location,utc_start,
    score,completionRatePercentage,possessionPercentage,
    timeInOwnHalf,timeInOppHalf,timeInOwn20,timeInOpp20,
    setRestarts,
    goalLineDropouts
  ) |> 
  unnest(competition_id) |> 
  rename(competition_id = id) |> 
  janitor::clean_names() |> 
  group_by(match_id, team_name) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  mutate(utc_start = as_date(utc_start))



## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Append Historical Data with Previous Round Data

ladder_df <- bind_rows(ladder_df, prev_ladder_df)
player_df <- bind_rows(player_df, prev_player_df)
team_df <- bind_rows(team_df, prev_team_df)

write.csv(ladder_df, here("Data/Raw/Ladder Data.csv"))
write.csv(player_df, here("Data/Raw/Player Data.csv"))
write.csv(team_df, here("Data/Raw/Team Data.csv"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Upcoming Round Fixtures

fixture_df <- fetch_results(seasons = season,league = "nrl", source = "rugbyleagueproject") |> 
    mutate(
      across(
        c(home_team, away_team),
        ~ case_when(
          .x == "Brisbane" ~ "Brisbane Broncos",
          .x == "Newcastle" ~ "Newcastle Knights",
          .x == "Melbourne" ~ "Melbourne Storm",
          .x == "Cronulla" ~ "Cronulla-Sutherland Sharks",
          .x == "Manly" ~ "Manly-Warringah Sea Eagles",
          .x == "North Qld" ~ "North Queensland Cowboys",
          .x == "South Sydney" ~ "South Sydney Rabbitohs",
          .x == "Sydney" ~ "Sydney Roosters",
          .x == "Penrith" ~ "Penrith Panthers",
          .x == "Parramatta" ~ "Parramatta Eels",
          .x == "St Geo Illa" ~ "St George-Illawarra Dragons",
          .x == "Gold Coast" ~ "Gold Coast Titans",
          .x == "Canberra" ~ "Canberra Raiders",
          .x == "Canterbury" ~ "Canterbury-Bankstown Bulldogs",
          T ~ .x
        )
        )
      ) |> 
  filter(round == round_number+1) |> 
  arrange(date) |> 
  mutate(match_id = (comp*10000L+round_number*100L) + row_number()) |> # This is the method of indexation being used
  select(match_id, round, home = home_team, away = away_team, utc_start = date, season) |> 
  mutate(round = round_number) |> # For some reason this fetches a round number ahead
  pivot_longer(
    cols = home:away,
    values_to = "team_name",
    names_to = "team_location"
  ) |> 
  select(match_id, round, team_name, team_location, utc_start)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Upcoming Round Lineup

match <- fetch_lineups(url = lineup_url, source = "nrl.com", type = "team_list") |> 
  rename(firstname = first_name, surname = last_name,
         position = role) |> 
  mutate(team = ifelse(team == "Wests Tigers", "Tigers", team),
         team = ifelse(team == "Sea Eagles", "Eagles", team),
         position = case_when(
           position == "Winger" ~ "Wing",
           position == "2nd" ~ "Second Row",
           T ~ position
         )) |> 
  filter(position != "Reserve") |> 
  left_join(player_key, by = c("firstname", "surname"))

unmatched <- match |> 
  filter(is.na(player_id)) |> 
  select(-player_id) |> 
  mutate(
    firstname = case_when(
      firstname == "KL" ~ "Kayal",
      firstname == "Sebastian" ~ "Seb",
      firstname == "Leka" ~ "Selumiela",
      T ~ firstname),
    surname = case_when(
      surname == "Vaimauga" ~ "Sifakula",
      T ~ surname)
    )

fuzzy <- stringdist_left_join( # KL Iro -> Kayal, Sebastian Kris -> Seb, Leka Halasima -> Selumiela, Demitric Vaimauga -> Sifakula
  unmatched,
  player_key,
  by = c("firstname", "surname"),
  method = "jw",
  max_dist = 0.22
) |> 
  rename(firstname = firstname.x, surname = surname.x) |> 
  select(-firstname.y, -surname.y)

lineup_df <- match |> 
  drop_na(player_id) |> 
  bind_rows(fuzzy) |> 
  select(player_id, firstname, surname, team, position) |> 
  left_join(
    fixture_df |> 
      mutate(team = str_extract(team_name, "\\w+$")) |> 
      select(team, match_id, round, team_location, utc_start),
    by = c("team")
    ) |> 
  rename(team_name = team)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Current Ladder

current_ladder_df <- fetch_ladder_nrl(season = season, round_number = round) |> 
      select(team, ladder_points = points, points_for, points_against, points_diff) |> 
      mutate(season = season, round = round, ladder_position = row_number()) |> 
  mutate(team = ifelse(team == "Wests Tigers", "Tigers", team),
         team = ifelse(team == "Sea Eagles", "Eagles", team))



## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Days Rest

team_and_recovery_df <- team_df |> 
  bind_rows(fixture_df) |> 
  select(competition_id, match_id, round, utc_start, team_name, team_location, score) |> 
  group_by(competition_id, team_name) |> 
  arrange(team_name, utc_start) |> 
  mutate(days_rest = as.numeric(utc_start - lag(utc_start))) |> 
  ungroup() |> 
  select(days_rest) |> 
  bind_cols(team_df |> bind_rows(fixture_df)) |> 
  mutate(team = str_extract(team_name, "\\w+$")) |> 
  mutate(season = year(utc_start)) |> 
  left_join(ladder_df |> bind_rows(current_ladder_df), by = c("team", "season", "round")) |> 
  select(-team, -season) |> 
  rename(ladder_points_for = points_for, ladder_points_against = points_against, ladder_points_diff = points_diff) |> 
  group_by(team_name, utc_start, round) |> # Fix Join Error
  slice_head(n = 1) |> 
  ungroup() |> 
  arrange(desc(utc_start))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Elos

elo_input <- team_and_recovery_df |> 
  select(match_id, round, utc_start, team_name, team_location, score) |> 
  rename(team = team_name) |> 
  pivot_wider(
    names_from = team_location,
    values_from = c(team, score)
  ) |> 
  unnest(team_home, team_away, score_home, score_away) |> 
  mutate(
    result = case_when(
    score_home > score_away ~ 1,
    score_home < score_away ~ 0,
    T ~ 0.5
  )) |> 
  arrange(utc_start)

elo_model <- elo.run(result ~ team_home + team_away, data = elo_input, k = min_k) |> 
  as_tibble() |> 
  mutate(elo_home = elo.A-update.A, elo_away = elo.B - update.B) |> # This ensures no data leakage
  select(elo_home, elo_away, pr_home = p.A)

recent_elo_model <- elo.run(result ~ team_home + team_away, data = elo_input, k = 10) |> 
  as_tibble() |> 
  mutate(shortelo_home = elo.A-update.A, shortelo_away = elo.B - update.B) |> # This ensures no data leakage
  select(shortelo_home, shortelo_away, shortpr_home = p.A)

long_elo_model <- elo.run(result ~ team_home + team_away, data = elo_input, k = 40) |> 
  as_tibble() |> 
  mutate(longelo_home = elo.A-update.A, longelo_away = elo.B - update.B) |> # This ensures no data leakage
  select(longelo_home, longelo_away, longpr_home = p.A)

elo_df <- elo_input |> 
  select(utc_start, team_home, team_away, match_id) |> 
  bind_cols(elo_model, recent_elo_model, long_elo_model) |> 
  arrange(desc(utc_start)) |> 
  pivot_longer(
    cols = c(team_home, team_away, elo_home, elo_away, shortelo_home, shortelo_away, longelo_home, longelo_away),
    names_to = c(".value", "team_location"),
    names_sep = "_"
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Combining team data into lagged features

team_recovery_elo_df <- team_and_recovery_df |> 
  left_join(elo_df |> select(2:10) |> rename(team_name = team), by = c("match_id", "team_name")) |> 
  mutate(result = score) |> 
  arrange(utc_start) |> 
  group_by(team_name) |> 
  mutate(across(
    score:goal_line_dropouts,
    .fns = ~ lag(accumulate(., ~lambda*.x + (1-lambda)*.y ), n=1)
    )) |> 
  mutate(across(
    ladder_points:ladder_position,
    .fns = ~lag(.x, n = 1)
  )) |> 
  select(-team_location.y) |> 
  rename(team_location = team_location.x) |> 
  ungroup()


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Scaled form statistics

player_forms_df <- player_df |> 
  bind_rows(lineup_df) |> 
  unite("name", firstname:surname, sep = "_") |> 
  mutate(conversion_attempts = ifelse(conversion_attempts == 0, NA, conversion_attempts),
         penalty_goal_attempts = ifelse(penalty_goal_attempts == 0, NA, penalty_goal_attempts),
         possessions = ifelse(possessions == 0, NA, possessions)) |> 
  mutate(goal_kick_ratio = (conversions+penalty_goals) / (conversion_attempts+penalty_goal_attempts)) |> 
  select(-conversions, -conversion_attempts, -penalty_goals, -penalty_goal_attempts,
         -runs_hitup, -runs_normal, -runs_hitup_metres, -runs_normal_metres, -tackleds, -handling_errors, -post_contact_metres) |> 
  left_join(position_minutes_weights, by = "position") |> 
  mutate(
    m_per_run=run_metres/runs,tb_per_run=tackle_breaks/runs,lb_per_run=line_breaks/runs,offs_per_run=offloads/runs, # Attacking Runs
    ta_per_touch=try_assists/possessions,lba_per_touch=line_break_assists/possessions,kick_per_touch=kicks_general_play/possessions,pass_per_touch=passes/possessions,
      km_per_kick=ifelse(kicks_general_play ==0, NA, kick_metres/kicks_general_play),goals_per_kick=goal_kick_ratio, # Attacking Play-making
    tries_per_game=tries,err_per_touch=errors/possessions,points_per_game=points, # Attacking Overall
    tackles_time=tackles/w,saves_per_game=try_saves, # Defensive Work
    miss_per_t=missed_tackles/tackles,pen_per_t=penalties_conceded/tackles,ineff_per_t=tackles_ineffective/tackles # Defensive Discipline
  ) |> 
  select(-goal_kick_ratio,-(12:30)) |> 
  group_by(player_id) |> 
  arrange(player_id, utc_start) |> 
  select(utc_start, name, player_id, team_name, jumper_number, position, squad_id, match_id, competition_id, season, round,
         team_location, everything()) |> 
  mutate(across(
    13:30,
    .fns = ~ lag(accumulate(., ~lambda*.x + (1-lambda)*.y ), n=1)
    )) |> 
  ungroup() |> 
  group_by(competition_id, round, position) |> 
  mutate(across(
    11:28,
    .fns = ~scale(.x)
  )) |> 
  ungroup()


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
model_data <- player_forms_df |> 
  group_by(match_id, team_name) |> 
  summarise(
    date = first(utc_start),
    squad_id = first(squad_id),
    comp_id = first(competition_id),
    season = first(season),
    round = first(round),
    team_location = first(team_location),
    across(m_per_run:ineff_per_t, ~ mean(.x, na.rm = TRUE))
    ) |> 
  ungroup() |> 
  left_join(team_recovery_elo_df |> select(-utc_start, -team_location, -competition_id, -round), by = c("match_id", "team_name")) |> 
  select(result, everything()) |> 
  pivot_wider(
    id_cols = c(match_id, date),
    names_from = team_location,
    values_from = c(result, team_name, m_per_run:longelo),
    names_sep = "_"
  ) |> 
  mutate(
    result = factor(case_when(
      result_home > result_away ~ "H",
      result_away > result_home ~ "A",
      T ~ "A" # Not multiclass
    ))
    ) |> 
  mutate(
    days_rest_home = ifelse(is.na(days_rest_home), 0, days_rest_home),
    days_rest_away = ifelse(is.na(days_rest_away), 0, days_rest_away)
  ) |> 
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) |> 
  arrange(date) |> 
  select(-pr_home_away, -shortpr_home_away, -longpr_home_away,
         -elo_away, -elo_home, -shortelo_away, -shortelo_home, -longelo_away, -longelo_home) |> 
  rename(prhome = pr_home_home, shortprhome = shortpr_home_home, longprhome = longpr_home_home,
         away_result = result_away, home_result = result_home, away_team = team_name_away, home_team = team_name_home) |> 
  mutate(
    across(ends_with("_home"),
           ~ .-get(str_replace(cur_column(), "_home$", "_away")),
           .names = "{str_remove(.col,'_home$')}_diff"
           )
  ) |> 
  select(-ends_with("_home"), -ends_with("_away"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Data split

cutoff <- floor_date(Sys.Date(), unit = "weeks", week_start = 2)

train_data <- model_data |>  filter(date < cutoff)
pred_data  <- model_data |>  filter(date >= cutoff)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(234)

final_wf <- readRDS(here("Outputs/modelling_pipeline_workflow.rds"))
final_fit <- fit(final_wf, data = train_data)

predictions_df <- read_csv(here("Outputs/prediction_log.csv"), col_select = -1)

predictions <- bind_rows(
  predictions_df,
  predict(final_fit, new_data = pred_data, type = "prob") |> 
  bind_cols(pred_data) |> 
  select(1:4, away_team, home_team) |> 
  mutate(pred = ifelse(.pred_H > .pred_A, home_team, away_team))
) 

write.csv(predictions, here("Outputs/prediction_log.csv"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::purl("pipeline_nrlR.Rmd", output = "pipeline_nrlR.R")

