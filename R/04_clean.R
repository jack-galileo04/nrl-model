#################### clean previous round results #################### 

clean_PreviousRound_ladder <- function(PreviousRound_ladder_raw, historical_ladder, params) {
  
  PreviousRound_ladder_raw |> 
    janitor::clean_names() |> # cleans names if format changes
    rename(ladder_points = points) |> # clearer name
    arrange(
      desc(ladder_points), 
      desc(points_diff), 
      desc(points_for)
    ) |> # nrl ladder order
    mutate(
      season = params$season,
      round = params$round_number - 1,
      ladder_position = row_number(), # after arrangin in ladder order, row_number is the position
      team = case_when(
        team == "Wests Tigers" ~ "Tigers",
        team == "Sea Eagles" ~ "Eagles",
        T ~ team)
    ) |> 
    select(colnames(historical_ladder)) # select columns from historical data
  
}

clean_PreviousRound_player <- function(PreviousRound_player_raw, historical_player, params) {
  
  PreviousRound_player_raw |> 
    mutate(
      season = params$season,
      utc_start = as_datetime(utc_start)
    ) |> 
    janitor::clean_names() |>  # cleans names if format changes
    select(colnames(historical_player)) # select columns from historical data
  
}

clean_PreviousRound_team <- function(PreviousRound_team_raw, historical_team, params) {
  
  PreviousRound_team_raw |> 
    mutate(
      season = params$season,
      utc_start = as_datetime(utc_start)
    ) |> 
    janitor::clean_names() |>
    select(colnames(historical_team))
  
}

#################### clean upcoming round data #################### 


# upcoming ladder
clean_UpcomingRound_ladder <- function(UpcomingRound_ladder_raw, historical_ladder, params) {
  
  UpcomingRound_ladder_raw |> 
    janitor::clean_names() |> # cleans names if format changes
    rename(ladder_points = points) |> # clearer name
    arrange(
      desc(ladder_points), 
      desc(points_diff), 
      desc(points_for)
    ) |> # nrl ladder order
    mutate(
      season = params$season,
      round = params$round_number,
      ladder_position = row_number(), # after arrangin in ladder order, row_number is the position
      team = case_when(
        team == "Wests Tigers" ~ "Tigers",
        team == "Sea Eagles" ~ "Eagles",
        T ~ team)
    ) |> 
    select(colnames(historical_ladder)) # select columns from historical data
}

# upcoming fixtures
map_city_to_team_name <- function(team_name) {
  case_when(
    team_name == "Brisbane" ~ "Brisbane Broncos",
    team_name == "Newcastle" ~ "Newcastle Knights",
    team_name == "Melbourne" ~ "Melbourne Storm",
    team_name == "Cronulla" ~ "Cronulla-Sutherland Sharks",
    team_name == "Manly" ~ "Manly-Warringah Sea Eagles",
    team_name == "North Qld" ~ "North Queensland Cowboys",
    team_name == "South Sydney" ~ "South Sydney Rabbitohs",
    team_name == "Sydney" ~ "Sydney Roosters",
    team_name == "Penrith" ~ "Penrith Panthers",
    team_name == "Parramatta" ~ "Parramatta Eels",
    team_name == "St Geo Illa" ~ "St George-Illawarra Dragons",
    team_name == "Gold Coast" ~ "Gold Coast Titans",
    team_name == "Canberra" ~ "Canberra Raiders",
    team_name == "Canterbury" ~ "Canterbury-Bankstown Bulldogs",
    TRUE ~ team_name)
}

clean_UpcomingRound_fixtures <- function(UpcomingRound_fixtures_raw, params) {
  
  UpcomingRound_fixtures_clean_wide <- UpcomingRound_fixtures_raw |> 
    mutate(across(
      c(home_team, away_team),
      ~map_city_to_team_name(.x)
    )) |> # keeping consistent formatting
    arrange(date) |> 
    mutate(
      match_id = (params$comp*10000L+params$round_number*100L) + row_number(),
      time_clean = str_remove(time, "^[A-Za-z]{3}\\s+"),
      utc_start = ymd_hm(paste(date, time)) 
    ) |> # keeping consistent formatting
    select(match_id, round, home = home_team, away = away_team, utc_start, season)
  
  UpcomingRound_fixtures_clean_wide |> 
    pivot_longer(
      home:away,
      values_to = "team_name",
      names_to = "team_location"
    ) |> 
    mutate(team = str_extract(team_name, "\\w+$")) |> 
    select(team, match_id, round, season, team_location, utc_start)
  
}

####################  upcoming lineups #################### 

clean_player_firstname <- function(firstname) {
  
  case_when(
    firstname == "KL" ~ "Kayal",
    firstname == "Sebastian" ~ "Seb",
    firstname == "Leka" ~ "Selumiela",
    T ~ firstname
  )
}
clean_player_surname <- function(surname) {
  
  case_when(
    surname == "Vaimauga" ~ "Sifakula",
    T ~ surname
  )
}

build_UpcomingRound_lineup_NaiveMatch <- function(UpcomingRound_lineups_raw, player_key) {
  
  UpcomingRound_lineups_raw |> 
    rename(
      firstname = first_name, 
      surname = last_name,
      position = role) |> 
    mutate(
      team = ifelse(team == "Wests Tigers", "Tigers", team),
      team = ifelse(team == "Sea Eagles", "Eagles", team),
      position = case_when(
        position == "Winger" ~ "Wing",
        position == "2nd" ~ "Second Row",
        T ~ position),
      firstname = clean_player_firstname(firstname), # Cleaning historical culprits
      surname = clean_player_surname(surname) # Cleaning historical culprits
    ) |> # keeping consistent formatting
    filter(position != "Reserve") |> # reserves rarely get on, so remove for now
    left_join(player_key, by = c("firstname", "surname")) # naive join with player key
  
}


pull_UpcomingRound_lineup_UnMatched <- function(UpcomingRound_lineup_NaiveMatch) {
  
  UpcomingRound_lineup_NaiveMatch |> 
    filter(is.na(player_id)) |> 
    select(-player_id)
  
}


build_UpcomingRound_lineup_FuzzyMatch <- function(UpcomingRound_lineup_UnMatched, player_key) {
  
  UpcomingRound_lineup_UnMatched |> 
    stringdist_left_join( 
      player_key,
      by = c("firstname", "surname"),
      method = "jw",
      max_dist = 0.22 # Found that this is the sweet spot historically, reproducible because we manually update player_key now
    ) |> 
    rename(firstname = firstname.x, surname = surname.x) |> 
    select(-firstname.y, -surname.y)
  
}


pull_UpcomingRound_lineup_NewPlayers <- function(UpcomingRound_lineup_FuzzyMatch) {
  
  UpcomingRound_lineup_FuzzyMatch |> 
    filter(is.na(player_id)) |> 
    select(firstname, surname, player_id)
  
}


add_NewPlayers_player_key <- function(UpcomingRound_lineup_NewPlayers, player_key) {
  
  player_key |> 
    bind_rows(
      UpcomingRound_lineup_NewPlayers
    ) |> 
    arrange(player_id) |> 
    mutate(
      player_id = accumulate(player_id, ~ ifelse(is.na(.y), .x + 1, .y))
    )
  
}


pull_UpComingRound_lineup_NewPlayers_key <- function(UpcomingRound_lineup_NewPlayers, updated_player_key) {
  
  UpcomingRound_lineup_NewPlayers |> 
    select(-player_id) |> 
    left_join(
      updated_player_key, 
      by = c("firstname", "surname")
    )
  
}


combine_UpcomingRound_lineup_matches <- function(UpcomingRound_lineup_NaiveMatch, UpcomingRound_lineup_FuzzyMatch, UpComingRound_lineup_NewPlayers_key) {
  
  UpcomingRound_lineup_NaiveMatch |> 
    drop_na(player_id) |> 
    bind_rows(
      UpcomingRound_lineup_FuzzyMatch |> 
        left_join(
          UpComingRound_lineup_NewPlayers_key, 
          by = c("firstname", "surname")
        ) |> 
        rename(player_id = player_id.x) |> 
        mutate(player_id = ifelse(is.na(player_id), player_id.y, player_id)) |> 
        select(-player_id.y)
    ) |> 
    select(player_id, firstname, surname, team, position)
  
}


join_UpcomingRound_fixtures_to_lineup <- function(UpcomingRound_lineup_combined, UpcomingRound_fixtures_clean_long) {
  
  UpcomingRound_lineup_combined |> 
    left_join(
      UpcomingRound_fixtures_clean_long,
      by = c("team")
    ) |> 
    rename(team_name = team)
  
}

clean_UpcomingRound_lineup <- function(UpcomingRound_lineups_raw, UpcomingRound_fixtures_clean_long, player_key) {
  
  UpcomingRound_lineup_NaiveMatch <- build_UpcomingRound_lineup_NaiveMatch(UpcomingRound_lineups_raw, player_key)
  
  UpcomingRound_lineup_UnMatched <- pull_UpcomingRound_lineup_UnMatched(UpcomingRound_lineup_NaiveMatch)
  
  UpcomingRound_lineup_FuzzyMatch <- build_UpcomingRound_lineup_FuzzyMatch(UpcomingRound_lineup_UnMatched, player_key)
  
  UpcomingRound_lineup_NewPlayers <- pull_UpcomingRound_lineup_NewPlayers(UpcomingRound_lineup_FuzzyMatch)
  
  updated_player_key <- add_NewPlayers_player_key(UpcomingRound_lineup_NewPlayers, player_key)
  
  UpComingRound_lineup_NewPlayers_key <- pull_UpComingRound_lineup_NewPlayers_key(UpcomingRound_lineup_NewPlayers, updated_player_key)
  
  UpcomingRound_lineup_combined <- combine_UpcomingRound_lineup_matches(UpcomingRound_lineup_NaiveMatch, UpcomingRound_lineup_FuzzyMatch, UpComingRound_lineup_NewPlayers_key)
  
  UpcomingRound_lineup_clean <- join_UpcomingRound_fixtures_to_lineup(UpcomingRound_lineup_combined, UpcomingRound_fixtures_clean_long)
  
  UpcomingRound_lineup_clean
}




















