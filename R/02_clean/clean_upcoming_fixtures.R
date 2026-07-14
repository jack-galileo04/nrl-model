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