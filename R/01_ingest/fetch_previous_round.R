fetch_PreviousRound_ladder <- function(params) {
  
  fetch <- nrlR::fetch_ladder_nrl(
    season = params$season, 
    round_number = params$round_number-1
  )
  message("Previous Ladder Fetch Successful")
  
  fetch |> 
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
      ladder_position = row_number(), # after arranging in ladder order, row_number is the position
      team = case_when(
        team == "Wests Tigers" ~ "Tigers",
        team == "Sea Eagles" ~ "Eagles",
        T ~ team)
    ) |> 
    distinct(team, season, round, .keep_all = TRUE)
  
}

fetch_PreviousRound_player <- function(params) {
  
  fetch <- nrlR::fetch_player_stats(
    season = params$season, 
    round = params$round_number-1,
    league = "nrl", 
    source = "championdata", 
    comp = params$comp
  )
  message("Previous Player Fetch Successful")
  
  fetch |>  
    mutate(
      season = params$season,
      utc_start = as_datetime(utc_start)
    ) |> 
    janitor::clean_names() |> 
    distinct(player_id, match_id, .keep_all = TRUE)
  
  
}

fetch_PreviousRound_team <- function(params) {
  
  fetch <- nrlR::fetch_team_stats_championdata(
    round = params$round_number-1,
    comp = params$comp
  )
  message("Previous Team Fetch Successful")
  
  fetch |> 
    mutate(
      season = params$season,
      utc_start = as_datetime(utc_start)
    ) |> 
    janitor::clean_names() |> 
    distinct(team_name, match_id, .keep_all = TRUE)
  
}








