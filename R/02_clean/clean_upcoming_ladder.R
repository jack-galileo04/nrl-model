
# upcoming ladder
clean_UpcomingRound_ladder <- function(UpcomingRound_ladder_raw, params) {
  
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
    )
}