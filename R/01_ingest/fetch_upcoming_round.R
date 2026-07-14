build_lineup_url <- function(params) {
  
  tuesday <- gsub("-", "/", floor_date(Sys.Date(), "week", week_start = params$week_start))
  
  if (params$special_round != ""){
    paste0("https://www.nrl.com/news/", tuesday, "/", params$special_round, "/")
  } else {
    paste0("https://www.nrl.com/news/", tuesday, "/nrl-team-lists-round-", params$round_number, "/")
  }
}

fetch_UpcomingRound_ladder_raw <- function(params) {
  
  fetch_ladder_nrl(
    season = params$season, 
    round_number = params$round_number
  )
  
}

fetch_UpcomingRound_lineups_raw <- function(params) {
  
  lineup_url <- build_lineup_url(params)
  
  nrlR::fetch_lineups(
    url = lineup_url, 
    source = "nrl.com", 
    type = "team_list"
  )
  
}

fetch_UpcomingRound_fixtures_raw <- function(params) {
  
  nrlR::fetch_results(
    seasons = params$season,
    league = "nrl", 
    source = "rugbyleagueproject"
  ) |> 
    filter(round == params$round_number + 1) # fetching only upcoming round
  
}

