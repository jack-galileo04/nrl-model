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


build_UpcomingRound_lineup_FuzzyMatch <- function(UpcomingRound_lineup_NaiveMatch, player_key) {
  
  UpcomingRound_lineup_NaiveMatch |> 
    filter(is.na(player_id)) |> 
    select(-player_id) |> 
    stringdist_left_join( 
      player_key,
      by = c("firstname", "surname"),
      method = "jw",
      max_dist = 0.22 # Found that this is the sweet spot historically, reproducible because we manually update player_key now
    ) |> 
    rename(firstname = firstname.x, surname = surname.x) |> 
    select(-firstname.y, -surname.y)
  
}


add_NewPlayers_player_key <- function(UpcomingRound_lineup_FuzzyMatch, player_key) {
  
  new_players <- UpcomingRound_lineup_FuzzyMatch |> 
    filter(is.na(player_id)) |> 
    select(firstname, surname, player_id)
  
  message("Number of new players: ")
  print(nrow(new_players))
  print((new_players |> unite("name", firstname:surname, sep = " "))$name)
  
  updated_player_key <- player_key |> 
    bind_rows(
      new_players
    ) |> 
    arrange(player_id) |> 
    mutate(
      player_id = accumulate(player_id, ~ ifelse(is.na(.y), .x + 1, .y))
    )
  
  return(updated_player_key)
  
}


combine_UpcomingRound_lineup_matches <- function(UpcomingRound_lineup_NaiveMatch, UpcomingRound_lineup_FuzzyMatch, updated_player_key) {
  
  UpComingRound_lineup_NewPlayers_key <- UpcomingRound_lineup_FuzzyMatch |> 
    filter(is.na(player_id)) |> 
    select(firstname, surname) |> 
    left_join(
      updated_player_key, 
      by = c("firstname", "surname")
    )
  
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


clean_UpcomingRound_lineups <- function(UpcomingRound_lineups_raw, UpcomingRound_fixtures_clean_long, player_key) {
  
  UpcomingRound_lineup_NaiveMatch <- build_UpcomingRound_lineup_NaiveMatch(UpcomingRound_lineups_raw, player_key)
  message("Naive Match Successful")
  
  UpcomingRound_lineup_FuzzyMatch <- build_UpcomingRound_lineup_FuzzyMatch(UpcomingRound_lineup_NaiveMatch, player_key)
  message("Fuzzy Match Successful")
  
  updated_player_key <- add_NewPlayers_player_key(UpcomingRound_lineup_FuzzyMatch, player_key)
  message("Updated Player Key Successful")
  
  UpcomingRound_lineup_combined <- combine_UpcomingRound_lineup_matches(
    UpcomingRound_lineup_NaiveMatch, 
    UpcomingRound_lineup_FuzzyMatch, 
    updated_player_key
    )
  message("Combined Lineup Matches Succesful")
  
  UpcomingRound_lineups_clean <- join_UpcomingRound_fixtures_to_lineup(UpcomingRound_lineup_combined, UpcomingRound_fixtures_clean_long)
  message("Combined Lineup with Fixtures Successful")
  
  return(UpcomingRound_lineups_clean)
}
