## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(here)
library(readxl)

library(nrlR)

comps <- fetch_cd_comps() |> 
  filter(str_detect(name, "NRL Finals") | str_detect(name, "NRL Premiership")) |> 
  slice_tail(n = -4)

comp_ids <- comps$id



## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Fetching Outcome Variable

results <- fetch_results_rugbyproject(seasons = 2016:2026, league = "nrl") |> # Removes upcoming fixtures
  filter(!is.na(home_score))

results |> distinct(round)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Fetching Ladder Position Feature

ladder_df <- tibble(team = NA, ladder_points = NA, points_for = NA, points_against = NA, points_diff = NA, season = NA, round = NA, ladder_position = NA)


for(i in 2016:2026){
  
  for(j in 1:35){
    
    ladder_increment <- fetch_ladder_nrl(season = i, round_number = j) |> 
      select(team, ladder_points = points, points_for, points_against, points_diff) |> 
      mutate(season = i, round = j, ladder_position = row_number())
                           
    ladder_df <- bind_rows(
      ladder_df,
      ladder_increment
    )
  }
}

ladder_df <- ladder_df |> 
  drop_na() |> 
  janitor::clean_names()

write.csv(ladder_df, here("Data/Raw/Ladder Data.csv"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
player_df <- tibble(playerId=NA, firstname=NA, surname=NA, team_name=NA, squadId=NA,position=NA,jumperNumber=NA,
         utc_start=NA, match_id=NA, competition_id=NA, round=NA, team_location=NA,
         points=NA, tries=NA, conversionAttempts=NA, conversions=NA, penaltyGoalAttempts=NA,penaltyGoals=NA,
         tryAssists=NA,lineBreakAssists=NA,passes=NA,kickMetres=NA,kicksGeneralPlay=NA,
         possessions=NA,runs=NA,runsHitup=NA,runsNormal=NA,runMetres=NA,runsHitupMetres=NA,runsNormalMetres=NA,postContactMetres=NA,
         offloads=NA,lineBreaks=NA,tackleBreaks=NA,tackleds=NA,
         tackles=NA,tacklesIneffective=NA,missedTackles=NA,trySaves=NA,
         handlingErrors=NA,errors=NA,penaltiesConceded=NA)


for(i in 1:length(comp_ids)){
  player_df <- bind_rows(player_df,
                         fetch_player_stats(season = NULL, 
                                            round = NULL, 
                                            league = "nrl", 
                                            source = "championdata", 
                                            comp = comp_ids[i] ) |> 
                           mutate(season = as.numeric(comps[i,"season"])) |> 
                           select(season,
                                  playerId, firstname, surname, team_name, squadId,position,jumperNumber,
                                  utc_start, match_id, competition_id, round, team_location,
                                  points, tries, conversionAttempts, conversions, penaltyGoalAttempts,penaltyGoals,
                                  tryAssists,lineBreakAssists,passes,kickMetres,kicksGeneralPlay,
                            possessions,runs,runsHitup,runsNormal,runMetres,runsHitupMetres,runsNormalMetres,postContactMetres,
                                  offloads,lineBreaks,tackleBreaks,tackleds,
                                  tackles,tacklesIneffective,missedTackles,trySaves,
                                  handlingErrors,errors,penaltiesConceded) 
                         )
}

player_df <- player_df |> 
  drop_na(playerId) |> 
  janitor::clean_names()

write.csv(player_df, here("Data/Raw/Player Data.csv"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
team_df <- tibble(competition_id = NA,match_id = NA,round = NA,team_name = NA,team_location = NA,utc_start = NA,
                             score = NA,completionRatePercentage = NA,possessionPercentage = NA,
                             timeInOwnHalf = NA,timeInOppHalf = NA,timeInOwn20 = NA,timeInOpp20 = NA,
                             setRestarts = NA,
                             goalLineDropouts = NA)


for(i in 1:length(comp_ids)){
  
  team_df <- bind_rows(team_df,
                         fetch_team_stats_championdata(round = NULL,
                                                       comp = comp_ids[i]) |> 
                           select(
                             competition_id,match_id,round,team_name,team_location,utc_start,
                             score,completionRatePercentage,possessionPercentage,
                             timeInOwnHalf,timeInOppHalf,timeInOwn20,timeInOpp20,
                             setRestarts,
                             goalLineDropouts
                             ) 
                         )
}

team_df <- team_df |> 
  drop_na(team_name) |> 
  janitor::clean_names() |> 
  group_by(match_id, team_name) |> 
  slice_head(n = 1)

write.csv(team_df, here("Data/Raw/Team Data.csv"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
player_key <- player_df |> 
  distinct(player_id) |> 
  left_join(player_df |> select(player_id, firstname, surname), by = "player_id") |> 
  group_by(player_id) |> 
  slice_head(n = 1) |> 
  ungroup()

write.csv(player_key, here("Data/Raw/Player Key.csv"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::purl("nrlR_original_fetch.Rmd", output)

