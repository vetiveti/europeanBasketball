# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# source functions to use
source('functions/BBL_functions.R')
source('functions/pbp_actions.R')

library(tidyverse)


load(file ="data/id_games2008.rda")
load(file ="data/id_games2009.rda")
load(file ="data/id_games2010.rda")
load(file ="data/id_games2011.rda")
load(file ="data/id_games2012.rda")
load(file ="data/id_games2013.rda")
load(file ="data/id_games2014.rda")
load(file ="data/id_games2015.rda")
load(file ="data/id_games2016.rda")
load(file ="data/id_games2017.rda")
load(file ="data/id_games2018.rda")

#' Get play by play data for a specific game
id_2018 <- id_games2018 %>% t %>%  as_tibble() %>% 
    rename(game_id = V1,
           home_id = V2) %>% 
    mutate_if(is.character,as.numeric)

year <- 2018
game_nr <- 1
game_id <- id_2018[game_nr,]
pbp <- get_pbp(year,game_id)

# compute boxscore for teams:
team_h <- home_team
team_a <- away_team
bx_team <- get_boxscore_team(pbp,team_h,team_a)

# compute boxscore for players:
bx_players <- data.frame()
for(i in 1:12) {
    player <- roster$Player[i+12]
    bx_player <- get_boxscore_player(pbp,player)
    bx_players <- bind_rows(bx_players, bx_player)
}
#' assists are tricky!
#' in the NBA assists are not granted for pass before FT in the FIBA world the count as assists!
#' I compute the NBA style assists as I work with methods which are developed for the NBA


#' still missing...
#' playing time for players
#' auswechslungen und einwechslungen nicht erfasst
