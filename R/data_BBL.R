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
    player <- roster$Player[i]
    bx_player <- get_boxscore_player(pbp,player)
    bx_players <- bind_rows(bx_players, bx_player)
}
#' assists are tricky!
#' in the NBA assists are not granted for pass before FT in the FIBA world the count as assists!
#' I compute the NBA style assists as I work with methods which are developed for the NBA


#' still missing...
#' playing time for players
#' auswechslungen und einwechslungen nicht erfasst




url_starter <- gsub(" ","",paste("http://live.easycredit-bbl.de/data",year,
                              "/bbl/",game_id$home_id,
                              "/",game_id$game_id,
                              ".JSN"))

# Which teams did play?
json_starter <- get_json(url_starter)

df <- json_starter$statind %>% 
    as_tibble() %>% 
    rename(teamcode = V1,
           spielcode = V2,
           spieler_nummer = V3,
           pts = V4,
           ftm = V5,
           fta = V6,
           ft_pct = V7,
           p2m = V8,
           p2a = V9,
           p2_pct = V10,
           p3m = V11,
           p3a = V12,
           p3_pct = V13,
           pf = V14,
           trb = V15,
           ast = V16,
           blk = V17,
           stl = V18,
           tov = V19,
           index1 = V20,
           index2 = V21,
           min_mmss = V22,
           min_mm = V23,
           drb = V24,
           orb = V25,
           starter_1 = V26,
           starter_2 = V27,
           starter_3 = V28,
           starter_4 = V29) %>% 
    type_convert()
sum(df$starter_1)
sum(df$starter_2)
sum(df$starter_3)
sum(df$starter_4)
# now it should work
