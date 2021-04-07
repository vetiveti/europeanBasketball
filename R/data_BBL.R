# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# source functions to use
source('functions/BBL_functions.R')

library(tidyverse)
library(lubridate)

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
quarter <- i

url_pbp <- gsub(" ","",paste("http://live.easycredit-bbl.de/data",year,
                            "/bbl/", id_2018$home_id[game_nr],
                            "/", id_2018$game_id[game_nr],
                            "Q", quarter,
                            ".JSN"))

url_info <- gsub(" ","",paste("http://live.easycredit-bbl.de/data",year,
                              "/bbl/",id_2018$home_id[game_nr],
                              "/",id_2018$game_id[game_nr],
                              "_INIT.JSN"))

json_pbp <- get_json(url_pbp)

pbp <- json_pbp %>% 
    data.frame %>%
    as_tibble() %>% 
    rename(teamcode = actions.1,
           spielzeit = actions.2,
           sn_Spieler_1 = actions.3,
           sn_Spieler_2 = actions.4,
           aktion = actions.5,
           zusatzinfo_1 = actions.6,
           zusatzinfo_2 = actions.7,
           zusatzinfo_3 = actions.8,
           resultat = actions.9,
           spielstand_A = actions.10,
           spielstand_B = actions.11,
           x_val = actions.12,
           y_val = actions.13,
           number_action = actions.14,
           timestamp = actions.15)

pbp$nummer_aktion <- nrow(pbp):1

pbp <- pbp %>% 
    mutate(spielzeit_sec = lubridate::ms(spielzeit), .keep="unused") %>% 
    relocate(teamcode,spielzeit_sec, everything()) %>% 
    arrange(nummer_aktion)

pbp$spielzeit_sec <- as.numeric(pbp$spielzeit_sec)

# Which teams did play?
json_info <- get_json(url_info)

teams <- json_info$teamroster
team_h <- teams$TeamName[1]
team_a <- teams$TeamName[2]

# Roster (who played?)
roster <- json_info$roster %>% 
    data.frame %>%
    as_tibble() %>%
    type_convert() %>% 
    mutate(Player = paste(FirstName, Name, sep=", "), .keep = "unused") %>%
    mutate(Club = if_else(TC == "A",team_h,team_a)) %>% 
    mutate(Pos = Posshort, .keep ="unused") %>% 
    relocate(Player, Pos, .before = Is)

roster_h <- filter(roster, TC =="A")
roster_a <- filter(roster, TC =="B")

roster_merge <- roster %>% 
    select(TC, Nr, Player, Club)

# Merge Roster and PbP to obtain player who did perform the action:
pbp_merge <- merge(pbp,roster_merge,
                by.x = c("teamcode","sn_Spieler_1"),
                by.y = c("TC", "Nr"),
                all.x = TRUE) %>% 
    arrange(nummer_aktion)

# Do that a second time because of assists
pbp_merge <- merge(pbp_merge,roster_merge,
            by.x = c("teamcode","sn_Spieler_2"),
            by.y = c("TC", "Nr"),
            all.x = TRUE) %>%
    arrange(nummer_aktion)

# save/return the data frame
file_name <- paste(id_2018$game_id[1],"_",team_h,"_",team_a,"_", quarter,".rda")
file_name <- gsub(" ","", file_name)
print(file_name)


source('functions/BBL_functions.R')
year <- 2018
game_nr <- 1
game_id <- id_2018[game_nr,]
pbp_g1 <- get_pbp(year,game_id)
