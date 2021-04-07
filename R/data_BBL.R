# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# source functions to use
source('functions/BBL_functions.R')

library(tidyverse)
library(rjson)
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
quarter <- 1
game_nr <- 1
url_pbp <- gsub(" ","",paste("http://live.easycredit-bbl.de/data",year,
                            "/bbl/", id_2018$home_id[game_nr],
                            "/", id_2018$game_id[game_nr],
                            "Q", quarter,
                            ".JSN"))

url_info <- gsub(" ","",paste("http://live.easycredit-bbl.de/data",year,
                              "/bbl/",id_2018$home_id[game_nr],
                              "/",id_2018$game_id[game_nr],
                              "_INIT.JSN"))

res <- httr::GET(url_pbp)

json <- res$content %>% 
    rawToChar() %>%
    jsonlite::fromJSON(simplifyVector = T)

df <- json %>% 
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

df$nummer_aktion <- nrow(df):1

df <- df %>% 
    mutate(spielzeit_sec = lubridate::ms(spielzeit), .keep="unused") %>% 
    relocate(teamcode,spielzeit_sec, everything()) %>% 
    arrange(nummer_aktion)

df$spielzeit_sec <- as.numeric(df$spielzeit_sec)


json_file <- url_info
json_test <- rjson::fromJSON(paste(readLines(json_file), collapse=""))
json_test <- rjson::fromJSON(file=json_file)
team_home <- json_test$teamroster[[1]]$TeamName
team_away <- json_test$teamroster[[2]]$TeamName

res <- httr::GET(url_info)

json <- res$content %>% 
    rawToChar() %>%
    jsonlite::fromJSON(simplifyVector = T)

teams <- json$teamroster
team_h <- teams$TeamName[1]
team_a <- teams$TeamName[2]
    
df1$team <- 0
df1$contrary_team <- 0
n <- nrow(df1)
index <- 1
while(index < n){
    if(df1$X1[index]=="A"){
        df1$team[index] <- team_home
        df1$contrary_team[index] <- team_away
        
    }
    if(df1$X1[index]=="B"){
        df1$team[index] <- team_away
        df1$contrary_team[index] <- team_home
    }
    
    
    index <- index +1
}

test <- df1

test$game_id <- id_2018$game_id[1]
test$home_id <- id_2018$home_id[1]

test$X1 <- NULL
test$X14 <- NULL
test$Viertel <- quarter
# Roster (who played?)
res <- httr::GET(url_info)

json <- res$content %>% 
    rawToChar() %>%
    jsonlite::fromJSON(simplifyVector = T)

roster <- json$roster %>% 
    data.frame %>%
    as_tibble() %>%
    type_convert() %>% 
    mutate(Player = paste(FirstName, Name, sep=", "), .keep = "unused") %>%
    mutate(Club = if_else(TC == "A",team_h,team_a)) %>% 
    mutate(Pos = Posshort, .keep ="unused") %>% 
    relocate(Player, Pos, .before = Is)

roster_h <- filter(roster, TC =="A")
roster_a <- filter(roster, TC =="B")

df_merge <- merge(df1,kader,
                by.x = c("teamcode","sn_Spieler_1"),
                by.y = c("team", "nr"),
                all.x = TRUE) %>% 
    arrange(nummer_aktion)

df <- merge(df_merge,kader,
            by.x = c("teamcode","sn_Spieler_2"),
            by.y = c("team", "nr"),
            all.x = TRUE) %>%
    arrange(nummer_aktion)

datei_name <- paste(id_2008$game_id[1],team_home,team_away,"_", quarter,".csv")
datei_name <- gsub(" ","", datei_name)
print(datei_name)
#write.table(df, datei_name)


source('~/Uni Tübingen/0. Masterarbeit/7. R/Masterarbeit/functions/BBL_functions.R')
library(tidyverse)
year = 2018:2018


debugonce(BBL_game_ids)
id_year <- BBL_game_ids(year)
