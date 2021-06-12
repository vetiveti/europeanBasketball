# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# source functions to use
source('functions/BBL_functions.R')
source('functions/pbp_actions.R')

library(tidyverse, warn.conflicts = FALSE)
library(zoo)

#******************************************************************************#
# Load roster files:----
roster_files = paste0("Data/rosters_", 2008:2018, ".Rds")
name <- gsub("\\.Rds$", "", roster_files) %>% 
    gsub("Data/", "", .)
roster_data <- lapply(roster_files, readRDS)
names(roster_data) <- gsub("\\.Rds$", "", name)

#******************************************************************************#
# load identifiers: ----
game_id_files = paste0("Data/identifiers_", 2008:2018, ".Rds")
name <- gsub("\\.Rds$", "", game_id_files) %>% 
    gsub("Data/", "", .)
game_id_data <- lapply(game_id_files, readRDS)
names(game_id_data) <- gsub("\\.Rds$", "", name)

# get results safely, because some games have no data
safer_results <- possibly(get_pbp, otherwise = as_tibble("Error finding file"))

#******************************************************************************#
# Load play by play files: ----
pbp_files = paste0("Data/pbp", 2008:2018, ".Rds")
name <- gsub("\\.Rds$", "", pbp_files) %>% 
    gsub("Data/", "", .)
pbp_data <- lapply(pbp_files, readRDS)
names(pbp_data) <- gsub("\\.Rds$", "", name)

# YEAR BY YEAR:  ----
#******************************************************************************#
# Clean pbp data: ----
# prepare game ids
id <- game_id_data$identifiers_2014

# prepare pbp data
pbp <- pbp_data$pbp2014 %>% 
    arrange(desc(spielzeit_sec )) %>% 
    arrange(game_nr)

# prepare roster data
roster <- roster_data$rosters_2014 %>% 
    type_convert()
unique(roster$Club)

# 2017
roster$Club[roster$Club == "s.Oliver Würzurg"] <- "s.Oliver Würzburg"
roster$Club[roster$Club == "Oettinger Rockets"] <- "Rockets"
roster <- roster %>% 
    filter(.,game_nr != 20881 & game_nr != 20885)
unique(roster$Club)

id <- id %>% 
    filter(.,game_id != 20881 & game_id != 20885)

pbp <- pbp %>% 
    filter(.,game_nr != 55 & game_nr != 60)    

###
id_merge <- bind_cols(id, unique(pbp$game_nr)) %>% 
    rename(nr = `...3`)

pbp <- merge(pbp,id_merge,
             by.x = "game_nr",
             by.y = "nr")

# clean for games which have no files
pbp <- pbp %>%
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")

#******************************************************************************#
# calculate starters: ----
#' starter data is far from perfect but I figured out what is wrong
#' Have to do that for every year...
#' 
#' Still wrong are players who switch in timeouts! But I do not know how to
#' solve this issue!!!
#' 
#' Anyway the corrections are done by hand and can be found in the excel file.
#' 

# 2014
roster2014 <- calc_starters(pbp,roster) 

#
starters2014 <- roster2014 %>% 
    mutate(starter_Q5 = replace_na(starter_Q5,0),
           starter_Q6 = replace_na(starter_Q6,0))

#
# pbp_starter <- pbp %>%
#     mutate(game_q = game_id * quarter)
# 
# pbp_game_1 <- filter(pbp_starter,
#                      game_q == 17251*1)
# 
# solve <- calc_starters(pbp_game_1,roster)
# sum(solve$starter_Q1)
# view(solve)
# debugonce(calc_starters)
# solve2 <- calc_starters(pbp_game_1,roster)
#

starters2014$starter_Q2[starters2014$game_nr == 17103 & starters2014$Player == "Kristian, Kuhn"] <- 0
starters2014$starter_Q5[starters2014$game_nr == 17110 & starters2014$Player == "Janis, Strelnieks"] <- 1
starters2014$starter_Q3[starters2014$game_nr == 17114 & starters2014$Player == "Philip, Zwiener"] <- 0
starters2014$starter_Q2[starters2014$game_nr == 17124 & starters2014$Player == "Jared, Newson"] <- 0
starters2014$starter_Q2[starters2014$game_nr == 17124 & starters2014$Player == "Jason, Clark"] <- 0
starters2014$starter_Q2[starters2014$game_nr == 17124 & starters2014$Player == "Dominik, Spohr"] <- 1
starters2014$starter_Q2[starters2014$game_nr == 17124 & starters2014$Player == "Acha, Njei"] <- 0
starters2014$starter_Q5[starters2014$game_nr == 17126 & starters2014$Player == "Patrick, Richard"] <- 1
starters2014$starter_Q6[starters2014$game_nr == 17126 & starters2014$Player == "Immanuel, McElroy"] <- 1
starters2014$starter_Q5[starters2014$game_nr == 17137 & starters2014$Player == "Calvin, Harris"] <- 1
starters2014$starter_Q3[starters2014$game_nr == 17178 & starters2014$Player == "Keith, Ramsey"] <- 1
starters2014$starter_Q4[starters2014$game_nr == 17190 & starters2014$Player == "David, Brembly"] <- 1
starters2014$starter_Q1[starters2014$game_nr == 17226 & starters2014$Player == "Moritz, Wagner"] <- 0
starters2014$starter_Q4[starters2014$game_nr == 17226 & starters2014$Player == "Ryan, Thompson"] <- 0
starters2014$starter_Q1[starters2014$game_nr == 17232 & starters2014$Player == "Jon, Brockman"] <- 1
starters2014$starter_Q1[starters2014$game_nr == 17251 & starters2014$Player == "Urule, Igbavboa"] <- 0
starters2014$starter_Q1[starters2014$game_nr == 17251 & starters2014$Player == "Aleksandar, Nadjfeji"] <- 0
starters2014$starter_Q3[starters2014$game_nr == 17252 & starters2014$Player == "Till-Joscha, Jönke"] <- 0
starters2014$starter_Q3[starters2014$game_nr == 17257 & starters2014$Player == "Joshua, Duncan"] <- 0
starters2014$starter_Q1[starters2014$game_nr == 17281 & starters2014$Player == "Tim, Koch"] <- 0
starters2014$starter_Q5[starters2014$game_nr == 17285 & starters2014$Player == "Quantez, Robertson"] <- 1
starters2014$starter_Q4[starters2014$game_nr == 17293 & starters2014$Player == "Johannes, Thiemann"] <- 0
starters2014$starter_Q4[starters2014$game_nr == 17298 & starters2014$Player == "David, McCray"] <- 1
starters2014$starter_Q1[starters2014$game_nr == 17308 & starters2014$Player == "Leo, Niebuhr"] <- 0
starters2014$starter_Q2[starters2014$game_nr == 17342 & starters2014$Player == "Jusuf, El Domiaty"] <- 1
starters2014$starter_Q3[starters2014$game_nr == 17358 & starters2014$Player == "Jermaine, Anderson"] <- 1
starters2014$starter_Q1[starters2014$game_nr == 17372 & starters2014$Player == "Ryan, Brooks"] <- 0
starters2014$starter_Q1[starters2014$game_nr == 17372 & starters2014$Player == "Mickey, McConnell"] <- 0
starters2014$starter_Q4[starters2014$game_nr == 17384 & starters2014$Player == "Heiko, Schaffartzik"] <- 0
starters2014$starter_Q2[starters2014$game_nr == 17388 & starters2014$Player == "Martin, Bogdanov"] <- 0

anzahl <- unique(starters2014$game_nr) %>% as_tibble() %>% nrow()
print(anzahl)
sum(starters2014$starter_Q1) / 10
sum(starters2014$starter_Q2) / 10
sum(starters2014$starter_Q3) / 10
sum(starters2014$starter_Q4) / 10
sum(starters2014$starter_Q5) / 10

# 2015
roster2015 <- calc_starters(pbp,roster) 

#
# pbp_starter <- pbp %>%
#     mutate(game_q = game_id * quarter)
# 
# pbp_game_1 <- filter(pbp_starter,
#                      game_q == 18365*4)
# 
# solve <- calc_starters(pbp_game_1,roster)
# view(solve)
# debugonce(calc_starters)
# solve2 <- calc_starters(pbp_game_1,roster)
#
starters2015 <- roster2015 %>% 
    mutate(starter_Q5 = replace_na(starter_Q5,0),
           starter_Q6 = replace_na(starter_Q6,0))

starters2015$starter_Q3[starters2015$game_nr == 18314 & starters2015$Player == "Alex, King"] <- 0
starters2015$starter_Q4[starters2015$game_nr == 18272 & starters2015$Player == "Robert, Zinn"] <- 1
starters2015$starter_Q5[starters2015$game_nr == 18280 & starters2015$Player == "Anthony, Di Leo"] <- 1
starters2015$starter_Q2[starters2015$game_nr == 18286 & starters2015$Player == "Martin, Bogdanov"] <- 0
starters2015$starter_Q1[starters2015$game_nr == 18288 & starters2015$Player == "Lance, Jeter"] <- 0
starters2015$starter_Q4[starters2015$game_nr == 18288 & starters2015$Player == "Frantz, Massenat"] <- 0
starters2015$starter_Q2[starters2015$game_nr == 18339 & starters2015$Player == "Mitchell, Watt"] <- 1
starters2015$starter_Q2[starters2015$game_nr == 18340 & starters2015$Player == "Paul, Zipser"] <- 0
starters2015$starter_Q2[starters2015$game_nr == 18340 & starters2015$Player == "Anton, Gavel"] <- 0
starters2015$starter_Q4[starters2015$game_nr == 18310 & starters2015$Player == "Lance, Jeter"] <- 0
starters2015$starter_Q4[starters2015$game_nr == 18298 & starters2015$Player == "Akeem, Vargas"] <- 0
starters2015$starter_Q4[starters2015$game_nr == 18342 & starters2015$Player == "Marque, Perry"] <- 0
starters2015$starter_Q5[starters2015$game_nr == 18345 & starters2015$Player == "Larry, Gordon"] <- 1
starters2015$starter_Q1[starters2015$game_nr == 18302 & starters2015$Player == "Ruben, Spoden"] <- 0
starters2015$starter_Q2[starters2015$game_nr == 18353 & starters2015$Player == "Dennis, Kramer"] <- 0
starters2015$starter_Q2[starters2015$game_nr == 18361 & starters2015$Player == "Ivan, Elliott"] <- 1
starters2015$starter_Q3[starters2015$game_nr == 18365 & starters2015$Player == "Amin, Stevens"] <- 0
starters2015$starter_Q4[starters2015$game_nr == 18365 & starters2015$Player == "Dominique, Johnson"] <- 1
starters2015$starter_Q4[starters2015$game_nr == 18372 & starters2015$Player == "Tyrus, Thomas"] <- 0
starters2015$starter_Q3[starters2015$game_nr == 18418 & starters2015$Player == "Bastian, Doreth"] <- 1
starters2015$starter_Q4[starters2015$game_nr == 18440 & starters2015$Player == "Chris, Frazier"] <- 0
starters2015$starter_Q1[starters2015$game_nr == 18469 & starters2015$Player == "Andre, Calvin"] <- 0
starters2015$starter_Q4[starters2015$game_nr == 18482 & starters2015$Player == "Per, Günther"] <- 0
starters2015$starter_Q2[starters2015$game_nr == 18485 & starters2015$Player == "David, Godbold"] <- 1
starters2015$starter_Q4[starters2015$game_nr == 18568 & starters2015$Player == "Johannes, Richter"] <- 0
starters2015$starter_Q2[starters2015$game_nr == 18509 & starters2015$Player == "Lucas, Gertz"] <- 0
starters2015$starter_Q2[starters2015$game_nr == 18509 & starters2015$Player == "Tyrone, Nash"] <- 0
starters2015$starter_Q2[starters2015$game_nr == 18515 & starters2015$Player == "Lucas, Gertz"] <- 0
starters2015$starter_Q4[starters2015$game_nr == 18515 & starters2015$Player == "Lucas, Gertz"] <- 0
starters2015$starter_Q3[starters2015$game_nr == 18521 & starters2015$Player == "Nicolai, Simon"] <- 1

anzahl <- unique(starters2015$game_nr) %>% as_tibble() %>% nrow()
print(anzahl)
sum(starters2015$starter_Q1) / 10
sum(starters2015$starter_Q2) / 10
sum(starters2015$starter_Q3) / 10
sum(starters2015$starter_Q4) / 10
sum(starters2015$starter_Q5) / 10

pbp <- filter(pbp,
              (game_id != 18393 | quarter != 5))

# 2016
roster2016 <- calc_starters(pbp,roster) 

#
# pbp_starter <- pbp %>%
#     mutate(game_q = game_id * quarter)
# 
# pbp_game_1 <- filter(pbp_starter,
#                      game_q == 19619 * 5)
# 
# solve <- calc_starters(pbp_game_1,roster)
# view(solve)
# debugonce(calc_starters)
# solve2 <- calc_starters(pbp_game_1,roster)
#
starters2016 <- roster2016 %>% 
    mutate(starter_Q5 = replace_na(starter_Q5,0),
           starter_Q6 = replace_na(starter_Q6,0))

starters2016$starter_Q1[starters2016$game_nr == 19579 & starters2016$Player == "Jonas, Grof"] <- 0
starters2016$starter_Q2[starters2016$game_nr == 19599 & starters2016$Player == "Moses, Ehambe"] <- 1
starters2016$starter_Q2[starters2016$game_nr == 19603 & starters2016$Player == "Paul, Carter"] <- 0
starters2016$starter_Q4[starters2016$game_nr == 19625 & starters2016$Player == "Wes, Washpun"] <- 1
starters2016$starter_Q5[starters2016$game_nr == 19749 & starters2016$Player == "Brian, Qvale"] <- 1
starters2016$starter_Q5[starters2016$game_nr == 19783 & starters2016$Player == "Shawn, Huff"] <- 1
starters2016$starter_Q4[starters2016$game_nr == 19798 & starters2016$Player == "Ojars, Silins"] <- 1
starters2016$starter_Q5[starters2016$game_nr == 19817 & starters2016$Player == "Stefan, Ilzhöfer"] <- 1
starters2016$starter_Q5[starters2016$game_nr == 19619 & starters2016$Player == "Nihad, Djedovic"] <- 0


anzahl <- unique(starters2016$game_nr) %>% as_tibble() %>% nrow()
print(anzahl)
sum(starters2016$starter_Q1) / 10
sum(starters2016$starter_Q2) / 10
sum(starters2016$starter_Q3) / 10
sum(starters2016$starter_Q4) / 10
sum(starters2016$starter_Q5) / 10

pbp <- filter(pbp,
                (game_id != 19619 | quarter != 5))

# 2017
roster2017 <- calc_starters(pbp,roster) 

# pbp_starter <- pbp %>%
#     mutate(game_q = game_id * quarter)
# 
# pbp_game_1 <- filter(pbp_starter,
#                      game_q == 21126 * 4)
# 
# solve <- calc_starters(pbp_game_1,roster)
# view(solve)
# debugonce(calc_starters)
# solve2 <- calc_starters(pbp_game_1,roster)

starters2017 <- roster2017 %>% 
    mutate(starter_Q5 = replace_na(starter_Q5,0))
starters2017$starter_Q4[starters2017$game_nr == 20923 & starters2017$Player == "Lucca, Staiger"] <- 1
starters2017$starter_Q3[starters2017$game_nr == 20978 & starters2017$Player == "Kruize, Pinkins"] <- 1
starters2017$starter_Q5[starters2017$game_nr == 21030 & starters2017$Player == "Jamar, Abrams"] <- 1
starters2017$starter_Q3[starters2017$game_nr == 21068 & starters2017$Player == "Thomas, Klepeisz"] <- 0
starters2017$starter_Q3[starters2017$game_nr == 21067 & starters2017$Player == "Andrew, Warren"] <- 1
starters2017$starter_Q4[starters2017$game_nr == 20987 & starters2017$Player == "Aleksej, Nikolic"] <- 1
starters2017$starter_Q4[starters2017$game_nr == 21100 & starters2017$Player == "Anthony, Morse"] <- 1
starters2017$starter_Q3[starters2017$game_nr == 21108 & starters2017$Player == "Kameron, Taylor"] <- 0
starters2017$starter_Q4[starters2017$game_nr == 21108 & starters2017$Player == "Leon, Kratzer"] <- 0
starters2017$starter_Q4[starters2017$game_nr == 21126 & starters2017$Player == "Skyler, Bowlin"] <- 1

anzahl <- unique(starters2017$game_nr) %>% as_tibble() %>% nrow()
print(anzahl)
sum(starters2017$starter_Q1) / 10
sum(starters2017$starter_Q2) / 10
sum(starters2017$starter_Q3) / 10
sum(starters2017$starter_Q4) / 10
sum(starters2017$starter_Q5) / 10

# 2018
roster2018 <- calc_starters(pbp,roster) 

# pbp_game_1 <- filter(pbp_starter,
#                      game_q == 22072 * 4)
# 
# # source functions to use
# source('functions/BBL_functions.R')
# source('functions/pbp_actions.R')
# solve <- calc_starters(pbp_game_1,roster)
# view(solve)
# debugonce(calc_starters)
# solve2 <- calc_starters(pbp_game_1,roster)
#

starters2018 <- roster2018 %>% 
    mutate(starter_Q5 = replace_na(starter_Q5,0),
           starter_Q6 = replace_na(starter_Q6,0))
starters2018$starter_Q4[starters2018$game_nr == 22072 & starters2018$Player == "Benjamin, Lischka"] <- 0
starters2018$starter_Q4[starters2018$game_nr == 22144 & starters2018$Player == "Jason, Clark"] <- 0
starters2018$starter_Q1[starters2018$game_nr == 22274 & starters2018$Player == "Dru, Joyce"] <- 1
starters2018$starter_Q4[starters2018$game_nr == 22326 & grepl("Brooks",starters2018$Player)] <- 1
starters2018$starter_Q3[starters2018$game_nr == 22325 & starters2018$Player == "Elston, Turner"] <- 1

anzahl <- unique(starters2018$game_nr) %>% as_tibble() %>% nrow()
print(anzahl)
sum(starters2018$starter_Q1) / 10
sum(starters2018$starter_Q2) / 10
sum(starters2018$starter_Q3) / 10
sum(starters2018$starter_Q4) / 10
sum(starters2018$starter_Q5) / 10
sum(starters2018$starter_Q6) / 10


#******************************************************************************#
# boxscores teams pg & totals: ----
un <- unique(roster$game_nr)

id_plus_teams <- tibble(id = un,
                        home_id = 0,
                        team_1 = 0,
                        team_2 = 0)
bx_teams <- tibble()

for (i in seq_along(un)) {
    current_game <- filter(pbp,
                game_id == un[i])
    current_teams <- na.omit(unique(current_game$Club_1))
    
    id_plus_teams$team_1[i] <- current_teams[1]
    id_plus_teams$team_2[i] <- current_teams[2]
    id_plus_teams$home_id[i] <- current_game$home_id[1]
    
    current_boxscore <- get_boxscore_team(current_game,current_teams[1],current_teams[2])
    current_boxscore_against <- get_boxscore_team(current_game,current_teams[2],current_teams[1])
    current_boxscore$game_nr <- un[i]
    current_boxscore_against$game_nr <- un[i]
    bx_teams <- bind_rows(bx_teams,current_boxscore,current_boxscore_against)
}

bx_teams <- bx_teams %>% 
    mutate(W = if_else(pts>opp_pts,1,0),
           L = if_else(pts<opp_pts,1,0),
           G = W + L) %>% 
    rename(team = stats)

# 2015
unique(bx_teams$team)

# 2016
unique(bx_teams$team)

# 2017
unique(bx_teams$team)
bx_teams$team[bx_teams$team == "Oettinger Rockets"] <- "Rockets"
bx_teams$team[bx_teams$team == "s.Oliver Würzurg"] <- "s.Oliver Würzburg"

# 2018
unique(bx_teams$team)
bx_teams$team[bx_teams$team == "SYNTAINICS MBC"] <- "Mitteldeutscher BC"





team_totals <- bx_teams %>% 
    group_by(team) %>% 
    summarise_at(vars(min:G), .funs = sum)

#******************************************************************************#
# boxscore players pg & totals: ----
players <- roster %>% 
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")


length(unique(pbp$game_id))
length(unique(players$game_nr))

player_tot_perTeam_pg <- tibble()
for(i in unique(pbp$game_id)) {
    players_cur <- filter(players,
                          game_nr == i)
    pbp_cur <- filter(pbp,
                      game_id == i)
    
    player_tot_cur <- tibble()
    for (j in seq_along(players_cur$Player)) {
        player_cur <- players_cur$Player[j]
        bx_player_cur <- get_boxscore_player(pbp_cur,player_cur)
        bx_player_cur$team <- players_cur$Club[j]
        bx_player_cur$game <- players_cur$game_nr[j]
        
        player_tot_cur <- bind_rows(player_tot_cur, bx_player_cur)
    }
    player_tot_perTeam_pg <- bind_rows(player_tot_perTeam_pg, player_tot_cur)
}

player_tot_perTeam <- player_tot_perTeam_pg %>% 
    group_by(stats,team) %>% 
    summarise(
        across(everything(), .fns = sum),
               .groups = "drop"
    ) %>%
    select(-game) %>% 
    rename(player = stats) %>% 
    relocate(team, .after =player)

#' assists are tricky!
#' in the NBA assists are not granted for pass before FT in the FIBA world the count as assists!
#' I compute the NBA style assists as I work with methods which are developed for the NBA

#******************************************************************************#
# calc. minutes played: ----
source('functions/BBL_functions.R')
debugonce(playing_time)
z <- tibble()
y <- filter(pbp,
            game_id ==17189)
for (i in unique(y$game_id)) {
    a <- filter(y,
                game_id == i)
    b <- filter(starters2015,
                game_nr == i)
    c <- playing_time(b,a)
    c$game_nr <- i
    
    z <- bind_rows(z,c)
}
view(z)

#' therefore a data frame must be build which tells who is on the court and when
#' this must be done for every single game!
#' 
#' CHANGE every year!
play_time <- tibble()
for (i in unique(pbp$game_id)) {
    a <- filter(pbp,
                game_id == i)
    b <- filter(starters2014,
                game_nr == i)
    c <- playing_time(b,a)
    c$game_nr <- i
    
    play_time <- bind_rows(play_time,c)
}
#
play_time$player <- trimws(play_time$player)

# 2014
play_time$player[play_time$player == "Jake, O#Brien"] <- "Jake, O'Brien"
play_time$player[play_time$player == "Jake, O`Brien"] <- "Jake, O'Brien"
play_time$player[play_time$player == "Jake, OBrien"] <- "Jake, O'Brien"
play_time$player[play_time$player == "Chad, Topper"] <- "Chad, Toppert"
play_time$player[play_time$player == "Nihad, Dedovic"] <- "Nihad, Djedovic"

# 2015
play_time$player[play_time$player == "Anthony, Di Leo"] <- "Anthony, DiLeo"
play_time$player[play_time$player == "Joshua Patrick, Gasser"] <- "Joshua, Gasser"
play_time$player[play_time$player == "Nicolò, Melli"] <- "Nicolo, Melli"

# 2016
play_time$player[play_time$player == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"

# 2017
play_time$player[play_time$player == "Miles, Jackson-C"] <- "Miles, Jackson-Cartwright"
play_time$player[play_time$player == "Darvin, Davis"] <- "Darwin, Davis"
play_time$player[play_time$player == "E. J., Singler"] <- "E.J., Singler"
play_time$player[play_time$player == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"

# 2018
play_time$player[play_time$player == "Ra#Shad, James"] <- "Ra'Shad, James"
play_time$Club[play_time$Club == "SYNTAINICS MBC"] <- "Mitteldeutscher BC"


#
games_played  <- play_time %>%                         # Count rows by group
    group_by(player,Club) %>% 
    summarise(G = n(), .groups ="keep")

play_time1 <- play_time %>% 
    mutate_at("sec_total", ~replace(., is.na(.), 0))

df <- play_time1 %>% group_by(player,Club) %>%
    summarize(Sum_sec = sum(sec_total), .groups = "keep") %>% 
    mutate(min_sec_played = lubridate::seconds_to_period(Sum_sec)) %>% 
    mutate(min_sec = round(Sum_sec / 60, digits = 2))


n_distinct(roster2014$game_nr)
n_distinct(roster$game_nr)
setdiff(roster$game_nr, starters2014$game_nr)

#******************************************************************************#
# Merge boxscore & playing time: ----
df_new <- merge(df,player_tot_perTeam,
                by.x = c("player","Club"),
                by.y = c("player","team")) %>% 
    rename(team = Club)

player_data <- merge(df_new, games_played,
             by.x = c("player","team"),
             by.y = c("player","Club")) %>% 
    relocate(G, .after = player)
#******************************************************************************#
# Download Position etc. & merge: ----

# 2014
player_info <- pos_cm_kg(2014)
player_info$player[player_info$player == "Chad, Topper"] <- "Chad, Toppert"
player_info$player[player_info$player == "Jake, O#Brien"] <- "Jake, O'Brien"
player_info$player[player_info$player == "Jake, O`Brien"] <- "Jake, O'Brien"
player_info$player[player_info$player == "Jake, OBrien"] <- "Jake, O'Brien"
player_info$player[player_info$player == "Chad, Topper"] <- "Chad, Toppert"
player_info$player[player_info$player == "Nihad, Dedovic"] <- "Nihad, Djedovic"

# 2015
player_info <- pos_cm_kg(2015)
player_info$player[player_info$player == "Anthony, Di Leo"] <- "Anthony, DiLeo"
player_info$player[player_info$player == "Joshua Patrick, Gasser"] <- "Joshua, Gasser"
player_info$player[player_info$player == "Nicolò, Melli"] <- "Nicolo, Melli"

# 2016
player_info <- pos_cm_kg(2016)
player_info$player[player_info$player == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"

# 2017
player_info <- pos_cm_kg(2017)
player_info$player[player_info$player == "Miles, Jackson-C"] <- "Miles, Jackson-Cartwright"
player_info$player[player_info$player == "Darvin, Davis"] <- "Darwin, Davis"
player_info$player[player_info$player == "E. J., Singler"] <- "E.J., Singler"
player_info$player[player_info$player == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"

# 2018
player_info <- pos_cm_kg(2018)
player_info$player[player_info$player == "Ra#Shad, James"] <- "Ra'Shad, James"

#
player_data_info<- merge(player_data,player_info,
                     by = "player") %>% 
    filter(., not_played == 0) %>% 
    distinct(.,player,team, .keep_all = TRUE)

#******************************************************************************#
# calc. team fg, opp_fg, win_pct:----
team_totals <- team_totals %>% 
    mutate(fga = p2a + p3a,
           fgm = p2m + p3m,
           opp_fga = opp_p2a + opp_p3a,
           opp_fgm = opp_p2m + opp_p3m,
           min = round(min / 60 *5)) %>% 
    relocate(fga:opp_fgm, .before = opp_pts) %>% 
    drop_na()

#******************************************************************************#
# boxscore team pg:----
team_pg <- team_totals %>% 
    mutate(across(.cols = min:opp_pts, ~ .x / G),
           win_pct = W/G)

#******************************************************************************#
# calc. player min_p, fg:----
player_data_info <- player_data_info %>% 
    mutate(min_p = round(Sum_sec /60),
           fga = p2a + p3a,
           fgm = p2m + p3m,) %>% 
    relocate(min_p, .after = G) %>% 
    relocate(fgm, .before=pts) %>% 
    relocate(fga, .before= pts) %>% 
    relocate(team, .after = player) %>% 
    select(-min_sec, -min_sec_played, -Sum_sec)

#******************************************************************************#
# boxscore player pg:----
player_pg <- player_data_info %>%
    mutate(across(.cols = min_p:pts, ~ .x / G))

#******************************************************************************#
# player foul percentage:----
# foul percentage
team_for_merge <- team_totals %>% 
    select(team, opp_ftm, pf, G) %>% 
    rename(pf_t = pf) %>% 
    rename(G_t = G)

player_for_merge <- player_data_info %>% 
    select(player, team, pf,G) %>% 
    mutate(pf_p = pf) %>% 
    rename(G_p = G)

player_perT <- merge(player_for_merge,team_for_merge,
                     by = "team",
                     all = TRUE) %>% 
    mutate(pf_p = pf_p / G_t)

player_perT <- player_perT %>% 
    group_by(team) %>% 
    mutate(PF_perc = pf_p / pf_t,
           pf_p = opp_ftm * PF_perc * G_t) %>% 
    ungroup() %>% 
    select(player,team,pf_p)

player_totals <- merge(player_data_info, player_perT,
                       by = c("player","team"))
#******************************************************************************#
# save files:----
saveRDS(object = player_pg, file = paste0("Data/player_pg_2014",".Rds"))
saveRDS(object = player_totals, file = paste0("Data/player_totals_2014",".Rds"))

saveRDS(object = team_pg, file = paste0("Data/team_pg_2014",".Rds"))
saveRDS(object = team_totals, file = paste0("Data/team_totals_2014",".Rds"))

#******************************************************************************#
# load & merge files:----
# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

require(tidyverse, warn.conflicts = FALSE)


require(data.table)
files <- list.files(path = './data', pattern ='team_totals')
team_dat_list = lapply(paste0("Data/",files), function (x) data.table(readRDS(x)))
team_data_totals = rbindlist(team_dat_list, fill = TRUE, idcol="ID") %>% 
    mutate(year = ID +2013) %>% 
    relocate(team, year, G, W, L, everything()) %>% 
    relocate(fga, fgm, .after = min) %>% 
    relocate(opp_fga, opp_fgm, .after = opp_min) %>% 
    select(-ID,-game_nr) %>% 
    mutate(opp_min = round(opp_min/60 * 5))
