# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# source functions to use
source('functions/BBL_functions.R')

library(tidyverse, warn.conflicts = FALSE)
library(zoo)

#******************************************************************************#
# Problems:----
#' starter data is far from perfect but I figured out what is wrong.
#' Still wrong are players who switch in timeouts! But I do not know how to
#' solve this issue!!!
#' Anyway the corrections are done by hand and can be found in the excel file.

#******************************************************************************#
# Load cleaned pbp files: ----
pbp_files = paste0("Data/clean_pbp/pbp_", 2014:2018, ".Rds")
name <- gsub("\\.Rds$", "", pbp_files) %>% 
    gsub("Data/", "", .)
pbp_data <- lapply(pbp_files, readRDS)
names(pbp_data) <- gsub("\\.Rds$", "", name)

#******************************************************************************#
# Load roster files:----
roster_files = paste0("Data/rosters_", 2008:2018, ".Rds")
name <- gsub("\\.Rds$", "", roster_files) %>% 
    gsub("Data/", "", .)
roster_data <- lapply(roster_files, readRDS)
names(roster_data) <- gsub("\\.Rds$", "", name)

# YEAR BY YEAR:  ----
#******************************************************************************#
# calculate starters 2014: ----
pbp <- pbp_data$`clean_pbp/pbp_2014`
roster <- roster_data$rosters_2014
roster2014 <- calc_starters(pbp,roster) 

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

#******************************************************************************#
# Save starters 2014:
saveRDS(object = starters2014, file = paste0("Data/starters/starters_2014",".Rds"))

#******************************************************************************#
# calculate starters 2015: ----
pbp <- pbp_data$`clean_pbp/pbp_2015`
roster <- roster_data$rosters_2015
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

#******************************************************************************#
# Save starters 2015:
saveRDS(object = starters2015, file = paste0("Data/starters/starters_2015",".Rds"))

#******************************************************************************#
# calculate starters 2016: ----
pbp <- pbp_data$`clean_pbp/pbp_2016`
roster <- roster_data$rosters_2016
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

anzahl <- unique(starters2016$game_nr) %>% as_tibble() %>% nrow()
print(anzahl)
sum(starters2016$starter_Q1) / 10
sum(starters2016$starter_Q2) / 10
sum(starters2016$starter_Q3) / 10
sum(starters2016$starter_Q4) / 10
sum(starters2016$starter_Q5) / 10

#******************************************************************************#
# Save starters 2016:
saveRDS(object = starters2016, file = paste0("Data/starters/starters_2016",".Rds"))

#******************************************************************************#
# calculate starters 2017: ----
pbp <- pbp_data$`clean_pbp/pbp_2017`
roster <- roster_data$rosters_2017
roster2017 <- calc_starters(pbp,roster) 
#
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
#
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

#******************************************************************************#
# Save starters 2017:
saveRDS(object = starters2017, file = paste0("Data/starters/starters_2017",".Rds"))

#******************************************************************************#
# calculate starters 2018: ----
pbp <- pbp_data$`clean_pbp/pbp_2018`
roster <- roster_data$rosters_2018
roster2018 <- calc_starters(pbp,roster) 
#
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
# Save starters 2018:
saveRDS(object = starters2018, file = paste0("Data/starters/starters_2018",".Rds"))
#******************************************************************************#
#******************************************************************************#