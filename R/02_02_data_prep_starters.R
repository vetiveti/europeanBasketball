# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# source functions to use
source('functions/BBL_functions.R')

# set working directory
setwd("~/Uni Tübingen/0. Masterarbeit/7. R/europeanBasketball")

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
pbp_files = paste0("Data/clean_pbp/pbp_", 2010:2020, ".Rds")
name <- gsub("\\.Rds$", "", pbp_files) %>% 
    gsub("Data/", "", .)
pbp_data <- lapply(pbp_files, readRDS)
names(pbp_data) <- gsub("\\.Rds$", "", name)

#******************************************************************************#
# Load roster files:----
roster_files = paste0("Data/rosters_", 2008:2020, ".Rds")
name <- gsub("\\.Rds$", "", roster_files) %>% 
    gsub("Data/", "", .)
roster_data <- lapply(roster_files, readRDS)
names(roster_data) <- gsub("\\.Rds$", "", name)

# YEAR BY YEAR:  ----
#******************************************************************************#
# calculate starters 2010: ----
pbp <- pbp_data$`clean_pbp/pbp_2010`
roster <- roster_data$rosters_2010
roster2010 <- calc_starters(pbp,roster) 

starters2010 <- roster2010 %>% 
    mutate(starter_Q5 = replace_na(starter_Q5,0),
           starter_Q6 = replace_na(starter_Q6,0))

#
pbp_starter <- pbp %>%
    mutate(game_q = game_id * quarter)

pbp_game_1 <- filter(pbp_starter,
                     game_q == 8940*2)

solve <- calc_starters(pbp_game_1,roster)
view(solve)
debugonce(calc_starters)
solve2 <- calc_starters(pbp_game_1,roster)
#
starters2010$starter_Q1[starters2010$game_nr == 8704 & starters2010$Player == "Michael, Crowell"] <- 0
starters2010$starter_Q2[starters2010$game_nr == 8735 & starters2010$Player == "Rickey, Paulding"] <- 1
starters2010$starter_Q1[starters2010$game_nr == 8721 & starters2010$Player == "Christoph, Tetzner"] <- 0
starters2010$starter_Q5[starters2010$game_nr == 8779 & starters2010$Player == "Rastko, Dramicanin"] <- 1
starters2010$starter_Q1[starters2010$game_nr == 8669 & starters2010$Player == "Jamaal, Tatum"] <- 0
starters2010$starter_Q1[starters2010$game_nr == 8825 & starters2010$Player == "Coleman, Collins"] <- 0
starters2010$starter_Q5[starters2010$game_nr == 8855 & starters2010$Player == "Per, GÃ¼nther"] <- 1
starters2010$starter_Q1[starters2010$game_nr == 8909 & starters2010$Player == "Quantez, Robertson"] <- 1
starters2010$starter_Q5[starters2010$game_nr == 8937 & starters2010$Player == "Marius, Nolte"] <- 1
starters2010$starter_Q4[starters2010$game_nr == 8938 & starters2010$Player == "John, Goldsberry"] <- 0
starters2010$starter_Q2[starters2010$game_nr == 8940 & starters2010$Player == "Branislav, Ratkovica"] <- 1

anzahl <- unique(starters2010$game_nr) %>% as_tibble() %>% nrow()
print(anzahl)
sum(starters2010$starter_Q1) / 10
sum(starters2010$starter_Q2) / 10
sum(starters2010$starter_Q3) / 10
sum(starters2010$starter_Q4) / 10
sum(starters2010$starter_Q5) / 10
sum(starters2010$starter_Q6) / 10

#******************************************************************************#
# Save starters 2010:
saveRDS(object = starters2010, file = paste0("Data/starters/starters_2010",".Rds"))

#******************************************************************************#
# calculate starters 2011: ----
pbp <- pbp_data$`clean_pbp/pbp_2011`
roster <- roster_data$rosters_2011
roster2011 <- calc_starters(pbp,roster) 

starters2011 <- roster2011 %>% 
    mutate(starter_Q5 = replace_na(starter_Q5,0),
           starter_Q6 = replace_na(starter_Q6,0))

#
# pbp_starter <- pbp %>%
#     mutate(game_q = game_id * quarter)
# 
# pbp_game_1 <- filter(pbp_starter,
#                      game_q == 11160*6)
# 
# solve <- calc_starters(pbp_game_1,roster)
# view(solve)
# debugonce(calc_starters)
# solve2 <- calc_starters(pbp_game_1,roster)
#
starters2011$starter_Q2[starters2011$game_nr == 10906 & starters2011$Player == "Louis, Campbell"] <- 0
starters2011$starter_Q2[starters2011$game_nr == 10921 & starters2011$Player == "Louis, Dale"] <- 1
starters2011$starter_Q3[starters2011$game_nr == 10936 & starters2011$Player == "Velimir, Radinovic"] <- 0
starters2011$starter_Q3[starters2011$game_nr == 10953 & starters2011$Player == "Zvonko, Buljan"] <- 0
starters2011$starter_Q3[starters2011$game_nr == 10953 & starters2011$Player == "Andrej, Mangold"] <- 0
starters2011$starter_Q5[starters2011$game_nr == 10956 & starters2011$Player == "Joshua Adam, Young"] <- 1
starters2011$starter_Q5[starters2011$game_nr == 10958 & starters2011$Player == "Nate, Linhart"] <- 1
starters2011$starter_Q3[starters2011$game_nr == 10981 & starters2011$Player == "Sead, Sehovic"] <- 1
starters2011$starter_Q4[starters2011$game_nr == 10981 & starters2011$Player == "Kenny, Hasbrouck"] <- 1
starters2011$starter_Q4[starters2011$game_nr == 11001 & starters2011$Player == "Dominik, Spohr"] <- 1
starters2011$starter_Q1[starters2011$game_nr == 11020 & starters2011$Player == "LaMarr, Greer"] <- 1
starters2011$starter_Q3[starters2011$game_nr == 11046 & starters2011$Player == "Demond, Greene"] <- 1
starters2011$starter_Q5[starters2011$game_nr == 11102 & starters2011$Player == "Adam, Chubb"] <- 1
starters2011$starter_Q2[starters2011$game_nr == 11110 & starters2011$Player == "Andreas, Wenzl"] <- 0
starters2011$starter_Q3[starters2011$game_nr == 11115 & starters2011$Player == "Simonas, Serapinas"] <- 1
starters2011$starter_Q1[starters2011$game_nr == 11124 & starters2011$Player == "Steven Michael, Esterkamp"] <- 1
starters2011$starter_Q3[starters2011$game_nr == 11078 & starters2011$Player == "Jordan, Hasquet"] <- 1
starters2011$starter_Q5[starters2011$game_nr == 11135 & starters2011$Player == "Anthony, Smith"] <- 1
starters2011$starter_Q5[starters2011$game_nr == 11160 & starters2011$Player == "Brandon, Thomas"] <- 1
starters2011$starter_Q6[starters2011$game_nr == 11160 & starters2011$Player == "Brandon, Thomas"] <- 1
starters2011$starter_Q6[starters2011$game_nr == 11160 & starters2011$Player == "Guido, GrÃ¼nheid"] <- 1
starters2011$starter_Q5[starters2011$game_nr == 11167 & starters2011$Player == "Robin, Benzing"] <- 1
starters2011$starter_Q1[starters2011$game_nr == 11168 & starters2011$Player == "Michael, Thompson"] <- 1
starters2011$starter_Q5[starters2011$game_nr == 11178 & starters2011$Player == "Devin, Gibson"] <- 1
starters2011$starter_Q1[starters2011$game_nr == 11183 & starters2011$Player == "Jermareo, Davidson"] <- 0
starters2011$starter_Q1[starters2011$game_nr == 11183 & starters2011$Player == "Quantez, Robertson"] <- 1
starters2011$starter_Q1[starters2011$game_nr == 11183 & starters2011$Player == "Danilo, Barthel"] <- 0
starters2011$starter_Q1[starters2011$game_nr == 11183 & starters2011$Player == "Fabian, Franke"] <- 0


anzahl <- unique(starters2011$game_nr) %>% as_tibble() %>% nrow()
print(anzahl)
sum(starters2011$starter_Q1) / 10
sum(starters2011$starter_Q2) / 10
sum(starters2011$starter_Q3) / 10
sum(starters2011$starter_Q4) / 10
sum(starters2011$starter_Q5) / 10
sum(starters2011$starter_Q6) / 10

#******************************************************************************#
# Save starters 2011:
saveRDS(object = starters2011, file = paste0("Data/starters/starters_2011",".Rds"))

#******************************************************************************#
# calculate starters 2012: ----
pbp <- pbp_data$`clean_pbp/pbp_2012`
roster <- roster_data$rosters_2012
roster2012 <- calc_starters(pbp,roster) 

starters2012 <- roster2012 %>% 
    mutate(starter_Q5 = replace_na(starter_Q5,0))

#
# pbp_starter <- pbp %>%
#     mutate(game_q = game_id * quarter)
# 
# pbp_game_1 <- filter(pbp_starter,
#                      game_q == 13556*6)
# 
# solve <- calc_starters(pbp_game_1,roster)
# view(solve)
# debugonce(calc_starters)
# solve2 <- calc_starters(pbp_game_1,roster)
#
starters2012$starter_Q4[starters2012$game_nr == 13261 & starters2012$Player == "Barry, Stewart"] <- 1
starters2012$starter_Q4[starters2012$game_nr == 13291 & starters2012$Player == "Stefan, Schmidt"] <- 0
starters2012$starter_Q5[starters2012$game_nr == 13324 & starters2012$Player == "Kyle, Weems"] <- 1
starters2012$starter_Q5[starters2012$game_nr == 13360 & starters2012$Player == "Phillipp, Heyden"] <- 1
starters2012$starter_Q5[starters2012$game_nr == 13361 & starters2012$Player == "Alex, King"] <- 1
starters2012$starter_Q3[starters2012$game_nr == 13362 & starters2012$Player == "Tom, SpÃ¶ler"] <- 0
starters2012$starter_Q4[starters2012$game_nr == 13373 & starters2012$Player == "Sergerio, Gipson"] <- 0
starters2012$starter_Q4[starters2012$game_nr == 13422 & starters2012$Player == "Jonas, Wohlfarth-Bottermann"] <- 0
starters2012$starter_Q1[starters2012$game_nr == 13455 & starters2012$Player == "Mahir, Agva"] <- 0
starters2012$starter_Q2[starters2012$game_nr == 13409 & starters2012$Player == "Marius, Nolte"] <- 0
starters2012$starter_Q1[starters2012$game_nr == 13487 & starters2012$Player == "Quantez, Robertson"] <- 1
starters2012$starter_Q1[starters2012$game_nr == 13506 & starters2012$Player == "Mahir, Agva"] <- 0
starters2012$starter_Q1[starters2012$game_nr == 13532 & starters2012$Player == "Andrej, Mangold"] <- 1
starters2012$starter_Q5[starters2012$game_nr == 13556 & starters2012$Player == "Yotam, Halperin"] <- 1


anzahl <- unique(starters2012$game_nr) %>% as_tibble() %>% nrow()
print(anzahl)
sum(starters2012$starter_Q1) / 10
sum(starters2012$starter_Q2) / 10
sum(starters2012$starter_Q3) / 10
sum(starters2012$starter_Q4) / 10
sum(starters2012$starter_Q5) / 10

#******************************************************************************#
# Save starters 2012:
saveRDS(object = starters2012, file = paste0("Data/starters/starters_2012",".Rds"))

#******************************************************************************#
# calculate starters 2013: ----
pbp <- pbp_data$`clean_pbp/pbp_2013`
roster <- roster_data$rosters_2013
roster2013 <- calc_starters(pbp,roster) 

starters2013 <- roster2013 %>% 
    mutate(starter_Q5 = replace_na(starter_Q5,0),
           starter_Q6 = replace_na(starter_Q6,0),
           starter_Q7 = replace_na(starter_Q7,0))

#
pbp_starter <- pbp %>%
    mutate(game_q = game_id * quarter)

pbp_game_1 <- filter(pbp_starter,
                     game_q == 15537*4)

solve <- calc_starters(pbp_game_1,roster)
sum(solve$starter_Q1)
view(solve)
debugonce(calc_starters)
solve2 <- calc_starters(pbp_game_1,roster)
#
starters2013$starter_Q2[starters2013$game_nr == 15276 & starters2013$Player == "Nils, Mittmann"] <- 1
starters2013$starter_Q5[starters2013$game_nr == 15344 & starters2013$Player == "Djordje, Pantelic"] <- 1
starters2013$starter_Q3[starters2013$game_nr == 15338 & starters2013$Player == "Boris, Savovic"] <- 1
starters2013$starter_Q4[starters2013$game_nr == 15357 & starters2013$Player == "Keaton, Nankivil"] <- 1
starters2013$starter_Q5[starters2013$game_nr == 15385 & starters2013$Player == "Andrea, Crosariol"] <- 1
starters2013$starter_Q4[starters2013$game_nr == 15400 & starters2013$Player == "Dominique, Johnson"] <- 1
starters2013$starter_Q5[starters2013$game_nr == 15433 & starters2013$Player == "John, Bryant"] <- 1
starters2013$starter_Q5[starters2013$game_nr == 15433 & starters2013$Player == "Deon, Thompson"] <- 1
starters2013$starter_Q3[starters2013$game_nr == 15443 & starters2013$Player == "Nils, Mittmann"] <- 0
starters2013$starter_Q1[starters2013$game_nr == 15489 & starters2013$Player == "Tyree, Chambers"] <- 0
starters2013$starter_Q6[starters2013$game_nr == 15515 & starters2013$Player == "Benas, Veikalas"] <- 1
starters2013$starter_Q3[starters2013$game_nr == 15576 & starters2013$Player == "Max, Merz"] <- 1
starters2013$starter_Q4[starters2013$game_nr == 15526 & starters2013$Player == "Jamar, Smith"] <- 0
starters2013$starter_Q4[starters2013$game_nr == 15537 & starters2013$Player == "Aaron, Doornekamp"] <- 1

anzahl <- unique(starters2013$game_nr) %>% as_tibble() %>% nrow()
print(anzahl)
sum(starters2013$starter_Q1) / 10
sum(starters2013$starter_Q2) / 10
sum(starters2013$starter_Q3) / 10
sum(starters2013$starter_Q4) / 10
sum(starters2013$starter_Q5) / 10
sum(starters2013$starter_Q6) / 10
sum(starters2013$starter_Q7) / 10

#******************************************************************************#
# Save starters 2013:
saveRDS(object = starters2013, file = paste0("Data/starters/starters_2013",".Rds"))

#******************************************************************************#
# calculate starters 2014: ----
pbp <- pbp_data$`clean_pbp/pbp_2014`
roster <- roster_data$rosters_2014
roster2014 <- calc_starters(pbp,roster) 

starters2014 <- roster2014 %>% 
    mutate(starter_Q5 = replace_na(starter_Q5,0),
           starter_Q6 = replace_na(starter_Q6,0))

pbp_table <- pbp %>% 
    filter(game_id == 17099, quarter == 2, nummer_aktion >= 2, nummer_aktion <=9) %>% 
    dplyr::select(spielzeit_sec,aktion,zusatzinfo_1,zusatzinfo_3,
                  resultat,spielstand_A,spielstand_B,Player_1,Club_1,Player_2,Club_2)

print(xtable(pbp_table, type = "latex",tabular.environment="longtable"), file = "export/pbp_table.tex",)
#
# pbp_starter <- pbp %>%
#     mutate(game_q = game_id * quarter)
# 
# pbp_game_1 <- filter(pbp_starter,
#                      game_q == 17276*1)
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
#                      game_q == 18412*1)
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
starters2016$starter_Q5[starters2016$game_nr == 19817 & starters2016$Player == "Albert Jay, English"] <- 1

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
#                      game_q == 22132 * 4)
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
# calculate starters 2019: ----
pbp <- pbp_data$`clean_pbp/pbp_2019`
roster <- roster_data$rosters_2019
roster2019 <- calc_starters(pbp,roster) 

# pbp_starter <- pbp %>%
#     mutate(game_q = game_id * quarter)
# 
# pbp_game_1 <- filter(pbp_starter,
#                      game_q == 24156 * 1)
# 
# # source functions to use
# source('functions/BBL_functions.R')
# source('functions/pbp_actions.R')
# solve <- calc_starters(pbp_game_1,roster)
# view(solve)
# debugonce(calc_starters)
# solve2 <- calc_starters(pbp_game_1,roster)

starters2019 <- roster2019 %>% 
    mutate(starter_Q5 = replace_na(starter_Q5,0))
starters2019$starter_Q4[starters2019$game_nr == 22072 & starters2019$Player == "Brandon, Thomas"] <- 1
starters2019$starter_Q5[starters2019$game_nr == 24005 & starters2019$Player == "Elias, Harris"] <- 1
starters2019$starter_Q5[starters2019$game_nr == 24077 & starters2019$Player == "Leon, Kratzer"] <- 1
starters2019$starter_Q1[starters2019$game_nr == 24094 & starters2019$Player == "Jaleen, Smith"] <- 1
starters2019$starter_Q4[starters2019$game_nr == 24094 & starters2019$Player == "Jaleen, Smith"] <- 1
starters2019$starter_Q1[starters2019$game_nr == 24156 & starters2019$Player == "Quantez, Robertson"] <- 1


#starters2018$starter_Q4[starters2018$game_nr == 22326 & grepl("Brooks",starters2018$Player)] <- 1

anzahl <- unique(starters2019$game_nr) %>% as_tibble() %>% nrow()
print(anzahl)
sum(starters2019$starter_Q1) / 10
sum(starters2019$starter_Q2) / 10
sum(starters2019$starter_Q3) / 10
sum(starters2019$starter_Q4) / 10
sum(starters2019$starter_Q5) / 10
sum(starters2019$starter_Q6) / 10

#******************************************************************************#
# Save starters 2019:
saveRDS(object = starters2019, file = paste0("Data/starters/starters_2019",".Rds"))

#******************************************************************************#
# calculate starters 2020: ----
pbp <- pbp_data$`clean_pbp/pbp_2020`
roster <- roster_data$rosters_2020
roster2020 <- calc_starters(pbp,roster) 

pbp_starter <- pbp %>%
    mutate(game_q = game_id * quarter)

pbp_game_1 <- filter(pbp_starter,
                     game_q == 25656 * 2)

# source functions to use
source('functions/BBL_functions.R')
source('functions/pbp_actions.R')
solve <- calc_starters(pbp_game_1,roster)
view(solve)
debugonce(calc_starters)
solve2 <- calc_starters(pbp_game_1,roster)

starters2020 <- roster2020 %>% 
    mutate(starter_Q5 = replace_na(starter_Q5,0))
starters2020$starter_Q4[starters2020$game_nr == 25499 & starters2020$Player == "Dennis, Clifford"] <- 1
starters2020$starter_Q5[starters2020$game_nr == 25511 & starters2020$Player == "Wes, Clark"] <- 1
starters2020$starter_Q3[starters2020$game_nr == 25564 & starters2020$Player == "Brandon, Thomas"] <- 1
starters2020$starter_Q3[starters2020$game_nr == 25598 & starters2020$Player == "Benjamin, Lischka"] <- 1
starters2020$starter_Q5[starters2020$game_nr == 25613 & starters2020$Player == "Robin, Christen"] <- 1
starters2020$starter_Q4[starters2020$game_nr == 25621 & starters2020$Player == "Rasheed, Moore"] <- 1
starters2020$starter_Q5[starters2020$game_nr == 25624 & starters2020$Player == "Haywood, Highsmith"] <- 1
starters2020$starter_Q2[starters2020$game_nr == 25656 & starters2020$Player == "Paul, Zipser"] <- 1

anzahl <- unique(starters2020$game_nr) %>% as_tibble() %>% nrow()
print(anzahl)
sum(starters2020$starter_Q1) / 10
sum(starters2020$starter_Q2) / 10
sum(starters2020$starter_Q3) / 10
sum(starters2020$starter_Q4) / 10
sum(starters2020$starter_Q5) / 10

#******************************************************************************#
# Save starters 2020:
saveRDS(object = starters2020, file = paste0("Data/starters/starters_2020",".Rds"))
#******************************************************************************#