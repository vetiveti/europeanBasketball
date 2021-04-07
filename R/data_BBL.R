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
pbp_g1 <- get_pbp(year,game_id)

# compute boxscore for teams:
pbp <- pbp_g1
team <- "A"

p3a <- get_p3a(pbp, team)
p3m <- get_p3m(pbp, team)
