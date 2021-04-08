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
team_h <- home_team
team_a <- away_team

min <- get_min(pbp, team)
p2a <- get_p2a(pbp, team)
p2m <- get_p2m(pbp, team)
p3a <- get_p3a(pbp, team)
p3m <- get_p3m(pbp, team)
fta <- get_fta(pbp, team)
ftm <- get_ftm(pbp, team)
# zusammen = total rebounds
reb <- get_reb(pbp, team)
rb_team <- get_rb_team(pbp, team)
# oder alleine
trb <- get_trb(pbp, team)
# 
orb <- get_orb(pbp, team)
drb <- get_drb(pbp, team)
stl <- get_stl(pbp, team)
# turnover stimmen noch nicht... 
# es fehlt einer.. wo ist der hin?
tov <- get_tov(pbp, team)
blk <- get_blk(pbp, team)
pf <- get_pf(pbp, team)
pfd <- get_pfd(pbp, team)

for_bx <- c("team")
boxscore_team_xy <- tibble(stats = for_bx,
                           min = min,
                           p2a = p2a,
                           p2m = p2m,
                           p3a = p3a,
                           p3m = p3m,
                           fta = fta,
                           ftm = ftm,
                           trb = trb,
                           orb = orb,
                           drb = drb,
                           stl = stl,
                           tov = tov,
                           blk = blk,
                           pf = pf,
                           pfd = pfd)

bx <- get_boxscore_team(pbp,team_h,team_a)
