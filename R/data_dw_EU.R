# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

setwd("~/Uni Tübingen/0. Masterarbeit/7. R/Masterarbeit")
source('~/Uni Tübingen/0. Masterarbeit/7. R/Masterarbeit/download_realGM.R')
#### install/load needed packages ####

#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("nbastatR")
#install.packages(jsonlite)

# load needed packages
library(nbastatR)
library(tidyverse)
library(dplyr)
library(jsonlite)
library(glue)
library(plm)
library(rvest)
#library(plyr)
library(lmtest) # for coeftest function

#devtools::install_github("solmos/eurolig")
library(eurolig)
library(purrr)
library(readr)
library(BAwiR)
library(openxlsx)

#devtools::install_github('bziarkowski/euRobasket')
library(euRobasket)

#download raw play by play
a <- get_raw_pbp_fibalivestats(742430)
# geht nicht
b <- get_raw_pbp_livefibaeurope(108510)

c <- get_stints_fibalivestats(742430)

url <- 'https://basketball.realgm.com/international/league/15/German-BBL/stats/2020/Totals/Qualified/All/points/All/desc/1/Regular_Season'
df <-  readHTMLTable(htmlParse(readLines(url)))

c <- get_scores_realgm('2017-03-01', '2017-03-07', 'Israeli BSL')


################################################################################
a <- teaminfo()

# 2018/2019 Final Four games
game_codes <- 1
season <- 2007

samplepbp <- map_df(game_codes, extractPbp, season = season)

stintstats <- getStintStats(samplepbp, players = "LARKIN, SHANE")
poss <- getPbpPoss(samplepbp)
on_off <- getOnOffStats(samplepbp, players = "LARKIN, SHANE")
plus <- getPlusMinus(samplepbp, players = "LARKIN, SHANE")
euroteams <- teaminfo

a <- extractTeamResults(team = "BAR", season = 2017)
b <- getTeamResults(team = "BAR", season = 2017)

df_bio_eur <- do_scraping_rosters(competition = "Euroleague", pcode = "007969", 
                                  year = "2017", verbose = TRUE, 
                                  r_user = "guillermo.vinue@uv.es")     
compet <- "ACB"
df <- do_join_games_bio(compet, acb_games_1718, acb_players_1718)
df1 <- do_add_adv_stats(df)
df2 <- do_stats(df1, "Total", "2017-2018", compet, "Regular Season")

df_bio <- scraping_rosters_euro("Euroleague", "005791", "2017", verbose = TRUE,
                                r_user = "guillermo.vinue@uv.es")

