# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# set working directory
setwd("~/Uni Tübingen/0. Masterarbeit/7. R/europeanBasketball")

# load packages
library(tidyverse, warn.conflicts = FALSE)

# load data
player_totals <- readRDS("Data/player_data_totals.Rds")
player_pg <- readRDS("Data/player_data_pg.Rds")

#******************************************************************************#
# NBA Efficiency & BBL EFFICIENCY
# NBA_eff = PTS + REB + STL + AST + BLK - TOV - MFG
efficiency <- player_totals %>% 
    mutate(nba_eff = pts + trb + stl +ast + blk - tov - (fga - fgm),
           bbl_eff = pts + trb + stl +ast + blk - tov - (fga - fgm) - (fta - ftm)) %>% 
    dplyr::select(player, min_p, year,team, bbl_eff, nba_eff)

# numbers do not coincide completely because assists are diffrently estimated in the nba and the bbl
# please confirm what the difference is

saveRDS(object = efficiency, file = paste0("Data/estimates/efficiency.Rds"))
