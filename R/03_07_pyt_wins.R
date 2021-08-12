# Clear Console
cat("\014")

# remove Environment
rm(list=ls())


library(tidyverse)

team_totals <- readRDS("Data/team_data_totals.Rds")

#******************************************************************************#
# Pythagorean wins:----
pyt_wins <- team_totals %>% 
    mutate(pyt_wins_perc = (pts^13.91)/ (pts^13.91+opp_pts^13.931),
           pyt_wins = pyt_wins_perc * G) %>% 
    select(year,team,pyt_wins)

saveRDS(object = pyt_wins, file = paste0("Data/estimates/pyt_wins.Rds"))
