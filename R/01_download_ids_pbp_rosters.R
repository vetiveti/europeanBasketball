# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# source functions to use
source('functions/BBL_functions.R')
source('functions/pbp_actions.R')
source('functions/minutes_played.R')

library(tidyverse, warn.conflicts = FALSE)
library(zoo)

# Download game ID's & save: ----
#a <- BBL_game_ids(year = 2008:2018)
# save_rds <- ls(pattern = "identifiers_")
# saveRDS(object = identifiers_2008, file = paste0("Data/identifiers_2008",".Rds"))
# saveRDS(object = identifiers_2009, file = paste0("Data/identifiers_2009",".Rds"))
# saveRDS(object = identifiers_2010, file = paste0("Data/identifiers_2010",".Rds"))
# saveRDS(object = identifiers_2011, file = paste0("Data/identifiers_2011",".Rds"))
# saveRDS(object = identifiers_2012, file = paste0("Data/identifiers_2012",".Rds"))
# saveRDS(object = identifiers_2013, file = paste0("Data/identifiers_2013",".Rds"))
# saveRDS(object = identifiers_2014, file = paste0("Data/identifiers_2014",".Rds"))
# saveRDS(object = identifiers_2015, file = paste0("Data/identifiers_2015",".Rds"))
# saveRDS(object = identifiers_2016, file = paste0("Data/identifiers_2016",".Rds"))
# saveRDS(object = identifiers_2017, file = paste0("Data/identifiers_2017",".Rds"))
# saveRDS(object = identifiers_2018, file = paste0("Data/identifiers_2018",".Rds"))

# load identifiers:
game_id_files = paste0("Data/identifiers_", 2008:2018, ".Rds")
name <- gsub("\\.Rds$", "", game_id_files) %>% 
    gsub("Data/", "", .)
game_id_data <- lapply(game_id_files, readRDS)
names(game_id_data) <- gsub("\\.Rds$", "", name)

# get results safely, because some games have no data
safer_results <- possibly(get_pbp, otherwise = as_tibble("Error finding file"))

#******************************************************************************#
# Download PBP & save: ----
#' do that for every year separately
# year <- 2008
# results <- tibble()
# for (i in 1:nrow(game_id_data$identifiers_2008)) {
#     all_results <- safer_results(year,game_id_data$identifiers_2008[i,])
#     all_results$game_nr <- i
#     results<- bind_rows(results,all_results)
# 
# }
# saveRDS(object = results, file = paste0("Data/pbp",year,".Rds"))


#******************************************************************************#
# Download team rosters for every game: ----
# safer_roster <- possibly(get_rosters, otherwise = as_tibble("Error finding file"))
# 
# year <- 2008:2018
# 
# for (j in year) {
#     id_extract <- game_id_data[[paste0("identifiers_",j)]]
#     df_roster <- tibble()
#     
#     for (i in 1:nrow(id_extract)) {
#         current <- safer_roster(id_extract[i,],j)
#         df_roster <- bind_rows(df_roster,current)
#         
#     }
#     assign(paste0("rosters_",j),df_roster)
#     saveRDS(object = get(paste0("rosters_",j)),file = paste0("Data/rosters_",j,".Rds"))
# }

