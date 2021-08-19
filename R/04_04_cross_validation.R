# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# set working directory
setwd("~/Uni Tübingen/0. Masterarbeit/7. R/europeanBasketball")
source('functions/metrics_functions2.R')

# load packages
library(tidyverse, warn.conflicts = FALSE)
require(lmtest)
require(plm)
require(openxlsx)

#******************************************************************************#
# Wins Produced:----
players <- readRDS("Data/players_each_game.Rds")
teams <- readRDS("Data/teams_each_game.Rds") %>% 
    rename(game = game_nr) %>% 
    mutate(REBTM = fga - (opp_tov + opp_fgm + 0.4720415 * opp_ftm + drb - tov
                            - 0.4720415 * fta + orb))

marg_values <- readRDS("Data/estimates/marg_values_wp.Rds")
DREBMATE = 0.504    # Berri's number
tASTpm <- 0.725     # Berri's number
pos_avg <- readRDS("Data/estimates/position_average_wp.Rds")

#debugonce(wp)
wp.cv <- wp(players,teams,marg_values,DREBMATE,tASTpm,pos_avg) %>% 
    filter(., min_p > 0.0)

#******************************************************************************#
# Win Shares:----
players <- readRDS("Data/players_each_game.Rds")
teams <- readRDS("Data/teams_each_game.Rds") %>% 
    rename(game = game_nr)

#source('functions/metrics_functions.R')
#debugonce(ws)
ws.cv <- ws(players,teams) %>% 
    filter(min_p > 0.0) %>% 
    mutate(ws_pm = win_shares / min_p) %>% 
    dplyr::select(player,team,year,game,min_p,win_shares,ws_pm)

#******************************************************************************#
# BPM:----
player <- readRDS("Data/players_each_game.Rds") %>% 
    rename(min = min_p)

team <- readRDS("Data/teams_each_game.Rds") %>% 
    rename(game = game_nr)
games_total <- team %>%              # Count rows by group
    group_by(team,year) %>% 
    summarise(G_tot = n(), .groups ="drop")
team <- merge(team,games_total,
              by = c("team","year"))

point_differential_pg <- 0.0279743 # from regression
pos_role <- readRDS("Data/estimates/pos_role_BPM.Rds")

#debugonce(BPM)
bpm.cv <- BPM(player,team,pos_role) %>% 
    filter(min_p > 0.0) %>% 
    mutate(w_add_BPM = VORP * point_differential_pg * G_tot,
           w_add_BPM_pm = w_add_BPM / min_p) %>% 
    dplyr::select(player,team,year,game,min_p,w_add_BPM,w_add_BPM_pm)

#******************************************************************************#
control <- wp.cv %>% 
    dplyr::select(player,team,year,game,min_p,wp_total) %>% 
    filter(team == "FC Bayern München" & year == 2014) %>% 
    group_by(player,team, year) %>% 
    summarise_at(vars(wp_total), .funs = sum) %>% 
    ungroup()
sum(control$wp_total)

#******************************************************************************#
# stats1 <- merge(bpm.cv,wp.cv,
#                by = c("player","team","year","game","min_p"), all = TRUE)
# stats2 <- merge(stats1,ws.cv,
#                 by = c("player","team","year","game","min_p"), all = TRUE) %>% 
#     filter(.,team == "FC Bayern München" & year == 2014) %>% 
#     arrange(game)
# 
# team_totals <- readRDS("Data/team_data_totals.Rds")
# win_true <- team_totals %>% 
#     select(team,year,G,W,L)
# 
# p_2014_fcb <- player %>% 
#     filter(.,team == "FC Bayern München" & year == 2014) %>% 
#     arrange(game)
# unique(p_2014_fcb$game)
# 
# t_2014_fcb <- team %>% 
#     filter(.,team == "FC Bayern München" & year == 2014)
# 
# # split at 10 games of 30 to predict the remaining 20
# first10_p_2014_fcb <- filter(stats2, game <=17224) %>% 
#     group_by(player,team) %>% 
#     summarise_at(vars(ws_pm,w_add_BPM_pm,wp_pm), funs(weighted.mean(.,min_p))) %>% 
#         ungroup()
# play_time <- filter(stats2, game <=17224) %>% 
#     group_by(player,team) %>%
#     summarise_at(vars(min_p), .funs = sum) %>% 
#     ungroup()
# 
# first_p_2014_fcb <- merge(first10_p_2014_fcb,play_time,
#                           by = c("player","team"))
# 
# first10_t_2014_fcb <- filter(t_2014_fcb, game <=17224) %>% 
#     dplyr::select(team,G,W,L,min) %>% 
#     group_by(team) %>% 
#     summarise_at(vars(G:min), .funs = sum) %>% 
#     ungroup() %>%
#     rename(min_t = min)
# 
# first10_merged <- merge(first_p_2014_fcb,first10_t_2014_fcb,
#                         by = "team") %>% 
#     mutate(min_pct = min_p /min_t) 
# 
# sum(first10_merged$min_pct)
# 
# # last 20 games
# last20_t_2014_fcb <- filter(t_2014_fcb, game >17224) %>% 
#     dplyr::select(team,G,W,L,min) %>% 
#     group_by(team) %>% 
#     summarise_at(vars(G:min), .funs = sum) %>% 
#     ungroup() %>%
#     rename(min_t = min)
# 
# pred_merge <- merge(first10_merged,last20_t_2014_fcb,
#                     by = "team", suffixes = c("_b","_a")) %>% 
#     rename(min_to_play = min_t_a)
# 
# prediction <- pred_merge %>% 
#     mutate(min_p_pred = min_pct * min_to_play,
#            ws_pred = ws_pm * min_p_pred,
#            wp_pred = wp_pm * min_p_pred,
#            BPM_pred = w_add_BPM_pm * min_p_pred) %>% 
#     dplyr::select(player,team,min_p_pred,ws_pred,wp_pred,BPM_pred,W_a) %>% 
#     summarise_at(vars(min_p_pred,ws_pred,wp_pred,BPM_pred,W_a), .funs = sum) %>% 
#     mutate(W_a = W_a / 10)

#******************************************************************************#
# doing that for all teams at the same time in a given year
source('functions/prediction_functions.R')
require(Metrics)

team <- readRDS("Data/teams_each_game.Rds") %>% 
    rename(game = game_nr)

model1 <- merge(bpm.cv,wp.cv,
                by = c("player","team","year","game","min_p"), all = TRUE)
all_models <- merge(model1,ws.cv,
                    by = c("player","team","year","game","min_p"), all = TRUE) #%>% 
    # mutate(w_add_BPM_pm = ifelse(min_p > 8,w_add_BPM_pm,0.045),
    #       wp_pm = ifelse(min_p > 12,wp_pm,0.045),
    #       ws_pm = ifelse(min_p > 12,ws_pm,0.045))

frac <- 0.5
years <- 2014:2018
seed <- 12
source('functions/prediction_functions.R')
#debugonce(pred_rmse)
coolio <- pred_rmse(all_models,player,team,years,frac,seed)

reg1 <- lm(data = coolio,
           W_a ~wp_pred)
summary(reg1)

reg2 <- lm(data = coolio,
           W_a ~ws_pred)
summary(reg2)

reg3 <- lm(data = coolio,
           W_a ~BPM_pred)
summary(reg3)

cor(coolio$W_a,coolio$wp_pred)
cor(coolio$W_a,coolio$ws_pred)
cor(coolio$W_a,coolio$BPM_pred)

#******************************************************************************#
# Inputs
# year input
year_input <- 2018

# splitting the season into parts
fraction <- 0.5

# prepare the data
player <- readRDS("Data/players_each_game.Rds") %>% 
    rename(min = min_p)

team <- readRDS("Data/teams_each_game.Rds") %>% 
    rename(game = game_nr)
games_total <- team %>%              # Count rows by group
    group_by(team,year) %>% 
    summarise(G_tot = n(), .groups ="drop")
team <- merge(team,games_total,
              by = c("team","year"))

set.seed(1)
train_games <- team %>% 
    filter(., year == year_input) %>% 
    group_by(team) %>% 
    slice_max(.,order_by = game, prop = fraction) #%>% # , replace = FALSE

df_player <- player %>% 
    filter(., game %in% train_games$game) %>% 
    group_by(player,team,year,Pos.) %>% 
    mutate(G = 1) %>% 
    summarise_at(vars(min:pts,G), .funs = sum) %>% 
    ungroup() %>% 
    rename(min_p = min)

df_team <- team %>% 
    filter(., game %in% train_games$game) %>% 
    group_by(team,G_tot,year) %>% 
    summarise_at(vars(G:opp_pts), .funs = sum) %>% 
    ungroup() %>% 
    mutate(REBTM = fga - (opp_tov + opp_fgm + 0.4720415 * opp_ftm + drb - tov
                          - 0.4720415 * fta + orb))

#******************************************************************************#
source('functions/metrics_functions2.R')
marg_values <- readRDS("Data/estimates/marg_values_wp.Rds")
DREBMATE = 0.504    # Berri's number
tASTpm <- 0.725     # Berri's number
pos_avg <- readRDS("Data/estimates/position_average_wp.Rds")

#debugonce(wp)
wp.cv <- wp(df_player,df_team,marg_values,DREBMATE,tASTpm,pos_avg) %>% 
    filter(., min_p > 0.0)

#******************************************************************************#
#debugonce(ws)
ws.cv <- ws(df_player,df_team) %>% 
    filter(min_p > 0.0) %>% 
    mutate(ws_pm = win_shares / min_p) %>% 
    dplyr::select(player,team,year,min_p,win_shares,ws_pm,min_t)

#******************************************************************************#

point_differential_pg <- 0.0279743 # from regression
pos_role <- readRDS("Data/estimates/pos_role_BPM.Rds") %>% 
    filter(., year == year_input)


#debugonce(BPM)
bpm.cv <- BPM(df_player,df_team,pos_role) %>% 
    filter(min_p > 0.0) %>% 
    mutate(w_add_BPM = VORP * point_differential_pg * G_t,
           w_add_BPM_pm = w_add_BPM / min_p) %>% 
    dplyr::select(player,team,year,min_p,w_add_BPM,w_add_BPM_pm)

#******************************************************************************#
# reality check
real_wins <- readRDS("Data/teams_each_game.Rds") %>% 
    rename(game = game_nr) %>% 
    filter(., game %in% train_games$game) %>% 
    dplyr::select(team,year,G,W,L) %>% 
    group_by(team,year) %>% 
    summarise_at(vars(G:L), sum)

model1 <- merge(bpm.cv,wp.cv,
                by = c("player","team","year","min_p"), all = TRUE)
all_models <- merge(model1,ws.cv,
                    by = c("player","team","year","min_p"), all = TRUE) %>% 
    group_by(team) %>% 
    summarise_at(vars(win_shares,wp_total,w_add_BPM), sum)

win_train <- merge(all_models,real_wins,by ="team")

reg1 <- lm(data = win_train,
           W ~ wp_total)
summary(reg1)

reg2 <- lm(data = win_train,
           W ~win_shares)
summary(reg2)

reg3 <- lm(data = win_train,
           W ~w_add_BPM)
summary(reg3)

#******************************************************************************#
# now the prediction part
model1 <- merge(bpm.cv,wp.cv,
                by = c("player","team","year","min_p"), all = TRUE)
all_models <- merge(model1,ws.cv,
                    by = c("player","team","year","min_p"), all = TRUE) %>% 
    mutate(min_pct = min_p / min_t)

minutes_to_play <- readRDS("Data/teams_each_game.Rds") %>% 
    rename(game = game_nr) %>% 
    filter(., !game %in% train_games$game & year == year_input) %>% 
    dplyr::select(team,year,G,W,L,min) %>% 
    group_by(team,year) %>% 
    summarise_at(vars(G:min), sum) %>% 
    rename(min_to_go = min,
           W_to_go = W,
           G_to_go = G,
           L_to_go = L,)

prediction <- merge(all_models,minutes_to_play,
                     by = c("team","year")) %>% 
    mutate(min_pred = min_pct * min_to_go,
           ws_pred = ws_pm * min_pred,
           wp_pred = wp_pm * min_pred,
           BPM_pred = w_add_BPM_pm * min_pred) %>% 
    dplyr::select(player,team,min_pred,min_t,G_to_go,W_to_go,ws_pred,wp_pred,BPM_pred) %>% 
    group_by(team,G_to_go,W_to_go,min_t) %>% 
    summarise_at(vars(min_pred,ws_pred,wp_pred,BPM_pred), .funs = sum) %>% 
    ungroup()

#******************************************************************************#
# Power, Error and correlation
# MRSE
require(Metrics)
error <- prediction %>% 
    mutate(WP_RMSE = rmse(W_to_go,wp_pred),
           WS_RMSE = rmse(W_to_go,ws_pred),
           BPM_RMSE = rmse(W_to_go,BPM_pred))

# R^2
reg1 <- lm(data = prediction,
           W_to_go ~ wp_pred)
summary(reg1)

reg2 <- lm(data = prediction,
           W_to_go ~ws_pred)
summary(reg2)

reg3 <- lm(data = prediction,
           W_to_go ~BPM_pred)
summary(reg3)

# Correlation
cor(prediction$W_to_go,prediction$wp_pred)
cor(prediction$W_to_go,prediction$ws_pred)
cor(prediction$W_to_go,prediction$BPM_pred)

#******************************************************************************#









##################################
a <- t(prediction_teams) %>% as_tibble()
colnames(a) <- prediction_teams$team
a$first <- colnames(prediction_teams)

a <- a %>% 
    relocate(first, .before = 1)
a <- a[-1,]
