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

# #******************************************************************************#
# # Wins Produced:----
# players <- readRDS("Data/players_each_game.Rds")
# teams <- readRDS("Data/teams_each_game.Rds") %>% 
#     rename(game = game_nr) %>% 
#     mutate(REBTM = fga - (opp_tov + opp_fgm + 0.4720415 * opp_ftm + drb - tov
#                             - 0.4720415 * fta + orb))
# 
# marg_values <- readRDS("Data/estimates/marg_values_wp.Rds")
# DREBMATE = 0.504    # Berri's number
# tASTpm <- 0.725     # Berri's number
# pos_avg <- readRDS("Data/estimates/position_average_wp.Rds")
# 
# #debugonce(wp)
# wp.cv <- wp(players,teams,marg_values,DREBMATE,tASTpm,pos_avg) %>% 
#     filter(., min_p > 0.0)
# 
# #******************************************************************************#
# # Win Shares:----
# players <- readRDS("Data/players_each_game.Rds")
# teams <- readRDS("Data/teams_each_game.Rds") %>% 
#     rename(game = game_nr)
# 
# #source('functions/metrics_functions.R')
# #debugonce(ws)
# ws.cv <- ws(players,teams) %>% 
#     filter(min_p > 0.0) %>% 
#     mutate(ws_pm = win_shares / min_p) %>% 
#     dplyr::select(player,team,year,game,min_p,win_shares,ws_pm)
# 
# #******************************************************************************#
# # BPM:----
# player <- readRDS("Data/players_each_game.Rds") %>% 
#     rename(min = min_p)
# 
# team <- readRDS("Data/teams_each_game.Rds") %>% 
#     rename(game = game_nr)
# games_total <- team %>%              # Count rows by group
#     group_by(team,year) %>% 
#     summarise(G_tot = n(), .groups ="drop")
# team <- merge(team,games_total,
#               by = c("team","year"))
# 
# point_differential_pg <- 0.0279743 # from regression
# pos_role <- readRDS("Data/estimates/pos_role_BPM.Rds")
# 
# #debugonce(BPM)
# bpm.cv <- BPM(player,team,pos_role) %>% 
#     filter(min_p > 0.0) %>% 
#     mutate(w_add_BPM = VORP * point_differential_pg * G_tot,
#            w_add_BPM_pm = w_add_BPM / min_p) %>% 
#     dplyr::select(player,team,year,game,min_p,w_add_BPM,w_add_BPM_pm)
# 
# #******************************************************************************#
# control <- wp.cv %>% 
#     dplyr::select(player,team,year,game,min_p,wp_total) %>% 
#     filter(team == "FC Bayern München" & year == 2014) %>% 
#     group_by(player,team, year) %>% 
#     summarise_at(vars(wp_total), .funs = sum) %>% 
#     ungroup()
# sum(control$wp_total)
# 
# #******************************************************************************#
# # stats1 <- merge(bpm.cv,wp.cv,
# #                by = c("player","team","year","game","min_p"), all = TRUE)
# # stats2 <- merge(stats1,ws.cv,
# #                 by = c("player","team","year","game","min_p"), all = TRUE) %>% 
# #     filter(.,team == "FC Bayern München" & year == 2014) %>% 
# #     arrange(game)
# # 
# # team_totals <- readRDS("Data/team_data_totals.Rds")
# # win_true <- team_totals %>% 
# #     select(team,year,G,W,L)
# # 
# # p_2014_fcb <- player %>% 
# #     filter(.,team == "FC Bayern München" & year == 2014) %>% 
# #     arrange(game)
# # unique(p_2014_fcb$game)
# # 
# # t_2014_fcb <- team %>% 
# #     filter(.,team == "FC Bayern München" & year == 2014)
# # 
# # # split at 10 games of 30 to predict the remaining 20
# # first10_p_2014_fcb <- filter(stats2, game <=17224) %>% 
# #     group_by(player,team) %>% 
# #     summarise_at(vars(ws_pm,w_add_BPM_pm,wp_pm), funs(weighted.mean(.,min_p))) %>% 
# #         ungroup()
# # play_time <- filter(stats2, game <=17224) %>% 
# #     group_by(player,team) %>%
# #     summarise_at(vars(min_p), .funs = sum) %>% 
# #     ungroup()
# # 
# # first_p_2014_fcb <- merge(first10_p_2014_fcb,play_time,
# #                           by = c("player","team"))
# # 
# # first10_t_2014_fcb <- filter(t_2014_fcb, game <=17224) %>% 
# #     dplyr::select(team,G,W,L,min) %>% 
# #     group_by(team) %>% 
# #     summarise_at(vars(G:min), .funs = sum) %>% 
# #     ungroup() %>%
# #     rename(min_t = min)
# # 
# # first10_merged <- merge(first_p_2014_fcb,first10_t_2014_fcb,
# #                         by = "team") %>% 
# #     mutate(min_pct = min_p /min_t) 
# # 
# # sum(first10_merged$min_pct)
# # 
# # # last 20 games
# # last20_t_2014_fcb <- filter(t_2014_fcb, game >17224) %>% 
# #     dplyr::select(team,G,W,L,min) %>% 
# #     group_by(team) %>% 
# #     summarise_at(vars(G:min), .funs = sum) %>% 
# #     ungroup() %>%
# #     rename(min_t = min)
# # 
# # pred_merge <- merge(first10_merged,last20_t_2014_fcb,
# #                     by = "team", suffixes = c("_b","_a")) %>% 
# #     rename(min_to_play = min_t_a)
# # 
# # prediction <- pred_merge %>% 
# #     mutate(min_p_pred = min_pct * min_to_play,
# #            ws_pred = ws_pm * min_p_pred,
# #            wp_pred = wp_pm * min_p_pred,
# #            BPM_pred = w_add_BPM_pm * min_p_pred) %>% 
# #     dplyr::select(player,team,min_p_pred,ws_pred,wp_pred,BPM_pred,W_a) %>% 
# #     summarise_at(vars(min_p_pred,ws_pred,wp_pred,BPM_pred,W_a), .funs = sum) %>% 
# #     mutate(W_a = W_a / 10)
# 
# #******************************************************************************#
# # doing that for all teams at the same time in a given year
# source('functions/prediction_functions.R')
# require(Metrics)
# 
# team <- readRDS("Data/teams_each_game.Rds") %>% 
#     rename(game = game_nr)
# 
# model1 <- merge(bpm.cv,wp.cv,
#                 by = c("player","team","year","game","min_p"), all = TRUE)
# all_models <- merge(model1,ws.cv,
#                     by = c("player","team","year","game","min_p"), all = TRUE) #%>% 
#     # mutate(w_add_BPM_pm = ifelse(min_p > 8,w_add_BPM_pm,0.045),
#     #       wp_pm = ifelse(min_p > 12,wp_pm,0.045),
#     #       ws_pm = ifelse(min_p > 12,ws_pm,0.045))
# 
# frac <- 0.5
# years <- 2014:2018
# seed <- 12
# source('functions/prediction_functions.R')
# #debugonce(pred_rmse)
# coolio <- pred_rmse(all_models,player,team,years,frac,seed)
# 
# reg1 <- lm(data = coolio,
#            W_a ~wp_pred)
# summary(reg1)
# 
# reg2 <- lm(data = coolio,
#            W_a ~ws_pred)
# summary(reg2)
# 
# reg3 <- lm(data = coolio,
#            W_a ~BPM_pred)
# summary(reg3)
# 
# cor(coolio$W_a,coolio$wp_pred)
# cor(coolio$W_a,coolio$ws_pred)
# cor(coolio$W_a,coolio$BPM_pred)

#******************************************************************************#
# Inputs
# year input
year_input <- 2017

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
    slice_max(., order_by = game, prop = fraction) #%>% # , replace = FALSE , order_by = game

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
    group_by(team) %>% 
    mutate(players = n()) %>% 
    ungroup() %>% 
    mutate(w_add_BPM = VORP * point_differential_pg * G_t,
           w_add_BPM_pm = w_add_BPM / min_p) %>% 
    dplyr::select(player,team,year,min_p,w_add_BPM,w_add_BPM_pm, VORP)

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

prediction_stats <- merge(all_models,minutes_to_play,
                     by = c("team","year")) %>% 
    mutate(min_pred = min_pct * min_to_go,
           ws_pred = ws_pm * min_pred,
           wp_pred = wp_pm * min_pred,
           BPM_pred = VORP * point_differential_pg * G_to_go) %>% 
    dplyr::select(player,team,min_pred,min_t,G_to_go,W_to_go,ws_pred,wp_pred,BPM_pred)

prediction <- prediction_stats %>% 
    group_by(team,G_to_go,W_to_go,min_t) %>% 
    summarise_at(vars(min_pred,ws_pred,wp_pred,BPM_pred), .funs = sum) %>% 
    ungroup() %>% 
    mutate(dumbo = G_to_go / 2,
           dumbo2 = real_wins$W / real_wins$G * G_to_go,
           BPM_pred2 = BPM_pred + (G_to_go / 2))

#******************************************************************************#
# Power, Error and correlation
# MRSE
require(Metrics)
error <- prediction %>% 
    mutate(WP_RMSE = rmse(W_to_go,wp_pred),
           WS_RMSE = rmse(W_to_go,ws_pred),
           BPM_RMSE = rmse(W_to_go,BPM_pred),
           BPM2_RMSE = rmse(W_to_go,BPM_pred2),
           dumbo_RMSE = rmse(W_to_go,dumbo),
           dumbo2_RMSE = rmse(W_to_go,dumbo2))
print(error)
# R^2
reg1 <- lm(data = prediction,
           W_to_go ~ wp_pred)
summary(reg1)



reg2 <- lm(data = prediction,
           W_to_go ~ws_pred)
summary(reg2)



reg3 <- lm(data = prediction,
           W_to_go ~ BPM_pred)
summary(reg3)

# plots for correlation regressions
ggplot(data = prediction, aes(x = wp_pred, y = W_to_go)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

ggplot(data = prediction, aes(x = ws_pred, y = W_to_go)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)
ggplot(data = prediction, aes(x = BPM_pred, y = W_to_go)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

reg4 <- lm(data = prediction,
           W_to_go ~dumbo)
summary(reg4)

ggplot(data = prediction, aes(x = dumbo, y = W_to_go)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

reg5 <- lm(data = prediction,
           W_to_go ~dumbo2)
summary(reg5)

ggplot(data = prediction, aes(x = dumbo2, y = W_to_go)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)


#******************************************************************************#
#******************************************************************************#
# Retrodiction (minutes to play are known in advance)
# minutes per player
retro_player <- player %>% 
    filter(., !game %in% train_games$game & year == year_input) %>% 
    group_by(player,team,year) %>% 
    summarise_at(vars(min), .funs = sum) %>% 
    ungroup() %>% 
    rename(min_to_go = min)

# number of games to predict and real wins for each team
team_stats_left <- team %>% 
    filter(., !game %in% train_games$game & year == year_input) %>% 
    group_by(team,G_tot) %>% 
    summarise_at(vars(G,W), .funs = sum) %>% 
    ungroup() %>%
    mutate(games_left = G) %>% 
    rename(W_to_go = W) %>% 
    dplyr::select(team,games_left,W_to_go)

# merge with players
retr_player <- merge(retro_player,team_stats_left,
                      by = "team", all = TRUE)

all_player <- merge(retr_player,prediction_stats, by = c("player","team")) %>% 
    rename(player_name = player) %>% 
    mutate(dif = abs(min_to_go - min_pred)) %>% 
    dplyr::select(player_name,team,min_to_go,min_pred,dif)

# merge with models
retrodiction <- merge(all_models,retr_player,
                      by = c("player","year","team"), all.y = TRUE) %>%
    # correcting for player which changed teams
    group_by(player) %>% 
    mutate(count = n()) %>% 
    #filter(count > 1) %>% 
    mutate(VORP = mean(unique(VORP[!is.na(VORP)])),
           w_add_BPM= mean(unique(w_add_BPM[!is.na(w_add_BPM)])),
           w_add_BPM_pm = mean(unique(w_add_BPM_pm[!is.na(w_add_BPM_pm)])),
           wp_total = mean(unique(wp_total[!is.na(wp_total)])),
           wp_pm= mean(unique(wp_pm[!is.na(wp_pm)])),
           win_shares= mean(unique(win_shares[!is.na(win_shares)])),
           ws_pm = mean(unique(ws_pm[!is.na(ws_pm)])),) %>% 
    ungroup() %>%
    
    # replace na's due to new comers in the league (players have not participated before)
    replace_na(list(team.x = "new")) %>% 
    replace_na(list(wp_total = -100,
                    wp_pm = -100,
                    win_shares = -100,
                    ws_pm = -100,
                    VORP = -100,
                    w_add_BPM = -100,
                    w_add_BPM_pm = -100,
                    min_p = 0)) %>% 
    dplyr::select(-min_t,-min_pct) %>% 
    
    # assign new players a value of 0.045 for ws and wp and -2 for VORP
    group_by(team) %>% 
    mutate(players = n()) %>% 
    ungroup() %>% 
    # and predict by multiplying with minutes played
    mutate(wp_pred = ifelse(wp_total == -100, 0.045 * min_to_go / 40, wp_pm * min_to_go),
           ws_pred = ifelse(win_shares == -100, 0.045 * min_to_go / 40, ws_pm * min_to_go),
           VORP = ifelse(VORP == -100, -2, VORP),
           
           BPM_pred = VORP * point_differential_pg * games_left) %>%  #+ (games_left / 2 / players),
           # w_add_BPM_pm = ifelse(min_p == 0,0, w_add_BPM / min_p),
           # BPM_pred = w_add_BPM_pm * min_to_go) %>% 
    group_by(team,games_left,W_to_go) %>% 
    summarise_at(vars(ws_pred,wp_pred,BPM_pred), .funs = sum) %>% 
    ungroup() %>% 
    mutate(dumbo = games_left / 2,
           BPM_pred2 = BPM_pred + (games_left / 2))

#******************************************************************************#
# Power, Error and correlation
# MRSE
require(Metrics)
error_retro <- retrodiction %>% 
    mutate(WP_RMSE = rmse(W_to_go,wp_pred),
           WS_RMSE = rmse(W_to_go,ws_pred),
           BPM_RMSE = rmse(W_to_go,BPM_pred),
           dumbo_RMSE = rmse(W_to_go,dumbo),
           BPM2_RMSE = rmse(W_to_go,BPM_pred2))
           
# R^2
reg1 <- lm(data = retrodiction,
           W_to_go ~ wp_pred)
summary(reg1)

reg2 <- lm(data = retrodiction,
           W_to_go ~ws_pred)
summary(reg2)

reg3 <- lm(data = retrodiction,
           W_to_go ~ BPM_pred)
summary(reg3)

reg3b <- lm(data = retrodiction,
           W_to_go ~ BPM_pred2)
summary(reg3b)

reg4 <- lm(data = retrodiction,
           W_to_go ~dumbo)
summary(reg4)



# plots to correlation regression:
ggplot(data = retrodiction, aes(x = wp_pred, y = W_to_go)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

ggplot(data = retrodiction, aes(x = ws_pred, y = W_to_go)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

ggplot(data = retrodiction, aes(x = BPM_pred, y = W_to_go)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

ggplot(data = retrodiction, aes(x = BPM_pred2, y = W_to_go)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

ggplot(data = retrodiction, aes(x = dumbo, y = W_to_go)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

reg5 <- lm(data = retrodiction,
           W_to_go ~dumbo2)
summary(reg5)

# ggplot(data = retrodiction, aes(x = dumbo2, y = W_to_go)) +
#     geom_point() +
#     geom_smooth(method = "lm",
#                 formula = y ~ x)


#******************************************************************************#












#******************************************************************************#
# KA was CV bringen soll... oder wie das geht...
# das was ich gemacht habe ist auf jedenfall falsch...
# ich müsste ihm ja wenigstens die spiele alle geben, sonst ist das ja komplett unnütz
# trying <- readRDS("Data/estimates/ranking.Rds") %>% 
#     filter(., year == year_input) %>% 
#     group_by(team) %>% 
#     summarise_at(vars(VORP,wp_A,win_shares), sum)
# 
# real_wins <- readRDS("Data/teams_each_game.Rds") %>% 
#     filter(year == year_input) %>% 
#     dplyr::select(team,G,W,L) %>% 
#     group_by(team) %>% 
#     summarise_at(vars(G:L), sum)
# 
# merger <- merge(trying,real_wins, by = "team") %>% 
#     mutate(BPM = VORP * point_differential_pg * G, .keep = "unused")


# library(DAAG)
# model1.daag<-CVlm(data = merger,m=3,form.lm=formula(W~BPM))
# model2.daag<-CVlm(data = merger,m=2,form.lm=formula(W~wp_A))



# Correlation
cor(prediction$W_to_go,prediction$wp_pred)
cor(prediction$W_to_go,prediction$ws_pred)
cor(prediction$W_to_go,prediction$BPM_pred)

#******************************************************************************#









##################################
# a <- t(prediction_teams) %>% as_tibble()
# colnames(a) <- prediction_teams$team
# a$first <- colnames(prediction_teams)
# 
# a <- a %>% 
#     relocate(first, .before = 1)
# a <- a[-1,]

# teams <- df_team$team
# team_current <- teams
# 
# junge <- read.xlsx("data/junge.xlsx", rowNames = FALSE, colNames = TRUE)
# 
# 
# rating_team <- junge %>% 
#     mutate(#marg_of_victory = (pts - opp_pts) / G,
#         mov = marg_of_victory,
#         opp_avg = 0) %>% 
#     dplyr::select(team,marg_of_victory,mov,opp_avg)
# 
# for (team_current in junge$team) {
#     
#     cur_team <- rating_team %>% 
#         filter(team == team_current)
#     
#     cur_opp_avg <- rating_team %>% 
#         filter(team != team_current) %>% 
#         summarise_at(vars(mov), mean)
#     
#     rating_team$opp_avg[rating_team$team == team_current] <-  cur_opp_avg$mov
#     rating_team$mov[rating_team$team == team_current] <- cur_team$marg_of_victory + cur_opp_avg$mov
# }