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
# Inputs
# year input
year_input <- 2020

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
#******************************************************************************#
# reality check:----
real_wins <- readRDS("Data/teams_each_game.Rds") %>% 
    rename(game = game_nr) %>% 
    filter(., game %in% train_games$game) %>% 
    dplyr::select(team,year,G,W,L) %>% 
    group_by(team,year) %>% 
    summarise_at(vars(G:L), sum)

print(paste0("mean number of Games for each team in train sample: ",round(mean(real_wins$G),digits = 2)))

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
# prediction part:----
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
# R^2, Error and correlation:----
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
# Retrodiction :----
# (minutes to play are known in advance)

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
    summarise_at(vars(ws_pred,wp_pred,BPM_pred,VORP), .funs = sum) %>% 
    ungroup() %>% 
    mutate(dumbo = games_left / 2,
           BPM_pred2 = BPM_pred + (games_left / 2))

#******************************************************************************#
# R^2, Error and correlation
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

#******************************************************************************#
# per-game point differential :----
G_tot <- mean(games_total$G_tot[games_total$year == year_input])
pd_pg <- retrodiction %>% 
    mutate(wp_pd_pg = (wp_pred - (mean(games_left) / 2)) / (point_differential_pg * G_tot),
           ws_pd_pg = (ws_pred - (mean(games_left) / 2)) / (point_differential_pg * G_tot),
    )

pd_pg_team <- team_stats_left <- team %>% 
    filter(., !game %in% train_games$game & year == year_input) %>% 
    group_by(team,G_tot) %>% 
    summarise_at(vars(G,W,pts,opp_pts), .funs = sum) %>% 
    ungroup() %>%
    mutate(pd_pg_true = (pts- opp_pts) / G) %>% 
    dplyr::select(team,pd_pg_true)

pd_pg_merg <- merge(pd_pg,pd_pg_team, by = "team") %>% 
    dplyr::select(team,pd_pg_true,BPM_pred,BPM_pred2,wp_pd_pg,ws_pd_pg,VORP)


require(Metrics)
error_pd <- pd_pg_merg %>% 
    mutate(WP_RMSE = rmse(pd_pg_true,wp_pd_pg),
           WS_RMSE = rmse(pd_pg_true,ws_pd_pg),
           VORP_RMSE = rmse(pd_pg_true, VORP))

# R^2
reg1 <- lm(data = pd_pg_merg,
           pd_pg_true ~ wp_pd_pg)
summary(reg1)

reg2 <- lm(data = pd_pg_merg,
           pd_pg_true ~ws_pd_pg)
summary(reg2)

reg3 <- lm(data = pd_pg_merg,
           pd_pg_true ~ VORP)
summary(reg3)

#******************************************************************************#
# save:----
saveRDS(object = error_retro, file = paste0("Data/prediction/error_retro_",year_input,".Rds"))
saveRDS(object = error_pd, file = paste0("Data/prediction/error_pd_",year_input,".Rds"))


