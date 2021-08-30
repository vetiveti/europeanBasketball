# net rating = pd_pg * games / possessions * 100!
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
require(Metrics)

#******************************************************************************#
# Inputs----
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

w_adj_netRtg <- team %>% 
    group_by(team,year) %>% 
    summarise_at(vars(G:opp_pts), sum) %>% 
    mutate(pd_pg_true = (pts- opp_pts) / G,
           possessions = 0.5*((fga+0.4*fta-1.07*(orb/(orb + opp_drb))*(fga-fgm)+tov)+(opp_fga+0.4*opp_fta-1.07*(opp_orb/(opp_orb+drb))*(opp_fga-opp_fgm)+opp_tov))) %>% 
    mutate(off_rating = (pts / possessions) * 100,
           def_rating = (opp_pts / possessions * 100)) %>% 
    group_by(year) %>% 
    mutate(adj_ORtg = off_rating - mean(off_rating),
           adj_DRtg = def_rating - mean(off_rating),
           adj_netRtg = adj_ORtg - adj_DRtg,
           netRtg = off_rating - def_rating,
           W_pct = W / G) %>% 
    dplyr::select(team,year,G,W,L,pd_pg_true,netRtg,adj_netRtg,possessions,W_pct)

#******************************************************************************#
# explaining win percentage with adj_net rating----
adj_W_reg <- lm(W_pct ~ adj_netRtg + factor(year), data = w_adj_netRtg)    
summary(adj_W_reg)

ggplot(data = adj_W_reg, aes(y = W_pct, x = adj_netRtg)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

#******************************************************************************#
# Split data set----
# train games
train_games <- team %>% 
    filter(., year == year_input) %>% 
    group_by(team) %>% 
    slice_max(., order_by = game, prop = fraction) #%>% # , replace = FALSE , order_by = game

# player stats in train games
df_player <- player %>% 
    filter(., game %in% train_games$game) %>% 
    group_by(player,team,year,Pos.) %>% 
    mutate(G = 1) %>% 
    summarise_at(vars(min:pts,G), .funs = sum) %>% 
    ungroup() %>% 
    rename(min_p = min)

# team stats in train games
df_team <- team %>% 
    filter(., game %in% train_games$game) %>% 
    group_by(team,G_tot,year) %>% 
    summarise_at(vars(G:opp_pts), .funs = sum) %>% 
    ungroup() %>% 
    mutate(REBTM = fga - (opp_tov + opp_fgm + 0.4720415 * opp_ftm + drb - tov
                          - 0.4720415 * fta + orb))

#******************************************************************************#
# player values by metrics:----
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
           w_add_BPM_pm = w_add_BPM / min_p,
           BPM_exp = BPM * (min_p / min_t) * 5) %>% 
    dplyr::select(player,team,year,min_p,w_add_BPM,w_add_BPM_pm, VORP,BPM,min_t)

#******************************************************************************#
#debugonce(BBL_eff)
bbl_eff.cv <- BBL_eff(df_player) %>% 
    filter(min_p > 0.0) %>% 
    mutate(bbl_eff_pm = bbl_eff / min_p) %>% 
    dplyr::select(player,team,year,min_p,bbl_eff,bbl_eff_pm)

#******************************************************************************#
# Combining all player values:----
model1 <- merge(bpm.cv,wp.cv,
                by = c("player","team","year","min_p"), all = TRUE)
model2 <- merge(model1,bbl_eff.cv,
                by = c("player","team","year","min_p"), all = TRUE)

all_models <- merge(model2,ws.cv,
                    by = c("player","team","year","min_p","min_t"), all = TRUE) %>% 
    mutate(min_pct = min_p / min_t)

#******************************************************************************#
#******************************************************************************#
# EXPLAINING THE CURRENT SPLIT:----
# outcome of split in terms of wins, point differential per game and team ratings
real_outcome_split <- readRDS("Data/teams_each_game.Rds") %>% 
    rename(game = game_nr) %>% 
    filter(., game %in% train_games$game) %>% 
    #dplyr::select(team,year,G,W,L) %>% 
    group_by(team,year) %>% 
    summarise_at(vars(G:opp_pts), sum) %>% 
    mutate(pd_pg_true = (pts- opp_pts) / G,
           possessions = 0.5*((fga+0.4*fta-1.07*(orb/(orb + opp_drb))
                          *(fga-fgm)+tov)+
                             (opp_fga+0.4*opp_fta-1.07*(opp_orb/(opp_orb+drb))
                              *(opp_fga-opp_fgm)+opp_tov))) %>% 
    mutate(off_rating = (pts / possessions) * 100,
           def_rating = (opp_pts / possessions * 100)) %>% 
    group_by(year) %>% 
    mutate(adj_ORtg = off_rating - mean(off_rating),
           adj_DRtg = def_rating - mean(off_rating),
           adj_netRtg = adj_ORtg - adj_DRtg,
           netRtg = off_rating - def_rating) %>% 
    dplyr::select(team,year,G,W,L,pd_pg_true,netRtg,adj_netRtg,possessions)

# all models explaining current split
all_models_current <- all_models %>% 
    group_by(team) %>% 
    summarise_at(vars(win_shares,wp_total,w_add_BPM,VORP,BPM,bbl_eff), sum)

# games in total for each team per year
G_tot <- mean(games_total$G_tot[games_total$year == year_input])
    
print(paste0("mean number of Games for each team in train sample: ",round(mean(real_outcome_split$G),digits = 2)))

# merge split outcomes with player estimated player values
outcome_split <- merge(all_models_current,real_outcome_split,by ="team") %>% 
    mutate(wp_pd_pg = (win_shares   - (mean(G) / 2)) / (point_differential_pg * G_tot),
           ws_pd_pg = (wp_total     - (mean(G) / 2)) / (point_differential_pg * G_tot),
           bbl_eff_pd_pg = (bbl_eff - (mean(G) / 2)) / (point_differential_pg * G_tot)) %>% 
    mutate(w_add_BPM2 = w_add_BPM + (G / 2))

reg1 <- lm(data = outcome_split,
           W ~ wp_total)
summary(reg1)

reg2 <- lm(data = outcome_split,
           W ~win_shares)
summary(reg2)

reg3 <- lm(data = outcome_split,
           W ~w_add_BPM)
summary(reg3)

reg4 <- lm(data = outcome_split,
          adj_netRtg ~VORP)
summary(reg4)

# ggplot(data = outcome_split, aes(x = wp_total, y = adj_netRtg)) +
#     geom_point() +
#     geom_smooth(method = "lm",
#                 formula = y ~ x)
# 
# ggplot(data = outcome_split, aes(x = W, y = adj_netRtg)) +
#     geom_point() +
#     geom_smooth(method = "lm",
#                 formula = y ~ x)

# for VORP intercept must be - 10 as a replacement player is defined at -2!

#******************************************************************************#
# rmse----
error_w_split <- outcome_split %>% 
    mutate(WP_RMSE = rmse(W,wp_total),
           WS_RMSE = rmse(W,win_shares),
           BPM_RMSE = rmse(W,w_add_BPM),
           BPM_RMSE2 = rmse(W,w_add_BPM2),
           BBL_eff_RMSE = rmse(W,bbl_eff),) %>% 
    dplyr::select(team,W,WS_RMSE,WP_RMSE,BPM_RMSE,BPM_RMSE2,BBL_eff_RMSE)

#pd_pg has to be changed!----
error_netRtg_split <- outcome_split %>% 
    mutate(wp_adj_netRtg = -23.3685 + wp_total * 1.4417,
           WP_RMSE = rmse(adj_netRtg,wp_total),
           WS_RMSE = rmse(adj_netRtg,win_shares),
           BPM = -10 + VORP,
           ws_add = -23.3685 + (1.4417 * win_shares),
           WS2_RMSE= rmse(adj_netRtg,ws_add),
           BPM_RMSE= rmse(adj_netRtg,BPM)) %>% 
    dplyr::select(team,adj_netRtg,WP_RMSE,WS_RMSE,WS2_RMSE,BPM_RMSE)

#******************************************************************************#
# PREDICTING THE NEXT SPLIT (minutes unkown):----
# outcome of future split in terms of wins, point differential per game and team ratings
real_outcome_pred <- readRDS("Data/teams_each_game.Rds") %>% 
    rename(game = game_nr) %>% 
    filter(., !game %in% train_games$game & year == year_input) %>% 
    group_by(team,year) %>% 
    summarise_at(vars(G:opp_pts), sum) %>% 
    rename(min_to_go = min,
           W_to_go = W,
           G_to_go = G,
           L_to_go = L) %>% 
    mutate(pd_pg_true = (pts- opp_pts) / G_to_go,
           possessions = 0.5*((fga+0.4*fta-1.07*(orb/(orb + opp_drb))*(fga-fgm)+tov)+(opp_fga+0.4*opp_fta-1.07*(opp_orb/(opp_orb+drb))*(opp_fga-opp_fgm)+opp_tov))) %>% 
    mutate(off_rating = (pts / possessions) * 100,
           def_rating = (opp_pts / possessions * 100)) %>% 
    group_by(year) %>% 
    mutate(adj_ORtg = off_rating - mean(off_rating),
           adj_DRtg = mean(off_rating) - def_rating,
           adj_netRtg = adj_ORtg + adj_DRtg,
           netRtg = off_rating - def_rating) %>% 
    dplyr::select(team,year,min_to_go,G_to_go,W_to_go,pd_pg_true,netRtg,adj_netRtg,possessions)

# combining and calculating predicted player values for each metric
outcome_pred <- merge(all_models,real_outcome_pred,
                          by = c("team","year")) %>% 
    mutate(min_pred = min_pct * min_to_go,
           ws_pred = ws_pm * min_pred,
           wp_pred = wp_pm * min_pred,
           BPM_pred = ((BPM + 2) * min_pct*5)* point_differential_pg * G_to_go)

# calculating team wins, point differential, and ratings per metric
prediction <- outcome_pred %>% 
    group_by(team,G_to_go,W_to_go,min_t,pd_pg_true,adj_netRtg) %>% 
    summarise_at(vars(min_pred,ws_pred,wp_pred,BPM_pred,VORP), .funs = sum) %>% 
    ungroup() %>% 
    mutate(dumbo = outcome_split$W / outcome_split$G * G_to_go,
           BPM_pred2 = BPM_pred + (G_to_go / 2),
           wp_pd_pg = (ws_pred - (mean(G_to_go) / 2)) / (point_differential_pg * G_tot),
           ws_pd_pg = (wp_pred - (mean(G_to_go) / 2)) / (point_differential_pg * G_tot))

#******************************************************************************#
# rmse:----
error_w_pred <- prediction %>% 
    mutate(WP_RMSE = rmse(W_to_go,wp_pred),
           WS_RMSE = rmse(W_to_go,ws_pred),
           BPM_RMSE = rmse(W_to_go,BPM_pred),
           BPM2_RMSE = rmse(W_to_go,BPM_pred2),
           dumbo_RMSE = rmse(W_to_go,dumbo))

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

reg4 <- lm(data = prediction,
           W_to_go ~dumbo)
summary(reg4)

# plots for correlation regressions
# ggplot(data = prediction, aes(x = wp_pred, y = W_to_go)) +
#     geom_point() +
#     geom_smooth(method = "lm",
#                 formula = y ~ x)
# 
# ggplot(data = prediction, aes(x = ws_pred, y = W_to_go)) +
#     geom_point() +
#     geom_smooth(method = "lm",
#                 formula = y ~ x)
# ggplot(data = prediction, aes(x = BPM_pred, y = W_to_go)) +
#     geom_point() +
#     geom_smooth(method = "lm",
#                 formula = y ~ x)
# 
# ggplot(data = prediction, aes(x = dumbo, y = W_to_go)) +
#     geom_point() +
#     geom_smooth(method = "lm",
#                 formula = y ~ x)

#******************************************************************************#
# rmse for Adjusted net Rating prediction
error_netRtg_pred <- prediction %>%
    mutate(BPM_VORP = -10 + (VORP),
           ws_ide = -17.8 + (2.16 * ws_pred)) %>% 
    # pd_pg must be changed!----
mutate(WP_RMSE = rmse(adj_netRtg,wp_pd_pg),
       WS_RMSE = rmse(pd_pg_true,ws_pd_pg),
       WS2_RMSE= rmse(adj_netRtg,ws_ide),
       BPM_RMSE= rmse(adj_netRtg,BPM_VORP),
       BPM2_RMSE= rmse(adj_netRtg,BPM_pred)) %>% 
    
    dplyr::select(team,adj_netRtg,WP_RMSE,WS_RMSE,WS2_RMSE,BPM_RMSE,BPM2_RMSE)

#******************************************************************************#
# RETRODICTING THE NEXT SPLIT (minutes are known) :----
# minutes per player
playing_time_retro <- player %>% 
    filter(., !game %in% train_games$game & year == year_input) %>% 
    group_by(player,team,year) %>% 
    summarise_at(vars(min), .funs = sum) %>% 
    ungroup() %>% 
    rename(min_to_go = min)

# number of games to predict and real wins for each team
real_outcome_retro <- team %>% 
    filter(., !game %in% train_games$game & year == year_input) %>% 
    group_by(team,G_tot) %>% 
    summarise_at(vars(G:opp_pts), .funs = sum) %>% 
    ungroup() %>%
    mutate(games_left = G) %>% 
    rename(W_to_go = W) %>% 
    mutate(pd_pg_true = (pts- opp_pts) / games_left,
           possessions = 0.5*((fga+0.4*fta-1.07*(orb/(orb + opp_drb))*(fga-fgm)+tov)+(opp_fga+0.4*opp_fta-1.07*(opp_orb/(opp_orb+drb))*(opp_fga-opp_fgm)+opp_tov))) %>% 
    mutate(off_rating = (pts / possessions) * 100,
           def_rating = (opp_pts / possessions * 100)) %>% 
    mutate(adj_ORtg = off_rating - mean(off_rating),
           adj_DRtg = def_rating - mean(off_rating),
           adj_netRtg = adj_ORtg - adj_DRtg,
           netRtg = off_rating - def_rating) %>% 
    dplyr::select(team,games_left,W_to_go,pd_pg_true,adj_netRtg,possessions,min) %>% 
    rename(min_t_left = min)

# merge with playing time for each player
retr_player <- merge(playing_time_retro,real_outcome_retro,
                     by = "team", all = TRUE)

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
           BPM= mean(unique(BPM[!is.na(BPM)])),
           ws_pm = mean(unique(ws_pm[!is.na(ws_pm)])),) %>% 
    ungroup() %>%
    
    # replace na's due to new comers in the league (players have not participated before)
    replace_na(list(team.x = "new")) %>% 
    replace_na(list(BPM = -100,
                    wp_total = -100,
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
    
    # predict by multiplying with real minutes to go
    mutate(wp_pred = ifelse(wp_total == -100, 0.045 , wp_pm * min_to_go), #* min_to_go / 40
           ws_pred = ifelse(win_shares == -100, 0.045 , ws_pm * min_to_go), #* min_to_go / 40
           min_pct = min_to_go / min_t_left,
           BPM = ifelse(BPM == -100, 0, BPM),
           VORP = ifelse(VORP == -100,-100,(BPM + 2) * min_pct * 5),
           VORP = ifelse(VORP == -100, -2, VORP),
           
           # add replacement level for players with small minutes (<= 30)
           BPM = ifelse(min_p > 30, BPM, 0),
           VORP = ifelse(min_p > 30, VORP, -2),
           wp_pred = ifelse(min_p > 30, wp_pred, 0.045),
           ws_pred = ifelse(min_p > 30, ws_pred, 0.045),
           
           BPM_pred = VORP * point_differential_pg * games_left,
           BPM2_pred = BPM * min_pct * 5 * point_differential_pg * games_left) %>%
    
    group_by(team,games_left,W_to_go,pd_pg_true,adj_netRtg) %>% 
    summarise_at(vars(ws_pred,wp_pred,BPM_pred,VORP,BPM2_pred,BPM), .funs = sum) %>% 
    ungroup() %>% 
    mutate(BPM_pred2 = BPM_pred + (games_left / 2),
           BPM2_pred2 = BPM2_pred + (games_left / 2))

#******************************************************************************#
# rmse:----
error_w_retro <- retrodiction %>% 
    mutate(WP_RMSE = rmse(W_to_go,wp_pred),
           WS_RMSE = rmse(W_to_go,ws_pred),
           BPM2_RMSE = rmse(W_to_go,BPM2_pred2),
           BPM2_RMSE2 = rmse(W_to_go,BPM_pred2),) %>% 
    dplyr::select(WP_RMSE,WS_RMSE,BPM2_RMSE,BPM2_RMSE2)

# R^2
reg1 <- lm(data = retrodiction,
           W_to_go ~ wp_pred)
summary(reg1)

reg2 <- lm(data = retrodiction,
           W_to_go ~ws_pred)
summary(reg2)

reg3 <- lm(data = retrodiction,
           W_to_go ~ BPM2_pred2)
summary(reg3)

# plots:
# ggplot(data = retrodiction, aes(x = wp_pred, y = W_to_go)) +
#     geom_point() +
#     geom_smooth(method = "lm",
#                 formula = y ~ x)
# 
# ggplot(data = retrodiction, aes(x = ws_pred, y = W_to_go)) +
#     geom_point() +
#     geom_smooth(method = "lm",
#                 formula = y ~ x)
# 
ggplot(data = retrodiction, aes(x = BPM2_pred2, y = pd_pg_true)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

#******************************************************************************#
# rmse for point differential retrodiction
G_tot <- mean(games_total$G_tot[games_total$year == year_input])
pd_pg <- retrodiction %>%
    mutate(wp_pd_pg = (wp_pred - (mean(games_left) / 2)) / (point_differential_pg * G_tot),
           ws_pd_pg = (ws_pred - (mean(games_left) / 2)) / (point_differential_pg * G_tot)) %>%
    dplyr::select(team,pd_pg_true,BPM_pred,wp_pd_pg,ws_pd_pg,VORP,BPM_pred2,BPM2_pred2,BPM)

error_pd_retro <- pd_pg %>%
    mutate(BPM2_pred2 = -10 + BPM2_pred2,
           WP_RMSE = rmse(pd_pg_true,wp_pd_pg),
           WS_RMSE = rmse(pd_pg_true,ws_pd_pg),
           VORP_RMSE = rmse(pd_pg_true, VORP),
           BPM_RMSE = rmse(pd_pg_true, BPM2_pred2)) %>%
    dplyr::select(WP_RMSE,WS_RMSE,VORP_RMSE,BPM_RMSE)

# R^2
reg1 <- lm(data = pd_pg,
           pd_pg_true ~ wp_pd_pg)
summary(reg1)

reg2 <- lm(data = pd_pg,
           pd_pg_true ~ws_pd_pg)
summary(reg2)

reg3 <- lm(data = pd_pg,
           pd_pg_true ~ BPM2_pred2)
summary(reg3)

#******************************************************************************#
# rmse for adjusted team ratings retrodiction
G_tot <- mean(games_total$G_tot[games_total$year == year_input])

netRtg_retro <- retrodiction %>% 
    mutate(wp_pd_pg = (wp_pred - (mean(games_left) / 2)) / (point_differential_pg * G_tot),
           ws_pd_pg = (ws_pred - (mean(games_left) / 2)) / (point_differential_pg * G_tot)) %>% 
    dplyr::select(team,W_to_go,adj_netRtg,BPM_pred,wp_pred,wp_pd_pg,ws_pd_pg,ws_pred,VORP,BPM2_pred2) %>% 
           mutate(ws_pred2 = -17.8 + (2.16 * ws_pred))

error_netRtg_retro <- netRtg_retro %>% 
    mutate(WP = -1.5942 + VORP*0.7134,
           WP_RMSE = rmse(adj_netRtg,wp_pd_pg),
           WS_RMSE = rmse(adj_netRtg,ws_pd_pg),
           WS2_RMSE = rmse(adj_netRtg,ws_pred2),
           VORP_RMSE = rmse(adj_netRtg, VORP),
           BPM_RMSE = rmse(adj_netRtg, BPM2_pred2)) %>% 
    #dplyr::select(team,adj_netRtg,VORP) %>% 
    #mutate(dif = abs(adj_netRtg - VORP))
    dplyr::select(team,W_to_go,adj_netRtg,wp_pd_pg,ws_pd_pg,VORP,WP_RMSE,WS_RMSE,VORP_RMSE,BPM_RMSE)

# R^2
reg1 <- lm(data = netRtg_retro,
           adj_netRtg ~ wp_pd_pg)
summary(reg1)

reg2 <- lm(data = netRtg_retro,
           adj_netRtg ~ws_pred)
summary(reg2)

reg3 <- lm(data = netRtg_retro,
           adj_netRtg ~ BPM2_pred2)
summary(reg3)

#******************************************************************************#
# calculate minute continuity:----
# what is needed
# min_p     t = 0
# min_p     t = 1
# min_t     t = 0
# min_t     t = 1
min_continuity_p <- merge(df_player,playing_time_retro, by = c("player","team","year")) %>% 
    dplyr::select(player,team,year,min_p,min_to_go)

min_continuity_t <- merge(df_team,real_outcome_retro, by = c("team")) %>% 
    dplyr::select(team,min,min_t_left) %>% 
    rename(min_t = min)

# calculation
min_continuity <- merge(min_continuity_p,min_continuity_t, by = "team") %>% 
    mutate(min_pct_t0 = min_p / min_t,
           min_pct_t1 = min_to_go / min_t_left) %>% 
    mutate(cont = (pmin(min_pct_t0,min_pct_t1))) %>% 
    group_by(team) %>% 
    mutate(stability = sum(cont)) %>% 
    ungroup() %>% 
    distinct(team,year,stability)

rmse <- merge(min_continuity,error_netRtg_retro, by = "team")

#******************************************************************************#
# save:----
saveRDS(object = rmse, file = paste0("Data/prediction/rmse_",year_input,".Rds"))

#******************************************************************************#
wins <- merge(netRtg_retro,real_outcome_split, by = "team", suffixes = c("_to_go","_before")) %>% 
    mutate(win_pct = W / G,
           WP_RMSE = rmse(adj_netRtg_to_go,wp_pd_pg),
           WS_RMSE = rmse(adj_netRtg_to_go,ws_pd_pg),
           VORP_RMSE = rmse(adj_netRtg_to_go, VORP),
           W_RMSE = rmse(adj_netRtg_to_go, win_pct),
          rating_RMSE = rmse(adj_netRtg_to_go, adj_netRtg_before))

wins2 <- merge(netRtg_retro,real_outcome_split, by = "team", suffixes = c("_to_go","_before")) %>% 
    mutate(win_pct = W / G,
           WP_RMSE = rmse(W_to_go,wp_pred),
           WS_RMSE = rmse(W_to_go,ws_pred),
           VORP_RMSE = rmse(W_to_go, VORP),
           W_RMSE = rmse(W_to_go, win_pct),
           rating_RMSE = rmse(W_to_go, adj_netRtg_before))

ggplot(data = wins2, aes(x = ws_pred, y = adj_netRtg_to_go)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

a <- wins2 %>% 
    select(team,VORP,W_to_go,adj_netRtg_to_go,adj_netRtg_before) %>% 
    mutate(BPM = -10 + (VORP))

reg3 <- lm(data = wins2,
           adj_netRtg_to_go ~ ws_pred)
summary(reg3)

