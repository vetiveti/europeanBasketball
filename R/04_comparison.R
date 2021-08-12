# Clear Console
cat("\014")

# remove Environment
rm(list=ls())


library(tidyverse)

#******************************************************************************#
# Load files:----
BPM <- readRDS("Data/estimates/BPM_final.Rds")
Win_Shares <- readRDS("Data/estimates/win_shares.Rds")
Wins_produced_A <- readRDS("Data/estimates/wp_berri_a.Rds")
Wins_produced_B <- readRDS("Data/estimates/wp_berri_b.Rds")
efficiency <- readRDS("Data/estimates/efficiency.Rds")
Hollinger <- readRDS("Data/estimates/PER_game_score.Rds")
RAPM <- readRDS("Data/estimates/rapm.Rds")

team_totals <- readRDS("Data/team_data_totals.Rds")
#******************************************************************************#
# Prep files: ----
Wins_produced_A <- Wins_produced_A %>% 
    filter(wp != 0) %>% 
    group_by(year) %>% 
    arrange(desc(wp)) %>% 
    mutate(wp_A_rank_y = row_number(desc(wp))) %>% 
    ungroup() %>%
    mutate(wp_A_rank_all = row_number(desc(wp))) %>% 
    rename(wp_A = wp)

for_merge <- Wins_produced_A %>% 
    select(player,team,year,min_p)

Wins_produced_A <- Wins_produced_A %>% 
    select(player,year,team,wp_A,wp_A_rank_y,wp_A_rank_all)

Wins_produced_B <- Wins_produced_B %>% 
    filter(wp != 0) %>% 
    group_by(year) %>% 
    arrange(desc(wp)) %>% 
    mutate(wp_B_rank_y = row_number(desc(wp))) %>% 
    ungroup() %>%
    mutate(wp_B_rank_all = row_number(desc(wp))) %>% 
    rename(wp_B = wp) %>% 
    select(player,year,team,wp_B,wp_B_rank_y,wp_B_rank_all)


BPM <- BPM %>% 
    group_by(year) %>% 
    arrange(desc(BPM)) %>% 
    mutate(BPM_rank_y = row_number(desc(BPM)),
           VORP_rank_y = row_number(desc(VORP))) %>% 
    ungroup() %>%
    mutate(BPM_rank_all = row_number(desc(BPM)),
           VORP_rank_all = row_number(desc(VORP))) %>% 
    select(player,year,team,BPM,VORP,BPM_rank_y,BPM_rank_all, VORP_rank_y, VORP_rank_all)

Win_Shares <- Win_Shares %>% 
    group_by(year) %>% 
    arrange(desc(win_shares)) %>% 
    mutate(ws_rank_y = row_number(desc(win_shares))) %>% 
    ungroup() %>%
    mutate(ws_rank_all = row_number(desc(win_shares))) %>% 
    select(player,year,team,win_shares,ws_rank_y,ws_rank_all)

efficiency <- efficiency %>% 
    filter(min_p != 0) %>% 
    group_by(year) %>% 
    arrange(desc(nba_eff)) %>% 
    mutate(nba_eff_y = row_number(desc(nba_eff)),
           bbl_eff_y = row_number(desc(bbl_eff))) %>% 
    ungroup() %>%
    mutate(nba_eff_all = row_number(desc(nba_eff)),
           bbl_eff_all = row_number(desc(bbl_eff))) %>% 
    select(player,year,team,nba_eff,bbl_eff,nba_eff_y,nba_eff_all, bbl_eff_y, bbl_eff_all)

# Hollinger <- Hollinger %>% 
#     filter(PER != 0 &
#        min != 0) %>% 
#     group_by(year) %>% 
#     arrange(desc(PER)) %>% 
#     mutate(PER_y = row_number(desc(PER)),
#            game_score_y = row_number(desc(game_score))) %>% 
#     ungroup() %>%
#     mutate(PER_all = row_number(desc(PER)),
#            game_score_all = row_number(desc(game_score))) %>% 
#     select(player,year,team,PER,game_score,PER_y,game_score_y, PER_all, game_score_all,min)

RAPM <- RAPM %>% 
    arrange(desc(RAPM)) %>% 
    mutate(RAPM = row_number(desc(RAPM)))

#******************************************************************************#
# combine BPM, WS, WP, efficiency: ----
ranking <- Reduce(function(x,y) merge(x = x, y = y, by = c("player","year","team"))
                  , list(for_merge, BPM, Win_Shares, Wins_produced_A,Wins_produced_B,efficiency)) %>% 
    arrange(VORP_rank_y)

saveRDS(object = ranking, file = paste0("Data/estimates/ranking.Rds"))
# hollinger makes problems...
results1 = setdiff(aa$player, Wins_produced_B$player)   # elements in a$x NOT in b$y
results2 = setdiff(Wins_produced_B$player, aa$player)   # elements in b$y NOT in a$x

#******************************************************************************#
# Descriptive comparison:----
rank_y <- ranking %>% 
    dplyr::select(year, player, team, BPM_rank_y, VORP_rank_y, ws_rank_y,wp_A_rank_y, wp_B_rank_y,nba_eff_y, bbl_eff_y)

rank_2014 <- rank_y %>% 
    filter(year == 2014) %>% 
    arrange(VORP_rank_y)

rank_2015 <- rank_y %>% 
    filter(year == 2015)%>% 
    arrange(VORP_rank_y)

rank_2016 <- rank_y %>% 
    filter(year == 2016)%>% 
    arrange(VORP_rank_y)

rank_2017 <- rank_y %>% 
    filter(year == 2017)%>% 
    arrange(VORP_rank_y)

rank_2018 <- rank_y %>% 
    filter(year == 2018)%>% 
    arrange(VORP_rank_y)

rank_2019 <- rank_y %>% 
    filter(year == 2019)%>% 
    arrange(VORP_rank_y)

rank_2020 <- rank_y %>% 
    filter(year == 2020)%>% 
    arrange(VORP_rank_y)

#******************************************************************************#
# Explanatory power:----
# win percentage vs point differential
team_totals <- team_totals %>% 
    mutate(point_differential_pg = (pts - opp_pts) / G,
           win_perc = W / G)

reg_point_dif <- lm(data = team_totals,
                    win_perc ~ point_differential_pg)
summary(reg_point_dif)

ggplot(data = team_totals, aes(x = point_differential_pg, y = win_perc)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)
#******************************************************************************#
# Explaining wins with regression R^2:----
team_totals <- readRDS("Data/team_data_totals.Rds")
win_true <- team_totals %>% 
    select(team,year,G,W,L)
win_est <- ranking %>% 
    dplyr::select(year,player,team,VORP,win_shares, wp_A, wp_B, bbl_eff,min_p) %>% 
    mutate(VORP_2 = ifelse(min_p < 100,0,VORP),
        BPM_est = VORP * coefficients(reg_point_dif)["point_differential_pg"],
        BPM_est_2 = VORP_2 * coefficients(reg_point_dif)["point_differential_pg"]) %>% 
    group_by(year,team) %>% 
    summarise(BPM_est_w = sum(BPM_est),
              BPM_est_w_2 = sum(BPM_est_2),
              win_shares_est = sum(win_shares),
              wp_A_est = sum(wp_A),
              wp_B_est = sum(wp_B),
              bbl_eff_est = sum(bbl_eff)) %>% 
    ungroup()

win_reg <- merge(win_true,win_est, by =c("year","team")) %>% 
    mutate(BPM_est = BPM_est_w * G,
           BPM_est_2 = BPM_est_w_2 * G,
           ) %>% 
    dplyr::select(-BPM_est_w,-BPM_est_w_2)

# win shares
reg_win_shares <- lm(data = win_reg,
              W ~  win_shares_est + factor(year))
summary(reg_win_shares)

ggplot(data = win_reg, aes(x = win_shares_est, y = W)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

# wins produced A
reg_wp_A <- lm(data = win_reg,
                     W ~ wp_A_est + factor(year))
summary(reg_wp_A)

ggplot(data = win_reg, aes(x = wp_A_est, y = W)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

# wins produced B 
reg_wp_B <- lm(data = win_reg,
               W ~ wp_B_est+ factor(year))
summary(reg_wp_B)

ggplot(data = win_reg, aes(x = wp_B_est, y = W)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

# BPM
reg_BPM <- lm(data = win_reg,
               W ~ BPM_est + factor(year))
summary(reg_BPM)

ggplot(data = win_reg, aes(x = BPM_est, y = W)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)
# BPM 2
reg_BPM_2 <- lm(data = win_reg,
              W ~ BPM_est_2 + factor(year))
summary(reg_BPM_2)

ggplot(data = win_reg, aes(x = BPM_est_2, y = W)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)
# bbl_eff
reg_bbl_eff <- lm(data = win_reg,
              W ~ bbl_eff_est + factor(year))
summary(reg_bbl_eff)

ggplot(data = win_reg, aes(x = bbl_eff_est, y = W)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

#******************************************************************************#
#*# radial chart for specific players:----
# Library
require(fmsb)

# Create data: note in High school for Jonathan:
data <- rank_2020 %>% 
    filter(player == "Paul, Zipser") %>% 
    dplyr::select(-player,-year,-team)

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(1,7) , rep(301,301) , data)

# Check your data, it has to look like this!
head(data)

# The default radar chart 
fmsb::radarchart(data,axistype=1 , 
                 
                 #custom polygon
                 pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
                 
                 #custom labels
                 vlcex=0.8 
)

