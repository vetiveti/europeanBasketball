# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# set working directory
setwd("~/Uni T�bingen/0. Masterarbeit/7. R/europeanBasketball")

# load packages
library(tidyverse, warn.conflicts = FALSE)
require(lmtest)
require(plm)

# load data
player_totals <- readRDS("Data/player_data_totals.Rds")
player_pg <- readRDS("Data/player_data_pg.Rds")

team_totals <- readRDS("Data/team_data_totals.Rds")
team_pg <- readRDS("Data/team_data_pg.Rds")

#******************************************************************************#
# WINS PRODUCED:----
# Descriptive statistics:----
deskriptive_statistics <- summary(team_totals)
deskriptive_statistics_pg <- summary(team_pg)

#******************************************************************************#
# Modelings wins:----
# with points made and opponent points made
pts_on_w_pct <- lm(data = team_pg, win_pct ~ pts + opp_pts)
summary(pts_on_w_pct)
# coeftest to get heterogeneous standard errors
coeftest(pts_on_w_pct, vcov = vcovHC(pts_on_w_pct))

# calculating Team rebounds
team_pg <- team_pg %>%
    mutate(FGA_DIF = fga - (opp_fgm + drb + opp_tov + orb - tov))

FGA_DIF <- lm(data = team_pg, FGA_DIF ~ opp_ftm + fta)
summary(FGA_DIF)

# test if both values are approx. equal with wald test
waldtest(FGA_DIF)
# cannot reject h0 that they are the same!

# redo analysis with a_1 = - b_1
FGA_DIF_est_re <- lm(data = team_pg,FGA_DIF ~ I(opp_ftm + (-fta)))
summary(FGA_DIF_est_re)
# coeftest to get heterogeneous standard errors
coeftest(FGA_DIF_est_re, vcov = vcovHC(FGA_DIF_est_re))

# Thus, we can now calculate team rebounds
# rearange FGA to TREB
#TREB = FGA - (DTOV + DFGM + a_1 * DFTM + DREB - TOV - b_1 * FTA + OREB)
team_pg <- team_pg %>%
    mutate(TeamREB = fga - (opp_tov + opp_fgm + coefficients(FGA_DIF_est_re)[2] * opp_ftm + drb - tov
                            - coefficients(FGA_DIF_est_re)[2] * fta + orb))

team_totals$REBTM <- team_pg$TeamREB * team_pg$G

# OLS regression win% on pts/pe + opp.pts/pa 
# PE & PA
team_pg <- team_pg %>%
    mutate(PE = fga + coefficients(FGA_DIF_est_re)[2] * fta + tov - orb) %>%
    mutate(PA = opp_tov + drb + TeamREB + opp_fgm + coefficients(FGA_DIF_est_re)[2] * opp_ftm) %>%
    mutate(PTS_per_PE = pts/PE) %>%
    mutate(OPP_PTS_per_PA = opp_pts/PA)

# finally OLS - equation (15)
coef_PTS = lm(win_pct~ PTS_per_PE + OPP_PTS_per_PA, data = team_pg)
summary(coef_PTS)
# coeftest to get heterogeneous standard errors
coeftest(coef_PTS, vcov = vcovHC(coef_PTS))

ggplot(team_pg, aes(y = win_pct,x = PTS_per_PE - OPP_PTS_per_PA)) +
    geom_point()

#******************************************************************************#
#Step 0: Marginal contributions:----
# PTS/PE -1 
PTS_per_POS = team_pg$pts/team_pg$PE
mean_PTSperPoss =mean(PTS_per_POS)

# take the derivative to get the marginal impact of points and PE/ opp_points and PA on wins
team_pg <- team_pg %>% 
    mutate(marg_val_PTS = mean(coefficients(coef_PTS)[2] * PE^-1),
           marg_val_PE =  mean(coefficients(coef_PTS)[2] * (pts * PE^-2) * -1),
           marg_val_Opp_PTS = mean(coefficients(coef_PTS)[3] * PA^-1),
           marg_val_Opp_PA = mean(coefficients(coef_PTS)[3] * (opp_pts * PA^-2) * -1)
    )

# connecting marginal contributions to player statistics
team_pg <- team_pg %>% 
    mutate(marg_val_3FGM = marg_val_PTS * 3 + marg_val_PE,
           marg_val_2FGM = marg_val_PTS * 2 + marg_val_PE,
           marg_val_FTA = coefficients(FGA_DIF_est_re)[2] * marg_val_PE,
           marg_val_FTM = marg_val_PTS + marg_val_FTA[[1]],
           marg_val_MFG = marg_val_PE,
           marg_val_MFT = marg_val_FTA[[1]],
           marg_val_ORB = marg_val_Opp_PA,
           marg_val_DRB = marg_val_Opp_PA,
           marg_val_STL = marg_val_Opp_PA,
           marg_val_TOV = marg_val_PE
    )

# marginal value BLK
blk_on_opp_FGM <- lm(data = team_pg , opp_p2m ~ blk + opp_fga +
                         factor(team) +
                         factor(year))

# he states it with fg2m_opp but I come much closer to his results with regressing fgm_opp
# that is proposed in the web article
blk_on_opp_FGM <- lm(data = team_pg , opp_fgm ~ blk + opp_fga +
                         factor(team) +
                         factor(year))
summary(blk_on_opp_FGM)

# with this I get from Berri 2008
unique(coefficients(blk_on_opp_FGM)["blk"] * - team_pg$marg_val_2FGM) # mein wert nach berris methode
team_pg <- team_pg %>% 
    mutate(marg_val_BLK = unique(coefficients(blk_on_opp_FGM)["blk"] * - team_pg$marg_val_2FGM))

#******************************************************************************#
# value of pf
# value of pf wagesofwins and web resource:
# approximately the same as FTM
team_pg <- team_pg %>% 
    mutate(marg_val_PF = -marg_val_FTM)

# value of assists
# indirect value as we can see if we add it to equation ...
useless_ass <- lm (win_pct ~ PTS_per_PE + OPP_PTS_per_PA + ast, data = team_pg)
summary(useless_ass)
# = assists are insignificant! if we know PTS scored and Opponents Points scored
# wages of wins shows val_AST = 0.022

team_pg <- team_pg %>% 
    mutate(marg_val_AST = 0.022,
           marg_val_opp_3FGM = - marg_val_3FGM,
           marg_val_opp_2FGM = - marg_val_2FGM,
           marg_val_opp_TOV = - marg_val_Opp_PA,
           marg_val_TREB = - marg_val_Opp_PA
    )

marg_values <- team_pg %>% 
    select(marg_val_3FGM, marg_val_2FGM, marg_val_FTM, marg_val_MFG,
           marg_val_MFT,marg_val_ORB, marg_val_DRB, marg_val_TOV, marg_val_STL,
           marg_val_PF, marg_val_BLK,marg_val_AST, marg_val_opp_3FGM,
           marg_val_opp_2FGM, marg_val_opp_TOV, marg_val_TREB) %>% 
    unique()

marg_value_names <- c("3FGM", "2FGM", "FTM", "MFG", "MFT", "ORB", "DRB", "TOV", "STL",
                      "PF","BLK","AST",
                      "opp_3FGM", "opp_2FGM", "opp_TOV", "TREB")

names(marg_values) <- marg_value_names
marg_values_rounded <- round(marg_values,3)
print(marg_values_rounded)

#******************************************************************************#
# Step 1 - Calculating WINS PRODUCED per team ----

# Step 1: player production per 48 minutes
#Player production:
# foul percentage
team_for_merge <- team_pg %>% 
    select(team, opp_ftm, pf, G, year) %>% 
    rename(pf_t = pf) %>% 
    rename(G_t = G)

player_for_merge <- player_totals %>% 
    select(player, team, pf,G,year) %>% 
    mutate(pf_p = pf) %>% 
    rename(G_p = G)

player_perT <- merge(player_for_merge,team_for_merge,
                     by = c("team","year"),
                     all = TRUE) %>% 
    mutate(pf_p1 = pf_p / G_t)

player_perT <- player_perT %>% 
    group_by(team,year) %>% 
    mutate(PF_perc = pf_p1 / pf_t,
           pf_p_new = opp_ftm * PF_perc * G_t) %>% 
    ungroup() %>% 
    select(player,team,year,pf_p_new)

player_totals <- merge(player_totals, player_perT,
                       by = c("player","team","year")) %>% 
    relocate(fga, fgm, .after = min_p)

#
player_totals <- player_totals %>% 
    mutate(total_production  = (
        p3m * marg_values$`3FGM`
        + p2m  * marg_values$`2FGM`
        + ftm  * marg_values$FTM
        + (p2a - p2m) * marg_values$MFG
        + (p3a - p3m)* marg_values$MFG
        + (fta - ftm) * marg_values$MFT
        + orb * marg_values$ORB
        + drb * marg_values$DRB
        + tov  * marg_values$TOV
        + stl  * marg_values$STL
        + pf_p_new * marg_values$PF
        + blk  * marg_values$BLK)
        #+ ast  * marg_values$AST)
    )

wins_produced <- player_totals %>% 
    select(player,team,year,G, min_p,Pos.,total_production) %>%
    mutate(P48 = (ifelse(min_p>0,(total_production/min_p) * 40, 0))) %>%
    arrange(player)

ggplot(wins_produced, aes(y = total_production,x = min_p)) +
    geom_point()

#******************************************************************************#
team_influence <- readRDS("data/teammates_influence.Rds")
# Step 2 - Adjusting for teammates production of defensive rebounds ----
# We now need to regress the single player productivity measures (FTA, REB, TOV, etc) like that:
# x_1 = x_0 + age + age^2 + (%of games played in the last two seasons) + dummy (position played)
#     + dummy(new coach) + dummy(new team) + dummy (year) + roster stability
#     + teammate's per minute of x
# teammate's productivity is not robust (only for defensive rebounds) thus we account onl for teammate's production of
# defensive rebounds
DREBMATE = 0.504 # Berri's number
print(DREBMATE)
DREBMATE = - team_influence$Estimate[team_influence$Statistic =="tDREBpm"] # my number
print(DREBMATE)
# Step 2.1
# TDREBPM = (Team DREB - Player DREB) / (Team minutes played - player minutes played)
# calculate how many DREB's a team gathers without the player of interest and adjust
# according to minutes played
DREB_adjustment <- merge(player_totals, team_totals,
                         by = c("team","year"),
                         all = TRUE) %>%
    arrange(player) %>%
    select(player,team,year,min_p,min,drb.x,drb.y)

DREB_adjustment <- DREB_adjustment %>%
    mutate(TDREBPM =(drb.y - drb.x) / (min - min_p))

# Step 2.2 - Estimate how much production is lost cause of that
# Thus, multiply by estimated value for DREB lost because of teammates (for now 0.504)
DREB_adjustment <- DREB_adjustment %>%
    mutate(TDREBPM_ind_reb_loss = TDREBPM * - DREBMATE)

# Step 2.3
# calculate marginal value of productivity of team defensive rebounds for each player lost
# thus multiply with playing time of specific player
DREB_adjustment <- DREB_adjustment %>%
    mutate(TDREB_ind_loss = TDREBPM_ind_reb_loss * marg_values$DRB * min_p)

# Step 2.4
# sum across all team members and id
DREB_adjustment <- DREB_adjustment %>%
    group_by(team,year) %>%
    mutate(TDREB_totalloss = sum(TDREB_ind_loss)) %>%
    ungroup()

# Step 2.5
# Now multiply this by % of defensive rebounds from each player per team and id
# this yields how many rebounds a player grabbed from his teammates
DREB_adjustment <- DREB_adjustment %>%
    group_by(team,year) %>%
    mutate(rebound_percentage = (drb.x / drb.y)) %>%
    mutate(eins = sum(rebound_percentage)) %>%
    ungroup()

DREB_adjustment$TDREB_captured_from_tm <- DREB_adjustment$TDREB_totalloss * DREB_adjustment$rebound_percentage

# Step 2.6
# subtract step 5 from step 4 and add the result to PROD
DREB_adjustment$net_TDREB <- DREB_adjustment$TDREB_captured_from_tm - DREB_adjustment$TDREB_ind_loss

wins_produced <- wins_produced %>%
    mutate(net_TDREB = DREB_adjustment$net_TDREB) %>% 
    mutate(PROD_adj = DREB_adjustment$net_TDREB + total_production) %>%
    mutate(P48Adj =if_else(min_p==0,0,PROD_adj / min_p * 40))

#******************************************************************************#
# Step 3 - Adjust for Assists----
# player_adj_fg% = f(player_adj_fg%_last_season + age + age^2 +%gp last 2 seasons +
#  dummy(position) + dummy(new coach) + dummy(new team) + dummy(year) + 
#                    dummy(roster stability) +
#                    teammate's production of assists +
#                    teammate's adjusted field goal percentage)
# Disclaimer: is this really the same value for all teams across time?!
assist_adjustment <- merge(player_totals, team_totals,
                           by.x = c("team","year"),
                           by.y = c("team","year"),
                           all = TRUE) %>%
    relocate(player, .before = team) %>% 
    arrange(player) %>%
    select(1:2,year,min,min_p,ast.x,ast.y, fga.x)

# Step 3.1 - Calculate Teammate's assist per minute
assist_adjustment <- assist_adjustment %>%
    mutate(TAPM =(ast.y - ast.x) / (min - min_p))

# Step 3.2 - Multiply TAPM by coefficient of assists produced by teammates
tASTpm <- 0.725
print(tASTpm)
tASTpm = team_influence$Estimate[team_influence$Statistic =="tASTpm"] # my number
print(tASTpm)

assist_adjustment <- assist_adjustment %>%
    mutate(TAPM_val =TAPM * tASTpm) %>%
    mutate(TAPM_val_FG = TAPM_val * 2)

# Step 3.3 - Multiply TAPM_val_FG by field goals taken. 
assist_adjustment <- assist_adjustment %>%
    mutate(points_credit_team =TAPM_val_FG * fga.x)

# Step 3.4 - Multiply by the impact that points have on wins (players production which is credit due teammate's)
assist_adjustment <- assist_adjustment %>%
    mutate(ast_val_team = points_credit_team * marg_values$`2FGM`)

# Step 3.5 - Sum across all players of team and year and compute assist percentages for every player
assist_adjustment <- assist_adjustment %>%
    group_by(team,year) %>%
    mutate(ast_total_val = sum(ast_val_team)) %>%
    mutate(ast_percentage = ast.x / ast.y) %>%
    mutate(eins = sum(ast_percentage)) %>%
    ungroup()

# Step 3.6 - Allocate ass_total_val according to assist percentages from players
# and calculate net assists for each player
assist_adjustment <- assist_adjustment %>%
    mutate(ast_val_ind = ast_total_val * ast_percentage) %>%
    mutate(ast_net = ast_val_ind - ast_val_team)

# Step 3.7 compute adjusted production and P48
wins_produced <- wins_produced %>%
    mutate(ast_net = assist_adjustment$ast_net) %>% 
    mutate(PROD_adj2 = PROD_adj + assist_adjustment$ast_net) %>%
    mutate(P48Adj2 =if_else(min_p==0, 0, (PROD_adj2 / min_p) * 40))

#******************************************************************************#
# Step 4 - Team defense adjustments----
# Calculating Team Turnovers
team_adjustment <- player_totals %>%
    group_by(team,year) %>%
    mutate(TTOV = sum(tov)) %>%
    ungroup()

team_adjustment <- merge(team_adjustment, team_totals,
                         by.x = c("team","year"),
                         by.y = c("team","year"),
                         all = TRUE) %>% 
    arrange(player) %>%
    select(1:3,min,min_p,opp_p3m,opp_p2m,opp_tov,REBTM,TTOV,blk.y,stl.y,tov.y) %>% 
    mutate(TeamTOV = tov.y - TTOV)

team_adjustment <- team_adjustment %>%
    mutate(TOV_forced = opp_tov - stl.y)

attach(team_adjustment)
team_adjustment$Team_adj <- (((opp_p3m * marg_values$opp_3FGM +
                                   opp_p2m * marg_values$opp_2FGM +
                                   TOV_forced * marg_values$opp_TOV +
                                   TeamTOV * marg_values$TOV +
                                   REBTM * marg_values$TREB +
                                   blk.y * -marg_values$BLK)/ (min)) * 40)
detach(team_adjustment)

team_adjustment <- team_adjustment %>%
    group_by(year) %>%
    mutate(Team_adj_avg = mean(Team_adj)) %>%
    mutate(Team_defense_adj = Team_adj - Team_adj_avg) %>%
    ungroup()

# Step 4.... compute adjusted P48
wins_produced <- wins_produced %>%
    mutate(team_adj = team_adjustment$Team_defense_adj) %>% 
    mutate(P48Adj3 = P48Adj2 + team_adjustment$Team_defense_adj)

#******************************************************************************#
# Step 5 Adjusting for position played----
pos_avg_1 <- wins_produced %>% 
    group_by(player,year) %>%
    filter(., min_p/G > 10 & G > 7) %>% 
    summarise_at("P48Adj3",mean)

pos_avg_2 <- merge(wins_produced,pos_avg_1,
                   by = c("player","year"),
                   all = TRUE)

pos_avg_3 <- pos_avg_2 %>% 
    filter(., min_p/G > 10 & G > 7) %>%
    group_by(Pos.,year) %>% 
    mutate(pos_mean = mean(P48Adj3.y)) %>% 
    ungroup()

ggplot(data = pos_avg_3, aes(x = min_p, y = P48Adj3.y)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x)

test <- lm(P48Adj3.y ~ min_p, data = pos_avg_3)
summary(test)

pos_avg_4 <- select(pos_avg_3, Pos., pos_mean,year)
pos_avg_5 <- pos_avg_4 %>% 
    group_by(year) %>% 
    dplyr::distinct(., Pos., .keep_all = T) %>% 
    ungroup
print(pos_avg_5)

pos_avg_final <- merge(wins_produced, pos_avg_5,
                       by = c("Pos.","year"))%>% 
    arrange(year,player)

wins_produced <- pos_avg_final

wins_produced <- wins_produced %>%
    mutate(P48Adj4 =P48Adj3 - pos_mean) %>% 
    mutate(P48Adj5 = (P48Adj4 + 0.099)) %>% 
    mutate(PROD_adj_total = (P48Adj5) / 40 * min_p)

mean(wins_produced$P48Adj4)

#******************************************************************************#
# Save final scores in df to compare:----
wp_berri_b <- wins_produced %>% 
    select(player,team,year,min_p,PROD_adj_total) %>% 
    rename(wp = PROD_adj_total)

saveRDS(object = wp_berri_b, file = paste0("Data/estimates/wp_berri_b.Rds"))

#******************************************************************************#
# Comparison:----
wins_for_merge <- team_totals %>% 
    select(team,year,G,W,L)

win_est <- wins_produced %>% 
    group_by(team,year) %>% 
    summarise(win_est = sum(PROD_adj_total))

wins<- merge(wins_for_merge,win_est,
             by = c("year","team"))

wins <- wins %>% 
    mutate(diff = W - win_est,
           abs_diff = abs(diff)) %>% 
    group_by(year) %>% 
    mutate(avg_dif = mean(abs_diff))