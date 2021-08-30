# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

library(tidyverse)

#******************************************************************************#
# load data----
player_totals <- readRDS("data/player_data_totals.Rds") %>% 
    rename(min = min_p) %>%
    filter(min > 0) %>% 
    dplyr::select(-PF_perc)

team_totals <- readRDS("data/team_data_totals.Rds")

#******************************************************************************#

a <- merge(player_totals,team_totals, by = c("team","year"), suffix = c("_p","_t"))

#******************************************************************************#
# calc. scoring possessions ----
#Individual scoring possessions reflect a player's contributions to a
# team's scoring possessions. The first contribution is through field goals,
#sharing credit with those who assisted on the shots:
    
scoring_poss <- a %>% 
    mutate(qAST = ((min_p / (min_t / 5)) * (1.14 * ((ast_t - ast_p) / fgm_t))) + 
               ((((ast_t / min_t) * min_p * 5 - ast_p) / ((fgm_t / min_t) * min_p * 5 - fgm_p)) * (1 - (min_p / (min_t / 5)))),
           
           FG_Part = ifelse(fga_p>0,fgm_p * (1 - 0.5 * ((pts_p - ftm_p) / (2 * fga_p)) * qAST),0),
           
           AST_Part = 0.5 * (((pts_t - ftm_t) - (pts_p - ftm_p)) / (2 * (fga_t - fga_p))) * ast_p,
           
           FT_Part = ifelse(fta_p>0, (1-(1-(ftm_p/fta_p))^2)*0.4*fta_p,0),
           
           Team_Scoring_Poss = fgm_t + (1 - (1 - (ftm_t / fta_t))^2) * fta_t * 0.4,
           
           
           ORB_t_pct = orb_t / (orb_t + (opp_trb - opp_orb)),
           
           Team_Play_pct = Team_Scoring_Poss / (fga_t + fta_t * 0.4 + tov_t),
           
           ORB_t_Weight = ((1 - ORB_t_pct) * Team_Play_pct) / ((1 - ORB_t_pct) * Team_Play_pct + ORB_t_pct * (1 - Team_Play_pct)),
           
           ORB_Part = orb_p * ORB_t_Weight * Team_Play_pct,
           
           
           ScPoss = (FG_Part + AST_Part + FT_Part) * (1 - (orb_t / Team_Scoring_Poss) * ORB_t_Weight * Team_Play_pct) + ORB_Part
    )

#******************************************************************************#
# Missed FG and Missed FT Possessions:----
tot_poss <- scoring_poss %>% 
    mutate(
        FGxPoss = (fga_p - fgm_p) * (1 - 1.07 * ORB_t_pct),
        FTxPoss = ifelse(fta_p>0, ((1 - (ftm_p / fta_p))^2) * 0.4 * fta_p, 0) 
    )

#******************************************************************************#
# Total Possessions:----
tot_poss <- tot_poss %>% 
    mutate(TotPoss = ScPoss + FGxPoss + FTxPoss + tov_p)

totposs_t <- tot_poss %>%
    group_by(team,year) %>% 
    mutate(tot_poss_t = sum(TotPoss)) %>% 
    select(player,team,year,tot_poss_t,qAST,min_p)

#******************************************************************************#
# Individual Points Produced:----
PTS_prod <- tot_poss %>% 
    mutate(
        PProd_FG_Part = ifelse(fga_p > 0, 2 * (fgm_p + 0.5 * p3m_p) * (1 - 0.5 * ((pts_p - ftm_p) / (2 * fga_p)) * qAST),0),
        PProd_AST_Part = 2 * ((fgm_t - fgm_p + 0.5 * (p3m_t - p3m_p)) / (fgm_t - fgm_p)) * 0.5 * (((pts_t - ftm_t) - (pts_p - ftm_p)) / (2 * (fga_t - fga_p))) * ast_p,
        PProd_ORB_Part = orb_p * ORB_t_Weight * Team_Play_pct * (pts_t / (fgm_t + (1 - (1 - (ftm_t / fta_t))^2) * 0.4 * fta_t)),
        
        PProd = (PProd_FG_Part + PProd_AST_Part + ftm_p) * (1 - (orb_t / Team_Scoring_Poss) * ORB_t_Weight * Team_Play_pct) + PProd_ORB_Part)

aa <- PTS_prod %>% 
    select(player,team,fgm_p,pts_p,ftm_p,fga_p,PProd_FG_Part,PProd)

#******************************************************************************#
# Individual offensive rating:----
oRTG <- PTS_prod %>% 
    mutate(ORtg = 100 * (PProd / TotPoss),
           poss_t = 0.5 *
               ((fga_t + 0.4 * fta_t - 1.07 * (orb_t / (orb_t + opp_drb)) * (fga_t - fgm_t) + tov_t) +
                    (opp_fga + 0.4 * opp_fta - 1.07 * (opp_orb / (opp_orb + drb_t)) * (fga_t - opp_fgm) + opp_tov))
           )

#******************************************************************************#
# Possessions & League averages:----
poss_t <- a %>% 
    mutate(poss_t = 0.5 *
               ((fga_t + 0.4 * fta_t - 1.07 * (orb_t / (orb_t + opp_drb)) * (fga_t - fgm_t) + tov_t) +
                    (opp_fga + 0.4 * opp_fta - 1.07 * (opp_orb / (opp_orb + drb_t)) * (fga_t - opp_fgm) + opp_tov))
    )

lg_avg <- poss_t %>% 
    group_by(year) %>% 
    mutate(avg_ppp_t = pts_t / poss_t,
           avg_ppg_t = pts_t / G_t,
           possible_min = mean(min_t)/G_t,
           pace_t = (possible_min/min_t)*(poss_t + poss_t)/2) %>%
    distinct(team, .keep_all = TRUE) %>% 
    mutate(ppp = mean(avg_ppp_t),
           ppg = mean(avg_ppg_t),
           pace = mean(pace_t)) %>% 
    distinct(ppp, .keep_all = TRUE) %>% 
    select(year,ppp,ppg,pace)

#******************************************************************************#
# Individual stop percentage:----
stop_pct <- poss_t %>% 
    mutate(
        dor_pct = opp_orb / (opp_orb + drb_t),
        dfg_pct = opp_fgm / opp_fga,
        FMwt = (dfg_pct * (1 - dor_pct)) / (dfg_pct * (1 - dor_pct) + (1 - dfg_pct) * dor_pct),
        Stops1 = stl_p + blk_p * FMwt * (1 - 1.07 * dor_pct) + drb_p * (1 - FMwt),
        
        Stops2 = (((opp_fga - opp_fgm - blk_t) / min_t) * FMwt * (1 - 1.07 * dor_pct) +
                      ((opp_tov - stl_t) / min_t)) * min_p + (pf_p / pf_t) * 0.4 * opp_fta * (1 - (opp_ftm / opp_fta))^2,
        
        Stops = Stops1 + Stops2,
        Stop_pct = (Stops * opp_min) / (poss_t * min_p)
    )

#******************************************************************************#
# Individual defensive rating:----
dRTG <- stop_pct %>% 
    mutate(
        Team_Defensive_Rating = 100 * (opp_pts / poss_t),
        D_Pts_per_ScPoss = opp_pts / (opp_fgm + (1 - (1 - (opp_ftm / opp_fta))^2) * opp_fta*0.4),
        
        DRtg = Team_Defensive_Rating + 0.2 * (100 * D_Pts_per_ScPoss * (1 - Stop_pct) - Team_Defensive_Rating)
    )

#******************************************************************************#
# WIN SHARES:----
# offensive win shares:----
oRTG_merg <- merge(oRTG,lg_avg, by = "year")
oWin_shares <- oRTG_merg %>% 
    mutate(marg_off = PProd - 0.92 * ppp * TotPoss,
           pace_t = (poss_t * 2 * 40) / (2 * min_t / 5),
           marg_ppw = 0.32 * ppg * (pace_t / pace),
           owin_share = marg_off / marg_ppw)

#******************************************************************************#
# defensive win shares----
dRTG_merg <- merge(dRTG,lg_avg, by = "year")
dWin_shares <- dRTG_merg %>% 
    mutate(
        marg_def = (min_p / min_t * poss_t)  * ((1.08 * ppp) - DRtg / 100),
        pace_t = (poss_t * 2 * 40) / (2 * min_t / 5),
        marg_ppw = 0.32 * ppg * (pace_t / pace),
        dwin_share = marg_def / marg_ppw)

#******************************************************************************#
# total win shares----
win_shares_merg <- merge(oWin_shares,dWin_shares, by = c("year","team","player","min_p"))
Win_Shares <- win_shares_merg %>% 
    mutate(win_shares = owin_share + dwin_share) %>% 
    dplyr::select(player, team,year, min_p, owin_share, dwin_share, win_shares) %>% 
    arrange(-win_shares)

sum(Win_Shares$win_shares)

#******************************************************************************#
# save final win shares in data frame: ----
saveRDS(object = Win_Shares, file = paste0("data/estimates/win_shares.Rds"))

attach(oRTG)
mean((orb_t / Team_Scoring_Poss) * ORB_t_Weight * Team_Play_pct)
detach(oRTG)
