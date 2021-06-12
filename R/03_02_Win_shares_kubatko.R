# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

library(tidyverse)

#******************************************************************************#
# Erinnerung:----
# change hard coded variables 
# - need all league data team, team opponent and player data 

#******************************************************************************#
# load data----
player_totals <- readRDS("Data/player_data_totals.Rds") %>% 
    filter(year == 2018) %>% 
    rename(min = min_p) %>% 
    select(-pf_p)

team_totals <- readRDS("Data/team_data_totals.Rds") %>% 
    filter(year == 2018)

#******************************************************************************#

a <- merge(player_totals,team_totals, by = "team", suffix = c("_p","_t"))

#******************************************************************************#
# hard coded variables please change:----
lg_avg_ppp <- 1.083
lg_avg_ppg <- 100
pace_lg <- 91.7


#******************************************************************************#
# calc. scoring possessions ----
a <- a %>% 
    mutate(qAST = ((min_p / (min_t / 5)) * (1.14 * ((ast_t - ast_p) / fgm_t))) + 
               ((((ast_t / min_t) * min_p * 5 - ast_p) / ((fgm_t / min_t) * min_p * 5 - fgm_p)) * (1 - (min_p / (min_t / 5)))),
           FG_Part = fgm_p * (1 - 0.5 * ((pts_p - ftm_p) / (2 * fga_p)) * qAST),
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
a <- a %>% 
    mutate(
        FGxPoss = (fga_p - fgm_p) * (1 - 1.07 * ORB_t_pct),
        FTxPoss = ifelse(fta_p>0, ((1 - (ftm_p / fta_p))^2) * 0.4 * fta_p, 0) 
    )

#******************************************************************************#
# Total Possessions:----
a <- a %>% 
    mutate(TotPoss = ScPoss + FGxPoss + FTxPoss + tov_p)

#******************************************************************************#
# Individual Points Produced:----
a <- a %>% 
    mutate(
        PProd_FG_Part = 2 * (fgm_p + 0.5 * p3m_p) * (1 - 0.5 * ((pts_p - ftm_p) / (2 * fga_p)) * qAST),
        PProd_AST_Part = 2 * ((fgm_t - fgm_p + 0.5 * (p3m_t - p3m_p)) / (fgm_t - fgm_p)) * 0.5 * (((pts_t - ftm_t) - (pts_p - ftm_p)) / (2 * (fga_t - fga_p))) * ast_p,
        PProd_ORB_Part = orb_p * ORB_t_Weight * Team_Play_pct * (pts_t / (fgm_t + (1 - (1 - (ftm_t / fta_t))^2) * 0.4 * fta_t)),
        
        PProd = (PProd_FG_Part + PProd_AST_Part + ftm_p) * (1 - (orb_t / Team_Scoring_Poss) * ORB_t_Weight * Team_Play_pct) + PProd_ORB_Part)

#******************************************************************************#
# Individual offensive rating:----
a <- a %>% 
    mutate(ORtg = 100 * (PProd / TotPoss))

#******************************************************************************#
# Individual defensive stops----
c <- a %>% 
    mutate(poss_t = 0.5 *
               ((fga_t + 0.4 * fta_t - 1.07 * (orb_t / (orb_t + opp_drb)) * (fga_t - fgm_t) + tov_t) +
                    (opp_fga + 0.4 * opp_fta - 1.07 * (opp_orb / (opp_orb + drb_t)) * (fga_t - opp_fgm) + opp_tov))
    )

c <- c %>% 
    mutate(
        dor_pct = opp_drb / (opp_orb + drb_t),
        dfg_pct = opp_fgm / opp_fga,
        FMwt = (dfg_pct * (1 - dor_pct)) / (dfg_pct * (1 - dor_pct) + (1 - dfg_pct) * dor_pct),
        Stops1 = stl_p + blk_p * FMwt * (1 - 1.07 * dor_pct) + drb_p * (1 - FMwt),
        
        Stops2 = (((opp_fga - opp_fgm - blk_t) / min_t) * FMwt * (1 - 1.07 * dor_pct) +
                      ((opp_tov - stl_t) / min_t)) * min_p + (pf_p / pf_t) * 0.4 * opp_fta * (1 - (opp_ftm / opp_fta))^2,
        
        Stops = Stops1 + Stops2,
        Stop_pct = (Stops * opp_min) / (poss_t * min_p)
        
    )

#******************************************************************************#
# Individual defensive stops----
c <- c %>% 
    mutate(
        Team_Defensive_Rating = 100 * (opp_pts / poss_t),
        D_Pts_per_ScPoss = opp_pts / (opp_fgm + (1 - (1 - (opp_ftm / opp_fta))^2) * opp_fta*0.4),
        
        DRtg = Team_Defensive_Rating + 0.2 * (100 * D_Pts_per_ScPoss * (1 - Stop_pct) - Team_Defensive_Rating)
    )

#******************************************************************************#
# offensive win shares:----
d <- c %>% 
    mutate(marg_off = PProd - 0.92 * lg_avg_ppp * TotPoss,
           pace_t = (poss_t * 2 * 40) / (2 * min_t / 5),
           marg_ppw = 0.32 * lg_avg_ppg * (pace_t / pace_lg),
           owin_share = marg_off / marg_ppw)

#******************************************************************************#
# defensive win shares----
e <- d %>% 
    mutate(
        marg_def = (min_p / min_t * poss_t)  * ((1.08 * lg_avg_ppp) - DRtg / 100),
        dwin_share = marg_def / marg_ppw
    )

#******************************************************************************#
# total win shares----
f <- e %>% 
    mutate(win_shares = owin_share + dwin_share)

sum(f$win_shares)

win_shares <- select(f,
                    player, team, min_p, owin_share, dwin_share, win_shares)
