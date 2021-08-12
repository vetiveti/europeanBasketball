# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# set working directory
setwd("~/Uni Tübingen/0. Masterarbeit/7. R/europeanBasketball")

library(tidyverse)

#******************************************************************************#
# load data----
player_totals <- readRDS("Data/player_data_totals.Rds") %>% 
    rename(min = min_p) %>%
    filter(min > 0) %>% 
    dplyr::select(-PF_perc)

team_totals <- readRDS("Data/team_data_totals.Rds")

#******************************************************************************#
a <- merge(player_totals,team_totals, by = c("team","year"), suffix = c("_p","_t"))

b <- a %>%
    mutate(poss_t = 0.5 *
          ((fga_t + 0.4 * fta_t - 1.07 * (orb_t / (orb_t + opp_drb)) * (fga_t - fgm_t) + tov_t) +
           (opp_fga + 0.4 * opp_fta - 1.07 * (opp_orb / (opp_orb + drb_t)) * (fga_t - opp_fgm) + opp_tov))) %>% 
    group_by(year) %>% 
    mutate(lg_AST = sum(ast_p),
           lg_FG  = sum(fgm_p),
           lg_FT  = sum(ftm_p),
           lg_PTS = sum(pts_p),
           lg_FGA = sum(fga_p),
           lg_TOV = sum(tov_p),
           lg_FTA = sum(fta_p),
           lg_TRB = sum(trb_p),
           lg_ORB = sum(orb_p),
           lg_TRB = sum(trb_p),
           lg_PF  = sum(pf_p),
           possible_min = mean(min_t)/G_t,
           Pace_t = (possible_min/min_t)*(poss_t + poss_t)/2 ,
           lg_Pace= mean(Pace_t)) %>%
    ungroup()

# hier und bei win_shares nochmal pace anschauen!----
# https://captaincalculator.com/sports/basketball/pace-factor-calculator/
#******************************************************************************#
# PER by John Hollinger:----



PER <- b %>% 
    mutate(factor = (2/3) - (0.5 * (lg_AST / lg_FG)) / (2 * (lg_FG / lg_FT)),
           # Value of possession = VOP
           VOP = lg_PTS / (lg_FGA - lg_ORB + lg_TOV + 0.44 * lg_FTA),
           DRB_perc = (lg_TRB - lg_ORB) / lg_TRB,
        uPER = (1 / min_p)*
               (p3m_p 
                + (2/3)*ast_p 
                + (2-factor*(ast_t / fgm_t))*fgm_p
                + (ftm_p*0.5*(1+(1-(ast_t / fgm_t)) + (2/3)*(ast_t/fgm_t)))
                - VOP * tov_p - VOP * DRB_perc * (fga_p - fgm_p) 
                - VOP * 0.44 * (0.44 + (0.56 * DRB_perc)) * (fta_p - ftm_p)
                + VOP * (1 - DRB_perc) * (trb_p - orb_p)
                + VOP * DRB_perc * orb_p
                + VOP * stl_p
                + VOP * DRB_perc * blk_p
                - pf_p * ((lg_FT / lg_PF) - 0.44 * (lg_FTA / lg_PF) * VOP))
               )

PER_adj <- PER %>% 
    mutate(pace_adj = lg_Pace / Pace_t,
           aPER = uPER * pace_adj) %>% 
    group_by(year) %>% 
    filter(min_p > 0) %>% 
    mutate(lg_aPER = mean(aPER)) %>% 
    ungroup()

PER_final <- PER_adj %>% 
    mutate(PER = aPER * (15 / lg_aPER)) %>% 
    dplyr::select(year,player,min_p,PER) %>% 
    arrange(player)

#******************************************************************************#
# Game Score by John Hollinger:----
Game_score <- player_totals %>% 
    mutate(game_score = pts + 0.4*fgm + 0.7*(orb) + 0.3*drb + stl + 0.7*ast 
           + 0.7*blk - 0.7*(fga) - 0.4*(fta-ftm) - 0.4*pf - tov) %>% 
    arrange(player) %>% 
    dplyr::select(game_score,player,year, min)

Hollinger <- merge(PER_final,Game_score, by = c("player","year"))

cor(Hollinger$PER,Hollinger$game_score)

saveRDS(object = Hollinger, file = paste0("Data/estimates/PER_game_score.Rds"))
