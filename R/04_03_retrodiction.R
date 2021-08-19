# Clear Console
cat("\014")

# remove Environment
rm(list=ls())


library(tidyverse)

#******************************************************************************#
# Load files:----
ranking <- readRDS("Data/estimates/ranking.Rds")

team_totals <- readRDS("Data/team_data_totals.Rds")
#******************************************************************************#
# predict team wins in 2015 with stats from 2014
metrics2014 <- ranking %>% 
    filter(year == 2014) %>% 
    mutate(VORP_pm = ifelse(min_p > 100, VORP /min_p, -2),
           ws_pm = ifelse(min_p > 100,win_shares / min_p,0.045),
           wp_B_pm = ifelse(min_p > 100,wp_B /min_p,0.045),
           bbl_eff_pm = ifelse(min_p > 100,bbl_eff / min_p,100)) %>% 
    dplyr::select(player,team,VORP_pm,ws_pm,wp_B_pm,bbl_eff_pm)

min2015 <- ranking %>% 
    filter(year == 2015) %>% 
    dplyr::select(player,team,min_p)

pred_merg <- merge(metrics2014,min2015, by = c("player","team"), all = TRUE)

wechsel_in14_no15 <- setdiff(metrics2014$player,min2015$player)
wechsel_in15_no14 <- setdiff(min2015$player,metrics2014$player)

# 80 von 482 bleiben beim selben team in der liga! :(

pred_merg_1 <- merge(metrics2014,min2015, by = c("player"), all = TRUE)
# 482 - 437 = 45 wechseln das team
# 45 + 80 = 125 sind weiterhin in der Liga

pred_merg_1 <- pred_merg_1 %>% 
    mutate(same_team = ifelse(team.x == team.y, 1, 0)) %>% 
    replace_na(list(same_team = 3)) %>% 
    mutate(new = ifelse(same_team == 3,1,0)) %>% 
    replace_na(list(min_p = 0)) %>%
    filter(., min_p != 0) %>% 
    group_by(same_team)
str(pred_merg_1)

# 2014/15 2015/16:: 48 changed team + 80 same team = 128 stayed + 156 new = 45%
# 2015/16 2016/17:: 55 changed team + 84 same team = 139 stayed + 151 new = 47.9 %
# 2016/17 2017/18:: 50 changed team + 92 same team = 142 stayed + 148 new = 51 %
# 2017/18 2018/19:: 42 changed team + 101 same team = 143 stayed + 145 new = 49.6 %
# 2018/19 2019/20:: 41 changed team + 88 same team = 129 stayed + 140 new = 47.9 %
# 2019/19 2020/21:: 55 changed team + 83 same team = 138 stayed + 166 new = 45.4 %