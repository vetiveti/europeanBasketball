# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# set working directory
setwd("~/Uni Tübingen/0. Masterarbeit/7. R/europeanBasketball")

library(tidyverse)
require(openxlsx)

#******************************************************************************#
# load data----
player_totals <- readRDS("Data/player_data_totals.Rds") %>% 
  rename(min = min_p) %>% 
  dplyr::select(-PF_perc)

team_totals <- readRDS("Data/team_data_totals.Rds") 

#******************************************************************************#
# Merge data frames:----
player_stats <-merge(player_totals,team_totals, by = c("team","year"), suffix = c("_p","_t")) %>% 
  filter(min_p > 0)

#******************************************************************************#

# per 100 possessions
# pace is defined as:
# pace = poss/min * 40
# poss = min * pace / 40

# adjusting for team shooting context: ----
player <- player_stats %>% 
  mutate(tsa = fga_p + 0.44 * fta_p) %>% 
  mutate(pt_tsa = if_else(pts_p>0, pts_p / tsa,0))  %>% 
  mutate(pts_tsa_t = pts_t / (fga_t + fta_t* 0.44)) %>%
  mutate(pts_tsa_base = 1) %>% 
  mutate(adj.pts = ((pt_tsa - pts_tsa_t) + pts_tsa_base) * tsa)
  
#******************************************************************************#
# calculate possessions:----
# Basketball-reference methode:
player <- player %>% 
  mutate(poss_t = 0.5*((fga_t+0.4*fta_t-1.07*(orb_t/(orb_t + opp_drb))*(fga_t-fgm_t)+tov_t)+
                     (opp_fga+0.4*opp_fta-1.07*(opp_orb/(opp_orb+drb_t))*(opp_fga-opp_fgm)+opp_tov)),
  pace_t = 40 * (poss_t / (min_t/5))
                  )

player <- player %>% 
  mutate(poss_p = min_p * pace_t / 40)

#******************************************************************************#
# threshpts:----
pts_Threshold <- -0.33
player <- player %>% 
  mutate(thresh_pts = tsa * (pt_tsa - (pts_tsa_t + pts_Threshold))) %>% 
  group_by(team,year) %>% 
  mutate(thresh_pts_t = sum(thresh_pts)) %>% 
  ungroup()

#******************************************************************************#
# calculate stats_per100----
player <- player %>%
  mutate(pts_100 = adj.pts / poss_p * 100,
         p3m_100 = p3m_p / poss_p * 100, 
         p3a_100 = p3a_p / poss_p * 100,
         p2m_100 = p2m_p / poss_p * 100,
         p2a_100 = p2a_p / poss_p * 100,
         fgm_100 = fgm_p / poss_p * 100, 
         fga_100 = fga_p / poss_p * 100, 
         ftm_100 = ftm_p / poss_p * 100, 
         fta_100 = fta_p / poss_p * 100, 
         drb_100 = drb_p / poss_p * 100, 
         orb_100 = orb_p / poss_p * 100, 
         trb_100 = trb_p / poss_p * 100, 
         ast_100 = ast_p / poss_p * 100, 
         stl_100 = stl_p / poss_p * 100, 
         tov_100 = tov_p / poss_p * 100, 
         blk_100 = blk_p / poss_p * 100, 
         pf_100 = pf_p / poss_p * 100)

#******************************************************************************#
# stats% ----
player <- player %>% 
  mutate(min_pct = min_p / (min_t/5),
         trb_pct = trb_p / trb_t / min_pct,
         stl_pct = stl_p / stl_t / min_pct,
         pf_pct = pf_p / pf_t / min_pct,
         ast_pct = ast_p / ast_t / min_pct,
         blk_pct = blk_p / blk_t / min_pct,
         thresh_pts_PCT = thresh_pts / thresh_pts_t / min_pct)

#******************************************************************************#
# POSITION PLAYED:----
# hard code position regression:
position <- data.frame(intercept = 2.1296,
                       TRB_pct = 8.6676,
                       STL_pct = -2.4861,
                       PF_pct = 0.992,
                       AST_pct = -3.5361,
                       BLK_pct = 1.6669)

#******************************************************************************#
# 1. estimate position ----
player <- player %>% 
  mutate(est_pos = position$intercept+
                   trb_pct * position$TRB_pct +
                   stl_pct * position$STL_pct +
                   pf_pct * position$PF_pct +
                   ast_pct * position$AST_pct +
                   blk_pct * position$BLK_pct)

# 2. adjust for players with low minutes (i.e. add their starting position more strongly)
player <- player %>% 
  mutate(pos = case_when(Pos. == "PG" ~ 1,
                         Pos. == "SG" ~ 2,
                         Pos. == "SF" ~ 3,
                         Pos. == "PF" ~ 4,
                         Pos. == "C" ~ 5))

player <- player %>% 
  mutate(min_adj_pos = (est_pos * min_p + pos * 50) / (min_p + 50))

# 3. case when >>1,5 to 1 or 5
player <- player %>% 
  mutate(trim_pos = case_when(
    min_adj_pos >5 ~ 5,
    min_adj_pos <1 ~ 1,
    min_adj_pos >1 & min_adj_pos < 5 ~min_adj_pos))

# 4. calculate team average position
player <- player %>% 
  group_by(team,year) %>% 
  mutate(pos_avg_t = sum(trim_pos * min_p) / min_t) %>% 
  ungroup()

# 2. round: Step 1-4 again:----
# pos_avg_t must be equal to 3
player <- player %>% 
  mutate(adj_pos = min_adj_pos - (pos_avg_t-3),
         trim_pos_2 =  case_when(
           adj_pos >5 ~ 5,
           adj_pos <1 ~ 1,
           adj_pos >1 & adj_pos < 5 ~ adj_pos))

# team average 
player <- player %>% 
  group_by(team,year) %>% 
  mutate(pos_avg_t_2 = sum(trim_pos_2 * min_p) / min_t) %>% 
  ungroup()

# 3. round: Step 1-4 again:----
# pos_avg_t must be equal to 3
player <- player %>% 
  mutate(adj_pos_2  = min_adj_pos - (pos_avg_t-3) - (pos_avg_t_2-3),
         trim_pos_3 =  case_when(
           adj_pos_2 >5 ~ 5,
           adj_pos_2 <1 ~ 1,
           adj_pos_2 >1 & adj_pos_2 < 5 ~adj_pos_2))

# team average 
player <- player %>% 
  group_by(team,year) %>% 
  mutate(pos_avg_t_3 = sum(trim_pos_3 * min_p) / min_t) %>% 
  ungroup()

# 4. round: Step 1-4 again:----
# pos_avg_t must be equal to 3
player <- player %>% 
  mutate(adj_pos_3  = min_adj_pos - (pos_avg_t-3) - (pos_avg_t_2-3) - (pos_avg_t_3-3),
         trim_pos_4 =  case_when(
           adj_pos_3 >5 ~ 5,
           adj_pos_3 <1 ~ 1,
           adj_pos_3 >1 & adj_pos_3 < 5 ~adj_pos_3))

# team average 
player <- player %>% 
  group_by(team,year) %>% 
  mutate(pos_avg_t_4 = sum(trim_pos_4 * min_p) / min_t) %>% 
  ungroup()

a <- select(player, team, pos_avg_t, pos_avg_t_2, pos_avg_t_3, pos_avg_t_4)
# ok fine. now the mean position of all teams is equal to 3

# position final ----
player <- player %>% 
  mutate(pos_final = trim_pos_4)

#******************************************************************************#
# OFFENSIVE ROLE ----

#******************************************************************************#
# hard code offensive role:
offensive_role <- data.frame(
  intercept = 6,
  AST_pct = -6.6416,
  thresh_pct = -8.5541,
  pt_threshhold = -0.33)

#******************************************************************************#
# 1. estimate offensive role ----
player <- player %>% 
  mutate(off_role_est = offensive_role$intercept +
           offensive_role$AST_pct * ast_pct +
           offensive_role$thresh_pct * thresh_pts_PCT)

# 2. adjust for players with few minutes
player <- player %>% 
  mutate(off_role_min_adj = 
           (off_role_est * min_p + 4 * 50) / (50 + min_p))

# 3. case when <>1,5 to 1 or 5
player <- player %>% 
  mutate(trim_role = case_when(
    off_role_min_adj >5 ~ 5,
    off_role_min_adj <1 ~ 1,
    off_role_min_adj >1 & off_role_min_adj < 5 ~off_role_min_adj))

# 4. team average 
player <- player %>% 
  group_by(team,year) %>% 
  mutate(role_avg_t = sum(trim_role * min_p) / min_t) %>% 
  ungroup()

b <- select(player, team, role_avg_t)

# 2. round:----
# pos_avg_t must be equal to 3
player <- player %>% 
  mutate(adj_role = off_role_min_adj - (role_avg_t-3),
         trim_role_2 =  case_when(
           adj_role >5 ~ 5,
           adj_role <1 ~ 1,
           adj_role >1 & adj_role < 5 ~adj_role))

# team average 
player <- player %>% 
  group_by(team,year) %>% 
  mutate(role_avg_t_2 = sum(trim_role_2 * min_p) / min_t) %>% 
  ungroup()

# 3. round:----
# pos_avg_t must be equal to 3
player <- player %>% 
  mutate(adj_role_2  = off_role_min_adj - (role_avg_t-3) - (role_avg_t_2-3),
         trim_role_3 =  case_when(
           adj_role_2 >5 ~ 5,
           adj_role_2 <1 ~ 1,
           adj_role_2 >1 & adj_role_2 < 5 ~adj_role_2))

# team average 
player <- player %>% 
  group_by(team,year) %>% 
  mutate(role_avg_t_3 = sum(trim_role_3 * min_p) / min_t) %>% 
  ungroup()

# 4. round:----
# pos_avg_t must be equal to 3
player <- player %>% 
  mutate(adj_role_3  = off_role_min_adj - (role_avg_t-3) - (role_avg_t_2-3) - (role_avg_t_3-3),
         trim_role_4 =  case_when(
           adj_role_3 >5 ~ 5,
           adj_role_3 <1 ~ 1,
           adj_role_3 >1 & adj_role_3 < 5 ~adj_role_3))

# team average 
player <- player %>% 
  group_by(team,year) %>% 
  mutate(role_avg_t_4 = sum(trim_role_4 * min_p) / min_t) %>% 
  ungroup()

b <- select(player, team, role_avg_t, role_avg_t_2, role_avg_t_3, role_avg_t_4)
# ok fine. now the mean position of all teams is equal to 3

# offensive role final ----
player <- player %>% 
  mutate(role_final = trim_role_4)

#******************************************************************************#
# calculate BPM coefficients ----
# load base regression:
bpm_coef <- read.xlsx("data/BPM_coef.xlsx", rowNames =  TRUE)
var.names<-tolower(colnames(bpm_coef))
colnames(bpm_coef)<-var.names
bpm_coef

player <- player %>% 
  mutate(ORtg_t = (pts_t / poss_t) * 100,
         DRtg_t = (opp_pts / poss_t) *100)

# calculate position and offensive role based values:
BPM <- select(player,
              player,team,year,min_p,G_p,pts_100:min_pct,pos_final,role_final,ORtg_t,DRtg_t,pace_t,G_t)

BPM <- BPM %>% 
  mutate(BPM_pts_coef=(5-pos_final)/4*bpm_coef$adj_pts[1]+(pos_final-1)/4*bpm_coef$adj_pts[2],
         BPM_fga_coef=(5-role_final)/4*bpm_coef$fga[1]+(role_final-1)/4*bpm_coef$fga[2],
         BPM_fta=(5-role_final)/4*bpm_coef$fta[1]+(role_final-1)/4*bpm_coef$fta[2],
         BPM_p3m=(5-pos_final)/4*bpm_coef$`3pts_bonus`[1]+(pos_final-1)/4*bpm_coef$`3pts_bonus`[2],
         BPM_ast=(5-pos_final)/4*bpm_coef$ast[1]+(pos_final-1)/4*bpm_coef$ast[2],
         BPM_tov=(5-pos_final)/4*bpm_coef$to[1]+(pos_final-1)/4*bpm_coef$to[2],
         BPM_orb=(5-pos_final)/4*bpm_coef$orb[1]+(pos_final-1)/4*bpm_coef$orb[2],
         BPM_drb =(5-pos_final)/4*bpm_coef$drb[1]+(pos_final-1)/4*bpm_coef$drb[2],
         BPM_trb=(5-pos_final)/4*bpm_coef$trb[1]+(pos_final-1)/4*bpm_coef$trb[2],
         BPM_stl=(5-pos_final)/4*bpm_coef$stl[1]+(pos_final-1)/4*bpm_coef$stl[2],
         BPM_blk=(5-pos_final)/4*bpm_coef$blk[1]+(pos_final-1)/4*bpm_coef$blk[2],
         BPM_pf=(5-pos_final)/4*bpm_coef$pf[1]+(pos_final-1)/4*bpm_coef$pf[2]
         ) 

# multiply with actual boxs core values:
BPM <- BPM %>% 
  mutate(scoring = pts_100 * BPM_pts_coef + 
           fga_100 * BPM_fga_coef +
           fta_100 * BPM_fta +
           p3m_100 * BPM_p3m,
         ballhandling = 
           ast_100 * BPM_ast +
           tov_100 * BPM_tov,
         rebounding =
           orb_100 * BPM_orb +
           drb_100 * BPM_drb +
           trb_100 * BPM_trb,
         defense = 
           stl_100 * BPM_stl +
           blk_100 * BPM_blk +
           pf_100 * BPM_pf)

#******************************************************************************#
# add positional and role constant:----
# mainly for defense
# load position regression
BPM_pos_coef <- read.xlsx("data/BPM_pos_coef.xlsx", rowNames = FALSE)
BPM_pos_coef

BPM <- BPM %>% 
  mutate(pos_cons = case_when(
    pos_final >= 3 ~ 0,
    pos_final < 3 ~ (3 - pos_final) * (BPM_pos_coef$Pos_1/2)),
    role_cons = (role_final - 3) * BPM_pos_coef$off_role_slope,
    constant = pos_cons + role_cons)

#******************************************************************************#
# Calc. raw BPM:----
BPM <- BPM %>% 
  mutate(raw_BPM = scoring + ballhandling + rebounding + defense + constant,
         contribution = raw_BPM * min_pct)

#******************************************************************************#
# team contribution:----
BPM <- BPM %>% 
  group_by(team,year) %>% 
  mutate(contr_team = sum(contribution)) %>% 
  ungroup() %>% 
  mutate(avg_Rtg = mean(ORtg_t),
         adj_ORtg = ORtg_t - avg_Rtg,
         adj_DRtg = avg_Rtg - DRtg_t,
         adj_netRtg = adj_ORtg + adj_DRtg)

#******************************************************************************#
# average lead and adj_team_Rtg:----
BPM <- BPM %>% 
  mutate(avg_lead = pace_t * adj_netRtg / 100 / 2,
         lead_bonus = 0.35 / 2 * avg_lead,
         adj_team_Rtg = adj_netRtg + lead_bonus)

#******************************************************************************#
# team adjustment:----
BPM <- BPM %>% 
  mutate(team_adj = (adj_team_Rtg - contr_team) / 5) %>% 
  mutate(BPM = raw_BPM + team_adj,
         VORP = (BPM + 2) * min_pct) #* G_t / 31)

#******************************************************************************#
# regression to the mean for players with small minutes:----
BPM <- BPM %>% 
  mutate(reg_min_PG = min_p / (G_p + 4),
         reg_min = pmax((200 - min_p)/3,0),
         exp_BPM = -4.75 + 0.175 * reg_min_PG,
         reg_BPM = (BPM * min_p + exp_BPM * reg_min) / (min_p + reg_min))

# final scores anschauen
a <- select(BPM, player,team, year, BPM, reg_BPM, VORP)

#******************************************************************************#

# OFFENSIVE BPM ----

#******************************************************************************#
# load coefficients
obpm_coef <- read.xlsx("data/OBPM_coef.xlsx", rowNames =  TRUE)
var.names<-tolower(colnames(obpm_coef))
colnames(obpm_coef)<-var.names
obpm_coef

# calc. OBMP coefficients ----
OBPM <- select(player, player,team,min_p,G_p,year,pts_100:min_pct,pos_final,role_final,ORtg_t,DRtg_t,pace_t,G_t) %>% 
  mutate(OBPM_pts_coef=(5-pos_final)/4*obpm_coef$adj_pts[1]+(pos_final-1)/4*obpm_coef$adj_pts[2],
         OBPM_fga_coef=(5-role_final)/4*obpm_coef$fga[1]+(role_final-1)/4*obpm_coef$fga[2],
         OBPM_fta=(5-role_final)/4*obpm_coef$fta[1]+(role_final-1)/4*obpm_coef$fta[2],
         OBPM_p3m=(5-pos_final)/4*obpm_coef$`3pts_bonus`[1]+(pos_final-1)/4*obpm_coef$`3pts_bonus`[2],
         OBPM_ast=(5-pos_final)/4*obpm_coef$ast[1]+(pos_final-1)/4*obpm_coef$ast[2],
         OBPM_tov=(5-pos_final)/4*obpm_coef$to[1]+(pos_final-1)/4*obpm_coef$to[2],
         OBPM_orb=(5-pos_final)/4*obpm_coef$orb[1]+(pos_final-1)/4*obpm_coef$orb[2],
         OBPM_drb=(5-pos_final)/4*obpm_coef$drb[1]+(pos_final-1)/4*obpm_coef$drb[2],
         OBPM_trb=(5-pos_final)/4*obpm_coef$trb[1]+(pos_final-1)/4*obpm_coef$trb[2],
         OBPM_stl=(5-pos_final)/4*obpm_coef$stl[1]+(pos_final-1)/4*obpm_coef$stl[2],
         OBPM_blk=(5-pos_final)/4*obpm_coef$blk[1]+(pos_final-1)/4*obpm_coef$blk[2],
         OBPM_pf =(5-pos_final)/4*obpm_coef$pf[1]+(pos_final-1)/4*obpm_coef$pf[2]
  ) 

# multiply with actual box score values:
OBPM <- OBPM %>% 
  mutate(scoring = pts_100 * OBPM_pts_coef + 
           fga_100 * OBPM_fga_coef +
           fta_100 * OBPM_fta +
           p3m_100 * OBPM_p3m,
         ballhandling = 
           ast_100 * OBPM_ast +
           tov_100 * OBPM_tov,
         rebounding =
           orb_100 * OBPM_orb +
           drb_100 * OBPM_drb +
           trb_100 * OBPM_trb,
         defense = 
           stl_100 * OBPM_stl +
           blk_100 * OBPM_blk +
           pf_100 * OBPM_pf)

#******************************************************************************#
# add positional and role constant ----
# mainly for defense
# load position regression
OBPM_pos_coef <- read.xlsx("data/OBPM_pos_coef.xlsx", rowNames = FALSE)
OBPM_pos_coef

OBPM <- OBPM %>% 
  mutate(pos_cons = case_when(
    pos_final >= 3 ~ 0,
    pos_final < 3 ~ (3 - pos_final) * (OBPM_pos_coef$Pos_1/2)),
    role_cons = (role_final - 3) * OBPM_pos_coef$off_role_slope,
    constant = pos_cons + role_cons)

#******************************************************************************#
# calcualte raw OBPM ----
OBPM <- OBPM %>% 
  mutate(raw_OBPM = scoring + ballhandling + rebounding + defense + constant,
         contribution = raw_OBPM * min_pct)

#******************************************************************************#
# team contribution ----
OBPM <- OBPM %>% 
  group_by(team,year) %>% 
  mutate(contr_team = sum(contribution)) %>% 
  ungroup() %>% 
  mutate(avg_Rtg = mean(ORtg_t),
         adj_ORtg = ORtg_t - avg_Rtg)

#******************************************************************************#
# average lead and adj_team_Rtg ----
OBPM <- OBPM %>% 
  mutate(avg_lead = pace_t * adj_ORtg / 100 / 2,
         lead_bonus = 0.35 / 2 * avg_lead,
         adj_team_Rtg = adj_ORtg + lead_bonus)

#******************************************************************************#
# team adjustment ----
OBPM <- OBPM %>% 
  mutate(team_adj = (adj_team_Rtg - contr_team) / 5) %>% 
  mutate(OBPM = raw_OBPM + team_adj,
         VORP = (OBPM + 2) * min_pct * G_t / 31)

#******************************************************************************#
# regression to the mean for players with small minutes: ----
OBPM <- OBPM %>% 
  mutate(reg_min_PG = min_p / (G_p + 4),
         reg_min = pmax((200 - min_p)/3,0),
         exp_BPM = -4.75 + 0.175 * reg_min_PG,
         reg_OBPM = (OBPM * min_p + exp_BPM * reg_min) / (min_p + reg_min))

# final scores anschauen
b <- select(OBPM, player,team,year, OBPM,reg_OBPM, VORP)

#******************************************************************************#
#******************************************************************************#
# plotting:----
ggplot(BPM, aes(x=BPM)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") # Overlay with transparent density plot

ggplot(BPM) + 
  geom_histogram(aes(x = BPM, y=..count..),      # Histogram with density instead of count on y-axis
                 binwidth=.5)

# plotting
ggplot(BPM, aes(x=reg_BPM)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") # Overlay with transparent density plot

# plotting
ggplot(OBPM, aes(x=OBPM)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.25,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") # Overlay with transparent density plot

# plotting
ggplot(OBPM, aes(x=reg_OBPM)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.25,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + # Overlay with transparent density plot
  geom_vline(aes(xintercept=mean(OBPM, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

ggplot(BPM, aes(x=BPM, y = VORP, colour = year)) +
  geom_point() +
  geom_vline(aes(xintercept=mean(BPM, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

#******************************************************************************#
# Final scores:----
BPM_for_merge <- select(BPM,min_p,player,team,year,BPM,VORP)
OBPM_for_merge <- select(OBPM, player, team, year, OBPM, VORP)

BPM_final <- merge(BPM_for_merge, OBPM_for_merge, by = c("player","team","year"),
                   suffixes = c("","_off.")) %>% 
  arrange(-BPM)

#******************************************************************************#
# save final win shares in data frame: ----
saveRDS(object = BPM_final, file = paste0("Data/estimates/BPM_final.Rds"))
