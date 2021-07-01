# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# source functions to use
source('functions/berri_functions.R')

# load needed packages
library(tidyverse)
library(data.table)
#******************************************************************************#
# Load data:----
team_totals <- readRDS("Data/team_data_totals.Rds")
team_pg <- readRDS("Data/team_data_pg.Rds")

# load player data
player_totals <- readRDS("Data/player_data_totals.Rds")
player_pg <- readRDS("Data/player_data_pg.Rds")

# load coach data
load(file = "data/coach_data.rda", .GlobalEnv)

#******************************************************************************#
# Data preparation:----
# preparing and setting up team data
player_pg <- player_pg %>%
  filter(., year >=1988 & year <2019)

team_pg <- team_pg %>% 
  filter(., year >=1988 & year <2019)

team_totals <- team_totals %>% 
  filter(., year >=1988 & year <2019)

# hab ich noch nicht----
# coach_data <- coach_data %>%
#   filter(., year >=1988 & year <2012)

#******************************************************************************#
# Defensive rebounds:----
# was wird gebraucht:
# dreb                              # check
# lag(dreb)                         # check
# age                               # check
# age^2                             # check
# % played last 2 seasons           # check
# new coach                         # check
# new team                          # check
# roster stability                  # check
# teammate's per minute production  # check

#******************************************************************************#
stat_prod <- player_totals %>% 
  mutate_if(is.numeric, as.numeric) %>%
  rename(age = Alter,
         pf_percent = pf_p,
         gp = G) %>% 
  mutate(age_2 = (age)^2)

# Step 1: compute %pct of games played in the last 2 seasons
df <- team_totals %>% 
  select(year,team,G,W,L)

stats1 <- merge(stat_prod,df,
                by = c("year","team"),
                all =TRUE) %>% 
  mutate(g_pct_current = gp/G) %>% 
  arrange(player,year)

i <-1
a <- NULL
a[1] <- 1

try(for (x in stats1$year) {
  i <- i + 1
  if (stats1$player[i] == stats1$player[i-1]) {
    if (stats1$year[i]-stats1$year[i-1] == 1){
      a[i] <- (stats1$g_pct[i] + stats1$g_pct[i-1])/2
    } else {
      a[i]<- stats1$g_pct[i]
    }
  } else {
    a[i]<- stats1$g_pct[i]
  }
}, silent =TRUE)

stats1_1 <- bind_cols(stats1,a) %>% 
  rename(pct_last2="NA")

#Step 2: Calculating pct_min played this and last season
stats2_1 <- merge(stats1_1,team_totals,
                  by.x = c("year","team"),
                  by.y = c("year","team"),
                  all= T, suffix = c("_p","_t") ) %>% 
  arrange(player,year)

stats2_2 <- stats2_1 %>% 
  mutate(pct_min_current = min_p/(min)) %>% 
  group_by(team,year) %>%
  mutate(eins = sum(pct_min_current)) %>%
  ungroup()

i <-1
a <- NULL
a[1] <- 0.3377777778

try(for (x in stats2_2$year) {
  i <- i + 1
  if (stats2_2$player[i] == stats2_2$player[i-1]) {
    if (stats2_2$year[i]-stats2_2$year[i-1] == 1){
      a[i] <- (stats2_2$pct_min_current[i] + stats2_2$pct_min_current[i-1])/2
    } else {
      a[i]<- stats2_2$pct_min_current[i]
    }
  } else {
    a[i]<- stats2_2$pct_min_current[i]
  }
},silent = TRUE)

stats2 <- bind_cols(stats2_2,a) %>% 
  rename(pct_min_last2="NA")
 
#******************************************************************************#
# Step 3: calculating if a player changed his team
# compute roster stability and if a player changed team or did not sign a contract continuously with the same team
stats3 <- roster_stability(stats2) %>% 
  type.convert(as.is = TRUE)

# Step 4: compute TDREBPM = (Team DREB - Player DREB) / (Team minutes played - player minutes played)
stats4 <- stats3 %>% 
  mutate(PDREPM = drb_p/min_p) %>% 
  mutate(TDREBPM = (drb_t-drb_p)/ (min_t-min_p)) %>% 
# Step 4_1: compute TOREBPM = (Team OREB - Player OREB) / (Team minutes played - player minutes played)
  mutate(POREPM = orb_p/min_p) %>% 
  mutate(TOREBPM = (orb_t-orb_p)/ (min_t-min_p)) %>% 
# Step 4_2: compute TASTPM = (Team AST - Player AST) / (Team minutes played - player minutes played)
  mutate(PASTPM =ast_p/min_p) %>%
  mutate(TASTPM = (ast_t-ast_p)/ (min_t-min_p)) %>% 
# Step 4_3: compute TFGAPM = (Team FGA - Player FGA) / (Team minutes played - player minutes played)
  mutate(PFGAPM = fga_p/min_p) %>%
  mutate(TFGAPM = (fga_t-fga_p)/ (min_t-min_p)) %>% 
# Step 4_4: compute TEFFFGPCTPM = (Team eff_fg_pct - Player eff_fg_pct) / (Team minutes played - player minutes played)
  mutate(eff_fg_pct_p = (fgm_p + (0.5 * p3m_p)) / fga_p) %>% 
  mutate(eff_fg_pct_t = (fgm_t + (0.5 * p3m_t)) / fga_t) %>% 
  mutate(Teff_fg_pctPM = (eff_fg_pct_t/eff_fg_pct_p)*eff_fg_pct_t) %>% 
# Step 4_5: compute TSTLPM = (Team STL - Player STL) / (Team minutes played - player minutes played)
  mutate(PSTLPM = stl_p/min_p) %>% 
  mutate(TSTLPM = (stl_t-stl_p)/ (min_t-min_p)) %>% 
  # Step 4_6: compute TBLKPM = (Team BLK - Player BLK) / (Team minutes played - player minutes played)
  mutate(PBLKPM = blk_p/min_p) %>% 
  mutate(TBLKPM = (blk_t-blk_p)/ (min_t-min_p)) %>% 
# Step 4_7: compute TPFPM = (Team PF - Player PF) / (Team minutes played - player minutes played)
  mutate(PPFPM = pf_p/min_p) %>% 
  mutate(TPFPM = (pf_t-pf_p)/ (min_t-min_p))

# Step 5: compute lagged dreb per minute
stats5 <- stats4 %>% as.data.table() %>% 
  arrange(player,year,team)
# remove both duplicated rows! ( exclude player seasons where a player changed teams in a season)
myduplicates <- duplicated(stats5, by= c("player","year")) | duplicated(stats5, by= c("player","year"), fromLast = T)
stats5 <- stats5[!myduplicates,] %>%  as_tibble()

stats5 <- stats5 %>% 
  group_by(player) %>% 
  mutate(pdrebpm_lag = dplyr::lag(PDREPM)) %>%
  # Step 5_1: compute lagged oreb per minute
  mutate(porebpm_lag = dplyr::lag(POREPM)) %>%
  # Step 5_2: compute lagged ast per minute
  mutate(pastpm_lag = dplyr::lag(PASTPM)) %>%
  # Step 5_3: compute lagged fga per minute
  mutate(pfgapm_lag = dplyr::lag(PFGAPM)) %>%
  # Step 5_4: compute lagged eff_fg%
  mutate(eff_fg_pct_lag = dplyr::lag(eff_fg_pct_p)) %>%
  # Step 5_5: compute lagged steals
  mutate(pstlpm_lag = dplyr::lag(PSTLPM)) %>% 
  # Step 5_6: compute lagged blocks
  mutate(pblkpm_lag = dplyr::lag(PBLKPM)) %>% 
  # Step 5_7: compute lagged Personal fouls
  mutate(ppfpm_lag = dplyr::lag(PPFPM)) %>% 
  ungroup() %>% 
  na.omit()

# Step 6: compute dummy new coach
coaches <- coach_data %>%
  arrange(year) %>% 
  group_by(team,year) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>% 
  arrange(team,year)

i <- 0
a <- NULL
try(for (x in coaches$nameCoach) {
  i <- i +1
  if (coaches$team[i] == coaches$team[i+1]) {
    if (coaches$nameCoach[i] == coaches$nameCoach[i+1]) {
      a[i+1] = 0
    } else {
      a[i+1] = 1
    }
    
  } else {
    a[i+1]  = 0
  }
},silent = TRUE)
a[1] <- 0

coaches1 <- bind_cols(coaches,a) %>% 
  rename(new_coach=...4)

df <- merge(stats5,coaches1,
            by.x = c("year","team"),
            by.y = c("year","team"),
            all.x = TRUE,
            all.y = FALSE) %>% 
  arrange(player,year,team)

# filter players out how have not played more than 20 games and have at least 12 min/pg
df_filtered <- df %>% 
  filter(gp_p >= 20,
        min_p/gp_p >= 12)
  
#******************************************************************************#
# for dreb
# OLS
regres <- lm(data = df,
             PDREPM ~ pdrebpm_lag + age + age_2 + pct_last2 + factor(pos)
             + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + TDREBPM)
summary(regres)

reg1 <- lm(data = df_filtered,
             PDREPM ~ pdrebpm_lag + age + age_2 + pct_last2 + factor(pos)
             + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + TDREBPM)
summary(reg1)
tdrebpm_reg <- summary(reg1)$coefficients [35,]

# only history:
reg2 <- lm(data = df_filtered,
             PDREPM ~ pdrebpm_lag)
summary(reg2)

#******************************************************************************#
# for fga
# OLS
reg_fga1 <- lm(data = df,
             PFGAPM ~ pfgapm_lag + age + age_2 + pct_last2 + factor(pos)
             + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + TFGAPM)
summary(reg_fga1)

# OLS
reg_fga2 <- lm(data = df_filtered,
               PFGAPM ~ pfgapm_lag + age + age_2 + pct_last2 + factor(pos)
               + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + TFGAPM)
summary(reg_fga2)

tfgapm_reg <- summary(reg_fga2)$coefficients[35,]

#******************************************************************************#
# for oreb
# OLS
reg_oreb1 <- lm(data = df,
               POREPM ~ porebpm_lag + age + age_2 + pct_last2 + factor(pos)
               + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + TOREBPM)
summary(reg_oreb1)

# OLS
reg_oreb2 <- lm(data = df_filtered,
               POREPM ~ porebpm_lag + age + age_2 + pct_last2 + factor(pos)
               + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + TOREBPM)
summary(reg_oreb2)

torebpm_reg <- summary(reg_oreb2)$coefficients[35,]

#******************************************************************************#
# for ast
# OLS
reg_ast1 <- lm(data = df,
               PASTPM ~ pastpm_lag + age + age_2 + pct_last2 + factor(pos)
               + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + TASTPM)
summary(reg_ast1)

# OLS
reg_ast2 <- lm(data = df_filtered,
               PASTPM ~ pastpm_lag + age + age_2 + pct_last2 + factor(pos)
               + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + TASTPM)
summary(reg_ast2)

#******************************************************************************#
# for effective field goal percentage with respect to assists
# OLS
reg_efg_pct1 <- lm(data = df,
               eff_fg_pct_p ~ eff_fg_pct_lag + age + age_2 + pct_last2 + factor(pos)
               + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + eff_fg_pct_t + TASTPM)
summary(reg_efg_pct1)

# OLS
reg_efg_pct2 <- lm(data = df_filtered,
               eff_fg_pct_p ~ eff_fg_pct_lag + age + age_2 + pct_last2 + factor(pos)
               + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + TASTPM)
summary(reg_efg_pct2)

tastpm_reg <- summary(reg_efg_pct2)$coefficients[35,]

#******************************************************************************#
# for steals
# OLS
reg_stl1 <- lm(data = df,
                   PSTLPM ~ pstlpm_lag + age + age_2 + pct_last2 + factor(pos)
                   + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + TSTLPM)
summary(reg_stl1)

# OLS
reg_stl2 <- lm(data = df_filtered,
               PSTLPM ~ pstlpm_lag + age + age_2 + pct_last2 + factor(pos)
                   + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + TSTLPM)
summary(reg_stl2)

tstlpm_reg <- summary(reg_stl2)$coefficients[35,]

#******************************************************************************#
# for blocks
# OLS
reg_stl1 <- lm(data = df,
               PBLKPM ~ pblkpm_lag + age + age_2 + pct_last2 + factor(pos)
               + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + TBLKPM)
summary(reg_stl1)

# OLS
reg_stl2 <- lm(data = df_filtered,
               PBLKPM ~ pblkpm_lag + age + age_2 + pct_last2 + factor(pos)
               + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + TBLKPM)
summary(reg_stl2)

tblkpm_reg <- summary(reg_stl2)$coefficients[35,]

#******************************************************************************#
# for personal fouls
# OLS
reg_pf1 <- lm(data = df,
               PPFPM ~ ppfpm_lag + age + age_2 + pct_last2 + factor(pos)
               + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + TPFPM)
summary(reg_pf1)

# OLS
reg_pf2 <- lm(data = df_filtered,
               PPFPM ~ ppfpm_lag + age + age_2 + pct_last2 + factor(pos)
               + factor(new_coach) + factor(new_team) + factor(year) + rstab_last2 + TPFPM)
summary(reg_pf2)

tpfpm_reg <- summary(reg_pf2)$coefficients[35,]

#******************************************************************************#
# save all important variable estimates
teammates_influence_names <- c("tASTpm", "tBLKpm", "tDREBpm","tFGApm","tOREBpm",
                               "tPFpm","tSTLpm")

teammates_influence <- bind_rows(tastpm_reg,tblkpm_reg,tdrebpm_reg,tfgapm_reg,torebpm_reg,
                                 tpfpm_reg,tstlpm_reg) %>% 
  bind_cols(teammates_influence_names,.) %>% 
  rename(Statistic=...1)

save(teammates_influence, file = "data/teammates_influence.rda")
#******************************************************************************#