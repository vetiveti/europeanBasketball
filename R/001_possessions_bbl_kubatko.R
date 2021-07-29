# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

library(tidyverse)
require(data.table)
#******************************************************************************#
# load data----
# cleaned pbp files
files <- list.files(path = './data/clean_pbp', pattern ='pbp')
pbp_list = lapply(paste0("Data/clean_pbp/",files), function (x) data.table(readRDS(x)))
pbp = rbindlist(pbp_list, fill = TRUE, idcol="ID") %>% 
    mutate(year = ID +2013) %>% 
    filter(year > 2013) %>% 
    select(teamcode,sn_Spieler_1,Player_1,Club_1, sn_Spieler_2,Player_2,Club_2,
           aktion,zusatzinfo_1, zusatzinfo_2,zusatzinfo_3, resultat,
           nummer_aktion,number_action,spielzeit_sec,timestamp,
           quarter,year,game_id)
length(unique(pbp$game_id))

#******************************************************************************#
# Error correction:----
# errors in pbp... bestimmt nicht alles...
pbp$nummer_aktion[pbp$game_id == 19579 & pbp$quarter == 1 & pbp$nummer_aktion == 99] <- 30.1
pbp$nummer_aktion[pbp$game_id == 19579 & pbp$quarter == 1 & pbp$nummer_aktion == 100] <- 30.2

pbp$quarter[pbp$game_id == 19579 & pbp$nummer_aktion == 30.1] <- 2
pbp$quarter[pbp$game_id == 19579 & pbp$nummer_aktion == 30.2] <- 2

pbp$zusatzinfo_1[pbp$game_id == 17141 & pbp$nummer_aktion == 109 & pbp$quarter == 3] <- "O"
pbp$zusatzinfo_2[pbp$game_id == 17144 & pbp$nummer_aktion == 39 & pbp$quarter == 1] <- "0"
pbp$zusatzinfo_1[pbp$game_id == 17144 & pbp$nummer_aktion == 38 & pbp$quarter == 1] <- "O"
pbp$aktion[pbp$aktion == "A"] <- "REB"
pbp$aktion[pbp$aktion == "OFFREB"] <- "REB"
pbp$zusatzinfo_1[pbp$aktion == "OFFREB"] <- "O"
pbp$aktion[pbp$aktion == "TFOUL"] <- "FOUL"
pbp$zusatzinfo_1[pbp$aktion == "TFOUL"] <- "T"

pbp <- pbp %>% 
    filter(.,
           aktion != "FD") %>% 
    arrange(.,year,game_id,quarter,nummer_aktion)

#******************************************************************************#
# possession calculation:----
pbp <- pbp %>% 
    mutate_all(na_if,"") %>% 
    mutate(einer = 0)

pbp <- pbp %>% 
    mutate(
        one_FT = case_when(
            aktion == "FOUL" & zusatzinfo_2 == "1" ~ 1,
            TRUE ~ 0),
        three_FT = case_when(
            aktion == "FOUL" & zusatzinfo_2 == "3" ~ 1,
            TRUE ~ 0),
        pos_end = case_when(
            aktion == "P2" & resultat == "+" ~ 1,
            aktion == "P3" & resultat == "+" ~ 1,
            aktion == "REB"& zusatzinfo_1 == "D"~ 1,
            aktion == "TREB" & zusatzinfo_1 == "D" ~ 1,
            aktion == "TO" ~ 1,
            aktion == "TTO"~ 1,
            aktion == "FT" & zusatzinfo_1 == "2" & resultat == "+" ~ 1,
            aktion == "FT" & resultat == "+" & one_FT == 1 ~ 1,
            aktion == "FOUL" & zusatzinfo_1 == "O" ~ 1,
            TRUE ~ 0)
    )


pbp$pos_end[pbp$aktion == "END"] <- 1
pbp$zusatzinfo_2 <- replace_na(pbp$zusatzinfo_2,"unknown")
pbp$zusatzinfo_1 <- replace_na(pbp$zusatzinfo_1,"unknown")
b <- which(pbp$aktion == "FOUL" | pbp$aktion == "FT")
j = 0
pbp$einer <- 0
pbp$drei <- 0
for (i in b) {
    j = j + 1
    
    if(pbp$aktion[i] == "FOUL" & pbp$zusatzinfo_1[i] == "T" & pbp$teamcode[i] != pbp$teamcode[b[j+1]]){
        pbp$pos_end[i] = 0
        pbp$einer[b[j+1]] = 0
    }
    if(pbp$aktion[i] == "FOUL" & pbp$zusatzinfo_2[i] == "1" & pbp$zusatzinfo_1[i] != "T"){
        pbp$einer[b[j+1]] = 1
        
        pbp$pos_end[i-1] = 0
    }
    if(pbp$aktion[i] == "FOUL" & pbp$zusatzinfo_2[i] == "3"){
        pbp$drei[b[j+3]] = 1
        
        pbp$pos_end[b[j+2]] = 0
    }
}

pbp <- pbp %>% 
    mutate(beendet = case_when(resultat == "+" & einer == 1 ~1,
                               resultat == "+" & drei == 1 ~1,
                               TRUE ~ 0),
           poss = ifelse(pos_end == 1 | beendet == 1,1,0))

# fehlerbehebung:----
fehlersuche <- pbp %>%
    filter(., poss == 0, aktion =="TTO") %>% 
    select(game_id,Club_1,aktion,resultat,zusatzinfo_1, nummer_aktion,quarter,poss)

pbp$poss[pbp$aktion == "REB" & pbp$zusatzinfo_1 == "D"] <- 1
pbp$poss[pbp$aktion == "TO"] <- 1

#******************************************************************************#
# calculate possessions per game:----
pos_per_game <- pbp %>% 
    group_by(game_id) %>% 
    mutate(tot_poss = sum(poss)) %>% 
    distinct(.,game_id, .keep_all = TRUE) %>% 
    select(game_id,tot_poss)

teams <- pbp %>% 
    group_by(game_id) %>% 
    summarise(team = unique(Club_1)) %>% 
    drop_na() %>% 
    mutate(ind = rep(c(1, 2),length.out = n())) %>%
    group_by(ind) %>%
    mutate(id = row_number()) %>%
    spread(ind, team) %>%
    select(-id) %>% 
    rename(team_1 = `1`,
           team_2 =`2`)

pos_pg <- merge(teams,pos_per_game,
                by = "game_id")

#******************************************************************************#
# Save possessions per game:----
saveRDS(object = pos_pg, file = paste0("Data/pos_pg",".Rds"))

#******************************************************************************#
#******************************************************************************#
# START!:----
# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

library(tidyverse)

#******************************************************************************#
# load additional data:----
pos_pg <- readRDS("Data/pos_pg.Rds")
bx_teams <- readRDS("Data/bx_teams_pg.Rds")

bx_teams <- bx_teams %>%
    drop_na(team)

bx_teams <- bx_teams %>% 
    mutate(fga = p2a + p3a,
       fgm = p2m + p3m,
       opp_fga = opp_p2a + opp_p3a,
       opp_fgm = opp_p2m + opp_p3m,
       min = round(min / 60 *5)) %>%
    relocate(team, year, G, W, L, everything()) %>% 
    relocate(fga, fgm, .after = min) %>% 
    relocate(opp_fga, opp_fgm, .after = opp_min) %>% 
    mutate(opp_min = round(opp_min/60 * 5)) %>% 
    drop_na()

merged_df <- merge(pos_pg,bx_teams,
                   by.x = "game_id",
                   by.y = "game_nr") %>% 
    mutate(poss_team = tot_poss/2,
           mfg = fga - fgm,
           mft = fta - ftm)

#******************************************************************************#
# Empircal modelling:----
model1 <- lm(poss_team ~ fga + mfg + fta + mft + orb + opp_drb + tov,data = merged_df)
beta1 <- t(coef(model1)) %>% as_tibble()
summary(model1)

model2 <- lm(poss_team ~ fga + fta + orb + tov,data = merged_df)
beta2 <- t(coef(model2)) %>% as_tibble()
summary(model2)

#******************************************************************************#
# accuracy per team:----
models <- merged_df %>% 
    mutate(poss_actual = poss_team,
           poss_est1 = beta1$`(Intercept)` + beta1$fga*fga+beta1$mfg*mfg+beta1$fta*fta
           +beta1$mft*mft+beta1$orb*orb+beta1$opp_drb*opp_drb+beta1$tov*tov,
           poss_est2 = beta2$`(Intercept)` + beta2$fga*fga + beta2$fta*fta + beta2$orb*orb + beta2$tov*tov,
           poss_est3 = fga + 0.44 * fta - orb + tov,
           poss_est4 = fga + 0.5 * fta - orb + tov,
           orbmissed = orb * (fga - fgm) / (orb + opp_drb),
           poss_est5 = fga + 0.4 * fta - 1.07 * orbmissed + tov) %>% 
    select(game_id,team_1,team_2,poss_actual,poss_est1,poss_est2,poss_est3,poss_est4,poss_est5)

poss_correlation_team <- models %>% 
    mutate(cor_actual = cor(poss_actual,poss_actual),
           cor_model_1 = cor(poss_est1,poss_actual),
           cor_model_2 = cor(poss_est2,poss_actual),
           cor_model_3 = cor(poss_est3,poss_actual),
           cor_model_4 = cor(poss_est4,poss_actual),
           cor_model_5 = cor(poss_est5,poss_actual),
           ) %>% 
    select(cor_actual:cor_model_5) %>% 
    distinct(cor_model_1, .keep_all = TRUE)

#******************************************************************************#
# accuracy total (averaged):----
models_averaged <- merged_df %>% 
    mutate(poss_actual = tot_poss,
           poss_est1 = beta1$`(Intercept)` + beta1$fga*fga+beta1$mfg*mfg+beta1$fta*fta
           +beta1$mft*mft+beta1$orb*orb+beta1$opp_drb*opp_drb+beta1$tov*tov,
           poss_est2 = beta2$`(Intercept)` + beta2$fga*fga + beta2$fta*fta + beta2$orb*orb + beta2$tov*tov,
           poss_est3 = fga + 0.44 * fta - orb + tov,
           poss_est4 = fga + 0.5 * fta - orb + tov,
           orbmissed = orb * (fga - fgm) / (orb + opp_drb),
           poss_est5 = fga + 0.4 * fta - 1.07 * orbmissed + tov) %>%
    group_by(game_id) %>% 
    mutate(poss_est1_avg = sum(poss_est1),
           poss_est2_avg = sum(poss_est2),
           poss_est3_avg = sum(poss_est3),
           poss_est4_avg = sum(poss_est4),
           poss_est5_avg = sum(poss_est5)) %>% 
    ungroup() %>% 
    select(game_id,team_1,team_2,poss_actual,poss_est1_avg,poss_est2_avg,poss_est3_avg,poss_est4_avg,poss_est5_avg) %>% 
    distinct(game_id, .keep_all = TRUE)
    
poss_correlation_total <- models_averaged %>% 
    mutate(cor_actual = cor(poss_actual,poss_actual),
           cor_model_1 = cor(poss_est1_avg,poss_actual),
           cor_model_2 = cor(poss_est2_avg,poss_actual),
           cor_model_3 = cor(poss_est3_avg,poss_actual),
           cor_model_4 = cor(poss_est4_avg,poss_actual),
           cor_model_5 = cor(poss_est5_avg,poss_actual),
    ) %>% 
    select(cor_actual:cor_model_5) %>% 
    distinct(cor_model_1, .keep_all = TRUE)

#******************************************************************************#
poss_correlation_team
poss_correlation_total
