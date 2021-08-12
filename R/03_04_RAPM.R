# Clear Console
cat("\014")

# remove Environment
rm(list=ls())


library(tidyverse)
library(zoo)
library(data.table)
source('functions/BBL_functions.R')
source('functions/pbp_actions.R')
# library(Matrix)
# library(glmnet)
# library(knitr)

#******************************************************************************#
# Load roster files: ----
files <- list.files(path = './data/starters', pattern ='starters')
roster_list = lapply(paste0("data/starters/",files), function (x) data.table(readRDS(x)))
roster = rbindlist(roster_list, fill = TRUE, idcol="year") %>%
    mutate(year = year +2013) %>% 
    filter(., year > 2013)

#******************************************************************************#
# Load cleaned pbp files: ----
files <- list.files(path = './data/clean_pbp', pattern ='pbp')
pbp_list = lapply(paste0("Data/clean_pbp/",files), function (x) data.table(readRDS(x)))
pbp = rbindlist(pbp_list, fill = TRUE, idcol="ID") %>% 
    mutate(year = ID +2013) %>% 
    filter(year > 2013)

#******************************************************************************#
# parse pbp to possession df:----
length(unique(pbp$game_id))

df_new <- tibble()
for (i in unique(pbp$game_id)) {
    start_time <- Sys.time()
    pbp_current <- filter(pbp,
                          game_id == i)
    roster_current <- filter(roster,
                             game_nr == i)
    
    df <- RAPM_data_frame(roster_current,pbp_current)
    df_new <- bind_rows(df_new,df)
    
    end_time <- Sys.time()
    time <- round(end_time - start_time,3)
    print(paste0("game ", i, " done in time:", time))
    flush.console()
}

df_new$V1 <- trimws(df_new$V1)
df_new$V2 <- trimws(df_new$V2)
df_new$V3 <- trimws(df_new$V3)
df_new$V4 <- trimws(df_new$V4)
df_new$V5 <- trimws(df_new$V5)
df_new$V6 <- trimws(df_new$V6)
df_new$V7 <- trimws(df_new$V7)
df_new$V8 <- trimws(df_new$V8)
df_new$V9 <- trimws(df_new$V9)
df_new$V10 <- trimws(df_new$V10)

df_new[df_new == "Jake, O#Brien"] <- "Jake, O'Brien"
df_new[df_new == "Jake, O`Brien"] <- "Jake, O'Brien"
df_new[df_new == "Jake, OBrien"] <- "Jake, O'Brien"
df_new[df_new == "Chad, Topper"] <- "Chad, Toppert"
df_new[df_new == "Nihad, Dedovic"] <- "Nihad, Djedovic"
df_new[df_new == "Anthony, Di Leo"] <- "Anthony, DiLeo"
df_new[df_new == "Joshua Patrick, Gasser"] <- "Joshua, Gasser"
df_new[df_new == "Nicolò, Melli"] <- "Nicolo, Melli"
df_new[df_new == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"
df_new[df_new == "Miles, Jackson-C"] <- "Miles, Jackson-Cartwright"
df_new[df_new == "Darvin, Davis"] <- "Darwin, Davis"
df_new[df_new == "E. J., Singler"] <- "E.J., Singler"
df_new[df_new == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"
df_new[df_new == "Ra#Shad, James"] <- "Ra'Shad, James"
df_new[df_new == "Konstantin, Klein"] <- "Konstantin, Konga"
df_new[df_new == "Leon Iduma, Okpara"] <- "Leon, Okpara"
df_new[df_new == "Quirin, Emanga Noupoue"] <- "Quirin, Emanga"
df_new[df_new == "Zan Mark, Sisko"] <- "Zan, Sisko"

df <- df_new %>% 
    mutate_all(na_if,"") %>% 
    mutate(einer = 0)
df$spielstand_A[df$quarter == 1 & df$aktion == "START" ] = 0
df$spielstand_B[df$quarter == 1 & df$aktion == "START" ] = 0

#******************************************************************************#
# Save untouched pbp file:----
saveRDS(object = df, file = paste0("Data/pbp_untouched",".Rds"))

#******************************************************************************#
# errors in pbp... bestimmt nicht alles...
df$nummer_aktion[df$game_id == 19579 & df$quarter == 1 & df$nummer_aktion == 99] <- 30.1
df$nummer_aktion[df$game_id == 19579 & df$quarter == 1 & df$nummer_aktion == 100] <- 30.2

df$quarter[df$game_id == 19579 & df$nummer_aktion == 30.1] <- 2
df$quarter[df$game_id == 19579 & df$nummer_aktion == 30.2] <- 2

df$zusatzinfo_1[df$game_id == 17141 & df$nummer_aktion == 109 & df$quarter == 3] <- "O"
df$zusatzinfo_2[df$game_id == 17144 & df$nummer_aktion == 39 & df$quarter == 1] <- "0"
df$zusatzinfo_1[df$game_id == 17144 & df$nummer_aktion == 38 & df$quarter == 1] <- "O"
df$aktion[df$aktion == "A"] <- "REB"
df$aktion[df$aktion == "OFFREB"] <- "REB"
df$zusatzinfo_1[df$aktion == "OFFREB"] <- "O"
df$aktion[df$aktion == "TFOUL"] <- "FOUL"
df$zusatzinfo_1[df$aktion == "TFOUL"] <- "T"


df <- df %>% 
    filter(.,
           aktion != "FD") %>% 
    arrange(.,year,game_id,quarter,nummer_aktion)

# weiter gehts:
df <- df %>%
    fill(spielstand_A,spielstand_B) %>% 
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
            aktion == "TO" ~ 1,
            aktion == "FT" & zusatzinfo_1 == "2" & resultat == "+" ~ 1,
            aktion == "FT" & resultat == "+" & one_FT == 1 ~ 1,
            aktion == "FOUL" & zusatzinfo_1 == "O" ~ 1,
            aktion == "TTO"~ 1,
            aktion == "TREB" & zusatzinfo_1 == "D" ~ 1,
            TRUE ~ 0)
        )

df$pos_end[df$aktion == "END"] <- 1
df$zusatzinfo_2 <- replace_na(df$zusatzinfo_2,"unknown")
df$zusatzinfo_1 <- replace_na(df$zusatzinfo_1,"unknown")
b <- which(df$aktion == "FOUL" | df$aktion == "FT")
j = 0
df$einer <- 0
df$drei <- 0
for (i in b) {
    j = j + 1
    
    if(df$aktion[i] == "FOUL" & df$zusatzinfo_1[i] == "T" & df$teamcode[i] != df$teamcode[b[j+1]]){
        df$pos_end[i] = 0
        df$einer[b[j+1]] = 0
    }
    if(df$aktion[i] == "FOUL" & df$zusatzinfo_2[i] == "1" & df$zusatzinfo_1[i] != "T"){
        df$einer[b[j+1]] = 1

        df$pos_end[i-1] = 0
    }
    if(df$aktion[i] == "FOUL" & df$zusatzinfo_2[i] == "3"){
        df$drei[b[j+3]] = 1
        
        df$pos_end[b[j+2]] = 0
    }
}

df <- df %>% 
    mutate(beendet = case_when(resultat == "+" & einer == 1 ~1,
                               resultat == "+" & drei == 1 ~1,
                               TRUE ~ 0),
           poss = ifelse(pos_end == 1 | beendet == 1,1,0))

df_points <- filter(df,
            poss == 1) %>%
    type_convert() %>% 
    group_by(game_id) %>% 
    mutate(diff_A = spielstand_A - lag(spielstand_A, default = 0)) %>% 
    relocate(diff_A, .after = spielstand_A) %>% 
    mutate(diff_B = spielstand_B - lag(spielstand_B, default = 0)) %>% 
    relocate(diff_B, .after = spielstand_B)

unique(df_points$aktion)

(sum(df$pos_end)) / 2              
(sum(df$poss)) / 2 

df_points <- df_points %>% 
    mutate(offense = case_when(
            aktion == "FT" ~ 1,
            aktion == "P2" ~ 1,
            aktion == "P3" ~ 1,
            aktion == "TO" ~ 1,
            aktion == "TTO" ~ 1,
            aktion == "FOUL" & zusatzinfo_1 == "O" ~ 1,
            TRUE ~ 0),
        
        defense = case_when(
            aktion == "REB" ~ 1,
            aktion == "TREB" ~ 1,
            TRUE ~ 0)) %>% 
    filter(.,aktion != "END")


df_points <- df_points %>% 
    mutate(offense_A = case_when(
                                 teamcode == "A" & offense == "1" ~ 1,
                                 teamcode == "B" & defense == "1" ~ 1,
                                TRUE ~ 0),
           offense_B = case_when(teamcode == "B" & offense == "1" ~ 1,
                                 teamcode == "A" & defense == "1" ~ 1,
                                 TRUE ~ 0))

# check if correct
b <- dplyr::select(df_points, teamcode, aktion, offense, defense, offense_A, offense_B,spielzeit_sec,quarter)
sum(b$offense_A) + sum(b$offense_B) - nrow(b)

# what possessions are wrong?
aber <- filter(df_points,
               diff_B > 0 & diff_A > 0)

loose <- dplyr::select(aber, nummer_aktion,quarter,game_id)

df_points1 <- anti_join(df_points, loose, by =c("nummer_aktion","quarter","game_id"))

possesions <- df_points1 %>% 
    mutate(off_player1 = ifelse(offense_A == 1, V1, V6),
           off_player2 = ifelse(offense_A == 1, V2, V7),
           off_player3 = ifelse(offense_A == 1, V3, V8),
           off_player4 = ifelse(offense_A == 1, V4, V9),
           off_player5 = ifelse(offense_A == 1, V5, V10),
           def_player1 = ifelse(offense_B == 1, V1, V6),
           def_player2 = ifelse(offense_B == 1, V2, V7),
           def_player3 = ifelse(offense_B == 1, V3, V8),
           def_player4 = ifelse(offense_B == 1, V4, V9),
           def_player5 = ifelse(offense_B == 1, V5, V10),
           
           points = diff_A + diff_B,
           year = year,
           possesion = 1,
           .keep = "none")

#******************************************************************************#
# Save possession file:----
saveRDS(object = possesions, file = paste0("Data/possessions",".Rds"))

#******************************************************************************#
# Clear Console
cat("\014")

# remove Environment
rm(list=ls())


library(tidyverse)
library(Matrix)
library(glmnet)
library(knitr)

possesion_data <- readRDS("data/possessions.Rds") %>% 
    filter(year > 2013)

head(possesion_data)

get_players <- function(possesions) {
    
    possesions <- distinct(possesions)
    players <- unique(c(unique(possesions$off_player1),
                        unique(possesions$off_player2),
                        unique(possesions$off_player3),
                        unique(possesions$off_player4),
                        unique(possesions$off_player5),
                        unique(possesions$def_player1),
                        unique(possesions$def_player2),
                        unique(possesions$def_player3),
                        unique(possesions$def_player4),
                        unique(possesions$def_player5)))
    return(players)
    
}

players <- sort(get_players(possesions = possesion_data))

print(paste0("There are ", length(players), " unique players in the dataset."))

possesion_data <- possesion_data %>% 
    mutate(ppp100 = 100 * points/possesion)


make_matrix_rows <- function(lineup, players_in) {
    
    player1 <- lineup[1]
    player2 <- lineup[2]
    player3 <- lineup[3]
    player4 <- lineup[4]
    player5 <- lineup[5]
    player6 <- lineup[6]
    player7 <- lineup[7]
    player8 <- lineup[8]
    player9 <- lineup[9]
    player10 <- lineup[10]
    
    zeroRow <- rep(0, length(players_in) * 2)
    
    # OFFENSE #
    zeroRow[which(players_in == player1)] <- 1
    zeroRow[which(players_in == player2)] <- 1
    zeroRow[which(players_in == player3)] <- 1
    zeroRow[which(players_in == player4)] <- 1
    zeroRow[which(players_in == player5)] <- 1
    
    # DEFENSE #
    zeroRow[which(players_in == player6) + length(players_in)] <- -1
    zeroRow[which(players_in == player7) + length(players_in)] <- -1
    zeroRow[which(players_in == player8) + length(players_in)] <- -1
    zeroRow[which(players_in == player9) + length(players_in)] <- -1
    zeroRow[which(players_in == player10) + length(players_in)] <- -1
    
    return(zeroRow)
    
}

possesion_data <- possesion_data %>% 
    relocate(game_id, .after = def_player5)

player_matrix <- t(apply(possesion_data[, 1:10], 1, 
                         function(x) make_matrix_rows(lineup = x, players_in = players)))

#******************************************************************************#
# Possession for each player:----
a <- player_matrix %>% as_tibble()
colnames(a)
b<- colSums(a) %>% as_tibble()
c <- tibble(players)
d <- bind_rows(c,c)

poss_p <- data.frame("player_id" = d,
                           "possessions" = b) %>% 
    rename(possessions = value,
           Player = players)
poss_p <- poss_p  %>% 
    mutate(off_poss = ifelse(possessions >= 0,possessions,0),
           def_poss = ifelse(possessions <= 0,-possessions,0)) %>% 
    mutate(poss_total = abs(possessions))
poss_player_combined <- poss_p %>%
    group_by(Player) %>% 
    mutate(poss_total_player =sum(poss_total)) %>% 
    distinct(.,Player, .keep_all = TRUE) %>% 
    select(Player, poss_total_player)

#******************************************************************************#
# save player small player matrix & possession numbers

player_matrix <- as(player_matrix, "dgCMatrix")

saveRDS(object = player_matrix, file = paste0("Data/player_matrix_2018.Rds"))
saveRDS(object = poss_player_combined, file = paste0("Data/poss_player_combined_2018.Rds"))

#******************************************************************************#
library(tidyverse)
library(Matrix)
library(glmnet)
library(knitr)

#******************************************************************************#
# load data
player_matrix <- readRDS("Data/player_matrix_2018.Rds")
poss_player_combined <- readRDS("Data/poss_player_combined_2018.Rds")
possesion_data <- readRDS("data/possessions.Rds") %>% 
    filter(year > 2013)

possesion_data <- possesion_data %>% 
    mutate(ppp100 = 100 * points/possesion)

#******************************************************************************#
target <- possesion_data$ppp100
weight <- rep(1, nrow(possesion_data)) #(possesion_data$year - 2013)
print(dim(player_matrix))


## this is a cross validated model to pick the lambda
cv_model <- glmnet::cv.glmnet(x = player_matrix,
                              y = target,
                              alpha = 0, 
                              weights = weight,
                              standardize = FALSE)

lam <- cv_model$lambda.min ## best lambda

## this is the model refit using that lambda
coef_model <- glmnet::glmnet(x = player_matrix,
                             y = target,
                             alpha = 0, 
                             standardize = FALSE,
                             weights = weight,
                             lambda = lam)
player_coefficients <- coef_model$beta 


o_rapm <- player_coefficients[1:length(players)]

d_rapm <- player_coefficients[length(players) + 1:length(players) * 2]

o_rapm_frame <- data.frame("player_id" = players,
                           "o_rapm" = o_rapm)

d_rapm_frame <- data.frame("player_id" = players,
                           "d_rapm" = d_rapm)

rapm <- left_join(o_rapm_frame, d_rapm_frame, by = "player_id") %>% 
    mutate(rapm = o_rapm + d_rapm) %>% 
    dplyr::select(Player = player_id,
           RAPM = rapm,
           `O-RAPM` = o_rapm,
           `D-RAPM` = d_rapm) %>% 
    arrange(-RAPM)    

rapm_pos <- merge(rapm, poss_player_combined, by ="Player") %>% 
    arrange(-RAPM)

kable(rapm_pos[1:20, ] %>% 
          mutate(across(where(is.numeric), function(x) round(x, 1))), align = "c") 

ggplot(data = rapm, aes(x = d_rapm, y = o_rapm))+
    geom_point()

ggplot(data = rapm, aes(x=RAPM))+
    geom_histogram()

#******************************************************************************#
# Save RAPM file:----
saveRDS(object = rapm_pos, file = paste0("Data/estimates/rapm",".Rds"))

#******************************************************************************#

#******************************************************************************#
library(doParallel)
target <- possesion_data$ppp100
player_matrix <- player_matrix

require(doParallel)
registerDoParallel(15)

lambdas = NULL
for (i in 1:8){
    set.seed( as.integer((as.double(Sys.time())*i+Sys.getpid()) %% 2^31) )
    fit <- cv.glmnet(x = player_matrix,y = target,alpha=0,weights= NULL,nfolds=10,parallel=TRUE,standardize=FALSE,lambda=seq(1.2, 1.5, by=0.005))
    errors = data.frame(fit$lambda,fit$cvm)
    lambdas <- rbind(lambdas,errors)
}

lambdas <- aggregate(lambdas[, 2], list(lambdas$fit.lambda), mean)
bestindex = which(lambdas[2]==min(lambdas[2]))
bestlambda = lambdas[bestindex,1]
fit <- glmnet(x = player_matrix,y = target,alpha=0,weights=NULL,standardize=FALSE,lambda=bestlambda)
coef(fit)

player_coefficients <- fit$beta 

o_rapm <- player_coefficients[1:length(players)]

d_rapm <- player_coefficients[length(players) + 1:length(players) * 2]

o_rapm_frame <- data.frame("player_id" = players,
                           "o_rapm" = o_rapm)

d_rapm_frame <- data.frame("player_id" = players,
                           "d_rapm" = d_rapm)

rapm1 <- left_join(o_rapm_frame, d_rapm_frame, by = "player_id") %>% 
    mutate(rapm = o_rapm + d_rapm) %>% 
    dplyr::select(Player = player_id,
                  RAPM = rapm,
                  `O-RAPM` = o_rapm,
                  `D-RAPM` = d_rapm) %>% 
    arrange(-RAPM)    

kable(rapm1[1:20, ] %>% 
          mutate(across(where(is.numeric), function(x) round(x, 1))), align = "c") 

rapm1_pos <- merge(rapm1, poss_player_combined, by = "Player") %>% 
    arrange(-RAPM)

kable(rapm1_pos[1:20, ] %>% 
          mutate(across(where(is.numeric), function(x) round(x, 1))), align = "c") 

#******************************************************************************#
# https://squared2020.com/2017/09/18/deep-dive-on-regularized-adjusted-plus-minus-ii-basic-application-to-2017-nba-data-with-r/
# approach:
# denke ist nicht so wichtig...
# dauert sehr lange zu rechnen da für alle lambdas geschaut wird und der Datensatz darf nicht zu groß sein!

library(broom)
library(MASS)

possessions <- possesion_data %>% 
    ungroup() %>% 
    filter(., year == 2018) %>% 
    dplyr::select(off_player1:def_player5,ppp100) %>% 
    rename(Margin = ppp100)

rapm2 <- lm.ridge(formula = Margin ~ . , data = possessions, lambda = seq(0,20000,200))
r2 <- tidy(rapm2)
g <- glance(rapm2)
ggplot(r2, aes(lambda, GCV)) +
    geom_line() +
    geom_vline(xintercept = g$lambdaGCV, col = "red", lty = 2)
