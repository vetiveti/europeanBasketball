# Clear Console
cat("\014")

# remove Environment
rm(list=ls())


library(tidyverse)
library(Matrix)
library(glmnet)
library(knitr)

possesion_data <- read_csv("https://raw.githubusercontent.com/rd11490/NBA_Tutorials/master/rapm/data/rapm_possessions.csv") %>% 
    filter(possessions != 0) %>% 
    select(-X1)

player_data <- read_csv("https://raw.githubusercontent.com/rd11490/NBA_Tutorials/master/rapm/data/player_names.csv")

head(possesion_data)

get_players <- function(possesions) {
    
    possesions <- distinct(possesions)
    players <- unique(c(unique(possesions$offensePlayer1Id),
                        unique(possesions$offensePlayer2Id),
                        unique(possesions$offensePlayer3Id),
                        unique(possesions$offensePlayer4Id),
                        unique(possesions$offensePlayer5Id),
                        unique(possesions$defensePlayer1Id),
                        unique(possesions$defensePlayer2Id),
                        unique(possesions$defensePlayer3Id),
                        unique(possesions$defensePlayer4Id),
                        unique(possesions$defensePlayer5Id)))
    return(players)
    
}

players <- sort(get_players(possesions = possesion_data))

print(paste0("There are ", length(players), " unique players in the dataset."))

possesion_data <- possesion_data %>% 
    mutate(ppp100 = 100 * points/possessions)


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

player_matrix <- t(apply(possesion_data[, 1:10], 1, 
                         function(x) make_matrix_rows(lineup = x, players_in = players)))

player_matrix <- as(player_matrix, "dgCMatrix")

target <- possesion_data$ppp100

print(dim(player_matrix))

## this is a cross validated model to pick the lambda
cv_model <- glmnet::cv.glmnet(x = player_matrix, ##ncol = 1058
                              y = target,
                              alpha = 0, 
                              standardize = FALSE)

lam <- cv_model$lambda.min ## best lambda

## this is the model refit using that lambda
coef_model <- glmnet::glmnet(x = player_matrix, 
                             y = target,
                             alpha = 0, 
                             standardize = FALSE,
                             lambda = lam)
player_coefficients <- coef_model$beta ## length = 1058

o_rapm <- player_coefficients[1:length(players)]

d_rapm <- player_coefficients[length(players) + 1:length(players) * 2]

o_rapm_frame <- data.frame("player_id" = players,
                           "o_rapm" = o_rapm)

d_rapm_frame <- data.frame("player_id" = players,
                           "d_rapm" = d_rapm)

rapm <- left_join(o_rapm_frame, d_rapm_frame, by = "player_id") %>% 
    left_join(player_data, by = c("player_id" = "playerId")) %>% 
    mutate(rapm = o_rapm + d_rapm) %>% 
    select(Player = playerName,
           RAPM = rapm,
           `O-RAPM` = o_rapm,
           `D-RAPM` = d_rapm) %>% 
    arrange(-RAPM)

kable(rapm[1:5, ] %>% 
          mutate(across(where(is.numeric), function(x) round(x, 1))), align = "c") 
