# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# source functions to use
source('functions/BBL_functions.R')
source('functions/pbp_actions.R')


library(tidyverse, warn.conflicts = FALSE)
library(zoo)

#******************************************************************************#
# Load roster files:----
roster_files = paste0("Data/rosters_", 2008:2018, ".Rds")
name <- gsub("\\.Rds$", "", roster_files) %>% 
    gsub("Data/", "", .)
roster_data <- lapply(roster_files, readRDS)
names(roster_data) <- gsub("\\.Rds$", "", name)

#******************************************************************************#
# load identifiers: ----
game_id_files = paste0("Data/identifiers_", 2008:2018, ".Rds")
name <- gsub("\\.Rds$", "", game_id_files) %>% 
    gsub("Data/", "", .)
game_id_data <- lapply(game_id_files, readRDS)
names(game_id_data) <- gsub("\\.Rds$", "", name)

# get results safely, because some games have no data
safer_results <- possibly(get_pbp, otherwise = as_tibble("Error finding file"))

#******************************************************************************#
# Load play by play files: ----
pbp_files = paste0("Data/pbp", 2008:2018, ".Rds")
name <- gsub("\\.Rds$", "", pbp_files) %>% 
    gsub("Data/", "", .)
pbp_data <- lapply(pbp_files, readRDS)
names(pbp_data) <- gsub("\\.Rds$", "", name)

# YEAR BY YEAR:  ----
#******************************************************************************#
# Clean pbp data: ----
# prepare game ids
id <- game_id_data$identifiers_2016

# prepare pbp data
pbp <- pbp_data$pbp2016 %>% 
    arrange(desc(spielzeit_sec )) %>% 
    arrange(game_nr)

# prepare roster data
roster <- roster_data$rosters_2016 %>% 
    type_convert()
unique(roster$Club)

# 2017
roster$Club[roster$Club == "s.Oliver Würzurg"] <- "s.Oliver Würzburg"
roster$Club[roster$Club == "Oettinger Rockets"] <- "Rockets"
roster <- roster %>% 
    filter(.,game_nr != 20881 & game_nr != 20885)
unique(roster$Club)

id <- id %>% 
    filter(.,game_id != 20881 & game_id != 20885)

pbp <- pbp %>% 
    filter(.,game_nr != 55 & game_nr != 60)    

#
id_merge <- bind_cols(id, unique(pbp$game_nr)) %>% 
    rename(nr = `...3`)

pbp <- merge(pbp,id_merge,
             by.x = "game_nr",
             by.y = "nr")

# clean for games which have no files
pbp <- pbp %>%
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")

#******************************************************************************#
# calculate starters: ----
#' starter data is far from perfect but I figured out what is wrong
#' Have to do that for every year...
#' 
#' Still wrong are players who switch in timeouts! But I do not know how to
#' solve this issue!!!
#' 
#' Anyway the corrections are done by hand and can be found in the excel file.

# 2016
roster2016 <- calc_starters(pbp,roster) 

#
# pbp_starter <- pbp %>%
#     mutate(game_q = game_id * quarter)
# 
pbp_game_1 <- filter(pbp_starter,
                     game_q == 19619 * 5)

solve <- calc_starters(pbp_game_1,roster)
view(solve)
# debugonce(calc_starters)
# solve2 <- calc_starters(pbp_game_1,roster)
#
starters2016 <- roster2016 %>% 
    mutate(starter_Q5 = replace_na(starter_Q5,0),
           starter_Q6 = replace_na(starter_Q6,0))

starters2016$starter_Q1[starters2016$game_nr == 19579 & starters2016$Player == "Jonas, Grof"] <- 0
starters2016$starter_Q2[starters2016$game_nr == 19599 & starters2016$Player == "Moses, Ehambe"] <- 1
starters2016$starter_Q2[starters2016$game_nr == 19603 & starters2016$Player == "Paul, Carter"] <- 0
starters2016$starter_Q4[starters2016$game_nr == 19625 & starters2016$Player == "Wes, Washpun"] <- 1
starters2016$starter_Q5[starters2016$game_nr == 19749 & starters2016$Player == "Brian, Qvale"] <- 1
starters2016$starter_Q5[starters2016$game_nr == 19783 & starters2016$Player == "Shawn, Huff"] <- 1
starters2016$starter_Q4[starters2016$game_nr == 19798 & starters2016$Player == "Ojars, Silins"] <- 1
starters2016$starter_Q5[starters2016$game_nr == 19817 & starters2016$Player == "Stefan, Ilzhöfer"] <- 1
starters2016$starter_Q5[starters2016$game_nr == 19619 & starters2016$Player == "Nihad, Djedovic"] <- 0


anzahl <- unique(starters2016$game_nr) %>% as_tibble() %>% nrow()
print(anzahl)
sum(starters2016$starter_Q1) / 10
sum(starters2016$starter_Q2) / 10
sum(starters2016$starter_Q3) / 10
sum(starters2016$starter_Q4) / 10
sum(starters2016$starter_Q5) / 10

pbp <- filter(pbp,
                (game_id != 19619 | quarter != 5))

# 2017
roster2017 <- calc_starters(pbp,roster) 

# pbp_starter <- pbp %>%
#     mutate(game_q = game_id * quarter)
# 
# pbp_game_1 <- filter(pbp_starter,
#                      game_q == 21126 * 4)
# 
# solve <- calc_starters(pbp_game_1,roster)
# view(solve)
# debugonce(calc_starters)
# solve2 <- calc_starters(pbp_game_1,roster)

starters2017 <- roster2017 %>% 
    mutate(starter_Q5 = replace_na(starter_Q5,0))
starters2017$starter_Q4[starters2017$game_nr == 20923 & starters2017$Player == "Lucca, Staiger"] <- 1
starters2017$starter_Q3[starters2017$game_nr == 20978 & starters2017$Player == "Kruize, Pinkins"] <- 1
starters2017$starter_Q5[starters2017$game_nr == 21030 & starters2017$Player == "Jamar, Abrams"] <- 1
starters2017$starter_Q3[starters2017$game_nr == 21068 & starters2017$Player == "Thomas, Klepeisz"] <- 0
starters2017$starter_Q3[starters2017$game_nr == 21067 & starters2017$Player == "Andrew, Warren"] <- 1
starters2017$starter_Q4[starters2017$game_nr == 20987 & starters2017$Player == "Aleksej, Nikolic"] <- 1
starters2017$starter_Q4[starters2017$game_nr == 21100 & starters2017$Player == "Anthony, Morse"] <- 1
starters2017$starter_Q3[starters2017$game_nr == 21108 & starters2017$Player == "Kameron, Taylor"] <- 0
starters2017$starter_Q4[starters2017$game_nr == 21108 & starters2017$Player == "Leon, Kratzer"] <- 0
starters2017$starter_Q4[starters2017$game_nr == 21126 & starters2017$Player == "Skyler, Bowlin"] <- 1

anzahl <- unique(starters2017$game_nr) %>% as_tibble() %>% nrow()
print(anzahl)
sum(starters2017$starter_Q1) / 10
sum(starters2017$starter_Q2) / 10
sum(starters2017$starter_Q3) / 10
sum(starters2017$starter_Q4) / 10
sum(starters2017$starter_Q5) / 10

# 2018
roster2017 <- calc_starters(pbp,roster) 

# pbp_game_1 <- filter(pbp_starter,
#                      game_q == 22072 * 4)
# 
# # source functions to use
# source('functions/BBL_functions.R')
# source('functions/pbp_actions.R')
# solve <- calc_starters(pbp_game_1,roster)
# view(solve)
# debugonce(calc_starters)
# solve2 <- calc_starters(pbp_game_1,roster)

starters2018 <- roster2018 %>% 
    mutate(starter_Q5 = replace_na(starter_Q5,0),
           starter_Q6 = replace_na(starter_Q6,0))
starters2018$starter_Q4[starters2018$game_nr == 22072 & starters2018$Player == "Benjamin, Lischka"] <- 0
starters2018$starter_Q4[starters2018$game_nr == 22144 & starters2018$Player == "Jason, Clark"] <- 0
starters2018$starter_Q1[starters2018$game_nr == 22274 & starters2018$Player == "Dru, Joyce"] <- 1
starters2018$starter_Q4[starters2018$game_nr == 22326 & grepl("Brooks",starters2018$Player)] <- 1
starters2018$starter_Q3[starters2018$game_nr == 22325 & starters2018$Player == "Elston, Turner"] <- 1

6740 - sum(duplicated(starters2018$game_nr))
sum(starters2018$starter_Q1) / 10
sum(starters2018$starter_Q2) / 10
sum(starters2018$starter_Q3) / 10
sum(starters2018$starter_Q4) / 10
sum(starters2018$starter_Q5) / 10
sum(starters2018$starter_Q6) / 10


#******************************************************************************#
# boxscores teams pg & totals: ----
un <- unique(roster$game_nr)

id_plus_teams <- tibble(id = un,
                        home_id = 0,
                        team_1 = 0,
                        team_2 = 0)
bx_teams <- tibble()

for (i in seq_along(un)) {
    current_game <- filter(pbp,
                game_id == un[i])
    current_teams <- na.omit(unique(current_game$Club_1))
    
    id_plus_teams$team_1[i] <- current_teams[1]
    id_plus_teams$team_2[i] <- current_teams[2]
    id_plus_teams$home_id[i] <- current_game$home_id[1]
    
    current_boxscore <- get_boxscore_team(current_game,current_teams[1],current_teams[2])
    current_boxscore_against <- get_boxscore_team(current_game,current_teams[2],current_teams[1])
    current_boxscore$game_nr <- un[i]
    current_boxscore_against$game_nr <- un[i]
    bx_teams <- bind_rows(bx_teams,current_boxscore,current_boxscore_against)
}

bx_teams <- bx_teams %>% 
    mutate(W = if_else(pts>opp_pts,1,0),
           L = if_else(pts<opp_pts,1,0),
           G = W + L) %>% 
    rename(team = stats)
# 2016
unique(bx_teams$team)

# 2017
unique(bx_teams$team)
bx_teams$team[bx_teams$team == "Oettinger Rockets"] <- "Rockets"
bx_teams$team[bx_teams$team == "s.Oliver Würzurg"] <- "s.Oliver Würzburg"

# 2018
unique(bx_teams$team)
bx_teams$team[bx_teams$team == "SYNTAINICS MBC"] <- "Mitteldeutscher BC"





team_totals <- bx_teams %>% 
    group_by(team) %>% 
    summarise_at(vars(min:G), .funs = sum)

#******************************************************************************#
# boxscore players pg & totals: ----
players <- roster %>% 
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")


length(unique(pbp$game_id))
length(unique(players$game_nr))

player_tot_perTeam_pg <- tibble()
for(i in unique(pbp$game_id)) {
    players_cur <- filter(players,
                          game_nr == i)
    pbp_cur <- filter(pbp,
                      game_id == i)
    
    player_tot_cur <- tibble()
    for (j in seq_along(players_cur$Player)) {
        player_cur <- players_cur$Player[j]
        bx_player_cur <- get_boxscore_player(pbp_cur,player_cur)
        bx_player_cur$team <- players_cur$Club[j]
        bx_player_cur$game <- players_cur$game_nr[j]
        
        player_tot_cur <- bind_rows(player_tot_cur, bx_player_cur)
    }
    player_tot_perTeam_pg <- bind_rows(player_tot_perTeam_pg, player_tot_cur)
}

player_tot_perTeam <- player_tot_perTeam_pg %>% 
    group_by(stats,team) %>% 
    summarise(
        across(everything(), .fns = sum),
               .groups = "drop"
    ) %>%
    select(-game) %>% 
    rename(player = stats) %>% 
    relocate(team, .after =player)

#' assists are tricky!
#' in the NBA assists are not granted for pass before FT in the FIBA world the count as assists!
#' I compute the NBA style assists as I work with methods which are developed for the NBA

#******************************************************************************#
# calc. minutes played: ----
#debugonce(playing_time)
z <- tibble()
y <- filter(pbp,
            game_id ==22325)
for (i in unique(y$game_id)) {
    a <- filter(y,
                game_id == i)
    b <- filter(starters2018,
                game_nr == i)
    c <- playing_time(b,a)
    c$game_nr <- i
    
    z <- bind_rows(z,c)
}
view(z)

#' therefore a data frame must be build which tells who is on the court and when
#' this must be done for every single game!
#' 
#' CHANGE every year!
play_time <- tibble()
for (i in unique(pbp$game_id)) {
    a <- filter(pbp,
                game_id == i)
    b <- filter(starters2016,
                game_nr == i)
    c <- playing_time(b,a)
    c$game_nr <- i
    
    play_time <- bind_rows(play_time,c)
}

games_played  <- play_time %>%                         # Count rows by group
    group_by(player) %>% 
    summarise(G = n())

play_time1 <- play_time %>% 
    mutate_at("sec_total", ~replace(., is.na(.), 0))

df <- play_time1 %>% group_by(player) %>%
    summarize(Sum_sec = sum(sec_total)) %>% 
    mutate(min_sec_played = lubridate::seconds_to_period(Sum_sec)) %>% 
    mutate(min_sec = round(Sum_sec / 60, digits = 2))


n_distinct(roster2016$game_nr)

n_distinct(roster$game_nr)

setdiff(roster$game_nr, starters2016$game_nr)

#******************************************************************************#
# Merge boxscore & playing time: ----
df_new <- merge(df,player_tot_perTeam,
                by = "player")

player_data <- merge(df_new, games_played,
             by = "player") %>% 
    relocate(G, .after = player)
player_data$player <- trimws(player_data$player)

# 2016
player_data$player[player_data$player == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"

# 2017
player_data$player[player_data$player == "Miles, Jackson-C"] <- "Miles, Jackson-Cartwright"

# 2018
player_data$player[player_data$player == "Ra#Shad, James"] <- "Ra'Shad, James"

#******************************************************************************#
# Download Position etc. & merge: ----
# 2017
player_info <- pos_cm_kg(2016)
player_info$player[player_data$player == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"

player_data_info<- merge(player_data,player_info,
                         by = "player") %>% 
    filter(., not_played == 0) %>% 
    distinct(.,player,team, .keep_all = TRUE)

# 2018
player_info <- pos_cm_kg(2018)
player_info$player[player_info$player == "Ra#Shad, James"] <- "Ra'Shad, James"

player_data_info<- merge(player_data,player_info,
                     by = "player") %>% 
    filter(., not_played == 0) %>% 
    distinct(.,player,team, .keep_all = TRUE)

#******************************************************************************#
# calc. team fg, opp_fg, win_pct:----
team_totals <- team_totals %>% 
    mutate(fga = p2a + p3a,
           fgm = p2m + p3m,
           opp_fga = opp_p2a + opp_p3a,
           opp_fgm = opp_p2m + opp_p3m,
           min = round(min / 60 *5)) %>% 
    relocate(fga:opp_fgm, .before = opp_pts) %>% 
    drop_na()

#******************************************************************************#
# boxscore team pg:----
team_pg <- team_totals %>% 
    mutate(across(.cols = min:opp_pts, ~ .x / G),
           win_pct = W/G)

#******************************************************************************#
# calc. player min_p, fg:----
player_data_info <- player_data_info %>% 
    mutate(min_p = round(Sum_sec /60),
           fga = p2a + p3a,
           fgm = p2m + p3m,) %>% 
    relocate(min_p, .after = G) %>% 
    relocate(fgm, .before=pts) %>% 
    relocate(fga, .before= pts) %>% 
    relocate(team, .after = player) %>% 
    select(-min_sec, -min_sec_played, -Sum_sec)

#******************************************************************************#
# boxscore player pg:----
player_pg <- player_data_info %>%
    mutate(across(.cols = min_p:pts, ~ .x / G))

#******************************************************************************#
# player foul percentage:----
# foul percentage
team_for_merge <- team_totals %>% 
    select(team, opp_ftm, pf, G) %>% 
    rename(pf_t = pf) %>% 
    rename(G_t = G)

player_for_merge <- player_data_info %>% 
    select(player, team, pf,G) %>% 
    mutate(pf_p = pf) %>% 
    rename(G_p = G)

player_perT <- merge(player_for_merge,team_for_merge,
                     by = "team",
                     all = TRUE) %>% 
    mutate(pf_p = pf_p / G_t)

player_perT <- player_perT %>% 
    group_by(team) %>% 
    mutate(PF_perc = pf_p / pf_t,
           pf_p = opp_ftm * PF_perc * G_t) %>% 
    ungroup() %>% 
    select(player,team,pf_p)

player_totals <- merge(player_data_info, player_perT,
                       by = c("player","team"))
#******************************************************************************#
# save files:----
saveRDS(object = player_pg, file = paste0("Data/player_pg_2017",".Rds"))
saveRDS(object = player_totals, file = paste0("Data/player_totals_2017",".Rds"))

saveRDS(object = team_pg, file = paste0("Data/team_pg_2017",".Rds"))
saveRDS(object = team_totals, file = paste0("Data/team_totals_2017",".Rds"))
