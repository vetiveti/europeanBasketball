# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# source functions to use
source('functions/BBL_functions.R')
source('functions/pbp_actions.R')
library(tidyverse, warn.conflicts = FALSE)
require(data.table)
require(zoo)
#******************************************************************************#
# Load cleaned pbp files: ----
files <- list.files(path = './data/clean_pbp', pattern ='pbp')
pbp_list = lapply(paste0("Data/clean_pbp/",files), function (x) data.table(readRDS(x)))
pbp = rbindlist(pbp_list, fill = TRUE, idcol="ID") %>% 
    mutate(year = ID +2013)

#******************************************************************************#
# Load roster files: ----
files <- list.files(path = './data', pattern ='rosters')
roster_list = lapply(paste0("Data/",files), function (x) data.table(readRDS(x)))
roster = rbindlist(roster_list, fill = TRUE, idcol="year") %>%
    mutate(year = year +2007) %>% 
    filter(., year >= 2014)

#******************************************************************************#
# Load starter files: ----
files <- list.files(path = './data/starters', pattern ='starters')
starter_list = lapply(paste0("Data/starters/",files), function (x) data.table(readRDS(x)))
starter = rbindlist(starter_list, fill = TRUE, idcol="year") %>%
    mutate(year = year +2013)

#******************************************************************************#
# Box scores teams totals: ----
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
    current_boxscore$year <- current_game$year[1]
    current_boxscore_against$year <- current_game$year[1]
    bx_teams <- bind_rows(bx_teams,current_boxscore,current_boxscore_against)
}

bx_teams <- bx_teams %>% 
    mutate(W = if_else(pts>opp_pts,1,0),
           L = if_else(pts<opp_pts,1,0),
           G = W + L) %>% 
    rename(team = stats)

unique(bx_teams$team)
bx_teams$team[bx_teams$team == "Oettinger Rockets"] <- "Rockets"
bx_teams$team[bx_teams$team == "s.Oliver Baskets"] <- "s.Oliver Würzburg"
bx_teams$team[bx_teams$team == "s.Oliver Würzurg"] <- "s.Oliver Würzburg"
bx_teams$team[bx_teams$team == "SYNTAINICS MBC"] <- "Mitteldeutscher BC"
bx_teams$team[bx_teams$team == "Brose Baskets"] <- "Brose Bamberg"
bx_teams$team[bx_teams$team == "HAKRO Merlins Crailsheim"] <- "Crailsheim Merlins"
bx_teams$team[bx_teams$team == "BG GA#ttingen"] <- "BG Göttingen"
bx_teams$team[bx_teams$team == "Basketball LA#wen Braunschweig"] <- "Basketball Löwen Braunschweig"
bx_teams$team[bx_teams$team == "JobStairs GIESSEN 46ers"] <- "GIESSEN 46ers"
unique(bx_teams$team)

# bx_teams:----
saveRDS(object = bx_teams, file = paste0("Data/bx_teams_pg",".Rds"))

team_totals <- bx_teams %>%
    drop_na(team)
team_totals <- team_totals %>% 
    relocate(game_nr, .after = G) %>% 
    group_by(team,year) %>% 
    summarise_at(vars(min:G), .funs = sum) %>% 
    ungroup() %>% 
    drop_na(.,year)

##******************************************************************************#
# calc. team fg, opp_fg, win_pct:
team_totals <- team_totals %>% 
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

#******************************************************************************#
# box score team pg:----
team_pg <- team_totals %>% 
    mutate(across(.cols = min:opp_pts, ~ .x / G),
           win_pct = W/G)

#******************************************************************************#
# box score players pg & totals: ----
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
    player_tot_cur$year <- pbp_cur$year[10]
    player_tot_perTeam_pg <- bind_rows(player_tot_perTeam_pg, player_tot_cur)
}
player_tot_perTeam <- player_tot_perTeam_pg %>% 
    rename(player = stats)
player_tot_perTeam$player[player_tot_perTeam$player == "Jake, O#Brien"] <- "Jake, O'Brien"
player_tot_perTeam$player[player_tot_perTeam$player == "Jake, O`Brien"] <- "Jake, O'Brien"
player_tot_perTeam$player[player_tot_perTeam$player == "Jake, OBrien"] <- "Jake, O'Brien"
player_tot_perTeam$player[player_tot_perTeam$player == "Chad, Topper"] <- "Chad, Toppert"
player_tot_perTeam$player[player_tot_perTeam$player == "Nihad, Dedovic"] <- "Nihad, Djedovic"
player_tot_perTeam$player[player_tot_perTeam$player == "Anthony, Di Leo"] <- "Anthony, DiLeo"
player_tot_perTeam$player[player_tot_perTeam$player == "Joshua Patrick, Gasser"] <- "Joshua, Gasser"
player_tot_perTeam$player[player_tot_perTeam$player == "Nicolò, Melli"] <- "Nicolo, Melli"
player_tot_perTeam$player[player_tot_perTeam$player == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"
player_tot_perTeam$player[player_tot_perTeam$player == "Miles, Jackson-C"] <- "Miles, Jackson-Cartwright"
player_tot_perTeam$player[player_tot_perTeam$player == "Darvin, Davis"] <- "Darwin, Davis"
player_tot_perTeam$player[player_tot_perTeam$player == "E. J., Singler"] <- "E.J., Singler"
player_tot_perTeam$player[player_tot_perTeam$player == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"
player_tot_perTeam$player[player_tot_perTeam$player == "Ra#Shad, James"] <- "Ra'Shad, James"
player_tot_perTeam$team[player_tot_perTeam$team == "Oettinger Rockets"] <- "Rockets"
player_tot_perTeam$team[player_tot_perTeam$team == "s.Oliver Baskets"] <- "s.Oliver Würzburg"
player_tot_perTeam$team[player_tot_perTeam$team == "s.Oliver Würzurg"] <- "s.Oliver Würzburg"
player_tot_perTeam$team[player_tot_perTeam$team == "SYNTAINICS MBC"] <- "Mitteldeutscher BC"
player_tot_perTeam$team[player_tot_perTeam$team == "Brose Baskets"] <- "Brose Bamberg"
player_tot_perTeam$team[player_tot_perTeam$team == "HAKRO Merlins Crailsheim"] <- "Crailsheim Merlins"
player_tot_perTeam$team[player_tot_perTeam$team == "BG GA#ttingen"] <- "BG Göttingen"
player_tot_perTeam$team[player_tot_perTeam$team == "Basketball LA#wen Braunschweig"] <- "Basketball Löwen Braunschweig"
player_tot_perTeam$team[player_tot_perTeam$team == "JobStairs GIESSEN 46ers"] <- "GIESSEN 46ers"

player_tot_perTeam <- player_tot_perTeam %>% 
    group_by(player,team, year) %>% 
    summarise_at(vars(p2a:pts), .funs = sum) %>% 
    ungroup() %>% 
    relocate(team, .after =player)

#' assists are tricky!
#' in the NBA assists are not granted for pass before FT in the FIBA world the count as assists!
#' I compute the BBL style for comparison reasons

#******************************************************************************#
# calc. minutes played:----
# source('functions/BBL_functions.R')
# debugonce(playing_time)
# z <- tibble()
# y <- filter(pbp,
#             game_id ==17189)
# for (i in unique(y$game_id)) {
#     a <- filter(y,
#                 game_id == i)
#     b <- filter(starters2015,
#                 game_nr == i)
#     c <- playing_time(b,a)
#     c$game_nr <- i
#     
#     z <- bind_rows(z,c)
# }
# view(z)

#' therefore a data frame must be build which tells who is on the court and when
#' this must be done for every single game!
#' 
play_time <- tibble()
for (i in unique(pbp$game_id)) {
    a <- filter(pbp,
                game_id == i)
    b <- filter(starter,
                game_nr == i)
    c <- playing_time(b,a)
    c$game_nr <- i
    c$year <- a$year[3]
    play_time <- bind_rows(play_time,c)
}

play_time$player <- trimws(play_time$player)

play_time$player[play_time$player == "Jake, O#Brien"] <- "Jake, O'Brien"
play_time$player[play_time$player == "Jake, O`Brien"] <- "Jake, O'Brien"
play_time$player[play_time$player == "Jake, OBrien"] <- "Jake, O'Brien"
play_time$player[play_time$player == "Chad, Topper"] <- "Chad, Toppert"
play_time$player[play_time$player == "Nihad, Dedovic"] <- "Nihad, Djedovic"
play_time$player[play_time$player == "Anthony, Di Leo"] <- "Anthony, DiLeo"
play_time$player[play_time$player == "Joshua Patrick, Gasser"] <- "Joshua, Gasser"
play_time$player[play_time$player == "Nicolò, Melli"] <- "Nicolo, Melli"
play_time$player[play_time$player == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"
play_time$player[play_time$player == "Miles, Jackson-C"] <- "Miles, Jackson-Cartwright"
play_time$player[play_time$player == "Darvin, Davis"] <- "Darwin, Davis"
play_time$player[play_time$player == "E. J., Singler"] <- "E.J., Singler"
play_time$player[play_time$player == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"
play_time$player[play_time$player == "Ra#Shad, James"] <- "Ra'Shad, James"
play_time$player[play_time$player == "Konstantin, Klein"] <- "Konstantin, Konga"
play_time$player[play_time$player == "Leon Iduma, Okpara"] <- "Leon, Okpara"
play_time$player[play_time$player == "Quirin, Emanga Noupoue"] <- "Quirin, Emanga"
play_time$player[play_time$player == "Zan Mark, Sisko"] <- "Zan, Sisko"
play_time$Club[play_time$Club == "Oettinger Rockets"] <- "Rockets"
play_time$Club[play_time$Club == "s.Oliver Baskets"] <- "s.Oliver Würzburg"
play_time$Club[play_time$Club == "s.Oliver Würzurg"] <- "s.Oliver Würzburg"
play_time$Club[play_time$Club == "SYNTAINICS MBC"] <- "Mitteldeutscher BC"
play_time$Club[play_time$Club == "Brose Baskets"] <- "Brose Bamberg"
play_time$Club[play_time$Club == "HAKRO Merlins Crailsheim"] <- "Crailsheim Merlins"
play_time$Club[play_time$Club == "BG GA#ttingen"] <- "BG Göttingen"
play_time$Club[play_time$Club == "Basketball LA#wen Braunschweig"] <- "Basketball Löwen Braunschweig"
play_time$Club[play_time$Club == "JobStairs GIESSEN 46ers"] <- "GIESSEN 46ers"
unique(play_time$Club)

games_played  <- play_time %>%              # Count rows by group
    group_by(player,Club,year) %>% 
    summarise(G = n(), .groups ="drop")

play_time1 <- play_time %>% 
    mutate_at("sec_total", ~replace(., is.na(.), 0))

df <- play_time1 %>% group_by(player,Club,year) %>%
    summarize(Sum_sec = sum(sec_total), .groups = "drop") %>% 
    mutate(min_sec_played = lubridate::seconds_to_period(Sum_sec)) %>% 
    mutate(min_sec = round(Sum_sec / 60, digits = 2))

# save player info:
saveRDS(object = df, file = paste0("Data/playing_time_player",".Rds"))

unique(df$Club)
unique(player_tot_perTeam$team)

# save player info:
saveRDS(object = player_tot_perTeam, file = paste0("Data/player_tot_perTeam_123",".Rds"))

#******************************************************************************#
# Merge boxscore & playing time:
df_new <- merge(df,player_tot_perTeam,
                by.x = c("player","Club","year"),
                by.y = c("player","team","year")) %>% 
    rename(team = Club)

player_data <- merge(df_new, games_played,
                     by.x = c("player","team","year"),
                     by.y = c("player","Club","year")) %>% 
    relocate(G, .after = player)
#******************************************************************************#
# Download Position etc. & merge: ----
rm$close()
# stop the selenium server
remDr$server$stop()
base::rm(remDr)
gc()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

# 2014
player_info1 <- pos_cm_kg(2015)
# save player info:
saveRDS(object = player_info1, file = paste0("Data/player_info/player_info2015",".Rds"))

player_info2 <- pos_cm_kg(2016)
# save player info:
saveRDS(object = player_info2, file = paste0("Data/player_info/player_info2016",".Rds"))

player_info3 <- pos_cm_kg(2017)
# save player info:
saveRDS(object = player_info3, file = paste0("Data/player_info/player_info2017",".Rds"))

player_info4 <- pos_cm_kg(2018)
# save player info:
saveRDS(object = player_info4, file = paste0("Data/player_info/player_info2018",".Rds"))

player_info5 <- pos_cm_kg(2014)
# save player info:
saveRDS(object = player_info5, file = paste0("Data/player_info/player_info2014",".Rds"))

player_info6 <- pos_cm_kg(2019)
# save player info:
saveRDS(object = player_info6, file = paste0("Data/player_info/player_info2019",".Rds"))

player_info7 <- pos_cm_kg(2020) %>% 
    distinct(., player, .keep_all = TRUE)
# save player info:
saveRDS(object = player_info2020, file = paste0("Data/player_info/player_info2020",".Rds"))

player_info <- bind_rows(player_info5, player_info1, player_info2, player_info3,
                         player_info4, player_info6, 
                         player_info2020, .id="id")
player_info$player[player_info$player == "Chad, Topper"] <- "Chad, Toppert"
player_info$player[player_info$player == "Jake, O#Brien"] <- "Jake, O'Brien"
player_info$player[player_info$player == "Jake, O`Brien"] <- "Jake, O'Brien"
player_info$player[player_info$player == "Jake, OBrien"] <- "Jake, O'Brien"
player_info$player[player_info$player == "Nihad, Dedovic"] <- "Nihad, Djedovic"
player_info$player[player_info$player == "Anthony, Di Leo"] <- "Anthony, DiLeo"
player_info$player[player_info$player == "Joshua Patrick, Gasser"] <- "Joshua, Gasser"
player_info$player[player_info$player == "Nicolò, Melli"] <- "Nicolo, Melli"
player_info$player[player_info$player == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"
player_info$player[player_info$player == "Miles, Jackson-C"] <- "Miles, Jackson-Cartwright"
player_info$player[player_info$player == "Darvin, Davis"] <- "Darwin, Davis"
player_info$player[player_info$player == "E. J., Singler"] <- "E.J., Singler"
player_info$player[player_info$player == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"
player_info$player[player_info$player == "Ra#Shad, James"] <- "Ra'Shad, James"
player_info$player[player_info$player == "Konstantin, Klein"] <- "Konstantin, Konga"
player_info$player[player_info$player == "Leon Iduma, Okpara"] <- "Leon, Okpara"
player_info$player[player_info$player == "Quirin, Emanga Noupoue"] <- "Quirin, Emanga"
player_info$player[player_info$player == "Zan Mark, Sisko"] <- "Zan, Sisko"

# check if the !same! player has different entries 
# z <- player_info %>% 
#     arrange(player)
# 
# c <- player_info %>% 
#     arrange(player) %>% 
#     distinct(.,player, .keep_all = TRUE)
# 
# a <- c %>% 
#     arrange(player) %>% 
#     dplyr::select(-id, -player,-year, -Alter, -`Letzter Verein`, -left_team,- not_played, -Nr.)
# b <- a %>% 
#     #group_by(year) %>% 
#     base::duplicated()
# 
# d <- a %>% 
#     #group_by(year) %>% 
#     base::duplicated(.,fromLast =TRUE)
# zz <- c
# zz$dup <- as.integer(b)
# zz$dup2 <- as.integer(d)
# 
# zz <- zz %>%
#     filter(dup == 1 | dup2 == 1)

# save player info:
saveRDS(object = player_info, file = paste0("Data/player_info",".Rds"))
# read Rds
player_info <- readRDS("Data/player_info.Rds")

player_data_info<- merge(player_data,player_info,
                         by = c("player","year")) %>% 
    filter(., not_played == 0) %>% 
    distinct(.,player,team,year, .keep_all = TRUE)

#******************************************************************************#
# calc. player min_p, fg:
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
    relocate(fga, fgm, .after = min_p) %>% 
    select(-id) %>% 
    mutate(across(.cols = min_p:pts, ~ .x / G))

#******************************************************************************#
# player foul percentage:----
# foul percentage
team_for_merge <- team_totals %>% 
    select(team, opp_ftm, pf, G, year) %>% 
    rename(pf_t = pf) %>% 
    rename(G_t = G)
unique(team_for_merge$team)

player_for_merge <- player_data_info %>% 
    select(player, team, pf,G,year) %>% 
    mutate(pf_p = pf) %>% 
    rename(G_p = G)

player_perT <- merge(player_for_merge,team_for_merge,
                     by = c("team","year"),
                     all = TRUE) 

player_perT <- player_perT %>% 
    group_by(team,year) %>% 
    mutate(PF_perc = pf_p / pf_t) %>% 
    ungroup() %>% 
    select(player,team,year,PF_perc)

player_totals <- merge(player_data_info, player_perT,
                       by = c("player","team","year")) %>% 
    relocate(fga, fgm, .after = min_p) %>% 
    select(-id)

#******************************************************************************#
# Save data merged files:----
saveRDS(object = player_pg, file = paste0("Data/player_data_pg",".Rds"))
saveRDS(object = player_totals, file = paste0("Data/player_data_totals",".Rds"))

saveRDS(object = team_pg, file = paste0("Data/team_data_pg",".Rds"))
saveRDS(object = team_totals, file = paste0("Data/team_data_totals",".Rds"))

#******************************************************************************#
#******************************************************************************#