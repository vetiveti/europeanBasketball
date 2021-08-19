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
# load box score per game:----
bx_teams_pg<- readRDS("Data/bx_teams_pg.Rds") %>% 
    drop_na(team) %>% 
    relocate(game_nr, .after = G)

# calc. team fg, opp_fg:
bx_teams_pg <- bx_teams_pg %>% 
    mutate(fga = p2a + p3a,
           fgm = p2m + p3m,
           opp_fga = opp_p2a + opp_p3a,
           opp_fgm = opp_p2m + opp_p3m,
           min = round(min / 60 *5)) %>% 
    relocate(team, year, G, W, L, everything()) %>% 
    relocate(fga, fgm, .after = min) %>% 
    relocate(opp_fga, opp_fgm, .after = opp_min) %>% 
    mutate(opp_min = round(opp_min/60 * 5))

#******************************************************************************#
# box score players every game: ----
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
player_tot_perTeam$player[player_tot_perTeam$player == "Konstantin, Klein"] <- "Konstantin, Konga"
player_tot_perTeam$player[player_tot_perTeam$player == "Leon Iduma, Okpara"] <- "Leon, Okpara"
player_tot_perTeam$player[player_tot_perTeam$player == "Quirin, Emanga Noupoue"] <- "Quirin, Emanga"
player_tot_perTeam$player[player_tot_perTeam$player == "Zan Mark, Sisko"] <- "Zan, Sisko"
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
    relocate(team, .after =player)

# playing time:
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

games_played  <- play_time %>%              # Count rows by group
    group_by(player,Club,year) %>% 
    summarise(G = n(), .groups ="drop")

play_time1 <- play_time %>% 
    mutate_at("sec_total", ~replace(., is.na(.), 0))

play_time1$sec_total[play_time1$player == "Lucas, Gertz" & play_time1$game_nr == 17276] <- 135
play_time1$sec_total[play_time1$player == "Jordon, Crawford" & play_time1$game_nr == 22132] <- 2589
play_time1$sec_total[play_time1$player == "David, Godbold" & play_time1$game_nr == 18412] <- 715

df <- play_time1 %>% group_by(player,Club,year,game_nr) %>%
    summarize(Sum_sec = sum(sec_total), .groups = "drop") %>% 
    mutate(min_sec_played = lubridate::seconds_to_period(Sum_sec)) %>% 
    mutate(min_sec = round(Sum_sec / 60, digits = 3)) %>% 
    rename(game = game_nr)

# check if all teams are the same
unique(df$Club)
unique(player_tot_perTeam$team)

#******************************************************************************#
# Merge box score & playing time:
df_new <- merge(df,player_tot_perTeam,
                by.x = c("player","Club","year","game"),
                by.y = c("player","team","year","game"),
                all = TRUE) %>% 
    rename(team = Club) %>% 
    drop_na(min_sec)

control <- df_new %>% 
    arrange(min_sec,desc(pts))

# save player_pg:
saveRDS(object = df_new, file = paste0("Data/players_pg",".Rds"))
# es sind ein paar games wo spieler punkte haben aber keine minute gespielt diese nochmal anschauen!

df_new <- readRDS("Data/players_pg.Rds")

#******************************************************************************#
# Download Position etc. & merge: ----
# rm$close()
# # stop the selenium server
# remDr$server$stop()
# base::rm(remDr)
# gc()
# system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
# 
# # since 2014
# player_info1 <- pos_cm_kg(2015)
# # save player info:
# saveRDS(object = player_info1, file = paste0("Data/player_info/player_info2015",".Rds"))
# player_info1 <- readRDS("Data/player_info/player_info2015.Rds")
# 
# player_info2 <- pos_cm_kg(2016)
# # save player info:
# saveRDS(object = player_info2, file = paste0("Data/player_info/player_info2016",".Rds"))
# player_info2 <- readRDS("Data/player_info/player_info2016.Rds")
# 
# player_info3 <- pos_cm_kg(2017)
# # save player info:
# saveRDS(object = player_info3, file = paste0("Data/player_info/player_info2017",".Rds"))
# 
# player_info4 <- pos_cm_kg(2018)
# # save player info:
# saveRDS(object = player_info4, file = paste0("Data/player_info/player_info2018",".Rds"))
# 
# player_info5 <- pos_cm_kg(2014)
# # save player info:
# saveRDS(object = player_info5, file = paste0("Data/player_info/player_info2014",".Rds"))
# 
# player_info6 <- pos_cm_kg(2019)
# # save player info:
# saveRDS(object = player_info6, file = paste0("Data/player_info/player_info2019",".Rds"))
# 
# player_info7 <- pos_cm_kg(2020) %>% 
#     distinct(., player, .keep_all = TRUE)
# player_info2020 <- player_info7
# # save player info:
# saveRDS(object = player_info2020, file = paste0("Data/player_info/player_info2020",".Rds"))
# 
# player_info <- bind_rows(player_info5, player_info1, player_info2, player_info3,
#                          player_info4, player_info6, 
#                          player_info2020, .id="id")
# player_info$player[player_info$player == "Chad, Topper"] <- "Chad, Toppert"
# player_info$player[player_info$player == "Jake, O#Brien"] <- "Jake, O'Brien"
# player_info$player[player_info$player == "Jake, O`Brien"] <- "Jake, O'Brien"
# player_info$player[player_info$player == "Jake, OBrien"] <- "Jake, O'Brien"
# player_info$player[player_info$player == "Nihad, Dedovic"] <- "Nihad, Djedovic"
# player_info$player[player_info$player == "Anthony, Di Leo"] <- "Anthony, DiLeo"
# player_info$player[player_info$player == "Joshua Patrick, Gasser"] <- "Joshua, Gasser"
# player_info$player[player_info$player == "Nicolò, Melli"] <- "Nicolo, Melli"
# player_info$player[player_info$player == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"
# player_info$player[player_info$player == "Miles, Jackson-C"] <- "Miles, Jackson-Cartwright"
# player_info$player[player_info$player == "Darvin, Davis"] <- "Darwin, Davis"
# player_info$player[player_info$player == "E. J., Singler"] <- "E.J., Singler"
# player_info$player[player_info$player == "Jonas, Wohlfarth-B."] <- "Jonas, Wohlfarth-Bottermann"
# player_info$player[player_info$player == "Ra#Shad, James"] <- "Ra'Shad, James"
# player_info$player[player_info$player == "Konstantin, Klein"] <- "Konstantin, Konga"
# player_info$player[player_info$player == "Leon Iduma, Okpara"] <- "Leon, Okpara"
# player_info$player[player_info$player == "Quirin, Emanga Noupoue"] <- "Quirin, Emanga"
# player_info$player[player_info$player == "Zan Mark, Sisko"] <- "Zan, Sisko"
# 
# # save player info:
# saveRDS(object = player_info, file = paste0("Data/player_info",".Rds"))
# read Rds
player_info <- readRDS("Data/player_info.Rds")

player_data_info<- merge(df_new,player_info,
                         by = c("player","year"), all =TRUE) %>% 
    filter(., not_played == 0) %>% 
    drop_na(game) %>% 
    distinct(.,player,team,year,game, .keep_all = TRUE)

#******************************************************************************#
# calc. player min_p, fg:
player_data_info <- player_data_info %>% 
    mutate(min_p = round(Sum_sec /60,1),
           fga = p2a + p3a,
           fgm = p2m + p3m,) %>% 
    relocate(min_p, .after = game) %>% 
    relocate(fgm, .before=pts) %>% 
    relocate(fga, .before= pts) %>% 
    relocate(team, .after = player) %>% 
    select(-min_sec, -min_sec_played, -Sum_sec)

#******************************************************************************#
# Save data :----
saveRDS(object = player_data_info, file = paste0("Data/players_each_game",".Rds"))

saveRDS(object = bx_teams_pg, file = paste0("Data/teams_each_game",".Rds"))
#******************************************************************************#