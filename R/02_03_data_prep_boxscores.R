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
    mutate(year = ID +2009)

#******************************************************************************#
# Load roster files: ----
files <- list.files(path = './data', pattern ='rosters')
roster_list = lapply(paste0("Data/",files), function (x) data.table(readRDS(x)))
roster = rbindlist(roster_list, fill = TRUE, idcol="year") %>%
    mutate(year = year +2007) %>% 
    filter(., year >= 2010)

#******************************************************************************#
# Load starter files: ----
files <- list.files(path = './data/starters', pattern ='starters')
starter_list = lapply(paste0("Data/starters/",files), function (x) data.table(readRDS(x)))
starter = rbindlist(starter_list, fill = TRUE, idcol="year") %>%
    mutate(year = year +2009)

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

#bx_teams <-readRDS("Data/bx_teams_pg.Rds")

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
bx_teams$team[bx_teams$team == "Gloria GIANTS DÃ¼sseldorf"] <- "Gloria GIANTS Düsseldorf"
bx_teams$team[bx_teams$team == "EisbÃ¤ren Bremerhaven"] <- "Eisbären Bremerhaven"
bx_teams$team[bx_teams$team == "LTi GIESSEN 46ers"] <- "GIESSEN 46ers"
bx_teams$team[bx_teams$team == "LTi  GIESSEN 46ers"] <- "GIESSEN 46ers"
bx_teams$team[bx_teams$team == "Deutsche Bank Skyliners"] <- "FRAPORT SKYLINERS"
bx_teams$team[bx_teams$team == "DEUTSCHE BANK SKYLINERS"] <- "FRAPORT SKYLINERS"
bx_teams$team[bx_teams$team == "WALTER Tigers TÃ¼bingen"] <- "WALTER Tigers Tübingen"
bx_teams$team[bx_teams$team == "New Yorker Phantoms Braunschwe"] <- "Basketball Löwen Braunschweig"
bx_teams$team[bx_teams$team == "Trier"] <- "TBB Trier"
bx_teams$team[bx_teams$team == "New Yorker Phantoms"] <- "Basketball Löwen Braunschweig"
bx_teams$team[bx_teams$team == "Alba Berlin"] <- "ALBA BERLIN"
bx_teams$team[bx_teams$team == "Brose Baskets Bamberg"] <- "Brose Bamberg"
bx_teams$team[bx_teams$team == "Walter Tigers TÃ¼bingen"] <- "WALTER Tigers Tübingen"
bx_teams$team[bx_teams$team == "ALBA Berlin"] <- "ALBA BERLIN"
bx_teams$team[bx_teams$team == "BERLIN"] <- "ALBA BERLIN"
bx_teams$team[bx_teams$team == "Artland  Dragons"] <- "Artland Dragons"
bx_teams$team[bx_teams$team == "Atrland Dragons"] <- "Artland Dragons"
bx_teams$team[bx_teams$team == "BG GÃ¶ttingen"] <- "BG Göttingen"
bx_teams$team[bx_teams$team == "Bose Baskets"] <- "Brose Bamberg"
bx_teams$team[bx_teams$team == "Brose Bakets"] <- "Brose Bamberg"
bx_teams$team[bx_teams$team == "brose baskets"] <- "Brose Bamberg"
bx_teams$team[bx_teams$team == "Deutsche Bank SKYLINERS"] <- "FRAPORT SKYLINERS"
bx_teams$team[bx_teams$team == "EWE Baskets"] <- "EWE Baskets Oldenburg"
bx_teams$team[bx_teams$team == "EWE Baskets Oldenbur"] <- "EWE Baskets Oldenburg"
bx_teams$team[bx_teams$team == "Ewe Baskets Oldenburg"] <- "EWE Baskets Oldenburg"
bx_teams$team[bx_teams$team == "EWE BASKETS OLDENBURG"] <- "EWE Baskets Oldenburg"
bx_teams$team[bx_teams$team == "FC Bayern MÃ¼nchen"] <- "FC Bayern Basketball"
bx_teams$team[bx_teams$team == "FC Bayern MÃ¼nchen Basketball"] <- "FC Bayern Basketball"
bx_teams$team[bx_teams$team == "FC BAYERN MÃoNCHEN"] <- "FC Bayern Basketball"
bx_teams$team[bx_teams$team == "FC Bayern Muenchen"] <- "FC Bayern Basketball"
bx_teams$team[bx_teams$team == "FC Bayern MÃ¼nchen Basketball"] <- "FC Bayern Basketball"
bx_teams$team[bx_teams$team == "FC Bayern München"] <- "FC Bayern Basketball"
bx_teams$team[bx_teams$team == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
bx_teams$team[bx_teams$team == "FRAPORT Skyliners"] <- "FRAPORT SKYLINERS"
bx_teams$team[bx_teams$team == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
bx_teams$team[bx_teams$team == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
bx_teams$team[bx_teams$team == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
bx_teams$team[bx_teams$team == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
bx_teams$team[bx_teams$team == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
bx_teams$team[bx_teams$team == "FC Bayern MÃ¼nchen Basketball"] <- "FC Bayern Basketball"
bx_teams$team[bx_teams$team == "ENBW Ludwigsburg"] <- "EnBW Ludwigsburg"
bx_teams$team[bx_teams$team == "ENBW Ludwigsburg"] <- "EnBW Ludwigsburg"
bx_teams$team[bx_teams$team == "BG GÃ¶ttingen"] <- "BG Göttingen"
bx_teams$team[bx_teams$team == "Gloria GIANTS"] <- "Gloria GIANTS Düsseldorf"
bx_teams$team[bx_teams$team == "Gloria Giants DÃ¼sseldorf"] <- "Gloria GIANTS Düsseldorf"
bx_teams$team[bx_teams$team == "LTI GieÃYen"] <- "GIESSEN 46ers"
bx_teams$team[bx_teams$team == "LTi GieÃYen 46ers"] <- "GIESSEN 46ers"
bx_teams$team[bx_teams$team == "LTi Giessen 46ers"] <- "GIESSEN 46ers"
bx_teams$team[bx_teams$team == "Ludwigsburg"] <- "MHP Ludwigsburg"
bx_teams$team[bx_teams$team == "medi bayreuth"] <- "Medi Bayreuth"
bx_teams$team[bx_teams$team == "medi Bayreuth"] <- "Medi Bayreuth"
bx_teams$team[bx_teams$team == "MHP RIESEN Ludswigsburg"] <- "MHP Ludwigsburg"
bx_teams$team[bx_teams$team == "MHP Riesen Ludwigsburg"] <- "MHP Ludwigsburg"
bx_teams$team[bx_teams$team == "MHP RIESEN Ludwigsburg"] <- "MHP Ludwigsburg"
bx_teams$team[bx_teams$team == "MHP Riesen Ludwigsburg 12"] <- "MHP Ludwigsburg"
bx_teams$team[bx_teams$team == "Mitteldeutscher Baketball Club"] <- "Mitteldeutscher BC"
bx_teams$team[bx_teams$team == "Mitteldeutscher Basketball"] <- "Mitteldeutscher BC"
bx_teams$team[bx_teams$team == "Mitteldeutscher Basketball Clu"] <- "Mitteldeutscher BC"
bx_teams$team[bx_teams$team == "MPH RIESEN Ludwigsburg"] <- "MHP Ludwigsburg"
bx_teams$team[bx_teams$team == "Neckar Riesen Ludwigsburg"] <- "MHP Ludwigsburg"
bx_teams$team[bx_teams$team == "Neckar RIESEN Ludwigsburg"] <- "MHP Ludwigsburg"
bx_teams$team[bx_teams$team == "NECKAR RIESEN Ludwigsburg"] <- "MHP Ludwigsburg"
bx_teams$team[bx_teams$team == "New Yoker Phantoms Braunschwei"] <- "Basketball Löwen Braunschweig"
bx_teams$team[bx_teams$team == "New York Phantoms Braunschweig"] <- "Basketball Löwen Braunschweig"
bx_teams$team[bx_teams$team == "New Yorker Phantoms BRAUNSCHWE"] <- "Basketball Löwen Braunschweig"
bx_teams$team[bx_teams$team == "NEW YORKER PHANTOMS"] <- "Basketball Löwen Braunschweig"
bx_teams$team[bx_teams$team == "NY Phantoms Braunschweig"] <- "Basketball Löwen Braunschweig"
bx_teams$team[bx_teams$team == "PhÃ¶nix Hagen"] <- "Phoenix Hagen"
bx_teams$team[bx_teams$team == "PHOENIX HAGEN"] <- "Phoenix Hagen"
bx_teams$team[bx_teams$team == "Poenix Hagen"] <- "Phoenix Hagen"
bx_teams$team[bx_teams$team == "rathiopharm ulm"] <- "Ratiopharm Ulm"
bx_teams$team[bx_teams$team == "rathiopharm Ulm"] <- "Ratiopharm Ulm"
bx_teams$team[bx_teams$team == "ratiopharm  Ulm"] <- "Ratiopharm Ulm"
bx_teams$team[bx_teams$team == "ratiopharm ulm"] <- "Ratiopharm Ulm"
bx_teams$team[bx_teams$team == "ratiopharm Ulm"] <- "Ratiopharm Ulm"
bx_teams$team[bx_teams$team == "s. Oliver Baskets"] <- "s.Oliver Würzburg"
bx_teams$team[bx_teams$team == "s.Oliver Baskets WÃ¼rzburg"] <- "s.Oliver Würzburg"
bx_teams$team[bx_teams$team == "SC Rasta Vechta"] <- "RASTA Vechta"
bx_teams$team[bx_teams$team == "SC RASTA Vechta"] <- "RASTA Vechta"
bx_teams$team[bx_teams$team == "Telekom Baskets Bonne"] <- "Telekom Baskets Bonn"
bx_teams$team[bx_teams$team == "EnBW Ludwgsburg"] <- "EnBW Ludwigsburg"
bx_teams$team[bx_teams$team == "EnBW Ludwigsurg"] <- "EnBW Ludwigsburg"
bx_teams$team[bx_teams$team == "EnBW Ludwigsburg"] <- "MHP Ludwigsburg"

bx_teams <- bx_teams %>% 
    arrange(team)
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
player_tot_perTeam$team[player_tot_perTeam$team == "Gloria GIANTS DÃ¼sseldorf"] <- "Gloria GIANTS Düsseldorf"
player_tot_perTeam$team[player_tot_perTeam$team == "EisbÃ¤ren Bremerhaven"] <- "Eisbären Bremerhaven"
player_tot_perTeam$team[player_tot_perTeam$team == "LTi GIESSEN 46ers"] <- "GIESSEN 46ers"
player_tot_perTeam$team[player_tot_perTeam$team == "LTi  GIESSEN 46ers"] <- "GIESSEN 46ers"
player_tot_perTeam$team[player_tot_perTeam$team == "Deutsche Bank Skyliners"] <- "FRAPORT SKYLINERS"
player_tot_perTeam$team[player_tot_perTeam$team == "DEUTSCHE BANK SKYLINERS"] <- "FRAPORT SKYLINERS"
player_tot_perTeam$team[player_tot_perTeam$team == "WALTER Tigers TÃ¼bingen"] <- "WALTER Tigers Tübingen"
player_tot_perTeam$team[player_tot_perTeam$team == "New Yorker Phantoms Braunschwe"] <- "Basketball Löwen Braunschweig"
player_tot_perTeam$team[player_tot_perTeam$team == "Trier"] <- "TBB Trier"
player_tot_perTeam$team[player_tot_perTeam$team == "New Yorker Phantoms"] <- "Basketball Löwen Braunschweig"
player_tot_perTeam$team[player_tot_perTeam$team == "Alba Berlin"] <- "ALBA BERLIN"
player_tot_perTeam$team[player_tot_perTeam$team == "Brose Baskets Bamberg"] <- "Brose Bamberg"
player_tot_perTeam$team[player_tot_perTeam$team == "Walter Tigers TÃ¼bingen"] <- "WALTER Tigers Tübingen"
player_tot_perTeam$team[player_tot_perTeam$team == "ALBA Berlin"] <- "ALBA BERLIN"
player_tot_perTeam$team[player_tot_perTeam$team == "BERLIN"] <- "ALBA BERLIN"
player_tot_perTeam$team[player_tot_perTeam$team == "Artland  Dragons"] <- "Artland Dragons"
player_tot_perTeam$team[player_tot_perTeam$team == "Atrland Dragons"] <- "Artland Dragons"
player_tot_perTeam$team[player_tot_perTeam$team == "BG GÃ¶ttingen"] <- "BG Göttingen"
player_tot_perTeam$team[player_tot_perTeam$team == "Bose Baskets"] <- "Brose Bamberg"
player_tot_perTeam$team[player_tot_perTeam$team == "Brose Bakets"] <- "Brose Bamberg"
player_tot_perTeam$team[player_tot_perTeam$team == "brose baskets"] <- "Brose Bamberg"
player_tot_perTeam$team[player_tot_perTeam$team == "Deutsche Bank SKYLINERS"] <- "FRAPORT SKYLINERS"
player_tot_perTeam$team[player_tot_perTeam$team == "EWE Baskets"] <- "EWE Baskets Oldenburg"
player_tot_perTeam$team[player_tot_perTeam$team == "EWE Baskets Oldenbur"] <- "EWE Baskets Oldenburg"
player_tot_perTeam$team[player_tot_perTeam$team == "Ewe Baskets Oldenburg"] <- "EWE Baskets Oldenburg"
player_tot_perTeam$team[player_tot_perTeam$team == "EWE BASKETS OLDENBURG"] <- "EWE Baskets Oldenburg"
player_tot_perTeam$team[player_tot_perTeam$team == "FC Bayern MÃ¼nchen"] <- "FC Bayern Basketball"
player_tot_perTeam$team[player_tot_perTeam$team == "FC Bayern MÃ¼nchen Basketball"] <- "FC Bayern Basketball"
player_tot_perTeam$team[player_tot_perTeam$team == "FC BAYERN MÃoNCHEN"] <- "FC Bayern Basketball"
player_tot_perTeam$team[player_tot_perTeam$team == "FC Bayern Muenchen"] <- "FC Bayern Basketball"
player_tot_perTeam$team[player_tot_perTeam$team == "FC Bayern MÃ¼nchen Basketball"] <- "FC Bayern Basketball"
player_tot_perTeam$team[player_tot_perTeam$team == "FC Bayern München"] <- "FC Bayern Basketball"
player_tot_perTeam$team[player_tot_perTeam$team == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
player_tot_perTeam$team[player_tot_perTeam$team == "FRAPORT Skyliners"] <- "FRAPORT SKYLINERS"
player_tot_perTeam$team[player_tot_perTeam$team == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
player_tot_perTeam$team[player_tot_perTeam$team == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
player_tot_perTeam$team[player_tot_perTeam$team == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
player_tot_perTeam$team[player_tot_perTeam$team == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
player_tot_perTeam$team[player_tot_perTeam$team == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
player_tot_perTeam$team[player_tot_perTeam$team == "FC Bayern MÃ¼nchen Basketball"] <- "FC Bayern Basketball"
player_tot_perTeam$team[player_tot_perTeam$team == "ENBW Ludwigsburg"] <- "EnBW Ludwigsburg"
player_tot_perTeam$team[player_tot_perTeam$team == "ENBW Ludwigsburg"] <- "EnBW Ludwigsburg"
player_tot_perTeam$team[player_tot_perTeam$team == "BG GÃ¶ttingen"] <- "BG Göttingen"
player_tot_perTeam$team[player_tot_perTeam$team == "Gloria GIANTS"] <- "Gloria GIANTS Düsseldorf"
player_tot_perTeam$team[player_tot_perTeam$team == "Gloria Giants DÃ¼sseldorf"] <- "Gloria GIANTS Düsseldorf"
player_tot_perTeam$team[player_tot_perTeam$team == "LTI GieÃYen"] <- "GIESSEN 46ers"
player_tot_perTeam$team[player_tot_perTeam$team == "LTi GieÃYen 46ers"] <- "GIESSEN 46ers"
player_tot_perTeam$team[player_tot_perTeam$team == "LTi Giessen 46ers"] <- "GIESSEN 46ers"
player_tot_perTeam$team[player_tot_perTeam$team == "Ludwigsburg"] <- "MHP Ludwigsburg"
player_tot_perTeam$team[player_tot_perTeam$team == "medi bayreuth"] <- "Medi Bayreuth"
player_tot_perTeam$team[player_tot_perTeam$team == "medi Bayreuth"] <- "Medi Bayreuth"
player_tot_perTeam$team[player_tot_perTeam$team == "MHP RIESEN Ludswigsburg"] <- "MHP Ludwigsburg"
player_tot_perTeam$team[player_tot_perTeam$team == "MHP Riesen Ludwigsburg"] <- "MHP Ludwigsburg"
player_tot_perTeam$team[player_tot_perTeam$team == "MHP RIESEN Ludwigsburg"] <- "MHP Ludwigsburg"
player_tot_perTeam$team[player_tot_perTeam$team == "MHP Riesen Ludwigsburg 12"] <- "MHP Ludwigsburg"
player_tot_perTeam$team[player_tot_perTeam$team == "Mitteldeutscher Baketball Club"] <- "Mitteldeutscher BC"
player_tot_perTeam$team[player_tot_perTeam$team == "Mitteldeutscher Basketball"] <- "Mitteldeutscher BC"
player_tot_perTeam$team[player_tot_perTeam$team == "Mitteldeutscher Basketball Clu"] <- "Mitteldeutscher BC"
player_tot_perTeam$team[player_tot_perTeam$team == "MPH RIESEN Ludwigsburg"] <- "MHP Ludwigsburg"
player_tot_perTeam$team[player_tot_perTeam$team == "Neckar Riesen Ludwigsburg"] <- "MHP Ludwigsburg"
player_tot_perTeam$team[player_tot_perTeam$team == "Neckar RIESEN Ludwigsburg"] <- "MHP Ludwigsburg"
player_tot_perTeam$team[player_tot_perTeam$team == "NECKAR RIESEN Ludwigsburg"] <- "MHP Ludwigsburg"
player_tot_perTeam$team[player_tot_perTeam$team == "New Yoker Phantoms Braunschwei"] <- "Basketball Löwen Braunschweig"
player_tot_perTeam$team[player_tot_perTeam$team == "New York Phantoms Braunschweig"] <- "Basketball Löwen Braunschweig"
player_tot_perTeam$team[player_tot_perTeam$team == "New Yorker Phantoms BRAUNSCHWE"] <- "Basketball Löwen Braunschweig"
player_tot_perTeam$team[player_tot_perTeam$team == "NEW YORKER PHANTOMS"] <- "Basketball Löwen Braunschweig"
player_tot_perTeam$team[player_tot_perTeam$team == "NY Phantoms Braunschweig"] <- "Basketball Löwen Braunschweig"
player_tot_perTeam$team[player_tot_perTeam$team == "PhÃ¶nix Hagen"] <- "Phoenix Hagen"
player_tot_perTeam$team[player_tot_perTeam$team == "PHOENIX HAGEN"] <- "Phoenix Hagen"
player_tot_perTeam$team[player_tot_perTeam$team == "Poenix Hagen"] <- "Phoenix Hagen"
player_tot_perTeam$team[player_tot_perTeam$team == "rathiopharm ulm"] <- "Ratiopharm Ulm"
player_tot_perTeam$team[player_tot_perTeam$team == "rathiopharm Ulm"] <- "Ratiopharm Ulm"
player_tot_perTeam$team[player_tot_perTeam$team == "ratiopharm  Ulm"] <- "Ratiopharm Ulm"
player_tot_perTeam$team[player_tot_perTeam$team == "ratiopharm ulm"] <- "Ratiopharm Ulm"
player_tot_perTeam$team[player_tot_perTeam$team == "ratiopharm Ulm"] <- "Ratiopharm Ulm"
player_tot_perTeam$team[player_tot_perTeam$team == "s. Oliver Baskets"] <- "s.Oliver Würzburg"
player_tot_perTeam$team[player_tot_perTeam$team == "s.Oliver Baskets WÃ¼rzburg"] <- "s.Oliver Würzburg"
player_tot_perTeam$team[player_tot_perTeam$team == "SC Rasta Vechta"] <- "RASTA Vechta"
player_tot_perTeam$team[player_tot_perTeam$team == "SC RASTA Vechta"] <- "RASTA Vechta"
player_tot_perTeam$team[player_tot_perTeam$team == "Telekom Baskets Bonne"] <- "Telekom Baskets Bonn"
player_tot_perTeam$team[player_tot_perTeam$team == "EnBW Ludwgsburg"] <- "EnBW Ludwigsburg"
player_tot_perTeam$team[player_tot_perTeam$team == "EnBW Ludwigsurg"] <- "EnBW Ludwigsburg"
player_tot_perTeam$team[player_tot_perTeam$team == "EnBW Ludwigsburg"] <- "MHP Ludwigsburg"

player_tot_perTeam$player[player_tot_perTeam$player == "Acha, Njei"] <- "Acha, Njej"
player_tot_perTeam$player[player_tot_perTeam$player == "Achmadscha, Zazai"] <- "Achmadschah, Zazai"
player_tot_perTeam$player[player_tot_perTeam$player == "Albert King, Nolan"] <- "Albert King, Nolen"
player_tot_perTeam$player[player_tot_perTeam$player == "Ali, TraorÃ©"] <- "Ali, Traore"
player_tot_perTeam$player[player_tot_perTeam$player == "Andreas, BÃ¼chert"] <- "Andreas, Büchert"
player_tot_perTeam$player[player_tot_perTeam$player == "Aziz, N#Diaye"] <- "Aziz, N'Diaye"
player_tot_perTeam$player[player_tot_perTeam$player == "Aziz, NDiaye"] <- "Aziz, N'Diaye"
player_tot_perTeam$player[player_tot_perTeam$player == "Bazoumana, Kante"] <- "Bazoumana, Kone"
player_tot_perTeam$player[player_tot_perTeam$player == "Brandon Kyle, Bowman"] <- "Brandon, Bowman"
player_tot_perTeam$player[player_tot_perTeam$player == "Byron, Allen"] <- "Bryon, Allen"
player_tot_perTeam$player[player_tot_perTeam$player == "Casey (C), Jacobsen"] <- "Casey, Jacobsen"
player_tot_perTeam$player[player_tot_perTeam$player == "Christian, M#Baidanoum"] <- "Christian, M'Baidanoum"
player_tot_perTeam$player[player_tot_perTeam$player == "D#Or, Fischer"] <- "D'Or, Fischer"
player_tot_perTeam$player[player_tot_perTeam$player == "Darren (C), Fenn"] <- "Darren, Fenn"
player_tot_perTeam$player[player_tot_perTeam$player == "Dashaun, Wood"] <- "DaShaun, Wood"
player_tot_perTeam$player[player_tot_perTeam$player == "DaShaun (C), Wood"] <- "DaShaun, Wood"
player_tot_perTeam$player[player_tot_perTeam$player == "David Johnelle, Kennedy"] <- "David John, Kennedy"
player_tot_perTeam$player[player_tot_perTeam$player == "DeAndre (C), Haynes"] <- "DeAndre, Haynes"
player_tot_perTeam$player[player_tot_perTeam$player == "Derick, Allen"] <- "Derrick, Allen"
player_tot_perTeam$player[player_tot_perTeam$player == "Dirk, MÃ¤drich"] <- "Dirk, Mädrich"
player_tot_perTeam$player[player_tot_perTeam$player == "Dominik, Lockhardt"] <- "Dominic, Lockhart"
player_tot_perTeam$player[player_tot_perTeam$player == "Dragan (C), Dojcin"] <- "Dragan, Dojcin"
player_tot_perTeam$player[player_tot_perTeam$player == "Ekenechukwu, Ibekwe"] <- "Ekene, Ibekwe"
player_tot_perTeam$player[player_tot_perTeam$player == "Elvir (C), Ovcina"] <- "Elvir, Ovcina"
player_tot_perTeam$player[player_tot_perTeam$player == "Flavio, StÃ¼ckemann"] <- "Flavio, Stückemann"
player_tot_perTeam$player[player_tot_perTeam$player == "Greg-Emeka, Onwuegbuzie"] <- "Greg-Emeka, Onwuegbuze"
player_tot_perTeam$player[player_tot_perTeam$player == "Guido, GrÃ¼nheid"] <- "Guido, Grünheid"
player_tot_perTeam$player[player_tot_perTeam$player == "Heiko, Schaffratzik"] <- "Heiko, Schaffartzik"
player_tot_perTeam$player[player_tot_perTeam$player == "Jared (C), Reiner"] <- "Jared, Reiner"
player_tot_perTeam$player[player_tot_perTeam$player == "Jason Gregory, Boone"] <- "Jason, Boone"
player_tot_perTeam$player[player_tot_perTeam$player == "Jay (C), Thomas"] <- "Jay, Thomas"
player_tot_perTeam$player[player_tot_perTeam$player == "Jeremiah William, Davis"] <- "Jeremiah, Davis"
player_tot_perTeam$player[player_tot_perTeam$player == "Jerome, Tillman"] <- "Jerome, Tillmann"
player_tot_perTeam$player[player_tot_perTeam$player == "Jerry -CAP-, Green"] <- "Jerry, Green"
player_tot_perTeam$player[player_tot_perTeam$player == "Julius (C), Jenkins"] <- "Julius, Jenkins"
player_tot_perTeam$player[player_tot_perTeam$player == "Kenneth, Williams"] <- "Kenneth, Wiliams"
player_tot_perTeam$player[player_tot_perTeam$player == "Laquan, Prowell"] <- "LaQuan, Prowell"
player_tot_perTeam$player[player_tot_perTeam$player == "Larry D, Wright"] <- "Larry, Wright"
player_tot_perTeam$player[player_tot_perTeam$player == "Louis (C), Campbell"] <- "Louis, Campbell"
player_tot_perTeam$player[player_tot_perTeam$player == "Mathis, MÃ¶nninghoff"] <- "Mathis, Mönninghoff"
player_tot_perTeam$player[player_tot_perTeam$player == "Nils (C), Mittmann"] <- "Nils, Mittmann"
player_tot_perTeam$player[player_tot_perTeam$player == "Oskar, FaÃYler"] <- "Oskar, Fassler"
player_tot_perTeam$player[player_tot_perTeam$player == "Pascal (C), Roller"] <- "Pascal, Roller"
player_tot_perTeam$player[player_tot_perTeam$player == "Patrick (C), Femerling"] <- "Patrick, Femerling"
player_tot_perTeam$player[player_tot_perTeam$player == "Patrick -CAP-, Femerling"] <- "Patrick, Femerling"
player_tot_perTeam$player[player_tot_perTeam$player == "Per, GÃ¼nther"] <- "Per, Günther"
player_tot_perTeam$player[player_tot_perTeam$player == "qu, Robertson"] <- "Quantez, Robertson"
player_tot_perTeam$player[player_tot_perTeam$player == "Quentin (C), Pryor"] <- "Quentin, Pryor"
player_tot_perTeam$player[player_tot_perTeam$player == "Rickey (C), Paulding"] <- "Rickey, Paulding"
player_tot_perTeam$player[player_tot_perTeam$player == "Robin, PflÃ¼ger"] <- "Robin, Pflüger"
player_tot_perTeam$player[player_tot_perTeam$player == "Robin, PflÃ¼gler"] <- "Robin, Pflüger"
player_tot_perTeam$player[player_tot_perTeam$player == "Stefan, IlzhÃ¶fer"] <- "Stefan, Ilzhöfer"
player_tot_perTeam$player[player_tot_perTeam$player == "Steven Michael, Esterkamp"] <- "Steven, Esterkamp"
player_tot_perTeam$player[player_tot_perTeam$player == "Sven (C), Schultze"] <- "Sven, Schultze"
player_tot_perTeam$player[player_tot_perTeam$player == "Terrel, Harris"] <- "Terrell, Harris"
player_tot_perTeam$player[player_tot_perTeam$player == "Thaddus, McFadden"] <- "Thaddeus, McFadden"
player_tot_perTeam$player[player_tot_perTeam$player == "Till-Joscha, JÃ¶nke"] <- "Till-Joscha, Jönke"
player_tot_perTeam$player[player_tot_perTeam$player == "Trenton (C), Meacham"] <- "Trenton, Meacham"
player_tot_perTeam$player[player_tot_perTeam$player == "Wayne (C), Bernard"] <- "Wayne, Bernard"
player_tot_perTeam$player[player_tot_perTeam$player == "Yorman Polas, Bartolo"] <- "Yorman, Polas Bartolo"

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
play_time$Club[play_time$Club == "Gloria GIANTS DÃ¼sseldorf"] <- "Gloria GIANTS Düsseldorf"
play_time$Club[play_time$Club == "EisbÃ¤ren Bremerhaven"] <- "Eisbären Bremerhaven"
play_time$Club[play_time$Club == "LTi GIESSEN 46ers"] <- "GIESSEN 46ers"
play_time$Club[play_time$Club == "LTi  GIESSEN 46ers"] <- "GIESSEN 46ers"
play_time$Club[play_time$Club == "Deutsche Bank Skyliners"] <- "FRAPORT SKYLINERS"
play_time$Club[play_time$Club == "DEUTSCHE BANK SKYLINERS"] <- "FRAPORT SKYLINERS"
play_time$Club[play_time$Club == "WALTER Tigers TÃ¼bingen"] <- "WALTER Tigers Tübingen"
play_time$Club[play_time$Club == "New Yorker Phantoms Braunschwe"] <- "Basketball Löwen Braunschweig"
play_time$Club[play_time$Club == "Trier"] <- "TBB Trier"
play_time$Club[play_time$Club == "New Yorker Phantoms"] <- "Basketball Löwen Braunschweig"
play_time$Club[play_time$Club == "Alba Berlin"] <- "ALBA BERLIN"
play_time$Club[play_time$Club == "Brose Baskets Bamberg"] <- "Brose Bamberg"
play_time$Club[play_time$Club == "Walter Tigers TÃ¼bingen"] <- "WALTER Tigers Tübingen"
play_time$Club[play_time$Club == "ALBA Berlin"] <- "ALBA BERLIN"
play_time$Club[play_time$Club == "BERLIN"] <- "ALBA BERLIN"
play_time$Club[play_time$Club == "Artland  Dragons"] <- "Artland Dragons"
play_time$Club[play_time$Club == "Atrland Dragons"] <- "Artland Dragons"
play_time$Club[play_time$Club == "BG GÃ¶ttingen"] <- "BG Göttingen"
play_time$Club[play_time$Club == "Bose Baskets"] <- "Brose Bamberg"
play_time$Club[play_time$Club == "Brose Bakets"] <- "Brose Bamberg"
play_time$Club[play_time$Club == "brose baskets"] <- "Brose Bamberg"
play_time$Club[play_time$Club == "Deutsche Bank SKYLINERS"] <- "FRAPORT SKYLINERS"
play_time$Club[play_time$Club == "EWE Baskets"] <- "EWE Baskets Oldenburg"
play_time$Club[play_time$Club == "EWE Baskets Oldenbur"] <- "EWE Baskets Oldenburg"
play_time$Club[play_time$Club == "Ewe Baskets Oldenburg"] <- "EWE Baskets Oldenburg"
play_time$Club[play_time$Club == "EWE BASKETS OLDENBURG"] <- "EWE Baskets Oldenburg"
play_time$Club[play_time$Club == "FC Bayern MÃ¼nchen"] <- "FC Bayern Basketball"
play_time$Club[play_time$Club == "FC Bayern MÃ¼nchen Basketball"] <- "FC Bayern Basketball"
play_time$Club[play_time$Club == "FC BAYERN MÃoNCHEN"] <- "FC Bayern Basketball"
play_time$Club[play_time$Club == "FC Bayern Muenchen"] <- "FC Bayern Basketball"
play_time$Club[play_time$Club == "FC Bayern MÃ¼nchen Basketball"] <- "FC Bayern Basketball"
play_time$Club[play_time$Club == "FC Bayern München"] <- "FC Bayern Basketball"
play_time$Club[play_time$Club == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
play_time$Club[play_time$Club == "FRAPORT Skyliners"] <- "FRAPORT SKYLINERS"
play_time$Club[play_time$Club == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
play_time$Club[play_time$Club == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
play_time$Club[play_time$Club == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
play_time$Club[play_time$Club == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
play_time$Club[play_time$Club == "Fraport Skyliners"] <- "FRAPORT SKYLINERS"
play_time$Club[play_time$Club == "FC Bayern MÃ¼nchen Basketball"] <- "FC Bayern Basketball"
play_time$Club[play_time$Club == "ENBW Ludwigsburg"] <- "EnBW Ludwigsburg"
play_time$Club[play_time$Club == "ENBW Ludwigsburg"] <- "EnBW Ludwigsburg"
play_time$Club[play_time$Club == "BG GÃ¶ttingen"] <- "BG Göttingen"
play_time$Club[play_time$Club == "Gloria GIANTS"] <- "Gloria GIANTS Düsseldorf"
play_time$Club[play_time$Club == "Gloria Giants DÃ¼sseldorf"] <- "Gloria GIANTS Düsseldorf"
play_time$Club[play_time$Club == "LTI GieÃYen"] <- "GIESSEN 46ers"
play_time$Club[play_time$Club == "LTi GieÃYen 46ers"] <- "GIESSEN 46ers"
play_time$Club[play_time$Club == "LTi Giessen 46ers"] <- "GIESSEN 46ers"
play_time$Club[play_time$Club == "Ludwigsburg"] <- "MHP Ludwigsburg"
play_time$Club[play_time$Club == "medi bayreuth"] <- "Medi Bayreuth"
play_time$Club[play_time$Club == "medi Bayreuth"] <- "Medi Bayreuth"
play_time$Club[play_time$Club == "MHP RIESEN Ludswigsburg"] <- "MHP Ludwigsburg"
play_time$Club[play_time$Club == "MHP Riesen Ludwigsburg"] <- "MHP Ludwigsburg"
play_time$Club[play_time$Club == "MHP RIESEN Ludwigsburg"] <- "MHP Ludwigsburg"
play_time$Club[play_time$Club == "MHP Riesen Ludwigsburg 12"] <- "MHP Ludwigsburg"
play_time$Club[play_time$Club == "Mitteldeutscher Baketball Club"] <- "Mitteldeutscher BC"
play_time$Club[play_time$Club == "Mitteldeutscher Basketball"] <- "Mitteldeutscher BC"
play_time$Club[play_time$Club == "Mitteldeutscher Basketball Clu"] <- "Mitteldeutscher BC"
play_time$Club[play_time$Club == "MPH RIESEN Ludwigsburg"] <- "MHP Ludwigsburg"
play_time$Club[play_time$Club == "Neckar Riesen Ludwigsburg"] <- "MHP Ludwigsburg"
play_time$Club[play_time$Club == "Neckar RIESEN Ludwigsburg"] <- "MHP Ludwigsburg"
play_time$Club[play_time$Club == "NECKAR RIESEN Ludwigsburg"] <- "MHP Ludwigsburg"
play_time$Club[play_time$Club == "New Yoker Phantoms Braunschwei"] <- "Basketball Löwen Braunschweig"
play_time$Club[play_time$Club == "New York Phantoms Braunschweig"] <- "Basketball Löwen Braunschweig"
play_time$Club[play_time$Club == "New Yorker Phantoms BRAUNSCHWE"] <- "Basketball Löwen Braunschweig"
play_time$Club[play_time$Club == "NEW YORKER PHANTOMS"] <- "Basketball Löwen Braunschweig"
play_time$Club[play_time$Club == "NY Phantoms Braunschweig"] <- "Basketball Löwen Braunschweig"
play_time$Club[play_time$Club == "PhÃ¶nix Hagen"] <- "Phoenix Hagen"
play_time$Club[play_time$Club == "PHOENIX HAGEN"] <- "Phoenix Hagen"
play_time$Club[play_time$Club == "Poenix Hagen"] <- "Phoenix Hagen"
play_time$Club[play_time$Club == "rathiopharm ulm"] <- "Ratiopharm Ulm"
play_time$Club[play_time$Club == "rathiopharm Ulm"] <- "Ratiopharm Ulm"
play_time$Club[play_time$Club == "ratiopharm  Ulm"] <- "Ratiopharm Ulm"
play_time$Club[play_time$Club == "ratiopharm ulm"] <- "Ratiopharm Ulm"
play_time$Club[play_time$Club == "ratiopharm Ulm"] <- "Ratiopharm Ulm"
play_time$Club[play_time$Club == "s. Oliver Baskets"] <- "s.Oliver Würzburg"
play_time$Club[play_time$Club == "s.Oliver Baskets WÃ¼rzburg"] <- "s.Oliver Würzburg"
play_time$Club[play_time$Club == "SC Rasta Vechta"] <- "RASTA Vechta"
play_time$Club[play_time$Club == "SC RASTA Vechta"] <- "RASTA Vechta"
play_time$Club[play_time$Club == "Telekom Baskets Bonne"] <- "Telekom Baskets Bonn"
play_time$Club[play_time$Club == "EnBW Ludwgsburg"] <- "EnBW Ludwigsburg"
play_time$Club[play_time$Club == "EnBW Ludwigsurg"] <- "EnBW Ludwigsburg"
play_time$Club[play_time$Club == "EnBW Ludwigsburg"] <- "MHP Ludwigsburg"
unique(play_time$Club)

play_time$player[play_time$player == "Acha, Njei"] <- "Acha, Njej"
play_time$player[play_time$player == "Achmadscha, Zazai"] <- "Achmadschah, Zazai"
play_time$player[play_time$player == "Albert King, Nolan"] <- "Albert King, Nolen"
play_time$player[play_time$player == "Ali, TraorÃ©"] <- "Ali, Traore"
play_time$player[play_time$player == "Andreas, BÃ¼chert"] <- "Andreas, Büchert"
play_time$player[play_time$player == "Aziz, N#Diaye"] <- "Aziz, N'Diaye"
play_time$player[play_time$player == "Aziz, NDiaye"] <- "Aziz, N'Diaye"
play_time$player[play_time$player == "Bazoumana, Kante"] <- "Bazoumana, Kone"
play_time$player[play_time$player == "Brandon Kyle, Bowman"] <- "Brandon, Bowman"
play_time$player[play_time$player == "Byron, Allen"] <- "Bryon, Allen"
play_time$player[play_time$player == "Casey (C), Jacobsen"] <- "Casey, Jacobsen"
play_time$player[play_time$player == "Christian, M#Baidanoum"] <- "Christian, M'Baidanoum"
play_time$player[play_time$player == "D#Or, Fischer"] <- "D'Or, Fischer"
play_time$player[play_time$player == "Darren (C), Fenn"] <- "Darren, Fenn"
play_time$player[play_time$player == "Dashaun, Wood"] <- "DaShaun, Wood"
play_time$player[play_time$player == "DaShaun (C), Wood"] <- "DaShaun, Wood"
play_time$player[play_time$player == "David Johnelle, Kennedy"] <- "David John, Kennedy"
play_time$player[play_time$player == "DeAndre (C), Haynes"] <- "DeAndre, Haynes"
play_time$player[play_time$player == "Derick, Allen"] <- "Derrick, Allen"
play_time$player[play_time$player == "Dirk, MÃ¤drich"] <- "Dirk, Mädrich"
play_time$player[play_time$player == "Dominik, Lockhardt"] <- "Dominic, Lockhart"
play_time$player[play_time$player == "Dragan (C), Dojcin"] <- "Dragan, Dojcin"
play_time$player[play_time$player == "Ekenechukwu, Ibekwe"] <- "Ekene, Ibekwe"
play_time$player[play_time$player == "Elvir (C), Ovcina"] <- "Elvir, Ovcina"
play_time$player[play_time$player == "Flavio, StÃ¼ckemann"] <- "Flavio, Stückemann"
play_time$player[play_time$player == "Greg-Emeka, Onwuegbuzie"] <- "Greg-Emeka, Onwuegbuze"
play_time$player[play_time$player == "Guido, GrÃ¼nheid"] <- "Guido, Grünheid"
play_time$player[play_time$player == "Heiko, Schaffratzik"] <- "Heiko, Schaffartzik"
play_time$player[play_time$player == "Jared (C), Reiner"] <- "Jared, Reiner"
play_time$player[play_time$player == "Jason Gregory, Boone"] <- "Jason, Boone"
play_time$player[play_time$player == "Jay (C), Thomas"] <- "Jay, Thomas"
play_time$player[play_time$player == "Jeremiah William, Davis"] <- "Jeremiah, Davis"
play_time$player[play_time$player == "Jerome, Tillman"] <- "Jerome, Tillmann"
play_time$player[play_time$player == "Jerry -CAP-, Green"] <- "Jerry, Green"
play_time$player[play_time$player == "Julius (C), Jenkins"] <- "Julius, Jenkins"
play_time$player[play_time$player == "Kenneth, Williams"] <- "Kenneth, Wiliams"
play_time$player[play_time$player == "Laquan, Prowell"] <- "LaQuan, Prowell"
play_time$player[play_time$player == "Larry D, Wright"] <- "Larry, Wright"
play_time$player[play_time$player == "Louis (C), Campbell"] <- "Louis, Campbell"
play_time$player[play_time$player == "Mathis, MÃ¶nninghoff"] <- "Mathis, Mönninghoff"
play_time$player[play_time$player == "Nils (C), Mittmann"] <- "Nils, Mittmann"
play_time$player[play_time$player == "Oskar, FaÃYler"] <- "Oskar, Fassler"
play_time$player[play_time$player == "Pascal (C), Roller"] <- "Pascal, Roller"
play_time$player[play_time$player == "Patrick (C), Femerling"] <- "Patrick, Femerling"
play_time$player[play_time$player == "Patrick -CAP-, Femerling"] <- "Patrick, Femerling"
play_time$player[play_time$player == "Per, GÃ¼nther"] <- "Per, Günther"
play_time$player[play_time$player == "qu, Robertson"] <- "Quantez, Robertson"
play_time$player[play_time$player == "Quentin (C), Pryor"] <- "Quentin, Pryor"
play_time$player[play_time$player == "Rickey (C), Paulding"] <- "Rickey, Paulding"
play_time$player[play_time$player == "Robin, PflÃ¼ger"] <- "Robin, Pflüger"
play_time$player[play_time$player == "Robin, PflÃ¼gler"] <- "Robin, Pflüger"
play_time$player[play_time$player == "Stefan, IlzhÃ¶fer"] <- "Stefan, Ilzhöfer"
play_time$player[play_time$player == "Steven Michael, Esterkamp"] <- "Steven, Esterkamp"
play_time$player[play_time$player == "Sven (C), Schultze"] <- "Sven, Schultze"
play_time$player[play_time$player == "Terrel, Harris"] <- "Terrell, Harris"
play_time$player[play_time$player == "Thaddus, McFadden"] <- "Thaddeus, McFadden"
play_time$player[play_time$player == "Till-Joscha, JÃ¶nke"] <- "Till-Joscha, Jönke"
play_time$player[play_time$player == "Trenton (C), Meacham"] <- "Trenton, Meacham"
play_time$player[play_time$player == "Wayne (C), Bernard"] <- "Wayne, Bernard"
play_time$player[play_time$player == "Yorman Polas, Bartolo"] <- "Yorman, Polas Bartolo"


games_played  <- play_time %>%              # Count rows by group
    group_by(player,Club,year) %>% 
    summarise(G = n(), .groups ="drop")

play_time1 <- play_time %>% 
    mutate_at("sec_total", ~replace(., is.na(.), 0))

play_time1$sec_total[play_time1$player == "Lucas, Gertz" & play_time1$game_nr == 17276] <- 135
play_time1$sec_total[play_time1$player == "Jordon, Crawford" & play_time1$game_nr == 22132] <- 2589
play_time1$sec_total[play_time1$player == "David, Godbold" & play_time1$game_nr == 18412] <- 715
play_time1$sec_total[play_time1$player == "Andrew, Rautins" & play_time1$game_nr == 15397] <- 3024
play_time1$sec_total[play_time1$player == "Albert Jay, English" & play_time1$game_nr == 19817] <- 2340 + 52

mhm <- play_time1 %>% 
    mutate(laber = ifelse(start_time == end_time & sec_total == 0,0,1))

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
# Merge box score & playing time:
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

player_info2010 <- pos_cm_kg(2010)
# save player info:
saveRDS(object = player_info2010, file = paste0("Data/player_info/player_info2010",".Rds"))

player_info2011 <- pos_cm_kg(2011)
# save player info:
saveRDS(object = player_info2011, file = paste0("Data/player_info/player_info2011",".Rds"))

player_info2012 <- pos_cm_kg(2012)
# save player info:
saveRDS(object = player_info2012, file = paste0("Data/player_info/player_info2012",".Rds"))

player_info2013 <- pos_cm_kg(2013)
# save player info:
saveRDS(object = player_info2013, file = paste0("Data/player_info/player_info2013",".Rds"))

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

player_info1 <- readRDS("Data/player_info/player_info2015.Rds")
player_info2 <- readRDS("Data/player_info/player_info2016.Rds")
player_info3 <- readRDS("Data/player_info/player_info2017.Rds")
player_info4 <- readRDS("Data/player_info/player_info2018.Rds")
player_info5 <- readRDS("Data/player_info/player_info2014.Rds")
player_info6 <- readRDS("Data/player_info/player_info2019.Rds")
player_info2020 <- readRDS("Data/player_info/player_info2020.Rds")

player_info <- bind_rows(player_info2010,player_info2011,player_info2012,player_info2013,
                         player_info5, player_info1, player_info2, player_info3,
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

player_info$player[player_info$player == "Alassane, Dioubaté"] <- "Alassane, Dioubate"
player_info$player[player_info$player == "Kai, Lagemann"] <- "Christian, Lagemann"

player_info$player[player_info$player == "Acha, Njei"] <- "Acha, Njej"
player_info$player[player_info$player == "Achmadscha, Zazai"] <- "Achmadschah, Zazai"
player_info$player[player_info$player == "Albert King, Nolan"] <- "Albert King, Nolen"
player_info$player[player_info$player == "Ali, TraorÃ©"] <- "Ali, Traore"
player_info$player[player_info$player == "Andreas, BÃ¼chert"] <- "Andreas, Büchert"
player_info$player[player_info$player == "Aziz, N#Diaye"] <- "Aziz, N'Diaye"
player_info$player[player_info$player == "Aziz, NDiaye"] <- "Aziz, N'Diaye"
player_info$player[player_info$player == "Bazoumana, Kante"] <- "Bazoumana, Kone"
player_info$player[player_info$player == "Brandon Kyle, Bowman"] <- "Brandon, Bowman"
player_info$player[player_info$player == "Byron, Allen"] <- "Bryon, Allen"
player_info$player[player_info$player == "Casey (C), Jacobsen"] <- "Casey, Jacobsen"
player_info$player[player_info$player == "Christian, M#Baidanoum"] <- "Christian, M'Baidanoum"
player_info$player[player_info$player == "D#Or, Fischer"] <- "D'Or, Fischer"
player_info$player[player_info$player == "Darren (C), Fenn"] <- "Darren, Fenn"
player_info$player[player_info$player == "Dashaun, Wood"] <- "DaShaun, Wood"
player_info$player[player_info$player == "DaShaun (C), Wood"] <- "DaShaun, Wood"
player_info$player[player_info$player == "David Johnelle, Kennedy"] <- "David John, Kennedy"
player_info$player[player_info$player == "DeAndre (C), Haynes"] <- "DeAndre, Haynes"
player_info$player[player_info$player == "Derick, Allen"] <- "Derrick, Allen"
player_info$player[player_info$player == "Dirk, MÃ¤drich"] <- "Dirk, Mädrich"
player_info$player[player_info$player == "Dominik, Lockhardt"] <- "Dominic, Lockhart"
player_info$player[player_info$player == "Dragan (C), Dojcin"] <- "Dragan, Dojcin"
player_info$player[player_info$player == "Ekenechukwu, Ibekwe"] <- "Ekene, Ibekwe"
player_info$player[player_info$player == "Elvir (C), Ovcina"] <- "Elvir, Ovcina"
player_info$player[player_info$player == "Flavio, StÃ¼ckemann"] <- "Flavio, Stückemann"
player_info$player[player_info$player == "Greg-Emeka, Onwuegbuzie"] <- "Greg-Emeka, Onwuegbuze"
player_info$player[player_info$player == "Guido, GrÃ¼nheid"] <- "Guido, Grünheid"
player_info$player[player_info$player == "Heiko, Schaffratzik"] <- "Heiko, Schaffartzik"
player_info$player[player_info$player == "Jared (C), Reiner"] <- "Jared, Reiner"
player_info$player[player_info$player == "Jason Gregory, Boone"] <- "Jason, Boone"
player_info$player[player_info$player == "Jay (C), Thomas"] <- "Jay, Thomas"
player_info$player[player_info$player == "Jeremiah William, Davis"] <- "Jeremiah, Davis"
player_info$player[player_info$player == "Jerome, Tillman"] <- "Jerome, Tillmann"
player_info$player[player_info$player == "Jerry -CAP-, Green"] <- "Jerry, Green"
player_info$player[player_info$player == "Julius (C), Jenkins"] <- "Julius, Jenkins"
player_info$player[player_info$player == "Kenneth, Williams"] <- "Kenneth, Wiliams"
player_info$player[player_info$player == "Laquan, Prowell"] <- "LaQuan, Prowell"
player_info$player[player_info$player == "Larry D, Wright"] <- "Larry, Wright"
player_info$player[player_info$player == "Louis (C), Campbell"] <- "Louis, Campbell"
player_info$player[player_info$player == "Mathis, MÃ¶nninghoff"] <- "Mathis, Mönninghoff"
player_info$player[player_info$player == "Nils (C), Mittmann"] <- "Nils, Mittmann"
player_info$player[player_info$player == "Oskar, FaÃYler"] <- "Oskar, Fassler"
player_info$player[player_info$player == "Pascal (C), Roller"] <- "Pascal, Roller"
player_info$player[player_info$player == "Patrick (C), Femerling"] <- "Patrick, Femerling"
player_info$player[player_info$player == "Patrick -CAP-, Femerling"] <- "Patrick, Femerling"
player_info$player[player_info$player == "Per, GÃ¼nther"] <- "Per, Günther"
player_info$player[player_info$player == "qu, Robertson"] <- "Quantez, Robertson"
player_info$player[player_info$player == "Quentin (C), Pryor"] <- "Quentin, Pryor"
player_info$player[player_info$player == "Rickey (C), Paulding"] <- "Rickey, Paulding"
player_info$player[player_info$player == "Robin, PflÃ¼ger"] <- "Robin, Pflüger"
player_info$player[player_info$player == "Robin, PflÃ¼gler"] <- "Robin, Pflüger"
player_info$player[player_info$player == "Stefan, IlzhÃ¶fer"] <- "Stefan, Ilzhöfer"
player_info$player[player_info$player == "Steven Michael, Esterkamp"] <- "Steven, Esterkamp"
player_info$player[player_info$player == "Sven (C), Schultze"] <- "Sven, Schultze"
player_info$player[player_info$player == "Terrel, Harris"] <- "Terrell, Harris"
player_info$player[player_info$player == "Thaddus, McFadden"] <- "Thaddeus, McFadden"
player_info$player[player_info$player == "Till-Joscha, JÃ¶nke"] <- "Till-Joscha, Jönke"
player_info$player[player_info$player == "Trenton (C), Meacham"] <- "Trenton, Meacham"
player_info$player[player_info$player == "Wayne (C), Bernard"] <- "Wayne, Bernard"
player_info$player[player_info$player == "Yorman Polas, Bartolo"] <- "Yorman, Polas Bartolo"

# check if the !same! player has different entries 
z <- player_info %>%
    arrange(player)

c <- player_info %>%
    arrange(player) %>%
    distinct(.,player, .keep_all = TRUE)

a <- c %>%
    arrange(player) %>%
    dplyr::select(-id, -player,-year, -Alter, -`Letzter Verein`, -left_team,- not_played, -Nr., -img.url)
b <- a %>%
    #group_by(year) %>%
    base::duplicated()

d <- a %>%
    #group_by(year) %>%
    base::duplicated(.,fromLast =TRUE)
zz <- c
zz$dup <- as.integer(b)
zz$dup2 <- as.integer(d)

zz <- zz %>%
    filter(dup == 1 | dup2 == 1)

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
    mutate(min_p = round(Sum_sec /60,1),
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