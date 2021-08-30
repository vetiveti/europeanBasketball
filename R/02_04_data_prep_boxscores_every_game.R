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