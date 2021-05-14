# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# source functions to use
source('functions/BBL_functions.R')
source('functions/pbp_actions.R')

library(tidyverse, warn.conflicts = FALSE)
library(zoo)

#a <- BBL_game_ids(year = 2008:2018)
# save_rds <- ls(pattern = "identifiers_")
# saveRDS(object = identifiers_2008, file = paste0("Data/identifiers_2008",".Rds"))
# saveRDS(object = identifiers_2009, file = paste0("Data/identifiers_2009",".Rds"))
# saveRDS(object = identifiers_2010, file = paste0("Data/identifiers_2010",".Rds"))
# saveRDS(object = identifiers_2011, file = paste0("Data/identifiers_2011",".Rds"))
# saveRDS(object = identifiers_2012, file = paste0("Data/identifiers_2012",".Rds"))
# saveRDS(object = identifiers_2013, file = paste0("Data/identifiers_2013",".Rds"))
# saveRDS(object = identifiers_2014, file = paste0("Data/identifiers_2014",".Rds"))
# saveRDS(object = identifiers_2015, file = paste0("Data/identifiers_2015",".Rds"))
# saveRDS(object = identifiers_2016, file = paste0("Data/identifiers_2016",".Rds"))
# saveRDS(object = identifiers_2017, file = paste0("Data/identifiers_2017",".Rds"))
# saveRDS(object = identifiers_2018, file = paste0("Data/identifiers_2018",".Rds"))

# load identifiers:
game_id_files = paste0("Data/identifiers_", 2008:2018, ".Rds")
name <- gsub("\\.Rds$", "", game_id_files) %>% 
    gsub("Data/", "", .)
game_id_data <- lapply(game_id_files, readRDS)
names(game_id_data) <- gsub("\\.Rds$", "", name)

# get results safely, because some games have no data
safer_results <- possibly(get_pbp, otherwise = as_tibble("Error finding file"))

################################################################################
#' start downloading results and saving them
#' do that for every year separately
# year <- 2008
# results <- tibble()
# for (i in 1:nrow(game_id_data$identifiers_2008)) {
#     all_results <- safer_results(year,game_id_data$identifiers_2008[i,])
#     all_results$game_nr <- i
#     results<- bind_rows(results,all_results)
# 
# }
# saveRDS(object = results, file = paste0("Data/pbp",year,".Rds"))


################################################################################
#' download team rosters for every game
# safer_roster <- possibly(get_rosters, otherwise = as_tibble("Error finding file"))
# 
# year <- 2008:2018
# 
# for (j in year) {
#     id_extract <- game_id_data[[paste0("identifiers_",j)]]
#     df_roster <- tibble()
#     
#     for (i in 1:nrow(id_extract)) {
#         current <- safer_roster(id_extract[i,],j)
#         df_roster <- bind_rows(df_roster,current)
#         
#     }
#     assign(paste0("rosters_",j),df_roster)
#     saveRDS(object = get(paste0("rosters_",j)),file = paste0("Data/rosters_",j,".Rds"))
# }




################################################################################
#' load play by play files
pbp_files = paste0("Data/pbp", 2008:2018, ".Rds")
name <- gsub("\\.Rds$", "", pbp_files) %>% 
    gsub("Data/", "", .)
pbp_data <- lapply(pbp_files, readRDS)
names(pbp_data) <- gsub("\\.Rds$", "", name)

################################################################################
# clean pbp data 
# prepare game ids
id <- game_id_data$identifiers_2018

# prepare pbp data
pbp <- pbp_data$pbp2018 %>% 
    #filter(game_nr == 10) %>% 
    arrange(desc(spielzeit_sec )) %>% 
    arrange(game_nr)

id_merge <- bind_cols(id, unique(pbp$game_nr)) %>% 
    rename(nr = `...3`)

pbp <- merge(pbp,id_merge,
             by.x = "game_nr",
             by.y = "nr")

# clean for games which have no files
pbp <- pbp %>%
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")

################################################################################
#' load roster files
roster_files = paste0("Data/rosters_", 2008:2018, ".Rds")
name <- gsub("\\.Rds$", "", roster_files) %>% 
    gsub("Data/", "", .)
roster_data <- lapply(roster_files, readRDS)
names(roster_data) <- gsub("\\.Rds$", "", name)

# prepare roster data
roster <- roster_data$rosters_2018 %>% 
    type_convert()
roster$Club[roster$Club == "SYNTAINICS MBC"] <- "Mitteldeutscher BC"

################################################################################
#' calculate starters
# delete some quarters as starters are not clear
# pbp_starter <- pbp [-c(89409),]
# 
# pbp_starter <- pbp_starter %>% 
#     mutate(game_q = game_id * quarter)
# pbp_starter <- filter(pbp_starter,
#                 game_q != 22072 * 4,
#                 game_q != 22221 * 2,
#                 game_q != 22326 * 4,
#                 game_q != 22211 * 2,
#                 game_q != 22325 * 3,
#                 game_q != 22067 * 5,
#                 game_q != 22067 * 6,
#                 game_q != 22093 * 5,
#                 game_q != 22105 * 5,
#                 game_q != 22132 * 5,
#                 game_q != 22149 * 5,
#                 game_q != 22149 * 5,
#                 game_q != 22187 * 5,
#                 game_q != 22267 * 5,
#                 game_q != 22274 * 1,
#                 game_q != 22290 * 5,
#                 game_q != 22333 * 5,)

roster2018 <- calc_starters(pbp,roster) 

#' starter data is not perfect but I figured out what is wrong in 2018.
#' Have to do that for every year...
#' 
#' Still wrong are players who switch in timeouts! But I do not know how to
#' solve this issue!!!
#' 
#' Anyway the corrections are done by hand and can be found in the excel file.

starters2018 <- roster2018
starters2018$starter_Q4[starters2018$game_nr == 22072 & starters2018$Player == "Benjamin, Lischka"] <- 0
starters2018$starter_Q4[starters2018$game_nr == 22144 & starters2018$Player == "Jason, Clark"] <- 0
starters2018$starter_Q1[starters2018$game_nr == 22274 & starters2018$Player == "Dru, Joyce"] <- 1
starters2018$starter_Q4[starters2018$game_nr == 22326 & grepl("Brooks",starters2018$Player)] <- 1
starters2018$starter_Q3[starters2018$game_nr == 22325 & starters2018$Player == "Elston, Turner"] <- 1


6740 - sum(duplicated(starters2018$game_nr))

sum(starters2018$starter_Q1)
sum(starters2018$starter_Q2)
sum(starters2018$starter_Q3)
sum(starters2018$starter_Q4)
sum(starters2018$starter_Q5, na.rm = TRUE)
sum(starters2018$starter_Q6, na.rm = TRUE)


pbp_game_1 <- filter(pbp_starter,
                     game_q == 22072 * 4)

# source functions to use
source('functions/BBL_functions.R')
source('functions/pbp_actions.R')
solve <- calc_starters(pbp_game_1,roster)
view(solve)
debugonce(calc_starters)
solve2 <- calc_starters(pbp_game_1,roster)

################################################################################
# compute boxscore for teams per game:
#' Am Ende will ich für Spieler und Teams haben
#' =stats per game & stats_totals
un <- unique(pbp$game_id)

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

bx_teams$team[bx_teams$team == "SYNTAINICS MBC"] <- "Mitteldeutscher BC"


team_totals <- bx_teams %>% 
    group_by(team) %>% 
    summarise_at(vars(min:G), .funs = sum)



player_2018 <- distinct(roster, Player, Club, .keep_all = TRUE) %>% 
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")

################################################################################
# compute boxscore for players:
player_totals <- data.frame()
for(i in seq_along(player_2018$Player)) {
    player <- player_2018$Player[i]
    bx_player <- get_boxscore_player(pbp,player)
    bx_player$team <- player_2018$Club[i]
    player_totals <- bind_rows(player_totals, bx_player)
}

a <- unique(roster$Player) %>%  as_tibble()
#' assists are tricky!
#' in the NBA assists are not granted for pass before FT in the FIBA world the count as assists!
#' I compute the NBA style assists as I work with methods which are developed for the NBA


#' still missing...
#' playing time for players
#' auswechslungen und einwechslungen nicht erfasst
#' fucking spielzeit wie kann man das ausrechnen?!





# calculating minutes played.
#' therefore a data frame must be build which tells who is on the court and when
#' this must be done for every single game!

pbp_game <- pbp %>% 
    filter(game_id == 22045)

b <- as_tibble(cbind(nms = names(roster_game), t(roster_game))) %>% 
    

c <- bind_cols(pbp_game,b) %>% 
    filter(quarter == 1)

for (j in 2:nrow(c)) {
    if(c$aktion[j] == "SUBST"){
    }else{
        c[j,c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10")] <- "fill"
    }
}

d <- c %>% 
    filter(aktion == "SUBST")

e <- d %>% 
    mutate_at(vars(starts_with("V")),
              funs(case_when(
                  Player_1 == . ~ Player_2,
                  Player_1 != . ~ "fill"
              )))

f <- bind_rows(c,e) %>% 
    group_by(nummer_aktion, quarter) %>% 
    filter(row_number() == n()) %>% 
    ungroup() %>% 
    arrange(nummer_aktion)


aaa <- unique(pbp$game_id)
for (game in pbp) {
    # calculating minutes played for players and lineups per game
    pbp_game <- pbp %>% 
        filter(game_id == game)
    
    roster_game <- roster2018 %>% 
        filter(game_nr == game)
    
    roster_game <- roster_game %>% 
        select(Player, starter_Q1:starter_Q4) %>% 
        mutate(Q1 = if_else(starter_Q1 == 1,Player,"0"),
               Q2 = if_else(starter_Q2 == 1,Player,"0"),
               Q3 = if_else(starter_Q3 == 1,Player,"0"),
               Q4 = if_else(starter_Q4 == 1,Player,"0"))
    
    
    pbp_merg <- tibble()
    for(viertel in 1:10){
        assign(paste0("quarter_",viertel),subset(pbp_game, quarter == viertel))
        if(nrow(subset(pbp_game, quarter == viertel)) == 0){break}
        
        b <- roster_game %>% 
            select(paste0("Q",viertel)) %>% 
            filter(. != "0") %>% 
            t(.) %>% 
            as_tibble()
        
        c <- bind_cols(pbp_game,b) %>% 
            filter(quarter == viertel)
        
        for (j in 2:nrow(c)) {
            if(c$aktion[j] == "SUBST"){
            }else{
                c[j,c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10")] <- NA
            }
        }
        
        
        d <- c %>% 
            filter(aktion == "SUBST" | aktion == "START")
        
        for (i in 2:nrow(d)) {
            d[i,"V1"] <- ifelse(d$Player_1[i] != d$V1[i-1] ,d[i-1,"V1"], d$Player_2[i])
            d[i,"V2"] <- ifelse(d$Player_1[i] != d$V2[i-1] ,d[i-1,"V2"], d$Player_2[i])
            d[i,"V3"] <- ifelse(d$Player_1[i] != d$V3[i-1] ,d[i-1,"V3"], d$Player_2[i])
            d[i,"V4"] <- ifelse(d$Player_1[i] != d$V4[i-1] ,d[i-1,"V4"], d$Player_2[i])
            d[i,"V5"] <- ifelse(d$Player_1[i] != d$V5[i-1] ,d[i-1,"V5"], d$Player_2[i])
            d[i,"V6"] <- ifelse(d$Player_1[i] != d$V6[i-1] ,d[i-1,"V6"], d$Player_2[i])
            d[i,"V7"] <- ifelse(d$Player_1[i] != d$V7[i-1] ,d[i-1,"V7"], d$Player_2[i])
            d[i,"V8"] <- ifelse(d$Player_1[i] != d$V8[i-1] ,d[i-1,"V8"], d$Player_2[i])
            d[i,"V9"] <- ifelse(d$Player_1[i] != d$V9[i-1] ,d[i-1,"V9"], d$Player_2[i])
            d[i,"V10"] <- ifelse(d$Player_1[i] != d$V10[i-1] ,d[i-1,"V10"], d$Player_2[i])
        }
        
        data_new <- d[- 1, ]
        
        
        
        f <- bind_rows(c,data_new) %>%
            group_by(nummer_aktion, quarter) %>%
            filter(row_number() == n()) %>%
            ungroup() %>%
            arrange(nummer_aktion)
        if(viertel == 1){
            g <- f   
        }
        
        pbp_merg <- bind_rows(pbp_merg,f)
    }
    
    
    pbp_merge <- na.locf(pbp_merg, na.rm = FALSE)
    
    cols <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10")
    
    eve <- tibble()
    for (ii in 1:10) {
        a <- cols[ii]
        b <- first.changes(pbp_merge[[a]])
        
        eve <- bind_rows(eve,b)
    }
    
    a <- eve %>% 
        mutate(start_time = pbp_merge$spielzeit_sec[eve$start],
               end_time = pbp_merge$spielzeit_sec[eve$end],
               start_q = pbp_merge$quarter[eve$start],
               end_q = pbp_merge$quarter[eve$end])
    
    t_played <- a %>% 
        mutate_at("end_q", ~replace(., is.na(.), 4)) %>% 
        mutate_at("end_time", ~replace(., is.na(.), 0)) %>% 
        mutate(sec = case_when(
            start_q == end_q ~ start_time - end_time,
            start_q != end_q & end_time == 600 ~ start_time - 0,
            start_q == (end_q -1) & end_time != 600 ~ start_time - 0 + 600 - end_time
        ))
    
    total <- sum(t_played$sec) / 60
    
    t_ply <- t_played %>% 
        group_by(player) %>% 
        mutate(sec_total = sum(sec)) %>% 
        ungroup() %>% 
        distinct(player, .keep_all = TRUE) %>% 
        mutate(min_sec_played = seconds_to_period(sec_total))
}




################################################################################
#' download starters for every quarter for every game and team does only work
#' for 2018. Before data is not provided...
#' I think I have to compute the starters manually from the pbp data and use
#' 2018 to check if I do it right
url_starter <- gsub(" ","",paste("http://live.easycredit-bbl.de/data",2018,
                                 "/bbl/",439,
                                 "/",22325,
                                 ".JSN"))
id_extract <- game_id_data[[paste0("identifiers_",2018)]]
url_starter <- gsub(" ","",paste("http://live.easycredit-bbl.de/data",2018,
                                 "/bbl/",id_extract[1,2], "/",id_extract[1,1], ".JSN"))
# Which teams did play?
json_starter <- get_json(url_starter)

df <- json_starter$statind %>% 
    as_tibble() %>% 
    rename(teamcode = V1,
           spielcode = V2,
           spieler_nummer = V3,
           pts = V4,
           ftm = V5,
           fta = V6,
           ft_pct = V7,
           p2m = V8,
           p2a = V9,
           p2_pct = V10,
           p3m = V11,
           p3a = V12,
           p3_pct = V13,
           pf = V14,
           trb = V15,
           ast = V16,
           blk = V17,
           stl = V18,
           tov = V19,
           index1 = V20,
           index2 = V21,
           min_mmss = V22,
           min_mm = V23,
           drb = V24,
           orb = V25,
           starter_1 = V26,
           starter_2 = V27,
           starter_3 = V28,
           starter_4 = V29) %>% 
    type_convert()
df$game_nr <- json_starter$Game
