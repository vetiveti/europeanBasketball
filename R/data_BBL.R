# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# source functions to use
source('functions/BBL_functions.R')
source('functions/pbp_actions.R')

library(tidyverse)


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

my_files = paste0("Data/identifiers_", 2008:2018, ".Rds")
name <- gsub("\\.Rds$", "", my_files) %>% 
    gsub("Data/", "", .)
my_data <- lapply(my_files, readRDS)
names(my_data) <- gsub("\\.Rds$", "", name)


year <- 2018
pbp_2018<- data.frame()
for(i in 1:1){
    game_nr <- i
    game_id <- id_2018[game_nr,]
    pbp <- get_pbp(year,game_id)
    pbp$game_nr <- i
    
    pbp_2018 <- bind_rows(pbp_2018,pbp)
}

safer_results <- possibly(get_pbp, otherwise = as_data_frame("Error finding file"))

results <- tibble()
year <- 2008
for (i in 1:nrow(my_data$identifiers_2008)) {
    all_results <- safer_results(year,my_data$identifiers_2008[i,])
    all_results$game_nr <- i
    results<- bind_rows(results,all_results)
    
}
saveRDS(object = results, file = paste0("Data/pbp",year,".Rds"))


my_files = paste0("Data/pbp", 2008:2018, ".Rds")
name <- gsub("\\.Rds$", "", my_files) %>% 
    gsub("Data/", "", .)
my_data <- lapply(my_files, readRDS)
names(my_data) <- gsub("\\.Rds$", "", name)

# compute boxscore for teams:
team_h <- home_team
team_a <- away_team
bx_team <- get_boxscore_team(pbp,team_h,team_a)

# compute boxscore for players:
bx_players <- data.frame()
for(i in 1:12) {
    player <- roster$Player[i]
    bx_player <- get_boxscore_player(pbp,player)
    bx_players <- bind_rows(bx_players, bx_player)
}

#' assists are tricky!
#' in the NBA assists are not granted for pass before FT in the FIBA world the count as assists!
#' I compute the NBA style assists as I work with methods which are developed for the NBA


#' still missing...
#' playing time for players
#' auswechslungen und einwechslungen nicht erfasst




url_starter <- gsub(" ","",paste("http://live.easycredit-bbl.de/data",year,
                              "/bbl/",game_id$home_id,
                              "/",game_id$game_id,
                              ".JSN"))

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
sum(df$starter_1)
sum(df$starter_2)
sum(df$starter_3)
sum(df$starter_4)

con <- "google.com"
check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
suppressWarnings(try(close.connection(con),silent=T))
ifelse(is.null(check),1,0)


game_id <- id_2018[68,]
url_info <- gsub(" ","",paste("http://live.easycredit-bbl.de/data",year,
                              "/bbl/",game_id$home_id,
                              "/",game_id$game_id,
                              "_INIT.JSN"))

# Which teams did play?
json_info <- get_json(url_info)

teams <- json_info$teamroster
team_h <- teams$TeamName[1]
team_a <- teams$TeamName[2]

assign('home_team', team_h, envir =.GlobalEnv)
assign('away_team', team_a, envir =.GlobalEnv)
