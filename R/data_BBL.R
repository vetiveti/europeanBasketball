# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

setwd("~/Uni Tübingen/0. Masterarbeit/7. R/Masterarbeit")
source('~/Uni Tübingen/0. Masterarbeit/7. R/Masterarbeit/functions/download_and_prep_functions.R')
source('~/Uni Tübingen/0. Masterarbeit/7. R/Masterarbeit/functions/BBL_functions.R')

library(RCurl)
library(XML)
library(stringr)
library(RSelenium)
library(tidyverse)
library(rJava)
library(rvest)
library(glue)
library(rjson)
library(lubridate)

remDr <- rsDriver(verbose = T,
                  remoteServerAddr = "localhost",
                  port = 4444L,
                  browser=c("firefox"))

rm <- remDr$client
rm$getStatus()

rm$navigate("https://www.easycredit-bbl.de/de/saison/spielplan/alle-spiele/") 

base <- "//select[@id='saison']/option[@value="
year = 2008:2018

c <- "//select[@id='saison']/option[@value='2017']"
year_id <- NULL
for(i in 1:length(year)) {
year_id[i] <- glue('{base}', "\'",
          year[i],"\']") %>% as.character()
}

for (i in 1:length(year_id)) {
####
    option_season <- rm$findElement(using = 'xpath', year_id[i])
    option_season$clickElement()
    
    option_contest <- rm$findElement(using = 'xpath', "//select[@id='wettbewerb']/option[@value='1']")
    option_contest$clickElement()
    
    button_filter <- rm$findElement(using = 'xpath', "//input[@class = 'btn'][@type = 'submit'][@value = 'Filter übernehmen']")
    button_filter$sendKeysToElement(list("R Cran", key = "enter"))
    Sys.sleep(5)
    
    
    
    page <- unlist(rm$getPageSource())
    tpage <- htmlParse(page)
    #games <- try(xpathSApply(tpage, "//a[@class='icon boxscore']", xmlAttrs))[3,]
    #
    
    page <- page %>%
        readr::read_lines() %>%
        str_replace_all("<!--|-->", "") %>%
        str_trim() %>%
        stringi::stri_trans_general("Latin-ASCII") %>%
        str_c(collapse = "") %>%
        xml2::read_html()
    
    xml_tables <- page %>%
        rvest::html_nodes(xpath = "//a[@class= 'icon boxscore']")
    games <- xml_tables %>%
        html_attr("href")
    
    identifiers <- c()
    for(index in 1:length(games)){
        page_game <- RCurl::getURL(paste0("https://www.easycredit-bbl.de", games[index]))
        tpage <- htmlParse(page_game)
        game_id <- str_extract(str_extract(page_game,"bekoGameId = '[0-9]{1,8}"), "[0-9]{1,8}")
        home_id <- str_extract(str_extract(page_game,"bekoHomeTeamId = '[0-9]{1,8}"), "[0-9]{1,8}")
        identifiers <- cbind(identifiers, c(game_id, home_id))
    }
    assign(paste0("identifiers_",i,sep =""),identifiers)    
}

id_games2008 <- identifiers1_
id_games2009 <- identifiers2_
id_games2010 <- identifiers3_
id_games2011 <- identifiers4_
id_games2012 <- identifiers5_
id_games2013 <- identifiers6_
id_games2014 <- identifiers7_
id_games2015 <- identifiers8_
id_games2016 <- identifiers9_
id_games2017 <- identifiers10_
id_games2018 <- identifiers11_

save(id_games2008, file ="data/id_games2008.rda")
save(id_games2009, file ="data/id_games2009.rda")
save(id_games2010, file ="data/id_games2010.rda")
save(id_games2011, file ="data/id_games2011.rda")
save(id_games2012, file ="data/id_games2012.rda")
save(id_games2013, file ="data/id_games2013.rda")
save(id_games2014, file ="data/id_games2014.rda")
save(id_games2015, file ="data/id_games2015.rda")
save(id_games2016, file ="data/id_games2016.rda")
save(id_games2017, file ="data/id_games2017.rda")
save(id_games2018, file ="data/id_games2018.rda")


load(file ="data/id_games2008.rda")
load(file ="data/id_games2009.rda")
load(file ="data/id_games2010.rda")
load(file ="data/id_games2011.rda")
load(file ="data/id_games2012.rda")
load(file ="data/id_games2013.rda")
load(file ="data/id_games2014.rda")
load(file ="data/id_games2015.rda")
load(file ="data/id_games2016.rda")
load(file ="data/id_games2017.rda")
load(file ="data/id_games2018.rda")

#' Get play by play data for a specific game

id_2008 <- id_games2008 %>% t %>%  as_tibble() %>% 
    rename(game_id = V1,
           home_id = V2) %>% 
    mutate_if(is.character,as.numeric)

id_2018 <- id_games2018 %>% t %>%  as_tibble() %>% 
    rename(game_id = V1,
           home_id = V2) %>% 
    mutate_if(is.character,as.numeric)
year <- 2018
quarter <- 1
link_4 <- paste("http://live.easycredit-bbl.de/data",year,"/bbl/",id_2018$home_id[1],"/",id_2018$game_id[1],"Q",quarter,".JSN")
link_4 <- gsub(" ", "", link_4)

info_link <- paste("http://live.easycredit-bbl.de/data",year,"/bbl/",id_2018$home_id[1],"/",id_2018$game_id[1],"_INIT.JSN")
info_link <- gsub(" ","", info_link)

json_file1 <- link_4
json_data1 <- fromJSON(paste(readLines(json_file1), collapse=""))
json_data1 <- fromJSON(file=json_file1)


x1 <- length(json_data1$actions)
df1 <- data.frame(matrix(unlist(json_data1), nrow=x1, byrow=T)) %>% 
    rename(teamcode = X1,
           spielzeit = X2,
           sn_Spieler_1 = X3,
           sn_Spieler_2 = X4,
           aktion = X5,
           zusatzinfo_1 = X6,
           zusatzinfo_2 = X7,
           zusatzinfo_3 = X8,
           resultat = X9,
           spielstand_A = X10,
           spielstand_B = X11,
           x_val = X12,
           y_val = X13,
           number_action = X14,
           timestamp = X15)
df1$nummer_aktion <- nrow(df1):1
df1 <- df1 %>% 
    mutate(spielzeit_sec = ms(spielzeit), .keep="unused") %>% 
    relocate(teamcode,spielzeit_sec, everything()) %>% 
    arrange(nummer_aktion)


df1$spielzeit_sec <- as.numeric(df1$spielzeit_sec)


json_file <- info_link
json_test <- fromJSON(paste(readLines(json_file), collapse=""))
json_test <- fromJSON(file=json_file)
team_home <- json_test$teamroster[[1]]$TeamName
team_away <- json_test$teamroster[[2]]$TeamName

df1$team <- 0
df1$contrary_team <- 0
n <- nrow(df1)
index <- 1
while(index < n){
    if(df1$X1[index]=="A"){
        df1$team[index] <- team_home
        df1$contrary_team[index] <- team_away
        
    }
    if(df1$X1[index]=="B"){
        df1$team[index] <- team_away
        df1$contrary_team[index] <- team_home
    }
    
    
    index <- index +1
}

test <- df1

test$game_id <- id_2018$game_id[1]
test$home_id <- id_2018$home_id[1]

test$X1 <- NULL
test$X14 <- NULL
test$Viertel <- quarter

json <- fromJSON(paste(readLines(info_link), collapse=""))
team_a <- json$teamroster[[1]]$TeamName
team_b <- json$teamroster[[2]]$TeamName
kader <- data.frame(matrix(unlist(json$roster), nrow=length(json$roster), byrow=T)) %>% 
    mutate_if(is.numeric,as.numeric)
colnames(kader) <- c("team", "nr", "id","name", "first_name", "status", "position","age", "club","height")
kader <- kader %>% 
    mutate_at(c("nr","id","age","height"),as.numeric) %>% 
    mutate(spieler = paste(first_name, name, sep=", ")) %>% 
    mutate(club = if_else(team == "A",team_a,team_b)) %>% 
    select(c(-name,-first_name,-status))



kader_home <- dplyr::filter(kader, team == "A")
kader_away <- dplyr::filter(kader, team == "B")

df_merge <- merge(df1,kader,
                by.x = c("teamcode","sn_Spieler_1"),
                by.y = c("team", "nr"),
                all.x = TRUE) %>% 
    arrange(nummer_aktion)

df <- merge(df_merge,kader,
            by.x = c("teamcode","sn_Spieler_2"),
            by.y = c("team", "nr"),
            all.x = TRUE) %>%
    arrange(nummer_aktion)

datei_name <- paste(id_2008$game_id[1],team_home,team_away,"_", quarter,".csv")
datei_name <- gsub(" ","", datei_name)
print(datei_name)
#write.table(df, datei_name)


source('~/Uni Tübingen/0. Masterarbeit/7. R/Masterarbeit/functions/BBL_functions.R')
library(tidyverse)
year = 2018:2018


debugonce(BBL_game_ids)
id_year <- BBL_game_ids(year)
