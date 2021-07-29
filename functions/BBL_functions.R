#' get BBL game ID's ####
BBL_game_ids <- function(year){
    #' start R Selenium
    remDr <- RSelenium::rsDriver(verbose = T,
                                 remoteServerAddr = "localhost",
                                 port = 4444L,
                                 browser=c("firefox"))
    rm <- remDr$client
    rm$navigate("https://www.easycredit-bbl.de/de/saison/spielplan/alle-spiele/") 
    
    base <- "//select[@id='saison']/option[@value="
    
    year_id <- NULL
    for(i in 1:length(year)) {
        year_id[i] <- glue::glue('{base}', "\'",
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
        tpage <- XML::htmlParse(page)

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
            rvest::html_attr("href")
        
        identifiers <- c()
        for(index in 1:length(games)){
            page_game <- RCurl::getURL(paste0("https://www.easycredit-bbl.de", games[index]))
            tpage <- XML::htmlParse(page_game)
            game_id <- str_extract(str_extract(page_game,"bekoGameId = '[0-9]{1,8}"), "[0-9]{1,8}")
            home_id <- str_extract(str_extract(page_game,"bekoHomeTeamId = '[0-9]{1,8}"), "[0-9]{1,8}")
            identifiers <- cbind(identifiers, c(game_id, home_id))
        }
        
        identifiers<- identifiers %>% t %>%  as_tibble() %>% 
            rename(game_id = V1,
                   home_id = V2) %>% 
            mutate_if(is.character,as.numeric)
        
        assign(paste0("identifiers_",year[i],sep =""),identifiers,envir =.GlobalEnv) 
    }
    
    rm$close()
    # stop the selenium server
    remDr$server$stop()
    base::rm(remDr)
    gc()
    system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
    
    return(year_id)
}

#' get JSON from BBL
get_json <- function(url) {
    res <- httr::GET(url)
    
    json <- res$content %>% 
        rawToChar() %>%
        jsonlite::fromJSON(simplifyVector = T)
    
}

#' check if urls are valid
valid_url <- function(url_in,t=2){
    con <- url(url_in)
    check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
    suppressWarnings(try(close.connection(con),silent=T))
    ifelse(is.null(check),1,0)
}

#' get BBL PbP ####
get_pbp <- function(year, game_id){
    #' Get play by play data for a specific game
    # Get Team info's
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
    
    # Roster (who played?)
    roster <- json_info$roster %>% 
        data.frame %>%
        as_tibble() %>%
        mutate(Player = paste(FirstName, Name, sep=", "), .keep = "unused") %>%
        mutate(Club = if_else(TC == "A",team_h,team_a)) %>% 
        mutate(Pos = Posshort, .keep ="unused") %>% 
        relocate(Player, Pos, .before = Is)
    
    assign('roster',roster, envir=.GlobalEnv)
    
    roster_h <- filter(roster, TC =="A")
    roster_a <- filter(roster, TC =="B")
    
    roster_merge <- roster %>% 
        select(TC, Nr, Player, Club)
    
    
    # get PbP for each quarter played
    # check how many quarters are played:
    exists <- vector()
    overtime_url <- vector()
    for (i in 1:6) {
        urls <- gsub(" ","",paste("http://live.easycredit-bbl.de/data",year,
                                     "/bbl/",game_id$home_id,
                                     "/",game_id$game_id,
                                     "X", i,
                                     ".JSN"))
        exists[i] <- RCurl::url.exists(urls)
        overtime_url[i] <- urls
    }
    
    overtime_played <- sum(exists)
    
    
    
    pbp <- data.frame()
    for(i in 1:4){
        quarter <- i
        url_pbp <- gsub(" ","",paste("http://live.easycredit-bbl.de/data",year,
                                     "/bbl/",game_id$home_id,
                                     "/",game_id$game_id,
                                     "Q", quarter,
                                     ".JSN"))

        json_pbp <- get_json(url_pbp)
        
        pbp_quarter <- json_pbp %>% 
            data.frame %>%
            as_tibble()
        if (year > 2013) {
        pbp_quarter <- pbp_quarter %>% 
            rename(teamcode = actions.1,
                   spielzeit = actions.2,
                   sn_Spieler_1 = actions.3,
                   sn_Spieler_2 = actions.4,
                   aktion = actions.5,
                   zusatzinfo_1 = actions.6,
                   zusatzinfo_2 = actions.7,
                   zusatzinfo_3 = actions.8,
                   resultat = actions.9,
                   spielstand_A = actions.10,
                   spielstand_B = actions.11,
                   x_val = actions.12,
                   y_val = actions.13,
                   number_action = actions.14)
        
        if (year > 2016){
            pbp_quarter <- pbp_quarter %>% 
            rename(timestamp = actions.15)
        }
        } else{
            pbp_quarter <- pbp_quarter %>% 
                rename(teamcode = actions.1,
                       spielzeit = actions.2,
                       sn_Spieler_1 = actions.3,
                       sn_Spieler_2 = actions.4,
                       aktion = actions.5,
                       zusatzinfo_1 = actions.6,
                       zusatzinfo_3 = actions.7,
                       resultat = actions.8,
                       spielstand_A = actions.9,
                       spielstand_B = actions.10,
                       x_val = actions.11,
                       y_val = actions.12,
                       number_action = actions.13)
        }       
        
        pbp_quarter$nummer_aktion <- nrow(pbp_quarter):1
        pbp_quarter$quarter <- quarter
        
        pbp <- bind_rows(pbp,pbp_quarter)
    }
    
    # add overtime if necessary:
    
    if(overtime_played > 0) {
        for(i in 1:6){
            quarter <- i + 4
            url_pbp <- overtime_url[i]
            
            json_pbp <- try(get_json(url_pbp))
            
            if(length(json_pbp$action) == 0 ){
                break
            }
            
            pbp_quarter <- json_pbp %>% 
                data.frame %>%
                as_tibble()
            if (year > 2013) {
                pbp_quarter <- pbp_quarter %>% 
                    rename(teamcode = actions.1,
                           spielzeit = actions.2,
                           sn_Spieler_1 = actions.3,
                           sn_Spieler_2 = actions.4,
                           aktion = actions.5,
                           zusatzinfo_1 = actions.6,
                           zusatzinfo_2 = actions.7,
                           zusatzinfo_3 = actions.8,
                           resultat = actions.9,
                           spielstand_A = actions.10,
                           spielstand_B = actions.11,
                           x_val = actions.12,
                           y_val = actions.13,
                           number_action = actions.14)
                
                if (year > 2016){
                    pbp_quarter <- pbp_quarter %>% 
                        rename(timestamp = actions.15)
                }
            } else{
                pbp_quarter <- pbp_quarter %>% 
                    rename(teamcode = actions.1,
                           spielzeit = actions.2,
                           sn_Spieler_1 = actions.3,
                           sn_Spieler_2 = actions.4,
                           aktion = actions.5,
                           zusatzinfo_1 = actions.6,
                           zusatzinfo_3 = actions.7,
                           resultat = actions.8,
                           spielstand_A = actions.9,
                           spielstand_B = actions.10,
                           x_val = actions.11,
                           y_val = actions.12,
                           number_action = actions.13)
            }       
            
            pbp_quarter$nummer_aktion <- nrow(pbp_quarter):1
            pbp_quarter$quarter <- quarter
            
            pbp <- bind_rows(pbp,pbp_quarter)
        }
    }
    
    
    # change how playing time is stored:
    pbp <- pbp %>% 
        mutate(spielzeit_sec = lubridate::ms(spielzeit), .keep="unused") %>% 
        relocate(teamcode,spielzeit_sec, everything())
    
    pbp$spielzeit_sec <- as.numeric(pbp$spielzeit_sec)
    
    # Merge Roster and PbP to obtain player who did perform the action:
    pbp_merge <- merge(pbp,roster_merge,
                       by.x = c("teamcode","sn_Spieler_1"),
                       by.y = c("TC", "Nr"),
                       all.x = TRUE)
    
    # Do that a second time because of assists
    pbp_merge <- merge(pbp_merge,roster_merge,
                       by.x = c("teamcode","sn_Spieler_2"),
                       by.y = c("TC", "Nr"),
                       all.x = TRUE) %>%
        arrange(quarter,nummer_aktion) %>% 
        mutate(Player_1 = Player.x,
               Club_1 = Club.x,
               Player_2 = Player.y,
               Club_2 = Club.y,
               .keep = "unused") %>% 
        suppressMessages(type_convert())
    
    pbp_merge <- pbp_merge %>% 
        mutate(club_1 = case_when(teamcode=="A" ~team_h,
                                  teamcode=="B" ~team_a))
    
    return(pbp_merge)
}

#' get BBL rosters per game
get_rosters <- function(game_id,year){
    url_info <- gsub(" ","",paste("http://live.easycredit-bbl.de/data",year,
                     "/bbl/",game_id$home_id, "/",game_id$game_id, "_INIT.JSN"))
    
    # Which teams did play?
    json_info <- get_json(url_info)
    
    teams <- json_info$teamroster
    team_h <- teams$TeamName[1]
    team_a <- teams$TeamName[2]
    game_nr <- json_info$Game
    
    # Roster (who played?)
    roster <- json_info$roster %>% 
        data.frame %>%
        as_tibble() 
    roster <- roster %>% 
        mutate(Player = paste(FirstName, Name, sep=", "), .keep = "unused") %>%
        mutate(Club = if_else(TC == "A",team_h,team_a)) %>% 
        mutate(Pos = Posshort, .keep ="unused") %>% 
        relocate(Player, Pos, .before = Is)
    roster$game_nr <- game_nr
    
    return(roster)

}

#' compute starting rosters for each quarter per game and team
calc_starters <- function(pbp_game,roster){
    roster_final <- tibble()
    for (current_game in unique(pbp_game$game_id)) {
        pbp_g <- filter(pbp_game,game_id == current_game)
        roster_g <- filter(roster,game_nr == current_game)
        
        for (current_Q in unique(pbp_g$quarter)) {
            pbp <- filter(pbp_g,quarter == current_Q)
            
            starter <- c()
            for(player in roster_g$Player){
                # subbed out
                sub_out <- nrow(filter(pbp, aktion == "SUBST", Player_1 == player))
                
                act_sub_out<- filter(pbp, aktion == "SUBST", Player_1 == player) %>% 
                    select(nummer_aktion)
                
                # subbed in
                sub_in <- nrow(filter(pbp, aktion == "SUBST", Player_2 == player))
                
                act_sub_in<- filter(pbp, aktion == "SUBST", Player_2 == player) %>% 
                    select(nummer_aktion)
                    
                if(sub_out > 0 && sub_in > 0){
                    
                    if (min(act_sub_out) < min(act_sub_in)){
                        starter <- append(starter,player)
                    }
                    
                } else if(sub_out > 0 && sub_in == 0){
                    starter <- append(starter,player)
                    
                }else if(sub_out == 0 && sub_in == 0){
                    actions <- nrow(filter(pbp, Player_1 == player | Player_2 == player,
                                           nummer_aktion != 2,
                                           aktion != "FOUL"))
                    if(actions > 0){
                        starter <- append(starter,player)
                    }
                }
            }
            
            
            if(length(starter) < 10 ){
                message(paste0("starters missing in game ", current_game, ", quarter ", current_Q))
                
            } else if(length(starter) > 10){
                message(paste0("to many starters in game ", current_game, ", quarter ", current_Q))
            }
            
            quarter <- current_Q
            roster_g <- roster_g %>% 
                mutate("starter_Q{quarter}" := if_else((roster_g$Player %in% starter) == TRUE,1,0))
            
        }
        roster_final <- bind_rows(roster_final,roster_g)
    }
    return(roster_final)
}

#' download player position age weight and height
pos_cm_kg <- function(year){
    #' start R Selenium
    remDr <- RSelenium::rsDriver(verbose = T,
                                 remoteServerAddr = "localhost",
                                 port = 4444L,
                                 browser=c("firefox"))
    rm <- remDr$client
    rm$navigate("https://www.easycredit-bbl.de/de/saison/tabelle/gesamt/") 
    Sys.sleep(2)
    
    base <- "//select[@id='saison']/option[@value="
    year_id <- glue::glue('{base}', "\'",
                          year,"\']") %>% as.character()
    
    option_season <- rm$findElement(using = 'xpath', year_id)
    option_season$clickElement()
    
    page <- unlist(rm$getPageSource())
    
    page <- page %>%
        readr::read_lines() %>%
        str_replace_all("<!--|-->", "") %>%
        str_trim() %>%
        stringi::stri_trans_general("ASCII-Latin") %>%
        str_c(collapse = "") %>%
        xml2::read_html()
    
    xml_tables <- page %>%
        rvest::html_nodes(css = "[href*='/de/easycredit-bbl/historie/teams/t/']")
    
    team_urls <- xml_tables %>%
        rvest::html_attr("href")
    
    team_urls<- unique(team_urls)
    
    b <- paste0(year,"-",year+1)
    a <- grepl(b,team_urls)
    team_urls <- team_urls[a]
    Sys.sleep(2)
    
    all_team_rosters <- tibble()
    for (i in 1:length(team_urls)) {
        current_team_url <- paste0("https://www.easycredit-bbl.de",team_urls[i])
        rm$navigate(current_team_url)
        Sys.sleep(2)
        
        page <- unlist(rm$getPageSource())
        page <- page %>%
            readr::read_lines() %>%
            str_replace_all("<!--|-->", "") %>%
            str_trim() %>%
            stringi::stri_trans_general("ASCII-de") %>%
            str_c(collapse = "") %>%
            xml2::read_html()
        
        current_team <- page %>%
            rvest::html_node(css = "table") %>% 
            rvest::html_table()
        
        all_team_rosters <- rbind(all_team_rosters,current_team)
    }
    
    all_team_rosters$player <- paste0(all_team_rosters$Vorname, ", ",all_team_rosters$Name)
    
    all_team_rosters$left_team <- grepl(pattern = "\\*\\*",all_team_rosters$Name)
    all_team_rosters$not_played <- grepl(pattern = "\\*",all_team_rosters$Name)
    
    all_team_rosters <- all_team_rosters%>% 
        mutate(not_played = ifelse(not_played==TRUE,1,0),
               left_team= ifelse(left_team==TRUE,1,0))
    
    all_team_rosters$player <- gsub("\\*","",all_team_rosters$player)
    
    rosters <- all_team_rosters %>% 
        select(player,Nr.,Geburtsdatum:left_team,not_played) %>% 
        mutate(not_played = ifelse(not_played+left_team>1,0,not_played))
    rosters$year <- year
    rosters$player <- trimws(rosters$player)
    
    
    rm$close()
    # stop the selenium server
    remDr$server$stop()
    base::rm(remDr)
    gc()
    system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
    
    return(rosters)
}

# calculate minutes played
playing_time <- function(roster_game, pbp_game){
    roster_game <- roster_game %>% 
        select(Player,Club,starts_with("starter_Q")) %>% 
        mutate(Q1 = if_else(starter_Q1 == 1,Player,"0"),
               Q2 = if_else(starter_Q2 == 1,Player,"0"),
               Q3 = if_else(starter_Q3 == 1,Player,"0"),
               Q4 = if_else(starter_Q4 == 1,Player,"0"))
    
    if("starter_Q5" %in% colnames(roster_game))
    {
        roster_game <- roster_game %>% 
            mutate(Q5 = if_else(starter_Q5 == 1,Player,"0"))
    }
    if("starter_Q6" %in% colnames(roster_game))
    {
        roster_game <- roster_game %>% 
            mutate(Q6 = if_else(starter_Q6 == 1,Player,"0"))
    }
    if("starter_Q7" %in% colnames(roster_game))
    {
        roster_game <- roster_game %>% 
            mutate(Q7 = if_else(starter_Q7 == 1,Player,"0"))
    }
    
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
        
        if(nrow(d) > 1){
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
        }else{
            data_new <- d
        }

        
        data_new <- d[- 1, ]
        
        
        
        f <- bind_rows(c,data_new) %>%
            group_by(nummer_aktion, quarter) %>%
            filter(row_number() == n()) %>%
            ungroup() %>%
            arrange(nummer_aktion)
        
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
        mutate_at("end_q", ~replace(., is.na(.), viertel -1)) %>% 
        mutate_at("end_time", ~replace(., is.na(.), 0))
    
    t_played <- t_played %>% 
        mutate(sec = case_when(
            # same quarter
            start_q == end_q ~ start_time - end_time,
            start_q == end_q -1 & end_time == 600 ~ start_time - 0,
            start_q == end_q -2 & end_time == 600 ~ 600 + start_time - 0,
            start_q == end_q -3 & end_time == 600 ~ 600 + 600 + start_time - 0,
            # different quarter in regular playing time
            start_q == (end_q -1) & end_time != 600 & end_q <= 4 ~ start_time - 0 + 600 - end_time,
            start_q == (end_q -2) & end_time != 600 & end_q <= 4 ~ start_time - 0 + 600 + 600 - end_time,
            start_q == (end_q -3) & end_time != 600 & end_q <= 4 ~ start_time - 0 + 600 + 600 + 600 - end_time,
            start_q == (end_q -4) & end_q <= 4 ~ 600 + 600 + 600 + 600,
            
            start_q == 2 & end_q == 5  ~ start_time - 0 + 600 + 600 + 300 - end_time,
            start_q == 3 & end_q == 5  ~ start_time - 0 + 600 + 300 - end_time,
            start_q == 3 & end_q == 6  ~ start_time - 0 + 600 + 300 + 300 - end_time,
            start_q == 4 & end_q == 5  ~ start_time - 0 + 300 - end_time,
            start_q == 4 & end_q == 6  ~ start_time - 0 + 300 + 300 - end_time,
            start_q == 4 & end_q == 7  ~ start_time - 0 + 300 + 300 + 300 - end_time,
            
            # different quarter in overtime
            start_q == end_q -1 & end_time == 300 & start_q > 4 ~ start_time - 0,
            start_q == end_q -2 & end_time == 300 & start_q > 4 ~ 300 + start_time - 0,
            start_q == 5 & end_q == 6 & end_time != 300 ~ start_time - 0 + 300 - end_time,
            start_q == 5 & end_q == 7 & end_time != 300 ~ start_time - 0 + 300 + 300 - end_time,
            start_q == 5 & end_q == 8 & end_time != 300 ~ start_time - 0 + 300 + 300 + 300 - end_time,
            start_q == 6 & end_q == 7 & end_time != 300 ~ start_time - 0 + 300 - end_time,
            start_q == 6 & end_q == 8 & end_time != 300 ~ start_time - 0 + 300 + 300 - end_time,
            start_q == 7 & end_q == 8 & end_time != 300 ~ start_time - 0 + 300 - end_time,
        ))
    roster_for_merge <- select(roster_game,
                               Player,Club)
    time_merge <- merge(t_played,roster_for_merge,
                        by.x = "player",
                        by.y = "Player")
    
    t_ply <- time_merge %>%
        mutate(sec = ifelse(sec >= 0,sec,0)) %>% 
        group_by(player) %>%
        mutate(sec_total = sum(sec)) %>%
        ungroup() %>%
        distinct(player, .keep_all = TRUE) %>%
        mutate(min_sec_played = lubridate::seconds_to_period(sec_total))
}

# RAPM data frame
RAPM_data_frame <- function(roster_game, pbp_game){
    roster_game <- roster_game %>% 
        dplyr::select(Player,Club,starts_with("starter_Q")) %>% 
        mutate(Q1 = if_else(starter_Q1 == 1,Player,"0"),
               Q2 = if_else(starter_Q2 == 1,Player,"0"),
               Q3 = if_else(starter_Q3 == 1,Player,"0"),
               Q4 = if_else(starter_Q4 == 1,Player,"0"))
    
    if("starter_Q5" %in% colnames(roster_game))
    {
        roster_game <- roster_game %>% 
            mutate(Q5 = if_else(starter_Q5 == 1,Player,"0"))
    }
    if("starter_Q6" %in% colnames(roster_game))
    {
        roster_game <- roster_game %>% 
            mutate(Q6 = if_else(starter_Q6 == 1,Player,"0"))
    }
    if("starter_Q7" %in% colnames(roster_game))
    {
        roster_game <- roster_game %>% 
            mutate(Q7 = if_else(starter_Q7 == 1,Player,"0"))
    }
    
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
        
        if(nrow(d) > 1){
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
        }else{
            data_new <- d
        }
        
        
        data_new <- d[- 1, ]
        
        
        
        f <- bind_rows(c,data_new) %>%
            group_by(nummer_aktion, quarter) %>%
            filter(row_number() == n()) %>%
            ungroup() %>%
            arrange(nummer_aktion)
        
        pbp_merg <- bind_rows(pbp_merg,f)
    }
    
    
    pbp_merge <- na.locf(pbp_merg, na.rm = FALSE)
    
    return(pbp_merge)
}

# Coach data
get_coaches <- function(year){
    remDr <- RSelenium::rsDriver(verbose = T,
                                 remoteServerAddr = "localhost",
                                 port = 4444L,
                                 browser=c("firefox"))
    rm <- remDr$client
    rm$navigate("https://www.easycredit-bbl.de/de/saison/tabelle/gesamt/") 
    Sys.sleep(2)
    
    base <- "//select[@id='saison']/option[@value="
    year_id <- glue::glue('{base}', "\'",
                          year,"\']") %>% as.character()
    
    option_season <- rm$findElement(using = 'xpath', year_id)
    option_season$clickElement()
    
    page <- unlist(rm$getPageSource())
    
    page <- page %>%
        readr::read_lines() %>%
        str_replace_all("<!--|-->", "") %>%
        str_trim() %>%
        stringi::stri_trans_general("ASCII-Latin") %>%
        str_c(collapse = "") %>%
        xml2::read_html()
    
    xml_tables <- page %>%
        rvest::html_nodes(css = "[href*='/de/easycredit-bbl/historie/teams/t/']")
    
    team_urls <- xml_tables %>%
        rvest::html_attr("href")
    
    team_urls<- unique(team_urls)
    
    b <- paste0(year,"-",year+1)
    a <- grepl(b,team_urls)
    team_urls <- team_urls[a]
    Sys.sleep(2)
    
    coaches <- tibble()
    team <- tibble()
    for (i in 1:length(team_urls)) {
        current_team_url <- paste0("https://www.easycredit-bbl.de",team_urls[i])
        rm$navigate(current_team_url)
        Sys.sleep(2)
        
        page <- unlist(rm$getPageSource())
        page <- page %>%
            readr::read_lines() %>%
            str_replace_all("<!--|-->", "") %>%
            str_trim() %>%
            stringi::stri_trans_general("ASCII-de") %>%
            str_c(collapse = "") %>%
            xml2::read_html()
        
        team_name <- page %>% 
            rvest::html_node(css = "[class = 'klubheader']") %>%
            rvest::html_text()
        
        current_team <- page %>%
            rvest::html_node(css = "[class = 'boxcol3']") %>%
            rvest::html_text()
        
        coaches <- rbind(coaches,current_team)
        team <- rbind(team,team_name)
    }
    
    rm$close()
    # stop the selenium server
    remDr$server$stop()
    base::rm(remDr)
    gc()
    system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
    
    
    coach <- cbind(coaches,team)
    colnames(coach) <- c("coach","team")
    
    coach$coach <- gsub("Headcoach","",coach$coach)
    coach$team <- gsub("Platz:.*",'',coach$team)
    
    coach$year <- year
    coach$coach <- trimws(coach$coach)
    coach$team <- trimws(coach$team)
    
    return(coach)
}
