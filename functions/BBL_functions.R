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
        
        button_filter <- rm$findElement(using = 'xpath', "//input[@class = 'btn'][@type = 'submit'][@value = 'Filter ?bernehmen']")
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
    
    # Roster (who played?)
    roster <- json_info$roster %>% 
        data.frame %>%
        as_tibble() %>%
        mutate(Player = paste(FirstName, Name, sep=", "), .keep = "unused") %>%
        mutate(Club = if_else(TC == "A",team_h,team_a)) %>% 
        mutate(Pos = Posshort, .keep ="unused") %>% 
        relocate(Player, Pos, .before = Is)
    
    roster_h <- filter(roster, TC =="A")
    roster_a <- filter(roster, TC =="B")
    
    roster_merge <- roster %>% 
        select(TC, Nr, Player, Club)
    
    
    # get PbP for each quarter played
    # check how many quarters are played:
    urls <- vector()
    for (i in 1:12) {
        urls[i] <- gsub(" ","",paste("http://live.easycredit-bbl.de/data",year,
                                     "/bbl/",game_id$home_id,
                                     "/",game_id$game_id,
                                     "Q", i,
                                     ".JSN")) 
    }
    
    quarters_played <- sum(sapply(urls,valid_url))
    
    
    
    pbp <- data.frame()
    for(i in 1:quarters_played){
        quarter <- i
        url_pbp <- gsub(" ","",paste("http://live.easycredit-bbl.de/data",year,
                                     "/bbl/",game_id$home_id,
                                     "/",game_id$game_id,
                                     "Q", quarter,
                                     ".JSN"))

        json_pbp <- get_json(url_pbp)
        
        pbp_quarter <- json_pbp %>% 
            data.frame %>%
            as_tibble() %>% 
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
                   number_action = actions.14,
                   timestamp = actions.15)
        
        pbp_quarter$nummer_aktion <- nrow(pbp_quarter):1
        pbp_quarter$quarter <- quarter
        
        pbp <- bind_rows(pbp,pbp_quarter)
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
    
    return(pbp_merge)
}