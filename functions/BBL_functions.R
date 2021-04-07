#' #### get BBL game ID's ####
#' start R Selenium
BBL_game_ids <- function(year){
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

#' #### get BBL PbP ####
#' 