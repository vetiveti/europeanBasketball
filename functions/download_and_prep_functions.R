#### function for possible download queries ####
query <- function(query_type = "general", type = "team"){
  a <- tibble(typeQuery = c("splits", "splits", "general", "general", "defense", "defense", "clutch", "clutch",
                            "hustle", "hustle", "shots", "shots", "shot locations", "shot locations"),
              slugQuery = c("teamdashboardbygeneralsplits", "playerdashboardbygeneralsplits", "leaguedashteamstats", "leaguedashplayerstats" ,"leaguedashptdefend", "leaguedashptdefend", "leaguedashteamclutch", "leaguedashplayerclutch","leaguehustlestatsteam", "leaguehustlestatsplayer", "leaguedashteamptshot", "leaguedashplayerptshot",
                            "leaguedashteamshotlocations", "leaguedashplayershotlocations"),
              typeSearch = c("team", "player", "team", "player", "team", "player", "team", "player", "team", "player", "team", "player", "team", "player")) %>% suppressWarnings()
  df_query_dict <- a
  
  query_slug <- df_query_dict %>%
    filter(typeQuery %>% str_detect(str_to_lower(query_type))) %>%
    filter(typeSearch %>% str_detect(str_to_lower(type))) %>%
    pull(slugQuery)
  
  base <- glue::glue("https://stats.nba.com/stats/{query_slug}/?") %>% as.character()
  base
  
}

# parameters
std_parameter <- function(MeasureType = "Base",
                          PerMode = "Totals",
                          DateFrom = '',
                          DateTo = '',
                          PlusMinus= "N",
                          PaceAdj = "N",
                          Rank = "N",
                          SeasonType="Regular%20Season",
                          Outcome = '',
                          Location = '',
                          Month = 0,
                          SeasonSegment = '',
                          OpponentTeamID = 0,
                          VsConference = '',
                          VsDivision = '',
                          GameSegment = '',
                          Period = 0,
                          LastNGames = 0,
                          Season = "2004-05",
                          type = "team",
                          GameScope = '',
                          PlayerExperience = "",
                          PlayerPosition = "",
                          StarterBench="")
{
  # Season_0 <- 1993:1999 %>% as.character() %>% paste(., "-",sep="")
  # Season_01 <- sprintf("%02d",94:99) %>% as.character()
  # Season_01[7] = "00"
  season_1 <- 2004:2018 %>% as.character() %>% paste(., "-",sep="")
  season_2 <- sprintf("%02d",5:19) %>% as.character()
  Season <- paste(season_1, season_2, sep="")
  
  assign('MeasureType', MeasureType, envir =.GlobalEnv)
  assign('PerMode', PerMode, envir =.GlobalEnv)
  assign('PlusMinus', PlusMinus, envir =.GlobalEnv)
  assign('PaceAdj', PaceAdj, envir =.GlobalEnv)
  assign('Rank', Rank, envir =.GlobalEnv)
  assign('SeasonType', SeasonType, envir =.GlobalEnv)
  assign('Outcome', Outcome, envir =.GlobalEnv)
  assign('Location', Location, envir =.GlobalEnv)
  assign('Month', Month, envir =.GlobalEnv)
  assign('SeasonSegment', SeasonSegment, envir =.GlobalEnv)
  assign('DateFrom', DateFrom, envir =.GlobalEnv)
  assign('DateTo', DateTo, envir =.GlobalEnv)
  assign('OpponentTeamID', OpponentTeamID, envir =.GlobalEnv)
  assign('VsConference', VsConference, envir =.GlobalEnv)
  assign('VsDivision', VsDivision, envir =.GlobalEnv)
  assign('GameSegment', GameSegment, envir =.GlobalEnv)
  assign('Period', Period, envir =.GlobalEnv)
  assign('LastNGames', LastNGames, envir =.GlobalEnv)
  assign('type', type, envir =.GlobalEnv)
  assign('Season', Season, envir =.GlobalEnv)
  assign('GameScope', GameScope, envir =.GlobalEnv)
  assign('PlayerExperience', PlayerExperience, envir =.GlobalEnv)
  assign('PlayerPosition', PlayerPosition, envir =.GlobalEnv)
  assign('StarterBench',StarterBench, envir=.GlobalEnv)
  
  
  
parameters <-  list(MeasureType,PerMode,PlusMinus,PaceAdj,Rank,SeasonType,Outcome,Location,Month,
       SeasonSegment,DateFrom,DateTo,OpponentTeamID,VsConference,VsDivision,GameSegment,
       Period,LastNGames,Season,type)
parameters

}
##### download function for NBA.com ####
dl <- function (url = "https://stats.nba.com/stats/leaguegamelog?Counter=1000&Season=2019-20&Direction=DESC&LeagueID=00&PlayerOrTeam=P&SeasonType=Regular%20Season&Sorter=DATE") 
{
  URL <- URL %>% as.character() %>% URLencode()
  
  headers = c(Connection = "close", Accept = "application/json, text/plain, */*", 
              `x-nba-stats-token` = "true", `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36", 
              `x-nba-stats-origin` = "stats", `Sec-Fetch-Site` = "same-origin", 
              `Sec-Fetch-Mode` = "cors", Referer = "https://downwiththechinazis.com", 
              `Accept-Encoding` = "gzip, deflate, br", `Accept-Language` = "en-US,en;q=0.9")
  headers <- c(Host = "stats.nba.com", `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv =72.0) Gecko/20100101 Firefox/72.0", 
               Accept = "application/json, text/plain, */*", `Accept-Language` = "en-US,en;q=0.5", 
               `Accept-Encoding` = "gzip, deflate, br", `x-nba-stats-origin` = "stats", 
               `x-nba-stats-token` = "true", Connection = "keep-alive", 
               Referer = "https =//stats.nba.com/", Pragma = "no-cache", 
               `Cache-Control` = "no-cache")
  
  res <- httr::GET(url, httr::add_headers(.headers = headers))
  
  json <- res$content %>% rawToChar() %>% jsonlite::fromJSON(simplifyVector = T)
  json
}


##### Download function for Basketball-reference.com ####
dl_bref <- function(URL = "https://www.basketball-reference.com/leagues/NBA_", Season =2018) {
  data_team <- data.frame()
  
  for (i in Season) {
    Season <- i
    base <- base
    url <- glue('{base}',
                '{Season}',
                '.html') %>%
      as.character() %>%
      URLencode()
    

    
      page <- url %>%
        readr::read_lines() %>%
        str_replace_all("<!--|-->", "") %>%
        str_trim() %>%
        stringi::stri_trans_general("Latin-ASCII") %>%
        str_c(collapse = "") %>%
        xml2::read_html()
      
      xml_tables <- page %>%
        html_nodes(xpath = "//*[contains(@class, 'sortable')]")
      
      all_data <- seq_along(xml_tables)
      
      table_id <- xml_tables %>%
        html_attr("id")
      
      table_name <-
        xml_tables %>%
        xml_nodes("caption") %>%
        html_text()
      
      is_team_base <- which(str_detect(table_id,"team-stats-base"))
      is_opponent_base <- which(str_detect(table_id,"opponent-stats-base"))
      x <- c(is_team_base, is_opponent_base)
      ids <- i
      
      for (i in x) {
      data <- xml_tables[[i]] %>%
        html_table(header = T,
                   trim = T,
                   fill = F) %>%
        tibble::as_tibble()
      
      team_nodes <-
        xml_tables[i] %>%
        html_nodes("a") %>%
        html_attr('href')
      
      team_slugs <-
        team_nodes %>% str_replace_all("/teams/", "") %>%
        substr(1, 3)
      
      url_team <-
        team_nodes %>%
        str_c("https://www.basketball-reference.com", .)
      
      name_team <-
        xml_tables[i] %>%
        html_nodes("a") %>%
        html_text()
      
      df_urls <-
        tibble(
          slugTeamBREF = team_slugs,
          nameTeam = name_team,
          urlBREFTeamData = url_team
        )
      
      table_data <- data %>%
        filter(!Team %>% str_detect("Average"))
      
      table_data$slugTeamBREF <- team_slugs
      
      table_data <-
        table_data %>%
        left_join(df_urls) %>%
        suppressMessages()
      
      if (i ==which(str_detect(table_id,"team-stats-base"))) {
        Team <- table_data #%>%
        #str_to_lower()
      }
      else {
        Opponent <- table_data
        colnames(Opponent)[4:25] <- paste("opp", colnames(Opponent)[4:25], sep = "_") %>%
          str_to_lower()
      }
      
    }
    
    Opponent <- select(Opponent,-Rk)
    Team <- select(Team,-Rk)
    
    team_table <- merge(Team, Opponent, by = c("Team","G","urlBREFTeamData", "slugTeamBREF", "nameTeam"))
    assign('Team_table',team_table, envir =.GlobalEnv)
    
    
    #
    #
    #
    is_div_e <- which(str_detect(table_id,"divs_standings_E"))
    is_div_w <- which(str_detect(table_id,"divs_standings_W"))
    z <- c(is_div_e, is_div_w)
    
    # engage with the first and second table 
    for (i in z) {
      data <- xml_tables[[i]] %>%
        html_table(header = T,
                   trim = T,
                   fill = F) %>%
        tibble::as_tibble() %>%
        filter_all(all_vars(!grepl("Division", .)))
      
      team_nodes <-
        xml_tables[i] %>%
        html_nodes("a") %>%
        html_attr('href')
      
      
      team_slugs <-
        team_nodes %>% str_replace_all("/teams/", "") %>%
        substr(1, 3)
      
      name_team <-
        xml_tables[i] %>%
        html_nodes("a") %>%
        html_text()
      
      df_urls <-
        tibble(
          slugTeamBREF = team_slugs,
          nameTeam = name_team
        )
      
      table_data1 <- data 
      table_data1$slugTeamBREF <- team_slugs
      
      table_data1 <-
        table_data1 %>%
        left_join(df_urls) %>%
        suppressMessages()
      if (i == which(str_detect(table_id,"divs_standings_E"))) {
        first <- table_data1
      }
      else {
        second <- table_data1
      }
    }
    
    team_w_l <- bind_rows(first, second)
    team <- team_w_l %>%
      select(., W,L,slugTeamBREF,nameTeam) %>%
      transform(.,W = as.numeric(team_w_l$W),
                L = as.numeric(team_w_l$L))
    
    assign('team_w_l',team, envir =.GlobalEnv) 
    
    # merge both tables
    team_complete <- merge(Team_table,team, by = c("slugTeamBREF", "nameTeam")) %>%
      mutate(win_pct = W/G) %>%
      mutate(min = MP, .keep ="unused") %>%
      select(.,slugTeamBREF, nameTeam,G,W,L,win_pct,min,everything()) %>%
      select(.,-Team, Team) %>%
      select(., -urlBREFTeamData, urlBREFTeamData) %>% as.data.frame()
    team_complete$year <- ids
    
    assign(paste0("Season_", substr(Season, 1,4)), team_complete)
    data_team <- bind_rows(data_team,team_complete)
  }
  # merging years
  # team_years <- bind_rows(Season_1988,
  #                         Season_1989,Season_1990,Season_1991,Season_1992,Season_1993,
  #                         Season_1994,Season_1995,Season_1996,Season_1997,Season_1998,
  #                         Season_1999,Season_2000,Season_2001,Season_2002,Season_2003,
  #                         Season_2004,Season_2005,Season_2006,Season_2007,Season_2008,
  #                         Season_2009,Season_2010,Season_2011,Season_2012,Season_2013,
  #                         Season_2014,Season_2015,Season_2016,Season_2017,Season_2018,
  #                         Season_2019, .id ="id")
  # team_years <- team_years %>%
  #   arrange(.,nameTeam) %>%
  #   rename_at(colnames(team_years), ~ str_to_lower(colnames(team_years))) %>%
  #   rename(.,nameTeam = nameteam)
  data_team <- data_team %>% 
    arrange(nameTeam) %>% 
    relocate(year, everything()) %>% 
    rename_at(colnames(data_team), ~ str_to_lower(colnames(data_team))) %>% 
    rename(nameTeam = nameteam)
    
}
##### Download coach data function for Basketball-reference.com ####
dl_coach_data <- function(coach_dict = coach_dict){
  
  coach.panel1 <- data.frame(matrix(ncol = 12, nrow = 0))
  colnames(coach.panel1) <- c("year","coach_name","slugteambref","Season","Age","Lg","G","W","L","W/L%","W > .500","Finish")
  coach.panel1$year <- as.numeric()
  coach.panel1[sapply(coach.panel1, is.logical)] <- lapply(coach.panel1[sapply(coach.panel1, is.logical)], 
                                                           as.character)
  
  coach.panel2 <- data.frame(matrix(ncol = 10, nrow = 0))
  colnames(coach.panel2) <- c("coach_name","slugteambref", "Season","Age","Lg","G","W","L","W/L%","W > .500")
  coach.panel2[sapply(coach.panel2, is.logical)] <- lapply(coach.panel2[sapply(coach.panel2, is.logical)], 
                                                           as.character)
  
  base <- "https://www.basketball-reference.com/coaches/" %>%
    as.character() %>% URLencode()
  
  for (i in coach_dict$slugCoachBREF) {
    x <- i
    
    url <- glue('{base}',
                '{x}',
                '.html') %>%
      as.character() %>%
      URLencode()
    page <- url %>%
      readr::read_lines() %>%
      str_replace_all("<!--|-->", "") %>%
      str_trim() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_c(collapse = "") %>%
      xml2::read_html()
    
    xml_tables <- page %>%
      html_nodes(xpath = "//*[contains(@class, 'sortable')]")
    
    all_data <- seq_along(xml_tables)
    
    table_id <- xml_tables %>%
      html_attr("id")
    
    table_name <-
      xml_tables %>%
      xml_nodes("caption") %>%
      html_text()
    
    data <- xml_tables[[1]] %>%
      html_table(header = F,
                 trim = T,
                 fill = F)
    b <- data[2,]
    colnames(data) <- b
    
    data <- data %>% 
      select(1:10) %>% 
      filter(G !="Regular Season" & G != "Assistant Coach") %>% 
      filter(Season != "Season") %>% 
      mutate(slugteambref = Tm, .keep = "unused")
    
    
    data$coach_name <- x
    
    data1 <- data %>% 
      slice(1:which(str_detect(data$Season,"Career"))-1) %>% 
      mutate(year = as.numeric(substr(Season, 1,4)) + 1 ) %>% 
      relocate(year,coach_name,slugteambref, everything())
    
    data2 <-data %>% 
      slice(which(str_detect(data$Season,"Career")):length(data[,1])) %>% 
      select(-"Finish") %>% 
      relocate(coach_name,slugteambref, everything())
    data2 <- data2[-2,]
    
    coach.panel1 <- bind_rows(coach.panel1,data1)
    coach.panel2 <- bind_rows(coach.panel2,data2)
    
  }
  
  coaches <- merge(coach.panel1, coach_dict,
                   by.x = "coach_name",
                   by.y = "slugCoachBREF")
  coach_1988 <- select(coaches,year, nameCoach, slugteambref, Lg) %>% 
    filter(.,year >=1988 & year <= 2019 & Lg == "NBA") %>% 
    select(.,-Lg)
  
  assign('coach.panel2',coach.panel2, envir=.GlobalEnv)
  coach_1988
  
  
}
##### Download player age function for Basketball-reference.com ####
dl_player_age <- function(a = a){
  player.age <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(player.age) <- c("year","slugteambref","Season","Age","Lg","player_name" )
  player.age$year <- as.numeric()
  player.age[sapply(player.age, is.logical)] <- lapply(player.age[sapply(player.age, is.logical)], 
                                                       as.character)
  
  player.age$Age <- as.numeric()
  player.age$Age <- as.integer()
  j <- 0
  for (i in a$urlPlayerBioBREF) {
    j = j + 1
    url <- i %>% 
      as.character() %>%
      URLencode()
    
    page <- url %>%
      readr::read_lines() %>%
      str_replace_all("<!--|-->", "") %>%
      str_trim() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_c(collapse = "") %>%
      xml2::read_html()
    
    xml_tables <- page %>%
      html_nodes(xpath = "//*[contains(@class, 'sortable')]")
    
    all_data <- seq_along(xml_tables)
    
    table_id <- xml_tables %>%
      html_attr("id")
    
    table_name <-
      xml_tables %>%
      xml_nodes("caption") %>%
      html_text()
    
    data <- xml_tables[[2]] %>%
      html_table(header = T,
                 trim = T,
                 fill = F)
    
    data <- data %>% 
      select(1:PTS) %>% 
      mutate(slugteambref = Tm, .keep = "unused") %>% 
      slice(1:which(str_detect(data$Season,"Career"))-1) %>% 
      mutate(year = as.numeric(substr(Season, 1,4)) + 1 ) %>% 
      relocate(year,slugteambref, everything()) #%>% 
      #filter(slugteambref != "TOT")
    
    data$player_name <- as.character(a$namePlayerBREF[j])
    data$slugplayerbref <- as.character(a$slugPlayerBREF[j])
    
    player.age<- bind_rows(player.age,data)
  }
player.age
}
#### Download player height and weight ####
dl_player_cm_kg <- function(a = a){

  df <- data.frame(height = character())
  df_weight <- data.frame(weight = character())
  for(i in a$urlPlayerBioBREF){
    
    url <- i %>% 
      as.character() %>%
      URLencode()
    
    page <- url %>%
      readr::read_lines() %>%
      str_replace_all("<!--|-->", "") %>%
      str_trim() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_c(collapse = "") %>%
      xml2::read_html()
    
    xml_tables <- page %>%
      html_nodes(xpath = "//*[contains(@itemprop, 'height')]")
    if(length(xml_tables) == 0) {
      df_add <- "0"
    } else {
      df_add <- xml_tables[[1]] %>% 
        html_text()
    }
    
    df <- rbind(df, df_add)
    colnames(df) = "height"
    height <- df %>%
      select(height) %>% 
      separate(height, c('feet', 'inches'), "-", convert = TRUE) %>% 
      mutate(height_cm = (12*feet + inches)*2.54) %>% 
      select(height_cm)
    
    # weight:
    xml_tables <- page %>%
      html_nodes(xpath = "//*[contains(@itemprop, 'weight')]")
    if(length(xml_tables) == 0) {
      df_add <- "0"
    } else {
      df_add <- xml_tables[[1]] %>% 
        html_text()
    }
    
    df_weight <- rbind(df_weight, df_add)
    colnames(df_weight) = "weight"
    
    weight <- gsub("lb", "",df_weight$weight) %>% 
      as.numeric() %>% 
      as.data.frame()
    colnames(weight) = "weight"
    weight <- weight %>% 
      mutate(weight_kg = weight / 2.20462262) %>% 
      select(weight_kg)
  }
  
  player_data <- bind_cols(a$namePlayerBREF, a$slugPlayerBREF,height,weight)
  
  return(player_data)
}



#### data prep function ####
prep <- function(team_data){
  # get headers
  names_active <- team_data$resultSets$headers[1] %>% unlist() %>% 
    str_to_lower()
  
  # data as tibble
  data_tibble <- team_data$resultSets$rowSet[1] %>% data.frame(stringsAsFactors = F) %>% 
    as_tibble()
  
  # Die gezogen variablen namen als Spaltennamen einsetzen
  names(data_tibble) <- names_active
  
  
  
  # data_tibble$year <- substr(data_tibble$year,1,4) %>% as.numeric()
  # data_tibble <- subset(data_tibble, year >2011 & year<2019)
  # 
  # 
  # data_tibble <- select(data_tibble, -"div_rank", -"po_wins",-"po_losses",-"conf_count", -"div_count", -"nba_finals_appearance")
  # 
  # var_active <- colnames(data_tibble)
  # num_var <- var_active[! var _active %in% c("team_city","team_name")]
  # 
  # data_tibble <- data_tibble %>% mutate(across(.cols = all_of(num_var), as.numeric))
  
  data_tibble
}
#### download data from leaguedashteamstats and combine in vector ####
leagueTeamStats <- function(MeasureType = "Base",PerMode = "Totals"){
  for (i in Season) {
    Season <- i
    URL <- glue('{base}',
                'MeasureType={MeasureType}','&',
                'PerMode={PerMode}','&',
                'PlusMinus={PlusMinus}', '&',
                'PaceAdjust={PaceAdj}','&',
                'Rank={Rank}','&',
                'Season={Season}','&',
                'SeasonType={SeasonType}','&',
                'Outcome={Outcome}','&',
                'Location={Location}','&',
                'Month={Month}', '&',
                'SeasonSegment={SeasonSegment}', '&',
                'DateFrom={DateFrom}','&',
                'DateTo={DateTo}','&',
                'OpponentTeamID={OpponentTeamID}','&',
                'VsConference={VsConference}','&',
                'VsDivision={VsDivision}','&',
                'GameSegment={GameSegment}','&',
                'Period={Period}','&',
                'LastNGames={LastNGames}','&') %>% as.character()
    
    print(URL)
    assign('URL', URL, envir =.GlobalEnv)
    data <- dl(URL)
    
    prep_team <- prep(data)
    
    assign(paste0("Season_", substr(i, 1,4)), prep_team)
  }
  
  
  all <- bind_rows(Season_2004,Season_2005,Season_2006,Season_2007,Season_2008,Season_2009,Season_2010,
                   Season_2011,Season_2012,Season_2013,Season_2014,Season_2015,Season_2016,Season_2017,
                   Season_2018, .id ="id")
  
  
  
  var_active <- names(all)
  num_var <- var_active[! var_active %in% c("cfparams","team_name")]
  all_team <- all %>% mutate(across(.cols = all_of(num_var), as.numeric)) %>% as.tibble()
  #assign(paste0("Team_",MeasureType), all_team, envir = .GlobalEnv)
}

#### Dowload data from leaguedashplayerstats and combine in vector ####
playerstats <- function(MeasureType = "Base",PerMode = "Totals"){
  for (i in Season) {
    Season <- i
    URL <- glue('{base}',
                'MeasureType={MeasureType}','&',
                'PerMode={PerMode}','&',
                'PlusMinus={PlusMinus}', '&',
                'PaceAdjust={PaceAdj}','&',
                'Rank={Rank}','&',
                'Season={Season}','&',
                'SeasonType={SeasonType}','&',
                'Outcome={Outcome}','&',
                'Location={Location}','&',
                'Month={Month}', '&',
                'SeasonSegment={SeasonSegment}', '&',
                'DateFrom={DateFrom}','&',
                'DateTo={DateTo}','&',
                'OpponentTeamID={OpponentTeamID}','&',
                'VsConference={VsConference}','&',
                'VsDivision={VsDivision}','&',
                'GameSegment={GameSegment}','&',
                'Period={Period}','&',
                'LastNGames={LastNGames}','&',
                'GameScope={GameScope}', '&',
                'PlayerExperience={PlayerExperience}','&',
                'PlayerPosition={PlayerPosition}','&',
                'StarterBench={StarterBench}') %>% as.character()

    print(URL)
    assign('URL', URL, envir =.GlobalEnv)
    data <- dl(URL)

    prep_team <- prep(data)

    assign(paste0("Season_", substr(i, 1,4)), prep_team)
  }


  all <- bind_rows(Season_2004,Season_2005,Season_2006,Season_2007,Season_2008,Season_2009,Season_2010,
                   Season_2011,Season_2012,Season_2013,Season_2014,Season_2015,Season_2016,Season_2017,
                   Season_2018, .id ="id")



  var_active <- names(all)
  num_var <- var_active[! var_active %in% c("cfparams","player_name","team_abbreviation")]
  all_team <- all %>% mutate(across(.cols = all_of(num_var), as.numeric)) %>% as.tibble()
  #assign(paste0("Team_",MeasureType), all_team, envir = .GlobalEnv)
}
#### build dictionaries: teams ####
team_dict <- function(){
  url <- "https://stats.nba.com/js/data/ptsd/stats_ptsd.js"
  json <- url %>% readr::read_lines() %>% str_replace_all("var stats_ptsd =|\\;","")
  json <- json %>% jsonlite::fromJSON(flatten = TRUE, simplifyDataFrame = TRUE) %>%
    .parse_for_teams()
  #json <- subset(json,isNonNBATeam <1)
  team_dictionary <- select(json, c("nameTeam", "idTeam", "slugTeam","teamName", "cityTeam", "teamNameFull", "idConference", "idDivision"))
  
  assign('team_dictionary', team_dictionary, envir =.GlobalEnv)
  
}


#### function for possible download queries ####
query <- function(query_type = "general", type = "team"){
  a <- tibble(typeQuery = c("splits", "splits", "general", "general", "defense", "defense", "clutch", "clutch",
                            "hustle", "hustle", "shots", "shots", "shot locations", "shot locations"),
              slugQuery = c("teamdashboardbygeneralsplits", "playerdashboardbygeneralsplits", "leaguedashteamstats", "leaguedashplayerstats" ,"leaguedashptdefend", "leaguedashptdefend", "leaguedashteamclutch", "leaguedashplayerclutch","leaguehustlestatsteam", "leaguehustlestatsplayer", "leaguedashteamptshot", "leaguedashplayerptshot",
                            "leaguedashteamshotlocations", "leaguedashplayershotlocations"),
              typeSearch = c("team", "player", "team", "player", "team", "player", "team", "player", "team", "player", "team", "player", "team", "player")) %>% suppressWarnings()
  df_query_dict <- a
  
  query_slug <- df_query_dict %>%
    filter(typeQuery %>% str_detect(str_to_lower(query_type))) %>%
    filter(typeSearch %>% str_detect(str_to_lower(type))) %>%
    pull(slugQuery)
  
  base <- glue::glue("https://stats.nba.com/stats/{query_slug}/?") %>% as.character()
  base
  
}
#
#
#
#
#
#
#

##### season_slug function ####
generate_season_slug <- function(season = 2018) {
  season_start <- season - 1
  season_end_slug <- season %>% substr(3,4)
  
  glue::glue("{season_start}-{season_end_slug}") %>%
    as.character()
}

##### roster stability calculation ####
roster_stability <- function(statistic = stats4){
  df_roster <- data.frame(matrix(ncol = 9, nrow = 0))
  colnames(df_roster) <- c("player_name","slugteambref","year","min_p","min_t","pct_min_last2","pct_min_geblieben_current",
                           "sum_pct_min_geblieben","sum_last2_min_pct")
  df_roster$year <- as.numeric()
  df_roster$min_p <- as.numeric()
  df_roster$min_t <- as.numeric()
  df_roster$pct_min_last2 <- as.numeric()
  df_roster$pct_min_geblieben_current <- as.numeric()
  df_roster$sum_pct_min_geblieben <- as.numeric()
  df_roster$sum_last2_min_pct <- as.numeric()
  
  df_roster[sapply(df_roster, is.logical)] <- lapply(df_roster[sapply(df_roster, is.logical)], 
                                                           as.character)
  for(i in 1988:2010) {
    for(team in teamslugs) {
      a <- filter(statistic,
                  slugteambref == team,
                  year == i) %>% 
        select(player_name,slugteambref,year,min_p,min_t,pct_min_last2)
      
      b <- filter(statistic,
                  slugteambref == team,
                  year == i+1) %>% 
        select(player_name,slugteambref,year,min_p,min_t,pct_min_last2)
      
      transfer <- b$player_name %in% a$player_name 
      neue <- b[!transfer, ]
      alte <- b[transfer, ]
      
      wechsel <- a$player_name %in% b$player_name
      bleiben <- a[wechsel, ]
      gewechselt <- a[!wechsel,]
      #stabil <- dim(bleiben)[1] / dim(b)[1]
      
      pct_min_bleiben <- alte %>% 
        mutate(pct_min_geblieben_current = min_p /min_t) %>% 
        mutate(sum_pct_min_geblieben= sum(pct_min_geblieben_current)) %>% 
        mutate(sum_last2_min_pct = sum(pct_min_last2))
      
      df_roster <- bind_rows(df_roster,pct_min_bleiben)
    }
    df_roster <- bind_rows(df_roster,pct_min_bleiben)
  }
  roster_adj <- merge(statistic, df_roster,
                      by = c("player_name","year","slugteambref","min_p","min_t","pct_min_last2"),
                      all = TRUE,
                      no.dups = TRUE)%>% 
    arrange(year,slugteambref,player_name)
  dups <- roster_adj %>% duplicated ()
  rosters <- roster_adj[!dups,]
  
  team_rosters <- rosters %>% 
    group_by(slugteambref, year, .add=T) %>% 
    mutate(new_team = if_else(is.na(pct_min_geblieben_current) == TRUE,1,0)) %>% 
    mutate(rstab_current = mean(sum_pct_min_geblieben, na.rm = T)) %>% 
    mutate(rstab_last2 = mean(sum_last2_min_pct, na.rm = T)) %>%
    ungroup()
  team_rosters <- sapply(team_rosters, function(i){ifelse(is.na(i),0,i)}) %>%
    as_tibble() %>%
    relocate(year,season,player_name, slugplayerbref,slugteambref,pos,everything()) %>% 
    select(-team_name,
           -pct_min_geblieben_current,
           -sum_pct_min_geblieben,
           -sum_last2_min_pct) %>% 
    mutate_at(-c(2:6), as.numeric)
  
  
  assign('team_rosters',team_rosters, envir=.GlobalEnv)
  team_rosters
}

#### BBL Download function ####
id_games_BBL <- function(c = c){
  option_season <- rm$findElement(using = 'xpath', c)
  option_season$clickElement()
  
  option_contest <- rm$findElement(using = 'xpath', "//select[@id='wettbewerb']/option[@value='1']")
  option_contest$clickElement()
  
  button_filter <- rm$findElement(using = 'xpath', "//input[@class = 'btn'][@type = 'submit'][@value = 'Filter übernehmen']")
  button_filter$sendKeysToElement(list("R Cran", key = "enter"))
  
  
  
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
  
  #
  
  
  
  identifiers <- c()
  for(index in 1:length(games)){
    page_game <- RCurl::getURL(paste0("https://www.easycredit-bbl.de", games[index]))
    tpage <- htmlParse(page_game)
    game_id <- str_extract(str_extract(page_game,"bekoGameId = '[0-9]{1,8}"), "[0-9]{1,8}")
    home_id <- str_extract(str_extract(page_game,"bekoHomeTeamId = '[0-9]{1,8}"), "[0-9]{1,8}")
    identifiers <- cbind(identifiers, c(game_id, home_id))
  }
  return(identifiers)
}

###