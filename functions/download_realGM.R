#### download player data from realGM.com ####
player_data_realGM <- function(league = 'German BBL', season = 2020, type = 'Totals', position = 'All') {
  
  load(file = "data/league_ids.rda")
  league_id = leagues_ids$id[leagues_ids$league_name == league]
  
  dat = data.frame()
  
  for(i in 1:5) {
    
    #create url
    url = paste('https://basketball.realgm.com/international/league/',league_id,'/',league,'/stats/',season,'/',type,'/All/All/points/',position,'/asc/',i,'/Regular_Season', sep = '')
    #url <- paste('https://basketball.realgm.com/international/league/15/German-BBL/stats/2020/Totals/All/All/points/All/desc/',i,'/Regular_Season', sep = '')
    df = readHTMLTable(htmlParse(readLines(url)))
    
    if(length(df)>0) {
      df = df[[1]]
    } else {
      df = NULL
    }
    
    if(length(df)>0) {
      dat = rbind(dat, df)
    }
  }
  
  #remove duplicates from dat
  dat = dat[!duplicated(dat),]
  
  #convert factors to numeric
  dat <- dat %>% mutate_at(vars(-c('Player', 'Team')), as.numeric)
  
  player <- dat
  
  # mit bayern aufräumen...
  
  dat = data.frame()
  
  #create url
  url <- paste('https://basketball.realgm.com/international/league/15/German-BBL/team/343/Bayern-Munich/stats/',season,'/Totals/All/All', sep = '')
  df = readHTMLTable(htmlParse(readLines(url)))
  
  if(length(df)>0) {
    df = df[[1]]
  } else {
    df = NULL
  }
  
  if(length(df)>0) {
    dat = rbind(dat, df)
  }
  
  #remove duplicates from dat
  dat = dat[!duplicated(dat),]
  
  #convert factors to numeric
  dat <- dat %>% mutate_at(vars(-c('Player', 'Team')), as.numeric)
  
  bayern <- dat
  bayern$Team <- "FCB"
  
  player <- bind_rows(bayern,player) %>% 
    distinct(Player, FGA,FGM,GP, .keep_all = TRUE)
  
  
}
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
#### download team data from realGM.com ####
team_data_realGM <- function(league = 'German BBL', season = 2020, type = 'Totals') {
  
  load(file = "data/league_ids.rda")
  league_id = leagues_ids$id[leagues_ids$league_name == league]
  
  dat = data.frame()
  
  #create url
  url <- paste('https://basketball.realgm.com/international/league/',league_id,'/German-BBL/team-stats/',season,'/',type,'/Team_Totals', sep = '')
  df = readHTMLTable(htmlParse(readLines(url)))
  
  if(length(df)>0) {
    df = df[[1]]
  } else {
    df = NULL
  }
  
  if(length(df)>0) {
    dat = rbind(dat, df)
  }
  
  #remove duplicates from dat
  dat = dat[!duplicated(dat),]
  
  #convert factors to numeric
  dat <- dat %>% mutate_at(vars(-c('Team')), as.numeric)
  
  team <- dat %>% 
    select(-`#`)
  
  ###############################################################################
  # probieren ob alles stimmt... eh nicht...
  # player <- player %>% 
  #   group_by(Team) %>% 
  #   mutate(FGM_t = sum (FGM)) %>% 
  #   mutate(FGA_t = sum (FGA)) %>% 
  #   ungroup()
  # doch stimmt mit teams �berein.
  
  # team_for_merge <- team %>% 
  #   select(Team, FGM, FGA)
  # df_merge <- merge(player, team_for_merge,
  #                   by.x = c("FGM_t", "FGA_t"),
  #                   by.y = c("FGM", "FGA")) %>% 
  #   rename(., team = Team.y) %>% 
  #   rename(., teamslug = Team.x) %>% 
  #   select(-`#`, -FGM_t, -FGA_t)
  # 
  # # merge again to obtain all team stats
  # team_for_merge <- team
  # colnames(team_for_merge) <- paste(colnames(team_for_merge),"t", sep = "_")
  # df_merge2 <- merge(df_merge,team_for_merge, by.x = "team", by.y = "Team_t") %>% 
  #   select(-`#_t`)
  
  #merge again to obtain all team advanced stats (ORtg / DRtg / Pace / Possessions)
  # df_merge3 <- merge(df_merge2,team_adv, by.x = "team", by.y = "Team") %>% 
  #   relocate(Player, everything())
  # 
  # player <- df_merge3
  
}

################################################################################
# dictionary für teams bauen
# irgendwann später...