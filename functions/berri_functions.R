# Roster stability calculation----
roster_stability <- function(statistic = stats4){
    df_roster <- data.frame(matrix(ncol = 9, nrow = 0))
    colnames(df_roster) <- c("player","team","year","min_p","min_t","pct_min_last2","pct_min_geblieben_current",
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
    year <- unique(statistic$year)

    statistic <- statistic %>% 
        mutate(min_t = min )
    for(i in year) {
        for(current_team in unique(statistic$team)) {
            a <- filter(statistic,
                        current_team == team,
                        year == i) %>% 
                select(player,team,year,min_p,min_t,pct_min_last2)
            
            b <- filter(statistic,
                        current_team == team,
                        year == i+1) %>% 
                select(player,team,year,min_p,min_t,pct_min_last2)
            
            transfer <- b$player %in% a$player 
            neue <- b[!transfer, ]
            alte <- b[transfer, ]
            
            wechsel <- a$player %in% b$player
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
                        by = c("player","year","team","min_p","min_t","pct_min_last2"),
                        all = TRUE,
                        no.dups = TRUE)%>% 
        arrange(year,team,player)
    dups <- roster_adj %>% duplicated ()
    rosters <- roster_adj[!dups,]
    
    team_rosters <- rosters %>% 
        group_by(team, year) %>% 
        mutate(new_team = if_else(is.na(pct_min_geblieben_current) == TRUE,1,0)) %>% 
        mutate(rstab_current = mean(sum_pct_min_geblieben, na.rm = T)) %>% 
        mutate(rstab_last2 = mean(sum_last2_min_pct, na.rm = T)) %>%
        ungroup()
    team_rosters <- sapply(team_rosters, function(i){ifelse(is.na(i),0,i)}) %>%
        as_tibble() #%>%
        # relocate(year,player,team,Pos.,everything()) %>% 
        # select(-team_name,
        #        -pct_min_geblieben_current,
        #        -sum_pct_min_geblieben,
        #        -sum_last2_min_pct) %>% 
        # mutate_at(-c(2:6), as.numeric)
    
    return(team_rosters)
}