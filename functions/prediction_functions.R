pred_rmse <- function(model,player,team,years,perc,seed){
    
    pred_m_years <- tibble()
    for (year_current in years) {
        models <- model %>% 
            filter(., year == year_current)
        
        i <- unique(models$team)
        
        prediction_teams <- tibble()
        for (x in i) {
            
            df_team<- team %>% 
                filter(.,team == x & year == year_current)
            
            # set seed
            set.seed(seed)
            # split at X% of the games to predict the remaining
            train_games <- df_team %>% 
                slice_sample(.,prop = perc, replace = FALSE)
            
            df_train <- filter(models,
                               game %in% train_games$game) %>% 
                group_by(player,team) %>% 
                summarise_at(vars(ws_pm,w_add_BPM_pm,wp_pm), funs(weighted.mean(.,min_p))) %>% 
                ungroup()
            
            play_time <- filter(models,
                                game %in% train_games$game) %>% 
                group_by(player,team) %>%
                summarise_at(vars(min_p), .funs = sum) %>% 
                ungroup()
            
            train_players <- merge(df_train,play_time,
                                   by = c("player","team"))
            
            train_team <- filter(df_team,
                                 game %in% train_games$game) %>% 
                dplyr::select(team,G,W,L,min) %>%
                group_by(team) %>% 
                summarise_at(vars(G:min), .funs = sum) %>%
                rename(min_t = min)
            
            train_set <- merge(train_players,train_team,
                               by = "team") %>% 
                mutate(min_pct = min_p /min_t) 
            #sum(train_set$min_pct)
            
            # test set
            test_set <- df_team %>% 
                filter(!game %in% train_games$game)
            
            test_team <- test_set %>% 
                dplyr::select(team,G,W,L,min) %>% 
                group_by(team) %>% 
                summarise_at(vars(G:min), .funs = sum) %>% 
                ungroup() %>%
                rename(min_t = min)
            
            pred_merge <- merge(train_set,test_team,
                                by = "team", suffixes = c("_b","_a")) %>% 
                rename(min_to_play = min_t_a)
            
            prediction <- pred_merge %>% 
                mutate(min_p_pred = min_pct * min_to_play,
                       ws_pred = ws_pm * min_p_pred,
                       wp_pred = wp_pm * min_p_pred,
                       BPM_pred = w_add_BPM_pm * min_p_pred) %>% 
                dplyr::select(player,team,min_p_pred,ws_pred,wp_pred,BPM_pred) %>% 
                group_by(team) %>% 
                summarise_at(vars(min_p_pred,ws_pred,wp_pred,BPM_pred), .funs = sum) %>% 
                mutate(W_a = test_team$W,
                       ws_pred = case_when(ws_pred < 0 ~ 0,
                                           ws_pred > test_team$G ~ test_team$G,
                                           TRUE ~ ws_pred),
                       wp_pred = case_when(wp_pred < 0 ~ 0,
                                           wp_pred > test_team$G ~ test_team$G,
                                           TRUE ~ wp_pred),
                       BPM_pred = case_when(BPM_pred < 0 ~ 0,
                                            BPM_pred > test_team$G ~ test_team$G,
                                            TRUE ~ BPM_pred))
            
            prediction_teams <- bind_rows(prediction_teams,prediction) %>% 
                mutate(year = year_current,
                       train_pct = perc,
                       test_pct = 1 - perc,)
        }
        
        pred_m_years <- bind_rows(pred_m_years,prediction_teams)
    }

    pred_m_years <- pred_m_years %>% 
        mutate(WP_RMSE = rmse(W_a,wp_pred),
               WS_RMSE = rmse(W_a,ws_pred),
               BPM_RMSE = rmse(W_a,BPM_pred))
    
    return(pred_m_years)
}
