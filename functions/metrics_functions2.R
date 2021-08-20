wp <- function(player,team,marg_values,DREBMATE,tASTpm,pos_avg){
    # Step 1 - player production per 48 minutes----
    # foul percentage
    team_for_merge <- team %>% 
        dplyr::select(team, opp_ftm,pf,year) %>% 
        rename(pf_t = pf)
    
    player_for_merge <- player %>% 
        dplyr::select(player, team, pf,year) %>% 
        mutate(pf_p = pf)
    
    player_perT <- merge(player_for_merge,team_for_merge,
                         by = c("team","year"),
                         all = TRUE) %>% 
        mutate(pf_p1 = pf_p)
    
    player_perT <- player_perT %>% 
        group_by(team,year) %>% 
        mutate(PF_perc = pf_p1 / pf_t,
               pf_p_new = opp_ftm * PF_perc) %>% 
        ungroup() %>% 
        dplyr::select(player,team,year,pf_p_new)
    
    player_totals <- merge(player, player_perT,
                           by = c("player","team","year")) %>% 
        relocate(fga, fgm, .after = min_p)
    
    ####
    player_totals <- player_totals %>% 
        mutate(total_production  = (
            p3m * marg_values$`3FGM`
            + p2m  * marg_values$`2FGM`
            + ftm  * marg_values$FTM
            + (p2a - p2m) * marg_values$MFG
            + (p3a - p3m)* marg_values$MFG
            + (fta - ftm) * marg_values$MFT
            + orb * marg_values$ORB
            + drb * marg_values$DRB
            + tov  * marg_values$TOV
            + stl  * marg_values$STL
            + pf_p_new * marg_values$PF
            + blk  * marg_values$BLK)
            #+ ast  * marg_values$AST)
        )
    
    wins_produced <- player_totals %>% 
        select(player,team,year,min_p,Pos.,total_production) %>%
        mutate(P48 = (ifelse(min_p>0,(total_production/min_p) * 40, 0))) %>%
        arrange(player)
    
    #******************************************************************************#
    # Step 2 - Adjusting for teammates production of defensive rebounds ----
    DREB_adjustment <- merge(player_totals, team,
                             by = c("team","year"),
                             all = TRUE) %>%
        arrange(player) %>%
        dplyr::select(player,team,year,min_p,min,drb.x,drb.y)
    
    DREB_adjustment <- DREB_adjustment %>%
        mutate(TDREBPM =(drb.y - drb.x) / (min - min_p))
    
    # Step 2.2 - Estimate how much production is lost cause of that
    # Thus, multiply by estimated value for DREB lost because of teammates (for now 0.504)
    DREB_adjustment <- DREB_adjustment %>%
        mutate(TDREBPM_ind_reb_loss = TDREBPM * - DREBMATE)
    
    # Step 2.3
    # calculate marginal value of productivity of team defensive rebounds for each player lost
    # thus multiply with playing time of specific player
    DREB_adjustment <- DREB_adjustment %>%
        mutate(TDREB_ind_loss = TDREBPM_ind_reb_loss * marg_values$DRB * min_p)
    
    # Step 2.4
    # sum across all team members and id
    DREB_adjustment <- DREB_adjustment %>%
        group_by(team,year) %>%
        mutate(TDREB_totalloss = sum(TDREB_ind_loss)) %>%
        ungroup()
    
    # Step 2.5
    # Now multiply this by % of defensive rebounds from each player per team and id
    # this yields how many rebounds a player grabbed from his teammates
    DREB_adjustment <- DREB_adjustment %>%
        group_by(team,year) %>%
        mutate(rebound_percentage = (drb.x / drb.y)) %>%
        mutate(eins = sum(rebound_percentage)) %>%
        ungroup()
    
    DREB_adjustment$TDREB_captured_from_tm <- DREB_adjustment$TDREB_totalloss * DREB_adjustment$rebound_percentage
    
    # Step 2.6
    # subtract step 5 from step 4 and add the result to PROD
    DREB_adjustment$net_TDREB <- DREB_adjustment$TDREB_captured_from_tm - DREB_adjustment$TDREB_ind_loss
    
    wins_produced <- wins_produced %>%
        mutate(net_TDREB = DREB_adjustment$net_TDREB) %>% 
        mutate(PROD_adj = DREB_adjustment$net_TDREB + total_production) %>%
        mutate(P48Adj =if_else(min_p==0,0,PROD_adj / min_p * 40))
    
    #******************************************************************************#
    # Step 3 - Adjust for Assists----
    assist_adjustment <- merge(player_totals, team,
                               by = c("team","year"),
                               all = TRUE) %>%
        relocate(player, .before = team) %>% 
        arrange(player) %>%
        dplyr::select(1:2,year,min,min_p,ast.x,ast.y, fga.x)
    
    # Step 3.1 - Calculate Teammate's assist per minute
    assist_adjustment <- assist_adjustment %>%
        mutate(TAPM =(ast.y - ast.x) / (min - min_p))
    
    # Step 3.2 - Multiply TAPM by coefficient of assists produced by teammates
    assist_adjustment <- assist_adjustment %>%
        mutate(TAPM_val =TAPM * tASTpm) %>%
        mutate(TAPM_val_FG = TAPM_val * 2)
    
    # Step 3.3 - Multiply TAPM_val_FG by field goals taken. 
    assist_adjustment <- assist_adjustment %>%
        mutate(points_credit_team =TAPM_val_FG * fga.x)
    
    # Step 3.4 - Multiply by the impact that points have on wins (players production which is credit due teammate's)
    assist_adjustment <- assist_adjustment %>%
        mutate(ast_val_team = points_credit_team * marg_values$`2FGM`)
    
    # Step 3.5 - Sum across all players of team and year and compute assist percentages for every player
    assist_adjustment <- assist_adjustment %>%
        group_by(team,year) %>%
        mutate(ast_total_val = sum(ast_val_team)) %>%
        mutate(ast_percentage = ast.x / ast.y) %>%
        mutate(eins = sum(ast_percentage)) %>%
        ungroup()
    
    # Step 3.6 - Allocate ass_total_val according to assist percentages from players
    # and calculate net assists for each player
    assist_adjustment <- assist_adjustment %>%
        mutate(ast_val_ind = ast_total_val * ast_percentage) %>%
        mutate(ast_net = ast_val_ind - ast_val_team)
    
    # Step 3.7 compute adjusted production and P48
    wins_produced <- wins_produced %>%
        mutate(ast_net = assist_adjustment$ast_net) %>% 
        mutate(PROD_adj2 = PROD_adj + assist_adjustment$ast_net) %>%
        mutate(P48Adj2 =if_else(min_p==0, 0, (PROD_adj2 / min_p) * 40))
    
    #******************************************************************************#
    # Step 4 - Team defense adjustments----
    # Calculating Team Turnovers
    team_adjustment <- player_totals %>%
        group_by(team,year) %>%
        mutate(TTOV = sum(tov)) %>%
        ungroup()
    
    team_adjustment <- merge(team_adjustment, team,
                             by = c("team","year"),
                             all = TRUE) %>% 
        arrange(player) %>%
        dplyr::select(1:3,min,min_p,opp_p3m,opp_p2m,opp_tov,REBTM,TTOV,blk.y,stl.y,tov.y) %>% 
        mutate(TeamTOV = tov.y - TTOV)
    
    team_adjustment <- team_adjustment %>%
        mutate(TOV_forced = opp_tov - stl.y)
    
    attach(team_adjustment)
    team_adjustment$Team_adj <- (((opp_p3m * marg_values$opp_3FGM +
                                       opp_p2m * marg_values$opp_2FGM +
                                       TOV_forced * marg_values$opp_TOV +
                                       TeamTOV * marg_values$TOV +
                                       REBTM * marg_values$TREB +
                                       blk.y * -marg_values$BLK)/ (min)) * 40)
    detach(team_adjustment)
    
    team_adjustment <- team_adjustment %>%
        group_by(year) %>%
        mutate(Team_adj_avg = mean(Team_adj)) %>%
        mutate(Team_defense_adj = Team_adj - Team_adj_avg) %>%
        ungroup()
    
    # Step 4.... compute adjusted P48
    wins_produced <- wins_produced %>%
        mutate(team_adj = team_adjustment$Team_defense_adj) %>% 
        mutate(P48Adj3 = P48Adj2 + team_adjustment$Team_defense_adj)
    
    #******************************************************************************#
    # Step 5 Adjusting for position played----
    wins_produced <- merge(wins_produced, pos_avg,
                           by = c("Pos.","year"))%>% 
        arrange(year,player) %>% 
        mutate(P48Adj4 = P48Adj3 - pos_mean,
               P48Adj5 = P48Adj4 + 0.099,
               PROD_adj_total = (P48Adj5) / 40 * min_p,
               wp_pm = PROD_adj_total / min_p) %>% 
        rename(wp_total = PROD_adj_total) %>% 
        dplyr::select(player,team,year,min_p,wp_total,wp_pm)
    
    return(wins_produced)
}

ws <- function(player,team){
    
    a <- merge(player,team, by = c("team","year"), suffix = c("_p","_t")) %>% 
        rename(min_t = min) %>% 
        filter(min_p > 0.0)
    
    #******************************************************************************#
    # Step 1 - Possessions ----
    # scoring possessions
    scoring_poss <- a %>% 
        mutate(qAST = ((min_p / (min_t / 5)) * (1.14 * ((ast_t - ast_p) / fgm_t))) + 
                   (ifelse(min_p == 1.6| min_p == 2.6,0.001,(((ast_t / min_t) * min_p * 5 - ast_p) / ((fgm_t / min_t) * min_p * 5 - fgm_p)) * (1 - (min_p / (min_t / 5))))),
               
               FG_Part = ifelse(fga_p>0,fgm_p * (1 - 0.5 * ((pts_p - ftm_p) / (2 * fga_p)) * qAST),0),
               
               AST_Part = 0.5 * (((pts_t - ftm_t) - (pts_p - ftm_p)) / (2 * (fga_t - fga_p))) * ast_p,
               
               FT_Part = ifelse(fta_p>0, (1-(1-(ftm_p/fta_p))^2)*0.4*fta_p,0),
               
               Team_Scoring_Poss = fgm_t + (1 - (1 - ifelse(fta_t>0,(ftm_t / fta_t),1))^2) * fta_t * 0.4,
               
               
               ORB_t_pct = orb_t / (orb_t + (opp_trb - opp_orb)),
               
               Team_Play_pct = Team_Scoring_Poss / (fga_t + fta_t * 0.4 + tov_t),
               
               ORB_t_Weight = ((1 - ORB_t_pct) * Team_Play_pct) / ((1 - ORB_t_pct) * Team_Play_pct + ORB_t_pct * (1 - Team_Play_pct)),
               
               ORB_Part = orb_p * ORB_t_Weight * Team_Play_pct,
               
               
               ScPoss = (FG_Part + AST_Part + FT_Part) * (1 - (orb_t / Team_Scoring_Poss) * ORB_t_Weight * Team_Play_pct) + ORB_Part
        )

    #******************************************************************************#
    # Missed FG and Missed FT Possessions
    tot_poss <- scoring_poss %>% 
        mutate(
            FGxPoss = (fga_p - fgm_p) * (1 - 1.07 * ORB_t_pct),
            FTxPoss = ifelse(fta_p>0, ((1 - (ftm_p / fta_p))^2) * 0.4 * fta_p, 0) 
        )
    
    # Total Possessions
    tot_poss <- tot_poss %>% 
        mutate(TotPoss = ScPoss + FGxPoss + FTxPoss + tov_p)
    
    totposs_t <- tot_poss %>%
        group_by(team,year) %>% 
        mutate(tot_poss_t = sum(TotPoss)) %>% 
        select(player,team,year,tot_poss_t,qAST,min_p,min_t)
    #******************************************************************************#
    # Step 2 - Offensive rating----
    # Individual Points Produced
    PTS_prod <- tot_poss %>% 
        mutate(
            PProd_FG_Part = ifelse(fga_p > 0, 2 * (fgm_p + 0.5 * p3m_p) * (1 - 0.5 * ((pts_p - ftm_p) / (2 * fga_p)) * qAST),0),
            PProd_AST_Part = 2 * ((fgm_t - fgm_p + 0.5 * (p3m_t - p3m_p)) / (fgm_t - fgm_p)) * 0.5 * (((pts_t - ftm_t) - (pts_p - ftm_p)) / (2 * (fga_t - fga_p))) * ast_p,
            PProd_ORB_Part = orb_p * ORB_t_Weight * Team_Play_pct * (pts_t / (fgm_t + (1 - (1- ifelse(fta_t>0,(ftm_t / fta_t),1))^2) * 0.4 * fta_t)),
            
            PProd = (PProd_FG_Part + PProd_AST_Part + ftm_p) * (1 - (orb_t / Team_Scoring_Poss) * ORB_t_Weight * Team_Play_pct) + PProd_ORB_Part)
    
    # Offensive rating
    oRTG <- PTS_prod %>% 
        mutate(ORtg = ifelse(PProd > 0 & TotPoss > 0, 100 * (PProd / TotPoss),0),
               poss_t = 0.5 *
                   ((fga_t + 0.4 * fta_t - 1.07 * (orb_t / (orb_t + opp_drb)) * (fga_t - fgm_t) + tov_t) +
                        (opp_fga + 0.4 * opp_fta - 1.07 * (opp_orb / (opp_orb + drb_t)) * (fga_t - opp_fgm) + opp_tov))
        )
    
    #******************************************************************************#
    # Step 3 - defensive rating----
    # Stop percentage
    poss_t <- a %>% 
        mutate(poss_t = 0.5 *
                   ((fga_t + 0.4 * fta_t - 1.07 * (orb_t / (orb_t + opp_drb)) * (fga_t - fgm_t) + tov_t) +
                        (opp_fga + 0.4 * opp_fta - 1.07 * (opp_orb / (opp_orb + drb_t)) * (fga_t - opp_fgm) + opp_tov))
        )
    
    stop_pct <- poss_t %>% 
        mutate(
            dor_pct = opp_orb / (opp_orb + drb_t),
            dfg_pct = opp_fgm / opp_fga,
            FMwt = (dfg_pct * (1 - dor_pct)) / (dfg_pct * (1 - dor_pct) + (1 - dfg_pct) * dor_pct),
            Stops1 = stl_p + blk_p * FMwt * (1 - 1.07 * dor_pct) + drb_p * (1 - FMwt),
            
            Stops2 = (((opp_fga - opp_fgm - blk_t) / min_t) * FMwt * (1 - 1.07 * dor_pct) +
                          ((opp_tov - stl_t) / min_t)) * min_p + (pf_p / pf_t) * 0.4 * opp_fta * (1 - ifelse(opp_fta>0,(opp_ftm / opp_fta),1))^2,
            
            Stops = Stops1 + Stops2,
            Stop_pct = (Stops * opp_min) / (poss_t * min_p)
        )
    
    #******************************************************************************#
    # Individual defensive rating
    dRTG <- stop_pct %>% 
        mutate(
            Team_Defensive_Rating = 100 * (opp_pts / poss_t),
            D_Pts_per_ScPoss = opp_pts / (opp_fgm + (1 - (1 - ifelse(opp_fta>0,(opp_ftm / opp_fta),1))^2) * opp_fta*0.4),
            
            DRtg = Team_Defensive_Rating + 0.2 * (100 * D_Pts_per_ScPoss * (1 - Stop_pct) - Team_Defensive_Rating)
        )
    
    #******************************************************************************#
    # Step 4 - League averages:----
    lg_avg <- poss_t %>% 
        group_by(year) %>% 
        mutate(avg_ppp_t = pts_t / poss_t,
               avg_ppg_t = pts_t / G_t,
               possible_min = mean(min_t) / G_t,
               pace_t = (possible_min/min_t)*(poss_t + poss_t)/2) %>%
        distinct(team, .keep_all = TRUE) %>% 
        mutate(ppp = mean(avg_ppp_t),
               ppg = mean(avg_ppg_t),
               pace = mean(pace_t)) %>% 
        distinct(ppp, .keep_all = TRUE) %>% 
        dplyr::select(year,ppp,ppg,pace)
    
    #******************************************************************************#
    # WIN SHARES:----
    # offensive win shares:----
    oRTG_merg <- merge(oRTG,lg_avg, by = c("year"))
    oWin_shares <- oRTG_merg %>% 
        mutate(marg_off = PProd - 0.92 * ppp * TotPoss,
               pace_t = (poss_t * 2 * 40) / (2 * min_t / 5),
               marg_ppw = 0.32 * ppg * (pace_t / pace),
               owin_share = marg_off / marg_ppw)
    
    #******************************************************************************#
    # defensive win shares----
    dRTG_merg <- merge(dRTG,lg_avg, by = c("year"))
    dWin_shares <- dRTG_merg %>% 
        mutate(
            marg_def = (min_p / min_t * poss_t)  * ((1.08 * ppp) - DRtg / 100),
            pace_t = (poss_t * 2 * 40) / (2 * min_t / 5),
            marg_ppw = 0.32 * ppg * (pace_t / pace),
            dwin_share = marg_def / marg_ppw)
    
    #******************************************************************************#
    # total win shares----
    win_shares_merg <- merge(oWin_shares,dWin_shares, by = c("year","team","player","min_p"))
    Win_Shares <- win_shares_merg %>% 
        mutate(win_shares = owin_share + dwin_share) %>% 
        rename(min_t = min_t.x) %>% 
        dplyr::select(player,team,year,min_p,owin_share,dwin_share,win_shares,min_t) %>% 
        arrange(-win_shares)
    
    return(Win_Shares)
}

BPM <- function(player,team,pos_role){
    # Merge data 
    player_stats <-merge(player,team, by = c("team","year"), suffix = c("_p","_t")) %>% 
        filter(min_p > 0.0) %>% 
        rename(min_t = min)
    
    # Step 1 - per 100 possessions----
    # adjusting for team shooting context:
    player <- player_stats %>% 
        mutate(tsa = fga_p + 0.44 * fta_p) %>% 
        mutate(pt_tsa = if_else(pts_p>0, pts_p / tsa,0))  %>% 
        mutate(pts_tsa_t = pts_t / (fga_t + fta_t* 0.44)) %>%
        mutate(pts_tsa_base = 1) %>% 
        mutate(adj.pts = ((pt_tsa - pts_tsa_t) + pts_tsa_base) * tsa)
    
    # calculate possessions:
    # Basketball-reference method:
    player <- player %>% 
        mutate(poss_t = 0.5*((fga_t+0.4*fta_t-1.07*(orb_t/(orb_t + opp_drb))*(fga_t-fgm_t)+tov_t)+
                                 (opp_fga+0.4*opp_fta-1.07*(opp_orb/(opp_orb+drb_t))*(opp_fga-opp_fgm)+opp_tov)),
               pace_t = 40 * (poss_t / (min_t/5))
        )
    
    player <- player %>% 
        mutate(poss_p = min_p * pace_t / 40)
    
    # # threshpts:
    # pts_Threshold <- -0.33
    # player <- player %>% 
    #     mutate(thresh_pts = tsa * (pt_tsa - (pts_tsa_t + pts_Threshold))) %>% 
    #     group_by(team,year) %>% 
    #     mutate(thresh_pts_t = sum(thresh_pts)) %>% 
    #     ungroup()
    
    # calculate stats_per100
    player <- player %>%
        mutate(pts_100 = adj.pts / poss_p * 100,
               p3m_100 = p3m_p / poss_p * 100, 
               p3a_100 = p3a_p / poss_p * 100,
               p2m_100 = p2m_p / poss_p * 100,
               p2a_100 = p2a_p / poss_p * 100,
               fgm_100 = fgm_p / poss_p * 100, 
               fga_100 = fga_p / poss_p * 100, 
               ftm_100 = ftm_p / poss_p * 100, 
               fta_100 = fta_p / poss_p * 100, 
               drb_100 = drb_p / poss_p * 100, 
               orb_100 = orb_p / poss_p * 100, 
               trb_100 = trb_p / poss_p * 100, 
               ast_100 = ast_p / poss_p * 100, 
               stl_100 = stl_p / poss_p * 100, 
               tov_100 = tov_p / poss_p * 100, 
               blk_100 = blk_p / poss_p * 100, 
               pf_100 = pf_p / poss_p * 100,
               min_pct = min_p / (min_t/5))
    
    #******************************************************************************#
    # Step 2 - Raw BPM ----
    bpm_coef <- read.xlsx("data/BPM_coef.xlsx", rowNames =  TRUE)
    var.names<-tolower(colnames(bpm_coef))
    colnames(bpm_coef)<-var.names
    
    player <- player %>% 
        mutate(ORtg_t = (pts_t / poss_t) * 100,
               DRtg_t = (opp_pts / poss_t) *100)
    
    
    player <- merge(player,pos_role,
                    by = c("player","team","year"), all = FALSE)
    
    BPM <- dplyr::select(player,
                         player,team,year,min_p,pts_100:min_pct,pos_final,
                         role_final,ORtg_t,DRtg_t,pace_t,G_t,G_tot)
    
    # coefficient depending on position and role
    BPM <- BPM %>% 
        mutate(BPM_pts_coef=(5-pos_final)/4*bpm_coef$adj_pts[1]+(pos_final-1)/4*bpm_coef$adj_pts[2],
               BPM_fga_coef=(5-role_final)/4*bpm_coef$fga[1]+(role_final-1)/4*bpm_coef$fga[2],
               BPM_fta=(5-role_final)/4*bpm_coef$fta[1]+(role_final-1)/4*bpm_coef$fta[2],
               BPM_p3m=(5-pos_final)/4*bpm_coef$`3pts_bonus`[1]+(pos_final-1)/4*bpm_coef$`3pts_bonus`[2],
               BPM_ast=(5-pos_final)/4*bpm_coef$ast[1]+(pos_final-1)/4*bpm_coef$ast[2],
               BPM_tov=(5-pos_final)/4*bpm_coef$to[1]+(pos_final-1)/4*bpm_coef$to[2],
               BPM_orb=(5-pos_final)/4*bpm_coef$orb[1]+(pos_final-1)/4*bpm_coef$orb[2],
               BPM_drb =(5-pos_final)/4*bpm_coef$drb[1]+(pos_final-1)/4*bpm_coef$drb[2],
               BPM_trb=(5-pos_final)/4*bpm_coef$trb[1]+(pos_final-1)/4*bpm_coef$trb[2],
               BPM_stl=(5-pos_final)/4*bpm_coef$stl[1]+(pos_final-1)/4*bpm_coef$stl[2],
               BPM_blk=(5-pos_final)/4*bpm_coef$blk[1]+(pos_final-1)/4*bpm_coef$blk[2],
               BPM_pf=(5-pos_final)/4*bpm_coef$pf[1]+(pos_final-1)/4*bpm_coef$pf[2]
        ) 
    
    # multiply with actual box score values:
    BPM <- BPM %>% 
        mutate(scoring = pts_100 * BPM_pts_coef + 
                   fga_100 * BPM_fga_coef +
                   fta_100 * BPM_fta +
                   p3m_100 * BPM_p3m,
               ballhandling = 
                   ast_100 * BPM_ast +
                   tov_100 * BPM_tov,
               rebounding =
                   orb_100 * BPM_orb +
                   drb_100 * BPM_drb +
                   trb_100 * BPM_trb,
               defense = 
                   stl_100 * BPM_stl +
                   blk_100 * BPM_blk +
                   pf_100 * BPM_pf)
    
    # add positional and role constant
    # load position regression
    BPM_pos_coef <- read.xlsx("data/BPM_pos_coef.xlsx", rowNames = FALSE)
    BPM_pos_coef
    
    BPM <- BPM %>% 
        mutate(pos_cons = case_when(
            pos_final >= 3 ~ 0,
            pos_final < 3 ~ (3 - pos_final) * (BPM_pos_coef$Pos_1/2)),
            role_cons = (role_final - 3) * BPM_pos_coef$off_role_slope,
            constant = pos_cons + role_cons)
    
    # raw BPM CALCULATION
    BPM <- BPM %>% 
        mutate(raw_BPM = scoring + ballhandling + rebounding + defense + constant,
               contribution = raw_BPM * min_pct)
    
    #******************************************************************************#
    # Step 3 - team adjustment:----
    # team ratings
    BPM <- BPM %>% 
        group_by(team,year) %>% 
        mutate(contr_team = sum(contribution)) %>% 
        ungroup() %>% 
        group_by(year) %>% 
        mutate(avg_Rtg = mean(ORtg_t)) %>% 
        ungroup() %>% 
        mutate(adj_ORtg = ORtg_t - avg_Rtg,
               adj_DRtg = avg_Rtg - DRtg_t,
               adj_netRtg = adj_ORtg + adj_DRtg)
    
    # average lead and adj_team_Rtg
    BPM <- BPM %>% 
        mutate(avg_lead = pace_t * adj_netRtg / 100 / 2,
               lead_bonus = 0.35 / 2 * avg_lead,
               adj_team_Rtg = adj_netRtg + lead_bonus)
    
    # team adjustment
    BPM <- BPM %>% 
        mutate(team_adj = (adj_team_Rtg - contr_team) / 5) %>% 
        mutate(BPM = raw_BPM + team_adj,
               VORP = (BPM + 2) * min_pct * G_t / G_tot)
    
    
    #******************************************************************************#
    # Step 4 - regression to the mean for players with small minutes:----
    BPM <- BPM %>%
        mutate(reg_min_PG = min_p,
               reg_min = pmax((100 - min_p)/3,0),
               exp_BPM = -4.75 + 0.175 * reg_min_PG,
               reg_BPM = (BPM * min_p + exp_BPM * reg_min) / (min_p + reg_min),
               reg_VORP = (reg_BPM + 2) * min_pct)
    
    return(BPM)
}