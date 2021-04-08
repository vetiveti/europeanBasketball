#### functions for player actions in pbp data
# 3 pointers
get_p3a <- function(pbp, team){
    set_team <- team
    p3a <- nrow(filter(pbp, aktion=="P3",
                       club_1==set_team))
    return(p3a)
}
get_p3m <- function(pbp, team){
    set_team <- team
    p3m <- nrow(filter(pbp, aktion=="P3",
                       club_1==set_team,
                       resultat=="+"))
    return(p3m)
}
# 2 pointers
get_p2a <- function(pbp, team){
    set_team <- team
    p2a <- nrow(filter(pbp, aktion=="P2",
                       club_1==set_team))
    return(p2a)
}
get_p2m <- function(pbp, team){
    set_team <- team
    p2m <- nrow(filter(pbp, aktion=="P2",
                       club_1==set_team,
                       resultat=="+"))
    return(p2m)
}
# free throws
get_fta <- function(pbp, team){
    set_team <- team
    fta <- nrow(filter(pbp, aktion=="FT",
                       club_1==set_team))
    return(fta)
}
get_ftm <- function(pbp, team){
    set_team <- team
    ftm <- nrow(filter(pbp, aktion=="FT",
                       club_1==set_team,
                       resultat=="+"))
    return(ftm)
}
# Fouls
get_pf <- function(pbp, team){
    set_team <- team
    pf <- nrow(filter(pbp, aktion=="FOUL",
                      club_1==set_team))
    return(pf)
}
get_pfd <- function(pbp, team){
    set_team <- team
    pfd <- nrow(filter(pbp, aktion=="RFOUL",
                       club_1==set_team))
    return(pfd)
}
# Rebounds by players
get_reb <- function(pbp, team){
    set_team <- team
    reb <- nrow(filter(pbp, aktion=="REB",
                       club_1==set_team))
    return(reb)
}
# Rebounds by team
get_rb_team <- function(pbp, team){
    set_team <- team
    rb_team <- nrow(filter(pbp, aktion=="TREB",
                           club_1==set_team))
    return(rb_team)
}
# Total # of rebounds
get_trb <- function(pbp, team){
    set_team <- team
    trb <- nrow(filter(pbp, aktion=="REB" | aktion=="TREB",
                       club_1==set_team))
    return(trb)
}
# offensive rebounds
get_orb <- function(pbp, team){
    set_team <- team
    orb <- nrow(filter(pbp, aktion=="REB" | aktion=="TREB",
                       club_1==set_team,
                       zusatzinfo_1 =="O"))
    return(orb)
}
# defensive rebounds
get_drb <- function(pbp, team){
    set_team <- team
    drb <- nrow(filter(pbp, aktion=="REB" | aktion=="TREB",
                       club_1==set_team,
                       zusatzinfo_1 =="D"))
    return(drb)
}
# Turnover
get_tov  <- function(pbp, team){
    set_team <- team
    tov <- nrow(filter(pbp, aktion=="TO" | aktion=="TTO",
                       club_1==set_team))
    return(tov)
}
# team turnover
get_ttov <- function(pbp, team){
    set_team <- team
    ttov <- nrow(filter(pbp, aktion=="TTO",
                        club_1==set_team))
    return(ttov)
}
# Steals
get_stl <- function(pbp, team){
    set_team <- team
    stl <- nrow(filter(pbp, aktion=="ST",
                       club_1==set_team))
    return(stl)
}
# blocks
get_blk <- function(pbp, team){
    set_team <- team
    blk <- nrow(filter(pbp, aktion=="BS",
                       club_1==set_team))
    return(blk)
}
# lineup changes
get_subst <- function(pbp, team){
    set_team <- team
    subst <- nrow(filter(pbp, aktion=="SUBST",
                         club_1==set_team))
    return(subst)
}
# time 
get_min <- function(pbp, team){
    set_team <- team
    min <- max(pbp$spielzeit_sec) * max(pbp$quarter)
    return(min)
}

get_boxscore_team <- function(pbp,team_h,team_a){
    team <- team_h
    
    min <- get_min(pbp, team)
    p2a <- get_p2a(pbp, team)
    p2m <- get_p2m(pbp, team)
    p3a <- get_p3a(pbp, team)
    p3m <- get_p3m(pbp, team)
    fta <- get_fta(pbp, team)
    ftm <- get_ftm(pbp, team)
    # zusammen = total rebounds
    #reb <- get_reb(pbp, team)
    #rb_team <- get_rb_team(pbp, team)
    # oder alleine
    trb <- get_trb(pbp, team)
    # 
    orb <- get_orb(pbp, team)
    drb <- get_drb(pbp, team)
    stl <- get_stl(pbp, team)
    # turnover stimmen noch nicht... 
    # es fehlen welche.. wo sind die hin?
    tov <- get_tov(pbp, team)
    blk <- get_blk(pbp, team)
    pf <- get_pf(pbp, team)
    pfd <- get_pfd(pbp, team)
    
    boxscore_team_xy <- tibble(stats = team_h,
                               min = min,
                               p2a = p2a,
                               p2m = p2m,
                               p3a = p3a,
                               p3m = p3m,
                               fta = fta,
                               ftm = ftm,
                               trb = trb,
                               orb = orb,
                               drb = drb,
                               stl = stl,
                               tov = tov,
                               blk = blk,
                               pf = pf,
                               pfd = pfd)
    
    ## Opponent stats:
    team <- team_a
    
    min <- get_min(pbp, team)
    p2a <- get_p2a(pbp, team)
    p2m <- get_p2m(pbp, team)
    p3a <- get_p3a(pbp, team)
    p3m <- get_p3m(pbp, team)
    fta <- get_fta(pbp, team)
    ftm <- get_ftm(pbp, team)
    # zusammen = total rebounds
    #reb <- get_reb(pbp, team)
    #rb_team <- get_rb_team(pbp, team)
    # oder alleine
    trb <- get_trb(pbp, team)
    # 
    orb <- get_orb(pbp, team)
    drb <- get_drb(pbp, team)
    stl <- get_stl(pbp, team)
    # turnover stimmen noch nicht... 
    # es fehlen welche.. wo sind die hin?
    tov <- get_tov(pbp, team)
    blk <- get_blk(pbp, team)
    pf <- get_pf(pbp, team)
    pfd <- get_pfd(pbp, team)
    
    boxscore_team_xy <- boxscore_team_xy %>% 
        add_column(opp_min = min,
                   opp_p2a = p2a,
                   opp_p2m = p2m,
                   opp_p3a = p3a,
                   opp_p3m = p3m,
                   opp_fta = fta,
                   opp_ftm = ftm,
                   opp_trb = trb,
                   opp_orb = orb,
                   opp_drb = drb,
                   opp_stl = stl,
                   opp_tov = tov,
                   opp_blk = blk,
                   opp_pf = pf,
                   opp_pfd = pfd)
    
    return(boxscore_team_xy)
}