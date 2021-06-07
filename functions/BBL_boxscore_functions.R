#### functions for team actions in pbp data----
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
    tov_1 <- nrow(filter(pbp, aktion=="TO" | aktion=="TTO",
                         club_1==set_team))
    tov_2 <- nrow(filter(pbp, (aktion=="FOUL" & zusatzinfo_1=="O"),
                         club_1==set_team))
    tov <- tov_1 + tov_2
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
# assists
get_ast <- function(pbp, team){
    ast <- nrow(filter(pbp, sn_Spieler_2 !="",
                       aktion != "SUBST",
                       aktion != "JB",
                       aktion != "FT",
                       Club_2 ==team))
    return(ast)
}

#### functions for player actions in pbp data----
# 3 pointers
get_p3a_p <- function(pbp, player){
    p3a <- nrow(filter(pbp, aktion=="P3", Player_1==player))
    return(p3a)
}
get_p3m_p <- function(pbp, player){
    p3m <- nrow(filter(pbp, aktion=="P3",
                       Player_1==player,
                       resultat=="+"))
    return(p3m)
}
# 2 pointers
get_p2a_p <- function(pbp, player){
    p2a <- nrow(filter(pbp, aktion=="P2", Player_1==player))
    return(p2a)
}
get_p2m_p <- function(pbp, player){
    p2m <- nrow(filter(pbp, aktion=="P2",
                       Player_1==player,
                       resultat=="+"))
    return(p2m)
}
# free throws
get_fta_p <- function(pbp, player){
    fta <- nrow(filter(pbp, aktion=="FT",
                       Player_1==player))
    return(fta)
}
get_ftm_p <- function(pbp, player){
    ftm <- nrow(filter(pbp, aktion=="FT",
                       Player_1==player,
                       resultat=="+"))
    return(ftm)
}
# Fouls
get_pf_p <- function(pbp, player){
    pf <- nrow(filter(pbp, aktion=="FOUL",
                      Player_1==player))
    return(pf)
}
get_pfd_p <- function(pbp, player){
    pfd <- nrow(filter(pbp, aktion=="RFOUL",
                       Player_1==player))
    return(pfd)
}
# total rebounds by player
get_trb_p <- function(pbp, player){
    trb <- nrow(filter(pbp, aktion=="REB",
                       Player_1==player))
    return(trb)
}
# offensive rebounds
get_orb_p <- function(pbp, player){
    orb <- nrow(filter(pbp, aktion=="REB",
                       Player_1==player,
                       zusatzinfo_1 =="O"))
    return(orb)
}
# defensive rebounds
get_drb_p <- function(pbp, player){
    drb <- nrow(filter(pbp, aktion=="REB",
                       Player_1==player,
                       zusatzinfo_1 =="D"))
    return(drb)
}
# Turnover
get_tov_p  <- function(pbp, player){
    tov <- nrow(filter(pbp, aktion=="TO" | (aktion=="FOUL" & zusatzinfo_1=="O"),
                       Player_1==player))
    return(tov)
}
# Steals
get_stl_p <- function(pbp, player){
    stl <- nrow(filter(pbp, aktion=="ST",
                       Player_1==player))
    return(stl)
}
# blocks
get_blk_p <- function(pbp, player){
    blk <- nrow(filter(pbp, aktion=="BS",
                       Player_1==player))
    return(blk)
}
# assists
get_ast_p <- function(pbp, player){
    ast <- nrow(filter(pbp, Player_2==player,
                       aktion != "SUBST",
                       aktion != "FT",
                       aktion != "JB"))
    return(ast)
}