#### functions for player actions in pbp data
# 3 pointers
get_p3a <- function(pbp, team){
    set_team <- team
    p3a <- nrow(dplyr::filter(pbp, aktion=="P3" & teamcode==set_team))
    return(p3a)
}
get_p3m <- function(pbp, team){
    set_team <- team
    p3m <- nrow(dplyr::filter(pbp, aktion=="P3" & teamcode==set_team & resultat=="+"))
    return(p3m)
}

# 2 pointers
get_p2a <- function(pbp, team){
    set_team <- team
    p3a <- nrow(dplyr::filter(pbp, aktion=="P2" & teamcode==set_team))
    return(p2a)
}
get_p2m <- function(pbp, team){
    set_team <- team
    p2m <- nrow(dplyr::filter(pbp, aktion=="P2" & teamcode==set_team & resultat=="+"))
    return(p2m)
}

# free throws
get_fta <- function(pbp, team){
    set_team <- team
    fta <- nrow(dplyr::filter(pbp, aktion=="FT" & teamcode==set_team))
    return(fta)
}
get_ftm <- function(pbp, team){
    set_team <- team
    ftm <- nrow(dplyr::filter(pbp, aktion=="FT" & teamcode==set_team & resultat=="+"))
    return(ftm)
}

# Fouls
get_pf <- function(pbp, team){
    set_team <- team
    pf <- nrow(dplyr::filter(pbp, aktion=="FOUL" & teamcode==set_team))
    return(pf)
}
get_pfd <- function(pbp, team){
    set_team <- team
    pfd <- nrow(dplyr::filter(pbp, aktion=="RFOUL" & teamcode==set_team))
    return(pfd)
}

# Rebounds
get_trb <- function(pbp, team){
    set_team <- team
    trb <- nrow(dplyr::filter(pbp, aktion=="REB" & teamcode==set_team))
    return(trb)
}
get_rb_team <- function(pbp, team){
    set_team <- team
    rb_team <- nrow(dplyr::filter(pbp, aktion=="TREB" & teamcode==set_team))
    return(rb_team)
}

# Turnover
get_tov  <- function(pbp, team){
    set_team <- team
    tov <- nrow(dplyr::filter(pbp, aktion=="TO" & teamcode==set_team))
    return(tov)
}
# team turnover
get_ttov <- function(pbp, team){
    set_team <- team
    ttov <- nrow(dplyr::filter(pbp, aktion=="TTO" & teamcode==set_team))
    return(ttov)
}

# Steals
get_stl <- function(pbp, team){
    set_team <- team
    stl <- nrow(dplyr::filter(pbp, aktion=="ST" & teamcode==set_team))
    return(stl)
}

# blocks
get_blk <- function(pbp, team){
    set_team <- team
    blk <- nrow(dplyr::filter(pbp, aktion=="BS" & teamcode==set_team))
    return(blk)
}
# lineup changes
get_subst <- function(pbp, team){
    set_team <- team
    subst <- nrow(dplyr::filter(pbp, aktion=="SUBST" & teamcode==set_team))
    return(subst)
}
