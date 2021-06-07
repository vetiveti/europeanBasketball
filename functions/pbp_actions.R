get_boxscore_team <- function(pbp,team_h,team_a){
    source('functions/BBL_boxscore_functions.R')
    team <- team_h
    
    min <- get_min(pbp, team)
    p2a <- get_p2a(pbp, team)
    p2m <- get_p2m(pbp, team)
    p3a <- get_p3a(pbp, team)
    p3m <- get_p3m(pbp, team)
    fta <- get_fta(pbp, team)
    ftm <- get_ftm(pbp, team)
    rb_team <- get_rb_team(pbp, team)
    orb <- get_orb(pbp, team)
    drb <- get_drb(pbp, team)
    trb <- get_trb(pbp, team)
    ast <- get_ast(pbp, team)
    stl <- get_stl(pbp, team)
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
                               orb = orb,
                               drb = drb,
                               rb_team = rb_team,
                               trb = trb,
                               ast = ast,
                               stl = stl,
                               tov = tov,
                               blk = blk,
                               pf = pf,
                               pfd = pfd,
                               pts = p3m * 3 + p2m * 2+ ftm)
    
    ## Opponent stats:
    team <- team_a
    
    min <- get_min(pbp, team)
    p2a <- get_p2a(pbp, team)
    p2m <- get_p2m(pbp, team)
    p3a <- get_p3a(pbp, team)
    p3m <- get_p3m(pbp, team)
    fta <- get_fta(pbp, team)
    ftm <- get_ftm(pbp, team)
    orb <- get_orb(pbp, team)
    drb <- get_drb(pbp, team)
    rb_team <- get_rb_team(pbp, team)
    trb <- get_trb(pbp, team)
    stl <- get_stl(pbp, team)
    tov <- get_tov(pbp, team)
    blk <- get_blk(pbp, team)
    pf <- get_pf(pbp, team)
    pfd <- get_pfd(pbp, team)
    ast <- get_ast(pbp, team)
    
    boxscore_team_xy <- boxscore_team_xy %>% 
        add_column(opp_min = min,
                   opp_p2a = p2a,
                   opp_p2m = p2m,
                   opp_p3a = p3a,
                   opp_p3m = p3m,
                   opp_fta = fta,
                   opp_ftm = ftm,
                   opp_orb = orb,
                   opp_drb = drb,
                   opp_rb_team = rb_team,
                   opp_trb = trb,
                   opp_ast = ast,
                   opp_stl = stl,
                   opp_tov = tov,
                   opp_blk = blk,
                   opp_pf = pf,
                   opp_pfd = pfd,
                   opp_pts = p3m * 3 + p2m * 2 + ftm)
    
    return(boxscore_team_xy)
}

get_boxscore_player <- function(pbp,player){
    source('functions/BBL_boxscore_functions.R')
    p2a <- get_p2a_p(pbp, player)
    p2m <- get_p2m_p(pbp, player)
    p3a <- get_p3a_p(pbp, player)
    p3m <- get_p3m_p(pbp, player)
    fta <- get_fta_p(pbp, player)
    ftm <- get_ftm_p(pbp, player)
    trb <- get_trb_p(pbp, player)
    orb <- get_orb_p(pbp, player)
    drb <- get_drb_p(pbp, player)
    stl <- get_stl_p(pbp, player)
    tov <- get_tov_p(pbp, player)
    blk <- get_blk_p(pbp, player)
    pf <- get_pf_p(pbp, player)
    pfd <- get_pfd_p(pbp, player)
    ast <- get_ast_p(pbp, player)
    
    boxscore_player_xy <- tibble(stats = player,
                               p2a = p2a,
                               p2m = p2m,
                               p3a = p3a,
                               p3m = p3m,
                               fta = fta,
                               ftm = ftm,
                               orb = orb,
                               drb = drb,
                               trb = trb,
                               ast = ast,
                               stl = stl,
                               tov = tov,
                               blk = blk,
                               pf = pf,
                               pfd = pfd,
                               pts = p3m * 3 + p2m * 2 + ftm)
    return(boxscore_player_xy)
}

first.changes <- function(d) {
    a <- rle(d)
    p <- cumsum(a$lengths) + 1
    p[-length(p)]
    
    q <- lag(p)
    q[1] <- 1
    
    ply <- a$values
    
    
    z <- tibble(player = ply,start = q, end = p)
    
}