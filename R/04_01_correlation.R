# Clear Console
cat("\014")

# remove Environment
rm(list=ls())


library(tidyverse)

ranking <- readRDS("Data/estimates/ranking.Rds")

#******************************************************************************#
# Correlation matrix (Pearson):----
cor.pearson <- ranking %>% 
    dplyr::select(VORP,win_shares, wp_A, wp_B, nba_eff,bbl_eff,min_p) %>% 
    filter(min_p > 250) %>% 
    dplyr::select(-min_p) %>% 
    cor(.) %>% 
    round(.,3)

upper.p<-cor.pearson
upper.p[upper.tri(cor.pearson)]<-""
upper.p<-as.data.frame(upper.p)

#require(xtable)
#print(xtable(upper))       # for latex table!!!
require(knitr)
kable(upper.p[1:6, ], align = "c")

#******************************************************************************#
# Correlation matrix (Spearman):----
cor.spearman <- ranking %>% 
    dplyr::select(VORP,win_shares, wp_A, wp_B, nba_eff,bbl_eff,min_p) %>% 
    filter(min_p > 250) %>% 
    dplyr::select(-min_p) %>% 
    cor(., method = "spearman") %>% 
    round(.,3)

upper.s<-cor.spearman
upper.s[upper.tri(cor.spearman)]<-""
upper.s<-as.data.frame(upper.s)

kable(upper.s[1:6, ], align = "c")
