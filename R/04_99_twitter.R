# Clear Console
cat("\014")

# remove Environment
rm(list=ls())


library(tidyverse)

ranking <- readRDS("Data/estimates/ranking.Rds")

#******************************************************************************#
# Top25 2020-2021
rank_2020 <- rank_y %>% 
    filter(year == 2020)%>% 
    arrange(VORP_rank_y)

top25 <- rank_2020 %>% 
    filter(., VORP_rank_y <= 25)
require(gridExtra)
pdf("test.pdf", height=20, width=15)
grid.table(top25)
dev.off()