# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# set working directory
setwd("~/Uni Tübingen/0. Masterarbeit/7. R/europeanBasketball")
source('functions/metrics_functions2.R')

# load packages
library(tidyverse, warn.conflicts = FALSE)
library(Metrics)
#******************************************************************************#
# load rmse files
rmse_files = paste0("Data/prediction/rmse_", 2010:2020, ".Rds")
name <- gsub("\\.Rds$", "", rmse_files) %>% 
    gsub("Data/prediction", "", .)
rmse_data <- lapply(rmse_files, readRDS)
names(rmse_data) <- gsub("\\.Rds$", "", name)

rmse <- bind_rows(rmse_data)

#******************************************************************************#
# slice data into 2 categories
fraction <- 0.5
low_stab <- rmse %>% 
    slice_min(., order_by = stability, prop = fraction)

high_stab <- rmse %>% 
    slice_max(., order_by = stability, prop = fraction)

rmse_low_stab <- low_stab %>% 
    mutate(WP = rmse(adj_netRtg,wp_pd_pg),
           WS = rmse(adj_netRtg,ws_pd_pg),
           BPM= rmse(adj_netRtg,VORP)) %>% 
    distinct(WP,WS,BPM)

rmse_high_stab <- high_stab %>% 
    mutate(WP = rmse(adj_netRtg,wp_pd_pg),
           WS = rmse(adj_netRtg,ws_pd_pg),
           BPM= rmse(adj_netRtg,VORP)) %>% 
    distinct(WP,WS,BPM)
