# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

# source functions to use
source('functions/BBL_functions.R')
library(tidyverse, warn.conflicts = FALSE)

#******************************************************************************#
# Load roster files:----
roster_files = paste0("Data/rosters_", 2008:2020, ".Rds")
name <- gsub("\\.Rds$", "", roster_files) %>% 
    gsub("Data/", "", .)
roster_data <- lapply(roster_files, readRDS)
names(roster_data) <- gsub("\\.Rds$", "", name)

#******************************************************************************#
# load identifiers: ----
game_id_files = paste0("Data/identifiers_", 2008:2020, ".Rds")
name <- gsub("\\.Rds$", "", game_id_files) %>% 
    gsub("Data/", "", .)
game_id_data <- lapply(game_id_files, readRDS)
names(game_id_data) <- gsub("\\.Rds$", "", name)

# get results safely, because some games have no data
safer_results <- possibly(get_pbp, otherwise = as_tibble("Error finding file"))

#******************************************************************************#
# Load play by play files: ----
pbp_files = paste0("Data/pbp", 2008:2020, ".Rds")
name <- gsub("\\.Rds$", "", pbp_files) %>% 
    gsub("Data/", "", .)
pbp_data <- lapply(pbp_files, readRDS)
names(pbp_data) <- gsub("\\.Rds$", "", name)

# YEAR BY YEAR:  ----
#******************************************************************************#
# Clean pbp data 2010: ----
# prepare game ids
id <- game_id_data$identifiers_2010

# prepare pbp data
pbp <- pbp_data$pbp2010 %>% 
    arrange(desc(spielzeit_sec )) %>% 
    arrange(game_nr)

# prepare roster data
roster <- roster_data$rosters_2010 %>% 
    type_convert() %>%
    drop_na(.,Club)
unique(roster$Club)

id_merge <- bind_cols(id, unique(pbp$game_nr)) %>% 
    rename(nr = `...3`)

pbp <- merge(pbp,id_merge,
             by.x = "game_nr",
             by.y = "nr") %>% 
    select(-game_nr)

# clean for games which have no files
pbp <- pbp %>%
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")

pbp <- filter(pbp,
              (game_id != 8684 | quarter != 5),
              (game_id != 8792 | quarter != 5),
              )

#******************************************************************************#
# Save cleaned pbp files 2010:
saveRDS(object = pbp, file = paste0("Data/clean_pbp/pbp_2010",".Rds"))

#******************************************************************************#
# Clean pbp data 2011: ----
# prepare game ids
id <- game_id_data$identifiers_2011

# prepare pbp data
pbp <- pbp_data$pbp2011 %>% 
    arrange(desc(spielzeit_sec )) %>% 
    arrange(game_nr)

# prepare roster data
roster <- roster_data$rosters_2011 %>% 
    type_convert() %>%
    drop_na(.,Club)
unique(roster$Club)

id_merge <- bind_cols(id, unique(pbp$game_nr)) %>% 
    rename(nr = `...3`)

pbp <- merge(pbp,id_merge,
             by.x = "game_nr",
             by.y = "nr") %>% 
    select(-game_nr)

# clean for games which have no files
pbp <- pbp %>%
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")

pbp <- filter(pbp,
              (game_id != 11051 | quarter != 5),
              (game_id != 11130 | quarter != 5),
              (game_id != 11155 | quarter != 5),
              (game_id != 11158 | quarter != 5),
              (game_id != 11169 | quarter != 5))

#******************************************************************************#
# Save cleaned pbp files 2011:
saveRDS(object = pbp, file = paste0("Data/clean_pbp/pbp_2011",".Rds"))

#******************************************************************************#
# Clean pbp data 2012: ----
# prepare game ids
id <- game_id_data$identifiers_2012

# prepare pbp data
pbp <- pbp_data$pbp2012 %>% 
    arrange(desc(spielzeit_sec )) %>% 
    arrange(game_nr)

# prepare roster data
roster <- roster_data$rosters_2012 %>% 
    type_convert() %>%
    drop_na(.,Club)
unique(roster$Club)

id_merge <- bind_cols(id, unique(pbp$game_nr)) %>% 
    rename(nr = `...3`)

pbp <- merge(pbp,id_merge,
             by.x = "game_nr",
             by.y = "nr") %>% 
    select(-game_nr)

# clean for games which have no files
pbp <- pbp %>%
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")

pbp <- filter(pbp,
              (game_id != 13332 | quarter != 5),
              (game_id != 13375 | quarter != 5),
              (game_id != 13521 | quarter != 5),
              (game_id != 13556 | quarter != 6))

#******************************************************************************#
# Save cleaned pbp files 2012:
saveRDS(object = pbp, file = paste0("Data/clean_pbp/pbp_2012",".Rds"))

#******************************************************************************#
# Clean pbp data 2013: ----
# prepare game ids
id <- game_id_data$identifiers_2013

# prepare pbp data
pbp <- pbp_data$pbp2013 %>% 
    arrange(desc(spielzeit_sec )) %>% 
    arrange(game_nr)

# prepare roster data
roster <- roster_data$rosters_2013 %>% 
    type_convert() %>%
    drop_na(.,Club)
unique(roster$Club)

id_merge <- bind_cols(id, unique(pbp$game_nr)) %>% 
    rename(nr = `...3`)

pbp <- merge(pbp,id_merge,
             by.x = "game_nr",
             by.y = "nr") %>% 
    select(-game_nr)

# clean for games which have no files
pbp <- pbp %>%
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")

#******************************************************************************#
# Save cleaned pbp files 2013:
saveRDS(object = pbp, file = paste0("Data/clean_pbp/pbp_2013",".Rds"))

#******************************************************************************#
# Clean pbp data 2014: ----
# prepare game ids
id <- game_id_data$identifiers_2014

# prepare pbp data
pbp <- pbp_data$pbp2014 %>% 
    arrange(desc(spielzeit_sec )) %>% 
    arrange(game_nr)

# prepare roster data
roster <- roster_data$rosters_2014 %>% 
    type_convert() %>%
    drop_na(.,Club)
unique(roster$Club)

id_merge <- bind_cols(id, unique(pbp$game_nr)) %>% 
    rename(nr = `...3`)

pbp <- merge(pbp,id_merge,
             by.x = "game_nr",
             by.y = "nr") %>% 
    select(-game_nr)

# clean for games which have no files
pbp <- pbp %>%
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")

#******************************************************************************#
# Save cleaned pbp files 2014:
saveRDS(object = pbp, file = paste0("Data/clean_pbp/pbp_2014",".Rds"))

#******************************************************************************#
# Clean pbp data 2015: ----
# prepare game ids
id <- game_id_data$identifiers_2015

# prepare pbp data
pbp <- pbp_data$pbp2015 %>% 
    arrange(desc(spielzeit_sec )) %>% 
    arrange(game_nr)

# prepare roster data
roster <- roster_data$rosters_2015 %>% 
    type_convert() %>%
    drop_na(.,Club)
unique(roster$Club)

id_merge <- bind_cols(id, unique(pbp$game_nr)) %>% 
    rename(nr = `...3`)

pbp <- merge(pbp,id_merge,
             by.x = "game_nr",
             by.y = "nr") %>% 
    select(-game_nr)

# clean for games which have no files
pbp <- pbp %>%
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")

pbp <- filter(pbp,
              (game_id != 18393 | quarter != 5))

#******************************************************************************#
# Save cleaned pbp files 2015:
saveRDS(object = pbp, file = paste0("Data/clean_pbp/pbp_2015",".Rds"))

#******************************************************************************#
# Clean pbp data 2016: ----
# prepare game ids
id <- game_id_data$identifiers_2016

# prepare pbp data
pbp <- pbp_data$pbp2016 %>% 
    arrange(desc(spielzeit_sec )) %>% 
    arrange(game_nr)

# prepare roster data
roster <- roster_data$rosters_2016 %>% 
    type_convert() %>%
    drop_na(.,Club)
unique(roster$Club)

id_merge <- bind_cols(id, unique(pbp$game_nr)) %>% 
    rename(nr = `...3`)

pbp <- merge(pbp,id_merge,
             by.x = "game_nr",
             by.y = "nr") %>% 
    select(-game_nr)

# clean for games which have no files
pbp <- pbp %>%
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")

pbp <- filter(pbp,
              (game_id != 19619 | quarter != 5))

#******************************************************************************#
# Save cleaned pbp files 2016:
saveRDS(object = pbp, file = paste0("Data/clean_pbp/pbp_2016",".Rds"))

#******************************************************************************#
# Clean pbp data 2017: ----
# prepare game ids
id <- game_id_data$identifiers_2017 %>% 
    filter(.,game_id != 20881 & game_id != 20885)

# prepare pbp data
pbp <- pbp_data$pbp2017 %>% 
    arrange(desc(spielzeit_sec )) %>% 
    arrange(game_nr) %>% 
    filter(.,game_nr != 55 & game_nr != 60)  

# prepare roster data
roster <- roster_data$rosters_2017 %>% 
    type_convert() %>%
    drop_na(.,Club)
unique(roster$Club)

roster$Club[roster$Club == "s.Oliver Würzurg"] <- "s.Oliver Würzburg"
roster$Club[roster$Club == "Oettinger Rockets"] <- "Rockets"
roster <- roster %>% 
    filter(.,game_nr != 20881 & game_nr != 20885)
unique(roster$Club)

id_merge <- bind_cols(id, unique(pbp$game_nr)) %>% 
    rename(nr = `...3`)

pbp <- merge(pbp,id_merge,
             by.x = "game_nr",
             by.y = "nr") %>% 
    select(-game_nr)

# clean for games which have no files
pbp <- pbp %>%
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")

#******************************************************************************#
# Save cleaned pbp files 2017:
saveRDS(object = pbp, file = paste0("Data/clean_pbp/pbp_2017",".Rds"))

#******************************************************************************#
# Clean pbp data 2018: ----
# prepare game ids
id <- game_id_data$identifiers_2018

# prepare pbp data
pbp <- pbp_data$pbp2018 %>% 
    arrange(desc(spielzeit_sec )) %>% 
    arrange(game_nr)

# prepare roster data
roster <- roster_data$rosters_2018 %>% 
    type_convert() %>%
    drop_na(.,Club)
unique(roster$Club)
roster$Club[roster$Club == "SYNTAINICS MBC"] <- "Mitteldeutscher BC"
unique(roster$Club)

id_merge <- bind_cols(id, unique(pbp$game_nr)) %>% 
    rename(nr = `...3`)

pbp <- merge(pbp,id_merge,
             by.x = "game_nr",
             by.y = "nr") %>% 
    select(-game_nr)

# clean for games which have no files
pbp <- pbp %>%
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")

#******************************************************************************#
# Save cleaned pbp files 2018:
saveRDS(object = pbp, file = paste0("Data/clean_pbp/pbp_2018",".Rds"))

#******************************************************************************#
# Clean pbp data 2019: ----
# prepare game ids
id <- game_id_data$identifiers_2019

# prepare pbp data
pbp <- pbp_data$pbp2019 %>% 
    arrange(desc(spielzeit_sec )) %>% 
    arrange(game_nr)

# prepare roster data
roster <- roster_data$rosters_2019 %>% 
    type_convert() %>%
    drop_na(.,Club)
unique(roster$Club)
roster$Club[roster$Club == "SYNTAINICS MBC"] <- "Mitteldeutscher BC"
unique(roster$Club)

id_merge <- bind_cols(id, unique(pbp$game_nr)) %>% 
    rename(nr = `...3`)

pbp <- merge(pbp,id_merge,
             by.x = "game_nr",
             by.y = "nr") %>% 
    dplyr::select(-game_nr)

# clean for games which have no files
pbp <- pbp %>%
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")

#******************************************************************************#
# Save cleaned pbp files 2019:
saveRDS(object = pbp, file = paste0("Data/clean_pbp/pbp_2019",".Rds"))

#******************************************************************************#
#******************************************************************************#
# Clean pbp data 2020: ----
# prepare game ids
id <- game_id_data$identifiers_2020

# prepare pbp data
pbp <- pbp_data$pbp2020 %>% 
    arrange(desc(spielzeit_sec )) %>% 
    arrange(game_nr)

# prepare roster data
roster <- roster_data$rosters_2020 %>% 
    type_convert() %>%
    drop_na(.,Club)
unique(roster$Club)
roster$Club[roster$Club == "SYNTAINICS MBC"] <- "Mitteldeutscher BC"
roster$Club[roster$Club == "BG GA#ttingen"] <- "BG Göttingen"
roster$Club[roster$Club == "Basketball LA#wen Braunschweig"] <- "Basketball Löwen Braunschweig"
unique(roster$Club)

id_merge <- bind_cols(id, unique(pbp$game_nr)) %>% 
    rename(nr = `...3`)

pbp <- merge(pbp,id_merge,
             by.x = "game_nr",
             by.y = "nr") %>% 
    dplyr::select(-game_nr)

# clean for games which have no files
pbp <- pbp %>%
    mutate_at("value", ~replace(., is.na(.), 0)) %>% 
    filter(value != "Error finding file")

#******************************************************************************#
# Save cleaned pbp files 2020:
saveRDS(object = pbp, file = paste0("Data/clean_pbp/pbp_2020",".Rds"))
#******************************************************************************#
#******************************************************************************#