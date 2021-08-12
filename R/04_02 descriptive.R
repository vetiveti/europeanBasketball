# Clear Console
cat("\014")

# remove Environment
rm(list=ls())


library(tidyverse)

#For nicer fonts 
library(extrafont)

#load data
ranking <- readRDS("Data/estimates/ranking.Rds")

theme_owen <- function () { 
    theme_minimal(base_size=12, base_family="Consolas") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
        )
}
#******************************************************************************#
# MVP graph:----

#load data
ranking <- readRDS("Data/estimates/ranking.Rds")

df <- ranking %>% 
    filter(year == 2020) %>%
    dplyr::select(player,VORP_rank_y, ws_rank_y,wp_A_rank_y, wp_B_rank_y,nba_eff_y, bbl_eff_y)

df_a <- df %>%
    pivot_longer(!player, names_to = "Method", values_to = "Value")

df_a %>% 
    slice(1:60) %>%  
    arrange(Method, Value) %>%
    mutate(Player = factor(player, unique(player))) %>% 
    ggplot(aes(x = Value, y = player)) + 
    geom_jitter(height = 0, width = 0, shape = 21, color = 'black', fill = '#E64B35FF', size = 4, aes(alpha = Method)) + 
    theme_owen() + 
    labs(title = "MVP Rankings by different methods combined*", 
         subtitle = paste0("As of ", format(Sys.Date(), format="%B %d, %Y")), 
         caption = "*VORP,Win Share, Wins Produced, NBA efficiency, BBL efficiency",
         x = "", 
         y = "", 
         fill = "") +
    scale_x_continuous(breaks = seq(0, 100, 10), labels = paste0("#", seq(0, 100, 10))) +
    theme(legend.position = 'top',
          plot.title.position = 'plot',
          plot.title = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 9, face = 'italic'),
          plot.caption = element_text(size = 6, vjust = 3, face = 'italic'), 
          axis.text.x = element_text(size = 7), 
          axis.text.y = element_text(margin = margin(r = -5)), 
          plot.margin = margin(5.5, 5.5, 10, 5.5, "pt")) +
    scale_alpha_manual(values = c(1, rep(.1, 9))) + 
    guides(alpha = FALSE) 

#******************************************************************************#
# MVP graph only VORP, WP, WS:----
#load data
ranking <- readRDS("Data/estimates/ranking.Rds")

df <- ranking %>% 
    filter(year == 2020) %>%
    dplyr::select(player,VORP_rank_y, ws_rank_y,wp_A_rank_y)

df_a <- df %>%
    pivot_longer(!player, names_to = "Method", values_to = "Value")

df_a %>% 
    slice(1:30) %>%  
    arrange(Method, Value) %>%
    mutate(Player = factor(player, unique(player))) %>% 
    ggplot(aes(x = Value, y = player)) + 
    geom_jitter(height = 0, width = 0, shape = 21, color = 'black', fill = '#E64B35FF', size = 5, aes(alpha = Method)) + 
    theme_owen() + 
    labs(title = "BBL MVP Rankings 2020-2021 by VORP, WP, WS combined", 
         subtitle = paste0("As of ", format(Sys.Date(), format="%B %d, %Y")," by Lukas Cramer"), 
         x = "", 
         y = "", 
         fill = "") +
    scale_x_continuous(breaks = seq(0, 100, 10), labels = paste0("#", seq(0, 100, 10))) +
    theme(legend.position = 'top',
          plot.title.position = 'plot',
          plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(size = 12, face = 'italic'),
          plot.caption = element_text(size = 8, vjust = 3, face = 'italic'), 
          axis.text.x = element_text(size = 7), 
          axis.text.y = element_text(margin = margin(r = -5)), 
          plot.margin = margin(5.5, 5.5, 10, 5.5, "pt")) +
    scale_alpha_manual(values = c(1, rep(.1, 9))) + 
    guides(alpha = FALSE) 

ggsave("export/MVP_2020.png", w = 10, h = 10, dpi = 300, type = 'cairo')

#******************************************************************************#
# MVP stats VORP, WP, WS:----
#For making tables 
require(gt)

#load data
ranking <- readRDS("Data/estimates/ranking.Rds")

df <- ranking %>% 
    filter(year == 2020) %>%
    dplyr::select(player,team,min_p,VORP_rank_y, ws_rank_y,wp_A_rank_y)

# merge with player image url
player_info2020 <- readRDS("Data/player_info/player_info2020.Rds")
df_1_1 <- merge(df,b, by = "player")
df_1_1$team <- trimws(df_1_1$team)

# merge with team image url
source('functions/BBL_functions.R')
team_pngs <- image_team(2020)

df_1_2 <- merge(df_1_1,team_pngs, by ="team", all = TRUE)

# create graph
df_1 <- df_1_2 %>% 
    rename(min = min_p,
           VORP = VORP_rank_y,
           Win_Shares = ws_rank_y,
           Wins_Produced = wp_A_rank_y,
           img = img.url) %>% 
    mutate(avg_rank = (VORP + Win_Shares + Wins_Produced)/ 3) %>%
    mutate(avg_rank = round(avg_rank,digits = 2)) %>% 
    arrange(avg_rank) %>% 
    filter(., avg_rank <= 25) %>% 
    relocate(img, .before = player) %>% 
    relocate(img_team, .after = player) %>% 
    relocate(team, .after = img_team) %>% 
    gt() %>%
    tab_header(
        title = md("**MVP Award Tracker by advanced stats**"),
        subtitle = paste0("As of ", format(Sys.Date(), format="%B %d, %Y")," by Lukas Cramer") 
    )  %>%
    cols_align(
        align = "left",
        columns = 1:4
    )  %>%
    cols_align(
        align = "center",
        columns = 5:9
    )  %>%
    cols_label(img = md(""),
               player = md("PLAYER"), 
               img_team = md(""),
               team = ("TEAM"), 
               min = md("MINUTES"), 
               VORP = md("VORP"), 
               Win_Shares = md("WIN SHARES"), 
               Wins_Produced = md("WINS PRODUCED"), 
               avg_rank = md("AVG. RANK")) %>% 
    text_transform(
        locations = cells_body(vars(img,img_team)),
        fn = function(x) {
            web_image(url = x) 
        }
    ) %>% 
    opt_row_striping() %>% 
    tab_options(
        table.background.color = "floralwhite",
        column_labels.font.size = 12,
        column_labels.font.weight = 'bold',
        row_group.font.weight = 'bold',
        row_group.background.color = "#E5E1D8",
        table.font.size = 10,
        heading.title.font.size = 20,
        heading.subtitle.font.size = 12.5,
        table.font.names = "Consolas", 
        data_row.padding = px(2)
    ) %>% 
    tab_source_note(
        source_note = "credit to: Owen Phillips, easyCredit BBL")

gtsave(data = df_1, "export/MVP_2020_table.png") # can be a latex table!!!
