# Clear Console
cat("\014")

# remove Environment
rm(list=ls())

setwd("~/Uni Tübingen/0. Masterarbeit/7. R/Masterarbeit")
source('~/Uni Tübingen/0. Masterarbeit/7. R/Masterarbeit/functions/download_and_prep_functions.R')

# install needed packages
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("nbastatR")
#install.packages(jsonlite)

# load needed packages
library(nbastatR)
library(tidyverse)
library(jsonlite)
library(glue)
library(plm)
library(rvest)
library(readr)

url <- 'https://www.easycredit-bbl.de/de/statistiken/teams/team-statistiken/'
page <- url %>%
  read_lines() %>%
  str_replace_all("<!--|-->", "") %>%
  str_trim() %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  str_c(collapse = "") %>%
  xml2::read_html()

tables <- page %>%
  html_nodes(xpath = "//*[contains(@class, 'dontsortfirstrow')]")

all_data <- seq_along(xml_tables)

table_id <- tables %>%
  html_attr("id")

table_name <-
  xml_tables %>%
  xml_nodes("caption") %>%
  html_text()
