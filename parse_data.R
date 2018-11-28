library(tidyverse)

games <- readRDS("data//games.rds")
boxscores <- readRDS("data//boxscores.rds")
player_names <- readRDS("data//player_names.rds")

players_data <- merge(games, boxscores, by = c("year", "date")) %>% 
  filter(player %in% player_names)

write_rds(players_data, path = "players_data.rds")
