library(tidyverse)
library(rvest)

data_dir <- "data/"
dir.create(data_dir, showWarnings = FALSE)

safely_read <- safely(html_session, otherwise = NA, quiet = TRUE)

page <- safely_read("http://goduke.statsgeek.com/basketball-m/players/all.php")
player_names <- page[[1]] %>%
  html_nodes(".stattextline .stattextline") %>% 
  html_text() %>% 
  str_replace("[^A-Z]+", ". ")

write_rds(player_names, path = "data//player_names.rds")
