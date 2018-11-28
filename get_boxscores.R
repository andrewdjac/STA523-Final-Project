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

get_game_stats <- function(season, date, year){
  page <- safely_read(season %>% str_replace("game", "boxscore")) %>% 
    .[[1]]
  df <- data_frame(
    year = year,
    date = date,
    player = page %>% 
      html_nodes("#gamepackage-boxscore-module a span") %>% 
      html_text() %>% 
      .[c(TRUE, FALSE)],
    points = page %>% 
      html_nodes(".dnp , td.pts") %>% 
      html_text() %>% 
      .[-(which(. == "") - 1)] %>% 
      .[which(. != "")],
    rebounds = page %>% 
      html_nodes(".dnp , td.reb") %>% 
      html_text() %>% 
      .[-(which(. == "") - 1)] %>% 
      .[which(. != "")],
    assists = page %>% 
      html_nodes(".dnp , td.ast") %>% 
      html_text() %>% 
      .[-(which(. == "") - 1)] %>% 
      .[which(. != "")],
    fouls = page %>% 
      html_nodes(".dnp , td.pf") %>% 
      html_text() %>% 
      .[-(which(. == "") - 1)] %>% 
      .[which(. != "")],
    blocks = page %>% 
      html_nodes(".dnp , td.blk") %>% 
      html_text() %>% 
      .[-(which(. == "") - 1)] %>% 
      .[which(. != "")],
    steals = page %>% 
      html_nodes(".dnp , td.stl") %>% 
      html_text() %>% 
      .[-(which(. == "") - 1)] %>% 
      .[which(. != "")]  
  ) %>% 
    filter(points != "Did not play") %>% 
    mutate(points = points %>% as.numeric(),
           rebounds = rebounds %>% as.numeric(),
           assists = assists %>% as.numeric(),
           fouls = fouls %>% as.numeric(),
           blocks = blocks %>% as.numeric(),
           steals = steals %>% as.numeric()
           )
  return(df)
}

get_season <- function(year){
  url <- paste0("http://www.espn.com/mens-college-basketball/team/schedule/_/id/150/season/",
               year)
  page <- read_html(url)
  dates <- page %>% 
    html_nodes(".ml4 a , .Table2__td:nth-child(1) span") %>% 
    html_text() %>% 
    .[which(. != "Date")] %>% 
    .[c(TRUE, FALSE)]
  links <- page %>% 
    html_nodes(".ml4 a") %>% 
    html_attr("href")
  df <- map_df(1:length(links), function(i)
    {get_game_stats(links[i], dates[i], as.character(year))})
  return(df)
}

years <- 2015:2018
boxscores <- map_df(years, function(x){get_season(x)})

write_rds(boxscores, path = "data//boxscores.rds")

