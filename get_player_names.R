library(tidyverse)
library(rvest)
library(dplyr)

data_dir <- "data/"

get_player_names = function(season){
  base_url = "http://www.goduke.com/SportSelect.dbml?DB_OEM_ID=4200&SPID=1845&SPSID=22727&KEY=&Q_SEASON="
  url = paste0(base_url, as.character(season))
  page = read_html(url)
  number = page %>% html_nodes(".number") %>% html_text()
  index = (length(number)-1)/2
  leng = (length(number)+1)/2
  number = number[1:index]
  if (season >=2009){
    name = page %>% html_nodes(".showPopup") %>% html_text()}
  if (season <2009){
    name = page %>% html_nodes(".name") %>% html_text() %>% .[2:leng]}
  position = page %>% html_nodes(".position") %>% html_text() %>% .[1:index]
  height = page %>% html_nodes(".height") %>% html_text() %>% .[1:index]
  weight = page %>% html_nodes(".weight") %>% html_text() %>% .[2:leng]
  year = page %>% html_nodes(".year") %>% html_text() %>% .[1:index]
  home = page %>% html_nodes(".hometown") %>% html_text() %>% .[1:index]
  seas = paste0(as.character(season-1),"-",as.character(season))
  
  info = data.frame(
    Season = seas,
    Number = number,
    Name = name,
    Position = position,
    Height = height,
    Weight = weight,
    Year = year,
    Hometown = home
  )
  
  info = info %>% mutate(
    Number = Number %>% as.character(),
    Position = str_remove_all(Position, "Position:\\s|\\t|\\n"),
    Height = str_remove_all(Height, "\\n|\\t|\\d\\d\\d\\slbs"),
    Weight = str_remove_all(Weight, "\\t|\\n"),
    Year = str_remove_all(Year, "\\t|\\n|Year:\\s") %>%
      plyr::mapvalues(from=c("Fr.", "So.", "Jr.", "Sr.","RFr.","RSo.","RJr.","RSr."), 
                      to=c("Freshman", "Sophomore", "Junior", "Senior", 
                           "Redshirt Freshman","Redshirt Sophomore", "Redshirt Junior", "Redshirt Senior"),
                      warn_missing = FALSE),
    Hometown = str_remove_all(Hometown, "\\t|\\n|Hometown:\\s"))
  
  info$Name = str_remove_all(info$Name, "\\t|\\n") %>% substr(.,nchar(.)/2+1,nchar(.))
  info$Number=str_remove_all(info$Number, "\\#") %>% as.numeric()
  return (info)
}


temp = get_player_names(2018)

dir.create("data/player_names/", showWarnings = FALSE, recursive = TRUE)
seasons = c(1989:2018)

player_names = data.frame()
for (season in seasons){
  df = get_player_names(season)
  player_names = rbind(player_names, df)
}

write_rds(player_names, path = "data//player_names.rds")

