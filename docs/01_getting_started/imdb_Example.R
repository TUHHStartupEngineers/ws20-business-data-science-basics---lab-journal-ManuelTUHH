
library(RSQLite)
library(glue)
library(httr)
# use that URL to scrape the S&P 500 table using rvest
library(rvest)
library(tibble)


url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"

html <- url %>% 
  read_html()

rank <- html %>%
  html_nodes(".titleColumn") %>%
  html_text() %>%
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  as.numeric()

title <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_text()

year <- html %>%
  html_nodes(".titleColumn .secondaryInfo") %>%
  html_text() %>%
  stringr::str_extract("[:digit:]+")

people <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_attr("title")

rating <- html %>%
  html_nodes(".ratingColumn.imdbRating > strong") %>%
  html_text()

num_ratings <- html %>%
  html_nodes(".ratingColumn.imdbRating > strong") %>%
  html_attr("title") %>%
  stringr::str_extract("(?<=based on ).*(?= user rating)") %>%
  stringr::str_replace_all(pattern = ",", replacement = "") %>%
  as.numeric
  
imdb_tbl <- tibble(rank, title, year, people, rating, num_ratings)
  
  
  
  
  
  
  
  