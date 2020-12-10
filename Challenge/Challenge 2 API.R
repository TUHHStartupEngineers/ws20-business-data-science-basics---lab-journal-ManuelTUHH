
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(httr)

Read_API <-function(nr){
  
ram_api <- GET(glue("https://rickandmortyapi.com/api/character/?page={nr}"))

ram_loop <- ram_api   %>%
  .$content  %>% 
  rawToChar()  %>% 
  fromJSON() %>%
  purrr::pluck("results") %>% as.tibble
}

ram_character <- map(c(1:33),Read_API) %>% 
    bind_rows

ram_origin <- bind_cols(ram_character$origin) %>% select(name) %>% rename(origin = name)

ram_location <- bind_cols(ram_character$location) %>% select (name) %>% rename(current_location = name)
    
ram_character <- ram_character %>%
  select(name, status, species, gender) %>% 
  add_column(ram_origin, ram_location) %>%
  arrange(species, status, Origin, current_location)


