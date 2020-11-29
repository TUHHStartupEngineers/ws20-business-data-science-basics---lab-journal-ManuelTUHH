

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(httr)

url <- "https://www.rosebikes.com/bikes"

html <- read_html(url)

bike_tbl <- html %>% 
          html_nodes(".catalog-navigation__link") %>% 
          html_attrs() %>% 
          bind_rows()  %>%
          select(title, href) %>%
  rename(family_url = href, bike_family = title) %>%
  mutate(family_url = glue("https://www.rosebikes.com{family_url}")) %>%
  filter(bike_family != "Sale")

read_categories <- function(nr){
  
  bike_category_url <- bike_tbl$family_url[nr]
  
  bike_category_html <- read_html(bike_category_url)
  
  bike_category_names_tbl <- bike_category_html %>%
    html_nodes(".catalog-category-bikes__title-text") %>%
    html_text() %>%
    stringr::str_extract("(?<=\\n).*(?=\\n)") %>%
    as_tibble 
  
  bike_category_price_tbl <- bike_category_html %>%
    html_nodes(".catalog-category-bikes__price-title") %>%
    html_text() %>%
    stringr::str_extract("(?<=from ).*(?=.00)") %>%
    stringr::str_replace(pattern = ",", replacement = "") %>%
    as_tibble 
  
  pricelist_map <- bind_cols(bike_tbl$bike_family[nr], bike_category_names_tbl, bike_category_price_tbl)
}

pricelist_tbl <- map(c(1:9),read_categories) %>%
  bind_rows() %>% rename(Family = ...1, Category = value...2, Price = value...3) 

pricelist_tbl[is.na(pricelist_tbl)] <- "Coming Soon"



