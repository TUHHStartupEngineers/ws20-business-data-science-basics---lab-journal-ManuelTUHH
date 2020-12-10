

library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(maps)
library(viridis)

######### Challenge 4.1

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

country <- c("Germany", "United_Kingdom", "France", "Spain", "United_States_of_America")

country_tbl <- data.frame(country)

rm(country)

covid_data <- merge(x = country_tbl, y = covid_data_tbl,
                        by.x = "country",
                        by.y = "countriesAndTerritories",
                        all = FALSE)  %>%  
  
  mutate(dateRep = as.Date(dateRep, "%d/%m/%Y")) %>%
  
  rename(date = dateRep) %>%
  
  select(country, date, cases) %>% 
  
  arrange(date, country) %>%
  
  group_by(country) %>%
  
  mutate(cumulative_cases = cumsum(cases)) 


label_data <- head(tail(covid_data, 9), 5)


covid_data %>% ggplot(aes(x = date, y = cumulative_cases)) +
  
  geom_line(size = 0.8, aes(group = country, color = country)) +
  
  theme_minimal(base_size = 15) +
  
  labs(title = "COVID 19 Cases", x = "Month (2020)", y = "Cases in Mio") + 
    
  scale_y_continuous(labels = scales::label_number(scale = 1e-6),
                     breaks = seq(0, 20e+6, 25e+5)) +
  
  scale_x_date(labels = scales::label_date(format = "%B"),
               breaks = scales::date_breaks("1 month")) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom",
        legend.background = element_rect(fill = "grey", linetype = "solid", color = "grey")) +
  
  geom_text_repel(data =  label_data,
                aes(label = scales::number(prefix = glue("{country}: "), cumulative_cases)),
                hjust = 1.3,
                color = "black",
                fontface = "bold",
                vjust = 1,
                size = 4.5,
                ylim = c(1e+6, 13e+6),
                xlim = as.Date(c("2020-09-01", "2020-10-17"))) 


 
  geom_label(data = USA_label, 
             aes(x = date, 
                 y = cumulative_cases, 
                 label = scales::number(cumulative_cases)),
             hjust = 1.3,
             fill  = "white",
             color = "black",
             fontface = "bold",
             vjust = 1,
             size = 4.5)
  

  
  ######### Challenge 4.2
  
  
  world <- map_data("world")
  
  covid_data_names <- covid_data_tbl %>%    
    
    mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
    
    mutate(countriesAndTerritories = case_when(
      countriesAndTerritories == "United Kingdom" ~ "UK",
      countriesAndTerritories == "United States of America" ~ "USA",
      countriesAndTerritories == "Czechia" ~ "Czech Republic",
      TRUE ~ countriesAndTerritories))
  
  
mortality_data <- covid_data_names %>%  

    mutate(date = as.Date(dateRep, "%d/%m/%Y")) %>%
        
    select(date, countriesAndTerritories, deaths, popData2019) %>%
    
    rename("region" = "countriesAndTerritories") %>%
    
    filter(!is.na("popData2019") & date < "2020-11-03" & !deaths < 1) %>%
    
    group_by(region, popData2019) %>%
    
    summarize(deaths_per_pop = sum(deaths)) %>%
  
    mutate(deaths_per_pop = round(100 * (deaths_per_pop/popData2019), 3)) %>%
  
    select(-popData2019)


world_mortality_data <- merge(x = world, y = mortality_data,
                              by.x = "region",
                              by.y = "region",
                              all.x = T) 

ggplot() +
  
  geom_map(data = world_mortality_data,
           map = world,
           mapping = aes(x = long, y = lat, map_id = region, fill = deaths_per_pop)
           ) +
  
  geom_polygon(color = "white") +
  
  labs(title = "Mortality Rate",
       subtitle = "COVID 19",
       caption = format(Sys.time(), "%d.%m.%Y"),
       x = "",
       y = "") +
  
  theme_minimal() +
  
  theme(legend.position = "bottom", axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  
  scale_x_continuous(expand=c(0,0)) +
  
  scale_y_continuous(expand=c(0,0)) +
  
  scale_fill_viridis(
    option = "inferno", 
    direction = -1,
    name = "Mortality Rate in %",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(200, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 0.5))
  

  
  
  
  
  
  
