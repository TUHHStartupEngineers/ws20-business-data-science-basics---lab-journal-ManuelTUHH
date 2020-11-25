# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)

# 2.0 Importing Files ----
bikes_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/bikes.xlsx")
bikeshops_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
orderlines_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")

# 3.0 Examining Data ----


# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# Wrangling Data

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%

    # 5.1 Separate category name
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  
  # 5.2 Add the total price (price * quantity) 
  mutate(total.price = price * quantity) %>%
  
  # 5.3 Reorganize
  select(-...1, -gender) %>%
  select(-ends_with(".id")) %>%
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))


####### Challenge

##### Sales by State

## Manipulate

sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>%
  
  separate(col = location,
           into = c('city', 'state'),
           sep = ', ',
           convert = T) %>%
  
  select(state, total_price) %>%
  
  group_by(state)  %>%
  
  summarize(sales = sum(total_price)) %>%
  
  arrange(state) %>%
  
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

## Visualize

sales_by_state_tbl %>%
  
  arrange(sales) %>%
  
  mutate(state = factor(state, levels = state)) %>%
  
  ggplot(aes(x = state, y = sales)) +

  geom_col(fill = "#008E19") +

  theme_minimal(base_size = 16) +

  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  geom_label(aes(label = sales_text)) +

  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by state",
    x = "State",
    y = "Revenue"
  )

AspectRatio <- 16/9

s <- 8

ggsave("Sales_by_State.png",
       path = "journal_files/figure-html", 
       height = s, width = s * AspectRatio)


#### Sales by State and Year

## Manipulate

sales_by_state_and_year_tbl <- bike_orderlines_wrangled_tbl %>%
  
  separate(col = location,
           into = c('city', 'state'),
           sep = ', ',
           convert = T) %>%
  
  mutate(year = year(order_date)) %>%
  
  select(state, year, total_price) %>%
  
  group_by(state, year)  %>%

  summarize(sales = sum(total_price)) %>%
  
  arrange(state, year) %>%
  
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

## Visualize

for (jj in c(1:nrow(sales_by_state_tbl))) {

  assign(paste("sales_in_", sales_by_state_tbl[jj,1], sep=''), sales_by_state_and_year_tbl[sales_by_state_and_year_tbl$state==sales_by_state_tbl[[jj,1]],])
  
  eval(as.name(paste("sales_in_", sales_by_state_tbl[jj,1], sep=''))) %>%

    ggplot(aes(x = year, y = sales)) +

    geom_col(fill = "#008E19") +

    theme_minimal(base_size = 16) +

    geom_label(aes(label = sales_text)) +
    
    geom_smooth(method = "lm", se = FALSE) +

    scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                        decimal.mark = ",",
                                                        prefix = "",
                                                        suffix = " €")) +
    labs(
     title = paste("Revenue by year in ", sales_by_state_tbl[jj,1], sep=""),
      x = "Year",
      y = "Revenue"
   )
  
  AspectRatio <- 1
  
  s <- 8
  
  ggsave(paste("sales_in_", sales_by_state_tbl[jj,1], ".png", sep = ""),
         path = "journal_files/figure-html", 
         height = s, width = s * AspectRatio)
}

# 7.0 Writing Files ----

# 7.1 Excel ----
#install.packages("writexl")
#library("writexl")
#bike_orderlines_wrangled_tbl %>%
#  write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----
#bike_orderlines_wrangled_tbl %>% 
#  write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----
#bike_orderlines_wrangled_tbl %>% 
#  write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")