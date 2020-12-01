

# Tidyverse
library(tidyverse)
library(vroom)

# Data Table
library(data.table)

# Counter
library(tictoc)

####### set up Patent Assignee Table

patent_assignee_col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "00_data/04_patent_data/patent_assignee.tsv", 
  delim      = "\t",
  col_types = patent_assignee_col_types,
  na         = c("", "NA", "NULL")
)

class(patent_assignee_tbl)

setDT(patent_assignee_tbl)

######## Set up Assignee Table

assignee_col_types <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "00_data/04_patent_data/assignee.tsv", 
  delim      = "\t",
  col_types = assignee_col_types,
  na         = c("", "NA", "NULL")
)

class(assignee_tbl)

setDT(assignee_tbl)


########### Set up patent Table

patent_col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

patent_tbl <- vroom(
  file       = "00_data/04_patent_data/patent.tsv", 
  delim      = "\t", 
  col_types  = patent_col_types,
  na         = c("", "NA", "NULL")
)

class(patent_tbl)

setDT(patent_tbl)


###### merge patent_assignee with assignee

patent_dominance_tbl <- merge(x = patent_assignee_tbl, y = assignee_tbl,
                              by.x = "assignee_id",
                              by.y = "id",
                              all.x = TRUE,
                              all.y = FALSE)

patent_dominance_tbl <- select(patent_dominance_tbl, organization, type, patent_id, assignee_id)[
  type == 2 & !is.na(organization)]


##### wrangle patent_tbl

patent_tbl <- select(separate(patent_tbl,
                       col = date,
                       into = c("year", "month", "day"),
                       sep = "-", remove = FALSE),
                     id, year)

patent_tbl <- patent_tbl[year == 2019]

##### nerge patent_dominance with patents

patent_activity_tbl <- merge(x = patent_dominance_tbl, y = patent_tbl,
                         by.x = "patent_id",
                         by.y = "id",
                         all.x = TRUE,
                         all.y = FALSE)

sorted_patent_activity <- patent_activity_tbl[!is.na(year), .N, organization][order(-N)]