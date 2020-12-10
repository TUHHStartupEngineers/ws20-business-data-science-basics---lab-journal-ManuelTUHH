

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
  type = col_integer(),
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


########### Set up uspc Table

uspc_col_types <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_character()
)

uspc_tbl <- vroom(
  file       = "00_data/04_patent_data/uspc.tsv", 
  delim      = "\t", 
  col_types  = uspc_col_types,
  na         = c("", "NA", "NULL")
)

class(uspc_tbl)

setDT(uspc_tbl)

uspc_tbl <- uspc_tbl[sequence == 0]


patent_dominance_tbl <- merge(x = patent_assignee_tbl, y = assignee_tbl,
                              by.x = "assignee_id",
                              by.y = "id",
                              all = FALSE)

comb_tbl_1 <- select(patent_dominance_tbl[
  type > 1 & type < 4 & !is.na(organization)], organization, patent_id)

comb_tbl_2 <- merge(x = comb_tbl_1, y = uspc_tbl,
                  by = "patent_id",
                  all = FALSE)

comb_tbl_2 <- select(comb_tbl_2, organization, patent_id, mainclass_id)

top_ten_tbl <- select(patent_dominance_tbl, organization, type, patent_id)[
  type > 1 & type < 4 & !is.na(organization), .N, organization]

top_ten_tbl <- slice(top_ten_tbl[order(-N)], c(1:10))

patent_innovation_tbl <- merge(x = comb_tbl_2, y = top_ten_tbl,
                               by = "organization",
                               all = FALSE)
sorted_patent_innovation <- patent_innovation_tbl[, .N, mainclass_id][order(-N)]

