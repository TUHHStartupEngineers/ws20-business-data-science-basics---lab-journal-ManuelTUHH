

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

tic()
setDT(assignee_tbl)
toc()

# merge into 1 table
patent_dominance_tbl <- merge(x = patent_assignee_tbl, y = assignee_tbl,
                          by.x = "assignee_id",
                          by.y = "id",
                          all.x = TRUE,
                          all.y = FALSE)

patent_dominance_tbl <- select(patent_dominance_tbl, organization, type, patent_id, assignee_id)[
                               type == 2 & !is.na(organization)]
                                 

sorted_patent_dominance <- arrange(patent_dominance_tbl[, .N, by = organization],  desc(N))

######################## Challenge 3.2

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

patent_DT <- setDT(patent_tbl)


recent_patent_activity_DT <- rename(
                              select(
                                separate(
                                  select(patent_DT, id, date),
                                     col  = date,
                                     into = c("year", "month", "day"),
                                     sep  = "-", remove = FALSE),
                                  id, year),
                                patent_id = id)[year == 2019]


combined_data_patent_activity <- merge(x = recent_patent_activity_DT, y = patent_dominance_wrngl, 
                         by    = "patent_id", 
                         all.x = TRUE, 
                         all.y = FALSE)

patent_activity <- arrange(
                        combined_data_patent_activity[
                                    !is.na(year) & !is.na(organization), 
                                    .N, 
                                    by = organization],
                        desc(N))


#### Challenge 3.3

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

uspc
