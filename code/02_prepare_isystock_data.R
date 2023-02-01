library(tidyverse)
library(readr)
library(readxl)
library(janitor)
library(lubridate)
library(glue)
library(here)

# choose ISYSTOCK data file
isystock_xlsx_file <-
  if (interactive() &&
     .Platform$OS.type == "windows")
     choose.files() else
      file.choose()

# import Isystock data
isy_data <- 
  # read Isystock xlsx file
  read_excel(
    isystock_xlsx_file
    ) %>%
  clean_names() %>%
  # choose regular donation only
  filter(
    flux == "OUTMSF"
    ) %>% 
  # extract the month from date column 
  mutate(
    month = month(date)
    ) %>% 
  # keep only information we need
  select(
    code, month, 
    unite_dest, qt
    ) %>% 
  # sum all quantities per item per month per facility
  group_by(
    code, month, unite_dest
    ) %>% 
  summarise(
    qt = sum(qt),
    .groups = "drop"
    ) 
# extract project name from the file path
project_isy <-
  str_replace(isystock_xlsx_file, ".*\\\\", "") %>% 
  str_replace("\\.xlsx", "")

# write the processed Isystock data to csv file in "processed" folder
write.csv(
  isy_data,
  glue(here("processed/{project_isy}_isy_data.csv")),
  row.names = FALSE
  )
