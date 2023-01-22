library(tidyverse)
library(janitor)
library(readxl)
library(here)


# choose the sentinel file from STD folder
sentinel_file <-
  if(interactive() &&
     .Platform$OS.type == "windows")
    choose.files() else 
      file.choose()
# import the sentinel data
PHC_sentinel_list <- 
  # read the sentinel xlsx file (SPECIFY SHEET NAME)
  read_excel(
    sentinel_file,
    sheet = "PHC"
  ) %>% 
  clean_names() %>% 
  # convert the WHO categories into factors variables
  mutate(
    atc_3 = as_factor(atc_3),
    atc_4 = as_factor(atc_4),
    aware = as_factor(a_wa_re)
  ) %>% 
  # select only needed columns 
  select(
    code, article, dose_per_unit, ddd, u,
    admin_r, atc_3, atc_4, rounded_course,
    aware, is_it_more_than_5
  ) 

write.csv(
  PHC_sentinel_list, 
  here("processed/sentinel.csv"),
  row.names = FALSE
  )
