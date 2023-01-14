library(tidyverse)
library(lubridate)
library(readxl)
library(glue)


# list to harmonize facilities name between HMIS and ISYSTOCK
unit_conversion <-
c(
  "Idal Mobile Clinic 1" = "MSF Mobile Team",
  "Idal Mobile Clinic 2" = "WAC Mobile Team",
  "Idal Mobile Clinic 3" = "KAFER NASEH MOBILE CLINIC",
  "Idal Mobile Clinic 4" = "KAFER BONI MOBILE CLINIC",
  "KAFR BONY PHC" = "Kafar Bonny PHC",
  "KAFR NASEH PHC" = "KAFER NASEH PHC",
  "Mashhad Ruhin PHC" = "Mashhed Ruhin PHC",
  "TERMANIN PHC" = "Termanin PHC"
  )

# choose HIMS data file
hmis_xls_file <-
  if (interactive() &&
     .Platform$OS.type == "windows")
     choose.files() else
      file.choose()

# extract project name from the file path
project_hmis <-
  str_replace(hmis_xls_file, ".*/(w*)", "") %>%
  str_replace("\\.xls", "")

# import HMIS data
hmis_data <-
  # read HMIS xls file
  read_excel(
    hmis_xls_file
    ) %>%
  # rename columns without names
  rename(
    unit = `organisationunitname...1`,
    consultation = `organisationunitname...2`
    ) %>%
  # tidy HMIS wide data to long data
  pivot_longer(
    cols = -c(unit, consultation),
    names_to = "month_cat",
    values_to = "nr_consultation",
    values_drop_na = TRUE
    ) %>%
  # separate the age group from the month
  separate(
    month_cat,
    c("month", "category"),
    sep = " 2022 "
    ) %>%
  mutate(
    # convert HMIS facilities names to ISYSTOCK ones
    unit = str_replace_all(unit, unit_conversion),
    # convert consultation groups into factor variable
    consultation = as_factor(consultation),
    # convert months names to number
    month = match(month, month.name),
    # convert age group into logical variable
    is_it_more_than_5 = case_when(
      category == ">= 5 y" ~ "TRUE",
      category == "< 5 y" ~ "FALSE",
      TRUE ~ "CHECK"
      ),
    is_it_more_than_5 = as.logical(is_it_more_than_5)
    ) %>%
  select(-category)  %>%
  # pivot the data so HF, month and age category become rowwise
  pivot_wider(
    names_from = consultation,
    values_from = nr_consultation,
    values_fill = 0
  ) %>%
  # group then sum number of consultation to become
  # unique per for unit, month and age
  group_by(
    unit, month, is_it_more_than_5
  ) %>%
  mutate(
    total_consultation = sum(
      `Antenatal Care`,
      `External Consultations`,
      `Postnatal Care`,
      `Emergency Room`
    )
  ) %>%
  ungroup()

# write the processed HMIS data to csv file in "processed" folder
write.csv(
  hmis_data,
  glue("processed/{project_hmis}_hmis_data.csv"),
  row.names = FALSE
  )
