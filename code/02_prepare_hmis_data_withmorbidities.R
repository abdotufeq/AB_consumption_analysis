library(tidyverse)
library(lubridate)
library(readxl)
library(glue)
library(here)


# list to harmonize facilities name between HMIS and ISYSTOCK
unit_conversion_morbidity <-
  c(
    "Idal Mobile Clinic 1-Idlib" = "MSF Mobile Team",
    "Idal Mobile Clinic 2- WAC" = "WAC Mobile Team",
    "Idal Mobile Clinic 3" = "KAFER NASEH MOBILE CLINIC",
    "Idal Mobile Clinic 4" = "KAFER BONI MOBILE CLINIC",
    "KAFR BONY PHC" = "Kafar Bonny PHC",
    "KAFR NASEH PHC" = "KAFER NASEH PHC",
    "Mashhad Ruhin PHC" = "Mashhed Ruhin PHC",
    "TERMANIN PHC" = "Termanin PHC",
    "AFRIN - AFRIN PHCC" = "AFRIN PHC",
    "AFRIN - SHARAM MC" = "SHARRAN MC",
    "AFRIN - SHARAN PHCC" = "SHARRAN PHC",
    "Azzaz -  Mobile Clinic 1" = "AZAZ MC",
    "AFRIN - BULBUL BemonC" = "BULBUL PHC",
    "Azaz - Maree  Cemonc" = "Marea PHC",
    "AL BAB - Qabazin BemonC" = "QABBASIN PHC",
    "Al Kindy Maternity Hospital" = "Al Kindy Mat Hospital",
    "Daret Ezza PHC" = "Daret Ezzeh PHC",
    "AFRIN - JANDARIS Maternity and Paediatric hospital" = "JANDARIS MATERNITY HOSPITAL",
    "AFRIN - SHEIKH AL HADID PHCC" = "Shiekh Al Hadid PHC"
  )

# choose HIMS data file
hmis_xls_file <-
  if (interactive() &&
      .Platform$OS.type == "windows")
    choose.files() else
      file.choose()

# extract project name from the file path
project_hmis <-
  if (interactive() &&
      .Platform$OS.type == "windows")
    str_replace(hmis_xls_file, ".*\\\\", "") %>%
  str_replace("\\.xls", "") else
    str_replace(hmis_xls_file, ".*\\/", "") %>%
  str_replace("\\.xls", "")

# import HMIS data
hmis_data_mor <-
  # read HMIS xls file
  read_excel(
    hmis_xls_file,
    col_types = c("text", "text",
                  "numeric", "numeric", "numeric", "numeric", 
                  "numeric", "numeric", "numeric", "numeric",
                  "numeric", "numeric", "numeric", "numeric", 
                  "numeric", "numeric", "numeric", "numeric", 
                  "numeric", "numeric", "numeric", "numeric",
                  "numeric", "numeric", "numeric", "numeric",
                  "numeric", "numeric")
  ) %>%
  # rename columns without names
  rename(
    unit = `organisationunitname`,
    morbidity = `dataname`
  ) %>%
  # tidy HMIS wide data to long data
  pivot_longer(
    cols = -c(unit, morbidity),
    names_to = "month_cat",
    values_to = "nr_morbidities",
    values_drop_na = TRUE
  ) %>%
  # separate the age group from the month & year
  separate(
    month_cat,
    c("category", "limit", "y", "month", "year"),
    sep = " "
  ) %>%
  mutate(
    # convert HMIS facilities names to ISYSTOCK ones
    unit = str_replace_all(unit, unit_conversion_morbidity),
    # convert consultation groups into factor variable
    morbidity = as_factor(morbidity),
    # convert months names to number
    month = match(month, month.name),
    # convert age group into logical variable
    is_it_more_than_5 = case_when(
      category == ">=" ~ "TRUE",
      category == "<" ~ "FALSE",
      TRUE ~ "CHECK"
    ),
    is_it_more_than_5 = as.logical(is_it_more_than_5)
  ) %>%
  select(-category, -limit, -y)  %>%
  # pivot the data so HF, month and age category become row-wise
  pivot_wider(
    names_from = morbidity,
    values_from = nr_morbidities,
    values_fill = 0
  ) %>%
  rename(
    total_consultaion = `Outpatients total consultations (aggregated data only) - All`
  ) %>%
  # group then sum number of consultation to become
  # unique per for unit, month and age
  group_by(
    unit, year, month, is_it_more_than_5
  ) %>%
  mutate(
    total_consultation_morbidity = sum(
      `Lower Respiratory Tract Infection`,
      `Other infectious and parasitic diseases`,
      `Upper Respiratory Tract Infection`,
      `Urinary tract infection`
    )
  ) %>% ungroup()

# write the processed HMIS data to csv file in "processed" folder
write.csv(
  hmis_data_mor,
  glue(here("processed/{project_hmis}_hmis_data_withmorbities.csv")),
  row.names = FALSE
)

