library(tidyverse)


idal_isy <- 
  read.csv(
    "processed/Jun-Nov_IDAL_isy_data.csv"
  )

idal_hmis<-
  read.csv(
    "processed/Jun-Nov_PHCs_IDAL_hmis_data.csv"
  )

sentinel_list <- 
  read.csv(
    "processed/sentinel.csv"
  )

idal_wd_course_isy <-
  idal_isy %>% 
    inner_join(
      .,
      sentinel_list,
      by = "code"
    ) %>%
    mutate(
      course = round(
        qt / rounded_course,
        digits = 2
      )
    ) %>% 
    select(
      unite_dest, month, is_it_more_than_5,
      admin_r, aware, atc_3, atc_4, code, course 
    ) %>% 
    pivot_longer(
      cols = c(
        admin_r, aware, atc_3, atc_4, code
      ),
      names_to = "RK",
      values_to = "LV"
    ) %>% 
    group_by(
      unite_dest, month, 
      is_it_more_than_5, LV
    ) %>% 
    summarise(
      value = sum(course),
      .groups = "drop"
    ) %>% 
    pivot_wider(
      names_from = LV,
      values_from = value,
      values_fill = 0
    ) 
    
