library(tidyverse)
library(here)

# read processed Isystock data
isy <-
  read.csv(
    here("processed/Jan-Dec_Mission_isy_data.csv")
  )
# read processed HMIS data
hmis <-
  read.csv(
    here("processed/Jun-Dec__PHCs__IDAL-NAP_hmis_data.csv")
  )
# read processed sentinel list data
sentinel_list <-
  read.csv(
    here("processed/sentinel.csv")
  )

# keep the items we need only
sy_course_isy <-
  isy %>%
    # merge Isystock data with sentinel list by code
    inner_join(
      .,
      sentinel_list,
      by = "code"
    ) %>%
    # calculate number of course of medication
    mutate(
      course = round(
        qt / rounded_course,
        digits = 2
      )
    ) %>%
    # keep the columns we need in upcoming steps
    select(
      unite_dest, month, is_it_more_than_5,
      admin_r, aware, atc_3, atc_4, code, course
    ) %>%
    # changing types of variables
    mutate(
      unite_dest = as.factor(unite_dest),
      admin_r = as.factor(admin_r),
      aware = as.factor(aware),
      atc_3 = as.factor(atc_3)
    )  %>%
    # calculate total number of courses per each rank per facility per month
    pivot_longer(
      cols = c(
        admin_r, aware, atc_3, atc_4, code
      ),
      names_to = "rank_name",
      values_to = "rank"
    ) %>%
    group_by(
      unite_dest, month,
      is_it_more_than_5, rank
    ) %>%
    summarise(
      value = sum(course),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = rank,
      values_from = value,
      values_fill = 0
    )

