library(tidyverse)
library(here)

# read processed Isystock data
isy <-
  read.csv(
    here("processed/2022-Jun-2023-Jun_Mission_isy_data.csv")
  )
# read processed Isystock data shifted 1 month
isy_m <-
  read.csv(
    here("processed/2022-Jun-2023-Jun_Mission_isy_data_shifted1month.csv")
  )
# read processed Isystock data shifted 15 days
isy_d <-
  read.csv(
    here("processed/2022-Jun-2023-Jun_Mission_isy_data_shifted15days.csv")
  )
# read processed HMIS data
hmis <-
  read.csv(
    here("processed/2022-Jun-2023-Jun__PHCs__IDAL-NAP_hmis_data.csv")
  )
# read processed HMIS data with morbidity
hmis_mor <-
  read.csv(
    here("processed/2023-JAN-JUN__PHCs__IDAL-NAP_WithMorbiditiy_hmis_data_withmorbities.csv")
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
      unite_dest, month, year, is_it_more_than_5,
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
      unite_dest, month, year,
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


# keep the items we need only
sy_course_isy_m <-
  isy_m %>%
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
    unite_dest, month, year, is_it_more_than_5,
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
    unite_dest, month, year,
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

# keep the items we need only
sy_course_isy_d <-
  isy_d %>%
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
    unite_dest, month, year, is_it_more_than_5,
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
    unite_dest, month, year,
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