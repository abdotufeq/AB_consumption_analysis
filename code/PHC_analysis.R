library(here)
library(skimr)
library(ggtext)
library(RColorBrewer)
library(haven)
library(plotly)
library(GGally)
library(scales)
library(ggrepel)

source(here("code/04_read_IDAL_processed_data.R"))



idal_df_course <- 
  inner_join(
    idal_hmis,
    idal_wd_course_isy,
    by = c(
      "unit" = "unite_dest",
      "month" = "month",
      "is_it_more_than_5" = "is_it_more_than_5"
    )
  ) %>%
  mutate(
    unit = factor(unit)
  ) %>% 
  pivot_longer(
    cols = -c(
      unit, month,
      is_it_more_than_5
    ),
    names_to = "parameter",
    values_to = "value",
    values_drop_na = T
  ) %>% 
  filter(
    value > 0
  )


AB_course_stats <-
  idal_df_course %>%
  filter(
    parameter %in% sentinel_list$aware | 
      parameter == "total_consultation"
  ) %>%
  group_by(
    unit, month, is_it_more_than_5, parameter
  ) %>%
  summarise(
    para = sum(value),
    .groups = "drop"
  ) %>% 
  pivot_wider(
    names_from = parameter,
    values_from = para,
    values_fill = 0
  ) %>% 
  group_by(
    unit, month
  ) %>% 
  mutate(
    AB = sum(Access, Watch),
    access_rel = percent(sum(Access)/AB),
    watch_rel = percent(sum(Watch)/AB)
  ) %>% 
  ungroup() %>%
  group_by(
    unit, month
  ) %>% 
  mutate(
    all_consultation = sum(total_consultation)
  ) %>% 
  ungroup() 


annotation_qt <- 
  AB_qt_stats %>% 
  group_by(
    unit, month 
  ) %>% 
  mutate(
    Watch = sum(Watch),
    Access = sum(Access)
  ) %>% 
  ungroup() %>% 
  select(
    unit, month, 
    Access, Watch, 
    access_rel,watch_rel
  ) %>% 
  unique()

annotation_course <- 
  AB_course_stats %>% 
  group_by(
    unit, month 
  ) %>% 
  mutate(
    Watch = sum(Watch),
    Access = sum(Access)
  ) %>% 
  ungroup() %>% 
  select(
    unit, month, 
    Access, Watch, 
    access_rel,watch_rel
  ) %>% 
  unique()

idal_aware_qty <- 
  AB_qt_stats %>%
  pivot_longer(
    cols = c(
      Access, Watch, AB
    ),
    names_to = "antibiotics",
    values_to = "quantity"
  ) %>% 
  group_by(
    unit, is_it_more_than_5, month, antibiotics
    ) %>% 
  ggplot(
    aes(
      x = month 
      )
    ) +
  geom_col(
    aes(
      y = quantity,
      fill = antibiotics
    ),
    position = "dodge"
  ) +
  scale_fill_manual(
    name = NULL,
    breaks = c(
      "Access",
      "Watch",
      "AB",
      "TRUE",
      "FALSE"
      ),
    labels = c(
      "Access group Antibiotics",
      "Wactch group Antibiotics",
      "Total Antibiotics quantity",
      "more than 5 years",
      "less than 5 years"
      ),
    values = c("blue","red","darkgreen", "darkgrey", "black")
  ) +
    geom_col(
      data = AB_qt_stats,
      aes(
        x = month,
        y = total_consultation,
        fill = is_it_more_than_5,
        alpha = 0.6
      ),
      position = "stack",
      show.legend = FALSE
    ) +
  facet_wrap(
    ~unit, 
    nrow = 3, ncol = 3, 
    scales = "free_y"
    ) +
  scale_y_continuous(
    expand = c(0, NA)
    ) +
  labs(
    x = NULL,
    y = "Quantity of Antibiotics prescribed per month"
    ) +
  geom_label(
    data = annotation_qt,
    aes(
      y = Watch,
      label = watch_rel
      ),
    nudge_x = 0.25,
    nudge_y = 0.2,
    size = 3,
    alpha = 0.4
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_markdown(),
    legend.text = element_markdown(),
    legend.position = c(0.8, 0.1)
    )


ggsave(
  here("figures/idal_quanities_Ab.png"), 
  idal_aware_qty, 
  width = 8, height = 4.5,
  dpi = 1080)

idal_aware_course <- 
  AB_course_stats %>%
  pivot_longer(
    cols = c(
      Access, Watch, AB
    ),
    names_to = "antibiotics",
    values_to = "quantity"
  ) %>% 
  group_by(
    unit, is_it_more_than_5, month, antibiotics
  ) %>% 
  ggplot(
    aes(
      x = month 
    )
  ) +
  geom_col(
    aes(
      y = quantity,
      fill = antibiotics
    ),
    position = "dodge"
  ) +
  scale_fill_manual(
    name = NULL,
    breaks = c(
      "Access",
      "Watch",
      "AB",
      "TRUE",
      "FALSE"
    ),
    labels = c(
      "Access group Antibiotics",
      "Wactch group Antibiotics",
      "Total Antibiotics quantity",
      "more than 5 years",
      "less than 5 years"
    ),
    values = c("blue","red","darkgreen", "darkgrey", "black")
  ) +
  geom_col(
    data = AB_course_stats,
    aes(
      x = month,
      y = total_consultation,
      fill = is_it_more_than_5,
      alpha = 0.6
    ),
    position = "stack",
    show.legend = FALSE
  ) +
  facet_wrap(
    ~unit, 
    nrow = 3, ncol = 3, 
    scales = "free_y"
  ) +
  scale_y_continuous(
    expand = c(0, NA)
  ) +
  labs(
    x = NULL,
    y = "courses of Antibiotics prescribed per month"
  ) +
  geom_label(
    data = annotation_course,
    aes(
      y = Watch,
      label = watch_rel
    ),
    nudge_x = 0.25,
    nudge_y = 0.2,
    size = 3,
    alpha = 0.4
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_markdown(),
    legend.text = element_markdown(),
    legend.position = c(0.8, 0.1)
  )


ggsave(
  here("figures/idal_courses_Ab.png"), 
  idal_aware_course, 
  width = 8, height = 4.5,
  dpi = 1080)


