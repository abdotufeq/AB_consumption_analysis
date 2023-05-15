library(here)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(viridis)


# defining function to plot the ratio of (Access, Watch and non AB)
# will need 3 arguments df dataframe containg course from isystock
# hf the health facility name and True for adult age group
plot_access_watch <- function(df,hf,age_cat){
  p <- 
    df %>%
    select(
      unite_dest, month, 
      is_it_more_than_5, 
      Access, Watch
    ) %>%
    filter(
      is_it_more_than_5 == age_cat
    ) %>%
    group_by(
      unite_dest, month
    ) %>%
    summarise(
      Access,
      Watch,
      total_AB = sum(Access, Watch),
      access_percentage = round(
        100 * sum(Access) / (sum(Access) + sum(Watch)),0
      ),
      watch_percentage = round(
        100 * sum(Watch) / (sum(Access) + sum(Watch)),0 
      ),
      .groups = "drop"
    ) %>%
    filter(unite_dest == hf) %>%
    plot_ly(x = ~month) %>% 
    add_lines(
      y = ~total_AB, 
      name = "number of course of all AB",
      text = ~paste0(
        "number of all AB cources for<br>", 
        month.name[month],
        " is : ", 
        total_AB
      ),
      hoverinfo = "text"
    ) %>% 
    add_bars(
      y = ~Watch, 
      color = I("red"), 
      name = "watch group",
      stroke = I("black"),
      span = I(1),
      text = ~paste0(
        "Number of Watch group courses in<br>",
        month.name[month],
        "= ",
        Watch,
        "<br>the Watch group percenatge for<br>",
        month.name[month],
        " is: ",
        watch_percentage,
        "%"
      ),
      hoverinfo = "text"
    ) %>%
    add_bars(
      y = ~Access, 
      color = I("green"), 
      name = "acces group",
      stroke = I("black"),
      span = I(1),
      text = ~paste0(
        "Number of Access group courses in<br>",
        month.name[month],
        "= ",
        Access,
        "<br>the Access group percenatge for<br>",
        month.name[month],
        " is: ",
        access_percentage,
        "%"
      ),
      hoverinfo = "text"
    ) %>%
    add_text(
      x = ~month,
      y = ~Access,
      text = ~paste0(access_percentage, "%"),
      textposition = "top right",
      color = I("green"),
      showlegend = FALSE,
      size = I(9)
    ) %>%
    add_text(
      x = ~month,
      y = ~Watch,
      text = ~paste0(watch_percentage, "%"),
      textposition = "top left",
      color = I("red"),
      showlegend = FALSE,
      size = I(9)
    ) %>% 
    layout(
      title = list(
        text = ~paste0(
          hf, 
          if_else(
            age_cat, 
            "- 5 years or more", 
            "- Under 5 years"
          ),
          "<br>Access vs. Watch",
          "<br>Target for Access is 60-70%"
        ),
        x = 0.01,
        font = list(
          family = "Times New Roman",
          color = I("black")
        )
      ),
      xaxis = list(
        title_text= "", 
        tickangle = -45,
        #autotick = FALSE,
        tickmode = "array",
        tickvals = c(1:12),
        ticktext = month.abb[1:12]
      ),
      yaxis = list(
        title.text = "number of courses per month"
      )
    ) %>%
    config(
      edits = list(
        annotationPosition = TRUE,
        annotationTail = TRUE,
        annotationText = TRUE
      )
    )
  return(p)
}

# defining function to plot the ratio of (Access, Watch and non AB)
# will need 2 arguments df dataframe containg course from isystock
# hf the health facility name and True for adult age group
mission_plot_access_watch <- function(df,age_cat){
  p <- 
    df %>%
    select(
      month, 
      is_it_more_than_5, 
      Access, Watch
    ) %>%
    filter(
      is_it_more_than_5 == age_cat
    ) %>%
    group_by(
      month
    ) %>%
    summarise(
      Access = sum(Access),
      Watch = sum(Watch),
      total_AB = sum(Access, Watch),
      access_percentage = round(
        100 * sum(Access) / (sum(Access) + sum(Watch)),0
      ),
      watch_percentage = round(
        100 * sum(Watch) / (sum(Access) + sum(Watch)),0 
      ),
      .groups = "drop"
    ) %>%
    unique() %>% 
    plot_ly(x = ~month) %>% 
    add_lines(
      y = ~total_AB, 
      name = "number of course of all AB",
      text = ~paste0(
        "number of all AB cources for<br>", 
        month.name[month],
        " is : ", 
        total_AB
      ),
      hoverinfo = "text"
    ) %>% 
    add_bars(
      y = ~Watch, 
      color = I("red"), 
      name = "watch group",
      stroke = I("black"),
      span = I(1),
      text = ~paste0(
        "Number of Watch group courses in<br>",
        month.name[month],
        "= ",
        Watch,
        "<br>the Watch group percenatge for<br>",
        month.name[month],
        " is: ",
        watch_percentage,
        "%"
      ),
      hoverinfo = "text"
    ) %>%
    add_bars(
      y = ~Access, 
      color = I("green"), 
      name = "acces group",
      stroke = I("black"),
      span = I(1),
      text = ~paste0(
        "Number of Access group courses in<br>",
        month.name[month],
        "= ",
        Access,
        "<br>the Access group percenatge for<br>",
        month.name[month],
        " is: ",
        access_percentage,
        "%"
      ),
      hoverinfo = "text"
    ) %>%
    add_text(
      x = ~month,
      y = ~Access,
      text = ~paste0(access_percentage, "%"),
      textposition = "top right",
      color = I("green"),
      showlegend = FALSE,
      size = I(9)
    ) %>%
    add_text(
      x = ~month,
      y = ~Watch,
      text = ~paste0(watch_percentage, "%"),
      textposition = "top left",
      color = I("red"),
      showlegend = FALSE,
      size = I(9)
    ) %>%
    layout(
      title = list(
        text = ~paste0(
          "Syria Mission ",
          if_else(
            age_cat, 
            "- 5 years or more", 
            "- Under 5 years"
          ),
          "<br>Access vs. Watch",
          "<br>Target for Access is 60-70%"
        ),
        x = 0.01,
        font = list(
          family = "Times New Roman",
          color = I("black")
        )
      ),
      xaxis = list(
        title_text= "", 
        tickangle = -45,
        #autotick = FALSE,
        tickmode = "array",
        tickvals = c(1:12),
        ticktext = month.abb[1:12]
      ),
      yaxis = list(
        title.text = "number of courses per month"
      )
    ) %>%
    config(
      edits = list(
        annotationPosition = TRUE,
        annotationTail = TRUE,
        annotationText = TRUE
      )
    )
  return(p)
}
# defining function to plot the ratio of (AB and analgesics to total consultaion)
# will need 4 arguments df_isy dataframe containg course from isystock
# df_hmis dataframe contining consultation data from HMIS
# hf the health facility name and True for adult age group
plot_AB_consult <- function(df_isy, df_hmis, hf,age_cat){
  p <- 
    df_isy %>%
    select(
      unite_dest, month, 
      is_it_more_than_5, 
      Access, Watch, not_AB
    ) %>%
    inner_join(
      .,
      df_hmis,
      by = c(
        "unite_dest" = "unit",
        "month" = "month",
        "is_it_more_than_5" = "is_it_more_than_5"
      )
    ) %>% 
    filter(
      is_it_more_than_5 == age_cat
    ) %>%
    group_by(
      unite_dest, month
    ) %>%
    summarise(
      not_AB,
      total_AB = sum(Access, Watch),
      AB_per_consult = round(
        100 * sum(total_AB) / total_consultation ,0
      ),
      anlg_cons = round(
        100 * sum(not_AB) / total_consultation ,0
      ),
      Antenatal.Care,
      External.Consultations,
      Postnatal.Care,
      Emergency.Room,
      total_consultation,
      .groups = "drop"
    ) %>%
    filter(unite_dest == hf) %>%
    plot_ly(x = ~month) %>%
    add_bars(
      y = ~total_consultation, 
      name = "# of consultations",
      span = I(1),
      stroke = I("black"),
      color = I("blue"),
      text = ~paste0(
        "Number of all consultations for<br>", 
        month.name[month],
        " is : ", 
        total_consultation
      ),
      hoverinfo = "text"
    ) %>% 
    add_bars(
      y = ~total_AB, 
      name = "# of courses of all AB",
      span = I(1),
      stroke = I("black"),
      color = I("red"),
      text = ~paste0(
        "Number of all AB cources for<br>", 
        month.name[month],
        " is : ", 
        total_AB
      ),
      hoverinfo = "text"
    ) %>% 
    add_bars(
      y = ~not_AB, 
      name = "# of courses of Profen & Sytamol",
      span = I(1),
      stroke = I("black"),
      color = I("grey"),
      text = ~paste0(
        "Number of all Profen &<br>Sytamol courses for ", 
        month.name[month],
        "<br>is: ", 
        not_AB
      ),
      hoverinfo = "text"
    ) %>%
    add_annotations(
      x = ~month,
      y = ~total_AB,
      text = ~paste0(AB_per_consult, "%"),
      font = list(color = ("red")),
      arrowcolor = I("red"),
      bordercolor = I("red"),
      xanchor = "center",
      showlegend = FALSE
    ) %>% 
    add_annotations(
      x = ~month,
      y = ~not_AB,
      text = ~paste0(anlg_cons, "%"),
      font = list(color = "grey"),
      xanchor = "left",
      arrowcolor = I("grey"),
      bordercolor = I("grey"),
      showlegend = FALSE,
      xshift = 33
    ) %>% 
    layout(
      title = list(
        text = ~paste0(
          hf, 
          if_else(
            age_cat, 
            "- 5 Years or more", 
            "- Under 5 Years" 
          ),
          "<br>Antibiotic percentage to # consultation"
        ),
        x = 0.01,
        font = list(
          family = "Times New Roman",
          color = I("black")
        )
      ),
      xaxis = list(
        title_text= "", 
        tickangle = -45,
        #autotick = FALSE,
        tickmode = "array",
        tickvals = c(1:12),
        ticktext = month.abb[1:12]
      ),
      yaxis = list(
        title.text = "count"
      )
    ) %>%
    config(
      edits = list(
        annotationPosition = TRUE,
        annotationTail = TRUE,
        annotationText = TRUE
      )
    )
  return(p)
}

# defining function to plot the ratio of (AB and analgesics to total consultaion)
# will need 4 arguments df_isy dataframe containg course from isystock
# df_hmis dataframe contining consultation data from HMIS
# hf the health facility name and True for adult age group
mission_plot_AB_consult <- function(df_isy, df_hmis,age_cat){
  p <- 
    df_isy %>%
    select(
      unite_dest, month, 
      is_it_more_than_5, 
      Access, Watch, not_AB
    ) %>%
    inner_join(
      .,
      df_hmis,
      by = c(
        "unite_dest" = "unit",
        "month" = "month",
        "is_it_more_than_5" = "is_it_more_than_5"
      )
    ) %>% 
    filter(
      is_it_more_than_5 == age_cat
    ) %>%
    group_by(
      month
    ) %>%
    summarise(
      not_AB = sum(not_AB),
      total_AB = sum(Access, Watch),
      AB_per_consult = round(
        100 * sum(total_AB) / sum(total_consultation) ,0
      ),
      anlg_cons = round(
        100 * sum(not_AB) / sum(total_consultation) ,0
      ),
      Antenatal.Care = sum(Antenatal.Care),
      External.Consultations = sum(External.Consultations),
      Postnatal.Care = sum(Postnatal.Care),
      Emergency.Room = sum(Emergency.Room),
      total_consultation = sum(total_consultation),
      .groups = "drop"
    ) %>%
    plot_ly(x = ~month) %>%
    add_bars(
      y = ~total_consultation, 
      name = "consultations",
      span = I(1),
      stroke = I("black"),
      color = I("blue"),
      text = ~paste0(
        "Number of all consultations for<br>", 
        month.name[month],
        " is : ", 
        total_consultation
      ),
      hoverinfo = "text"
    ) %>% 
    add_bars(
      y = ~total_AB, 
      name = "Antibiotics",
      span = I(1),
      stroke = I("black"),
      color = I("red"),
      text = ~paste0(
        "Number of all AB cources for<br>", 
        month.name[month],
        " is : ", 
        total_AB
      ),
      hoverinfo = "text"
    ) %>% 
    add_bars(
      y = ~not_AB, 
      name = "Antipyretics",
      span = I(1),
      stroke = I("black"),
      color = I("grey"),
      text = ~paste0(
        "Number of all Profen &<br>Sytamol courses for ", 
        month.name[month],
        "<br>is: ", 
        not_AB
      ),
      hoverinfo = "text"
    ) %>%
    add_annotations(
      x = ~month,
      y = ~total_AB,
      text = ~paste0(AB_per_consult, "%"),
      font = list(color = ("red")),
      arrowcolor = I("red"),
      bordercolor = I("red"),
      xanchor = "center",
      showlegend = FALSE
    ) %>% 
    add_annotations(
      x = ~month,
      y = ~not_AB,
      text = ~paste0(anlg_cons, "%"),
      font = list(color = "grey"),
      xanchor = "left",
      arrowcolor = I("grey"),
      bordercolor = I("grey"),
      showlegend = FALSE,
      xshift = 25
    ) %>% 
    layout(
      title = list(
        text = ~paste0(
          "Syria Mission", 
          if_else(
            age_cat, 
            "- 5 Years or more", 
            "- Under 5 Years" 
          ),
          "<br>Antibiotic percentage to # consultation"
        ),
        x = 0.01,
        font = list(
          family = "Times New Roman",
          color = I("black")
        )
      ),
      xaxis = list(
        title_text= "", 
        tickangle = -45,
        #autotick = FALSE,
        tickmode = "array",
        tickvals = c(1:12),
        ticktext = month.abb[1:12]
      ),
      yaxis = list(
        title.text = "count"
      )
    ) %>%
    config(
      edits = list(
        annotationPosition = TRUE,
        annotationTail = TRUE,
        annotationText = TRUE
      )
    )
  return(p)
}

atc3_plot <- function(df_isy, hf, age_cat){
  atc3_lst <- unique(sentinel_list$atc_3)
  p <- 
    df_isy %>%
    select(
      unite_dest, month, 
      is_it_more_than_5, 
      all_of(atc3_lst)
    ) %>% 
    filter(
      is_it_more_than_5 == age_cat
    ) %>%
    select(-is_it_more_than_5) %>% 
    pivot_longer(
      cols = c(-unite_dest, -month),
      names_to = "rank",
      values_to = "qty",
      values_drop_na = TRUE
    ) %>%
    filter(unite_dest == hf) %>%
    group_by(
      rank
    ) %>%
    plot_ly(
      x = ~month
    ) %>%
    add_bars(
      x = ~month,
      y = ~qty,
      split = ~rank,
      text = ~paste0(
        "Number of courses of<br>",
        rank,
        "<br>is :",
        qty
      ),
      hoverinfo = "text"
    ) %>% 
    layout(
      title = list(
        text = ~paste0(
          hf,
          if_else(
            age_cat,
            "- 5 or more",
            "- Under 5"
          )
        ),
        x = 0.01,
        font = list(
          family = "Times New Roman",
          color = I("black")
        )
      ),
      xaxis = list(
        title_text= "",
        tickangle = -45,
        tickmode = "array",
        tickvals = c(1:12),
        ticktext = month.abb[1:12]
      ),
      yaxis = list(
        title.text = "number of courses per month"
      ),
      showlegend = TRUE
    ) 
  return(p)
}

# defining function to plot the ratio of injectables to oral 
# will need 4 arguments df_isy dataframe containg course from isystock
# df_hmis dataframe contining consultation data from HMIS
# hf the health facility name and True for adult age group
plot_AB_admn_consult <- function(df_isy, df_hmis, hf, age_cat){
  p <- 
    df_isy %>%
    # filter(
    #   not_AB == 0
    # ) %>% 
    select(
      unite_dest, month, 
      is_it_more_than_5, 
      `O`, `P`
    ) %>%
    inner_join(
      .,
      df_hmis,
      by = c(
        "unite_dest" = "unit",
        "month" = "month",
        "is_it_more_than_5" = "is_it_more_than_5"
      )
    ) %>% 
    filter(
      is_it_more_than_5 == age_cat
    ) %>%
    group_by(
      unite_dest, month
    ) %>%
    summarise(
      `O` = sum(`O`),
      `P` = sum(`P`),
      total_AB_admin = sum(`O`, `P`),
      O_per_cons = round(
        100 * sum(`O`) / total_consultation ,0
      ),
      P_per_cons = round(
        100 * sum(`P`) / total_consultation ,0
      ),
      total_consultation,
      .groups = "drop"
    ) %>%
    filter(unite_dest == hf) %>%
    plot_ly(x = ~month) %>%
    add_bars(
      y = ~total_consultation, 
      name = "# of consultations",
      span = I(1),
      stroke = I("black"),
      color = I("blue"),
      text = ~paste0(
        "Number of all consultations for<br>", 
        month.name[month],
        " is : ", 
        total_consultation
      ),
      hoverinfo = "text"
    ) %>% 
    add_bars(
      y = ~`O`, 
      name = "# of courses of Oral AB",
      span = I(1),
      stroke = I("black"),
      color = I("green"),
      text = ~paste0(
        "Number of oral AB cources for<br>", 
        month.name[month],
        " is : ", 
        `O`
      ),
      hoverinfo = "text"
    ) %>% 
    add_bars(
      y = ~`P`, 
      name = "# of courses of Parenteral AB",
      span = I(1),
      stroke = I("black"),
      color = I("darkred"),
      text = ~paste0(
        "Number of all Parenteral AB<br>courses for ", 
        month.name[month],
        "<br>is: ", 
        `P`
      ),
      hoverinfo = "text"
    ) %>%
    add_annotations(
      x = ~month,
      y = ~`O`,
      text = ~paste0(O_per_cons, "%"),
      font = list(color = ("green")),
      arrowcolor = I("green"),
      bordercolor = I("green"),
      xanchor = "center",
      showlegend = FALSE
    ) %>% 
    add_annotations(
      x = ~month,
      y = ~`P`,
      text = ~paste0(P_per_cons, "%"),
      font = list(color = "darkred"),
      xanchor = "left",
      arrowcolor = I("darkred"),
      bordercolor = I("darkred"),
      showlegend = FALSE,
      xshift = 50
    ) %>% 
    layout(
      title = list(
        text = ~paste0(
          hf, 
          if_else(
            age_cat, 
            "- 5 Years or more", 
            "- Under 5 Years" 
          ),
          "<br>Administration route percentage to # consultation"
        ),
        x = 0.01,
        font = list(
          family = "Times New Roman",
          color = I("black")
        )
      ),
      xaxis = list(
        title_text= "", 
        tickangle = -45,
        #autotick = FALSE,
        tickmode = "array",
        tickvals = c(1:12),
        ticktext = month.abb[1:12]
      ),
      yaxis = list(
        title.text = "count"
      )
    ) %>%
    config(
      edits = list(
        annotationPosition = TRUE,
        annotationTail = TRUE,
        annotationText = TRUE
      )
    )
  return(p)
}

write_AB_consult <- function(df_isy, df_hmis){
  p <- 
    df_isy %>%
    select(
      unite_dest, month, 
      is_it_more_than_5, 
      Access, Watch, not_AB
    ) %>%
    inner_join(
      .,
      df_hmis,
      by = c(
        "unite_dest" = "unit",
        "month" = "month",
        "is_it_more_than_5" = "is_it_more_than_5"
      )
    ) %>% 
    group_by(
      unite_dest, month
    ) %>%
    summarise(
      not_AB,
      total_AB = sum(Access, Watch),
      AB_per_consult = round(
        100 * sum(total_AB) / total_consultation ,0
      ),
      anlg_cons = round(
        100 * sum(not_AB) / total_consultation ,0
      ),
      Antenatal.Care,
      External.Consultations,
      Postnatal.Care,
      Emergency.Room,
      total_consultation,
      is_it_more_than_5,
      .groups = "drop"
    )
  write.xlsx(
    p,
    glue(here("processed/AB_cons.xlsx"))
  )
}


write_access_watch <- function(df){
  p <- 
    df %>%
    select(
      unite_dest, month, 
      is_it_more_than_5, 
      Access, Watch
    ) %>%
    group_by(
      unite_dest, month, is_it_more_than_5
    ) %>%
    summarise(
      Access,
      Watch,
      total_AB = sum(Access, Watch),
      access_percentage = round(
        100 * sum(Access) / (sum(Access) + sum(Watch)),0
      ),
      watch_percentage = round(
        100 * sum(Watch) / (sum(Access) + sum(Watch)),0 
      ),
      is_it_more_than_5,
      .groups = "drop"
    )
  write.xlsx(
    p,
    glue(here("processed/AB_access_watch.xlsx"))
  )
}
