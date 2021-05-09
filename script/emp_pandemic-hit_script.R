# indonesia economic recovery

# workers affected by the pandemic

# author: dzulfiqar fathur rahman
# created: 2021-05-08
# last updated: 2021-05-09
# page: employment


# setup -------------------------------------------------------------------

# packages
library(tidyverse)
library(lubridate)
library(plotly)
library(reactable)
library(reactablefmtr)
library(magick)


# data --------------------------------------------------------------------

# import
phw_raw <- read_csv("data/bps_pandemic-hit-workers_raw.csv")

# column names containing category and date
phw_col_names <- str_c(
  c(rep("men", 2),
    rep("women", 2), 
    rep("urban", 2), 
    rep("rural", 2),
    rep("total", 2)
  ),
  rep(c("2020-08-01", "2021-02-01"), 5),
  sep = "_"
)

# replace column names
names(phw_raw)[2:ncol(phw_raw)] <- phw_col_names

names(phw_raw)[1] <- "status"

# subset
phw_raw <- phw_raw %>% 
  dplyr::filter(!is.na(status)) %>% 
  slice(-c(1, 7)) %>% 
  mutate(
    status = c(
      "Unemployed",
      "Not in workforce",
      "Furloughed",
      "Reduced working hours",
      "total_dis",
      "share_of_working_age_pop"
    )
  )

# tidy
## number of affected workers
phw_tidy <- phw_raw %>% 
  slice(-c(5:6)) %>% 
  pivot_longer(2:ncol(.), names_to = "group", values_to = "workers") %>% 
  separate(
    group,
    into = c("category", "date"),
    sep = "_"
  ) %>% 
  mutate(
    category = as_factor(category), 
    date = ymd(date),
    workers = as.numeric(str_replace_all(workers, ",", "."))
  )

## total number of affected workers by group
phw_total_dis <- phw_raw %>% 
  dplyr::filter(status == "total_dis") %>% 
  pivot_longer(2:ncol(.), names_to = "group", values_to = "workers") %>% 
  separate(
    group,
    into = c("category", "date"),
    sep = "_"
  ) %>% 
  mutate(
    category = as_factor(category),
    date = ymd(date),
    workers = as.numeric(str_replace_all(workers, ",", "."))
  )

## share of affected workers in the working age population
phw_share_tidy <- phw_raw %>% 
  dplyr::filter(status == "share_of_working_age_pop") %>% 
  pivot_longer(2:ncol(.), names_to = "group", values_to = "share_of_working_age_pop") %>% 
  select(-1) %>%
  separate(
    group,
    into = c("category", "date"),
    sep = "_"
  ) %>% 
  mutate(
    category = as_factor(category),
    date = ymd(date),
    share_of_working_age_pop = as.numeric(str_replace_all(share_of_working_age_pop, ",", "."))
  )

# extract total, wide
phw_total_wide <- phw_tidy %>% 
  dplyr::filter(category == "total") %>% 
  pivot_wider(names_from = status, values_from = workers)

# calculate change
## by status
phw_trf <- phw_tidy %>% 
  dplyr::filter(category != "total") %>% 
  group_by(status, category) %>% 
  mutate(
    diff = workers - dplyr::lag(workers, 1),
    pct_chg = round(diff / dplyr::lag(workers, 1) * 100, 2)
  ) %>% 
  ungroup()

## total
phw_trf_tot_dis <- phw_total_dis %>% 
  group_by(category) %>% 
  mutate(
    diff = workers - dplyr::lag(workers, 1),
    pct_chg = round(diff / dplyr::lag(workers, 1) * 100, 2)
  ) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(pct_chg)) %>% 
  select(-diff)

## overall
phw_trf_overall <-  phw_tidy %>% 
  rbind(phw_total_dis) %>% 
  mutate(status = str_replace_all(status, "total_dis", "Overall")) %>% 
  group_by(status, category) %>% 
  mutate(
    diff = workers - dplyr::lag(workers, 1),
    pct_chg = round(diff / dplyr::lag(workers, 1) * 100, 2)
  ) %>% 
  ungroup() %>% 
  select(-diff)

# subset by sex, transform to wide
phw_trf_sex <- phw_trf %>% 
  dplyr::filter(category %in% c("men", "women"), !is.na(pct_chg)) %>%
  select(-c(diff, date)) %>% 
  pivot_wider(names_from = category, values_from = c(workers, pct_chg)) %>% 
  select(status, workers_men, pct_chg_men, workers_women, pct_chg_women)

# subset by area, transform to wide
phw_trf_area <- phw_trf %>% 
  dplyr::filter(category %in% c("urban", "rural"), !is.na(pct_chg)) %>% 
  select(-c(diff, date)) %>% 
  pivot_wider(names_from = category, values_from = c(workers, pct_chg)) %>% 
  select(status, workers_urban, pct_chg_urban, workers_rural, pct_chg_rural)


# plot --------------------------------------------------------------------

## total ----
plot_phw_total <- plot_ly(
  data = phw_total_wide,
  type = "bar",
  name = "Reduced working hours",
  x = ~date,
  y = ~`Reduced working hours`,
  hovertemplate = str_c(
    "<b>Status: reduced working hours</b><br><br>",
    "Number of workers: %{y} million<br>",
    "Date: %{x}",
    "<extra></extra>"
  ),
  marker = list(color = "#1d81a2"),
  height = 375
) %>% 
  add_trace(
    name = "Unemployed",
    y = ~Unemployed,
    hovertemplate = str_c(
      "<b>Status: unemployed</b><br><br>",
      "Number of workers: %{y} million<br>",
      "Date: %{x}",
      "<extra></extra>"
    ),
    marker = list(color = "#60b4d7")
  ) %>% 
  add_trace(
    name = "Furloughed",
    y = ~Furloughed,
    hovertemplate = str_c(
      "<b>Status: furloughed</b><br><br>",
      "Number of workers: %{y} million<br>",
      "Date: %{x}",
      "<extra></extra>"
    ),
    marker = list(color = "#ff725b")
  ) %>% 
  add_trace(
    name = "Not in workforce",
    y = ~`Not in workforce`,
    hovertemplate = str_c(
      "<b>Status: not in workforce</b><br><br>",
      "Number of workers: %{y} million<br>",
      "Date: %{x}",
      "<extra></extra>"
    ),
    marker = list(color = "#ffd882")
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      fixedrange = T,
      tickmode = "array",
      tickvals = c(phw_total_wide$date[1], phw_total_wide$date[2]),
      ticks = "outside",
      automargin = T,
      tickformat = "%b<br>%Y",
      hoverformat = "%b %Y",
      showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = NA,
      type = "linear",
      autorange = F,
      range = c(0, 31),
      fixedrange = T,
      dtick = 6,
      showline = F,
      showgrid = T,
      gridcolor = "#CFD8DC",
      side = "right"
    ),
    legend = list(
      orientation = "h",
      itemsizing = "constant",
      xanchor = "left",
      y = -.15,
      yanchor = "top"
    ),
    margin = list(l = 0, r = 0, t = 15, b = 0),
    barmode = "relative",
    bargap = 0.6
  ) %>% 
  plotly::config(displayModeBar = F)


## by sex ----

reactable_phw_sex <- reactable(
  phw_trf_sex,
  columns = list(
    status = colDef(name = "", minWidth = 150),
    workers_men = colDef(
      name = str_c(
        "Feb '21",
        "<br>",
        '<div style="color: #999; font-size: 12px">(millions)</div>'
      ),
      html = T
    ),
    pct_chg_men = colDef(
      name = str_c(
        "Change*",
        "<br>",
        '<div style="color: #999; font-size: 12px">(percent)</div>'
      ),
      html = T,
      style = function(value) {
        
        font_color <- if (value < 0) {
          "#1d81a2"
        } else {
          "#ff725b"
        }
        
        list(color = font_color, fontWeight = "bold")
        
      }
    ),
    workers_women = colDef(
      name = str_c(
        "Feb '21",
        "<br>",
        '<div style="color: #999; font-size: 12px">(millions)</div>'
      ),
      html = T
    ),
    pct_chg_women = colDef(
      name = str_c(
        "Change*",
        "<br>",
        '<div style="color: #999; font-size: 12px">(percent)</div>'
      ),
      html = T,
      style = function(value) {
        
        font_color <- if (value < 0) {
          "#1d81a2"
        } else {
          "#ff725b"
        }
        
        list(color = font_color, fontWeight = "bold")
        
      }
    )
  ),
  columnGroups = list(
    colGroup(
      name = "<b>Men</b>",
      columns = c("workers_men", "pct_chg_men"),
      html = T
    ),
    colGroup(
      name = "<b>Women</b>",
      columns = c("workers_women", "pct_chg_women"),
      html = T
    )
  ),
  showPageInfo = F,
  highlight = T,
  style = list(fontSize = "15px"),
  theme = reactableTheme(
    headerStyle = list(borderColor = "black")
  )
)


## by area ----

reactable_phw_area <- reactable(
  phw_trf_area,
  columns = list(
    status = colDef(name = "", minWidth = 150),
    workers_urban = colDef(
      name = str_c(
        "Feb '21",
        "<br>",
        '<div style="color: #999; font-size: 12px">(millions)</div>'
      ),
      html = T
    ),
    pct_chg_urban = colDef(
      name = str_c(
        "Change*",
        "<br>",
        '<div style="color: #999; font-size: 12px">(percent)</div>'
      ),
      html = T,
      style = function(value) {
        
        font_color <- if (value < 0) {
          "#1d81a2"
        } else {
          "#ff725b"
        }
        
        list(color = font_color, fontWeight = "bold")
        
      }
    ),
    workers_rural = colDef(
      name = str_c(
        "Feb '21",
        "<br>",
        '<div style="color: #999; font-size: 12px">(millions)</div>'
      ),
      html = T
    ),
    pct_chg_rural = colDef(
      name = str_c(
        "Change*",
        "<br>",
        '<div style="color: #999; font-size: 12px">(percent)</div>'
      ),
      html = T,
      style = function(value) {
        
        font_color <- if (value < 0) {
          "#1d81a2"
        } else {
          "#ff725b"
        }
        
        list(color = font_color, fontWeight = "bold")
        
      }
    )
  ),
  columnGroups = list(
    colGroup(
      name = "<b>Urban</b>",
      columns = c("workers_urban", "pct_chg_urban"),
      html = T
    ),
    colGroup(
      name = "<b>Rural</b>",
      columns = c("workers_rural", "pct_chg_rural"),
      html = T
    )
  ),
  showPageInfo = F,
  highlight = T,
  style = list(fontSize = "15px"),
  theme = reactableTheme(
    headerStyle = list(borderColor = "black")
  )
)


# export data -------------------------------------------------------------

if (file.exists("data/ier_pandemic-hit-workers_cleaned.csv") == F) {
  
  write_csv(phw_trf_overall, "data/ier_pandemic-hit-workers_cleaned.csv")
  
  message("The pandemic-hit workers dataset has been exported")
  
} else if (nrow(phw_trf_overall) != nrow(read_csv("data/ier_pandemic-hit-workers_cleaned.csv"))) {
  
  write_csv(phw_trf_overall, "data/ier_pandemic-hit-workers_cleaned.csv")
  
  message("The pandemic-hit workers dataset has been updated")
  
} else {
  
  message("The pandemic-hit workers dataset is up to date")
  
}
