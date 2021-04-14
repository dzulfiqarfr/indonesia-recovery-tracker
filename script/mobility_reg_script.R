# indonesia economic recovery

# google covid-19 community mobility reports
# regional

# author: dzulfiqar fathur rahman
# created: 2021-03-03
# last updated: 2021-04-14
# page: mobility


# setup -------------------------------------------------------------------

# packages
library(tidyverse)
library(lubridate)
library(zoo)
library(plotly)
library(crosstalk)

# download url
if (exists("mob_url") == F) {
  mob_url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
}


# data --------------------------------------------------------------------

# import global dataset
if (exists("mob_gbl_raw") == F) {
  
  mob_gbl_raw <- read_csv(
    mob_url,
    na = "",
    col_types = cols(
      census_fips_code = col_character(),
      metro_area = col_character(),
      sub_region_2 = col_character()
    )
  )
  
} else {
  
  rm(mob_gbl_raw)
  
}

# subset indonesia observations
if (exists("mob_idn_raw") == F) {
  
  # subset, rename variable
  mob_idn_raw <- mob_gbl_raw %>% 
    dplyr::filter(country_region == "Indonesia") %>% 
    select(
      country_region,
      sub_region_1,
      date,
      retail_and_recreation_percent_change_from_baseline,
      grocery_and_pharmacy_percent_change_from_baseline,
      parks_percent_change_from_baseline,
      transit_stations_percent_change_from_baseline,
      workplaces_percent_change_from_baseline,
      residential_percent_change_from_baseline
    ) %>% 
    rename(
      "country" = 1,
      "province" = 2,
      "date" = 3,
      "retail_recreation" = 4,
      "grocery_pharmacy" = 5,
      "parks" = 6,
      "transit_stations" = 7,
      "workplaces" = 8,
      "residential" = 9
    )
  
  # correct data types for `country`, `province`
  mob_idn_raw[, 1:2] <- lapply(mob_idn_raw[, 1:2], as.factor)
  
}

# subset regional data
mob_reg_raw <- mob_idn_raw %>% 
  dplyr::filter(!is.na(province)) %>%  
  dplyr::select(-1) 

# calculate 7-day moving averages
mob_reg_trf <- mob_reg_raw %>% 
  group_by(province) %>% 
  arrange(date) %>% 
  mutate(
    retail_recreation_avg = rollmean(retail_recreation, k = 7, fill = NA, align = "right"),
    grocery_pharmacy_avg = rollmean(grocery_pharmacy, k = 7, fill = NA, align = "right"),
    parks_avg = rollmean(parks, k = 7, fill = NA, align = "right"),
    transit_stations_avg = rollmean(transit_stations, k = 7, fill = NA, align = "right"),
    workplaces_avg = rollmean(workplaces, k = 7, fill = NA, align = "right"),
    residential_avg = rollmean(residential, k = 7, fill = NA, align = "right")
  ) %>% 
  ungroup() %>% 
  arrange(province, date)

# round moving averages to two decimal places
mob_reg_trf[, 3:ncol(mob_reg_trf)] <- lapply(
  mob_reg_trf[, 3:ncol(mob_reg_trf)],
  function(x) {round(x, 2)}
)


# export ------------------------------------------------------------------

# latest observation in most recent data
mob_reg_raw_last <- mob_reg_raw %>% 
  select(date) %>% 
  dplyr::filter(date == last(date), !duplicated(date)) %>% 
  deframe()

# latest observation in csv
mob_reg_raw_csv_last <- read_csv(
  "data/ier_mobility-idn_tidy.csv",
  col_types = cols(province = col_character())
) %>%  
  select(date) %>% 
  dplyr::filter(date == last(date), !duplicated(date)) %>% 
  deframe()

# data
mob_reg_trf_tidy <- mob_reg_trf %>% 
  pivot_longer(
    3:ncol(.),
    names_to = "place_category",
    values_to = "change_from_baseline"
  )

# write csv
if (file.exists("data/ier_mobility-reg_tidy.csv") == F) {
  
  write_csv(mob_reg_trf_tidy, "data/ier_mobility-reg_cleaned.csv")
  
  message("The regional mobility dataset has been exported")
  
} else if (mob_reg_raw_last != mob_reg_raw_csv_last) {
  
  write_csv(mob_reg_trf_tidy, "data/ier_mobility-reg_cleaned.csv")
  
  message("The regional mobility dataset has been updated")
  
} else {
  
  message("The regional mobility dataset is up to date")
  
}


# plot --------------------------------------------------------------------

# covid text annotation
anno_text_covid <- list(
  text = "<b>COVID-19<br>pandemic</b>",
  font = list(size = 7, color = "#90A4AE"),
  align = "left",
  bgcolor = "white",
  showarrow = F,
  xref = "x",
  x = "2020-03-10",
  xanchor = "left",
  yref = "y",
  y = 95,
  yanchor = "top"
) 

# subtitles
## retail & recreation
anno_sub_retail <- list(
  text = "<b>Retail and<br>recreation</b>",
  font = list(size = 10),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0,
  xanchor = "left",
  yref = "paper",
  y = 1.125,
  yanchor = "top"
)

## grocery & pharma
anno_sub_grocery <- list(
  text = "<b>Grocery and<br>pharmacy</b>",
  font = list(size = 10),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.345,
  xanchor = "left",
  yref = "paper",
  y = 1.125,
  yanchor = "top"
)

## parks
anno_sub_parks <- list(
  text = "<b>Parks</b>",
  font = list(size = 10),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.675,
  xanchor = "left",
  yref = "paper",
  y = 1.125,
  yanchor = "top"
)

## transit stations
anno_sub_transit <- list(
  text = "<b>Transit stations</b>",
  font = list(size = 10),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0,
  xanchor = "left",
  yref = "paper",
  y = 0.475,
  yanchor = "top"
)

## workplaces
anno_sub_workplace <- list(
  text = "<b>Workplace</b>",
  font = list(size = 10),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.345,
  xanchor = "left",
  yref = "paper",
  y = 0.475,
  yanchor = "top"
)

## residential
anno_sub_res <- list(
  text = "<b>Residential</b>",
  font = list(size = 10),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.675,
  xanchor = "left",
  yref = "paper",
  y = 0.475,
  yanchor = "top"
)

# variable names
mob_reg_trf_var <- names(mob_reg_trf)[9:14]

# shared data
mob_reg_trf_shared <- mob_reg_trf %>% 
  select(province, date, ends_with("_avg")) %>% 
  highlight_key()

# plot
plot_mob_reg <- lapply(
  mob_reg_trf_var,
  function(x) {
    plot_ly(
      mob_reg_trf_shared,
      type = "scatter",
      mode = "markers+lines",
      text = x %>% 
        str_remove("_avg") %>% 
        str_to_sentence() %>% 
        str_replace_all("_", " and ") %>% 
        str_replace("Transit and stations", "Transit stations")
    ) %>% 
      add_lines(
        x = ~date,
        y = as.formula(str_c("~", x)),
        hovertemplate = "<b>Category: %{text}</b><br><br>7-day moving average: %{y} percent<br>Date: %{x}<extra></extra>",
        line = list(width = 2, color = "#1d81a2")
      ) %>%
      plotly::layout(
        xaxis = list (
          title = NA,
          autorange = F,
          range = c(
            as.character(first(mob_ntl_trf$date) - 14),
            as.character(last(mob_ntl_trf$date) + 14)
          ),
          fixedrange = T,
          ticks = "outside",
          automargin = T,
          tickfont = list(size = 8),
          tickformat = "%b<br>'%y",
          hoverformat = "%b %d, %Y",
          showline = T,
          showgrid = F
        ),
        yaxis = list(
          title = NA,
          type = "linear",
          autorange = F,
          range = c(-100, 105),
          fixedrange = T,
          dtick = 50,
          tickfont = list(size = 8),
          showline = F,
          showgrid = T,
          gridcolor = "#CFD8DC",
          zerolinecolor = "#ff856c",
          side = "left"
        ),
        shapes = list(
          list(
            type = "line",
            layer = "below",
            xref = "x",
            x0 = "2020-03-02",
            x1 = "2020-03-02",
            yref = "y",
            y0 = -100,
            y1 = 100,
            line = list(color = "#90A4AE", width = 1, dash = "dot")
          )
        ),
        autosize = T,
        showlegend = F
      ) %>% 
      plotly::config(displayModeBar = F)
  }
)

# small multiples
sm_mob_reg <- subplot(
  plot_mob_reg, 
  nrows = 2,
  margin = c(0.0125, 0.0125, 0.1, 0.1),
  shareX = F,
  shareY = T
) %>% 
  plotly::layout(
    annotations = list(
      anno_text_covid,
      anno_sub_retail,
      anno_sub_grocery,
      anno_sub_parks,
      anno_sub_transit,
      anno_sub_workplace,
      anno_sub_res
    ),
    margin = list(l = 0, r = 0, t = 45, b = 0),
    clickmode = "none"
  )

# filter by province
filter_by_reg <- filter_select(
  "reg",
  "Select a province...",
  mob_reg_trf_shared,
  ~province,
  multiple = F
)
