# indonesia economic recovery

# overall inflation

# author: dzulfiqar fathur rahman
# created: 2021-03-08
# last updated: 2021-07-15
# page: inflation


# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(httr)
library(jsonlite)
library(plotly)
library(magick)
library(ggtext)


# data --------------------------------------------------------------------

# api key
if (!exists("BPS_KEY")) {
  BPS_KEY <- Sys.getenv("BPS_KEY")
}

# api url
## dyanmic table
base_url_dynamic <- "https://webapi.bps.go.id/v1/api/list"

## static table
base_url_static <- "https://webapi.bps.go.id/v1/api/view"

## annual inflation rate ----

# request data
inf_yoy_req <- GET(
  base_url_static,
  query = list(
    model = "statictable",
    domain = "0000",
    lang = "ind",
    id = "915",
    key = BPS_KEY
  )
)

# parse response
inf_yoy_parsed <- content(inf_yoy_req, "text") %>% 
  fromJSON()

# download data as temporary file
GET(
  inf_yoy_parsed$data$excel, 
  write_disk(inf_yoy_temp <- tempfile(fileext = ".xls"))
)

# import
inf_yoy_raw <- read_excel(inf_yoy_temp, skip = 2, na = "")

# latest year
inf_yoy_yr_latest <- names(inf_yoy_raw)[ncol(inf_yoy_raw)]

# date variable
inf_date_seq <- seq(
  ymd(str_c(inf_yoy_yr_latest, "01-01")), 
  ymd(str_c(inf_yoy_yr_latest, "12-01")),
  by = "month"
)

# replace month names
inf_yoy_raw$Bulan[1:12] <- as.character(inf_date_seq)

# correct date type
inf_yoy_raw$Bulan <- ymd(inf_yoy_raw$Bulan)

# remove table notes
inf_yoy_raw <- inf_yoy_raw %>% 
  dplyr::filter(!is.na(Bulan))

# tidy data
inf_yoy_tidy <- inf_yoy_raw %>% 
  pivot_longer(2:ncol(.), names_to = "yr", values_to = "rate_yoy") %>% 
  mutate(mo = format(Bulan, "-%m-01")) %>% 
  arrange(yr, mo) %>% 
  group_by(yr, Bulan) %>% 
  mutate(date = ymd(str_c(yr, mo)), mo = month(date)) %>% 
  ungroup() %>% 
  select(date, mo, yr, rate_yoy) %>% 
  dplyr::filter(!is.na(rate_yoy), yr >= 2020)


## monthly inflation rate ----

# request data
inf_mom_req <- GET(
  base_url_dynamic,
  query = list(
    model = "data",
    domain = "0000",
    var = "1708",
    key = BPS_KEY
  )
)

# parse response
inf_mom_parsed <- content(inf_mom_req, "text") %>% 
  fromJSON()

# extract keys
## city
inf_mom_key_ntl <- as_tibble(inf_mom_parsed$vervar) %>% 
  dplyr::filter(label == "INDONESIA")

## year
inf_mom_key_yr <- as_tibble(inf_mom_parsed$tahun)

## data
inf_mom_raw <- as_tibble(inf_mom_parsed$datacontent)

# subset national inflation
inf_mom_tidy <- inf_mom_raw %>% 
  pivot_longer(1:ncol(.), names_to = "key", values_to = "rate_mom") %>% 
  separate(key, into = c("key_exp", "key_period"), sep = "17080") %>% 
  mutate(
    key_yr = str_sub(key_period, 1, 3),
    key_mo = str_sub(key_period, 4, 5)
  ) %>% 
  dplyr::filter(key_exp == "9999")

# replace year key
inf_mom_tidy$key_yr <- inf_mom_tidy$key_yr %>% 
  str_replace_all(deframe(inf_mom_key_yr))

# replace month key, create date variable
inf_mom_trf <- inf_mom_tidy %>% 
  mutate(
    key_mo = case_when(
      key_mo != c(as.character(c("10", "11", "12"))) ~ str_c("0", key_mo),
      TRUE ~ as.character(key_mo)
    ),
    key_mo = str_c("-", key_mo, "-01"),
    date = ymd(str_c(key_yr, key_mo)),
    mo = month(date),
    yr = key_yr
  ) %>%  
  select(date, mo, yr, rate_mom)


## merge annual, monthly inflation data ----
inf_mom_yoy <- inf_mom_trf %>% 
  left_join(inf_yoy_tidy, by = c("date", "mo", "yr"))


# plot --------------------------------------------------------------------

## monthly inflation rate ----

# annotations
# 2020
anno_mom_2020 <- list(
  text = "<b>2020</b>",
  font = list(size = 12, color = "lightgrey"),
  bgcolor = "white",
  showarrow = F,
  xref = "x",
  x = 2.75,
  xanchor = "left",
  yref = "y",
  y = 0.25,
  yanchor = "top"
)

# 2021
anno_mom_2021 <- list(
  text = "<b>2021</b>",
  font = list(size = 12, color = "#2477B3"),
  bgcolor = "white",
  showarrow = F,
  xref = "x",
  x = 1,
  xanchor = "left",
  yref = "y",
  y = 0.05,
  yanchor = "top"
) 

#  plot
plot_inf_mom <- plot_ly(
  inf_mom_yoy,
  type = "scatter",
  mode = "markers+lines",
  line = list(width = 3),
  colors = c("lightgrey", "#2477B3"),
  text = ~format(date, "%b %Y"),
  hovertemplate = "Inflation rate: %{y} percent<br>Date: %{text}<extra></extra>",
  height = 300
) %>% 
  add_trace(
    x = ~mo,
    y = ~rate_mom,
    color = ~factor(yr)
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      autorange = T,
      fixedrange = T,
      tickmode = "array",
      tickvals = seq(1, 12),
      ticktext = format(seq(ymd("2021-01-01"), ymd("2021-12-01"), "1 month"), "%b"),
      ticks = "outside",
      automargin = T,
      showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = NA,
      type = "linear",
      autorange = F,
      range = c(-0.5, 0.6),
      fixedrange = T,
      dtick = 0.25,
      showline = F,
      showgrid = T,
      gridcolor = "#CFD8DC",
      zerolinecolor = "#E68F7E",
      side = "right"
    ),
    annotations = list(anno_mom_2020, anno_mom_2021),
    showlegend = F,
    margin = list(l = 0, r = 0, t = 15, b = 0),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)


## annual inflation rate ----

# annotations
# 2020
anno_yoy_2020 <- list(
  text = "<b>2020</b>",
  font = list(size = 12, color = "lightgrey"),
  bgcolor = "white",
  showarrow = F,
  xref = "x",
  x = 6,
  xanchor = "left",
  yref = "y",
  y = 2.4,
  yanchor = "top"
)

# 2021
anno_yoy_2021 <- list(
  text = "<b>2021</b>",
  font = list(size = 12, color = "#2477B3"),
  bgcolor = "white",
  showarrow = F,
  xref = "x",
  x = 1.5,
  xanchor = "left",
  yref = "y",
  y = 1.2,
  yanchor = "top"
) 

# plot
plot_inf_yoy <- inf_mom_yoy %>% 
  dplyr::filter(!is.na(rate_yoy)) %>% 
  plot_ly(
    type = "scatter",
    mode = "markers+lines",
    line = list(width = 3),
    colors = c("lightgrey", "#2477B3"),
    text = ~format(date, "%b %Y"),
    hovertemplate = "Inflation rate: %{y} percent<br>Date: %{text}<extra></extra>",
    height = 300
  ) %>% 
  add_trace(
    x = ~mo,
    y = ~rate_yoy,
    color = ~factor(yr)
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      autorange = T,
      fixedrange = T,
      tickmode = "array",
      tickvals = seq(1, 12),
      ticktext = format(seq(ymd("2021-01-01"), ymd("2021-12-01"), "1 month"), "%b"),
      ticks = "outside",
      automargin = T,
      showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = NA,
      type = "linear",
      autorange = F,
      range = c(0, 3.1),
      fixedrange = T,
      dtick = 0.6,
      showline = F,
      showgrid = T,
      gridcolor = "#CFD8DC",
      zerolinecolor = "#E68F7E",
      side = "right"
    ),
    annotations = list(anno_yoy_2020, anno_yoy_2021),
    showlegend = F,
    margin = list(l = 0, r = 0, t = 15, b = 0),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)


# export chart ------------------------------------------------------------

# rename column for csv
inf_mom_yoy_tidy <- inf_mom_yoy %>% 
  select(-c("mo", "yr")) %>% 
  rename(inflation_mom = 2, inflation_yoy = 3)

# path to inflation data
path_data_inf_ov <- "data/ier_inflation-overall_cleaned.csv"


## monthly inflation rate ----

if (nrow(inf_mom_yoy_tidy) != nrow(read_csv(path_data_inf_ov))) {
  
  # import functions to apply custom ggplot2 theme and add logo
  source("script/ggplot2_theme.R")
  
  # annotations
  anno_year_mom <- tibble(
    x = c(0.75, 2),
    y = c(0.1, 0.35),
    label = c("2021", "2020")
  )
  
  # plot
  ggplot(inf_mom_yoy, aes(mo, rate_mom)) +
    geom_hline(yintercept = 0, color = "#E68F7E") +
    geom_line(aes(color = as_factor(yr)), lwd = 1, show.legend = F) +
    geom_point(aes(color = as_factor(yr)), size = 1.5, show.legend = F) +
    scale_x_continuous(
      breaks = seq(1, 12),
      labels = format(seq(ymd("2021-01-01"), ymd("2021-12-01"), "1 month"), "%b")
    ) +
    scale_y_continuous(
      breaks = seq(-0.5, 0.5, 0.25),
      labels = c(-0.5, -0.25, 0, 0.25, 0.5),
      limits = c(-0.5, 0.5),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(values = c("lightgrey", "#2477B3")) +
    geom_richtext(
      data = anno_year_mom,
      aes(x, y, label = label),
      fill = "white",
      label.color = NA,
      text.color = c("#2477B3", "lightgrey"),
      hjust = 0,
      size = 3,
      fontface = "bold"
    ) +
    labs(
      title = "Monthly inflation",
      subtitle = "(percent)",
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Statistics Indonesia (BPS)"
    ) +
    theme_ier()
  
  # path to the plot
  path_plot_inf_mom <- "fig/ier_inflation-monthly_plot.png"
  
  # save the plot
  ggsave(path_plot_inf_mom, width = 6, height = 3.708, dpi = 300)
  
  # add ier logo to the plot
  add_ier_logo(path_plot_inf_mom)
  
  # message
  message("The monthly inflation rate chart has been updated")
  
} else {
  
  message("The monthly inflation rate chart is up to date")
  
}


## annual inflation rate ----

if (!is.na(last(inf_mom_yoy_tidy$inflation_mom)) && is.na(last(inf_mom_yoy_tidy$inflation_yoy))) {
  
  # import functions to apply custom ggplot2 theme and add logo
  source("script/ggplot2_theme.R")
  
  # annotations
  anno_year_yoy <- tibble(
    x = c(1.5, 5.5),
    y = c(1.8, 2.4),
    label = c("2021", "2020")
  )
  
  # plot
  inf_mom_yoy %>% 
    dplyr::filter(!is.na(rate_yoy)) %>% 
    ggplot(aes(mo, rate_yoy)) +
    geom_line(aes(color = as_factor(yr)), lwd = 1, show.legend = F) +
    geom_point(aes(color = as_factor(yr)), size = 1.5, show.legend = F) +
    scale_x_continuous(
      breaks = seq(1, 12),
      labels = format(seq(ymd("2021-01-01"), ymd("2021-12-01"), "1 month"), "%b")
    ) +
    scale_y_continuous(
      breaks = seq(0, 3, 0.6),
      labels = c(0, seq(0.6, 2.4, 0.6), 3),
      limits = c(0, 3),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(values = c("lightgrey", "#2477B3")) +
    geom_richtext(
      data = anno_year_yoy,
      aes(x, y, label = label),
      fill = "white",
      label.color = NA,
      text.color = c("#2477B3", "lightgrey"),
      hjust = 0,
      size = 3,
      fontface = "bold"
    ) +
    labs(
      title = "Annual inflation",
      subtitle = "(percent)",
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Statistics Indonesia (BPS)"
    ) +
    theme_ier()
  
  # path to the plot
  path_plot_inf_yoy <- "fig/ier_inflation-annual_plot.png"
  
  # save the plot
  ggsave(path_plot_inf_yoy, width = 6, height = 3.708, dpi = 300)
  
  # add ier logo to the plot
  add_ier_logo(path_plot_inf_yoy)
  
  # message
  message("The annual inflation rate chart has been updated")
  
} else {
  
  message("The annual inflation rate chart is up to date")
  
}


# export preview chart ----------------------------------------------------

if (!is.na(last(inf_mom_yoy_tidy$inflation_mom)) && is.na(last(inf_mom_yoy_tidy$inflation_yoy))) {
  
  # plot
  ggplot(inf_mom_yoy, aes(mo, rate_yoy)) +
    geom_line(aes(color = as_factor(yr)), lwd = 1.5, show.legend = F) +
    geom_point(aes(color = as_factor(yr)), size = 2.5, show.legend = F) +
    scale_y_continuous(
      breaks = seq(0, 3, 0.6),
      labels = c(0, seq(0.6, 2.4, 0.6), 3),
      limits = c(0, 3),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(values = c("lightgrey", "#2477B3")) +
    theme_void() +
    theme_ier_pre()
  
  # path to preview chart
  path_plot_inf_pre <- "fig/ier_inflation-annual_void_plot.png"
  
  # save the plot  
  ggsave(path_plot_inf_pre, width = 13.3, height = 6.6, dpi = 300)
  
  # message
  message("The annual inflation rate preview chart has been updated")
  
} else {
  
  message("The annual inflation rate preview chart is up to date")
  
}


# export data -------------------------------------------------------------

if (!file.exists(path_data_inf_ov)) {
  
  write_csv(inf_mom_yoy_tidy, path_data_inf_ov)
  
  message("The overall inflation dataset has been exported")
  
} else if (nrow(inf_mom_yoy_tidy) != nrow(read_csv(path_data_inf_ov))) {
  
  write_csv(inf_mom_yoy_tidy, path_data_inf_ov)
  
  message("The overall inflation dataset has been updated")
  
} else {
  
  message("The overall inflation dataset is up to date")
  
}