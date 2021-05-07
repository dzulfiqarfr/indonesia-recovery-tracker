# indonesia economic recovery

# inflation

# author: dzulfiqar fathur rahman
# created: 2021-03-08
# last updated: 2021-05-09
# page: inflation


# setup -------------------------------------------------------------------

# packages
library(tidyverse)
library(readxl)
library(lubridate)
library(httr)
library(jsonlite)
library(plotly)
library(magick)
library(ggtext)

# api key
if (exists("BPS_KEY") == F) {
  BPS_KEY <- Sys.getenv("BPS_KEY")
}

# bps api url
## dyanmic table
base_url_dynamic <- "https://webapi.bps.go.id/v1/api/list"

## statictable
base_url_static <- "https://webapi.bps.go.id/v1/api/view"


# data --------------------------------------------------------------------

# annual inflation rate ---------------------------------------------------

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
  pivot_longer(
    2:ncol(.),
    names_to = "yr",
    values_to = "rate_yoy"
  ) %>% 
  mutate(mo = format(Bulan, "-%m-01")) %>% 
  arrange(yr, mo) %>% 
  group_by(yr, Bulan) %>% 
  mutate(
    date = str_c(yr, mo), 
    mo = month(date)
  ) %>% 
  ungroup() %>% 
  select(date, mo, yr, rate_yoy) %>% 
  dplyr::filter(
    !is.na(rate_yoy),
    yr >= 2020 
  )

# correct data type
inf_yoy_tidy$date <- ymd(inf_yoy_tidy$date)


# monthly inflation rate --------------------------------------------------

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
  pivot_longer(
    1:ncol(.),
    names_to = "key",
    values_to = "rate_mom"
  ) %>% 
  separate(
    key,
    into = c("key_exp", "key_period"),
    sep = "17080"
  ) %>% 
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
      key_mo != c(as.character(seq(10, 12, 1))) ~ str_c("0", key_mo),
      TRUE ~ as.character(key_mo)
    ),
    key_mo = str_c("-", key_mo, "-01"),
    date = ymd(str_c(key_yr, key_mo)),
    mo = month(date),
    yr = key_yr
  ) %>%  
  select(date, mo, yr, rate_mom)


# merge data --------------------------------------------------------------

inf_mom_yoy <- inf_mom_trf %>% 
  left_join(inf_yoy_tidy, by = c("date", "mo", "yr"))


# plot --------------------------------------------------------------------

# monthly inflation rate --------------------------------------------------

# annotations
# 2020
anno_mom_2020 <- list(
  text = "<b>2020</b>",
  font = list(size = 12, color = "#90A4AE"),
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
  font = list(size = 12, color = "#1d81a2"),
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
  colors = c("#CFD8DC", "#1d81a2"),
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
      tickvals = c(3, 6, 9, 12),
      ticktext = c("Mar", "Jun", "Sep", "Dec"),
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
      zerolinecolor = "#ff856c",
      side = "right"
    ),
    annotations = list(anno_mom_2020, anno_mom_2021),
    showlegend = F,
    margin = list(l = 0, r = 0, t = 15, b = 0),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)


# annual inflation rate ---------------------------------------------------

# annotations
# 2020
anno_yoy_2020 <- list(
  text = "<b>2020</b>",
  font = list(size = 12, color = "#90A4AE"),
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
  font = list(size = 12, color = "#1d81a2"),
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
    colors = c("#CFD8DC", "#1d81a2"),
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
      tickvals = c(3, 6, 9, 12),
      ticktext = c("Mar", "Jun", "Sep", "Dec"),
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
      zerolinecolor = "#ff856c",
      side = "right"
    ),
    annotations = list(anno_yoy_2020, anno_yoy_2021),
    showlegend = F,
    margin = list(l = 0, r = 0, t = 15, b = 0),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)


# export chart ------------------------------------------------------------

# latest data
inf_mom_yoy_tidy <- inf_mom_yoy %>% 
  select(-c("mo", "yr")) %>% 
  rename(inflation_mom = 2, inflation_yoy = 3)

# export chart
if (nrow(inf_mom_yoy_tidy) != nrow(read_csv("data/ier_inflation-overall_cleaned.csv")) || 
    !is.na(inf_mom_yoy_tidy$inflation_yoy)) {
  
  # monthly inflation rate ----
  
  # annotations
  anno_year_mom <- tibble(
    x = c(0.75, 2),
    y = c(0.1, 0.35),
    label = c("2021", "2020")
  )
  
  # plot
  ggplot(inf_mom_yoy, aes(mo, rate_mom)) +
    geom_hline(yintercept = 0, color = "#ff856c") +
    geom_line(aes(color = as_factor(yr)), lwd = 1, show.legend = F) +
    geom_point(aes(color = as_factor(yr)), size = 1.5, show.legend = F) +
    scale_x_continuous(
      breaks = seq(2, 12, 2),
      labels = c("Feb", "April", "June", "Aug", "Oct", "Dec")
    ) +
    scale_y_continuous(
      breaks = seq(-0.5, 0.5, 0.25),
      labels = c(-0.5, -0.25, 0, 0.25, 0.5),
      limits = c(-0.5, 0.5),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(values = c("#CFD8DC", "#1d81a2")) +
    geom_richtext(
      data = anno_year_mom,
      aes(x, y, label = label),
      fill = "white",
      label.color = NA,
      text.color = c("#1d81a2", "#90A4AE"),
      hjust = 0,
      size = 3,
      fontface = "bold"
    ) +
    labs(
      title = "Inflation",
      subtitle = "Monthly inflation rate (in percent)",
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Statistics Indonesia (BPS)"
    ) +
    theme(
      text = element_text(size = 12),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(color = "black"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#CFD8DC"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.spacing.x = unit(2, "lines"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 35)),
      plot.caption = element_text(
        color = "#757575",
        hjust = 0,
        margin = margin(t = 35)
      )
    ) +
    ggsave(
      "fig/ier_inflation-monthly_plot.png",
      width = 7,
      height = 4,
      dpi = 300
    )
  
  # add logo
  ier_logo <- image_read("images/ier_hexsticker_small.png")
  
  # add base plot
  plot_inf_mom_png <- image_read("fig/ier_inflation-monthly_plot.png")
  
  # get plot height
  plot_height <- magick::image_info(plot_inf_mom_png)$height
  
  # get plot width
  plot_width <- magick::image_info(plot_inf_mom_png)$width
  
  # get logo height
  logo_width <- magick::image_info(ier_logo)$width
  
  # get logo width
  logo_height <- magick::image_info(ier_logo)$height
  
  # position for the bottom 1.5 percent
  pos_bottom <- plot_height - logo_height - plot_height * 0.015
  
  # position for the right 1.5 percent
  pos_right <- plot_width - logo_width - 0.015 * plot_width
  
  # overwrite plot
  plot_inf_mom_png %>% 
    image_composite(ier_logo, offset = str_c("+", pos_right, "+", pos_bottom)) %>% 
    image_write("fig/ier_inflation-monthly_plot.png")
  
  
  # annual inflation rate ----
  
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
      breaks = seq(2, 12, 2),
      labels = c("Feb", "April", "June", "Aug", "Oct", "Dec")
    ) +
    scale_y_continuous(
      breaks = seq(0, 3, 0.6),
      labels = c(0, seq(0.6, 2.4, 0.6), 3),
      limits = c(0, 3),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(values = c("#CFD8DC", "#1d81a2")) +
    geom_richtext(
      data = anno_year_yoy,
      aes(x, y, label = label),
      fill = "white",
      label.color = NA,
      text.color = c("#1d81a2", "#90A4AE"),
      hjust = 0,
      size = 3,
      fontface = "bold"
    ) +
    labs(
      title = "Inflation",
      subtitle = "Annual inflation rate (in percent)",
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Statistics Indonesia (BPS)"
    ) +
    theme(
      text = element_text(size = 12),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(color = "black"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#CFD8DC"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.spacing.x = unit(2, "lines"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 35)),
      plot.caption = element_text(
        color = "#757575",
        hjust = 0,
        margin = margin(t = 35)
      )
    ) +
    ggsave(
      "fig/ier_inflation-annual_plot.png",
      width = 7,
      height = 4,
      dpi = 300
    )
  
  # add logo
  ier_logo <- image_read("images/ier_hexsticker_small.png")
  
  # add base plot
  plot_inf_yoy_png <- image_read("fig/ier_inflation-annual_plot.png")
  
  # get plot height
  plot_height <- magick::image_info(plot_inf_yoy_png)$height
  
  # get plot width
  plot_width <- magick::image_info(plot_inf_yoy_png)$width
  
  # get logo height
  logo_width <- magick::image_info(ier_logo)$width
  
  # get logo width
  logo_height <- magick::image_info(ier_logo)$height
  
  # position for the bottom 1.5 percent
  pos_bottom <- plot_height - logo_height - plot_height * 0.015
  
  # position for the right 1.5 percent
  pos_right <- plot_width - logo_width - 0.015 * plot_width
  
  # overwrite plot
  plot_inf_yoy_png %>% 
    image_composite(ier_logo, offset = str_c("+", pos_right, "+", pos_bottom)) %>% 
    image_write("fig/ier_inflation-annual_plot.png")
  
  # message
  message("The annual and monthly inflation rate charts have been updated")
  
} else {
  
  message("The annual and monthly inflation rate charts are up to date")
  
}


# preview -----------------------------------------------------------------

if (nrow(inf_mom_yoy_tidy) != nrow(read_csv("data/ier_inflation-overall_cleaned.csv"))) {
  
  # plot
  ggplot(inf_mom_yoy, aes(mo, rate_yoy)) +
    geom_line(aes(color = as_factor(yr)), lwd = 2, show.legend = F) +
    geom_point(aes(color = as_factor(yr)), size = 3.5, show.legend = F) +
    scale_x_continuous(
      breaks = seq(2, 12, 2),
      labels = c("Feb", "April", "June", "Aug", "Oct", "Dec")
    ) +
    scale_y_continuous(
      breaks = seq(0, 3, 0.6),
      labels = c(0, seq(0.6, 2.4, 0.6), 3),
      limits = c(0, 3),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(values = c("#CFD8DC", "#1d81a2")) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#263238", color = NA),
      plot.margin = margin(t = 50, r = 50, b = 50, l = 50)
    ) +
    ggsave(
      "fig/ier_inflation-annual_void_plot.png",
      width = 13.3,
      height = 6.6,
      dpi = 300
    )
  
  # message
  message("The annual inflation rate preview chart has been updated")
  
} else {
  
  message("The annual inflation rate preview chart is up to date")
  
}


# export data -------------------------------------------------------------

# write csv
if (file.exists("data/ier_inflation-overall_cleaned.csv") == F) {
  
  write_csv(inf_mom_yoy_tidy, "data/ier_inflation-overall_cleaned.csv")
  
  message("The overall inflation dataset has been exported")
  
} else if (nrow(inf_mom_yoy_tidy) != nrow(read_csv("data/ier_inflation-overall_cleaned.csv"))) {
  
  write_csv(inf_mom_yoy_tidy, "data/ier_inflation-overall_cleaned.csv")
  
  message("The overall inflation dataset has been updated")
  
} else {
  
  message("The overall inflation dataset is up to date")
  
}