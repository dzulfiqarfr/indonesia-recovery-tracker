# indonesia economic recovery

# overall unemployment rate

# author: dzulfiqar fathur rahman
# created: 2021-03-23
# last updated: 2021-04-13
# page: employment


# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(plotly)
library(magick)


# data --------------------------------------------------------------------

# api key
if (!exists("BPS_KEY")) {
  BPS_KEY <- Sys.getenv("BPS_KEY")
}

# bps api url
if (!exists("base_url")) {
  base_url <- "https://webapi.bps.go.id/v1/api/list"
}

if (!exists("unemployment_req")) {
  
  # request data
  unemployment_req <- GET(
    base_url,
    query = list(
      model = "data",
      domain = "0000",
      var = "529",
      key = BPS_KEY
    )
  )
  
  # parse response
  unemployment_parsed <- content(unemployment_req, "text") %>% 
    fromJSON()
  
  # extract keys
  ## activities
  unemployment_key_act <- as_tibble(unemployment_parsed$vervar)
  
  ## year
  unemployment_key_yr <- as_tibble(unemployment_parsed$tahun) 
  
  # extract data
  unemployment_raw <- as_tibble(unemployment_parsed$datacontent)
  
  # tidy data
  unemployment_tidy <- unemployment_raw %>% 
    pivot_longer(1:ncol(.), names_to = "key", values_to = "val") %>% 
    separate(key, into = c("key_act", "key_date"), sep = "5290") %>% 
    mutate(
      key_yr = case_when(
        str_detect(key_date, "^[8|9]") ~ as.numeric(str_sub(key_date, 1, 2)),
        str_detect(key_date, "^1") ~ as.numeric(str_sub(key_date, 1, 3))
      ),
      key_mo = case_when(
        str_detect(key_date, "^[8|9]") ~ as.numeric(str_sub(key_date, 3, 5)),
        str_detect(key_date, "^1") ~ as.numeric(str_sub(key_date, 4, 6))
      )
    ) %>% 
    select(key_act, key_date, key_yr, key_mo, val)
  
  # add anchor to year key
  unemployment_key_yr$val <- str_c("^", unemployment_key_yr$val, "$")
  
  # replace year key
  unemployment_tidy$key_yr <- unemployment_tidy$key_yr %>% 
    str_replace_all(deframe(unemployment_key_yr)) %>% 
    as.numeric()
  
  # replace month key
  unemployment_tidy$key_mo <- unemployment_tidy$key_mo %>% 
    str_replace_all(c("^189$" = "-02-01", "^190$" = "-08-01", "^191$" = "-01-01"))
  
  # create date variable
  unemployment_tidy <- unemployment_tidy %>% 
    mutate(date = ymd(str_c(key_yr, key_mo)), mo = month(date)) %>% 
    rename(yr = key_yr) %>% 
    select(key_act, date, yr, mo, val)
  
  # add activity labels
  unemployment_tidy <- unemployment_tidy %>% 
    mutate(act = str_replace_all(unemployment_tidy$key_act, deframe(unemployment_key_act))) %>% 
    select(key_act, act, date, yr, mo, val)
  
}

# subset unemployment rate
unemp_rate <- unemployment_tidy %>% 
  dplyr::filter(key_act == 6, yr >= 2005) %>% 
  select(-c(1, 2, 4))

# calculate change, rename variable
unemp_trf <- unemp_rate %>% 
  group_by(mo) %>% 
  mutate(ppt_change_yoy = val - dplyr::lag(val, 1)) %>% 
  ungroup() %>% 
  select(-mo) %>% 
  rename(unemployment_rate = val)


# plot --------------------------------------------------------------------

# y-axis range
unemp_y_axis_range <- c(0, 12.5)

# plot
plot_unemp_rate <- plot_ly(
  unemp_trf,
  x = ~date,
  y = ~unemployment_rate,
  hovertemplate = "Unemployment rate: %{y} percent<br>Date: %{x}<extra></extra>",
  line = list(color = "#2477B3", width = 3),
  height = 300
) %>% 
  add_lines() %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      autorange = F,
      range = c(
        as.character(first(unemp_trf$date) - 180), 
        as.character(last(unemp_trf$date) + 180)
      ),
      fixedrange = T,
      tickmode = "auto",
      ticks = "outside",
      automargin = T,
      hoverformat = "%B %Y",
      showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = NA,
      type = "linear",
      autorange = F,
      range = unemp_y_axis_range,
      dtick = 3,
      fixedrange = T,
      showline = F,
      showgrid = T,
      gridcolor = "#CFD8DC",
      side = "right"
    ),
    shapes = list(
      list(
        type = "line",
        layer = "below",
        xref = "x",
        x0 = "2020-03-02",
        x1 = "2020-03-02",
        yref = "y",
        y0 = unemp_y_axis_range[1],
        y1 = unemp_y_axis_range[2] - 0.5,
        line = list(color = "#90A4AE", dash = "dash")
      )
    ),
    annotations = list(
      list(
        text = "COVID-19<br>pandemic",
        font = list(size = 12, color = "#90A4AE"),
        align = "right",
        bgcolor = "white",
        showarrow = F,
        xref = "x",
        x = "2020-02-01",
        xanchor = "right",
        yref = "y",
        y = unemp_y_axis_range[2] - 0.75,
        yanchor = "top"
      ) 
    ),
    margin = list(l = 0, r = 0, t = 15, b = 0),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)


# export chart ------------------------------------------------------------

# path to unemployment rate data
path_data_unemp_rate <- "data/ier_unemployment-rate_cleaned.csv"

# export chart
if (nrow(unemp_trf) != nrow(read_csv(path_data_unemp_rate))) {
  
  # import functions to apply custom ggplot2 theme and add logo
  source("script/ggplot2_theme.R")
  
  # plot
  ggplot(data = unemp_trf, aes(date, unemployment_rate)) +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      lty = "dashed"
    ) +
    geom_line(color = "#1d81a2", lwd = 1) +
    scale_x_date(
      breaks = seq(ymd("2005-01-01"), last(unemp_trf$date), by = "2 year"),
      labels = c(
        "2005", 
        c("'07", "'09"),
        str_c("'", seq(11, 21, 2))
      )
    ) +
    scale_y_continuous(
      breaks = seq(0, 12, 3),
      limits = c(0, 12),
      expand = c(0, 0),
      position = "right"
    ) +
    annotate(
      "text",
      x = ymd("2020-02-01"),
      y = 11,
      label = "COVID-19\npandemic",
      size = 3,
      hjust = 1,
      color = "#90A4AE"
    ) +
    labs(
      title = "Unemployment rate",
      subtitle = "(percent)",
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Statistics Indonesia (BPS)"
    ) +
    theme_ier() +
    theme(plot.caption = element_text(margin= margin(t = 35)))
  
  # path to the plot
  path_plot_unemp_rate <- "fig/ier_unemployment-rate_plot.png"
  
  # save the plot
  ggsave(path_plot_unemp_rate, width = 6, height = 3.708, dpi = 300)
  
  # add ier logo to the plot
  add_ier_logo(path_plot_unemp_rate)
  
  # message
  message("The unemployment rate chart has been updated")
  
} else {
  
  message("The unemployment rate chart is up to date")
  
}


# export preview chart ----------------------------------------------------

if (nrow(unemp_trf) != nrow(read_csv(path_data_unemp_rate))) {
  
  # import functions to apply custom ggplot2 theme and add logo
  source("script/ggplot2_theme.R")
  
  # plot
  ggplot(data = unemp_trf, aes(date, unemployment_rate)) +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      lty = "dashed"
    ) +
    geom_line(color = "#1d81a2", lwd = 1.5) +
    scale_y_continuous(
      breaks = seq(0, 12, 3),
      limits = c(0, 12),
      expand = c(0, 0),
      position = "right"
    ) +
    theme_void() +
    theme_ier_pre()
  
  # path to the plot
  path_plot_unemp_pre <- "fig/ier_unemployment-rate_void_plot.png"
  
  # save the plot
  ggsave(path_plot_unemp_pre, width = 13.3, height = 6.6, dpi = 300)
  
  # message
  message("The unemployment rate preview chart has been updated")
  
} else {
  
  message("The unemployment rate preview chart is up to date")
  
}


# export data -------------------------------------------------------------

if (!file.exists(path_data_unemp_rate)) {
  
  write_csv(unemp_trf, path_data_unemp_rate)
  
  message("The national unemployment rate dataset has been exported")
  
} else if (nrow(unemp_trf) != nrow(read_csv(path_data_unemp_rate))) {
  
  write_csv(unemp_trf, path_data_unemp_rate)
  
  message("The national unemployment rate dataset has been updated")
  
} else {
  
  message("The national unemployment rate dataset is up to date")
  
}