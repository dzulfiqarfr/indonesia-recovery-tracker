# indonesia economic recovery

# overall economic growth

# author: dzulfiqar fathur rahman
# created: 2021-02-21
# last updated: 2021-04-12
# page: gdp


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

if (!exists("growth_raw")) {
  
  # request data
  growth_req <- GET(
    base_url,
    query = list(
      model = "data",
      domain = "0000",
      var = "108",
      key = BPS_KEY
    )
  )
  
  # parse response
  growth_parsed <- content(growth_req, "text") %>% 
    fromJSON()
  
  # extract keys
  ## expenditure
  growth_key_exp <- as_tibble(growth_parsed$vervar)
  
  ### expenditure labels in english
  growth_key_exp <- growth_key_exp %>% 
    mutate(
      label_eng = c(
        "Household consumption",
        "Food and beverages, other than restaurants",
        "Clothes, footwear and their care services",
        "Housing and household supplies",
        "Healthcare and education",
        "Transportation and communication",
        "Restaurants and hotels",
        "Other",
        "NPISHs consumption",
        "Government consumption",
        "Collective consumption",
        "Individual consumption",
        "Gross fixed capital formation",
        "Buildings",
        "Machinery and equipment",
        "Vehicles",
        "Other equipments",
        "Cultivated biological resources",
        "Intellectual property products",
        "Changes in inventories",
        "Exports of goods and services",
        "Goods",
        "Non-oil and gas",
        "Oil and gas",
        "Services",
        "Imports of goods and services",
        "Goods",
        "Non-oil and gas",
        "Oil and gas",
        "Services",
        "Statistics discrepancy",
        "GDP"
      )
    )
  
  ## year
  growth_key_yr <- as_tibble(growth_parsed$tahun)
  
  # extract data
  growth_raw <- as_tibble(growth_parsed$datacontent)
  
}

# separate keys, subset quarterly observations
growth_tidy <- growth_raw %>% 
  pivot_longer(1:ncol(.), names_to = "key", values_to = "pct_change_yoy") %>% 
  separate(key, into = c("key_exp", "key_period"), sep = "108") %>%
  mutate(
    key_period_obs = str_sub(key_period, end = 1),
    key_yr = str_sub(key_period, 2, 4),
    key_q = str_sub(key_period, 5, 6)
  ) %>% 
  dplyr::filter(key_exp == "800", key_period_obs == "5", key_q != "35")

# replace year key
growth_tidy$key_yr <- growth_tidy$key_yr %>% 
  str_replace_all(deframe(growth_key_yr))

# replace quarter key
growth_tidy$key_q <- growth_tidy$key_q %>% 
  str_replace_all(
    c(
      "^31$" = "-01-01",
      "^32$" = "-04-01",
      "^33$" = "-07-01",
      "^34$" = "-10-01"
    )
  )

# create date variable
growth_tidy <- growth_tidy %>% 
  mutate(date = ymd(str_c(key_yr, key_q))) %>% 
  select(date, pct_change_yoy)


# plot --------------------------------------------------------------------

# quarter labels
labs_q <- str_c("Q", quarter(growth_tidy$date))

# y-axis range
growth_y_axis_range <- c(-12, 9)

# plot
plot_growth <- growth_tidy %>% 
  plot_ly(
    x = ~date,
    y = ~pct_change_yoy,
    text = labs_q,
    hovertemplate = str_c(
      "Growth: %{y} percent<br>",
      "Date: %{text} ",
      "%{x}",
      "<extra></extra>"
    ),
    line = list(color = "#2477B3", width = 3),
    height = 300
  ) %>% 
  add_lines() %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      range = c(
        as.character(first(growth_tidy$date) - 180), 
        as.character(last(growth_tidy$date) + 180)
      ),
      fixedrange = T,
      tickvals = c("2010", "2012", "2014", "2016", "2018", "2020"),
      ticks = "outside",
      automargin = T,
      tickformat = "%Y",
      hoverformat = "%Y",
      showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = NA,
      type = "linear",
      autorange = F,
      range = growth_y_axis_range,
      fixedrange = T,
      dtick = 4,
      showline = F,
      showgrid = T,
      gridcolor = "#CFD8DC",
      zerolinecolor = "#ff856c",
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
        y0 = growth_y_axis_range[1],
        y1 = growth_y_axis_range[2] - 1,
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
        y = growth_y_axis_range[1] + 4,
        yanchor = "top"
      )
    ),
    margin = list(l = 0, r = 0, t = 15, b = 0)
  ) %>% 
  plotly::config(displayModeBar = F)


# export chart ------------------------------------------------------------

# data
growth_exp_csv <- read_csv("data/ier_gdp-growth-exp_cleaned.csv")

# latest observation in most recent csv
growth_exp_csv_latest <- growth_exp_csv %>% 
  select(date) %>% 
  dplyr::filter(!duplicated(date), date == last(date)) %>% 
  deframe()

# latest observation in most recent data
growth_tidy_latest <- growth_tidy %>% 
  select(date) %>% 
  dplyr::filter(date == last(date)) %>% 
  deframe()

# export chart
if (growth_tidy_latest != growth_exp_csv_latest) {
  
  # import functions to apply custom ggplot2 theme and add logo
  source("script/ggplot2_theme.R")
  
  # labels
  labs_growth <- growth_tidy %>% 
    select(date) %>% 
    mutate(
      q = quarter(date), 
      yr = year(date),
      labs = str_c("Q", q, format(date, "\n%Y"))
    ) %>% 
    dplyr::filter(q == 1, yr %in% seq(2010, 2020, 2)) %>% 
    select(labs) %>% 
    deframe()
  
  # plot
  ggplot(growth_tidy, aes(date, pct_change_yoy)) +
    geom_hline(yintercept = 0, color = "#E68F7E") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_line(color = "#2477B3", lwd = 1) +
    scale_x_date(
      breaks = seq(ymd("2010-01-01"), ymd("2020-01-01"), by = "2 year"),
      labels = labs_growth
    ) +
    scale_y_continuous(
      breaks = seq(-12, 8, 4),
      limits = c(-12, 8),
      expand = c(0, 0),
      position = "right"
    ) +
    annotate(
      "text",
      x = ymd("2020-02-01"),
      y = -6,
      label = "COVID-19\npandemic",
      size = 3,
      hjust = 1,
      color = "#90A4AE"
    ) +
    labs(
      title = "Gross domestic product",
      subtitle = "(percent change from a year earlier)",
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Statistics Indonesia (BPS)"
    ) +
    theme_ier()
  
  # path to the plot
  path_plot_growth <- "fig/ier_gdp-growth_plot.png"
  
  # save the plot
  ggsave(path_plot_growth, width = 6, height = 3.708, dpi = 300)
  
  # add ier logo to the plot
  add_ier_logo(path_plot_growth)
  
  # message
  message("The GDP growth chart has been updated")
  
} else {
  
  message("The GDP growth chart is up to date")
  
}

# preview -----------------------------------------------------------------

if (growth_tidy_latest != growth_exp_csv_latest) {
  
  # plot
  ggplot(growth_tidy, aes(date, pct_change_yoy)) +
    geom_hline(yintercept = 0, color = "#E68F7E") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_line(color = "#2477B3", lwd = 1.5) +
    scale_y_continuous(
      breaks = seq(-12, 8, 4),
      limits = c(-12, 8),
      expand = c(0, 0),
      position = "right"
    ) +
    theme_void() +
    theme_ier_pre()
  
  # path to preview plot
  path_plot_growth_pre <- "fig/ier_gdp-growth_void_plot.png"
  
  # save the plot
  ggsave(path_plot_growth_pre, width = 13.3, height = 6.6, dpi = 300)
  
  # message
  message("The GDP growth preview chart has been updated")
  
} else {
  
  message("The GDP growth preview chart is up to date")
  
}
