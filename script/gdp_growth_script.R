# indonesia economic recovery

# gdp
# percent change on a year earlier

# author: dzulfiqar fathur rahman
# created: 2021-02-21
# last updated: 2021-04-09
# page: gdp


# setup -------------------------------------------------------------------

# packages
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(plotly)
library(magick)

# api key
if (exists("BPS_KEY") == F) {
  BPS_KEY <- Sys.getenv("BPS_KEY")
}

# bps api url
if (exists("base_url") == F) {
  base_url <- "https://webapi.bps.go.id/v1/api/list"
}

# data --------------------------------------------------------------------

if (exists("growth_raw") == F) {
  
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
  pivot_longer(
    1:ncol(.),
    names_to = "key",
    values_to = "pct_change_yoy"
  ) %>% 
  separate(
    key,
    into = c("key_exp", "key_period"),
    sep = "108"
  ) %>%
  mutate(
    key_period_obs = str_sub(key_period, end = 1),
    key_yr = str_sub(key_period, 2, 4),
    key_q = str_sub(key_period, 5, 6)
  ) %>% 
  dplyr::filter(
    key_exp == "800",
    key_period_obs == "5",
    key_q != "35"
  )

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
    line = list(color = "#1d81a2", width = 3),
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
      range = c(-12, 9),
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
        y0 = -12,
        y1 = 8,
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
        y = -8,
        yanchor = "top"
      )
    ),
    margin = list(l = 0, r = 0, t = 15, b = 0)
  ) %>% 
  plotly::config(displayModeBar = F)


# export chart ------------------------------------------------------------

# data
growth_exp_csv <- read_csv("data/ier_gdp-growth-exp_cleaned.csv")

# latest observation in csv
growth_exp_csv_latest <- growth_exp %>% 
  select(date) %>% 
  dplyr::filter(!duplicated(date), date == last(date)) %>% 
  deframe()

# latest observation in data
growth_tidy_latest <- growth_tidy %>% 
  select(date) %>% 
  dplyr::filter(date == last(date)) %>% 
  deframe()

# chart
if (growth_tidy_latest != growth_exp_csv_latest) {
  
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
    geom_hline(yintercept = 0, color = "#ff856c") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_line(color = "#1d81a2", lwd = 1) +
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
      size = 2.75,
      hjust = 1,
      color = "#90A4AE"
    ) +
    labs(
      title = "Economic growth",
      subtitle = "GDP (percent change from a year earlier)",
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
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 35)),
      plot.caption = element_text(
        color = "#757575",
        hjust = 0,
        margin = margin(t = 35)
      )
    ) +
    ggsave(
      "fig/ier_gdp-growth_plot.png",
      width = 7,
      height = 4,
      dpi = 300
    )
  
  # add logo
  ier_logo <- image_read("images/ier_hexsticker_small.png")
  
  # add base plot
  base_plot <- image_read("fig/ier_gdp-growth_plot.png")
  
  # get plot height
  plot_height <- magick::image_info(base_plot)$height
  
  # get plot width
  plot_width <- magick::image_info(base_plot)$width
  
  # get logo height
  logo_width <- magick::image_info(ier_logo)$width
  
  # get logo width
  logo_height <- magick::image_info(ier_logo)$height
  
  # position for the bottom 1.5 percent
  pos_bottom <- plot_height - logo_height - plot_height * 0.015
  
  # position for the right 1.5 percent
  pos_right <- plot_width - logo_width - 0.015 * plot_width
  
  # overwrite plot
  base_plot %>% 
    image_composite(ier_logo, offset = str_c("+", pos_right, "+", pos_bottom)) %>% 
    image_write("fig/ier_gdp-growth_plot.png")
  
  # message
  message("The GDP growth chart has been updated")
  
} else {
  
  message("The GDP growth chart is up to date")
  
}

# preview -----------------------------------------------------------------

if (growth_tidy_latest != growth_exp_csv_latest) {
  
  # plot
  ggplot(growth_tidy, aes(date, pct_change_yoy)) +
    geom_hline(yintercept = 0, color = "#ff856c") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_line(color = "#1d81a2", lwd = 2) +
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
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#263238", color = NA),
      plot.margin = margin(t = 50, r = 50, b = 50, l = 50)
    ) +
    ggsave(
      "fig/ier_gdp-growth_void_plot.png",
      width = 13.3,
      height = 6.6,
      dpi = 300
    )
  
  # message
  message("The GDP growth preview chart has been updated")
  
} else {
  
  message("The GDP growth preview chart is up to date")
  
}
