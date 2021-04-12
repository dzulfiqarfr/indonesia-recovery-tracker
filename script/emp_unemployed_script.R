# indonesia economic recovery

# unemployed people

# author: dzulfiqar fathur rahman
# created: 2021-03-25
# last updated: 2021-04-12
# page: employment


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

if (exists("unemployment_req") == F) {
  
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
    pivot_longer(
      1:ncol(.),
      names_to = "key",
      values_to = "val"
    ) %>% 
    separate(
      key,
      into = c("key_act", "key_date"),
      sep = "5290"
    ) %>% 
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
    mutate(
      date = ymd(str_c(key_yr, key_mo)),
      mo = month(date)
    ) %>% 
    rename(yr = key_yr) %>% 
    select(key_act, date, yr, mo, val)
  
  # add activity labels
  unemployment_tidy <- unemployment_tidy %>% 
    mutate(act = str_replace_all(unemployment_tidy$key_act, deframe(unemployment_key_act))) %>% 
    select(key_act, act, date, yr, mo, val)
  
}

# subset number of unemployed people
unemployed_ppl <- unemployment_tidy %>% 
  dplyr::filter(key_act == 5, yr >= 2005) %>% 
  select(-c(1, 2, 4))

# calculate change in number of unemployed people, rename variable
unemp_ppl_trf <- unemployed_ppl %>% 
  group_by(mo) %>% 
  mutate(change_yoy = val - dplyr::lag(val, 1)) %>% 
  ungroup() %>% 
  rename(unemployed_ppl = val) %>% 
  select(-mo)

# divide by a million, round to two decimal places
unemp_ppl_trf[, 2:ncol(unemp_ppl_trf)] <- lapply(
  unemp_ppl_trf[, 2:ncol(unemp_ppl_trf)],
  function(x) round(x / 1000000, 2)
)


# plot --------------------------------------------------------------------

# plot
plot_unemp_ppl <- plot_ly(
  unemp_ppl_trf,
  x = ~date,
  y = ~unemployed_ppl,
  hovertemplate = "Unemployed workers: %{y} million<br>Date: %{x}<extra></extra>",
  line = list(color = "#1d81a2", width = 3),
  height = 300
) %>% 
  add_lines() %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      autorange = F,
      range = c(
        as.character(first(unemp_ppl_trf$date) - 180), 
        as.character(last(unemp_ppl_trf$date) + 180)
      ),
      fixedrange = T,
      tickmode = "auto",
      ticks = "outside",
      automargin = T,
      hoverformat = "%b '%y",
      showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = NA,
      type = "linear",
      autorange = F,
      range = c(0, 12.5),
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
        y0 = 0,
        y1 = 12,
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
        y = 11.75,
        yanchor = "top"
      ) 
    ),
    margin = list(l = 0, r = 0, t = 15, b = 0),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)


# export chart -------------------------------------------------------------

if (nrow(unemp_ppl_trf) != nrow(read_csv("data/ier_unemployed_cleaned.csv"))) {
  # plot
  ggplot(data = unemp_ppl_trf, aes(date, unemployed_ppl)) +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_line(color = "#1d81a2", lwd = 1) +
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
      size = 2.75,
      hjust = 1,
      color = "#90A4AE"
    ) +
    labs(
      title = "Unemployment",
      subtitle = "Number of unemployed workers (in millions)",
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
      "fig/ier_unemployed_plot.png",
      width = 7,
      height = 4,
      dpi = 300
    )
  
  # add logo
  ier_logo <- image_read("images/ier_hexsticker_small.png")
  
  # add base plot
  base_plot <- image_read("fig/ier_unemployed_plot.png")
  
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
    image_write("fig/ier_unemployed_plot.png")
  
  # message
  message("The number of unemployed workers chart has been updated")
  
} else {
  
  message("The number of unemployed workers chart is up to date")
  
}


# export data -------------------------------------------------------------

if (file.exists("data/ier_unemployed_cleaned.csv") == F) {
  
  write_csv(unemp_ppl_trf, "data/ier_unemployed_cleaned.csv")
  
  message("The number of unemployed workers dataset has been exported")
  
} else if (nrow(unemp_ppl_trf) != nrow(read_csv("data/ier_unemployed_cleaned.csv"))) {
  
  write_csv(unemp_ppl_trf, "data/ier_unemployed_cleaned.csv")
  
  message("The number of unemployed workers dataset has been updated")
  
} else {
  
  message("The number of unemployed workers dataset is up to date")
  
}