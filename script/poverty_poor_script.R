# indonesia economic recovery

# poor poor
# total and by area

# author: dzulfiqar fathur rahman
# created: 2021-03-05
# last updated: 2021-04-08
# page: poverty


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

# request data
poor_req <- GET(
  base_url,
  query = list(
    model = "data",
    domain = "0000",
    var = "183",
    key = BPS_KEY
  )
)

# parse response
poor_parsed <- content(poor_req, "text") %>% 
  fromJSON()

# extract year key
poor_key_yr <- as_tibble(poor_parsed$tahun)

# data
poor_raw <- as_tibble(poor_parsed$datacontent)

# tidy data
poor_tidy <- poor_raw %>% 
  pivot_longer(
    1:ncol(.),
    names_to = "key",
    values_to = "poor"
  ) %>% 
  separate(
    key,
    into = c("key_area", "key_date"),
    sep = "1830"
  ) %>% 
  mutate(
    key_yr = as.numeric(
      case_when(
        str_detect(key_date, "^9") ~ str_sub(key_date, 1, 2),
        str_detect(key_date, "^1") ~ str_sub(key_date, 1, 3)
      )
    ),
    key_mo = as.numeric(
      case_when(
        str_detect(key_date, "^9") ~ str_sub(key_date, 3, 4),
        str_detect(key_date, "^1") ~ str_sub(key_date, 4, 5)
      )
    )
  )

# subset 2010s observations, arrange by date, area key
poor_tidy_sub <- poor_tidy %>% 
  dplyr::filter(key_yr > 110) %>% 
  arrange(key_date, key_area)

# replace year key
poor_tidy_sub$key_yr <- poor_tidy_sub$key_yr %>% 
  str_replace_all(deframe(poor_key_yr))

# replace month key
poor_tidy_sub$key_mo <- poor_tidy_sub$key_mo %>% 
  str_replace_all(c("^61$" = "-03-01", "^62$" = "-09-01"))

# create date variable
poor_tidy_sub <- poor_tidy_sub %>% 
  mutate(date = ymd(str_c(key_yr, key_mo))) %>% 
  select(key_area, date, poor)

# replace area key
poor_tidy_sub$key_area <- poor_tidy_sub$key_area %>% 
  str_replace_all(c("1" = "Urban", "2" = "Rural", "3" = "Total")) %>% 
  as_factor()

# calculate annual changes in percentage points
poor_trf <- poor_tidy_sub %>% 
  rename(area = 1) %>% 
  group_by(area) %>% 
  mutate(diff = poor - dplyr::lag(poor, 2))


# export data -------------------------------------------------------------

# data
poor_csv <- poor_trf %>% 
  rename(number_of_poor_ppl = 3, change_yoy = 4)

# write csv
if (file.exists("data/ier_poor_cleaned.csv") == F) {
  
  write_csv(poor_csv, "data/ier_poor_cleaned.csv")
  
  message("The number of poor poor, broken down by area, dataset has been exported")
  
} else if (nrow(poor_csv) != nrow(read_csv("data/ier_poor_cleaned.csv"))) {
  
  write_csv(poor_csv, "data/ier_poor_cleaned.csv")
  
  message("The number of poor poor, broken down by area, dataset has been updated")
  
} else {
  
  message("The number of poor poor, broken down by area, dataset is up to date")
  
}


# plot --------------------------------------------------------------------

# total -------------------------------------------------------------------

# plot
plot_poor_total <- plot_ly(
  poor_trf,
  x = ~date,
  y = ~poor,
  color = ~factor(area),
  colors = c("#09bb9f", "#1d81a2", "#90CAF9"),
  hovertemplate = "Number of poor people: %{y} million<br>Date: %{x}<extra></extra>",
  line = list(width = 3),
  height = 300
) %>% 
  add_lines() %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      range = c(
        as.character(first(poor_trf$date) - 180), 
        as.character(last(poor_trf$date) + 180)
      ),
      fixedrange = T,
      ticks = "outside",
      automargin = T,
      tickformat = "%Y",
      hoverformat = "%b %Y",
      showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = NA,
      type = "linear",
      autorange = F,
      range = c(0, 33),
      fixedrange = T,
      dtick = 8,
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
        y1 = 32,
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
        y = 32,
        yanchor = "top"
      ) 
    ),
    legend = list(
      orientation = "h",
      x = 0,
      xanchor = "left",
      y = 1.175,
      yanchor = "top"
    ),
    margin = list(l = 0, r = 0, t = 15, b = 0)
  ) %>% 
  plotly::config(displayModeBar = F)


# change ------------------------------------------------------------------

# remove missing values
poor_trf_sub <- poor_trf %>% 
  dplyr::filter(!is.na(diff)) 

# change in poor poor plot
plot_poor_diff <- plot_ly(
  poor_trf_sub,
  x = ~date,
  y = ~diff,
  color = ~factor(area),
  colors = c("#09bb9f", "#1d81a2", "#90CAF9"),
  hovertemplate = "Change in the number of poor people: %{y} million<br>Date: %{x}<extra></extra>",
  line = list(width = 3),
  height = 300
) %>% 
  add_lines() %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      range = c(
        as.character(first(poor_trf_sub$date) - 180), 
        as.character(last(poor_trf_sub$date) + 180)
      ),
      fixedrange = T,
      dtick = "M24",
      ticks = "outside",
      automargin = T,
      tickformat = "%Y",
      hoverformat = "%b %Y",
      showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = NA,
      type = "linear",
      autorange = F,
      range = c(-4.5, 3.1),
      fixedrange = T,
      dtick = 1.5,
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
        y0 = -4.5,
        y1 = 3,
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
        y = 3,
        yanchor = "top"
      ) 
    ),
    legend = list(
      orientation = "h",
      x = 0,
      xanchor = "left",
      y = 1.175,
      yanchor = "top"
    ),
    margin = list(l = 0, r = 0, t = 15, b = 0)
  ) %>% 
  plotly::config(displayModeBar = F)


# export chart ------------------------------------------------------------

if (nrow(poor_csv) != nrow(read_csv("data/ier_poor_cleaned.csv"))) {
  
  # plot
  ggplot(poor_trf, aes(date, poor, color = area)) +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_line(lwd = 1) +
    scale_x_date(
      breaks = seq(ymd("2012-03-01"), last(poor_trf$date), "2 year"),
      date_labels = "%b\n%Y"
    ) +
    scale_y_continuous(
      breaks = seq(0, 32, 8),
      limits = c(0, 32),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(values = c("#09bb9f", "#1d81a2", "#90CAF9")) +
    annotate(
      "text",
      x = ymd("2020-02-01"),
      y = 29,
      label = "COVID-19\npandemic",
      size = 2.75,
      hjust = 1,
      color = "#90A4AE"
    ) +
    labs(
      title = "Poverty",
      subtitle = "Number of poor people, by area (in millions)",
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Statistics Indonesia (BPS)"
    ) +
    theme(
      text = element_text(size = 12),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(color = "black"),
      legend.title = element_blank(),
      legend.key = element_rect(fill = "transparent"),
      legend.position = c(0.15, 1.175),
      legend.direction = "horizontal",
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#CFD8DC"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.spacing.x = unit(2, "lines"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 55)),
      plot.caption = element_text(
        color = "#757575",
        hjust = 0,
        margin = margin(t = 35)
      )
    ) +
    ggsave(
      "fig/ier_poverty-poor_plot.png",
      width = 7,
      height = 4,
      dpi = 300
    )
  
  # add logo
  ier_logo <- image_read("images/ier_hexsticker_small.png")
  
  # add base plot
  base_plot <- image_read("fig/ier_poverty-poor_plot.png")
  
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
    image_write("fig/ier_poverty-poor_plot.png")
  
  # message
  message("The number of poor by area chart has been updated")
  
} else {
  
  message("The number of poor by area is up to date")
  
}