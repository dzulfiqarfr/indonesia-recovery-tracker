# indonesia economic recovery

# poverty rate
# total and by area

# author: dzulfiqar fathur rahman
# created: 2021-03-04
# last updated: 2021-04-12
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
pov_req <- GET(
  base_url,
  query = list(
    model = "data",
    domain = "0000",
    var = "184",
    key = BPS_KEY
  )
)

# parse response
pov_parsed <- content(pov_req, "text") %>% 
  fromJSON()

# extract year key
pov_key_yr <- as_tibble(pov_parsed$tahun)

# extract data
pov_raw <- as_tibble(pov_parsed$datacontent)

# tidy data
pov_tidy <- pov_raw %>% 
  pivot_longer(
    1:ncol(.),
    names_to = "key",
    values_to = "pov_rate"
  ) %>% 
  separate(
    key,
    into = c("key_area", "key_date"),
    sep = "1840"
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
pov_tidy_sub <- pov_tidy %>% 
  dplyr::filter(key_yr > 110) %>% 
  arrange(key_date, key_area)

# replace year key
pov_tidy_sub$key_yr <- pov_tidy_sub$key_yr %>% 
  str_replace_all(deframe(pov_key_yr))

# replace month key
pov_tidy_sub$key_mo <- pov_tidy_sub$key_mo %>% 
  str_replace_all(c("^61$" = "-03-01", "^62$" = "-09-01"))

# create date variable
pov_tidy_sub <- pov_tidy_sub %>% 
  mutate(date = ymd(str_c(key_yr, key_mo))) %>% 
  select(key_area, date, pov_rate)

# replace area key
pov_tidy_sub$key_area <- pov_tidy_sub$key_area %>% 
  str_replace_all(c("1" = "Urban", "2" = "Rural", "3" = "Total")) %>% 
  as_factor()

# calculate annual changes in percentage points
pov_trf <- pov_tidy_sub %>% 
  rename(area = 1) %>% 
  group_by(area) %>% 
  mutate(diff = pov_rate - dplyr::lag(pov_rate, 2)) %>% 
  ungroup()


# plot --------------------------------------------------------------------

# rate --------------------------------------------------------------------

# plot
plot_pov_rate <- plot_ly(
  pov_trf,
  x = ~date,
  y = ~pov_rate,
  color = ~factor(area),
  colors = c("#09bb9f", "#1d81a2", "#90CAF9"),
  hovertemplate = "Poverty rate: %{y} percent<br>Date: %{x}<extra></extra>",
  line = list(width = 3),
  height = 300
) %>% 
  add_lines() %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      range = c(
        as.character(first(pov_trf$date) - 180), 
        as.character(last(pov_trf$date) + 180)
      ),
      fixedrange = T,
      ticks = "outside",
      automargin = T,
      hoverformat = "%b %Y",
      tickformat = "%Y",
      showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = NA,
      type = "linear",
      autorange = F,
      range = c(0, 17),
      fixedrange = T,
      dtick = 4,
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
        y1 = 16,
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
        y = 16,
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
plot_trf_sub <- pov_trf %>% 
  dplyr::filter(!is.na(diff)) 

# plot
plot_pov_diff <- plot_ly(
  plot_trf_sub,
  x = ~date,
  y = ~diff,
  color = ~factor(area),
  colors = c("#09bb9f", "#1d81a2", "#90CAF9"),
  hovertemplate = "Change in poverty rate: %{y} percentage points<br>Date: %{x}<extra></extra>",
  line = list(width = 3),
  height = 300
) %>% 
  add_lines() %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      range = c(
        as.character(first(plot_trf_sub$date) - 180), 
        as.character(last(plot_trf_sub$date) + 180)
      ),
      fixedrange = T,
      dtick = "M24",
      ticks = "outside",
      automargin = T,
      hoverformat = "%b %Y",
      tickformat = "%Y",
      showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = NA,
      type = "linear",
      autorange = F,
      range = c(-2, 2.1),
      fixedrange = T,
      dtick = 1,
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
        y0 = -2,
        y1 = 2,
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
        y = 1.9,
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

# latest data
pov_trf_tidy <- pov_trf %>% 
  rename(ppt_change_yoy = diff)

# export chart
if (nrow(pov_trf_tidy) != nrow(read_csv("data/ier_poverty-rate_cleaned.csv"))) {
  
  # plot
  ggplot(pov_trf, aes(date, pov_rate, color = area)) +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_line(lwd = 1) +
    scale_x_date(
      breaks = seq(ymd("2012-03-01"), last(pov_trf$date), "2 year"),
      date_labels = "%b\n%Y"
    ) +
    scale_y_continuous(
      breaks = seq(0, 16, 4),
      limits = c(0, 16),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(values = c("#09bb9f", "#1d81a2", "#90CAF9")) +
    annotate(
      "text",
      x = ymd("2020-02-01"),
      y = 14.5,
      label = "COVID-19\npandemic",
      size = 2.75,
      hjust = 1,
      color = "#90A4AE"
    ) +
    labs(
      title = "Poverty",
      subtitle = "Poverty rate, by area (in percent)",
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
      "fig/ier_poverty-rate_plot.png",
      width = 7,
      height = 4,
      dpi = 300
    )
  
  # add logo
  ier_logo <- image_read("images/ier_hexsticker_small.png")
  
  # add base plot
  base_plot <- image_read("fig/ier_poverty-rate_plot.png")
  
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
    image_write("fig/ier_poverty-rate_plot.png")
  
  # message
  message("The poverty rate by area chart has been updated")
  
} else{
  
  message("the poverty rate by area chart is up to date")
  
}


# preview -----------------------------------------------------------------

if (nrow(pov_trf_tidy) != nrow(read_csv("data/ier_poverty-rate_cleaned.csv"))) {
  
  # plot
  ggplot(pov_trf, aes(date, pov_rate, color = area)) +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_line(lwd = 2, show.legend = F) +
    scale_x_date(
      breaks = seq(ymd("2012-03-01"), last(pov_trf$date), "2 year"),
      date_labels = "%b\n%Y"
    ) +
    scale_y_continuous(
      breaks = seq(0, 16, 4),
      limits = c(0, 16),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(values = c("#09bb9f", "#1d81a2", "#90CAF9")) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#263238", color = NA),
      plot.margin = margin(t = 50, r = 50, b = 50, l = 50)
    ) +
    ggsave(
      "fig/ier_poverty-rate_void_plot.png",
      width = 13.3,
      height = 6.6,
      dpi = 300
    )
  
  # message
  message("The poverty preview chart has been updated")
  
} else {
  
  message("The poverty preview chart is up to dtae")
  
}


# export data -------------------------------------------------------------

# write csv
if (file.exists("data/ier_poverty-rate_cleaned.csv") == F) {
  
  write_csv(pov_trf_tidy, "data/ier_poverty-rate_cleaned.csv")
  
  message("The poverty rate by area dataset has been exported")
  
} else if (nrow(pov_trf_tidy) != nrow(read_csv("data/ier_poverty-rate_cleaned.csv"))) {
  
  write_csv(pov_trf_tidy, "data/ier_poverty-rate_cleaned.csv")
  
  message("The poverty rate by area dataset has been updated")
  
} else {
  
  message("The poverty rate by area dataset is up to date")
  
}