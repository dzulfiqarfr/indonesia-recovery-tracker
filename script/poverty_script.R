# indonesia economic recovery

# poverty rate

# author: dzulfiqar fathur rahman
# created: 2021-03-04
# last updated: 2021-07-14
# page: poverty


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
  pivot_longer(1:ncol(.), names_to = "key", values_to = "pov_rate") %>% 
  separate(key, into = c("key_area", "key_date"), sep = "1840") %>% 
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

# reshape to wide
pov_wide <- pov_trf %>% 
  select(-diff) %>% 
  pivot_wider(names_from = area, values_from = pov_rate)


# plot --------------------------------------------------------------------

# y-axis range
pov_rate_y_axis_range <- c(0, 17)

# plot
plot_pov_rate <- plot_ly(
  pov_wide,
  x = ~date,
  line = list(width = 2.5),
  height = 300
) %>% 
  add_lines(
    name = "Total",
    y = ~Total,
    hovertemplate = str_c(
      "<b>Total</b><br>",
      "Poverty rate: %{y}<br>Date: %{x}<extra></extra>"
    ),
    line = list(color = "#E66439")
  ) %>% 
  add_lines(
    name = "Urban",
    y = ~Urban,
    hovertemplate = str_c(
      "<b>Urban</b><br>",
      "Poverty rate: %{y}<br>Date: %{x}<extra></extra>"
    ),
    line = list(color = "#2477B3")
  ) %>% 
  add_lines(
    name = "Rural",
    y = ~Rural,
    hovertemplate = str_c(
      "<b>Rural</b><br>",
      "Poverty rate: %{y}<br>Date: %{x}<extra></extra>"
    ),
    line = list(color = "#36A3D9")
  ) %>% 
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
      hoverformat = "%B %Y",
      tickformat = "%Y",
      showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = NA,
      type = "linear",
      autorange = F,
      range = pov_rate_y_axis_range,
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
        y0 = pov_rate_y_axis_range[1],
        y1 = pov_rate_y_axis_range[2] - 1,
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
        y = pov_rate_y_axis_range[2] - 1,
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

# path to poverty rate data
path_data_pov_rate <- "data/ier_poverty-rate_cleaned.csv"

# export chart
if (nrow(pov_trf_tidy) != nrow(read_csv(path_data_pov_rate))) {
  
  # import functions to apply custom ggplot2 theme and add logo
  source("script/ggplot2_theme.R")
  
  # plot
  ggplot(pov_trf, aes(date, pov_rate, color = area)) +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      lty = "dashed"
    ) +
    geom_line(lwd = 0.75) +
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
    scale_color_manual(
      values = c(
        "Total" = "#E66439",
        "Urban" = "#2477B3",
        "Rural" = "#36A3D9"
      )  
    ) +
    annotate(
      "text",
      x = ymd("2020-02-01"),
      y = 14.5,
      label = "COVID-19\npandemic",
      size = 3,
      hjust = 1,
      color = "#90A4AE"
    ) +
    labs(
      title = "Poverty rate",
      subtitle = "(percent)",
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Statistics Indonesia (BPS)"
    ) +
    theme_ier() +
    theme(
      legend.text = element_text(size = rel(0.8)),
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.direction = "horizontal",
      legend.position = c(0.185, 1.075),
      legend.background = element_blank(),
      plot.subtitle = element_text(margin = margin(b = 37.5))
    )
  
  # path to the plot
  path_plot_pov_rate <- "fig/ier_poverty-rate_plot.png"
  
  # save the plot
  ggsave(path_plot_pov_rate, width = 6, height = 3.708, dpi = 300)
  
  # add ier logo to the plot
  add_ier_logo(path_plot_pov_rate)
  
  # message
  message("The poverty rate by area chart has been updated")
  
} else{
  
  message("the poverty rate by area chart is up to date")
  
}


# preview -----------------------------------------------------------------

if (nrow(pov_trf_tidy) != nrow(read_csv(path_data_pov_rate))) {
  
  # plot
  ggplot(pov_trf, aes(date, pov_rate, color = area)) +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      lty = "dashed"
    ) +
    geom_line(lwd = 1.5, show.legend = F) +
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
    scale_color_manual(
      values = c(
        "Total" = "#E66439",
        "Urban" = "#2477B3",
        "Rural" = "#36A3D9"
      )  
    ) +
    theme_void() +
    theme_ier_pre()
  
  # path to preview plot
  path_plot_pov_pre <- "fig/ier_poverty-rate_void_plot.png"
  
  # save the plot
  ggsave(path_plot_pov_pre, width = 13.3, height = 6.6, dpi = 300)
  
  # message
  message("The poverty preview chart has been updated")
  
} else {
  
  message("The poverty preview chart is up to dtae")
  
}


# export data -------------------------------------------------------------

if (!file.exists(path_data_pov_rate)) {
  
  write_csv(pov_trf_tidy, path_data_pov_rate)
  
  message("The poverty rate by area dataset has been exported")
  
} else if (nrow(pov_trf_tidy) != nrow(read_csv(path_data_pov_rate))) {
  
  write_csv(pov_trf_tidy, path_data_pov_rate)
  
  message("The poverty rate by area dataset has been updated")
  
} else {
  
  message("The poverty rate by area dataset is up to date")
  
}