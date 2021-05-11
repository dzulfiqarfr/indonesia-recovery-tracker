# indonesia economic recovery

# retail sales

# author: dzulfiqar fathur rahman
# created: 2021-02-25
# last updated: 2021-05-11
# page: retail sales


# setup -------------------------------------------------------------------

# packages
library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)
library(ggtext)
library(magick)

# date of most recent observation
if(exists("rsi_last_date") == F) {
  rsi_last_date <- "2021-04-01"
}


# data --------------------------------------------------------------------

# import
rsi_raw <- read_excel(
  "data/bi_rsi_raw.xlsx", 
  sheet = "Tabel 1",
  skip = 3
) 

# rename indicator variable
names(rsi_raw)[1] <- "Indices"

# remove columns, rows containing missing values
rsi_raw <- rsi_raw %>% 
  dplyr::filter(!is.na(Indices), Indices == "INDEKS TOTAL") %>% 
  select_if(function(x) {!all(is.na(x))}) %>% 
  select(-last(names(rsi_raw)))

# rename date columns
## dates
rsi_date_seq <- seq(ymd("2012-01-01"), ymd(rsi_last_date), by = "month")

## correct sorting
rsi_date_seq <- rsi_date_seq %>%
  tibble() %>% 
  mutate(
    yr = date(rsi_date_seq), 
    mo = month(rsi_date_seq)
  ) %>% 
  arrange(yr, mo) %>% 
  rename(date = 1)

## replace column names
names(rsi_raw)[2:ncol(rsi_raw)] <- as.character(rsi_date_seq$date)

# rename indices
rsi_raw$Indices[1] <- "rsi"

# tidy data
rsi_tidy <- rsi_raw %>% 
  pivot_longer(
    2:ncol(.),
    names_to = "date", 
    values_to = "rsi"
  ) %>% 
  select(!Indices)

# correct data types
rsi_tidy$date <- ymd(rsi_tidy$date)

rsi_tidy$rsi <- as.numeric(rsi_tidy$rsi)

# round to two decimal places
rsi_tidy$rsi <- round(rsi_tidy$rsi, 2)

# calculate annual change
rsi_trf <- rsi_tidy %>% 
  mutate(mo = month(date)) %>% 
  group_by(mo) %>% 
  mutate(
    diff_yoy = rsi - dplyr::lag(rsi, 1),
    pct_change_yoy = round(diff_yoy / dplyr::lag(rsi, 1) * 100, 2)
  ) %>%
  ungroup() %>% 
  select(-mo) %>% 
  dplyr::filter(!is.na(pct_change_yoy))

# subset data for seasonal plot
rsi_trf_sub <- rsi_trf %>% 
  mutate(
    mo = month(date), 
    yr = year(date)
  ) %>% 
  dplyr::filter(yr >= 2016) %>% 
  select(date, mo, yr, rsi)


# plot --------------------------------------------------------------------

# rsi change --------------------------------------------------------------

# plot
plot_rsi_change <- plot_ly(
  rsi_trf,
  height = 300
) %>%
  add_lines(
    x = ~date, 
    y = ~pct_change_yoy,
    hovertemplate = "Growth: %{y} percent<br>Date: %{x}<extra></extra>",
    line = list(
      color = "#1d81a2",
      width = 3
    )
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      autorange = F,
      range = c(
        as.character(first(rsi_trf$date) - 180), 
        as.character(last(rsi_trf$date) + 180)
      ),
      fixedrange = T,
      tickmode = "auto",
      nticks = 6,
      dtick = "M12",
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
      range = c(-45, 31),
      fixedrange = T,
      dtick = 15,
      showline = F,
      linewidth = 0,
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
        y0 = -45,
        y1 = 30,
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
        y = 29,
        yanchor = "top"
      )
    ),
    showlegend = F,
    margin = list(l = 0, r = 0, t = 15, b = 0),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)


# index -------------------------------------------------------------------

# annotations
# 2016-2019
anno_1619 <- list(
  text = "<b>2016-2019</b>",
  font = list(size = 12, color = "#90A4AE"),
  bgcolor = "white",
  showarrow = F,
  xref = "x",
  x = 9,
  xanchor = "left",
  yref = "y",
  y = 230,
  yanchor = "top"
) 

# 2020
anno_2020 <- list(
  text = "<b>2020</b>",
  font = list(size = 12, color = "#1d81a2"),
  bgcolor = "white",
  showarrow = F,
  xref = "x",
  x = 10,
  xanchor = "left",
  yref = "y",
  y = 177.5,
  yanchor = "top"
) 

# 2021
anno_2021 <- list(
  text = "<b>2021</b>",
  font = list(size = 12, color = "#ff725b"),
  bgcolor = "white",
  showarrow = F,
  xref = "x",
  x = 1.5,
  xanchor = "left",
  yref = "y",
  y = 175,
  yanchor = "top"
) 

# plot
plot_rsi_index <- plot_ly(
  rsi_trf_sub,
  type = "scatter",
  mode = "markers+lines",
  line = list(width = 3),
  colors = c("#CFD8DC", "#CFD8DC", "#CFD8DC", "#CFD8DC", "#1d81a2", "#ff725b"),
  text = ~format(date, "%b %Y"),
  hovertemplate = "Index: %{y}<br>Date: %{text}<extra></extra>",
  height = 300
) %>% 
  add_trace(
    x = ~mo,
    y = ~rsi,
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
      range = c(150, 251),
      fixedrange = T,
      dtick = 25,
      showline = F,
      showgrid = T,
      gridcolor = "#CFD8DC",
      side = "right"
    ),
    annotations = list(anno_1619, anno_2020, anno_2021),
    showlegend = F,
    margin = list(l = 0, r = 0, t = 15, b = 0),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)


# export chart ------------------------------------------------------------

# latest observation in most recent csv
rsi_csv <- rsi_trf %>% 
  rename(retail_sales_index = 2) %>% 
  select(-diff_yoy)

# export chart
if (nrow(rsi_csv) != nrow(read_csv("data/ier_rsi-overall_cleaned.csv"))) {
  
  # rsi change ----
  
  # plot
  ggplot(rsi_trf, aes(date, pct_change_yoy)) +
    geom_hline(yintercept = 0, color = "#ff856c") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_line(color = "#1d81a2", lwd = 1) +
    scale_y_continuous(
      breaks = seq(-45, 30, 15),
      limits = c(-45, 30),
      expand = c(0, 0),
      position = "right"
    ) +
    annotate(
      "text",
      x = ymd("2020-04-01"),
      y = 22.5,
      label = "COVID-19\npandemic \u2192",
      size = 2.75,
      hjust = 0,
      color = "#90A4AE"
    ) +
    labs(
      title = "Retail sales index",
      subtitle = "(percent change from a year earlier)",
      caption = str_c(
        str_c(format(last(rsi_trf$date), "%B %Y"), " figure is an estimate\n\n"),
        "Chart: Dzulfiqar Fathur Rahman | Source: Bank Indonesia"
      )
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
      "fig/ier_rsi-change_plot.png",
      width = 7,
      height = 4,
      dpi = 300
    )
  
  # add logo
  ier_logo <- image_read("images/ier_hexsticker_small.png")
  
  # add base plot
  plot_rsi_change_png <- image_read("fig/ier_rsi-change_plot.png")
  
  # get plot height
  plot_height <- magick::image_info(plot_rsi_change_png)$height
  
  # get plot width
  plot_width <- magick::image_info(plot_rsi_change_png)$width
  
  # get logo height
  logo_width <- magick::image_info(ier_logo)$width
  
  # get logo width
  logo_height <- magick::image_info(ier_logo)$height
  
  # position for the bottom 1.5 percent
  pos_bottom <- plot_height - logo_height - plot_height * 0.015
  
  # position for the right 1.5 percent
  pos_right <- plot_width - logo_width - 0.015 * plot_width
  
  # overwrite plot
  plot_rsi_change_png %>% 
    image_composite(ier_logo, offset = str_c("+", pos_right, "+", pos_bottom)) %>% 
    image_write("fig/ier_rsi-change_plot.png")
  
  
  # index ----
  
  # annotations
  anno_year <- tibble(
    x = c(0.5, 11, 9),
    y = c(175, 175, 225),
    label = c("2021", "2020", "2016-2019")
  )
  
  # plot
  ggplot(rsi_trf_sub, aes(mo, rsi)) +
    geom_line(aes(color = as_factor(yr)), lwd = 1, show.legend = F) +
    geom_point(aes(color = as_factor(yr)), size = 1.5, show.legend = F) +
    scale_x_continuous(
      breaks = seq(2, 12, 2),
      labels = c("Feb", "April", "June", "Aug", "Oct", "Dec")
    ) +
    scale_y_continuous(
      breaks = seq(150, 250, 25),
      limits = c(150, 250),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(values = c("#CFD8DC", "#CFD8DC", "#CFD8DC", "#CFD8DC", "#1d81a2", "#ff725b")) +
    geom_richtext(
      data = anno_year,
      aes(x, y, label = label),
      fill = "white",
      label.color = NA,
      text.color = c("#ff725b", "#1d81a2", "#90A4AE"),
      hjust = 0,
      size = 3,
      fontface = "bold"
    ) +
    labs(
      title = "Retail sales index",
      caption = str_c(
        str_c(format(last(rsi_trf$date), "%B %Y"), " figure is an estimate\n\n"),
        "Chart: Dzulfiqar Fathur Rahman | Source: Bank Indonesia"
      )
    ) +
    theme(
      text = element_text(size = 12),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(color = "black"),
      legend.title = element_blank(),
      legend.key = element_rect(fill = "transparent"),
      legend.position = c(0.0925, 1.175),
      legend.direction = "horizontal",
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#CFD8DC"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(face = "bold", margin = margin(b = 35)),
      plot.caption = element_text(
        color = "#757575",
        hjust = 0,
        margin = margin(t = 35)
      )
    ) +
    ggsave(
      "fig/ier_rsi_plot.png",
      width = 7,
      height = 4,
      dpi = 300
    )
  
  # add logo
  ier_logo <- image_read("images/ier_hexsticker_small.png")
  
  # add base plot
  plot_rsi_index_png <- image_read("fig/ier_rsi_plot.png")
  
  # get plot height
  plot_height <- magick::image_info(plot_rsi_index_png)$height
  
  # get plot width
  plot_width <- magick::image_info(plot_rsi_index_png)$width
  
  # get logo height
  logo_width <- magick::image_info(ier_logo)$width
  
  # get logo width
  logo_height <- magick::image_info(ier_logo)$height
  
  # position for the bottom 1.5 percent
  pos_bottom <- plot_height - logo_height - plot_height * 0.015
  
  # position for the right 1.5 percent
  pos_right <- plot_width - logo_width - 0.015 * plot_width
  
  # overwrite plot
  plot_rsi_index_png %>% 
    image_composite(ier_logo, offset = str_c("+", pos_right, "+", pos_bottom)) %>% 
    image_write("fig/ier_rsi_plot.png")
  
  # message
  message("The Retail Sales Index charts have been updated")
  
} else {
  
  message("The Retail Sales Index charts are up to date")
  
}


# preview -----------------------------------------------------------------

if (nrow(rsi_csv) != nrow(read_csv("data/ier_rsi-overall_cleaned.csv"))) {
  
  # plot
  ggplot(rsi_trf_sub, aes(mo, rsi)) +
    geom_line(aes(color = as_factor(yr)), lwd = 2, show.legend = F) +
    geom_point(aes(color = as_factor(yr)), size = 3.5, show.legend = F) +
    scale_x_continuous(
      breaks = seq(2, 12, 2),
      labels = c("Feb", "Apr", "Jun", "Aug", "Oct", "Dec")
    ) +
    scale_y_continuous(
      breaks = seq(150, 250, 25),
      limits = c(150, 250),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(values = c("#CFD8DC", "#CFD8DC", "#CFD8DC", "#CFD8DC", "#1d81a2", "#ff725b")) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#263238", color = NA),
      plot.margin = margin(t = 50, r = 50, b = 50, l = 50)
    ) +
    ggsave(
      "fig/ier_rsi_void_plot.png",
      width = 13.3,
      height = 6.6,
      dpi = 300
    )
  
  #message
  message("The Retail Sales Index preview chart has been updated")
  
} else {
  
  message("The Retail Sales Index preview chart is up to date")
  
}


# export data -------------------------------------------------------------

# write csv
if (file.exists("data/ier_rsi-overall_cleaned.csv") == F) {
  
  write_csv(rsi_csv, "data/ier_rsi-overall_cleaned.csv")
  
  message("The Retail Sales Index dataset has been exported")
  
} else if (nrow(rsi_csv) != nrow(read_csv("data/ier_rsi-overall_cleaned.csv"))) {
  
  write_csv(rsi_csv, "data/ier_rsi-overall_cleaned.csv")
  
  message("The Retail Sales Index dataset has been updated")
  
} else {
  
  message("The Retail Sales Index dataset is up to date")
  
}