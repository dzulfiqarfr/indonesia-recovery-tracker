# indonesia economic recovery

# retail sales

# author: dzulfiqar fathur rahman
# created: 2021-02-25
# last updated: 2021-07-15
# page: retail sales


# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)
library(ggtext)
library(magick)


# data --------------------------------------------------------------------

# date of most recent observation
if(!exists("rsi_last_date")) {
  rsi_last_date <- "2021-06-01"
}

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
  mutate(yr = date(rsi_date_seq), mo = month(rsi_date_seq)) %>% 
  arrange(yr, mo) %>% 
  rename(date = 1)

## replace column names
names(rsi_raw)[2:ncol(rsi_raw)] <- as.character(rsi_date_seq$date)

# rename indices
rsi_raw$Indices[1] <- "rsi"

# tidy data
rsi_tidy <- rsi_raw %>% 
  pivot_longer(2:ncol(.), names_to = "date", values_to = "rsi") %>% 
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
  mutate(mo = month(date), yr = year(date)) %>% 
  dplyr::filter(yr >= 2016) %>% 
  select(date, mo, yr, rsi)


# plot --------------------------------------------------------------------

## rsi change ----

# y-axis range
rsi_change_y_axis_range <- c(-45, 31)

# plot
plot_rsi_change <- plot_ly(rsi_trf, height = 300) %>%
  add_lines(
    x = ~date, 
    y = ~pct_change_yoy,
    hovertemplate = "Change: %{y} percent<br>Date: %{x}<extra></extra>",
    line = list(color = "#2477B3", width = 3)
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
      hoverformat = "%B %Y",
      showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = NA,
      type = "linear",
      autorange = F,
      range = rsi_change_y_axis_range,
      fixedrange = T,
      dtick = 15,
      showline = F,
      linewidth = 0,
      showgrid = T,
      gridcolor = "#CFD8DC",
      zerolinecolor = "#E68F7E",
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
        y0 = rsi_change_y_axis_range[1],
        y1 = rsi_change_y_axis_range[2] - 1,
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
        y = rsi_change_y_axis_range[2] - 2,
        yanchor = "top"
      )
    ),
    showlegend = F,
    margin = list(l = 0, r = 0, t = 15, b = 0),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)


## index ----

# annotations
# 2016-2019
anno_1619 <- list(
  text = "<b>2016-2019</b>",
  font = list(size = 12, color = "lightgrey"),
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
  font = list(size = 12, color = "#36A3D9"),
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
  font = list(size = 12, color = "#2477B3"),
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
  colors = c(rep("lightgrey", 4), "#36A3D9", "#2477B3"),
  text = ~format(date, "%B %Y"),
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

# rename column for csv 
rsi_csv <- rsi_trf %>% 
  rename(retail_sales_index = 2) %>% 
  select(-diff_yoy)

# path to rsi data
path_data_rsi_ov <- "data/ier_rsi-overall_cleaned.csv"

# export chart
if (nrow(rsi_csv) != nrow(read_csv(path_data_rsi_ov))) {
  
  # import functions to apply custom ggplot2 theme and add logo
  source("script/ggplot2_theme.R")
  
  ## rsi change ----
  
  # plot
  ggplot(rsi_trf, aes(date, pct_change_yoy)) +
    geom_hline(yintercept = 0, color = "#E68F7E") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      lty = "dashed"
    ) +
    geom_line(color = "#2477B3", lwd = 1) +
    scale_x_date(
      breaks = seq(ymd("2013-01-01"), ymd("2021-01-01"), "1 year"),
      labels = c("2013", str_c("'", seq(14, 21)))
    ) +
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
      size = 3,
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
    theme_ier()
    
  # path to the plot
  path_plot_rsi_chg <- "fig/ier_rsi-change_plot.png"
    
  # save the plot
  ggsave(path_plot_rsi_chg, width = 6, height = 3.708, dpi = 300)
  
  # add ier logo to the plot
  add_ier_logo(path_plot_rsi_chg)
  
  
  ## index ----
  
  # annotations
  anno_year <- tibble(
    x = c(2.75, 11, 9),
    y = c(175, 175, 225),
    label = c("2021", "2020", "2016-2019")
  )
  
  # plot
  ggplot(rsi_trf_sub, aes(mo, rsi)) +
    geom_line(aes(color = as_factor(yr)), lwd = 1, show.legend = F) +
    geom_point(aes(color = as_factor(yr)), size = 1.5, show.legend = F) +
    scale_x_continuous(
      breaks = seq(1, 12),
      labels = format(seq(ymd("2021-01-01"), ymd("2021-12-01"), "1 month"), "%b")
    ) +
    scale_y_continuous(
      breaks = seq(150, 250, 25),
      limits = c(150, 250),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(values = c(rep("lightgrey", 4), "#36A3D9", "#2477B3")) +
    geom_richtext(
      data = anno_year,
      aes(x, y, label = label),
      fill = "white",
      label.color = NA,
      text.color = c("#2477B3", "#36A3D9", "lightgrey"),
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
    theme_ier() +
    theme(plot.title = element_text(margin = margin(b = 25)))
    
  # path to the plot
  path_plot_rsi_idx <- "fig/ier_rsi_plot.png"
  
  # save the plot
  ggsave(path_plot_rsi_idx, width = 6, height = 3.708, dpi = 300)
  
  # add ier logo to the plot
  add_ier_logo(path_plot_rsi_idx)
  
  # message
  message("The Retail Sales Index charts have been updated")
  
} else {
  
  message("The Retail Sales Index charts are up to date")
  
}



# export preview chart ----------------------------------------------------

if (nrow(rsi_csv) != nrow(read_csv(path_data_rsi_ov))) {
  
  # plot
  ggplot(rsi_trf_sub, aes(mo, rsi)) +
    geom_line(aes(color = as_factor(yr)), lwd = 1.5, show.legend = F) +
    geom_point(aes(color = as_factor(yr)), size = 2.5, show.legend = F) +
    scale_y_continuous(
      breaks = seq(150, 250, 25),
      limits = c(150, 250),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(values = c(rep("lightgrey", 4), "#36A3D9", "#2477B3")) +
    theme_void() +
    theme_ier_pre()
  
  # path to preview plot
  path_plot_rsi_pre <- "fig/ier_rsi_void_plot.png"
  
  # save the plot
  ggsave(path_plot_rsi_pre, width = 13.3, height = 6.6, dpi = 300)
  
  #message
  message("The Retail Sales Index preview chart has been updated")
  
} else {
  
  message("The Retail Sales Index preview chart is up to date")
  
}


# export data -------------------------------------------------------------

if (!file.exists(path_data_rsi_ov)) {
  
  write_csv(rsi_csv, path_data_rsi_ov)
  
  message("The Retail Sales Index dataset has been exported")
  
} else if (nrow(rsi_csv) != nrow(read_csv(path_data_rsi_ov))) {
  
  write_csv(rsi_csv, path_data_rsi_ov)
  
  message("The Retail Sales Index dataset has been updated")
  
} else {
  
  message("The Retail Sales Index dataset is up to date")
  
}