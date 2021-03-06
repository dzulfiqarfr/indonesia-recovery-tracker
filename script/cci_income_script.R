# indonesia economic recovery

# consumer confidence by income group

# author: dzulfiqar fathur rahman
# created: 2021-02-24
# last updated: 2021-07-13
# page: consumer confidence


# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)
library(ggtext)
library(magick)


# data --------------------------------------------------------------------

# date of most recent observation
if (!exists("cci_last_date")) {
  cci_last_date <- "2021-06-01"
}  

# import cci by income group data
cci_by_income_raw <- read_excel(
  "data/bi_cci_raw.xlsx",
  sheet = "Tabel 2",
  skip = 7,
  na = c("-", "")
)

# rename indicator variable
names(cci_by_income_raw)[4] <- "Indices" 

# remove columns, rows containing missing values
cci_by_income_raw <- cci_by_income_raw %>% 
  dplyr::filter(!is.na(Indices)) %>% 
  select_if(function(x) {!all(is.na(x))}) %>% 
  dplyr::select(-last(names(cci_by_income_raw)))

# subset cci
cci_by_income_raw <- cci_by_income_raw %>% 
  slice(1:5)

# rename date columns
## dates
cci_date_seq <- seq(ymd("2012-01-01"), ymd(cci_last_date), by = "month")

## correct sorting
cci_date_seq <- cci_date_seq %>%
  tibble() %>% 
  dplyr::mutate(yr = date(cci_date_seq), mo = month(cci_date_seq)) %>% 
  dplyr::arrange(yr, mo) %>% 
  dplyr::rename(date = 1)

## replace column names
names(cci_by_income_raw)[2:ncol(cci_by_income_raw)] <- as.character(cci_date_seq$date)

# rename income groups
cci_by_income_raw[, 1] <- c("1_2", "2.1_3", "3.1_4", "4.1_5", "above_5")

# tidy data
cci_by_income_tidy <- cci_by_income_raw %>% 
  pivot_longer(2:ncol(.), names_to = "date", values_to = "cci")

# correct data types
cci_by_income_tidy$Indices <- as.factor(cci_by_income_tidy$Indices)
cci_by_income_tidy$date <- ymd(cci_by_income_tidy$date)
cci_by_income_tidy$cci <- as.numeric(cci_by_income_tidy$cci)

# reshape to wide format for plot
cci_by_income_wide <- cci_by_income_tidy %>% 
  pivot_wider(names_from = Indices, values_from = cci) %>% 
  arrange(date)

# correct data types
cci_by_income_wide[, 2:ncol(cci_by_income_wide)] <- lapply(
  cci_by_income_wide[, 2:ncol(cci_by_income_wide)],
  as.numeric
)

# round cci to two decimal places
cci_by_income_wide[, 2:ncol(cci_by_income_wide)] <- lapply(
  cci_by_income_wide[, 2:ncol(cci_by_income_wide)],
  function(x) {round(x, 2)}
)

# rename columns for csv
cci_by_income_csv <- cci_by_income_tidy %>% 
  rename(income_group = 1, consumer_confidence_index = 3)


# plot --------------------------------------------------------------------

# y-axis range
cci_inc_y_axis_range <- c(40, 141)

# plot
plot_cci_by_income <- plot_ly(
  cci_by_income_wide,
  x = ~date,
  line = list(width = 2.5),
  height = 375
) %>% 
  add_lines(
    name = "Rp 1-2 million",
    y = ~`1_2`,
    hovertemplate = str_c(
      "<b>Income group: Rp 1-2 million</b><br><br>",
      "Consumer confidence: %{y}<br>Date: %{x}<extra></extra>"
    ),
    line = list(color = "#2477B3")
  ) %>%
  add_lines(
    name = "Rp 2.1-3 million",
    y = ~`2.1_3`,
    hovertemplate = str_c(
      "<b>Income group: Rp 2.1-3 million</b><br><br>",
      "Consumer confidence: %{y}<br>Date: %{x}<extra></extra>"
    ),
    line = list(color = "#36A3D9")
  ) %>% 
  add_lines(
    name = "Rp 3.1-4 million",
    y = ~`3.1_4`,
    hovertemplate = str_c(
      "<b>Income group: Rp 3.1-4 million</b><br><br>",
      "Consumer confidence: %{y}<br>Date: %{x}<extra></extra>"
    ),
    line = list(color = "#55CBF2")
  ) %>% 
  add_lines(
    name = "Rp 4.1-5 million",
    y = ~`4.1_5`,
    hovertemplate = str_c(
      "<b>Income group: Rp 4.1-5 million</b><br><br>",
      "Consumer confidence: %{y}<br>Date: %{x}<extra></extra>"
    ),
    line = list(color = "#E66439")
  ) %>% 
  add_lines(
    name = "Above Rp 5 million",
    y = ~above_5,
    hovertemplate = str_c(
      "<b>Income group: above Rp 5 million</b><br><br>",
      "Consumer confidence: %{y}<br>Date: %{x}<extra></extra>"
    ),
    line = list(color = "#F2AA61")
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      autorange = F,
      range = c(
        as.character(first(cci_by_income_wide$date) - 180), 
        as.character(last(cci_by_income_wide$date) + 180)
      ),
      fixedrange = T,
      tickmode = "auto",
      ticks = "outside",
      automargin = T,
      showline = T,
      showgrid = F,
      hoverformat = "%B %Y"
    ),
    yaxis = list(
      title = NA,
      autorange = F,
      range = cci_inc_y_axis_range,
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
        x0 = as.character(first(cci_by_income_wide$date) - 180),
        x1 = as.character(last(cci_by_income_wide$date) + 180),
        yref = "y",
        y0 = 100,
        y1 = 100,
        line = list(color = "#E68F7E", size = 1)
      ),
      list(
        type = "line",
        layer = "below",
        xref = "x",
        x0 = "2020-03-02",
        x1 = "2020-03-02",
        yref = "y",
        y0 = cci_inc_y_axis_range[1],
        y1 = cci_inc_y_axis_range[2] - 1,
        line = list(color = "#90A4AE", dash = "dash")
      )
    ),
    annotations = list(
      list(
        text = "More optimistic &#8593;",
        font = list(size = 12),
        align = "left",
        showarrow = F,
        xref = "x",
        x = "2013-11-01",
        xref = "y",
        y = 137.5
      ),
      list(
        text = "More pessimistic &#8595;",
        font = list(size = 12),
        align = "left",
        showarrow = F,
        xref = "x",
        x = "2013-11-01",
        yref = "y",
        y = 90
      ),
      list(
        text = "COVID-19<br>pandemic",
        font = list(size = 12, color = "#90A4AE"),
        align = "left",
        bgcolor = "white",
        showarrow = F,
        xref = "x",
        x = "2020-03-10",
        xanchor = "left",
        yref = "y",
        y = cci_inc_y_axis_range[2] - 1.5,
        yanchor = "top"
      )
    ),
    legend = list(
      orientation = "h",
      xanchor = "left",
      y = 1.25,
      yanchor = "top",
      valign = "top"
    ),
    margin = list(l = 0, r = 0, t = 15, b = 0),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)


# export chart ------------------------------------------------------------

# path to cci by income group data
path_data_cci_inc <- "data/ier_cci-income_cleaned.csv"

# export chart
if (nrow(cci_by_income_csv) != nrow(read_csv(path_data_cci_inc))) {
  
  # import functions to apply custom ggplot2 theme and add logo
  source("script/ggplot2_theme.R")
  
  # annotations: confidence territories
  anno_conf_ths <- tribble(
    ~x, ~y, ~label,
    ymd("2014-01-01"), 90, "More pessimistic \u2193",
    ymd("2016-01-01"), 130, "More optimistic \u2191"
  )
  
  # plot
  ggplot(cci_by_income_tidy, aes(date, cci)) +
    geom_hline(yintercept = 100, color = "#E68F7E") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      lty = "dashed"
    ) +
    geom_line(aes(color = Indices), lwd = 0.75) +
    scale_x_date(
      breaks = seq(ymd("2012-01-01"), last(cci_by_income_tidy$date), by = "1 year"),
      labels = c("2012", str_c("'", seq(13, 21)))
    ) +
    scale_y_continuous(
      breaks = seq(40, 140, 20),
      limits = c(40, 140),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(
      labels = c(
        "1_2" = "Rp 1-2 million", 
        "2.1_3" = "Rp 2.1-3 million", 
        "3.1_4" = "Rp 3.1-4 million", 
        "4.1_5" = "Rp 4.1-5 million", 
        "above_5" = "Above Rp 5 million"
      ),
      values = c(
        "1_2" = "#2477B3",
        "2.1_3" = "#36A3D9",
        "3.1_4" = "#55CBF2",
        "4.1_5" = "#E66439",
        "above_5" = "#F2AA61"
      )    
    ) +
    ggtext::geom_richtext(
      data = anno_conf_ths,
      aes(x = x, y = y, label = label),
      fill = "white",
      label.color = NA,
      size = 3
    ) +
    annotate(
      "text",
      x = ymd("2020-04-01"),
      y = 130,
      label = "COVID-19\npandemic \u2192",
      size = 3,
      hjust = 0,
      color = "#90A4AE"
    ) +
    labs(
      title = "Consumer confidence index",
      subtitle = "By income group",
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Bank Indonesia"
    ) +
    theme_ier() +
    theme(
      legend.text = element_text(size = rel(0.7)),
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(0.4, "cm"),
      legend.direction = "horizontal",
      legend.position = c(0.49, 1.075),
      legend.background = element_blank(),
      plot.subtitle = element_text(margin = margin(b = 37.5))
    )
  
  # path to the plot
  path_plot_cci_inc <- "fig/ier_cci-income_plot.png"
  
  # save the plot
  ggsave(path_plot_cci_inc, width = 6, height = 3.708, dpi = 300)
  
  # add ier logo to the plot
  add_ier_logo(path_plot_cci_inc)
  
  # message
  message("The Consumer Confidence Index by income group chart has been updated")
  
} else {
  
  message("The Consumer Confidence Index by income group chart is up to date")
  
}


# export preview chart ----------------------------------------------------

if (nrow(cci_by_income_csv) != nrow(read_csv(path_data_cci_inc))) {
  
  # plot
  ggplot(cci_by_income_tidy, aes(date, cci)) +
    geom_hline(yintercept = 100, color = "#E68F7E") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      lty = "dashed"
    ) +
    geom_line(aes(color = Indices), lwd = 1.5, show.legend = F) +
    scale_y_continuous(
      breaks = seq(40, 140, 20),
      limits = c(40, 140),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(
      labels = c(
        "1_2" = "Rp 1-2 million", 
        "2.1_3" = "Rp 2.1-3 million", 
        "3.1_4" = "Rp 3.1-4 million", 
        "4.1_5" = "Rp 4.1-5 million", 
        "above_5" = "Above Rp 5 million"
      ),
      values = c(
        "1_2" = "#2477B3",
        "2.1_3" = "#36A3D9",
        "3.1_4" = "#55CBF2",
        "4.1_5" = "#E66439",
        "above_5" = "#F2AA61"
      )    
    ) +
    theme_void() +
    theme_ier_pre()
  
  # path to preview plot
  path_plot_cci_pre <- "fig/ier_cci-income_void_plot.png"
  
  # save the plot
  ggsave(path_plot_cci_pre, width = 13.3, height = 6.6, dpi = 300)
  
  # message
  message("The Consumer Confidence Index by income group preview chart has been updated")
  
} else {
  
  message("The Consumer Confidence Index by income group preview chart is up to date")
  
}


# export data -------------------------------------------------------------

if (!file.exists(path_data_cci_inc)) {
  
  write_csv(cci_by_income_csv, path_data_cci_inc)
  
  message("The Consumer Confidence Index dataset, broken down by income group, has been exported")
  
} else if(nrow(cci_by_income_csv) != nrow(read_csv(path_data_cci_inc))) {
  
  write_csv(cci_by_income_csv, path_data_cci_inc)
  
  message("The Consumer Confidence Index dataset, broken down by income group, has been updated")
  
} else {
  
  message("The Consumer Confidence Index dataset, broken down by income group is up to date")
  
}