# indonesia economic recovery

# overall consumer confidence

# author: dzulfiqar fathur rahman
# created: 2021-02-24
# last updated: 2021-07-13
# page: consumer confidence


# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)
library(magick)


# data --------------------------------------------------------------------

# date of most recent observation
if (!exists("cci_last_date")) {
  cci_last_date <- "2021-06-01"
}  

# import cci data
cci_raw <- read_excel(
  "data/bi_cci_raw.xlsx",
  sheet = "Tabel 1",
  skip = 5,
  na = c("-", "")
)

# rename indicator variable
names(cci_raw)[4] <- "Indices" 

# remove columns, rows containing missing values
cci_raw <- cci_raw %>% 
  dplyr::filter(!is.na(Indices)) %>% 
  select_if(function(x) {!all(is.na(x))}) %>% 
  dplyr::select(-last(names(cci_raw)))

# rename date columns
## dates
cci_date_seq <- seq(ymd("2012-01-01"), ymd(cci_last_date), by = "month")

## correct sorting
cci_date_seq <- cci_date_seq %>%
  tibble() %>% 
  mutate(yr = date(cci_date_seq), mo = month(cci_date_seq)) %>% 
  arrange(yr, mo) %>% 
  rename(date = 1)

## replace column names with date
names(cci_raw)[2:ncol(cci_raw)] <- as.character(cci_date_seq$date)

# rename indices
cci_raw$Indices[cci_raw$Indices == "Indeks Keyakinan Konsumen (IKK)"] <- "cci"

# tidy data
cci_tidy <- cci_raw %>% 
  dplyr::filter(Indices == "cci") %>% 
  pivot_longer(2:ncol(.), names_to = "date", values_to = "cci") %>%
  dplyr::select(!Indices)

# correct data types
cci_tidy$date <- ymd(cci_tidy$date)
cci_tidy$cci <- as.numeric(cci_tidy$cci)

# round cci to two decimal places
cci_tidy$cci <- round(cci_tidy$cci, 2)

# rename column for csv
cci_csv <- cci_tidy %>% 
  rename(consumer_confidence_index = 2)


# plot --------------------------------------------------------------------

# y-axis range
cci_y_axis_range <- c(40, 141)

# plot
plot_cci <- plot_ly(
  cci_tidy,
  height = 300
) %>%
  add_lines(
    x = ~date, 
    y = ~cci,
    hovertemplate = "Consumer confidence: %{y}<br>Date: %{x}<extra></extra>",
    line = list(color = "#2477B3", width = 3)
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      autorange = F,
      range = c(
        as.character(first(cci_tidy$date) - 180), 
        as.character(last(cci_tidy$date) + 180)
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
      range = cci_y_axis_range,
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
        x0 = as.character(first(cci_tidy$date) - 180),
        x1 = as.character(last(cci_tidy$date) + 180),
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
        y0 = cci_y_axis_range[1],
        y1 = cci_y_axis_range[2] - 1,
        line = list(color = "#90A4AE", dash = "dash")
      )
    ),
    annotations = list(
      list(
        text = "More optimistic &#8593;",
        font = list(size = 12),
        align = "left",
        bgcolor = "white",
        showarrow = F,
        xref = "x",
        x = "2013-11-01",
        yref = "y",
        y = 130
      ),
      list(
        text = "More pessimistic &#8595;",
        font = list(size = 12),
        align = "left",
        bgcolor = "white",
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
        y = cci_y_axis_range[2] - 1.5,
        yanchor = "top"
      ) 
    ),
    margin = list(l = 0, r = 0, t = 15, b = 0),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)


# export chart ------------------------------------------------------------

# path to overall cci data
path_data_cci <- "data/ier_cci-overall_cleaned.csv"

# export chart
if (nrow(cci_csv) != nrow(read_csv(path_data_cci))) {
  
  # import functions to apply custom ggplot2 theme and add logo
  source("script/ggplot2_theme.R")
  
  # annotations: confidence territories
  anno_conf_ths <- tribble(
    ~x, ~y, ~label,
    ymd("2013-01-01"), 90, "More pessimistic \u2193",
    ymd("2013-01-01"), 130, "More optimistic \u2191"
  )
  
  # plot
  ggplot(data = cci_tidy, aes(date, cci)) +
    geom_hline(yintercept = 100, color = "#E68F7E") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      lty = "dashed"
    ) +
    geom_line(color = "#2477B3", lwd = 1) +
    scale_x_date(
      breaks = seq(ymd("2012-01-01"), last(cci_tidy$date), by = "1 year"),
      labels = c("2012", str_c("'", seq(13, 21)))
    ) +
    scale_y_continuous(
      breaks = seq(40, 140, 20),
      limits = c(40, 140),
      expand = c(0, 0),
      position = "right"
    ) +
    ggtext::geom_richtext(
      data = anno_conf_ths,
      aes(x = x, y = y, label = label),
      fill = "white",
      label.color = NA,
      size = 3,
      hjust = 0
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
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Bank Indonesia"
    ) +
    theme_ier() +
    theme(plot.title = element_text(margin = margin(b = 25)))
  
  # path to the plot
  path_plot_cci <- "fig/ier_cci_plot.png"
  
  # save the plot
  ggsave(path_plot_cci, width = 6, height = 3.708, dpi = 300)
  
  # add ier logo to the plot
  add_ier_logo(path_plot_cci)
  
  # message
  message("The Consumer Confidence Index chart has been updated")
  
} else {
  
  message("The Consumer Confidence Index chart is up to date")
  
}


# export data -------------------------------------------------------------

if (!file.exists(path_data_cci)) {
  
  write_csv(cci_csv, path_data_cci)
  
  message("The Consumer Confidence Index dataset has been exported")
  
} else if (nrow(cci_csv) != nrow(read_csv(path_data_cci))) {
  
  write_csv(cci_csv, path_data_cci)
  
  message("The Consumer Confidence Index dataset has been updated")
  
} else {
  
  message("The Consumer Confidence Index dataset is up to date")
  
}