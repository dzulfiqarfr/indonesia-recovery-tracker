# indonesia economic recovery

# consumer confidence

# author: dzulfiqar fathur rahman
# created: 2021-02-24
# last updated: 2021-04-09
# page: consumer confidence


# setup -------------------------------------------------------------------

# packages
library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)
library(magick)

# date of most recent observation
if (exists("last_date") == F) {
  last_date <- "2021-03-01"
}  

# data --------------------------------------------------------------------

# import
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
cci_date_seq <- seq(ymd("2012-01-01"), ymd(last_date), by = "month")

## correct sorting
cci_date_seq <- cci_date_seq %>%
  tibble() %>% 
  mutate(
    yr = date(cci_date_seq), 
    mo = month(cci_date_seq)
  ) %>% 
  arrange(yr, mo) %>% 
  rename(date = 1)

## replace column names
names(cci_raw)[2:ncol(cci_raw)] <- as.character(cci_date_seq$date)

# rename indices
cci_raw$Indices[cci_raw$Indices == "Indeks Keyakinan Konsumen (IKK)"] <- "cci"

# tidy data
cci_tidy <- cci_raw %>% 
  dplyr::filter(Indices == "cci") %>% 
  pivot_longer(
    2:ncol(.),
    names_to = "date",
    values_to = "cci"
  ) %>%
  dplyr::select(!Indices)

# correct data types
cci_tidy$date <- ymd(cci_tidy$date)

cci_tidy$cci <- as.numeric(cci_tidy$cci)

# round cci to two decimal places
cci_tidy$cci <- round(cci_tidy$cci, 2)


# export data -------------------------------------------------------------

# data
cci_csv <- cci_tidy %>% 
  rename(consumer_confidence_index = 2)

# write csv
if (file.exists("data/ier_cci-overall_cleaned.csv") == F) {
  
  write_csv(cci_csv, "data/ier_cci-overall_cleaned.csv")
  
  message("The Consumer Confidence Index dataset has been exported")
  
} else if (nrow(cci_csv) != nrow(read_csv("data/ier_cci-overall_cleaned.csv"))) {
  
  write_csv(cci_csv, "data/ier_cci-overall_cleaned.csv")
  
  message("The Consumer Confidence Index dataset has been updated")
  
} else {
  
  message("The Consumer Confidence Index dataset is up to date")
  
}


# plot --------------------------------------------------------------------

# plot
plot_cci <- plot_ly(
  cci_tidy,
  height = 300
) %>%
  add_lines(
    x = ~date, 
    y = ~cci,
    hovertemplate = "Index: %{y}<br>Date: %{x}<extra></extra>",
    line = list(color = "#1d81a2", width = 3)
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
      hoverformat = "%b '%y",
      showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = NA,
      type = "linear",
      autorange = F,
      range = c(40, 141),
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
        line = list(color = "#ff856c", size = 1)
      ),
      list(
        type = "line",
        layer = "below",
        xref = "x",
        x0 = "2020-03-02",
        x1 = "2020-03-02",
        yref = "y",
        y0 = 40,
        y1 = 140,
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
        align = "right",
        bgcolor = "white",
        showarrow = F,
        xref = "x",
        x = "2020-02-01",
        xanchor = "right",
        yref = "y",
        y = 70,
        yanchor = "top"
      ) 
    ),
    margin = list(l = 0, r = 0, t = 15, b = 0),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)


# export chart ------------------------------------------------------------

if (nrow(cci_csv) != nrow(read_csv("data/ier_cci-overall_cleaned.csv"))) {
  
  # annotations
  ## confidence threshold
  anno_conf_ths <- tribble(
    ~x, ~y, ~label,
    ymd("2013-01-01"), 90, "More pessimistic \u2193",
    ymd("2013-01-01"), 130, "More optimistic \u2191"
  )
  
  # plot
  ggplot(data = cci_tidy, aes(date, cci)) +
    geom_hline(yintercept = 100, color = "#ff856c") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_line(color = "#1d81a2", lwd = 1) +
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
      size = 2.75,
      hjust = 0
    ) +
    annotate(
      "text",
      x = ymd("2020-04-01"),
      y = 130,
      label = "COVID-19\npandemic \u2192",
      size = 2.75,
      hjust = 0,
      color = "#90A4AE"
    ) +
    labs(
      title = "Consumer confidence index",
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Bank Indonesia"
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
      plot.title = element_text(face = "bold", margin = margin(b = 35)),
      plot.caption = element_text(
        color = "#757575",
        hjust = 0,
        margin = margin(t = 35)
      )
    ) +
    ggsave(
      "fig/ier_cci_plot.png",
      width = 7,
      height = 4,
      dpi = 300
    )
  
  # add logo
  ier_logo <- image_read("images/ier_hexsticker_small.png")
  
  # add base plot
  base_plot <- image_read("fig/ier_cci_plot.png")
  
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
    image_write("fig/ier_cci_plot.png")
  
  # message
  message("The Consumer Confidence Index chart has been updated")
  
} else {
  
  message("The Consumer Confidence Index chart is up to date")
  
}
