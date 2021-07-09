# indonesia economic recovery

# consumer confidence
# by income group

# author: dzulfiqar fathur rahman
# created: 2021-02-24
# last updated: 2021-07-09
# page: consumer confidence


# setup -------------------------------------------------------------------

# packages
library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)
library(ggtext)
library(magick)

# date of most recent observation
if (exists("cci_last_date") == F) {
  cci_last_date <- "2021-06-01"
}  


# data --------------------------------------------------------------------

# import
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
  dplyr::mutate(
    yr = date(cci_date_seq), 
    mo = month(cci_date_seq)
  ) %>% 
  dplyr::arrange(yr, mo) %>% 
  dplyr::rename(date = 1)

## replace column names
names(cci_by_income_raw)[2:ncol(cci_by_income_raw)] <- as.character(cci_date_seq$date)

# rename income groups
cci_by_income_raw[, 1] <- c("1_2", "2.1_3", "3.1_4", "4.1_5", "above_5")

# tidy data
cci_by_income_tidy <- cci_by_income_raw %>% 
  pivot_longer(
    2:ncol(.),
    names_to = "date", 
    values_to = "cci"
  )

# correct data types
cci_by_income_tidy$Indices <- as.factor(cci_by_income_tidy$Indices)

cci_by_income_tidy$date <- ymd(cci_by_income_tidy$date)

cci_by_income_tidy$cci <- as.numeric(cci_by_income_tidy$cci)

# wide data for plot
cci_by_income_wide <- cci_by_income_tidy %>% 
  pivot_wider(
    names_from = Indices, 
    values_from = cci
  ) %>% 
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


# plot --------------------------------------------------------------------

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
      "Index: %{y}<br>Date: %{x}<extra></extra>"
    ),
    line = list(color = "#ff5e4b")
  ) %>%
  add_lines(
    name = "Rp 2.1-3 million",
    y = ~`2.1_3`,
    hovertemplate = str_c(
      "<b>Income group: Rp 2.1-3 million</b><br><br>",
      "Index: %{y}<br>Date: %{x}<extra></extra>"
    ),
    line = list(color = "#ffca76")
  ) %>% 
  add_lines(
    name = "Rp 3.1-4 million",
    y = ~`3.1_4`,
    hovertemplate = str_c(
      "<b>Income group: Rp 3.1-4 million</b><br><br>",
      "Index: %{y}<br>Date: %{x}<extra></extra>"
    ),
    line = list(color = "#09bb9f")
  ) %>% 
  add_lines(
    name = "Rp 4.1-5 million",
    y = ~`4.1_5`,
    hovertemplate = str_c(
      "<b>Income group: Rp 4.1-5 million</b><br><br>",
      "Index: %{y}<br>Date: %{x}<extra></extra>"
    ),
    line = list(color = "#5cccfa")
  ) %>% 
  add_lines(
    name = "Above Rp 5 million",
    y = ~above_5,
    hovertemplate = str_c(
      "<b>Income group: above Rp 5 million</b><br><br>",
      "Index: %{y}<br>Date: %{x}<extra></extra>"
    ),
    line = list(color = "#607d8b")
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
      hoverformat = "%b '%y"
    ),
    yaxis = list(
      title = NA,
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
        x0 = as.character(first(cci_by_income_wide$date) - 180),
        x1 = as.character(last(cci_by_income_wide$date) + 180),
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
    legend = list(
      orientation = "h",
      xanchor = "left",
      y = 1.35,
      yanchor = "top",
      valign = "top"
    ),
    margin = list(l = 0, r = 0, t = 15, b = 0),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)


# export chart ------------------------------------------------------------

# latest observation in most recent csv
cci_by_income_csv <- cci_by_income_tidy %>% 
  rename(income_group = 1, consumer_confidence_index = 3)

# export chart
if (nrow(cci_by_income_csv) != nrow(read_csv("data/ier_cci-income_cleaned.csv"))) {
  
  # annotations
  ## confidence threshold
  anno_conf_ths <- tribble(
    ~x, ~y, ~label,
    ymd("2014-01-01"), 90, "More pessimistic \u2193",
    ymd("2016-01-01"), 130, "More optimistic \u2191"
  )
  
  # plot
  ggplot(data = cci_by_income_tidy, aes(date, cci)) +
    geom_hline(yintercept = 100, color = "#ff856c") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_line(aes(color = Indices), lwd = 0.75) +
    scale_y_continuous(
      breaks = seq(40, 140, 20),
      limits = c(40, 140),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(
      labels = c("Rp 1-2 million", "Rp 2.1-3 million", "Rp 3.1-4 million", "Rp 4.1-5 million", "Above Rp 5 million"),
      values = c("#ff5e4b", "#ffca76", "#09bb9f", "#5cccfa", "#607d8b")    
    ) +
    ggtext::geom_richtext(
      data = anno_conf_ths,
      aes(x = x, y = y, label = label),
      fill = "white",
      label.color = NA,
      size = 2.75
    ) +
    annotate(
      "text",
      x = ymd("2020-04-01"),
      y = 130,
      label = "COVID-19\npandemic \u2192",
      size = 2.5,
      hjust = 0,
      color = "#90A4AE"
    ) +
    labs(
      title = "Consumer confidence index",
      subtitle = "By income group",
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Bank Indonesia"
    ) +
    theme(
      text = element_text(size = 12),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(color = "black"),
      legend.title = element_blank(),
      legend.key = element_rect(fill = "transparent"),
      legend.position = c(0.495, 1.175),
      legend.direction = "horizontal",
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#CFD8DC"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 55)),
      plot.caption = element_text(
        color = "#757575",
        hjust = 0,
        margin = margin(t = 35)
      )
    ) +
    ggsave(
      "fig/ier_cci-income_plot.png",
      width = 7,
      height = 4,
      dpi = 300
    )
  
  # add logo
  ier_logo <- image_read("images/ier_hexsticker_small.png")
  
  # add base plot
  base_plot <- image_read("fig/ier_cci-income_plot.png")
  
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
    image_write("fig/ier_cci-income_plot.png")
  
  # message
  message("The Consumer Confidence Index by income group chart has been updated")
  
} else {
  
  message("The Consumer Confidence Index by income group chart is up to date")
  
}


# preview -----------------------------------------------------------------

if (nrow(cci_by_income_csv) != nrow(read_csv("data/ier_cci-income_cleaned.csv"))) {
  
  # plot
  ggplot(data = cci_by_income_tidy, aes(date, cci)) +
    geom_hline(yintercept = 100, color = "#ff856c") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_line(aes(color = Indices), lwd = 1.5, show.legend = F) +
    scale_y_continuous(
      breaks = seq(40, 140, 20),
      limits = c(40, 140),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(
      labels = c("Rp 1-2 million", "Rp 2.1-3 million", "Rp 3.1-4 million", "Rp 4.1-5 million", "Above Rp 5 million"),
      values = c("#ff5e4b", "#ffca76", "#09bb9f", "#5cccfa", "#607d8b")    
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#263238", color = NA),
      plot.margin = margin(t = 50, r = 50, b = 50, l = 50)
    ) +
    ggsave(
      "fig/ier_cci-income_void_plot.png",
      width = 13.3,
      height = 6.6,
      dpi = 300
    )
  
  # message
  message("The Consumer Confidence Index by income group preview chart has been updated")
  
} else {
  
  message("The Consumer Confidence Index by income group preview chart is up to date")
  
}


# export data -------------------------------------------------------------

# write csv
if (file.exists("data/ier_cci-income_cleaned.csv") == F) {
  
  write_csv(cci_by_income_csv, "data/ier_cci-income_cleaned.csv")
  
  message("The Consumer Confidence Index dataset, broken down by income group, has been exported")
  
} else if(nrow(cci_by_income_csv) != nrow(read_csv("data/ier_cci-income_cleaned.csv"))) {
  
  write_csv(cci_by_income_csv, "data/ier_cci-income_cleaned.csv")
  
  message("The Consumer Confidence Index dataset, broken down by income group, has been updated")
  
} else {
  
  message("The Consumer Confidence Index dataset, broken down by income group is up to date")
  
}