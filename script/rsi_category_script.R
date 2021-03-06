# indonesia economic recovery

# retail sales by category

# author: dzulfiqar fathur rahman
# created: 2021-02-26
# last updated: 2021-07-15
# page: retail sales


# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)
library(magick)
library(ggtext)


# data --------------------------------------------------------------------

# date of most recent observation
if (!exists("rsi_last_date")) {
  rsi_last_date <- "2021-06-01"
}

# import
rsi_by_cat_raw <- read_excel(
  "data/bi_rsi_raw.xlsx", 
  sheet = "Tabel 1",
  skip = 3
) 

# rename indicator variable
names(rsi_by_cat_raw)[1] <- "Indices"

# categories
cat <- c(
  "Suku Cadang dan Aksesori",
  "Makanan, Minuman & Tembakau",
  "Bahan Bakar Kendaraan Bermotor",
  "Peralatan Informasi dan Komunikasi",
  "Perlengkapan Rumah Tangga Lainnya",
  "Barang Budaya dan Rekreasi",
  "Barang Lainnya",
  "- o/w Sandang"
)

# remove columns, rows containing missing values
rsi_by_cat_raw <- rsi_by_cat_raw %>% 
  dplyr::filter(!is.na(Indices), Indices %in% cat) %>% 
  select_if(function(x) {!all(is.na(x))}) %>% 
  select(-last(names(rsi_by_cat_raw)))

# rename date columns
## dates
rsi_by_cat_date_seq <- seq(ymd("2012-01-01"), ymd(rsi_last_date), by = "month")

## correct sorting
rsi_by_cat_date_seq <- rsi_by_cat_date_seq %>%
  tibble() %>% 
  mutate(yr = date(rsi_by_cat_date_seq), mo = month(rsi_by_cat_date_seq)) %>% 
  arrange(yr, mo) %>% 
  rename(date = 1)

## replace column names
names(rsi_by_cat_raw)[2:ncol(rsi_by_cat_raw)] <- as.character(rsi_by_cat_date_seq$date)

# translate categories
cat_en <- c(
  "Spare parts and accessories",
  "Food, beverage and tobacco",
  "Vehicle fuel",
  "Information and communication tools",
  "Other household supplies",
  "Cultural and recreational goods",
  "Other goods",
  "Clothes"
)

# rename categories
rsi_by_cat_raw$Indices <- rsi_by_cat_raw$Indices %>% 
  str_replace_all(cat, cat_en)

# tidy data
rsi_by_cat_tidy <- rsi_by_cat_raw %>% 
  pivot_longer(2:ncol(.), names_to = "date", values_to = "rsi")

# correct data types
rsi_by_cat_tidy$Indices <- as.factor(rsi_by_cat_tidy$Indices)
rsi_by_cat_tidy$date <- ymd(rsi_by_cat_tidy$date)
rsi_by_cat_tidy$rsi <- as.numeric(rsi_by_cat_tidy$rsi)

# round to two decimal places
rsi_by_cat_tidy$rsi <- round(rsi_by_cat_tidy$rsi, 2)

# calculate annual change
rsi_by_cat_trf <- rsi_by_cat_tidy %>% 
  group_by(Indices) %>% 
  mutate(
    diff_yoy = rsi - dplyr::lag(rsi, 12),
    pct_change_yoy = round(diff_yoy / dplyr::lag(rsi, 12) * 100, 2)
  ) %>% 
  ungroup()

# reshape to wide format for small multiples
rsi_by_cat_chg_wide <- rsi_by_cat_trf %>% 
  dplyr::filter(!is.na(pct_change_yoy)) %>% 
  select(date, Indices, pct_change_yoy) %>% 
  pivot_wider(names_from = Indices, values_from = pct_change_yoy)

# reshape to wide format for seasonal small multiples
rsi_by_cat_idx_wide <- rsi_by_cat_trf %>% 
  mutate(mo = month(date), yr = year(date)) %>% 
  dplyr::filter(yr >= 2016) %>% 
  select(-c("diff_yoy", "pct_change_yoy")) %>% 
  pivot_wider(names_from = Indices, values_from = rsi)


# plot --------------------------------------------------------------------

## rsi change ----

# covid text annotation
anno_text_covid <- list(
  text = "COVID-19<br>pandemic",
  font = list(size = 7, color = "#90A4AE"),
  align = "right",
  bgcolor = "white",
  showarrow = F,
  xref = "x",
  x = "2020-02-01",
  xanchor = "right",
  yref = "y",
  y = 97.5,
  yanchor = "top"
)

# subtitles
## spare parts
anno_sub_spare <- list(
  text = "<b>Spare parts<br>and accessories<b>",
  font = list(size = 8),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0,
  xanchor = "left",
  yref = "paper",
  y = 1.125,
  yanchor = "top"
)

## f&b, tobacco
anno_sub_food <- list(
  text = "<b>Food, beverage<br>and tobacco<b>",
  font = list(size = 8),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.265,
  xanchor = "left",
  yref = "paper",
  y = 1.125,
  yanchor = "top"
)

## vehicle fuel
anno_sub_fuel <- list(
  text = "<b>Vehicle fuel<b>",
  font = list(size = 8),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.515,
  xanchor = "left",
  yref = "paper",
  y = 1.125,
  yanchor = "top"
)

## information, communication tools
anno_sub_info <- list(
  text = "<b>ICT<sup>*</sup><b>",
  font = list(size = 8),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.765,
  xanchor = "left",
  yref = "paper",
  y = 1.125,
  yanchor = "top"
)

## other household supplies
anno_sub_hh <- list(
  text = "<b>Other household<br>supplies<b>",
  font = list(size = 8),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0,
  xanchor = "left",
  yref = "paper",
  y = 0.5,
  yanchor = "top"
)

## cultural, recreational goods
anno_sub_culture <- list(
  text = "<b>CRG<sup>&#8224;</sup><b>",
  font = list(size = 8),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.265,
  xanchor = "left",
  yref = "paper",
  y = 0.5,
  yanchor = "top"
)

## other goods
anno_sub_other <- list(
  text = "<b>Other goods<b>",
  font = list(size = 8),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.515,
  xanchor = "left",
  yref = "paper",
  y = 0.5,
  yanchor = "top"
)

## clothes
anno_sub_clothes <- list(
  text = "<b>Clothes<b>",
  font = list(size = 8),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.765,
  xanchor = "left",
  yref = "paper",
  y = 0.5,
  yanchor = "top"
)

# variable names
rsi_by_cat_chg_var <- names(rsi_by_cat_chg_wide)[-1]

# plot
plot_rsi_by_cat_chg <- lapply(
  rsi_by_cat_chg_var, 
  function(x) {
    plot_ly(
      rsi_by_cat_chg_wide,
      x = ~date,
      y = as.formula(str_c("~", "`", x, "`")),
      text = x,
      hovertemplate = str_c(
        "<b>Category: %{text}</b><br><br>",
        "Change: %{y} percent<br>",
        "Date: %{x}<extra></extra>"
      ),
      line = list(color = "#2477B3", width = 2)
    ) %>% 
      add_lines() %>%
      plotly::layout(
        xaxis = list(
          title = NA,
          autorange = F,
          range = c(
            as.character(first(rsi_by_cat_chg_wide$date) - 180), 
            as.character(last(rsi_by_cat_chg_wide$date) + 180)
          ),
          fixedrange = T,
          tickmode = "auto",
          dtick = "M24",
          ticks = "outside",
          tickformat = "'%y",
          automargin = T,
          tickfont = list(size = 8),
          hoverformat = "%B %Y",
          showline = T,
          showgrid = F
        ),
        yaxis = list(
          title = NA,
          type = "linear",
          autorange = F,
          range = c(-100, 105),
          fixedrange = T,
          dtick = 50,
          tickfont = list(size = 8),
          showline = F,
          linewidth = 0,
          showgrid = T,
          gridcolor = "#CFD8DC",
          zerolinecolor = "#E68F7E",
          side = "left"
        ),
        shapes = list(
          list(
            type = "line",
            layer = "below",
            xref = "x",
            x0 = "2020-03-02",
            x1 = "2020-03-02",
            yref = "y",
            y0 = -100,
            y1 = 100,
            line = list(color = "#90A4AE", width = 1, dash = "dot")
          )
        ),
        showlegend = F,
        autosize = T
      ) %>% 
      plotly::config(displayModeBar = F)
  }
)

# small multiples
sm_rsi_by_cat_chg <- subplot(
  plot_rsi_by_cat_chg,
  nrows = 2,
  margin = c(0.0125, 0.0125, 0.1, 0.1),
  shareY = T
) %>% 
  plotly::layout(
    yaxis = list(showline = F),
    annotations = list(
      anno_text_covid,
      anno_sub_spare,
      anno_sub_food,
      anno_sub_fuel,
      anno_sub_info,
      anno_sub_hh,
      anno_sub_culture,
      anno_sub_other,
      anno_sub_clothes
    ),
    margin = list(l = 0, r = 0, t = 40, b = 0)
  )


# rsi by cat index --------------------------------------------------------

# plot
plot_rsi_by_cat_idx <- lapply(
  rsi_by_cat_chg_var, 
  function(x) {
    plot_ly(
      rsi_by_cat_idx_wide,
      type = "scatter",
      mode = "markers+lines",
      colors = c(rep("lightgrey", 4), "#36A3D9", "#2477B3"),
      text = ~format(date, "%B %Y"),
      hovertemplate = str_c(
        "<b>Category: ", x, "</b><br><br>",
        "Index: %{y}<br>Date: %{text}<extra></extra>"
      ),
      line = list(width = 2)
    ) %>% 
      add_trace(
        x = ~mo,
        y = as.formula(str_c("~", "`", x, "`")),
        color = ~factor(yr),
        marker = list(size = 4)
      ) %>%
      plotly::layout(
        xaxis = list(
          title = NA,
          autorange = T,
          fixedrange = T,
          tickmode = "array",
          tickvals = c(1, 4, 7, 10),
          ticktext = c("Jan", "Apr", "Jul", "Oct"),
          ticks = "outside",
          automargin = T,
          tickfont = list(size = 8),
          showline = T,
          showgrid = F
        ),
        yaxis = list(
          title = NA,
          type = "linear",
          autorange = F,
          range = c(0, 505),
          fixedrange = T,
          dtick = 100,
          tickfont = list(size = 8),
          showline = F,
          showgrid = T,
          gridcolor = "#CFD8DC",
          side = "left"
        ),
        showlegend = F,
        autosize = T
      ) %>% 
      plotly::config(displayModeBar = F)
  }
)

# annotations
# 2016-2019
anno_1619 <- list(
  text = "<b>2016-2019</b>",
  font = list(size = 7, color = "lightgrey"),
  align = "left",
  bgcolor = "white",
  showarrow = F,
  xref = "paper",
  x = 0.1,
  xanchor = "left",
  yref = "paper",
  y = 0.78,
  yanchor = "top"
) 

# 2020
anno_2020 <- list(
  text = "<b>2020</b>",
  font = list(size = 7, color = "#36A3D9"),
  align = "left",
  bgcolor = "white",
  showarrow = F,
  xref = "paper",
  x = 0.15,
  xanchor = "left",
  yref = "paper",
  y = 0.675,
  yanchor = "top"
) 

# 2021
anno_2021 <- list(
  text = "<b>2021</b>",
  font = list(size = 7, color = "#2477B3"),
  align = "left",
  bgcolor = "white",
  showarrow = F,
  xref = "paper",
  x = 0.015,
  xanchor = "left",
  yref = "paper",
  y = 0.675,
  yanchor = "top"
) 

# small multiples
sm_rsi_by_cat_idx <- subplot(
  plot_rsi_by_cat_idx,
  nrows = 2,
  margin = c(0.0125, 0.0125, 0.1, 0.1),
  shareY = T
) %>% 
  plotly::layout(
    yaxis = list(showline = F),
    annotations = list(
      anno_sub_spare,
      anno_sub_food,
      anno_sub_fuel,
      anno_sub_info,
      anno_sub_hh,
      anno_sub_culture,
      anno_sub_other,
      anno_sub_clothes,
      anno_1619,
      anno_2020,
      anno_2021
    ),
    margin = list(l = 0, r = 0, t = 40, b = 0)
  )


# export chart ------------------------------------------------------------

# rename column for csv
rsi_cat_csv <- rsi_by_cat_trf %>% 
  rename(category = 1, retail_sales_index = 3) %>% 
  select(-diff_yoy)

# path to rsi by category data
path_data_rsi_cat <- "data/ier_rsi-category_cleaned.csv"

# export chart
if (nrow(rsi_cat_csv) != nrow(read_csv(path_data_rsi_cat))) {
  
  # import functions to apply custom ggplot2 theme and add logo
  source("script/ggplot2_theme.R")
  
  ## rsi change ----
  
  # annotations
  anno_text_covid_ggplot <- tibble(
    x = ymd("2020-01-01"),
    y = 75,
    label = "COVID-19\npandemic",
    Indices = "Clothes"
  )
  
  # plot
  rsi_by_cat_trf %>% 
    dplyr::filter(!is.na(pct_change_yoy)) %>% 
    ggplot(aes(date, pct_change_yoy)) +
    geom_hline(yintercept = 0, color = "#E68F7E") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_line(color = "#2477B3", lwd = 0.75) +
    scale_x_date(
      breaks = seq(ymd("2013-01-01"), ymd("2021-01-01"), "2 year"),
      labels = c(2013, str_c("'", seq(15, 21, 2)))
    ) +
    scale_y_continuous(
      breaks = seq(-100, 100, 50),
      limits = c(-100, 100),
      expand = c(0, 0),
      position = "right"
    ) +
    geom_text(
      data = anno_text_covid_ggplot,
      aes(x = x, y = y, label = label),
      color = "#90A4AE",
      hjust = 1,
      size = 1.75
    ) +
    labs(
      title = "Retail sales index",
      subtitle = "By category (percent change from a year earlier)",
      caption = str_c(
        str_c(format(last(rsi_by_cat_trf$date), "%B %Y"), " figure is an estimate\n\n"),
        "Chart: Dzulfiqar Fathur Rahman | Source: Bank Indonesia"
      )
    ) +
    facet_wrap(
      ~ Indices, 
      nrow = 2,
      scales = "free_x", 
      labeller = labeller(Indices = label_wrap_gen(27.5))
    ) +
    theme_ier(rel_size = 0.7) +
    theme(
      panel.spacing = unit(1, "lines"),
      strip.background = element_blank(),
      strip.text = element_text(
        size = rel(0.7),
        hjust = 0,
        vjust = 1, 
        margin = margin(b = 10)
      )
    )
  
  # path to the plot
  path_plot_rsi_cat_chg <- "fig/ier_rsi-cat-change_plot.png"
  
  # save the plot
  ggsave(path_plot_rsi_cat_chg, width = 7, height = 5, dpi = 300)
  
  # add logo
  ier_logo <- image_read("images/ier_hexsticker_small.png")
  
  # add base plot
  plot_rsi_cat_change_png <- image_read(path_plot_rsi_cat_chg)
  
  # get plot height
  plot_height <- magick::image_info(plot_rsi_cat_change_png)$height
  
  # get plot width
  plot_width <- magick::image_info(plot_rsi_cat_change_png)$width
  
  # get logo height
  logo_width <- magick::image_info(ier_logo)$width
  
  # get logo width
  logo_height <- magick::image_info(ier_logo)$height
  
  # position for the bottom 1.5 percent
  pos_bottom <- plot_height - logo_height - plot_height * 0.015
  
  # position for the right 0.05 percent
  pos_right <- plot_width - logo_width - 0.005 * plot_width
  
  # overwrite plot
  plot_rsi_cat_change_png %>% 
    image_composite(ier_logo, offset = str_c("+", pos_right, "+", pos_bottom)) %>% 
    image_write(path_plot_rsi_cat_chg)
  
  
  # rsi by cat index ----
  
  # annotations
  anno_year <- tibble(
    x = c(1, 9, 7),
    y = c(25, 25, 250),
    label = c("2021", "2020", "2016-2019"),
    Indices = rep("Clothes", 3)
  )
  
  # plot
  rsi_by_cat_trf %>% 
    mutate(mo = month(date), yr = year(date)) %>% 
    dplyr::filter(yr >= 2016) %>% 
    ggplot(aes(mo, rsi)) +
    geom_line(aes(color = as_factor(yr)), lwd = 0.75, show.legend = F) +
    geom_point(aes(color = as_factor(yr)), size = 1, show.legend = F) +
    scale_x_continuous(
      breaks = c(1, 4, 7, 10),
      labels = c("Jan", "April", "July", "Oct")
    ) +
    scale_y_continuous(
      breaks = seq(0, 500, 100),
      limits = c(0, 500),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(values = c(rep("lightgrey", 4), "#36A3D9", "#2477B3")) +
    geom_text(
      data = anno_year,
      aes(x = x, y = y, label = label),
      hjust = 0,
      size = 2,
      color = c("#2477B3", "#36A3D9", "lightgrey"),
      fontface = "bold"
    ) +
    labs(
      title = "Retail sales index",
      subtitle = "By category",
      caption = str_c(
        str_c(format(last(rsi_by_cat_trf$date), "%B %Y"), " figure is an estimate\n\n"),
        "Chart: Dzulfiqar Fathur Rahman | Source: Bank Indonesia"
      )
    ) +
    facet_wrap(
      ~ Indices, 
      nrow = 2,
      scales = "free_x", 
      labeller = labeller(Indices = label_wrap_gen(27.5))
    ) +
    theme_ier(rel_size = 0.7) +
    theme(
      panel.spacing = unit(1, "lines"),
      strip.background = element_blank(),
      strip.text = element_text(
        size = rel(0.7),
        hjust = 0,
        vjust = 1, 
        margin = margin(b = 10)
      )
    )
  
  # path to the plot
  path_plot_rsi_cat_idx <- "fig/ier_rsi-cat_plot.png"
  
  # save the plot
  ggsave(path_plot_rsi_cat_idx, width = 7, height = 5, dpi = 300)
  
  # add logo
  ier_logo <- image_read("images/ier_hexsticker_small.png")
  
  # add base plot
  plot_rsi_cat_index_png <- image_read(path_plot_rsi_cat_idx)
  
  # get plot height
  plot_height <- magick::image_info(plot_rsi_cat_change_png)$height
  
  # get plot width
  plot_width <- magick::image_info(plot_rsi_cat_change_png)$width
  
  # get logo height
  logo_width <- magick::image_info(ier_logo)$width
  
  # get logo width
  logo_height <- magick::image_info(ier_logo)$height
  
  # position for the bottom 1.5 percent
  pos_bottom <- plot_height - logo_height - plot_height * 0.015
  
  # position for the right 0.05 percent
  pos_right <- plot_width - logo_width - 0.005 * plot_width
  
  # overwrite plot
  plot_rsi_cat_index_png %>% 
    image_composite(ier_logo, offset = str_c("+", pos_right, "+", pos_bottom)) %>% 
    image_write(path_plot_rsi_cat_idx)
  
  # message
  message("The Retail Sales Index by category charts have been updated")
  
} else {
  
  message("The Retail Sales Index by category charts are up to date")
  
}


# export data -------------------------------------------------------------

# write csv
if (!file.exists(path_data_rsi_cat)) {
  
  write_csv(rsi_cat_csv, path_data_rsi_cat)
  
  message("The Retail Sales Index dataset, broken down by category, has been exported")
  
} else if (nrow(rsi_cat_csv) != nrow(read_csv(path_data_rsi_cat))) {
  
  write_csv(rsi_cat_csv, path_data_rsi_cat)
  
  message("The Retail Sales Index dataset, broken down by category, has been updated")
  
} else {
  
  message("The Retail Sales Index dataset, broken down by category, is up to date")
  
}