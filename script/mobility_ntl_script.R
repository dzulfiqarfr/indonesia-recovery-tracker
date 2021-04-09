# indonesia economic recovery

# google covid-19 community mobility reports
# national

# author: dzulfiqar fathur rahman
# created: 2021-03-01
# last updated: 2021-04-09
# page: mobility


# setup -------------------------------------------------------------------

# packages
library(tidyverse)
library(lubridate)
library(zoo)
library(plotly)
library(magick)
library(ggrepel)

# download url
if (exists("mob_url") == F) {
  mob_url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
}


# data --------------------------------------------------------------------

# import global dataset
if (exists("mob_gbl_raw") == F) {
  mob_gbl_raw <- read_csv(mob_url, na = "")
} else {
  rm(mob_gbl_raw)
}

# subset indonesia observations
if (exists("mob_idn_raw") == F) {
  
  # subset, rename variable
  mob_idn_raw <- mob_gbl_raw %>% 
    dplyr::filter(country_region == "Indonesia") %>% 
    select(
      country_region,
      sub_region_1,
      date,
      retail_and_recreation_percent_change_from_baseline,
      grocery_and_pharmacy_percent_change_from_baseline,
      parks_percent_change_from_baseline,
      transit_stations_percent_change_from_baseline,
      workplaces_percent_change_from_baseline,
      residential_percent_change_from_baseline
    ) %>% 
    rename(
      "country" = 1,
      "province" = 2,
      "date" = 3,
      "retail_recreation" = 4,
      "grocery_pharmacy" = 5,
      "parks" = 6,
      "transit_stations" = 7,
      "workplaces" = 8,
      "residential" = 9
    )
  
  # correct data types for `country`, `province`
  mob_idn_raw[, 1:2] <- lapply(mob_idn_raw[, 1:2], as.factor)
  
}

# correct data types for `country`, `province`
mob_idn_raw[, 1:2] <- lapply(mob_idn_raw[, 1:2], as.factor)

# remove regional data
mob_ntl_raw <- mob_idn_raw %>% 
  dplyr::filter(is.na(province)) %>% 
  dplyr::select(-(1:2)) 

# calculate 7-day moving averages
mob_ntl_trf <- mob_ntl_raw %>% 
  arrange(date) %>% 
  mutate(
    retail_recreation_avg = rollmean(retail_recreation, k = 7, fill = NA, align = "right"),
    grocery_pharmacy_avg = rollmean(grocery_pharmacy, k = 7, fill = NA, align = "right"),
    parks_avg = rollmean(parks, k = 7, fill = NA, align = "right"),
    transit_stations_avg = rollmean(transit_stations, k = 7, fill = NA, align = "right"),
    workplaces_avg = rollmean(workplaces, k = 7, fill = NA, align = "right"),
    residential_avg = rollmean(residential, k = 7, fill = NA, align = "right")
  )

# round moving averages to two decimal places
mob_ntl_trf[, 2:ncol(mob_ntl_trf)] <- lapply(
  mob_ntl_trf[, 2:ncol(mob_ntl_trf)],
  function(x) {round(x, 2)}
)


# export data -------------------------------------------------------------

# latest observation in most recent data
mob_idn_raw_last <- mob_idn_raw %>% 
  select(date) %>% 
  dplyr::filter(date == last(date), !duplicated(date)) %>% 
  deframe()

# latest observation in csv
mob_idn_raw_csv_last <- read_csv("data/ier_mobility-idn_tidy.csv") %>% 
  select(date) %>% 
  dplyr::filter(date == last(date), !duplicated(date)) %>% 
  deframe()

# data
## raw indonesian observations
mob_idn_csv <- mob_idn_raw %>% 
  pivot_longer(
    4:ncol(.),
    names_to = "place_category",
    values_to = "change_from_baseline"
  )

## national mobility
mob_ntl_trf_tidy <- mob_ntl_trf %>% 
  pivot_longer(
    2:ncol(.),
    names_to = "place_category",
    values_to = "change_from_baseline"
  )

# write csv
## raw indonesian observations
if (file.exists("data/ier_mobility-idn_tidy.csv") == F) {
  
  write_csv(mob_idn_csv, "data/ier_mobility-idn_tidy.csv")
  
  message("The raw mobility dataset has been exported")
  
} else if (mob_idn_raw_last != mob_idn_raw_csv_last) {
  
  write_csv(mob_idn_csv, "data/ier_mobility-idn_tidy.csv")
  
  message("The raw mobility dataset has been updated")
  
} else {
  
  message("The raw mobility dataset is up to date")
  
}

## national mobility
if (file.exists("data/ier_mobility-ntl_cleaned.csv") == F) {
  
  write_csv(mob_ntl_trf_tidy, "data/ier_mobility-ntl_cleaned.csv")
  
  message("The national mobility dataset has been exported")
  
} else if (mob_idn_raw_last != mob_idn_raw_csv_last) {
  
  write_csv(mob_ntl_trf_tidy, "data/ier_mobility-ntl_cleaned.csv")
  
  message("The national mobility dataset has been updated")
  
} else {
  
  message("The national mobility dataset is up to date")
  
}


# plot --------------------------------------------------------------------

# covid text annotation
anno_text_covid <- list(
  text = "<b>COVID-19<br>pandemic</b>",
  font = list(size = 7, color = "#90A4AE"),
  align = "left",
  bgcolor = "white",
  showarrow = F,
  xref = "x",
  x = "2020-03-10",
  xanchor = "left",
  yref = "y",
  y = 45,
  yanchor = "top"
) 

# subtitles
## retail & recreation
anno_sub_retail <- list(
  text = "<b>Retail and<br>recreation</b>",
  font = list(size = 10),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0,
  xanchor = "left",
  yref = "paper",
  y = 1.125,
  yanchor = "top"
)

## grocery & pharma
anno_sub_grocery <- list(
  text = "<b>Grocery and<br>pharmacy</b>",
  font = list(size = 10),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.345,
  xanchor = "left",
  yref = "paper",
  y = 1.125,
  yanchor = "top"
)

## parks
anno_sub_parks <- list(
  text = "<b>Parks</b>",
  font = list(size = 10),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.675,
  xanchor = "left",
  yref = "paper",
  y = 1.125,
  yanchor = "top"
)

## transit stations
anno_sub_transit <- list(
  text = "<b>Transit stations</b>",
  font = list(size = 10),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0,
  xanchor = "left",
  yref = "paper",
  y = 0.475,
  yanchor = "top"
)

## workplaces
anno_sub_workplace <- list(
  text = "<b>Workplace</b>",
  font = list(size = 10),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.345,
  xanchor = "left",
  yref = "paper",
  y = 0.475,
  yanchor = "top"
)

## residential
anno_sub_res <- list(
  text = "<b>Residential</b>",
  font = list(size = 10),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.675,
  xanchor = "left",
  yref = "paper",
  y = 0.475,
  yanchor = "top"
)

# variable names
mob_ntl_trf_var <- names(mob_ntl_trf)[2:7]

# plot
plot_mob_ntl <- lapply(
  mob_ntl_trf_var,
  function(x) {
    plot_ly(
      mob_ntl_trf,
      type = "scatter",
      mode = "markers+lines",
      text = x %>% 
        str_to_sentence() %>% 
        str_replace_all("_", " and ") %>% 
        str_replace("Transit and stations", "Transit stations")
    ) %>% 
      add_markers(
        x = ~date, 
        y = as.formula(str_c("~", x)), 
        name = "Headline",
        showlegend = ifelse(x == "retail_recreation", T, F),
        legendgroup = "group1",
        hovertemplate = "<b>Category: %{text}</b><br><br>Headline: %{y} percent<br>Date: %{x}<extra></extra>",
        marker = list(size = 2.5, color = "#CFD8DC")
      ) %>%
      add_lines(
        x = ~date,
        y = as.formula(str_c("~", x, "_avg")),
        name = "7-day moving average",
        showlegend = ifelse(x == "retail_recreation", T, F),
        legendgroup = "group2",
        hovertemplate = "<b>Category: %{text}</b><br><br>7-day moving average: %{y} percent<br>Date: %{x}<extra></extra>",
        line = list(width = 2, color = "#1d81a2")
      ) %>%
      plotly::layout(
        xaxis = list (
          title = NA,
          autorange = F,
          range = c(
            as.character(first(mob_ntl_trf$date) - 14),
            as.character(last(mob_ntl_trf$date) + 14)
          ),
          fixedrange = T,
          ticks = "outside",
          automargin = T,
          tickfont = list(size = 8),
          tickformat = "%b<br>'%y",
          hoverformat = "%b %d, %Y",
          showline = T,
          showgrid = F
        ),
        yaxis = list(
          title = NA,
          type = "linear",
          autorange = F,
          range = c(-100, 55),
          fixedrange = T,
          dtick = 25,
          tickfont = list(size = 8),
          showline = F,
          showgrid = T,
          gridcolor = "#CFD8DC",
          zerolinecolor = "#ff856c",
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
            y1 = 50,
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
sm_mob_ntl <- subplot(
  plot_mob_ntl, 
  nrows = 2,
  margin = c(0.0125, 0.0125, 0.1, 0.1),
  shareX = F,
  shareY = T
) %>% 
  plotly::layout(
    annotations = list(
      anno_text_covid,
      anno_sub_retail,
      anno_sub_grocery,
      anno_sub_parks,
      anno_sub_transit,
      anno_sub_workplace,
      anno_sub_res
    ),
    showlegend = T,
    legend = list(
      orientation = "h",
      itemsizing = "constant",
      x = 0,
      xanchor = "left",
      y = 1.25,
      yanchor = "top"
    ),
    margin = list(l = 0, r = 0, t = 15, b = 0),
    clickmode = "none"
  )


# export chart ------------------------------------------------------------

if (mob_idn_raw_last != mob_idn_raw_csv_last) {
  
  # data
  mob_ntl_headline <- mob_ntl_trf %>% 
    select(date, !ends_with("_avg")) %>% 
    pivot_longer(
      2:ncol(.),
      names_to = "place_categories",
      values_to = "change_from_baseline"
    ) %>% 
    mutate(place_categories = as_factor(place_categories))
  
  mob_ntl_avg <- mob_ntl_trf %>% 
    select(date, ends_with("_avg")) %>% 
    pivot_longer(
      2:ncol(.),
      names_to = "place_categories",
      values_to = "avg_change_from_baseline"
    ) %>% 
    mutate(
      place_categories = as_factor(place_categories),
      place_categories = str_remove(place_categories, "_avg")
    )
  
  mob_ntl_trf_tidy_02 <- mob_ntl_headline %>% 
    left_join(mob_ntl_avg, by = c("date", "place_categories"))
  
  # annotations
  ## covid-19
  anno_text_covid_ggplot <- tibble(
    x = ymd("2020-03-15"),
    y = -75,
    label = "COVID-19\npandemic \u2192",
    place_categories = "grocery_pharmacy"
  )
  
  # panel labels
  labs_mob_ntl <- c(
    grocery_pharmacy = "Grocery & pharmacy",
    parks = "Parks",
    residential = "Residential*",
    retail_recreation = "Retail & recreation",
    transit_stations = "Transit stations",
    workplaces = "Workplaces"
  )
  
  # legends
  anno_text_avg <- tibble(
    x = ymd("2020-08-01"),
    y = -0.43,
    label = "7-day moving\naverage",
    place_categories = "grocery_pharmacy"
  )
  
  # plot
  ggplot(mob_ntl_trf_tidy_02, aes(date)) +
    geom_hline(yintercept = 0, color = "#ff856c") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_point(
      aes(y = change_from_baseline), 
      pch = 21,
      color = "white",
      fill = "#CFD8DC",
      size = 1, 
      alpha = 0.5
    ) +
    geom_line(
      aes(y = avg_change_from_baseline), 
      color = "#1d81a2",
      lwd = 0.75
    ) + 
    scale_x_date(
      breaks = seq(first(mob_ntl_trf_tidy_02$date), last(mob_ntl_trf_tidy_02$date), "3 month"),
      date_labels = "%b\n'%y"
    ) +
    scale_y_continuous(
      breaks = seq(-100, 50, 25),
      limits = c(-100, 50),
      expand = c(0, 0),
      position = "right"
    ) +
    geom_label(
      data = anno_text_covid_ggplot,
      aes(x = x, y = y, label = label),
      color = "#90A4AE",
      hjust = 0,
      size = 2,
      fill = "white",
      label.padding = unit(0.1, "lines"),
      label.r = unit(0, "lines"),
      label.size = 0
    ) +
    geom_text_repel(
      data = anno_text_avg,
      aes(x = x, y = y, label = label),
      hjust = 0,
      size = 2,
      color = "#1d81a2",
      nudge_x = 50,
      nudge_y = -35,
      fontface = "bold",
      segment.curvature = -0.25,
      segment.ncp = 3
    ) +
    labs(
      title = "Community mobility in Indonesia",
      subtitle = "Number of visitors, by place category\n(percent change from baseline in Jan-Feb 2020 period)",
      caption = str_c(
        "*Length of stay\n",
        "Last updated on ",
        format(Sys.Date(), "%B %d, %Y"),
        "\n\nChart: Dzulfiqar Fathur Rahman | Source: Google"
      )
    ) +
    facet_wrap(
      ~ place_categories, 
      nrow = 2,
      scales = "free_x",
      labeller = labeller(place_categories = labs_mob_ntl)
    ) +
    theme(
      text = element_text(size = 10),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(color = "black"),
      legend.direction = "horizontal",
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#CFD8DC"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.spacing = unit(1.5, "lines"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 20)),
      plot.caption = element_text(
        color = "#757575",
        hjust = 0,
        margin = margin(t = 20)
      ),
      strip.background = element_rect(fill = "white", color = NULL),
      strip.text = element_text(hjust = 0, vjust = 1, margin = margin(b = 10))
    ) +
    ggsave(
      "fig/ier_mobility-ntl_plot.png",
      width = 7,
      height = 5,
      dpi = 300
    )
  
  # add logo
  ier_logo <- image_read("images/ier_hexsticker_small.png")
  
  # add base plot
  base_plot <- image_read("fig/ier_mobility-ntl_plot.png")
  
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
    image_write("fig/ier_mobility-ntl_plot.png")
  
  # message
  message("The national mobility chart has been updated")
  
} else {
  
  message("The national mobility chart is up to date")
  
}


# preview -----------------------------------------------------------------

if (mob_idn_raw_last != mob_idn_raw_csv_last) {
  
  # plot
  ggplot(mob_ntl_trf_tidy_02, aes(date)) +
    geom_hline(yintercept = 0, color = "#ff856c") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_point(
      aes(y = change_from_baseline), 
      pch = 16,
      color = "#CFD8DC",
      size = 1, 
      alpha = 0.25
    ) +
    geom_line(
      aes(y = avg_change_from_baseline), 
      color = "#1d81a2",
      lwd = 0.75
    ) + 
    scale_x_date(
      breaks = seq(first(mob_ntl_trf_tidy_02$date), last(mob_ntl_trf_tidy_02$date), "3 month"),
      date_labels = "%b\n'%y"
    ) +
    scale_y_continuous(
      breaks = seq(-100, 50, 25),
      limits = c(-100, 50),
      expand = c(0, 0),
      position = "right"
    ) +
    facet_wrap(
      ~ place_categories, 
      nrow = 2,
      scales = "free_x",
      labeller = labeller(place_categories = labs_mob_ntl)
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#263238", color = NA),
      plot.margin = margin(t = 50, r = 50, b = 50, l = 50),
      strip.text = element_blank()
    ) +
    ggsave(
      "fig/ier_mobility-ntl_void_plot.png",
      width = 13.3,
      height = 6.6,
      dpi = 300
    )
  
  # message
  message("The national mobility preview chart has been updated")
  
} else {
  
  message("The national mobility preview chart is up to date")
  
}
