# COVID-19 case, death plots

# Setup -------------------------------------------------------------------

library(tidyverse)
library(jsonlite)
library(lubridate)
library(zoo)
library(plotly)


# Tidy the data -----------------------------------------------------------

# Cases, deaths data ------------------------------------------------------

# Store the API URL
url <- "https://data.covid19.go.id/public/api/update.json"

# Import the data
response_raw <- fromJSON(
  url,
  simplifyDataFrame = T,
  flatten = T
)

# Store data in R
cov_raw <- response_raw$update$harian

# Correct date format
cov_raw$key_as_string <- parse_datetime(cov_raw$key_as_string) %>% 
  str_replace(" UTC", "") %>% 
  ymd()

# Rename variables
cov_tidy <- cov_raw %>% 
  dplyr::rename(
    date = key_as_string,
    death_daily = jumlah_meninggal.value,
    recovery_daily = jumlah_sembuh.value,
    case_daily = jumlah_positif.value,
    hospitalized_daily = jumlah_dirawat.value,
    case_total = jumlah_positif_kum.value,
    recovery_total = jumlah_sembuh_kum.value,
    death_total = jumlah_meninggal_kum.value,
    hospitalized_total = jumlah_dirawat_kum.value
  )

# Create dataframe on case and death, calculate moving average
case_death <- cov_tidy %>% 
  select(date, case_daily, death_daily) %>% 
  mutate(
    case_daily_ma = rollmean(
      case_daily, 
      k = 7, 
      fill = NA, 
      align = "right"
    ),
    death_daily_ma = rollmean(
      death_daily, 
      k = 7, 
      fill = NA, 
      align = "right"
    )
  )

# Round moving averages to two decimal places
case_death[, 4:ncol(case_death)] <- lapply(
  case_death[, 4:ncol(case_death)],
  function(x) {
    round(x, digits = 2)
  }
)


# Create the plot ---------------------------------------------------------

# Cases plot
case_plot <- plot_ly(
  data = case_death,
  showlegend = F,
  height = 300
) %>% 
  add_bars(
    x = ~date,
    y = ~case_daily,
    hovertemplate = str_c(
      "%{y}",
      "<br>",
      "%{x}",
      "<extra></extra>"
    ),
    marker = list(color = "#74c7eb")
  ) %>% 
  add_trace(
    x = ~date,
    y = ~case_daily_ma,
    type = "scatter",
    mode = "lines",
    hovertemplate = str_c(
      "7-day moving averages",
      "<br>",
      "%{y}",
      "<br>",
      "%{x}",
      "<extra></extra>"
    ),
    line = list(color = "#1d81a2", width = 3)
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      fixedrange = T,
      range = c(
        as.character(first(case_death$date) - 14),
        as.character(last(case_death$date) + 14)
      ),
      showgrid = F,
      showline = T,
      ticks = "outside",
      hoverformat = "%b %d, %Y",
      tickformat = "%b"
    ),
    yaxis = list(
      side = "right",
      title = NA,
      gridcolor = "#CFD8DC",
      fixedrange = T,
      range = c(0, round(max(case_death$case_daily), -3) + 100),
      dtick = 5000,
      tickformat = ","
    ),
    annotations = list(
      list(
        x = "2020-09-07",
        ax = -25,
        xref = "x",
        xanchor = "right",
        xshift = 0,
        y = 3170.4,
        ay = 3170.4,
        ayref = "y",
        yref = "y",
        yshift = 0,
        text = "7-day moving average",
        font = list(size = 10),
        showarrow = T,
        arrowhead = 0,
        arrowwidth = 1
      )
    ),
    margin = list(
      t = 5,
      b = 0,
      l = 0,
      r = 0
    ),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)

# deaths plot
death_plot <- plot_ly(
  data = case_death,
  showlegend = F,
  height = 300
) %>% 
  add_bars(
    x = ~date,
    y = ~death_daily,
    hovertemplate = str_c(
      "%{y}",
      "<br>",
      "%{x}",
      "<extra></extra>"
    ),
    marker = list(color = "#ffbb9d")
  ) %>% 
  add_trace(
    x = ~date,
    y = ~death_daily_ma,
    type = "scatter",
    mode = "lines",
    hovertemplate = str_c(
      "7-day moving averages",
      "<br>",
      "%{y}",
      "<br>",
      "%{x}",
      "<extra></extra>"
    ),
    line = list(color = "#ee493a", width = 3)
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      fixedrange = T,
      range = c(
        as.character(first(case_death$date) - 14),
        as.character(last(case_death$date) + 14)
      ),
      showgrid = F,
      showline = T,
      ticks = "outside",
      hoverformat = "%b %d, %Y",
      tickformat = "%b"
    ),
    yaxis = list(
      side = "right",
      title = NA,
      gridcolor = "#CFD8DC",
      fixedrange = T,
      range = c(0, round(max(case_death$death_daily), -2) + 5),
      dtick = 100
    ),
    annotations = list(
      list(
        x = "2020-07-22",
        ax = -25,
        xref = "x",
        xanchor = "right",
        xshift = 0,
        y = 94.6,
        ay = 94.6,
        ayref = "y",
        yref = "y",
        yshift = 0,
        text = "7-day moving average",
        font = list(size = 10),
        showarrow = T,
        arrowhead = 0,
        arrowwidth = 1
      )
    ),
    margin = list(
      t = 5,
      b = 0,
      l = 0,
      r = 0
    ),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)