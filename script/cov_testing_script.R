# COVID-19 TEST

# Last updated: 2021-02-04
# Content: test, positive rate


# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(jsonlite)
library(plotly)


# data --------------------------------------------------------------------


# testing data ---------------------------------------------------------------

# store url for owid's raw csv
owid_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv"

# read owid's testing data
owid_raw <- read_csv(owid_url)

# tidy owid's testing data
owid_cleaned <- owid_raw %>% 
  dplyr::filter(`ISO code` == "IDN") %>% 
  select(Date, 7:8) %>% 
  rename(date = 1, test_new = 2, test_total = 3)


# cases data --------------------------------------------------------------

# store govt api base url
bnpb_url <- "https://data.covid19.go.id/public/api/update.json"

# import the data
cases_list <- fromJSON(
  bnpb_url,
  simplifyDataFrame = T,
  flatten = T
)

# store data in R
cases_raw <- cases_list$update$harian

# correct date format
cases_raw$key_as_string <- parse_datetime(cases_raw$key_as_string) %>% 
  str_replace(" UTC", "") %>% 
  ymd()

# rename variables
cases_raw <- cases_raw %>% 
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

# tidy govt's cases data
cases_cleaned <- cases_raw %>% 
  select(date, case_daily, case_total)


# merge owid, govt data ---------------------------------------------------

case_test_cln <- cases_cleaned %>% 
  left_join(owid_cleaned, by = "date")

# calculate positive rate
case_test_pos <- case_test_cln %>% 
  dplyr::filter(!is.na(test_new)) %>% 
  mutate(
    pos_rate_daily = round(case_daily / test_new * 100, 2),
    pos_rate_cum = round(case_total / test_total * 100, 2)
  )

# divide test by million
case_test_pos$test_total <- round(case_test_pos$test_total / 1000000, 2)


# plot --------------------------------------------------------------------

# test plot
test_plot <- plot_ly(
  showlegend = F,
  height = 300
) %>% 
  add_trace(
    data = case_test_pos,
    x = ~date,
    y = ~test_total,
    hovertemplate = str_c(
      "%{y} million",
      "<br>",
      "%{x}",
      "<extra></extra>"
    ),
    type = "scatter",
    mode = "lines",
    line = list(color = "#1d81a2", width = 3)
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      fixedrange = T,
      range = c("2020-02-15", as.character(last(case_test_pos$date) + 14)),
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
      range = c(0, ceiling(max(case_test_pos$test_total)) + 1.5),
      dtick = 2
    ),
    margin = list(
      t = 5,
      b = 0,
      l = 0,
      r = 0
    )
  ) %>% 
  plotly::config(displayModeBar = F)

# positive rate plot
pos_rate_plot <- plot_ly(
  showlegend = F,
  height = 300
) %>% 
  add_trace(
    data = case_test_pos,
    x = ~date,
    y = ~pos_rate_cum,
    hovertemplate = str_c(
      "%{y} percent",
      "<br>",
      "%{x}",
      "<extra></extra>"
    ),
    type = "scatter",
    mode = "lines",
    line = list(color = "#ff5e4b", width = 3)
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      fixedrange = T,
      range = c("2020-02-15", as.character(last(case_test_pos$date) + 14)),
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
      range = c(6, ceiling(max(case_test_pos$pos_rate_cum)) + 3.5),
      dtick = 6
    ),
    margin = list(
      t = 5,
      b = 0,
      l = 0,
      r = 0
    )
  ) %>% 
  plotly::config(displayModeBar = F)
