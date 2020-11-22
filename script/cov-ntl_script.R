# COVID-19 plot

# Setup -------------------------------------------------------------------

library(conflicted)
library(tidyverse)
library(lubridate)
library(zoo)
library(plotly)


# Tidy the data -----------------------------------------------------------

# Import
cov_raw <- read.csv("https://opendata.arcgis.com/datasets/6f0a390ef3dc491c99c41526d90eff7a_0.csv")

# Cases, deaths data ------------------------------------------------------

# Select variables, re-write dates
cases_deaths_raw <- cov_raw %>% 
  select(
    Tanggal,
    Jumlah_Kasus_Baru_per_Hari,
    Jumlah_Kasus_Meninggal_per_Hari,
  ) %>% 
  separate(Tanggal, into = c("date", "rm"), sep = " ") %>% 
  select(-rm) %>% 
  arrange(date) %>% 
  na.omit()

# Correct date format
cases_deaths_raw$date <- cases_deaths_raw$date %>% 
  format.Date("%Y-%m-%d") %>% 
  ymd()

# Correct data for Nov 18-19
case_death_correction_Nov19 <- cases_deaths_raw %>% 
  slice(262) %>% 
  summarize(
    date = "2020-11-19",
    Jumlah_Kasus_Baru_per_Hari = Jumlah_Kasus_Baru_per_Hari - 4265,
    Jumlah_Kasus_Meninggal_per_Hari = Jumlah_Kasus_Meninggal_per_Hari - 110
  )

cases_deaths_raw$Jumlah_Kasus_Baru_per_Hari[262] <- 4265

cases_deaths_raw$Jumlah_Kasus_Meninggal_per_Hari[262] <- 110

case_death_date_corr <- seq(
  ymd("2020-11-20"), 
  ymd(format(Sys.time(), "%Y-%m-%d")),
  by = "1 day"
)

cases_deaths_raw$date[
  263:nrow(cases_deaths_raw)
] <- case_death_date_corr

cases_deaths_raw <- rbind(
  cases_deaths_raw, 
  case_death_correction_Nov19
) %>% 
  arrange(date)

# Calculate moving averages
cases_deaths_tidy <- cases_deaths_raw %>% 
  mutate(
    daily_case_7d = rollmean(
      Jumlah_Kasus_Baru_per_Hari, k = 7, fill = NA, align = "right"
    ),
    daily_deaths_7d = rollmean(
      Jumlah_Kasus_Meninggal_per_Hari, k = 7, fill = NA, align = "right"
    ) 
  ) %>% 
  select(
   date,
   Jumlah_Kasus_Baru_per_Hari,
   daily_case_7d,
   Jumlah_Kasus_Meninggal_per_Hari,
   daily_deaths_7d
  )

# Rename variables
names(cases_deaths_tidy)[2] <- "daily_new_cases"

names(cases_deaths_tidy)[4] <- "daily_new_deaths"

# Round to one decimal place
cases_deaths_tidy$daily_case_7d <- round(
  cases_deaths_tidy$daily_case_7d, digits = 1
)

cases_deaths_tidy$daily_deaths_7d <- round(
  cases_deaths_tidy$daily_deaths_7d, digits = 1
)


# Tests data --------------------------------------------------------------

# Select variables, re-write dates 
test_raw <- cov_raw %>% 
  select(
    Tanggal,
    Jumlah_Kasus_Kumulatif,
    Jumlah_Kasus_Diperiksa
  ) %>% 
  separate(Tanggal, into = c("date", "rm"), sep = " ") %>% 
  select(-rm) %>% 
  arrange(date) %>% 
  na.omit()

# Correct date format
test_raw$date <- test_raw$date %>% 
  format.Date("%Y-%m-%d") %>% 
  ymd()

# Sum tests between May 21 and May 22, remove May 21 observation
merge_test <- test_raw %>% 
  slice(81:82) %>% 
  summarize(
    temp = Jumlah_Kasus_Diperiksa[1] + Jumlah_Kasus_Diperiksa[2]
  )
  
test_raw$Jumlah_Kasus_Diperiksa[82] <- merge_test$temp

test_raw <- test_raw %>% slice(-81)

# Correct data for Nov 18-19
test_correction_Nov19 <- test_raw %>% 
  slice(262) %>% 
  summarize(
    date = ymd("2020-11-19"),
    Jumlah_Kasus_Kumulatif = Jumlah_Kasus_Kumulatif - 4792,
    Jumlah_Kasus_Diperiksa = Jumlah_Kasus_Diperiksa - 39204
  )

test_raw$Jumlah_Kasus_Kumulatif[261] <- 478720

test_raw$Jumlah_Kasus_Diperiksa[261] <- 3415613

test_raw$date[262:nrow(test_raw)] <- case_death_date_corr

test_raw <- rbind(test_raw, test_correction_Nov19) %>% 
  arrange(date)

# Calculate positive rate
test_tidy <- test_raw %>% 
  mutate(
    pos_rate = Jumlah_Kasus_Kumulatif/Jumlah_Kasus_Diperiksa * 100
  )

# Round positive rate to one decimal place
test_tidy$pos_rate <- round(test_tidy$pos_rate, digits = 1)

# Round tests to two decimal places, in millions
test_tidy$Jumlah_Kasus_Diperiksa <- round(
  test_tidy$Jumlah_Kasus_Diperiksa/1000000, digits = 2
)

# Rename variables
names(test_tidy)[2] <- "confirmed_cases"

names(test_tidy)[3] <- "tests"


# Create the plot ---------------------------------------------------------

# Byline, source annotations
byline_source_cov <- list(
  x = 0,
  xref = "paper",
  xanchor = "left",
  xshift = 0,
  y = -0.15,
  yanchor = "top",
  yref = "paper",
  yshift = 0,
  text = "Chart: @dzulfiqarfr | Source: Indonesian COVID-19 task force",
  font = list(size = 10, color = "darkgrey"),
  showarrow = F
)

# Timestamp
cov_timestamp <- list(
  x = 0,
  xref = "paper",
  xanchor = "left",
  xshift = 0,
  y = -0.2,
  yref = "paper",
  yanchor = "top",
  yshift = 0,
  text = str_c("Last updated on ",
               format(Sys.time(), "%b %d, %Y")
  ),
  font = list(size = 10, color = "darkgrey"),
  showarrow = F
)

# Cases plot
case_plot <- plot_ly(
  data = cases_deaths_tidy,
  showlegend = F
) %>% 
  add_bars(
    x = ~date,
    y = ~daily_new_cases,
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
    y = ~daily_case_7d,
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
    line = list(color = "#1d81a2")
  ) %>% 
  plotly::layout(
    title = list(
      text = str_c(
        "<b>Daily new cases</b>",
        "<br>",
        "<sup>",
        "Number of confirmed cases per day",
        "</sup>"
      ),
      xref = "paper",
      x = 0,
      xanchor = "left",
      yref = "paper",
      y = 2
    ),
    xaxis = list (
      title = NA,
      fixedrange = T,
      showgrid = F,
      showline = T,
      ticks = "outside",
      hoverformat = "%b %d, %Y",
      tickformat = "%b"
    ),
    yaxis = list(
      side = "right",
      title = NA,
      gridcolor = "lightgrey",
      fixedrange = T,
      range = c(0, max(cases_deaths_tidy$daily_new_cases) + 1000),
      dtick = 1500,
      tickformat = ","
    ),
    annotations = list(
      byline_source_cov, 
      cov_timestamp,
      list(
        x = "2020-09-23",
        ax = -25,
        xref = "x",
        xanchor = "right",
        xshift = 0,
        y = 4056.4,
        ay = 4056.4,
        ayref = "y",
        yref = "y",
        yshift = 0,
        text = "7-day moving average",
        font = list(size = 8),
        showarrow = T,
        arrowhead = 0,
        arrowwidth = 1
      )
    ),
    margin = list(
      t = 75,
      b = 75,
      l = 25,
      r = 25
    ),
    autosize = T
  ) %>% 
  config(displayModeBar = F)

# deaths plot
death_plot <- plot_ly(
  data = cases_deaths_tidy,
  showlegend = F
) %>% 
  add_bars(
    x = ~date,
    y = ~daily_new_deaths,
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
    y = ~daily_deaths_7d,
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
    line = list(color = "#ee493a")
  ) %>% 
  plotly::layout(
    title = list(
      text = str_c(
        "<b>Daily new deaths</b>",
        "<br>",
        "<sup>",
        "Number of confirmed deaths per day",
        "</sup>"
      ),
      xref = "paper",
      x = 0,
      xanchor = "left",
      yref = "paper",
      y = 2
    ),
    xaxis = list (
      title = NA,
      fixedrange = T,
      showgrid = F,
      showline = T,
      ticks = "outside",
      hoverformat = "%b %d, %Y",
      tickformat = "%b"
    ),
    yaxis = list(
      side = "right",
      title = NA,
      gridcolor = "lightgrey",
      fixedrange = T,
      range = c(0, max(cases_deaths_tidy$daily_new_deaths) + 25),
      dtick = 60
    ),
    annotations = list(
      byline_source_cov,
      cov_timestamp,
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
        font = list(size = 8),
        showarrow = T,
        arrowhead = 0,
        arrowwidth = 1
      )
    ),
    margin = list(
      t = 75,
      b = 75,
      l = 25,
      r = 25
    ),
    autosize = T
  ) %>% 
  config(displayModeBar = F)

# test plot
test_plot <- plot_ly(showlegend = F) %>% 
  add_trace(
    data = test_tidy,
    x = ~date,
    y = ~tests,
    hovertemplate = str_c(
      "%{y} million",
      "<br>",
      "%{x}",
      "<extra></extra>"
    ),
    type = "scatter",
    mode = "lines",
    line = list(color = "#1d81a2")
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      fixedrange = T,
      showgrid = F,
      showline = T,
      ticks = "outside",
      hoverformat = "%b %d, %Y",
      tickformat = "%b"
    ),
    yaxis = list(
      side = "right",
      title = NA,
      gridcolor = "lightgrey",
      fixedrange = T,
      range = c(0, max(test_tidy$tests) + 1),
      dtick = 1
    ),
    annotations = list(
      list(
        x = 0,
        xref = "paper",
        xanchor = "left",
        xshift = 0,
        y = 1.1,
        yref = "paper",
        yanchor = "top",
        yshift = 0,
        text = "Cumulative tests<br>(million)",
        font = list(size = 10),
        showarrow = F,
        align = "left"
      )
    )
  )

# positive rate plot
pos_rate_plot <- plot_ly(showlegend = F) %>% 
  add_trace(
    data = test_tidy,
    x = ~date,
    y = ~pos_rate,
    hovertemplate = str_c(
      "%{y} percent",
      "<br>",
      "%{x}",
      "<extra></extra>"
    ),
    type = "scatter",
    mode = "lines",
    line = list(color = "#ff5e4b")
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      fixedrange = T,
      showgrid = F,
      showline = T,
      ticks = "outside",
      hoverformat = "%b %d, %Y",
      tickformat = "%b"
    ),
    yaxis = list(
      side = "right",
      title = NA,
      gridcolor = "lightgrey",
      fixedrange = T,
      range = c(0, max(test_tidy$pos_rate) + 5),
      dtick = 7
    ),
    annotations = list(
      list(
        x = 0,
        xref = "paper",
        xanchor = "left",
        xshift = 0,
        y = 1.1,
        yref = "paper",
        yanchor = "top",
        yshift = 0,
        text = "Share of tests returning positive<br>(percent)",
        font = list(size = 10),
        showarrow = F,
        align = "left"
      )
    )
  )

test_pos_plot <- subplot(
  test_plot, 
  pos_rate_plot,
  margin = 0.05
) %>% 
  plotly::layout(
    title = list(
      text = str_c("<b>COVID-19 tests</b>"),
      xref = "paper",
      x = 0,
      xanchor = "left",
      yref = "paper",
      y = 2
    ),
    annotations = list(
      byline_source_cov, 
      cov_timestamp
    ),
    margin = list(
      t = 75,
      b = 75,
      l = 25,
      r = 25
    ),
    autosize = T
  ) %>% 
  config(displayModeBar = F)
