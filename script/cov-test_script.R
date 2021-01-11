# COVID-19 test plot

# Setup -------------------------------------------------------------------

library(conflicted)
library(tidyverse)
library(lubridate)
library(zoo)
library(plotly)


# Tidy the data -----------------------------------------------------------

# Import
cov_raw <- read_csv(
  "https://opendata.arcgis.com/datasets/5d4f1b90101b418997b82ce7d532a770_0.csv",
  na = ""
)

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

test_raw$date[261] <- "2020-11-18"

test_raw <- rbind(test_raw, test_correction_Nov19) %>% 
  arrange(date)

# Correct data for Dec 20-21
test_corr_Dec20 <- test_raw %>% 
  slice(293) %>% 
  summarize(
    date = ymd("2020-12-20"),
    Jumlah_Kasus_Kumulatif = Jumlah_Kasus_Kumulatif - 6848,
    Jumlah_Kasus_Diperiksa = Jumlah_Kasus_Diperiksa - 24753
  )

test_raw[293:294, 1] <- c(ymd("2020-12-21"), ymd("2020-12-22"))

test_raw <- test_raw %>% 
  rbind(test_corr_Dec20) %>% 
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
    line = list(color = "#1d81a2", width = 2.5)
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
      range = c(0, max(test_tidy$tests) + 1.5),
      dtick = 1
    ),
    annotations = list(
      list(
        x = 0,
        xref = "paper",
        xanchor = "left",
        xshift = 0,
        y = 1.15,
        yref = "paper",
        yanchor = "top",
        yshift = 0,
        text = "Cumulative tests<br>(million)",
        font = list(size = 12),
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
    line = list(color = "#ff5e4b", width = 2.5)
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
      range = c(0, max(test_tidy$pos_rate) + 6),
      dtick = 7
    ),
    annotations = list(
      list(
        x = 0,
        xref = "paper",
        xanchor = "left",
        xshift = 0,
        y = 1.15,
        yref = "paper",
        yanchor = "top",
        yshift = 0,
        text = "Share of cumulative positive tests<br>(percent)",
        font = list(size = 12),
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

