# Consumer confidence index plot


# Date --------------------------------------------------------------------

cci_latest_update <- "2021-01-01"


# Setup -------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)


# Tidy the data -----------------------------------------------------------

# Import
CCI_raw <- read_excel(
  "Data/BI_IKK_raw.xlsx",
  sheet = "Tabel 1",
  skip = 5,
  na = c("-", "")
)

# Rename indicator variable
names(CCI_raw)[4] <- "Indices" 

# Remove columns, rows containing missing values
CCI_raw <- CCI_raw %>% 
  dplyr::filter(!is.na(Indices)) %>% 
  select_if(function(x) {!all(is.na(x))}) %>% 
  dplyr::select(-last(names(CCI_raw)))

# Rename date columns
CCI_col_names <- seq(ymd("2012-01-01"), ymd(cci_latest_update), by = "month")

CCI_col_names <- CCI_col_names %>%
  tibble() %>% 
  dplyr::mutate(
    yr = year(CCI_col_names),
    mo = month(CCI_col_names)
  ) %>% 
  dplyr::arrange(yr, mo) %>% 
  dplyr::rename(date = 1)

names(CCI_raw)[2:ncol(CCI_raw)] <- as.character(CCI_col_names$date)

# Rename indices
CCI_raw$Indices[
  CCI_raw$Indices == "Indeks Keyakinan Konsumen (IKK)"
] <- "CCI"

# Reshape data
CCI_tidy <- CCI_raw %>% 
  dplyr::filter(Indices == "CCI") %>% 
  pivot_longer(2:ncol(CCI_raw), names_to = "Year", values_to = "CCI") %>%
  dplyr::select(!Indices)

# Correct data types
CCI_tidy$Year <- ymd(CCI_tidy$Year)
CCI_tidy$CCI <- as.numeric(CCI_tidy$CCI)

# Round CCI to two decimal places
CCI_tidy$CCI <- round(CCI_tidy$CCI, digits =2)


# Create the plot ---------------------------------------------------------

# covid text annotation
covid_text <- list(
  x = "2020-01-02",
  xanchor = "right",
  y = 70,
  text = "COVID-19<br>pandemic",
  font = list(size = 10, color = "#90A4AE"),
  showarrow = F,
  bgcolor = "white"
) 

# Plot
CCI_plot <- plot_ly(
  CCI_tidy,
  height = 300
) %>%
  add_lines(
    x = ~Year, 
    y = ~CCI,
    color = I("#1d81a2"),
    hovertemplate = "%{y}<br>%{x}<extra></extra>",
    line = list(width = 3)
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      fixedrange = T,
      autorange = F,
      range = c("2011-10-01", as.character(last(CCI_tidy$Year) + 180)),
      showgrid = F,
      showline = T,
      tickmode = "auto",
      dtick = "M12",
      nticks = 6,
      ticks = "outside",
      hoverformat = "%b '%y",
      automargin = T
    ),
    yaxis = list(
      side = "right",
      title = NA,
      type = "linear",
      showgrid = T,
      gridcolor = "#CFD8DC",
      fixedrange = T,
      autorange = F,
      range = c(40, 141)
    ),
    shapes = list(
      list(
        type = "line",
        line = list(color = "#ff856c", size = 1),
        xref = "x",
        yref = "y",
        x0 = "2011-10-01",
        x1 = as.character(last(CCI_tidy$Year) + 180),
        y0 = 100,
        y1 = 100,
        layer = "below"
      ),
      list(
        type = "line",
        line = list(color = "#90A4AE", dash = "dash"),
        xref = "x",
        yref = "y",
        x0 = "2020-03-02",
        x1 = "2020-03-02",
        y0 = 40,
        y1 = 140,
        layer = "below"
      )
    ),
    annotations = list(
      list(
        x = "2013-10-01",
        y = 130,
        text = "More optimistic views &#9650;",
        xref = "x",
        yref = "y",
        showarrow = F,
        font = list(size = 10),
        align = "left"
      ),
      list(
        x = "2013-10-01",
        y = 90,
        text = "More pessimistic views &#9660;",
        xref = "x",
        yref = "y",
        showarrow = F,
        font = list(size = 10),
        align = "left"
      ),
      covid_text
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
