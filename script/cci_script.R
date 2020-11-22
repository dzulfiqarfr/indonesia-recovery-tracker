# Consumer confidence index plot


# Setup -------------------------------------------------------------------

library(conflicted)
library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)


# Tidy the data -----------------------------------------------------------

# Import
CCI_raw <- read_excel("Data/BI_IKK_raw.xlsx",
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
CCI_col_names <- seq(ymd("2012-01-01"), ymd("2020-10-01"), by = "month")

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

# Byline, source annotations
byline_source_BI <- list(
  x = 0,
  xref = "paper",
  xanchor = "left",
  xshift = 0,
  y = -0.1,
  yref = "paper",
  yanchor = "top",
  yshift = 0,
  text = "Chart: @dzulfiqarfr | Source: Bank Indonesia",
  font = list(color = "darkgrey"),
  showarrow = F
)

# Plot
CCI_plot <- plot_ly(CCI_tidy) %>%
  add_segments(
    x = "2012-01-01", 
    xend = "2020-10-01",
    y = 100,
    yend = 100,
    color = I("#ff856c"),
    line = list(width = 1),
    hoverinfo = "none",
    showlegend = F
  ) %>% 
  add_lines(
    x = ~Year, 
    y = ~CCI,
    color = I("#1d81a2"),
    hovertemplate = "%{y}<br>%{x}<extra></extra>"
  ) %>% 
  plotly::layout(
    title = list(
      text = str_c(
        "<b>Widespread pessimism returns</b>",
        "<br>",
        "<sup>",
        "Consumer confidence index",
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
      autorange = F,
      range = c("2012-01-01", "2020-10-01"),
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
      gridcolor = "lightgrey",
      fixedrange = T,
      autorange = F,
      range = c(40, 141)
    ),
    annotations = list(
      list(
        x = "2013-09-01",
        y = 130,
        text = "More optimistic views &#9650;",
        xref = "x",
        yref = "y",
        showarrow = F,
        font = list(size = 10, color = "darkgrey"),
        align = "left"
      ),
      list(
        x = "2013-09-01",
        y = 90,
        text = "More pessimistic views &#9660;",
        xref = "x",
        yref = "y",
        showarrow = F,
        font = list(size = 10, color = "darkgrey"),
        align = "left"
      ),
      byline_source_BI
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
