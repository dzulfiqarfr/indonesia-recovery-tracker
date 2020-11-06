# Economic growth plot


# Setup ----------------------------------------------------------------

library(conflicted)
library(tidyverse)
library(lubridate)
library(plotly)


# Tidy the data -----------------------------------------------------------

# Import
growth_raw <- read.csv("Data/BPS_growth-rate_raw.csv",
                       skip = 2, 
                       na.strings = c("-", "")
)

# Rename indicator variable
names(growth_raw)[1] <- "Comp"

# Remove columns, rows containing missing values
growth_raw <- growth_raw %>% 
  dplyr::filter(!is.na(Comp), Comp != "Diskrepansi Statistik") %>% 
  select_if(function(x) {!all(is.na(x))})

# Create a sequence of quarterly date series for column names
seq10_20 <- seq(ymd("2010-01-01"), ymd("2020-07-01"), by = "quarter")

GDP_col_names <- seq10_20 %>%
  tibble() %>% 
  mutate(
    yr = year(seq10_20),
    mo = month(seq10_20)
  ) %>% 
  arrange(desc(yr), mo) %>% 
  rename(date = 1)

# Rename date columns
names(growth_raw)[2:ncol(growth_raw)] <- as.character(GDP_col_names$date)

# Rename components
growth_comp_id <- c(
  "1. Pengeluaran Konsumsi Rumahtangga",
  "3. Pengeluaran Konsumsi Pemerintah",
  "4. Pembentukan Modal Tetap Domestik Bruto",
  "7. Dikurangi Impor Barang dan Jasa",
  "8. PRODUK DOMESTIK BRUTO"
)

growth_comp_en <- c(
  "Household expenditure",
  "Government expenditure",
  "Gross fixed capital formation",
  "Net export",
  "Gross domestic product"
)

growth_raw$Comp[growth_raw$Comp %in% growth_comp_id] <- growth_comp_en

# Correct data types
growth_raw[, 2:ncol(growth_raw)] <- lapply(
  growth_raw[, 2:ncol(growth_raw)],
  as.numeric
)

# Reshape data
growth_tidy <- growth_raw %>%
  dplyr::filter(Comp %in% growth_comp_en) %>% 
  pivot_longer(
    2:ncol(growth_raw),
    names_to = "Year", values_to = "Growth"
  ) %>% 
  arrange(ymd(Year))

# Correct data types
growth_tidy$Year <- ymd(growth_tidy$Year)

# Replace time values with quarters
growth_tidy$Year <- growth_tidy$Year %>%
  str_replace_all(
    c(
      "-01-01" = " Q1",
      "-04-01" = " Q2",
      "-07-01" = " Q3",
      "-10-01" = " Q4"
    )
  )


# Create the plot -----------------------------------------------------------

# Modebar buttons to remove
remove <- c(
  "zoom2d",
  "pan2d", 
  "select2d", 
  "lasso2d",
  "zoomIn2d", 
  "zoomOut2d",
  "toggleSpikelines",
  "autoScale2d",
  "toImage",
  "resetScale2d"
)

# Byline, source annotations
byline_source_BPS <- list(
  x = 0,
  xref = "paper",
  xanchor = "left",
  xshift = 0,
  y = -0.25,
  yref = "paper",
  yshift = 0,
  text = "Chart: @dzulfiqarfr | Source: Statsitics Indonesia (BPS)",
  font = list(color = "darkgrey"),
  showarrow = F
)

# Plot
growth_plot <- plot_ly(
  growth_tidy[growth_tidy$Comp == "Gross domestic product", ],
  x = ~Year,
  y = ~Growth,
  hovertemplate = "%{y} percent<br>%{x}<extra></extra>",
  width = 700,
  height = 400
) %>%
  add_lines(
    colors = "#1d81a2"
  ) %>% 
  plotly::layout(
    title = list(
      text = str_c(
        "<b>Growth rate</b>",
        "<br>",
        "<sup>",
        "GDP in constant 2010 prices (percent change from a year earlier)",
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
      tickmode = "array",
      nticks = 8,
      tickvals = c(
        "2010 Q3",
        "2012 Q3",
        "2014 Q3",
        "2016 Q3", 
        "2018 Q3",
        "2020 Q3"
      ),
      ticks = "outside",
      tickangle = 0
    ),
    yaxis = list(
      side = "right",
      title = NA,
      type = "linear",
      showgrid = T,
      gridcolor = "lightgrey",
      fixedrange = T,
      autorange = F,
      range = c(-12, 9),
      dtick = 4,
      zerolinecolor = "red"
    ),
    annotations = list(
      byline_source_BPS
    ),
    margin = list(
      t = 75,
      l = 0,
      r = 0,
      b = 75
    )
  ) %>% 
  config(
    displayModeBar = F
  )
