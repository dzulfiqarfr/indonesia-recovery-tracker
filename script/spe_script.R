# Retail sales index plot


# Date --------------------------------------------------------------------

spe_latest_update <- "2021-01-01"


# Setup -------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)


# Tidy the data -----------------------------------------------------------

# Import
SPE_raw <- read_excel(
  "Data/BI_SPE_raw.xlsx", 
  sheet = "Tabel 1",
  skip = 3
) 

# Rename indicator variable
names(SPE_raw)[1] <- "Indices"

# Remove columns, rows containing missing values
SPE_raw <- SPE_raw %>% 
  dplyr::filter(!is.na(Indices), Indices == "INDEKS TOTAL") %>% 
  select_if(function(x) {!all(is.na(x))}) %>% 
  dplyr::select(-last(names(SPE_raw)))

# Rename date columns
SPE_col_names <- seq(
  ymd("2012-01-01"), 
  ymd(spe_latest_update), 
  by = "month"
)

SPE_col_names <- SPE_col_names %>%
  tibble() %>% 
  dplyr::mutate(
    yr = year(SPE_col_names),
    mo = month(SPE_col_names)
  ) %>% 
  dplyr::arrange(yr, mo) %>% 
  dplyr::rename(date = 1)

names(SPE_raw)[2:ncol(SPE_raw)] <- as.character(SPE_col_names$date)

# Rename indices
SPE_raw$Indices[1] <- "Retail_sales_i"

# Reshape data
SPE_tidy <- SPE_raw %>% 
  pivot_longer(
    2:ncol(SPE_raw),
    names_to = "Year",
    values_to = "Retail_sales_i"
  ) %>% 
  dplyr::select(!Indices)

# Correct data types
SPE_tidy$Year <- ymd(SPE_tidy$Year)
SPE_tidy$Retail_sales_i <- as.numeric(SPE_tidy$Retail_sales_i)

# Round SPE to two decimal places
SPE_tidy$Retail_sales_i <- round(SPE_tidy$Retail_sales_i, digits = 2)

# Calculate growth
SPE_growth <- SPE_tidy %>% 
  mutate(mo = month(Year)) %>% 
  group_by(mo) %>% 
  mutate(
    diff = Retail_sales_i - dplyr::lag(Retail_sales_i, 1),
    pct_change_yoy = round(diff / dplyr::lag(Retail_sales_i, 1) * 100, 2)
  ) %>%
  ungroup() %>% 
  select(-mo) %>% 
  dplyr::filter(!is.na(pct_change_yoy))



# Create the plot ---------------------------------------------------------

# covid text annotation
covid_text <- list(
  x = "2020-01-02",
  xanchor = "right",
  y = 25,
  text = "COVID-19<br>pandemic",
  font = list(size = 10, color = "#90A4AE"),
  showarrow = F,
  bgcolor = "white"
) 


# Plot
SPE_plot <- plot_ly(
  SPE_growth,
  type = "scatter", 
  mode = "lines",
  line = list(width = 3),
  height = 300
) %>%
  add_lines(
    x = ~Year, 
    y = ~pct_change_yoy,
    color = I("#1d81a2"),
    hovertemplate = "%{y}<br>%{x}<extra></extra>",
    line = list(width = 3)
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      fixedrange = T,
      autorange = F,
      range = c(
        as.character(first(SPE_growth$Year) - 180), 
        as.character(last(SPE_growth$Year) + 180)
      ),
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
      range = c(-45, 31),
      dtick = 15,
      zerolinecolor = "#ff856c"
    ),
    shapes = list(
      list(
        type = "line",
        line = list(color = "#90A4AE", dash = "dash"),
        xref = "x",
        yref = "y",
        x0 = "2020-03-02",
        x1 = "2020-03-02",
        y0 = -45,
        y1 = 30,
        layer = "below"
      )
    ),
    annotations = list(covid_text),
    margin = list(
      t = 5,
      b = 0,
      l = 0,
      r = 0
    ),
    autosize = T,
    showlegend = F
  ) %>% 
  plotly::config(displayModeBar = F)