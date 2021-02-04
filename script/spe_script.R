# Retail sales index plot


# Date --------------------------------------------------------------------

spe_latest_update <- "2020-12-01"


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

# Reshape data
seq_2019 <- seq(
  ymd("2020-01-01"),
  ymd("2020-12-01"),
  by = "month"
)

month <- format(seq_2019, "%b")

SPE_res <- SPE_tidy %>% 
  slice(49:nrow(SPE_tidy)) %>% 
  mutate(category = rep(
    2016:2020, 
    each = 12, 
    length.out = nrow(.)
  )
  )

SPE_res$Year <- rep(
  month, 
  times = 5, 
  length.out = nrow(SPE_res)
)

SPE_wide <- SPE_res %>% 
  pivot_wider(
    names_from = category, 
    values_from = Retail_sales_i
  ) %>% 
  rowwise() %>% 
  mutate(four_year_avg = mean(`2016`:`2019`))

names(SPE_wide)[1] <- "month"


# Create the plot ---------------------------------------------------------

#Annotations
label_2020 <- list(
  x = "Sep",
  y = 185,
  text = "<b>2020</b>",
  font = list(size = 10, color = "#ff5e4b"),
  showarrow = F
)

label_4y_avg <- list(
  x = "Oct",
  xref = "x",
  xshift = 0,
  y = 206.43,
  ay = 230,
  ayref = "y",
  text = "Four-year average",
  font = list(size = 10, color = "#607D8B"),
  arrowhead = 0,
  arrowwidth = 1,
  arrowcolor = "#607D8B",
  bgcolor = "white"
)

label_2016_2019 <- list(
  x = "Jul",
  xshift = 10,
  y = 235,
  text = "2016-2019",
  font = list (size = 10, color = "darkgrey"),
  showarrow = F,
  bgcolor = "white"
)

# Plot
SPE_plot <- plot_ly(
  SPE_wide,
  type = "scatter", 
  mode = "lines",
  line = list(width = 3),
  height = 300
) %>% 
  add_trace(
    x = ~month,
    y = ~`2016`, 
    name = "2016",
    line = list(color = "#CFD8DC"),
    hovertemplate = "%{y}<br>%{x}, 2016<extra></extra>"
  ) %>% 
  add_trace(
    x = ~month,
    y = ~`2017`, 
    name = "2017",
    line = list(color = "#CFD8DC"),
    hovertemplate = "%{y}<br>%{x}, 2017<extra></extra>"
  ) %>% 
  add_trace(
    x = ~month,
    y = ~`2018`, 
    name = "2018",
    line = list(color = "#CFD8DC"),
    hovertemplate = "%{y}<br>%{x}, 2018<extra></extra>"
  ) %>% 
  add_trace(
    x = ~month,
    y = ~`2019`,
    name = "2019",
    line = list(color = "#CFD8DC"),
    hovertemplate = "%{y}<br>%{x}, 2019<extra></extra>"
  ) %>% 
  add_trace(
    x = ~month,
    y = ~four_year_avg, 
    name = "Four year average",
    line = list(color = "#607D8B"),
    hovertemplate = "%{y}<br>%{x}<br>Four-year average<extra></extra>"
  ) %>% 
  add_trace(
    x = ~month,
    y = ~`2020`,
    name = "2020",
    line = list(color = "#ff5e4b"),
    hovertemplate = "%{y}<br>%{x}, 2020<extra></extra>"
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      fixedrange = T,
      showgrid = F,
      showline = T,
      tickmode = "auto",
      ticks = "outside",
      tickmode = "array",
      automargin = T,
      categoryorder = "array",
      categoryarray = SPE_wide$month
    ),
    yaxis = list(
      side = "right",
      title = NA,
      type = "linear",
      showgrid = T,
      gridcolor = "#CFD8DC",
      fixedrange = T,
      autorange = F,
      range = c(160, 270),
      dtick = 20
    ),
    annotations = list(
      label_2020,
      label_4y_avg,
      label_2016_2019
    ),
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