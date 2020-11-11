# Retail sales index plot


# Setup -------------------------------------------------------------------

library(conflicted)
library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)


# Tidy the data -----------------------------------------------------------

# Import
SPE_raw <- read_excel("Data/BI_SPE_raw.xlsx", 
                      sheet = "Tabel 1",
                      skip = 3,
) 

# Rename indicator variable
names(SPE_raw)[1] <- "Indices"

# Remove columns, rows containing missing values
SPE_raw <- SPE_raw %>% 
  dplyr::filter(!is.na(Indices), Indices == "INDEKS TOTAL") %>% 
  select_if(function(x) {!all(is.na(x))}) %>% 
  dplyr::select(-last(names(SPE_raw)))

# Rename date columns
SPE_col_names <- seq(ymd("2012-01-01"), ymd("2020-10-01"), by = "month")
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


# Create the plot ---------------------------------------------------------

# Byline, source annotations
byline_source_BI <- list(
  x = 0,
  xref = "paper",
  xanchor = "left",
  xshift = 0,
  y = -0.3,
  yref = "paper",
  yshift = 0,
  text = "Chart: @dzulfiqarfr | Source: Bank Indonesia",
  font = list(color = "darkgrey"),
  showarrow = F
)

# Footnote
footnote <- list(
  x = 0,
  xref = "paper",
  xshift = 0,
  y = -0.20,
  yref = "paper",
  yshift = 0,
  text = "*October figure is a forecast",
  font = list(size = 10, color = "darkgrey"),
  showarrow = F
)

# Plot
SPE_plot <- plot_ly(
  SPE_tidy, 
  x = ~Year, 
  y = ~Retail_sales_i,
  hovertemplate = "%{y}<br>%{x}<extra></extra>",
  width = 700,
  height = 400
) %>% 
  add_lines(colors = "#1d81a2") %>% 
  plotly::layout(
    title = list(
      text = str_c(
        "<b>Stalled rebound looms in October</b>",
        "<br>",
        "<sup>",
        "Retail sales index*, January 2010 = 100",
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
      dtick = "M12",
      nticks = 6,
      tickangle = 0,
      ticks = "outside",
      hoverformat = "%b '%y"
    ),
    yaxis = list(
      side = "right",
      title = NA,
      type = "linear",
      showgrid = T,
      gridcolor = "lightgrey",
      fixedrange = T,
      autorange = F,
      range = c(50, 251)
    ),
    annotations = list(
      byline_source_BI,
      footnote),
    margin = list(
      t = 75,
      l = 0,
      r = 0,
      b = 75
    )
  ) %>% 
  config(displayModeBar = F)

