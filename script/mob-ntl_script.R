# National mobility plot


# Setup ----------------------------------------------------------------

library(conflicted)
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(viridis)
library(plotly)
library(crosstalk)

# Tidy the data -----------------------------------------------------------

# Import data 
mob_raw <- read.csv("data/Google_mobility_raw.csv",
                    header = T,
                    na.strings = ""
)

# Rename columns
names(mob_raw)[c(2, 3, 8:14)] <- c(
  "Country",
  "Province",
  "Date",
  "Retail_Recreation",
  "Grocery_Pharmacy",
  "Parks",
  "Transit_stations",
  "Workplaces",
  "Residential"
)

# Correct data types
mob_raw$Date <- ymd(mob_raw$Date)

mob_raw[, 2:3] <- lapply(mob_raw[, 2:3], as.factor)

# Subset observations for national level, reshape data
mob_ntl <- mob_raw %>% 
  dplyr::filter(Country == "Indonesia", is.na(Province)) %>% 
  dplyr::select(8:14) %>% 
  arrange(Date) %>% 
  mutate(
    Retail_Recreation_avg = rollmean(
      Retail_Recreation, k = 7, fill = NA, align = "right"
    ),
    Grocery_Pharmacy_avg = rollmean(
      Grocery_Pharmacy, k = 7, fill = NA, align = "right"
    ),
    Parks_avg = rollmean(
      Parks, k = 7, fill = NA, align = "right"
    ),
    Transit_stations_avg = rollmean(
      Transit_stations, k = 7, fill = NA, align = "right"
    ),
    Workplaces_avg = rollmean(
      Workplaces, k = 7, fill = NA, align = "right"
    ),
    Residential_avg = rollmean(
      Residential, k = 7, fill = NA, align = "right"
    )
  ) %>% 
  pivot_longer(2:13, names_to = "Categories", values_to = "Mobility") %>%
  arrange(Categories, Date)

# Round Mobility to two decimal places
mob_ntl$Mobility <- round(mob_ntl$Mobility, digits = 2)

# Correct data types  
mob_ntl$Categories <- as.factor(mob_ntl$Categories)

# String replacements
mob_cat_rpl <- c(
  "Grocery_Pharmacy" = "Grocery & pharmacy",
  "Retail_Recreation" = "Retail & recreation",
  "Transit_stations" = "Transit stations",
  "_avg" = " (7-day moving average)"
)

# Replace category names with human-readable category names
mob_ntl$Categories <- mob_ntl$Categories %>% 
  str_replace_all(mob_cat_rpl)


# Create the plot ---------------------------------------------------------

# Create object for filtering data in the plot
cat_line <- c(
  "Retail & recreation (7-day moving average)",
  "Grocery & pharmacy (7-day moving average)",
  "Parks (7-day moving average)",
  "Transit stations (7-day moving average)",
  "Workplaces (7-day moving average)",
  "Residential (7-day moving average)"
)

cat_point <- c(
  "Retail & recreation",
  "Grocery & pharmacy",
  "Parks",
  "Transit stations",
  "Workplaces",
  "Residential"
)

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

# Source, byline
byline_source_google <- list(
  x = 0,
  xref = "paper",
  xanchor = "left",
  xshift = 0,
  y = -0.175,
  yref = "paper",
  yshift = 0,
  text = "Chart: @dzulfiqarfr | Source: Google",
  font = list(color = "darkgrey"),
  showarrow = F
)

# Plot
mob_ntl_plot <- plot_ly(width = 700, height = 400) %>% 
  add_markers(
    data = mob_ntl[mob_ntl$Categories %in% cat_point, ],
    x = ~Date, 
    y = ~Mobility,
    color = ~Categories, 
    colors = "viridis",
    alpha = 0.1,
    mode = "scatter",
    hovertemplate = "%{y} percent<br>%{x}"
  ) %>% 
  add_trace(
    data = mob_ntl[mob_ntl$Categories %in% cat_line, ],
    x = ~Date, 
    y = ~Mobility,
    color = ~Categories,
    mode = "lines",
    colors = "viridis",
    hovertemplate = "Seven-day moving averages<br>%{y} percent<br>%{x}"
  ) %>% 
  plotly::layout(
    title = list(
      text = str_c(
        "<b>Mobility trends in Indonesia</b>",
        "<br>",
        "<sup>",
        "Number of visitors, by place categories (percent change from the median value for the Jan 3-Feb 6 period)",
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
      tickangle = 0,
      hovertemplate = "%b %d, '%y"
    ),
    yaxis = list(
      side = "right",
      title = NA,
      type = "linear",
      gridcolor = "lightgrey",
      fixedrange = T,
      autorange = F,
      range = c(-100, 41),
      zerolinecolor = "red"
    ),
    annotations = list(byline_source_google),
    legend = list(
      font = list(size = 10),
      traceorder = "reversed",
      itemclick = "toggleothers",
      itemdoubleclick = "toggle",
      xanchor = "right",
      x = 2
    ),
    margin = list(
      t = 75,
      l = 0,
      r = 0,
      b = 75
    )
  ) %>% 
  config(
    displaylogo = F,
    modeBarButtonsToRemove = remove
  )