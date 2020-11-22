# National mobility plot


# Setup ----------------------------------------------------------------

library(conflicted)
library(tidyverse)
library(lubridate)
library(zoo)
library(plotly)


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

# Create object to filter data in the plot
cat_line <- c(
  "Retail & recreation (7-day moving average)",
  "Grocery & pharmacy (7-day moving average)",
  "Parks (7-day moving average)",
  "Transit stations (7-day moving average)",
  "Workplaces (7-day moving average)",
  "Residential (7-day moving average)"
)

# Filter data
mob_ntl_7day <- mob_ntl %>% 
  dplyr::filter(Categories %in% cat_line)

# Rename categories
mob_ntl_7day$Categories[mob_ntl_7day$Categories %in% "Grocery & pharmacy (7-day moving average)"] <- "Grocery & pharmacy"

mob_ntl_7day$Categories[mob_ntl_7day$Categories %in% "Parks (7-day moving average)"] <- "Parks"

mob_ntl_7day$Categories[mob_ntl_7day$Categories %in% "Residential (7-day moving average)"] <- "Residential"

mob_ntl_7day$Categories[mob_ntl_7day$Categories %in% "Retail & recreation (7-day moving average)"] <- "Retail & recreation"

mob_ntl_7day$Categories[mob_ntl_7day$Categories %in% "Transit stations (7-day moving average)"] <- "Transit stations"

mob_ntl_7day$Categories[mob_ntl_7day$Categories %in% "Workplaces (7-day moving average)"] <- "Workplaces"

# Byline, source annotations
byline_source_google <- list(
  x = 0,
  xref = "paper",
  xanchor = "left",
  xshift = 0,
  y = -0.15,
  yref = "paper",
  yanchor = "top",
  yshift = 0,
  text = "Chart: @dzulfiqarfr | Source: Google",
  font = list(size = 10, color = "darkgrey"),
  showarrow = F
)

# Timestamp
mob_timestamp <- list(
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

# Plot
mob_ntl_plot <- plot_ly() %>% 
  add_trace(
    data = mob_ntl_7day[mob_ntl_7day$Categories %in% "Retail & recreation", ],
    x = ~Date, 
    y = ~Mobility,
    color = ~Categories, 
    line = list(color = "#ff5e4b"),
    type = "scatter",
    mode = "lines",
    hovertemplate = str_c(
      "7-day moving averages",
      "<br>",
      "%{y} percent",
      "<br>",
      "%{x}",
      "<extra></extra>"
    )
  ) %>% 
  add_trace(
    data = mob_ntl_7day[mob_ntl_7day$Categories %in% "Grocery & pharmacy", ],
    x = ~Date, 
    y = ~Mobility,
    color = ~Categories, 
    line = list(color = "#09bb9f"),
    type = "scatter",
    mode = "lines",
    hovertemplate = str_c(
      "7-day moving averages",
      "<br>",
      "%{y} percent",
      "<br>",
      "%{x}",
      "<extra></extra>"
    )
  ) %>%
  add_trace(
    data = mob_ntl_7day[mob_ntl_7day$Categories %in% "Parks", ],
    x = ~Date, 
    y = ~Mobility,
    color = ~Categories, 
    line = list(color = "#607d8b"),
    type = "scatter",
    mode = "lines",
    hovertemplate = str_c(
      "7-day moving averages",
      "<br>",
      "%{y} percent",
      "<br>",
      "%{x}",
      "<extra></extra>"
    )
  ) %>%
  add_trace(
    data = mob_ntl_7day[mob_ntl_7day$Categories %in% "Transit stations", ],
    x = ~Date, 
    y = ~Mobility,
    color = ~Categories, 
    line = list(color = "#ffca76"),
    type = "scatter",
    mode = "lines",
    hovertemplate = str_c(
      "7-day moving averages",
      "<br>",
      "%{y} percent",
      "<br>",
      "%{x}",
      "<extra></extra>"
    )
  ) %>%
  add_trace(
    data = mob_ntl_7day[mob_ntl_7day$Categories %in% "Workplaces", ],
    x = ~Date, 
    y = ~Mobility,
    color = ~Categories, 
    line = list(color = "#5cccfa"),
    type = "scatter",
    mode = "lines",
    hovertemplate = str_c(
      "7-day moving averages",
      "<br>",
      "%{y} percent",
      "<br>",
      "%{x}",
      "<extra></extra>"
    )
  ) %>%
  add_trace(
    data = mob_ntl_7day[mob_ntl_7day$Categories %in% "Residential", ],
    x = ~Date, 
    y = ~Mobility,
    color = ~Categories, 
    line = list(color = "#1d81a2"),
    type = "scatter",
    mode = "lines",
    hovertemplate = str_c(
      "7-day moving averages",
      "<br>",
      "%{y} percent",
      "<br>",
      "%{x}",
      "<extra></extra>"
    )
  ) %>%
  plotly::layout(
    title = list(
      text = str_c(
        "<b>Mobility trends in Indonesia</b>",
        "<br>",
        '<sup>',
        "Number of visitors, by place categories",
        "<br>",
        "<sup>(percent change from Jan 3-Feb 6 period, 7-day moving average)</sup>",
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
      tickformat = "%b",
      automargin = T
    ),
    yaxis = list(
      side = "left",
      title = NA,
      type = "linear",
      gridcolor = "lightgrey",
      fixedrange = T,
      autorange = F,
      range = c(-100, max(mob_ntl_7day$Mobility, na.rm = T) + 25),
      dtick = 20,
      zerolinecolor = "#ff856c"
    ),
    annotations = list(
      byline_source_google,
      mob_timestamp
    ),
    legend = list(
      xanchor = "left",
      y = 1.01,
      yanchor = "top",
      font = list(
        size = 7.5
      )
    ),
    margin = list(
      t = 100,
      b = 75,
      l = 25,
      r = 25
    ),
    autosize = T
  ) %>% 
  config(displayModeBar = F)
