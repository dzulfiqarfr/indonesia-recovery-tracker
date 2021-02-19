# National mobility plot


# Setup ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(zoo)
library(plotly)
library(crosstalk)


# Tidy the data -----------------------------------------------------------

# Import data 
mob_base <- read_csv(
  "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", 
  na = ""
)

# Subset Indonesia observations, rename variables
mob_raw <- mob_base %>% 
  dplyr::filter(country_region == "Indonesia") %>% 
  select(
    country_region,
    sub_region_1,
    date,
    retail_and_recreation_percent_change_from_baseline,
    grocery_and_pharmacy_percent_change_from_baseline,
    parks_percent_change_from_baseline,
    transit_stations_percent_change_from_baseline,
    workplaces_percent_change_from_baseline,
    residential_percent_change_from_baseline
  ) %>% 
  rename(
    "Country" = 1,
    "Province" = 2,
    "Date" = 3,
    "Retail_Recreation" = 4,
    "Grocery_Pharmacy" = 5,
    "Parks" = 6,
    "Transit_stations" = 7,
    "Workplaces" = 8,
    "Residential" = 9
  )

# Correct data types for `Country`, `Province`
mob_raw[, 1:2] <- lapply(mob_raw[, 1:2], as.factor)


# National ----------------------------------------------------------------


# Subset observations for national level, reshape data
mob_ntl <- mob_raw %>% 
  dplyr::filter(is.na(Province)) %>% 
  dplyr::select(-(1:2)) %>% 
  arrange(Date) %>% 
  mutate(
    Retail_Recreation_avg = rollmean(Retail_Recreation, k = 7, fill = NA, align = "right"),
    Grocery_Pharmacy_avg = rollmean(Grocery_Pharmacy, k = 7, fill = NA, align = "right"),
    Parks_avg = rollmean(Parks, k = 7, fill = NA, align = "right"),
    Transit_stations_avg = rollmean(Transit_stations, k = 7, fill = NA, align = "right"),
    Workplaces_avg = rollmean(Workplaces, k = 7, fill = NA, align = "right"),
    Residential_avg = rollmean(Residential, k = 7, fill = NA, align = "right")
  ) %>% 
  pivot_longer(2:ncol(.), names_to = "Categories", values_to = "Mobility") %>%
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


# Regional ----------------------------------------------------------------

# Subset observations for regional level, reshape data
mob_prov <- mob_raw %>% 
  dplyr::filter(!is.na(Province)) %>% 
  dplyr::select(-1) %>% 
  group_by(Province) %>% 
  arrange(Date) %>% 
  mutate(
    Retail_Recreation_avg = rollmean(Retail_Recreation, k = 7, fill = NA, align = "right"),
    Grocery_Pharmacy_avg = rollmean(Grocery_Pharmacy, k = 7, fill = NA, align = "right"),
    Parks_avg = rollmean(Parks, k = 7, fill = NA, align = "right"),
    Transit_stations_avg = rollmean(Transit_stations, k = 7, fill = NA, align = "right"),
    Workplaces_avg = rollmean(Workplaces, k = 7, fill = NA, align = "right"),
    Residential_avg = rollmean(Residential, k = 7, fill = NA, align = "right")
  ) %>% 
  pivot_longer(3:ncol(.), names_to = "Categories", values_to = "Mobility") %>%
  arrange(Province, Categories, Date)

# Round Mobility to two decimal places
mob_prov$Mobility <- round(mob_prov$Mobility, digits = 2)

# Correct data types
mob_prov$Categories <- as.factor(mob_prov$Categories)

# Replace category names with human-readable category names
mob_prov$Categories <- mob_prov$Categories %>% 
  str_replace_all(mob_cat_rpl)


# Create the plot for national observations---------------------------------------------------------

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

# Plot
mob_ntl_plot <- plot_ly(
  line = list(width = 3),
  height = 300
) %>% 
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
    xaxis = list (
      title = NA,
      fixedrange = T,
      range = c(
        as.character(first(mob_ntl_7day$Date) - 14),
        as.character(last(mob_ntl_7day$Date) + 14)
      ),
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
      gridcolor = "#CFD8DC",
      fixedrange = T,
      autorange = F,
      range = c(-100, 55),
      dtick = 25,
      zerolinecolor = "#ff856c"
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
      t = 5,
      b = 0,
      l = 0,
      r = 0
    ),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)


# Create the plot for regional observations ---------------------------------------------------------


# Filter data
mob_prov_7day <- mob_prov %>% 
  dplyr::filter(Categories %in% cat_line)

# Rename categories
mob_prov_7day$Categories[mob_prov_7day$Categories %in% "Grocery & pharmacy (7-day moving average)"] <- "Grocery & pharmacy"

mob_prov_7day$Categories[mob_prov_7day$Categories %in% "Parks (7-day moving average)"] <- "Parks"

mob_prov_7day$Categories[mob_prov_7day$Categories %in% "Residential (7-day moving average)"] <- "Residential"

mob_prov_7day$Categories[mob_prov_7day$Categories %in% "Retail & recreation (7-day moving average)"] <- "Retail & recreation"

mob_prov_7day$Categories[mob_prov_7day$Categories %in% "Transit stations (7-day moving average)"] <- "Transit stations"

mob_prov_7day$Categories[mob_prov_7day$Categories %in% "Workplaces (7-day moving average)"] <- "Workplaces"

# Create a shared data
mob_prov_key <- highlight_key(mob_prov_7day)

# Plot
mob_prov_plot <- plot_ly(
  mob_prov_key,
  x = ~Date,
  y = ~Mobility,
  color = ~Categories,
  colors = c(
    "#09bb9f", #Grocery
    "#607d8b", #Parks
    "#1d81a2", #Residential
    "#ff5e4b", #Retail & recreation
    "#ffca76", #Transit stations
    "#5cccfa" #Workplaces
  ),
  type = "scatter",
  mode = "line",
  hovertemplate = str_c(
    "7-day moving average",
    "<br>",
    "%{y} percent",
    "<br>",
    "%{x}",
    "<extra></extra>"
  ),
  line = list(width = 3),
  height = 300
) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      fixedrange = T,
      range = c(
        as.character(first(mob_prov_7day$Date) - 14),
        as.character(last(mob_prov_7day$Date) + 14)
      ),
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
      fixedrange = T,
      gridcolor = "#CFD8DC",
      autorange = F,
      range = c(-100, 55),
      dtick = 25,
      zerolinecolor = "#ff856c"
    ),
    legend = list(
      xanchor = "left",
      y = 1.01,
      yanchor = "top",
      font = list(
        size = 7.5
      )
    ),
    clickmode = "none",
    margin = list(
      t = 5,
      b = 0,
      l = 0,
      r = 0
    ),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)

# Create the filter button
mob_prov_fil <- filter_select(
  "Province_filter",
  "Select a province...",
  mob_prov_key,
  ~Province,
  multiple = F
)