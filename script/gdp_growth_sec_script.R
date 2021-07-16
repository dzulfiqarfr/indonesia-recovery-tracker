# indonesia economic recovery

# economic growth by sector

# author: dzuliqar fathur rahman
# created: 2021-02-23
# last updated: 2021-07-15
# page: gdp


# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
library(reactable)
library(scales)
library(ggthemes)
library(paletteer)
library(crosstalk)


# data --------------------------------------------------------------------

# api key
if (!exists("BPS_KEY")) {
  BPS_KEY <- Sys.getenv("BPS_KEY")
}

# bps api url
if (!exists("base_url")) {
  base_url <- "https://webapi.bps.go.id/v1/api/list"
}

# request data
growth_sec_req <- GET(
  base_url,
  query = list(
    model = "data",
    domain = "0000",
    var = "104",
    key = BPS_KEY
  )
)

# parse response
growth_sec_parsed <- content(growth_sec_req, "text") %>% 
  fromJSON()

# extract keys
## sectors
growth_key_sec <- as_tibble(growth_sec_parsed$vervar)

## year
growth_key_yr <- as_tibble(growth_sec_parsed$tahun)

## data
growth_sec_raw <- as_tibble(growth_sec_parsed$datacontent)

# separate keys, subset quarterly observations
growth_sec_tidy <- growth_sec_raw %>% 
  pivot_longer(1:ncol(.), names_to = "key", values_to = "pct_change_yoy") %>% 
  separate(key, into = c("key_sector", "key_period"), sep = "104") %>% 
  mutate(
    key_period_obs = str_sub(key_period, 1, 1),
    key_yr = str_sub(key_period, 2, 4),
    key_q = str_sub(key_period, 5, 6)
  ) %>% 
  dplyr::filter(key_period_obs == "5", key_q != "35")

# replace year key
growth_sec_tidy$key_yr <- growth_sec_tidy$key_yr %>% 
  str_replace_all(deframe(growth_key_yr))

# replace quarter key
growth_sec_tidy$key_q <- growth_sec_tidy$key_q %>% 
  str_replace_all(
    c(
      "^31$" = "-01-01",
      "^32$" = "-04-01",
      "^33$" = "-07-01",
      "^34$" = "-10-01"
    )
  )

# create date variable
growth_sec_tidy <- growth_sec_tidy %>% 
  mutate(date = ymd(str_c(key_yr, key_q))) %>% 
  select(key_sector, date, pct_change_yoy)

# subset main sectors
## main sector key
growth_key_sec_main <- seq(11000, 27000, 1000)

## subset
growth_sec_tidy <- growth_sec_tidy %>% 
  dplyr::filter(key_sector %in% growth_key_sec_main)

# replace sector key
growth_sec_tidy$key_sector <- growth_sec_tidy$key_sector %>% 
  str_replace_all(
    c(
      "11000" = "Agriculture, forestry and fishery",
      "12000" = "Mining",
      "13000" = "Manufacturing",
      "14000" = "Electricity and gas procurement",
      "15000" = "Water, waste and recycling management procurement",
      "16000" = "Construction",
      "17000" = "Wholesale trade and retail; car and motorcycle repair",
      "18000" = "Transportation and warehouse",
      "19000" = "Accommodation, food and beverage services",
      "20000" = "Information and communication",
      "21000" = "Financial and insurance services",
      "22000" = "Real estate",
      "23000" = "Corporate service",
      "24000" = "Government, defense and social security administration",
      "25000" = "Education service",
      "26000" = "Healthcare and social activity services",
      "27000" = "Other"
    )
  )

# reshape data to wide format
growth_sec_wide <- growth_sec_tidy %>% 
  pivot_wider(names_from = date, values_from = pct_change_yoy)


# table -------------------------------------------------------------------

# rename column
## column names
growth_sec_col_nms <- growth_sec_tidy %>% 
  select(date) %>% 
  dplyr::filter(!duplicated(date)) %>% 
  mutate(
    quarter = str_c("Q", quarter(date)),
    yr = format(ymd(date), "'%y"),
    q_yr = str_c(quarter, yr, sep = "<br>")
  ) %>% 
  select(q_yr) %>% 
  deframe()

## replace column names
names(growth_sec_wide)[2:ncol(growth_sec_wide)] <- growth_sec_col_nms

# shared data
growth_sec_shared <- SharedData$new(growth_sec_wide)

# style for sticky column
style_sticky <- list(
  position = "sticky",
  left = 0,
  background = "#fff",
  zIndex = 1
)

# color scale
reactable_col_pal <- function(x) {
  
  negative_values <- scales::col_numeric(
    palette = paletteer::paletteer_c("ggthemes::Red", n = 10) %>% as.character(),
    domain = c(min(growth_sec_wide[, 2:ncol(growth_sec_wide)]), 0),
    na.color = "white",
    reverse = T
  )
  
  positive_values <- scales::col_numeric(
    palette = paletteer::paletteer_c("ggthemes::Blue", n = 10) %>% as.character(),
    domain = c(0, max(growth_sec_wide[, 2:ncol(growth_sec_wide)])),
    na.color = "white"
  )
  
  if (x == 0) {
    "#CFD8DC"
  } else if (x < 0) {
    negative_values(x)
  } else {
    positive_values(x)
  }
  
}

# table
reactable_growth_sec <- reactable(
  growth_sec_shared,
  columns = list(
    key_sector = colDef(
      name = "",
      sortable = F,
      minWidth = 200,
      style = style_sticky,
      headerStyle = style_sticky
    )
  ),
  defaultColDef = colDef(
    html = T,
    style = function(value) {
      
      if (!is.numeric(value)) return()
      
      bg_color <- reactable_col_pal(value)
      
      font_color <- if (value > 7.5 || value < -10) {
        "white"
      } else {
        "black"
      }
      
      list(background = bg_color, color = font_color)
      
    }
  ),
  defaultSortOrder = "desc",
  defaultPageSize = 6,
  showPageSizeOptions = T,
  pageSizeOptions = c(6, 12, 17),
  showPageInfo = F,
  highlight = T,
  style = list(fontSize = "14px"),
  theme = reactableTheme(headerStyle = list(borderColor = "black"))
)


# export data -------------------------------------------------------------

#data
growth_sec_csv <- growth_sec_tidy %>% 
  rename(sector = 1)

# path to the data
path_data_growth_sec <- "data/ier_gdp-growth-sector_cleaned.csv"

# write csv
if (!file.exists(path_data_growth_sec)) {
  
  write_csv(growth_sec_csv, path_data_growth_sec)
  
  message("The GDP growth by sector dataset has been updated")
  
} else if (nrow(growth_sec_csv) != nrow(read_csv(path_data_growth_sec))) {
  
  write_csv(growth_sec_csv, path_data_growth_sec)
  
  message("The GDP growth by sector dataset has been updated")
  
} else {
  
  message("The GDP growth by sector dataset is up to date")
  
}