# indonesia economic recovery

# gdp
# by expenditure
# percent change on a year earlier

# author: dzulfiqar fathur rahman
# created: 2021-02-21
# last updated: 2021-04-08
# page: gdp


# setup -------------------------------------------------------------------

# packages
library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
library(reactable)
library(scales)
library(ggthemes)
library(paletteer)
library(crosstalk)

# api key
if (exists("BPS_KEY") == F) {
  BPS_KEY <- Sys.getenv("BPS_KEY")
}

# bps api url
if (exists("base_url") == F) {
  base_url <- "https://webapi.bps.go.id/v1/api/list"
}


# data --------------------------------------------------------------------

if (exists("growth_raw") == F) {
  
  # request data
  growth_req <- GET(
    base_url,
    query = list(
      model = "data",
      domain = "0000",
      var = "108",
      key = BPS_KEY
    )
  )
  
  # parse response
  growth_parsed <- content(growth_req, "text") %>% 
    fromJSON()
  
  # extract keys
  ## expenditure
  growth_key_exp <- as_tibble(growth_parsed$vervar)
  
  ### expenditure labels in english
  growth_key_exp <- growth_key_exp %>% 
    mutate(
      label_eng = c(
        "Household consumption",
        "Food and beverages, other than restaurants",
        "Clothes, footwear and their care services",
        "Housing and household supplies",
        "Healthcare and education",
        "Transportation and communication",
        "Restaurants and hotels",
        "Other",
        "NPISHs consumption",
        "Government consumption",
        "Collective consumption",
        "Individual consumption",
        "Gross fixed capital formation",
        "Buildings",
        "Machinery and equipment",
        "Vehicles",
        "Other equipments",
        "Cultivated biological resources",
        "Intellectual property products",
        "Changes in inventories",
        "Exports of goods and services",
        "Goods",
        "Non-oil and gas",
        "Oil and gas",
        "Services",
        "Imports of goods and services",
        "Goods",
        "Non-oil and gas",
        "Oil and gas",
        "Services",
        "Statistics discrepancy",
        "GDP"
      )
    )
  
  ## year
  growth_key_yr <- as_tibble(growth_parsed$tahun)
  
  # extract data
  growth_raw <- as_tibble(growth_parsed$datacontent)

}

# separate keys, subset quarterly observations
growth_exp_tidy <- growth_raw %>% 
  pivot_longer(
    1:ncol(.),
    names_to = "key",
    values_to = "pct_change_yoy"
  ) %>% 
  separate(
    key,
    into = c("key_exp", "key_period"),
    sep = "108"
  ) %>%
  mutate(
    key_period_obs = str_sub(key_period, end = 1),
    key_yr = str_sub(key_period, 2, 4),
    key_q = str_sub(key_period, 5, 6)
  ) %>% 
  dplyr::filter(key_period_obs == "5", key_q != "35")

# replace year key
growth_exp_tidy$key_yr <- growth_exp_tidy$key_yr %>% 
  str_replace_all(deframe(growth_key_yr))

# replace quarter key
growth_exp_tidy$key_q <- growth_exp_tidy$key_q %>% 
  str_replace_all(
    c(
      "^31$" = "01-01",
      "^32$" = "04-01",
      "^33$" = "07-01",
      "^34$" = "10-01"
    )
  )

# add expenditure label
## coerce expenditure keys to character
growth_key_exp$val <- as.character(growth_key_exp$val)

## add english labels
growth_exp_tidy <- growth_exp_tidy %>% 
  left_join(growth_key_exp[, -2], by = c("key_exp" = "val"))

# subset main components
growth_exp_tidy <- growth_exp_tidy %>% 
  mutate(date = str_c(key_yr, "-", key_q)) %>% 
  select(key_exp, label_eng, date, pct_change_yoy)

# reshape data to wide format
growth_exp_wide <- growth_exp_tidy %>% 
  dplyr::filter(key_exp %in% c(100, 200, 300, 400, 600, 700)) %>% 
  select(-key_exp) %>% 
  pivot_wider(names_from = date, values_from = pct_change_yoy)


# export ------------------------------------------------------------------

# data
growth_exp_csv <- growth_exp_tidy %>% 
  rename(expenditure_key = 1, expenditure_component = 2)

# write csv
if (file.exists("data/ier_gdp-growth-exp_cleaned.csv") == F) {
  
  write_csv(growth_exp_csv, "data/ier_gdp-growth-exp_cleaned.csv")
  
  message("The GDP growth by expenditure dataset has been exported")
  
} else if (nrow(growth_exp_csv) != nrow(read_csv("data/ier_gdp-growth-exp_cleaned.csv"))) {
  
  write_csv(growth_exp_csv, "data/ier_gdp-growth-exp_cleaned.csv")
  
  message("The GDP growth by expenditure dataset has been updated")
  
} else {
  
  message("The GDP growth by expenditure dataset is up to date")

}


# table -------------------------------------------------------------------

# add asterisk to NPISHs consumption
growth_exp_wide[growth_exp_wide$label_eng == "NPISHs consumption", 1] <- "NPISHs* consumption"

# rename column
## column names
growth_exp_col_nms <- growth_exp_tidy %>% 
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
names(growth_exp_wide)[2:ncol(growth_exp_wide)] <- growth_exp_col_nms

# shared data
growth_exp_shared <- SharedData$new(growth_exp_wide)

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
    domain = c(min(growth_exp_wide[, 2:ncol(growth_exp_wide)]), 0),
    na.color = "white",
    reverse = T
  )
  
  positive_values <- scales::col_numeric(
    palette = paletteer::paletteer_c("ggthemes::Blue", n = 10) %>% as.character(),
    domain = c(0, max(growth_exp_wide[, 2:ncol(growth_exp_wide)])),
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
reactable_growth_exp <- reactable(
  growth_exp_shared,
  columns = list(
    label_eng = colDef(
      name = "",
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
      
      font_color <- if (value > 10 || value < -10) {
        "white"
      } else {
        "black"
      }
      
      list(background = bg_color, color = font_color)
      
    }
  ),
  defaultSortOrder = "desc",
  highlight = T,
  style = list(fontSize = "15px"),
  theme = reactableTheme(
    headerStyle = list(borderColor = "black")
  )
)
