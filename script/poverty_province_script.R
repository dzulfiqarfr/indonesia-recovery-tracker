# indonesia economic recovery

# poverty by province

# author: dzulfiqar fathur rahman
# created: 2021-03-05
# last updated: 2021-07-14
# page: poverty


# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(paletteer)
library(reactable)
library(htmltools)
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

## poverty rate ----

# request data
pov_prov_req <- GET(
  base_url,
  query = list(
    model = "data",
    domain = "0000",
    var = "192",
    key = BPS_KEY
  )
)

# parse response
pov_prov_parsed <- content(pov_prov_req, "text") %>% 
  fromJSON()

# extract keys
## region
pov_prov_key_reg <- as_tibble(pov_prov_parsed$vervar)

## year
pov_prov_key_yr <- as_tibble(pov_prov_parsed$tahun)

# extract data
pov_prov_raw <- as_tibble(pov_prov_parsed$datacontent)

# tidy data, subset 2010s observations
pov_prov_tidy <- pov_prov_raw %>% 
  pivot_longer(1:ncol(.), names_to = "key", values_to = "pov_rate") %>% 
  separate(key, into = c("key_prov", "key_area_date"), sep = "192") %>% 
  mutate(
    key_area = as.numeric(str_sub(key_area_date, 1, 3)),
    key_yr = as.numeric(str_sub(key_area_date, 4, 6)),
    key_mo = as.numeric(str_sub(key_area_date, 7, 8))
  ) %>% 
  select(key_prov, key_area, key_area_date, key_mo, key_yr, pov_rate) %>% 
  arrange(key_prov, key_area, key_yr, key_mo) %>% 
  dplyr::filter(key_prov != 9999, key_yr > 110, key_mo != 63)

# replace province key
pov_prov_tidy$key_prov <- pov_prov_tidy$key_prov %>% 
  str_replace_all(deframe(pov_prov_key_reg)) %>% 
  as_factor()

# replace area key
pov_prov_tidy$key_area <- pov_prov_tidy$key_area %>% 
  str_replace_all(c("432" = "Urban", "433" = "Rural", "434" = "Total")) %>% 
  as_factor()

# replace period key
pov_prov_tidy$key_mo <- pov_prov_tidy$key_mo %>% 
  str_replace_all(c("61" = "-03-01", "62" = "-09-01"))

# replace year key
pov_prov_tidy$key_yr <- pov_prov_tidy$key_yr %>% 
  str_replace_all(deframe(pov_prov_key_yr))

# create date variable
pov_prov_tidy <- pov_prov_tidy %>%
  mutate(date = ymd(str_c(key_yr, key_mo))) %>%
  select(key_prov, key_area, date, pov_rate) %>% 
  arrange(key_prov, key_area, date)

# calculate annual changes in percentage points
pov_prov_trf <- pov_prov_tidy %>% 
  group_by(key_prov, key_area) %>% 
  mutate(diff = round(pov_rate - dplyr::lag(pov_rate, 2), 2)) %>% 
  ungroup()


## number of poor people ----

# request data
poor_prov_req <- GET(
  base_url,
  query = list(
    model = "data",
    domain = "0000",
    var = "185",
    key = BPS_KEY
  )
)

# parse response
poor_prov_parsed <- content(poor_prov_req, "text") %>% 
  fromJSON()

# extract keys
## region
poor_prov_key_reg <- as_tibble(poor_prov_parsed$vervar)

## year
poor_prov_key_yr <- as_tibble(poor_prov_parsed$tahun)

# extract data
poor_prov_raw <- as_tibble(poor_prov_parsed$datacontent)

# tidy data, subset 2010s observations
poor_prov_tidy <- poor_prov_raw %>% 
  pivot_longer(1:ncol(.), names_to = "key", values_to = "poor") %>% 
  separate(key, into = c("key_prov", "key_area_date"), sep = "185") %>% 
  mutate(
    key_area = as.numeric(str_sub(key_area_date, 1, 3)),
    key_yr = as.numeric(str_sub(key_area_date, 4, 6)), 
    key_mo = as.numeric(str_sub(key_area_date, 7, 8)) 
  ) %>% 
  select(key_prov, key_area, key_area_date, key_mo, key_yr, poor) %>% 
  arrange(key_prov, key_area, key_yr, key_mo) %>% 
  dplyr::filter(key_prov != 9999, key_yr > 110, key_mo != 63)

# replace province key
poor_prov_tidy$key_prov <- poor_prov_tidy$key_prov %>% 
  str_replace_all(deframe(poor_prov_key_reg)) %>% 
  as_factor()

# replace area key
poor_prov_tidy$key_area <- poor_prov_tidy$key_area %>% 
  str_replace_all(c("432" = "Urban", "433" = "Rural", "434" = "Total" )) %>% 
  as_factor()

# replace period key
poor_prov_tidy$key_mo <- poor_prov_tidy$key_mo %>% 
  str_replace_all(c("^61$" = "-03-01", "^62$" = "-09-01"))

# replace year keys
poor_prov_tidy$key_yr <- poor_prov_tidy$key_yr %>% 
  str_replace_all(deframe(poor_prov_key_yr))

# create date variable, remove period, year, area_date keys variables
poor_prov_tidy <- poor_prov_tidy %>%
  mutate(date = ymd(str_c(key_yr, key_mo))) %>% 
  select(key_prov, key_area, date, poor) %>% 
  arrange(key_prov, key_area, date)

# calculate annual changes in percentage points
poor_prov_trf <- poor_prov_tidy %>% 
  group_by(key_prov, key_area) %>% 
  mutate(diff = round(poor - dplyr::lag(poor, 2), 2)) %>% 
  ungroup()


## merge poverty rate, number of poor data ----

# merge
pov_poor_prov <- poor_prov_trf %>% 
  left_join(pov_prov_trf, by = c("key_prov", "key_area", "date")) %>% 
  rename(prov = key_prov, area = key_area)

# subset latest data, total
pov_poor_sub_wide <- pov_poor_prov %>% 
  dplyr::filter(date == last(date), area == "Total") %>% 
  select(-area) %>% 
  pivot_wider(
    names_from = date,
    values_from = c(poor, diff.x, pov_rate, diff.y)
  ) %>%
  rename(
    poor = 2,
    poor_diff = 3,
    pov_rate = 4,
    pov_rate_diff = 5
  )

# replace province names with english names
pov_poor_sub_wide$prov <- pov_poor_sub_wide$prov %>% 
  str_replace_all(
    c(
      "JAWA BARAT" = "West Java",
      "JAWA TENGAH" = "Central Java",
      "JAWA TIMUR" = "East Java",
      "DI " = "",
      "DKI " = "",
      "KALIMANTAN BARAT" = "West Kalimantan",
      "KALIMANTAN SELATAN" = "South Kalimantan",
      "KALIMANTAN TENGAH" = "Central Kalimantan", 
      "KALIMANTAN TIMUR" = "East Kalimantan",
      "KALIMANTAN UTARA" = "North Kalimantan",
      "KEP. BANGKA BELITUNG" = "Bangka Belitung Islands",
      "KEP. RIAU" = "Riau Islands",
      "MALUKU UTARA" = "North Maluku",
      "NUSA TENGGARA BARAT" = "West Nusa Tenggara",
      "NUSA TENGGARA TIMUR" = "East Nusa Tenggara",
      "PAPUA BARAT" = "West Papua",
      "SULAWESI BARAT" = "West Sulawesi",
      "SULAWESI SELATAN" = "South Sulawesi",
      "SULAWESI TENGAH" = "Central Sulawesi",
      "SULAWESI TENGGARA" = "Southeast Sulawesi",
      "SULAWESI UTARA" = "North Sulawesi",
      "SUMATERA SELATAN" = "South Sumatra",
      "SUMATERA UTARA" = "North Sumatra",
      "SUMATERA BARAT" = "West Sumatra"
    )
  ) %>% 
  str_to_title()


# table -------------------------------------------------------------------

# shared data
pov_poor_sub_wide_shared <- SharedData$new(pov_poor_sub_wide)

# color scale
reactable_col_pal <- function(x) {
  
  negative_values <- scales::col_numeric(
    palette = paletteer::paletteer_c("ggthemes::Blue", n = 2) %>% as.character(),
    domain = c(min(pov_poor_sub_wide$pov_rate_diff), 0),
    na.color = "white",
    reverse = T
  )
  
  positive_values <- scales::col_numeric(
    palette = paletteer::paletteer_c("ggthemes::Red", n = 10) %>% as.character(),
    domain = c(0, max(pov_poor_sub_wide$pov_rate_diff)),
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
reactable_pov_poor <- reactable(
  pov_poor_sub_wide_shared,
    columns = list(
      prov = colDef(name = "", sortable = F, minWidth = 200),
      poor = colDef(
        name = str_c(
          format(last(pov_poor_prov$date), "%b '%y"),
          "<br>",
          '<div style="color: #999; font-size: 12px">(thousand)</div>'
        ),
        format = colFormat(separators = T),
        html = T
      ),
      poor_diff = colDef(
        name = str_c(
          "Change",
          "<br>",
          '<div style="color: #999; font-size: 12px">(thousand)</div>'
        ),
        format = colFormat(separators = T),
        html = T
      ),
      pov_rate = colDef(
        name = str_c(
          format(last(pov_poor_prov$date), "%b '%y"),
          "<br>",
          '<div style="color: #999; font-size: 12px">(percent)</div>'
        ),
        html = T
      ),
      pov_rate_diff = colDef(
        name = str_c(
          "Change",
          "<br>",
          '<div style="color: #999; font-size: 12px">(% points)</div>'
        ),
        html = T,
        style = function(value) {
          
          bg_color <- reactable_col_pal(value)
          
          font_color <- if (value > 1 || value < -0.1) {
            "white"
          } else {
            "black"
          }
          
          list(background = bg_color, color = font_color)
          
        }
      )
    ),
    columnGroups = list(
      colGroup(
        name = "<b>Number of poor people</b>", 
        columns = c("poor", "poor_diff"),
        html = T
      ),
      colGroup(
        name = "<b>Poverty rate</b>",
        columns = c("pov_rate", "pov_rate_diff"),
        html = T
      )
    ),
    defaultSortOrder = "desc",
    defaultSorted = "pov_rate_diff",
    defaultPageSize = 10,
    showPageSizeOptions = T,
    pageSizeOptions = c(10, 20, 34),
    showPageInfo = F,
    highlight = T,
    style = list(fontSize = "14px"),
    theme = reactableTheme(headerStyle = list(borderColor = "black"))
  )


# export data -------------------------------------------------------------

# data
pov_poor_prov_csv <- pov_poor_prov %>% 
  dplyr::filter(area == "Total", date == last(date)) %>% 
  select(!area) %>% 
  rename(poor_diff = 4, rate_diff = 6)

# rename variables
pov_poor_prov_csv <- pov_poor_prov_csv %>% 
  rename(
    number_of_poor_ppl = poor,
    poor_change_yoy = poor_diff,
    pov_ppt_change_yoy = rate_diff
  )

# latest observation in most recent data
pov_poor_prov_last <-  pov_poor_prov_csv %>% 
  select(date) %>% 
  dplyr::filter(!duplicated(date), date == last(date)) %>% 
  deframe()

# path to poverty by province data
path_data_pov_prov <- "data/ier_poverty-province_cleaned.csv"

# latest observation in csv
pov_poor_prov_csv_last <-  read_csv(path_data_pov_prov) %>% 
  select(date) %>% 
  dplyr::filter(!duplicated(date), date == last(date)) %>% 
  deframe()

# write csv
if (!file.exists(path_data_pov_prov)) {
  
  write_csv(pov_poor_prov_csv, path_data_pov_prov)
  
  message("The regional poverty dataset has been exported")
  
} else if (pov_poor_prov_last != pov_poor_prov_csv_last) {
  
  write_csv(pov_poor_prov_csv, path_data_pov_prov)
  
  message("The regional poverty dataset has been updated")
  
} else {
  
  message("The regional poverty dataset is up to date")
  
}