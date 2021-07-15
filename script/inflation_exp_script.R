# indonesia economic recovery

# inflation by expenditure group

# author: dzulfiqar fathur rahman
# created: 2021-03-09
# last updated: 2021-07-15
# page: inflation


# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
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
if (!exists("base_url_static")) {
  base_url_static <- "https://webapi.bps.go.id/v1/api/view"
}

# request data
inf_exp_req <- GET(
  base_url_static,
  query = list(
    model = "statictable",
    domain = "0000",
    lang = "ind",
    id = "2083",
    key = BPS_KEY
  )
)

# parse response
inf_exp_parsed <- content(inf_exp_req, "text") %>% 
  fromJSON()

# download data as temporary file
GET(
  inf_exp_parsed$data$excel, 
  write_disk(inf_exp_temp <- tempfile(fileext = ".xls"))
)

# import
inf_exp_raw <- read_excel(inf_exp_temp, skip = 3, na = "") %>% 
  select(-Umum)

# extract years
inf_exp_yr <- inf_exp_raw %>% 
  select(`Tahun/Bulan`) %>% 
  dplyr::filter(str_detect(`Tahun/Bulan`, "^2")) %>% 
  mutate(`Tahun/Bulan` = as.integer(`Tahun/Bulan`)) %>% 
  arrange(`Tahun/Bulan`) %>% 
  deframe()

# latest month
inf_exp_mo_latest <- inf_exp_raw %>% 
  dplyr::filter(!is.na(`...2`)) %>% 
  select(`...2`) %>% 
  dplyr::filter(duplicated(`...2`)) %>% 
  tail(1) %>% 
  deframe()

# month variables
inf_exp_mo_var <- format(seq(ymd("2021-01-01"), ymd("2021-12-01"), "1 month"), "%b") %>% 
  enframe() %>% 
  mutate(
    name = case_when(name < 10 ~ str_c("0", name), TRUE ~ as.character(name)),
    value = case_when(
      value == "May" ~ "Mei",
      value == "Aug" ~ "Agu",
      value == "Oct" ~ "Okt",
      value == "Dec" ~ "Des",
      TRUE ~ value
    )
  )

# latest month in %m format
inf_exp_mo_m_latest <- inf_exp_mo_var %>% 
  dplyr::filter(value == str_sub(inf_exp_mo_latest, 1, 3)) %>% 
  select(name) %>% 
  deframe()

# date variable
inf_exp_date <- seq(
  ymd(str_c(first(inf_exp_yr), "-01-01")), 
  ymd(str_c(last(inf_exp_yr), inf_exp_mo_m_latest, "-01")),
  by = "month"
)

# sort date
inf_exp_date_sorted <- enframe(inf_exp_date) %>% 
  mutate(yr = year(value), mo = month(value)) %>% 
  arrange(desc(yr), mo) %>% 
  select(value) %>% 
  deframe()

# replace date variable
inf_exp_raw <- inf_exp_raw %>% 
  dplyr::filter(is.na(`Tahun/Bulan`), !is.na(...2)) %>% 
  mutate(`Tahun/Bulan` = inf_exp_date_sorted) %>% 
  rename(date = 1)

# remove month variable
inf_exp_raw <- inf_exp_raw %>% 
  select(-...2)

# rename variables
inf_exp_raw <- inf_exp_raw %>% 
  rename(
    `Food, beverage and tobacco` = 2,
    `Clothing and footwear` = 3,
    `Housing, water, electricity and household fuels` = 4,
    `Household equipment, tools and routine maintenance` = 5,
    `Healthcare` = 6,
    `Transportation` = 7,
    `Information, communication and financial services` = 8,
    `Recreation, sports and culture` = 9,
    `Education` = 10,
    `Food and beverage services or restaurants` = 11,
    `Personal care and other services` = 12
  )

# tidy data
inf_exp_tidy <- inf_exp_raw %>% 
  arrange(date) %>% 
  pivot_longer(2:ncol(.), names_to = "exp_group", values_to = "rate_mom") %>% 
  select(exp_group, date, rate_mom)

# reshape to wide format for reactable
inf_exp_wide <- inf_exp_tidy %>% 
  pivot_wider(names_from = date, values_from = rate_mom)


# table -------------------------------------------------------------------

# rename column
## column names 
inf_exp_tbl_col_nms <- enframe(inf_exp_date) %>% 
  mutate(mo = format(value, "%b '%y")) %>% 
  select(mo) %>% 
  deframe()

## replace column names
names(inf_exp_wide)[2:ncol(inf_exp_wide)] <- inf_exp_tbl_col_nms

# shared data
inf_exp_wide_shared <- SharedData$new(inf_exp_wide)

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
    palette = paletteer::paletteer_c("ggthemes::Orange", n = 5) %>% as.character(),
    domain = c(min(inf_exp_tidy$rate_mom), 0),
    na.color = "white",
    reverse = T
  )
  
  positive_values <- scales::col_numeric(
    palette = paletteer::paletteer_c("ggthemes::Green", n = 5) %>% as.character(),
    domain = c(0, max(inf_exp_tidy$rate_mom)),
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
reactable_inf_exp <- reactable(
  inf_exp_wide_shared,
  columns = list(
    exp_group = colDef(
      name = "",
      sortable = F,
      style = style_sticky,
      headerStyle = style_sticky,
      minWidth = 200
    )
  ),
  defaultColDef = colDef(
    style = function(value) {
      
      if (!is.numeric(value)) return()
      
      bg_color <- reactable_col_pal(value)
      
      font_color <- if (value > 0.5 || value < -0.5) {
        "white"
      } else {
        "black"
      }
      
      list(background = bg_color, color = font_color)
      
    }
  ),
  defaultSortOrder = "desc",
  defaultPageSize = 5,
  showPageSizeOptions = T,
  pageSizeOptions = c(5, 11),
  showPageInfo = F,
  style = list(fontSize = "14px"),
  highlight = T,
  theme = reactableTheme(headerStyle = list(borderColor = "black"))
)


# export chart ------------------------------------------------------------

# data
inf_exp_csv <- inf_exp_tidy %>% 
  rename(inflation_mom = rate_mom)

# path to inflation by expenditure group data
path_data_inf_exp <- "data/ier_inflation-expenditure_cleaned.csv"

# write csv
if (!file.exists(path_data_inf_exp)) {
  
  write_csv(inf_exp_csv, path_data_inf_exp)
  
  message("The inflation by expenditure group dataset has been exported")
  
} else if (nrow(inf_exp_csv) != nrow(read_csv(path_data_inf_exp))) {
  
  write_csv(inf_exp_csv, path_data_inf_exp)
  
  message("The inflation by expenditure group dataset has been updated")
  
} else {
  
  message("The inflation by expenditure group dataset is up to date")
  
}
