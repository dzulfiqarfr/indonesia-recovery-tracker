# indonesia economic recovery

# employment
# by sector

# author: dzulfiqar fathur rahman
# created: 2021-03-25
# last updated: 2021-03-28
# page: employment


# setup -------------------------------------------------------------------

# packages
library(tidyverse)
library(readxl)
library(lubridate)
library(httr)
library(jsonlite)
library(reactable)
library(paletteer)
library(crosstalk)

# api key
if (exists("BPS_KEY") == F) {
  BPS_KEY <- Sys.getenv("BPS_KEY")
}

# bps api url
if (exists("base_url_static") == F) {
  base_url_static <- "https://webapi.bps.go.id/v1/api/view"
}


# data --------------------------------------------------------------------

# request data
emp_sec_req <- GET(
  base_url_static,
  query = list(
    model = "statictable",
    domain = "0000",
    lang = "ind",
    id = "970",
    key = BPS_KEY
  )
)

# parse response
emp_sec_parsed <- content(emp_sec_req, "text") %>% 
  fromJSON()

# last update
emp_sec_last_update <- parse_datetime(emp_sec_parsed$data$updt_date)

# download data as temporary file
GET(
  emp_sec_parsed$data$excel, 
  write_disk(emp_sec_temp <- tempfile(fileext = ".xls"))
)

# import, subset observations for 2011 onward
emp_sec_raw <- read_excel(emp_sec_temp, skip = 20, na = "") %>% 
  dplyr::filter(!is.na(`Lapangan Pekerjaan Utama`))

# date of latest observation
## month
mo <- if (month(Sys.Date()) < 11 && month(Sys.Date()) > 4) {
  "02"
} else {
  "08"
}

## year
yr <- if (month(Sys.Date()) > 4 && month(Sys.Date()) < 12) {
  year(Sys.Date())
} else {
  year(Sys.Date()) - 1
}

## date
emp_date_seq_latest <- str_c(yr, mo, "01", sep = "-")

# date variable
emp_date_seq <- seq(ymd("2011-02-01"), ymd(emp_date_seq_latest), by = "6 month")

# replace variable names
names(emp_sec_raw)[3:ncol(emp_sec_raw)] <- as.character(emp_date_seq)

# subset total workforce
emp_sec_total <- emp_sec_raw %>% 
  dplyr::filter(`Lapangan Pekerjaan Utama` == "Total") %>% 
  select(-c(1:2)) %>% 
  pivot_longer(everything(), names_to = "date", values_to = "workers") %>% 
  mutate(date = ymd(date), workers = as.numeric(workers))

# remove total workforce
emp_sec_raw <- emp_sec_raw %>% 
  dplyr::filter(!is.na(`No.`))

# sector names in english
emp_sec_lab_eng <- emp_sec_raw %>% 
  select(1) %>% 
  mutate(
    label_eng = c(
      "Agriculture, forestry and fishery",
      "Mining and excavation",
      "Manufacturing",
      "Electricity, gas, steam/hot water and cold air procurement",
      "Water, waste and recycling management procurement",
      "Construction",
      "Wholesale and retail trade; car and motorcycle repair and maintenance",
      "Transportation and warehouse",
      "Accommodation and food and beverage services",
      "Information and communication",
      "Financial and insurance services",
      "Real estate",
      "Corporate service",
      "Government, defense and social security administration",
      "Education service",
      "Healthcare and social activity services",
      "Other"
    )
  )

# add anchor to sector keys
emp_sec_lab_eng$No. <- emp_sec_lab_eng$No. %>% 
  str_c("^", ., "$")

# add english sector names
emp_sec_raw$No. <- emp_sec_raw$No. %>% 
  str_replace_all(deframe(emp_sec_lab_eng))

# tidy data
emp_sec_tidy <- emp_sec_raw %>%
  select(-2) %>% 
  pivot_longer(2:ncol(.), names_to = "date", values_to = "workers") %>% 
  mutate(date = ymd(date), workers = as.numeric(workers)) %>% 
  rename(sector = 1)

# add total workforce
emp_sec_tidy <- emp_sec_tidy %>% 
  left_join(emp_sec_total, by = "date") %>% 
  rename(workers = 3, total = 4)

# convert number of workers into millions, calaculate change, distribution
emp_sec_trf <- emp_sec_tidy %>% 
  mutate(mo = month(date)) %>% 
  group_by(sector, mo) %>% 
  mutate(
    workers_diff_yoy = workers - dplyr::lag(workers, 1) ,
    workers_pct_chg_yoy = workers_diff_yoy / dplyr::lag(workers, 1) * 100,
    prop = workers / total * 100,
    prop_chg_yoy = prop - dplyr::lag(prop, 1),
    workers = round(workers / 1000000, 2)
  ) %>% 
  ungroup() %>% 
  select(sector, date, workers, workers_pct_chg_yoy, prop, prop_chg_yoy)

# round to two decimal places
emp_sec_trf[, 4:6] <- lapply(
  emp_sec_trf[, 4:6],
  function(x) {round(x, 2)}
)

# dates of latest observation
emp_sec_date_latest <- emp_sec_trf %>% 
  select(date) %>% 
  dplyr::filter(!duplicated(date)) %>% 
  mutate(month = month(date)) %>% 
  tail(4) %>% 
  dplyr::filter(month == last(month)) %>% 
  select(date) %>% 
  deframe()

# subset latest data
emp_sec_wide <- emp_sec_trf %>% 
  dplyr::filter(date %in% emp_sec_date_latest) %>% 
  pivot_wider(
    names_from = date, 
    values_from = c(workers, workers_pct_chg_yoy, prop, prop_chg_yoy)
  ) %>% 
  select(!matches(str_c("[workers_pct|prop]_chg_yoy_", emp_sec_date_latest[[1]]))) %>%
  rename(
    workers_1 = 2,
    workers_2 = 3,
    workers_pct_chg_yoy = 4,
    prop_1 = 5,
    prop_2 = 6,
    prop_chg_yoy = 7
  )


# export ------------------------------------------------------------------

# latest observation in recent csv
emp_sec_csv_latest <- read_csv("data/ier_employment-sector_cleaned.csv") %>% 
  select(date) %>% 
  dplyr::filter(!duplicated(date), date == last(date)) %>% 
  deframe()


# write csv
if (file.exists("data/ier_employment-sector_cleaned.csv") == F) {
  
  write_csv(emp_sec_trf, "data/ier_employment-sector_cleaned.csv")
  
  message("The number of workers by sector dataset has been exported")
  
} else if (emp_date_seq_latest != emp_sec_csv_latest) {
  
  write_csv(emp_sec_trf, "data/ier_employment-sector_cleaned.csv")
  
  message("The number of workers by sector dataset has been updated")
  
} else {
  
  message("The number of workers by sector dataset is up to date")
  
}


# table -------------------------------------------------------------------

# shared data
emp_sec_wide_shared <- SharedData$new(emp_sec_wide)

# color scale
reactable_col_pal <- function(x) {
  
  negative_values <- scales::col_numeric(
    palette = paletteer::paletteer_c("ggthemes::Red", n = 10) %>% as.character(),
    domain = c(min(emp_sec_wide$prop_chg_yoy), 0),
    na.color = "white",
    reverse = T
  )
  
  positive_values <- scales::col_numeric(
    palette = paletteer::paletteer_c("ggthemes::Blue", n = 10) %>% as.character(),
    domain = c(0, max(emp_sec_wide$prop_chg_yoy)),
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
reactable_emp_sec <- reactable(
  emp_sec_wide_shared,
  columns = list(
    sector = colDef(
      name = "",
      minWidth = 200
    ),
    workers_1 = colDef(
      name = str_c(
        format(emp_sec_date_latest[[1]], "%b '%y"),
        "<br>",
        '<div style="color: #999; font-size: 12px">(millions)</div>'
      ),
      html = T
    ),
    workers_2 = colDef(
      name = str_c(
        format(emp_sec_date_latest[[2]], "%b '%y"),
        "<br>",
        '<div style="color: #999; font-size: 12px">(millions)</div>'
      ),
      html = T
    ),
    workers_pct_chg_yoy = colDef(
      name = str_c(
        "Change",
        "<br>",
        '<div style="color: #999; font-size: 12px">(percent)</div>'
      ),
      html = T
    ),
    prop_1 = colDef(
      name = str_c(
        format(emp_sec_date_latest[[1]], "%b '%y"),
        "<br>",
        '<div style="color: #999; font-size: 12px">(percent)</div>'
      ),
      html = T
    ),
    prop_2 = colDef(
      name = str_c(
        format(emp_sec_date_latest[[2]], "%b '%y"),
        "<br>",
        '<div style="color: #999; font-size: 12px">(percent)</div>'
      ),
      html = T
    ),
    prop_chg_yoy = colDef(
      name = str_c(
        "Change",
        "<br>",
        '<div style="color: #999; font-size: 12px">(% points)</div>'
      ),
      html = T,
      style = function(value) {
        
        bg_color <- reactable_col_pal(value)
        
        font_color <- if (value > .4 || value < -.4) {
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
      name = "<b>Number of workers</b>", 
      columns = c("workers_1", "workers_2", "workers_pct_chg_yoy"),
      html = T
    ),
    colGroup(
      name = "<b>Distribution</b>",
      columns = c("prop_1", "prop_2", "prop_chg_yoy"),
      html = T
    )
  ),
  defaultSortOrder = "asc",
  defaultSorted = "prop_chg_yoy",
  defaultPageSize = 10,
  showPageSizeOptions = T,
  pageSizeOptions = c(10, 17),
  showPageInfo = F,
  highlight = T,
  style = list(fontSize = "15px"),
  theme = reactableTheme(
    headerStyle = list(borderColor = "black")
  )
)

# filter 
filter_emp_sec <- filter_select(
  "emp_sec",
  "Select a sector...",
  emp_sec_wide_shared,
  ~sector
)