# indonesia economic recovery

# employment
# by status

# author: dzulfiqar fathur rahman
# created: 2021-03-25
# last updated: 2021-06-05
# page: employment


# setup -------------------------------------------------------------------

# packages
library(tidyverse)
library(readxl)
library(lubridate)
library(httr)
library(jsonlite)
library(reactable)
library(htmltools)
library(paletteer)

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
emp_status_req <- GET(
  base_url_static,
  query = list(
    model = "statictable",
    domain = "0000",
    lang = "ind",
    id = "971",
    key = BPS_KEY
  )
)

# parse response
emp_status_parsed <- content(emp_status_req, "text") %>% 
  fromJSON()

# last update
emp_status_last_update <- parse_datetime(emp_status_parsed$data$updt_date)

# download data as temporary file
GET(
  emp_status_parsed$data$excel, 
  write_disk(emp_status_temp <- tempfile(fileext = ".xls"))
)

# import, remove observations before 2000s onward
emp_status_raw <- read_excel(emp_status_temp, skip = 2, na = c("", "-")) %>% 
  dplyr::filter(!is.na(`Status Pekerjaan Utama`)) %>% 
  select(!starts_with("1")) %>% 
  select(!matches("0[01234]$"))

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
emp_status_date_seq <- seq(ymd("2005-02-01"), ymd(emp_date_seq_latest), by = "6 month")

# replace variable names
names(emp_status_raw)[3:ncol(emp_status_raw)] <- as.character(emp_status_date_seq)

# subset total workforce
emp_status_total <- emp_status_raw %>% 
  dplyr::filter(`Status Pekerjaan Utama` ==  "Total") %>% 
  select(-c(1:2)) %>% 
  pivot_longer(everything(), names_to = "date", values_to = "workers") %>% 
  mutate(date = ymd(date), workers = as.numeric(workers))

# remove total workforce, no answer
emp_status_raw <- emp_status_raw %>% 
  dplyr::filter(!is.na(`No.`), `Status Pekerjaan Utama` != "Tak Terjawab")

# work status in english
emp_status_lab_eng <- emp_status_raw %>% 
  select(1) %>% 
  mutate(
    label_eng = c(
      "Own-account workers",
      "Employers assisted by temporary/unpaid workers",
      "Employers assisted by full time/paid workers",
      "Employees",
      "Casual employees in agriculture",
      "Casual employees in non-agriculture",
      "Unpaid family workers"
    )
  )

# add english status names
emp_status_raw$No. <- emp_status_raw$No. %>% 
  str_replace_all(deframe(emp_status_lab_eng))

# tidy data
emp_status_tidy <- emp_status_raw %>%
  select(-2) %>% 
  pivot_longer(2:ncol(.), names_to = "date", values_to = "workers") %>% 
  mutate(date = ymd(date), workers = as.numeric(workers)) %>% 
  rename(status = 1)

# add total workforce
emp_status_tidy <- emp_status_tidy %>% 
  left_join(emp_status_total, by = "date") %>% 
  rename(workers = 3, total = 4)

# convert number of workers into millions, calaculate change, distribution
emp_status_trf <- emp_status_tidy %>% 
  mutate(mo = month(date)) %>% 
  group_by(status, mo) %>% 
  mutate(
    workers_diff_yoy = workers - dplyr::lag(workers, 1) ,
    workers_pct_chg_yoy = workers_diff_yoy / dplyr::lag(workers, 1) * 100,
    prop = workers / total * 100,
    prop_chg_yoy = prop - dplyr::lag(prop, 1),
    workers = round(workers / 1000000, 2)
  ) %>% 
  ungroup() %>% 
  select(status, date, workers, workers_pct_chg_yoy, prop, prop_chg_yoy)

# round to two decimal places
emp_status_trf[, 4:6] <- lapply(
  emp_status_trf[, 4:6],
  function(x) {round(x, 2)}
)

# add category
emp_status_trf <- emp_status_trf %>% 
  mutate(
    formal_category = case_when(
      status %in% c("Employers assisted by full time/paid workers", "Employees") ~ "Formal",
      TRUE ~ "Informal"
    )
  ) %>% 
  select(status, formal_category, date, workers, workers_pct_chg_yoy, prop, prop_chg_yoy)

# dates of latest observation
emp_status_date_latest <- emp_status_trf %>% 
  select(date) %>% 
  dplyr::filter(!duplicated(date)) %>% 
  mutate(month = month(date)) %>% 
  tail(4) %>% 
  dplyr::filter(month == last(month)) %>% 
  select(date) %>% 
  deframe()

# subset latest data
emp_status_wide <- emp_status_trf %>% 
  dplyr::filter(date %in% emp_status_date_latest) %>% 
  pivot_wider(
    names_from = date, 
    values_from = c(workers, workers_pct_chg_yoy, prop, prop_chg_yoy)
  ) %>% 
  select(!matches(str_c("[workers_pct|prop]_chg_yoy_", emp_status_date_latest[[1]]))) %>%
  rename(
    workers_1 = 3,
    workers_2 = 4,
    workers_pct_chg_yoy = 5,
    prop_1 = 6,
    prop_2 = 7,
    prop_chg_yoy = 8
  )


# export ------------------------------------------------------------------

# latest observation in recent csv
emp_status_csv_latest <- read_csv("data/ier_employment-status_cleaned.csv") %>% 
  select(date) %>% 
  dplyr::filter(!duplicated(date), date == last(date)) %>% 
  deframe()


# write csv
if (file.exists("data/ier_employment-status_cleaned.csv") == F) {
  
  write_csv(emp_status_trf, "data/ier_employment-status_cleaned.csv")
  
  message("The number of workers by sector dataset has been exported")
  
} else if (emp_date_seq_latest != emp_status_csv_latest) {
  
  write_csv(emp_status_trf, "data/ier_employment-status_cleaned.csv")
  
  message("The number of workers by sector dataset has been updated")
  
} else {
  
  message("The number of workers by sector dataset is up to date")
  
}


# table -------------------------------------------------------------------

# color scale
reactable_col_pal <- function(x) {
  
  negative_values <- scales::col_numeric(
    palette = paletteer::paletteer_c("ggthemes::Red", n = 10) %>% as.character(),
    domain = c(min(emp_status_wide$prop_chg_yoy), 0),
    na.color = "white",
    reverse = T
  )
  
  positive_values <- scales::col_numeric(
    palette = paletteer::paletteer_c("ggthemes::Blue", n = 10) %>% as.character(),
    domain = c(0, max(emp_status_wide$prop_chg_yoy)),
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
reactable_emp_status <- reactable(
  emp_status_wide,
  columns = list(
    status = colDef(
      name = "",
      minWidth = 200,
      cell = function(value, index) {
        
        formal_category <- emp_status_wide$formal_category[index]
        
        tagList(
          div(value),
          div(style = list(fontSize = 12, color = "#999"), formal_category)
        )
        
      }
    ),
    formal_category = colDef(show = F),
    workers_1 = colDef(
      name = str_c(
        format(emp_status_date_latest[[1]], "%b '%y"),
        "<br>",
        '<div style="color: #999; font-size: 12px">(millions)</div>'
      ),
      html = T
    ),
    workers_2 = colDef(
      name = str_c(
        format(emp_status_date_latest[[2]], "%b '%y"),
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
        format(emp_status_date_latest[[1]], "%b '%y"),
        "<br>",
        '<div style="color: #999; font-size: 12px">(percent)</div>'
      ),
      html = T
    ),
    prop_2 = colDef(
      name = str_c(
        format(emp_status_date_latest[[2]], "%b '%y"),
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
        
        font_color <- if (value > .75 || value < -2.5) {
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
  highlight = T,
  compact = T,
  style = list(fontSize = "15px"),
  theme = reactableTheme(
    headerStyle = list(borderColor = "black")
  )
)
