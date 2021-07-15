# indonesia economic recovery

# number of poor people

# author: dzulfiqar fathur rahman
# created: 2021-03-05
# last updated: 2021-07-14
# page: poverty


# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)


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
poor_req <- GET(
  base_url,
  query = list(
    model = "data",
    domain = "0000",
    var = "183",
    key = BPS_KEY
  )
)

# parse response
poor_parsed <- content(poor_req, "text") %>% 
  fromJSON()

# extract year key
poor_key_yr <- as_tibble(poor_parsed$tahun)

# data
poor_raw <- as_tibble(poor_parsed$datacontent)

# tidy data
poor_tidy <- poor_raw %>% 
  pivot_longer(1:ncol(.), names_to = "key", values_to = "poor") %>% 
  separate(key, into = c("key_area", "key_date"), sep = "1830") %>% 
  mutate(
    key_yr = as.numeric(
      case_when(
        str_detect(key_date, "^9") ~ str_sub(key_date, 1, 2),
        str_detect(key_date, "^1") ~ str_sub(key_date, 1, 3)
      )
    ),
    key_mo = as.numeric(
      case_when(
        str_detect(key_date, "^9") ~ str_sub(key_date, 3, 4),
        str_detect(key_date, "^1") ~ str_sub(key_date, 4, 5)
      )
    )
  )

# subset 2010s observations, arrange by date, area key
poor_tidy_sub <- poor_tidy %>% 
  dplyr::filter(key_yr > 110) %>% 
  arrange(key_date, key_area)

# replace year key
poor_tidy_sub$key_yr <- poor_tidy_sub$key_yr %>% 
  str_replace_all(deframe(poor_key_yr))

# replace month key
poor_tidy_sub$key_mo <- poor_tidy_sub$key_mo %>% 
  str_replace_all(c("^61$" = "-03-01", "^62$" = "-09-01"))

# create date variable
poor_tidy_sub <- poor_tidy_sub %>% 
  mutate(date = ymd(str_c(key_yr, key_mo))) %>% 
  select(key_area, date, poor)

# replace area key
poor_tidy_sub$key_area <- poor_tidy_sub$key_area %>% 
  str_replace_all(c("1" = "Urban", "2" = "Rural", "3" = "Total")) %>% 
  as_factor()

# calculate annual changes in percentage points
poor_trf <- poor_tidy_sub %>% 
  rename(area = 1) %>% 
  group_by(area) %>% 
  mutate(diff = poor - dplyr::lag(poor, 2))