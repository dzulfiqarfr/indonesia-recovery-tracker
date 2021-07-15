# indonesia economic recovery

# poverty line

# author: dzulfiqar fathur rahman
# created: 2021-03-07
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
pov_line_req <- GET(
  base_url,
  query = list(
    model = "data",
    domain = "0000",
    var = "182",
    key = BPS_KEY
  )
)

# parse response
pov_line_parsed <- content(pov_line_req, "text") %>% 
  fromJSON()

# extract keys
## year
pov_line_key_yr <- as_tibble(pov_line_parsed$tahun)

## data
pov_line_raw <- as_tibble(pov_line_parsed$datacontent)

# tidy data
pov_line_tidy <- pov_line_raw %>% 
  pivot_longer(1:ncol(.), names_to = "key", values_to = "pov_line") %>% 
  separate(key, into = c("area", "date"), sep = "1820") %>% 
  dplyr::filter(!str_detect(date, "^9")) %>% 
  mutate(yr = str_sub(date, 1, 3), mo = str_sub(date, 4, 5)) %>% 
  select(area, date, mo, yr, pov_line) %>% 
  arrange(date, area)

# replace area keys
pov_line_tidy$area <- pov_line_tidy$area %>% 
  str_replace_all(c("1" = "Urban", "2" = "Rural", "3" = "Total"))

# replace period keys
pov_line_tidy$mo <- pov_line_tidy$mo %>% 
  str_replace_all(c("61" = "-03-01", "62" = "-09-01", "63" = "-01-01"))

# replace year keys
pov_line_tidy$yr <- pov_line_tidy$yr %>% 
  str_replace_all(deframe(pov_line_key_yr))

# create date variable, remove date keys
pov_line_trf <- pov_line_tidy %>% 
  group_by(yr, mo, area) %>% 
  rowwise() %>% 
  mutate(date = str_c(yr, mo)) %>% 
  ungroup() %>% 
  select(area, date, pov_line)

# correct data types
pov_line_trf$area <- as.factor(pov_line_trf$area)
pov_line_trf$date <- ymd(pov_line_trf$date)