# indonesia economic recovery

# number of unemployed workers

# author: dzulfiqar fathur rahman
# created: 2021-03-25
# last updated: 2021-07-13
# page: employment


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

if (!exists("unemployment_req")) {
  
  # request data
  unemployment_req <- GET(
    base_url,
    query = list(
      model = "data",
      domain = "0000",
      var = "529",
      key = BPS_KEY
    )
  )
  
  # parse response
  unemployment_parsed <- content(unemployment_req, "text") %>% 
    fromJSON()
  
  # extract keys
  ## activities
  unemployment_key_act <- as_tibble(unemployment_parsed$vervar)
  
  ## year
  unemployment_key_yr <- as_tibble(unemployment_parsed$tahun)
  
  # extract data
  unemployment_raw <- as_tibble(unemployment_parsed$datacontent)
  
  # tidy data
  unemployment_tidy <- unemployment_raw %>% 
    pivot_longer(1:ncol(.), names_to = "key", values_to = "val") %>% 
    separate(key, into = c("key_act", "key_date"), sep = "5290") %>% 
    mutate(
      key_yr = case_when(
        str_detect(key_date, "^[8|9]") ~ as.numeric(str_sub(key_date, 1, 2)),
        str_detect(key_date, "^1") ~ as.numeric(str_sub(key_date, 1, 3))
      ),
      key_mo = case_when(
        str_detect(key_date, "^[8|9]") ~ as.numeric(str_sub(key_date, 3, 5)),
        str_detect(key_date, "^1") ~ as.numeric(str_sub(key_date, 4, 6))
      )
    ) %>% 
    select(key_act, key_date, key_yr, key_mo, val)
  
  # add anchor to year key
  unemployment_key_yr$val <- str_c("^", unemployment_key_yr$val, "$")
  
  # replace year key
  unemployment_tidy$key_yr <- unemployment_tidy$key_yr %>% 
    str_replace_all(deframe(unemployment_key_yr)) %>% 
    as.numeric()
  
  # replace month key
  unemployment_tidy$key_mo <- unemployment_tidy$key_mo %>% 
    str_replace_all(c("^189$" = "-02-01", "^190$" = "-08-01", "^191$" = "-01-01"))
  
  # create date variable
  unemployment_tidy <- unemployment_tidy %>% 
    mutate(date = ymd(str_c(key_yr, key_mo)), mo = month(date)) %>% 
    rename(yr = key_yr) %>% 
    select(key_act, date, yr, mo, val)
  
  # add activity labels
  unemployment_tidy <- unemployment_tidy %>% 
    mutate(act = str_replace_all(unemployment_tidy$key_act, deframe(unemployment_key_act))) %>% 
    select(key_act, act, date, yr, mo, val)
  
}

# subset number of unemployed people
unemployed_ppl <- unemployment_tidy %>% 
  dplyr::filter(key_act == 5, yr >= 2005) %>% 
  select(-c(1, 2, 4))

# calculate change in number of unemployed people, rename variable
unemp_ppl_trf <- unemployed_ppl %>% 
  group_by(mo) %>% 
  mutate(change_yoy = val - dplyr::lag(val, 1)) %>% 
  ungroup() %>% 
  rename(unemployed_ppl = val) %>% 
  select(-mo)

# divide by a million, round to two decimal places
unemp_ppl_trf[, 2:ncol(unemp_ppl_trf)] <- lapply(
  unemp_ppl_trf[, 2:ncol(unemp_ppl_trf)],
  function(x) round(x / 1000000, 2)
)