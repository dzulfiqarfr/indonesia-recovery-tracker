# Inflation


# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(plotly)


# tidy the data -----------------------------------------------------------

# store base url of bps api
base_url <- "https://webapi.bps.go.id/v1/api/list"


# cpi prior to 2020 -------------------------------------------------------

# request the data
cpi_pre2020_req <- GET(
  base_url,
  query = list(
    model = "data",
    domain = "0000",
    var = "2",
    key = BPS_KEY
  )
)

# parse the response
cpi_pre2020_resp <- content(cpi_pre2020_req, "text")

cpi_pre2020_list <- fromJSON(
  cpi_pre2020_resp,
  simplifyDataFrame = T,
  flatten = T
)

# extract keys
bps_ntl_domain_id <- cpi_pre2020_key_city %>% 
  dplyr::filter(label == "INDONESIA") %>% 
  select(1) %>% 
  deframe() %>% 
  as.character()

cpi_pre2020_key_year <- cpi_pre2020_list$tahun %>% 
  as_tibble()

# extract data
cpi_pre2020_df_raw <- cpi_pre2020_list$datacontent %>% 
  as_tibble()

# tidy the data
cpi_pre2020_df_cleaned <- cpi_pre2020_df_raw %>% 
  pivot_longer(
    1:ncol(.),
    names_to = "key",
    values_to = "cpi"
  ) %>% 
  dplyr::filter(str_detect(key, bps_ntl_domain_id)) %>% 
  separate(
    key, 
    into = c(NA, "key_yrmo"), 
    sep = "999920"
  )

# turn key into ymd
cpi_pre2020_df_cleaned$key_yrmo[1:252] <- cpi_pre2020_df_cleaned$key_yrmo[1:252] %>% 
  str_replace_all(
    str_sub(cpi_pre2020_df_cleaned$key_yrmo[1:252], 1, 2),
    rep(str_c(cpi_pre2020_key_year$label[1:21], "-0"), each = 12)
  ) %>% 
  str_replace_all(c("-010" = "-10", "-011" = "-11", "-012" = "-12"))

cpi_pre2020_df_cleaned$key_yrmo[253:nrow(cpi_pre2020_df_cleaned)] <- cpi_pre2020_df_cleaned$key_yrmo[253:nrow(cpi_pre2020_df_cleaned)] %>% 
  str_replace(
    str_sub(cpi_pre2020_df_cleaned$key_yrmo[253:nrow(cpi_pre2020_df_cleaned)], 1, 3),
    rep(str_c(cpi_pre2020_key_year$label[22:nrow(cpi_pre2020_key_year)], "-0"), each = 12)
  ) %>% 
  str_replace_all(c("-010" = "-10", "-011" = "-11", "-012" = "-12"))

# correct data type
cpi_pre2020_df_cleaned[[1]] <- cpi_pre2020_df_cleaned[[1]] %>% 
  str_c("-01") %>% 
  ymd()


# cpi since 2020 ----------------------------------------------------------

# request data
cpi_snc2020_req <- GET(
  base_url,
  query = list(
    model = "data",
    domain = "0000",
    var = "1709",
    key = api_key
  )
)

# parse the response
cpi_snc2020_resp <- content(cpi_snc2020_req, "text")

cpi_snc2020_list <- fromJSON(
  cpi_snc2020_resp,
  simplifyDataFrame = T,
  flatten = T
)

# extract keys
cpi_snc2020_key_year <- cpi_snc2020_list$tahun %>% 
  as_tibble()

# extract data
cpi_snc2020_df_raw <- cpi_snc2020_list$datacontent %>% 
  as_tibble()

# tidy the data
cpi_snc2020_df_cleaned <- cpi_snc2020_df_raw %>% 
  pivot_longer(
    1:ncol(.),
    names_to = "key",
    values_to = "cpi"
  ) %>% 
  dplyr::filter(str_detect(key, bps_ntl_domain_id)) %>% 
  separate(
    key, 
    into = c(NA, "key_yrmo"), 
    sep = "17090"
  )

# turn key into ymd
cpi_snc2020_df_cleaned[[1]] <- cpi_snc2020_df_cleaned[[1]] %>%
  str_replace_all(c("120" = "2020-0", "121" = "2021-0")) %>% 
  str_replace_all(c("010" = "10", "011" = "11", "012" = "12")) %>% 
  str_c(., "-01")

cpi_snc2020_df_cleaned$key_yrmo <- cpi_snc2020_df_cleaned$key_yrmo %>% 
  ymd()


# join the data -----------------------------------------------------------

cpi_tidy <- cpi_pre2020_df_cleaned %>% 
  rbind(cpi_snc2020_df_cleaned) %>% 
  arrange(key_yrmo)

cpi_tidy %>% 
  dplyr::filter(str_detect(key_yrmo, "2020|2021")) %>% 
  ggplot(aes(key_yrmo, cpi)) +
  geom_line()
