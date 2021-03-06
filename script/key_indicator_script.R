# indonesia economic recovery

# key indicator tables

# author: dzulfiqar fathur rahman
# created: 2021-02-21
# last updated: 2021-07-14
# page: index


# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(httr)
library(jsonlite)
library(reactable)
library(htmltools)
library(rvest)


# data --------------------------------------------------------------------

# api key
BPS_KEY <- Sys.getenv("BPS_KEY")

# bps api url
## dyanmic table
base_url_dynamic <- "https://webapi.bps.go.id/v1/api/list"

## statictable
base_url_static <- "https://webapi.bps.go.id/v1/api/view"


## economic indicators ----

### gdp ----

# request data
growth_req <- GET(
  base_url_dynamic,
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

# separate keys, subset quarterly observations
growth_tidy <- growth_raw %>% 
  pivot_longer(1:ncol(.), names_to = "key", values_to = "pct_change_yoy") %>% 
  separate(key, into = c("key_exp", "key_period"), sep = "108") %>%
  mutate(
    key_period_obs = str_sub(key_period, end = 1),
    key_yr = str_sub(key_period, 2, 4),
    key_q = str_sub(key_period, 5, 6)
  ) %>% 
  dplyr::filter(key_exp == "800", key_period_obs == "5", key_q != "35")

# replace year key
growth_tidy$key_yr <- growth_tidy$key_yr %>% 
  str_replace_all(deframe(growth_key_yr))

# replace quarter key
growth_tidy$key_q <- growth_tidy$key_q %>% 
  str_replace_all(
    c(
      "^31$" = "-01-01",
      "^32$" = "-04-01",
      "^33$" = "-07-01",
      "^34$" = "-10-01"
    )
  )

# create date variable
growth_tidy <- growth_tidy %>% 
  mutate(date = ymd(str_c(key_yr, key_q))) %>% 
  select(date, pct_change_yoy)


### inflation ----

# request data
inf_yoy_req <- GET(
  base_url_static,
  query = list(
    model = "statictable",
    domain = "0000",
    lang = "ind",
    id = "915",
    key = BPS_KEY
  )
)

# parse response
inf_yoy_parsed <- content(inf_yoy_req, "text") %>% 
  fromJSON()

# download data as temporary file
GET(
  inf_yoy_parsed$data$excel, 
  write_disk(inf_yoy_temp <- tempfile(fileext = ".xls"))
)

# import
inf_yoy_raw <- read_excel(inf_yoy_temp, skip = 2, na = "")

# latest year
inf_yoy_yr_latest <- names(inf_yoy_raw)[ncol(inf_yoy_raw)]

# date variable
inf_date_seq <- seq(
  ymd(str_c(inf_yoy_yr_latest, "01-01")), 
  ymd(str_c(inf_yoy_yr_latest, "12-01")), 
  by = "month"
)

# replace month names
inf_yoy_raw$Bulan[1:12] <- as.character(inf_date_seq)

# correct date type
inf_yoy_raw$Bulan <- ymd(inf_yoy_raw$Bulan)

# remove table notes
inf_yoy_raw <- inf_yoy_raw %>% 
  dplyr::filter(!is.na(Bulan))

# tidy data
inf_yoy_tidy <- inf_yoy_raw %>% 
  pivot_longer(2:ncol(.), names_to = "yr", values_to = "rate_yoy") %>% 
  mutate(mo = format(Bulan, "-%m-01")) %>% 
  arrange(yr, mo) %>% 
  group_by(yr, Bulan) %>% 
  mutate(date = str_c(yr, mo), mo = month(date)) %>% 
  ungroup() %>% 
  select(date, mo, yr, rate_yoy) %>% 
  dplyr::filter(!is.na(rate_yoy), yr >= 2020)

# correct data type
inf_yoy_tidy$date <- ymd(inf_yoy_tidy$date)


### poverty ----

# request data
pov_req <- GET(
  base_url_dynamic,
  query = list(
    model = "data",
    domain = "0000",
    var = "184",
    key = BPS_KEY
  )
)

# parse response
pov_parsed <- content(pov_req, "text") %>% 
  fromJSON()

# extract year key
pov_key_yr <- as_tibble(pov_parsed$tahun)

# extract data
pov_raw <- as_tibble(pov_parsed$datacontent)

# tidy data
pov_tidy <- pov_raw %>% 
  pivot_longer(1:ncol(.), names_to = "key", values_to = "pov_rate") %>% 
  separate(key, into = c("key_area", "key_date"), sep = "1840") %>% 
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
pov_tidy_sub <- pov_tidy %>% 
  dplyr::filter(key_yr > 110) %>% 
  arrange(key_date, key_area)

# replace year key
pov_tidy_sub$key_yr <- pov_tidy_sub$key_yr %>% 
  str_replace_all(deframe(pov_key_yr))

# replace month key
pov_tidy_sub$key_mo <- pov_tidy_sub$key_mo %>% 
  str_replace_all(c("^61$" = "-03-01", "^62$" = "-09-01"))

# create date variable
pov_tidy_sub <- pov_tidy_sub %>% 
  mutate(date = ymd(str_c(key_yr, key_mo))) %>% 
  select(key_area, date, pov_rate)

# replace area key
pov_tidy_sub$key_area <- pov_tidy_sub$key_area %>% 
  str_replace_all(c("1" = "Urban", "2" = "Rural", "3" = "Total")) %>% 
  as_factor()


### unemployment ----

# request data
unemployment_req <- GET(
  base_url_dynamic,
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

# subset unemployment rate
unemp_rate <- unemployment_tidy %>% 
  dplyr::filter(key_act == 6, yr >= 2005) %>% 
  select(-c(1, 2, 4, 5)) %>% 
  rename(unemployment_rate = val)


### manufacturing pmi ----

# url for indonesia's manufacturing pmi data on investing.com
base_url_investing <- "https://www.investing.com/economic-calendar/indonesia-nikkei-pmi-1096"

# scrape pmi data
pmi_raw <- read_html(base_url_investing) %>% 
  html_elements("#eventTabDiv_history_0 tbody") %>% 
  html_elements("tr") %>% 
  html_text2()

# subset latest pmi data
pmi_unlist <- pmi_raw %>% 
  .[[2]] %>% 
  str_split("\\t") %>% 
  unlist()

# extract latest date in most recent pmi data
pmi_latest_date <- str_c(
  str_sub(pmi_unlist[[1]], 15, 17),
  format(Sys.Date(), " '%y")
)

# tidy pmi data
pmi_latest <- tibble(
  indicator = "Manufacturing PMI",
  unit = "(index point)",
  latest_date = pmi_latest_date,
  latest_fig = str_c(pmi_unlist[[3]], "<sup>&#8225;</sup>")
)


### merge indicators ----

# extract latest data, add indicator, unit variables
## gdp
growth_latest <- growth_tidy %>% 
  dplyr::filter(date == last(date)) %>% 
  mutate(
    indicator = "Economic growth",
    unit = "(percent)",
    latest_date = str_c("Q", quarter(date), " ", format(date, "'%y"))
  ) %>% 
  rename(latest_fig = pct_change_yoy) %>% 
  select(indicator, unit, latest_date, latest_fig)

## inflation
inf_latest <- inf_yoy_tidy %>% 
  dplyr::filter(date == last(date)) %>% 
  mutate(
    indicator = "Inflation rate",
    unit = "(percent)",
    latest_date = format(date, "%b '%y")
  ) %>% 
  rename(latest_fig = rate_yoy) %>% 
  select(indicator, unit, latest_date, latest_fig)

## poverty
pov_latest <- pov_tidy_sub %>% 
  dplyr::filter(key_area == "Total", date == last(date)) %>% 
  mutate(
    indicator = "Poverty rate",
    unit = "(percent)",
    latest_date = format(date, "%b '%y")
  ) %>% 
  rename(latest_fig = pov_rate) %>% 
  select(indicator, unit, latest_date, latest_fig)

## unemployment
unemp_latest <- unemp_rate %>% 
  dplyr::filter(date == last(date)) %>% 
  mutate(
    indicator = "Unemployment rate",
    unit = "(percent)",
    latest_date = format(date, "%b '%y")
  ) %>% 
  rename(latest_fig = unemployment_rate) %>% 
  select(indicator, unit, latest_date, latest_fig)

# merge data
key_indicators <- growth_latest %>% 
  rbind(inf_latest, pov_latest, unemp_latest, pmi_latest)


### add projection, govt target ----

# projection
key_ind_proj <- tribble(
  ~indicator, ~proj_fig,
  "Economic growth", "3.7-4.5*",
  "Inflation rate", "2.3**",
  "Poverty rate", "9.2-9.7*",
  "Unemployment rate", "6.5<sup>&#8224;</sup>",
  "Manufacturing PMI", "-"
)

# merge latest data with projection, government target
key_econ_ind <- key_indicators %>% 
  left_join(key_ind_proj, by = "indicator")


## covid-19 indicators ----

# import
covid_raw <- read_csv(
  "https://covid.ourworldindata.org/data/owid-covid-data.csv",
  na = "",
  col_types = cols(
    hosp_patients = col_number(),
    hosp_patients_per_million = col_number(),
    icu_patients = col_number(),
    icu_patients_per_million = col_number(),
    weekly_hosp_admissions = col_number(),
    weekly_hosp_admissions_per_million = col_number(),
    weekly_icu_admissions = col_number(),
    weekly_icu_admissions_per_million = col_number()
  )
)

# tidy
covid_ntl <- covid_raw %>%
  select(
    iso_code,
    location,
    date,
    total_cases,
    new_cases,
    total_deaths,
    new_deaths,
    new_tests,
    total_tests,
    tests_per_case,
    positive_rate,
    total_vaccinations,
    total_vaccinations_per_hundred,
    people_vaccinated,
    people_vaccinated_per_hundred,
    people_fully_vaccinated,
    people_fully_vaccinated_per_hundred,
    population
  ) %>% 
  dplyr::filter(iso_code == "IDN")

# latest daily positive rate
pos_rate <- covid_ntl %>% 
  select(date, positive_rate) %>% 
  dplyr::filter(!is.na(positive_rate)) %>% 
  tail(1) %>%
  mutate(
    indicator = "Daily positive rate",
    unit = "(percent)",
    date = format(date, "%b %d, '%y"),
    positive_rate = positive_rate * 100
  ) %>% 
  rename(latest_date = date, latest_fig = positive_rate)

# latest daily new case, death
case_death <- covid_ntl %>% 
  select(date, new_cases, new_deaths) %>% 
  dplyr::filter(date == last(date)) %>% 
  pivot_longer(2:ncol(.), names_to = "indicator", values_to = "latest_fig") %>% 
  mutate(
    date = format(date, "%b %d, '%y"),
    indicator = str_replace_all(
      indicator,
      c(
        new_cases = "Daily new cases",
        new_deaths = "Daily new deaths"
      )
    ),
    unit = c(rep("(people)", 2)),
    latest_fig = format(latest_fig, big.mark = ",")
  ) %>% 
  rename(latest_date = date)

# latest people vaccinated
vaccination <- covid_ntl %>% 
  select(
    date, 
    people_vaccinated_per_hundred, 
    people_fully_vaccinated_per_hundred
  ) %>% 
  dplyr::filter(!is.na(people_vaccinated_per_hundred)) %>% 
  tail(1) %>% 
  pivot_longer(2:ncol(.), names_to = "indicator", values_to = "latest_fig") %>% 
  mutate(
    date = format(date, "%b %d, '%y"),
    indicator = str_replace_all(
      indicator,
      c(
        people_vaccinated_per_hundred = "Share of population who have received at least one dose of vaccine",
        people_fully_vaccinated_per_hundred = "Share of population fully vaccinated"
      )
    ),
    unit = c(rep("(percent)", 2))
  ) %>% 
  rename(latest_date = date)

# merge, create empty column for projection
key_covid_ind <- case_death %>% 
  rbind(pos_rate, vaccination) %>% 
  select(indicator, unit, latest_date, latest_fig) %>%
  arrange(indicator)


# table -------------------------------------------------------------------

## economic indicators ----
reactable_key_econ <- key_econ_ind %>% 
  reactable(
    columns = list(
      indicator = colDef(
        name = "",
        cell = function(value, index) {
          
          unit <- key_econ_ind$unit[index]
          
          tagList(
            div(value),
            div(style = list(fontSize = 12, color = "#999"), unit)
          )
          
        },
        minWidth = 200
      ),
      unit = colDef(show = F),
      latest_date = colDef(
        name = "Date",
        align = "right"
      ),
      latest_fig = colDef(
        name = "Figure",
        align = "right",
        html = T
      ),
      proj_fig = colDef(
        name = str_c(
          "2021<br>",
          '<div style="color: #999; font-size: 12px">(projection)</div>'
        ),
        html = T,
        align = "right"
      )
    ),
    columnGroups = list(
      colGroup(
        name = "Latest",
        columns = c("latest_date", "latest_fig"),
        html = T
      )
    ),
    sortable = F,
    compact = T,
    striped = T,
    style = list(fontSize = "14px"),
    theme = reactableTheme(
      headerStyle = list(borderColor = "black"),
      cellStyle = list(
        display = "flex",
        flexDirection = "column",
        justifyContent = "center"
      ),
      stripedColor = "#ECEFF1"
    )
  )

## covid-19 ----
reactable_key_covid <- key_covid_ind %>% 
  reactable(
    columns = list(
      indicator = colDef(
        name = "",
        cell = function(value, index) {
          
          unit <- key_covid_ind$unit[index]
          
          tagList(
            div(value),
            div(style = list(fontSize = 12, color = "#999"), unit)
          )
          
        },
        minWidth = 200
      ),
      unit = colDef(show = F),
      latest_date = colDef(
        name = "Date",
        align = "right"
      ),
      latest_fig = colDef(
        name = "Figure",
        align = "right"
      )
    ),
    columnGroups = list(
      colGroup(
        name = "Latest",
        columns = c("latest_date", "latest_fig"),
        html = T
      )
    ),
    sortable = F,
    compact = T,
    striped = T,
    style = list(fontSize = "14px"),
    theme = reactableTheme(
      headerStyle = list(borderColor = "black"),
      cellStyle = list(
        display = "flex",
        flexDirection = "column",
        justifyContent = "center"
      ),
      stripedColor = "#ECEFF1"
    )
  )
