# indonesia economic recovery

# inflation by component

# author: dzulfiqar fathur rahman
# created: 2021-03-08
# last updated: 2021-07-15
# page: inflation


# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(httr)
library(jsonlite)
library(plotly)
library(magick)
library(ggtext)


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
inf_comp_req <- GET(
  base_url_static,
  query = list(
    model = "statictable",
    domain = "0000",
    lang = "ind",
    id = "908",
    key = BPS_KEY
  )
)

# parse response
inf_comp_parsed <- content(inf_comp_req, "text") %>% 
  fromJSON()

# download data as temporary file
GET(
  inf_comp_parsed$data$excel, 
  write_disk(inf_comp_temp <- tempfile(fileext = ".xls"))
)

# import
inf_comp_raw <- read_excel(inf_comp_temp, skip = 2, na = "")

# extract years
inf_comp_yr <- inf_comp_raw %>%
  select(`Tahun/Bulan`) %>% 
  dplyr::filter(!is.na(`Tahun/Bulan`)) %>%
  arrange(`Tahun/Bulan`) %>% 
  deframe()

# latest month
inf_comp_mo_latest <- inf_comp_raw %>% 
  dplyr::filter(!is.na(`...2`)) %>% 
  select(`...2`) %>% 
  head(1) %>% 
  deframe()

# month variables
inf_comp_mo_var <- format(seq(ymd("2021-01-01"), ymd("2021-12-01"), "1 month"), "%b") %>% 
  enframe() %>% 
  mutate(
    name = case_when(name < 10 ~ str_c("0", name), TRUE ~ as.character(name)),
    value = case_when(
      value == "May" ~ "Mei",
      value == "Aug" ~ "Agu",
      value == "Oct" ~ "Okt",
      value == "Dec" ~ "Des",
      TRUE ~ value)
  )

# latest month in %m format
inf_comp_mo_m_latest <- inf_comp_mo_var %>% 
  dplyr::filter(value == str_sub(inf_comp_mo_latest, 1, 3)) %>% 
  select(name) %>% 
  deframe()

# date variable
inf_comp_date <- seq(
  ymd(str_c(first(inf_comp_yr), "-01-01")), 
  ymd(str_c(last(inf_comp_yr), inf_comp_mo_m_latest, "-01")),
  by = "month"
)

# sort date
inf_comp_date_sorted <- enframe(inf_comp_date) %>% 
  arrange(desc(value)) %>% 
  select(value) %>% 
  deframe()

# replace date variable
inf_comp_raw <- inf_comp_raw %>% 
  dplyr::filter(is.na(`Tahun/Bulan`)) %>% 
  mutate(`Tahun/Bulan` = inf_comp_date_sorted) %>% 
  rename(date = 1)

# tidy data
inf_comp_tidy <- inf_comp_raw %>% 
  arrange(date) %>% 
  mutate(mo = month(date), yr = year(date)) %>% 
  select(!starts_with("...")) %>% 
  rename(
    overall = Umum,
    core = Inti,
    adm_prices = `Harga Yang Diatur Pemerintah`,
    volatile_prices = `Barang Bergejolak`
  ) %>% 
  select(date, mo, yr, overall, core, adm_prices, volatile_prices) %>% 
  dplyr::filter(yr >= 2020)

# correct data type, round to two decimal places
inf_comp_tidy[, ncol(inf_comp_tidy)] <- lapply(
  inf_comp_tidy[, ncol(inf_comp_tidy)],
  function(x) {round(as.double(x), 2)}
)

# remove overall
inf_comp_tidy <- inf_comp_tidy %>% 
  select(-overall)


# plot --------------------------------------------------------------------

# variable names
inf_comp_var <- names(inf_comp_tidy)[4:6]

# plot
plot_inf_comp <- lapply(
  inf_comp_var,
  function(x) {
    plot_ly(
      inf_comp_tidy,
      type = "scatter",
      mode = "markers+lines",
      line = list(width = 3),
      colors = c("lightgrey", "#2477B3"),
      text = ~format(date, "%B %Y"),
      hovertemplate = "Inflation rate: %{y} percent<br>Date: %{text}<extra></extra>",
      height = 300
    ) %>% 
      add_trace(
        x = ~mo,
        y = as.formula(str_c("~", x)),
        color = ~factor(yr),
        marker = list(size = 6)
      ) %>%
      plotly::layout(
        xaxis = list(
          title = NA,
          autorange = T,
          fixedrange = T,
          tickmode = "array",
          tickvals = c(1, 4, 7, 10),
          ticktext = c("Jan", "Apr", "Jul", "Oct"),
          ticks = "outside",
          automargin = T,
          showline = T,
          showgrid = F
        ),
        yaxis = list(
          title = NA,
          type = "linear",
          autorange = F,
          range = c(-3, 3.1),
          fixedrange = T,
          dtick = 1,
          showline = F,
          showgrid = T,
          gridcolor = "#CFD8DC",
          zerolinecolor = "#E68F7E",
          side = "left"
        ),
        showlegend = F,
        autosize = T
      ) %>% 
      plotly::config(displayModeBar = F)
  }
)

# annotations
## core
anno_sub_core <- list(
  text = "<b>Core<b>",
  font = list(size = 12),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0,
  xanchor = "left",
  yref = "paper",
  y = 1.2,
  yanchor = "top"
)

## administered prices
anno_sub_adm_pr <- list(
  text = "<b>Administered<br>prices<b>",
  font = list(size = 12),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.345,
  xanchor = "left",
  yref = "paper",
  y = 1.2,
  yanchor = "top"
)

## volatile prices
anno_sub_vol <- list(
  text = "<b>Volatile prices<b>",
  font = list(size = 12),
  align = "left",
  showarrow = F,
  xref = "paper",
  x = 0.68,
  xanchor = "left",
  yref = "paper",
  y = 1.2,
  yanchor = "top"
)

## 2020
anno_text_2020 <- list(
  text = "<b>2020</b>",
  font = list(size = 10, color = "lightgrey"),
  align = "left",
  bgcolor = "white",
  showarrow = F,
  xref = "paper",
  x = 0.052,
  xanchor = "left",
  yref = "y",
  y = 0.9,
  yanchor = "top"
) 

## 2021
anno_text_2021 <- list(
  text = "<b>2021</b>",
  font = list(size = 10, color = "#2477B3"),
  align = "left",
  bgcolor = "white",
  showarrow = F,
  xref = "paper",
  x = 0.015,
  xanchor = "left",
  yref = "paper",
  y = 0.475,
  yanchor = "top"
) 

# small multiples
sm_inf_comp <- subplot(
  plot_inf_comp,
  nrows = 1,
  margin = c(0.0125, 0.0125, 0.1, 0.1),
  shareY = T
) %>% 
  plotly::layout(
    yaxis = list(showline = F), 
    annotations = list(
      anno_sub_core,
      anno_sub_adm_pr,
      anno_sub_vol,
      anno_text_2020,
      anno_text_2021
    ),
    margin = list(l = 0, r = 0, t = 50, b = 0)
  )


# export chart ------------------------------------------------------------

# remove month, year variables, reshape, correct component names
inf_comp_tidy_csv <- inf_comp_tidy %>% 
  select(-c("mo", "yr")) %>% 
  rename(Core = 2, `Administered prices` = 3, `Volatile prices` = 4) %>% 
  pivot_longer(2:ncol(.), names_to = "component", values_to = "inflation_mom")

# export chart
if (nrow(inf_comp_tidy_csv) != nrow(read_csv("data/ier_inflation-component_cleaned.csv"))) {
  
  # import functions to apply custom ggplot2 theme and add logo
  source("script/ggplot2_theme.R")
  
  # annotations
  anno_year <- tibble(
    x = c(1, 4),
    y = c(-0.5, 0.75),
    label = c("2021", "2020"),
    component = as_factor(rep("core", 2))
  )
  
  # plot
  inf_comp_tidy %>% 
    pivot_longer(4:ncol(.), names_to = "component", values_to = "inflation_rate") %>% 
    mutate(component = factor(component, levels = c("core", "adm_prices", "volatile_prices"))) %>% 
    ggplot(aes(mo, inflation_rate)) +
    geom_hline(yintercept = 0, color = "#E68F7E") +
    geom_line(aes(color = as_factor(yr)), lwd = 1, show.legend = F) +
    geom_point(aes(color = as_factor(yr)), size = 1.5, show.legend = F) +
    scale_x_continuous(
      breaks = c(1, 4, 7, 10),
      labels = c("Jan", "April", "Jul", "Oct")
    ) +
    scale_y_continuous(
      breaks = seq(-3, 3, 1),
      limits = c(-3, 3),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_color_manual(values = c("lightgrey", "#2477B3")) +
    geom_richtext(
      data = anno_year,
      aes(x, y, label = label),
      fill = NA,
      label.color = NA,
      text.color = c("#2477B3", "lightgrey"),
      hjust = 0,
      size = 3,
      fontface = "bold"
    ) +
    labs(
      title = "Monthly inflation",
      subtitle = "By component (percent)",
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Statistics Indonesia (BPS)"
    ) +
    facet_wrap(
      ~ component, 
      nrow = 1,
      labeller = labeller(
        component = c(
          core = "Core", 
          adm_prices = "Administered prices",
          volatile_prices = "Volatile prices"
        )
      )
    ) +
    theme_ier() +
    theme(
      panel.spacing = unit(1, "lines"),
      strip.background = element_blank(),
      strip.text = element_text(
        size = rel(0.8),
        hjust = 0,
        vjust = 1, 
        margin = margin(b = 10)
      )
    )
  
  # path to the plot
  path_plot_inf_comp <- "fig/ier_inflation-component_plot.png"
  
  # save the plot
  ggsave(path_plot_inf_comp, width = 6, height = 3.708, dpi = 300)
  
  # add ier logo to the plot
  add_ier_logo(path_plot_inf_comp)
  
  # message
  message("The inflation by component chart has been updated")
  
} else {
  
  message("The inflation by component chart is up to date")
  
}


# export data -------------------------------------------------------------

if (!file.exists("data/ier_inflation-component_cleaned.csv")) {
  
  write_csv(inf_comp_tidy_csv, "data/ier_inflation-component_cleaned.csv")
  
  message("The inflation by component dataset has been exported")
  
} else if (nrow(inf_comp_tidy_csv) != nrow(read_csv("data/ier_inflation-component_cleaned.csv"))) {
  
  write_csv(inf_comp_tidy_csv, "data/ier_inflation-component_cleaned.csv")
  
  message("The inflation by component dataset has been updated")
  
} else {
  
  message("The inflation by component dataset is up to date")
  
}