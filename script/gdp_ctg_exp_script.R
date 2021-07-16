# indonesia economic recovery

# contribution to gdp growth by expenditure

# author: dzulfiqar fathur rahman
# created: 2021-02-23
# last updated: 2021-07-15
# page: gdp


# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
library(plotly)
library(magick)


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
ctg_req <- GET(
  base_url,
  query = list(
    model = "data",
    domain = "0000",
    var = "1956",
    key = BPS_KEY
  )
)

# parse response
ctg_parsed <- content(ctg_req, "text") %>% 
  fromJSON()

# extract keys
## expenditure
ctg_key_exp <- as_tibble(ctg_parsed$vervar)

## year
ctg_key_yr <- as_tibble(ctg_parsed$tahun)

## data
ctg_raw <- as_tibble(ctg_parsed$datacontent)

# subset quarterly observations
ctg_exp_tidy <- ctg_raw %>% 
  pivot_longer(1:ncol(.), names_to = "key", values_to = "exp") %>% 
  separate(key, into = c("key_exp", "key_period"), sep = "19560") %>% 
  mutate(
    key_yr = str_sub(key_period, 1, 3),
    key_q = str_sub(key_period, 4, 5)
  ) %>% 
  dplyr::filter(key_q != "35")

# replace year key
ctg_exp_tidy$key_yr <- ctg_exp_tidy$key_yr %>% 
  str_replace_all(deframe(ctg_key_yr))

# replace quarter key
ctg_exp_tidy$key_q <- ctg_exp_tidy$key_q %>% 
  str_replace_all(
    c(
      "^31$" = "-01-01",
      "^32$" = "-04-01",
      "^33$" = "-07-01",
      "^34$" = "-10-01"
    )
  )

# create date variable
ctg_exp_tidy <- ctg_exp_tidy %>% 
  mutate(date = ymd(str_c(key_yr, key_q))) %>% 
  select(key_exp, date, exp)

# subset main components
## main component expenditure keys
ctg_key_exp_main <- c(100, 200, 300, 400, 500, 600, 700, 790)

## subset
ctg_main_exp <- ctg_exp_tidy %>% 
  dplyr::filter(key_exp %in% ctg_key_exp_main)

# replace expenditure key
## indonesian
ctg_main_exp$key_exp <- ctg_main_exp$key_exp %>% 
  str_replace_all(deframe(ctg_key_exp))

## english
ctg_main_exp$key_exp <- ctg_main_exp$key_exp %>% 
  str_replace_all(
    c(
      "1. Pengeluaran Konsumsi Rumahtangga" = "Household consumption",
      "2. Pengeluaran Konsumsi LNPRT" = "NPISHs consumption",
      "3. Pengeluaran Konsumsi Pemerintah" = "Government consumption",
      "4. Pembentukan Modal Tetap Domestik Bruto" = "Gross fixed capital formation",
      "5. Perubahan Inventori" = "Changes in inventories",
      "6. Ekspor Barang dan Jasa" = "Exports of goods and services",
      "7. Dikurangi Impor Barang dan Jasa" = "Imports of goods and services",
      "Diskrepansi Statistik" = "Statistic discrepancy"
    )
  )

# calculate private consumption, investment, net export
ctg_main_exp_wide <- ctg_main_exp %>% 
  pivot_wider(names_from = key_exp, values_from = exp) %>% 
  rowwise() %>% 
  mutate(
    `Private consumption` = sum(`Household consumption`, `NPISHs consumption`),
    Investment = sum(`Gross fixed capital formation`, `Changes in inventories`),
    `Net export` = `Exports of goods and services` - `Imports of goods and services`
  ) %>% 
  select(
    date,
    `Private consumption`,
    Investment,
    `Net export`,
    `Government consumption`,
    `Statistic discrepancy`
  )

# subset total gdp
ctg_gdp <- ctg_exp_tidy %>% 
  dplyr::filter(key_exp == "800")

# add total gdp to main expenditure data
ctg_exp_wide <- ctg_main_exp_wide %>% 
  left_join(ctg_gdp, by = "date") %>% 
  select(-key_exp) %>% 
  rename(gdp = ncol(.))

# calculate change, proportion, contribution to growth
ctg_exp_trf <- ctg_exp_wide %>% 
  pivot_longer(2:(ncol(.) - 1), names_to = "comp", values_to = "exp") %>% 
  select(comp, date, exp, gdp) %>% 
  group_by(comp) %>% 
  mutate(
    diff = exp - dplyr::lag(exp, 4),
    pct_change = diff / dplyr::lag(exp, 4) * 100,
    prop = exp / gdp * 100,
    ctg = pct_change * dplyr::lag(prop, 4) / 100
  ) %>% 
  dplyr::filter(!is.na(ctg)) %>% 
  select(comp, date, ctg) %>% 
  pivot_wider(names_from = comp, values_from = ctg)

# round contribution to growth to two decimal places
ctg_exp_trf[, 2:ncol(ctg_exp_trf)] <- lapply(
  ctg_exp_trf[, 2:ncol(ctg_exp_trf)],
  function(x) {round(x, digits =2)}
)


# plot --------------------------------------------------------------------

# quarter labels
labs_q <- str_c("Q", quarter(ctg_exp_trf$date))

# y-axis range
ctg_y_axis_range <- c(-12, 13)

# plot
plot_ctg <- plot_ly(
  data = ctg_exp_trf,
  type = "bar",
  name = "Private consumption",
  x = ~date,
  y = ~`Private consumption`,
  text = labs_q,
  marker = list(
    color = "#2477B3",
    line = list(width = 0.25, color = "white")
  ),
  hovertemplate = str_c(
    "<b>Component: private consumption</b><br><br>",
    "Contribution: %{y} percentage points<br>",
    "Date: %{text} ",
    "%{x}",
    "<extra></extra>"
  ),
  height = 375
) %>% 
  add_trace(
    name = "Investment",
    y = ~Investment,
    marker = list(
      color = "#36A3D9", 
      line = list(width = 0.25, color = "white")
    ),
    hovertemplate = str_c(
      "<b>Component: investment</b><br><br>",
      "Contribution: %{y} percentage points<br>",
      "Date: %{text} ",
      "%{x}",
      "<extra></extra>"
    )
  ) %>% 
  add_trace(
    name = "Government consumption",
    y = ~`Government consumption`,
    marker = list(
      color = "#E66439", 
      line = list(width = 0.25, color = "white")
    ),
    hovertemplate = str_c(
      "<b>Component: government consumption</b><br><br>",
      "Contribution: %{y} percentage points<br>",
      "Date: %{text} ",
      "%{x}",
      "<extra></extra>"
    )
  ) %>% 
  add_trace(
    name = "Net export",
    y = ~`Net export`,
    marker = list(
      color = "#F2AA61", 
      line = list(width = 0.25, color = "white")
    ),
    hovertemplate = str_c(
      "<b>Component: net export</b><br><br>",
      "Contribution: %{y} percentage points<br>",
      "Date: %{text} ",
      "%{x}",
      "<extra></extra>"
    )
  ) %>% 
  add_trace(
    name = "Statistic discrepancy",
    y = ~`Statistic discrepancy`,
    marker = list(
      color = "lightgrey", 
      line = list(width = 0.25, color = "white")
    ),
    hovertemplate = str_c(
      "<b>Component: Statistic discrepancy</b><br><br>",
      "Contribution: %{y} percentage points<br>",
      "Date: %{text} ",
      "%{x}",
      "<extra></extra>"
    )
  ) %>%
  plotly::layout(
    xaxis = list (
      title = NA,
      range = c(
        as.character(first(ctg_exp_trf$date) - 180), 
        as.character(last(ctg_exp_trf$date) + 180)
      ),
      fixedrange = T,
      ticks = "outside",
      automargin = T,
      tickformat = "%Y",
      hoverformat = "%Y",
      showline = T,
      showgrid = F
    ),
    yaxis = list(
      title = NA,
      type = "linear",
      autorange = F,
      range = ctg_y_axis_range,
      fixedrange = T,
      dtick = 4,
      showline = F,
      showgrid = T,
      gridcolor = "#CFD8DC",
      zerolinecolor = "#E68F7E",
      side = "right"
    ),
    shapes = list(
      list(
        type = "line",
        layer = "below",
        xref = "x",
        x0 = "2020-03-02",
        x1 = "2020-03-02",
        yref = "y",
        y0 = ctg_y_axis_range[1],
        y1 = ctg_y_axis_range[2] - 1,
        line = list(color = "#90A4AE", dash = "dash")
      )
    ),
    annotations = list(
      list(
        text = "COVID-19<br>pandemic",
        font = list(size = 12, color = "#90A4AE"),
        align = "right",
        bgcolor = "white",
        showarrow = F,
        xref = "x",
        x = "2020-02-01",
        xanchor = "right",
        yref = "y",
        y = ctg_y_axis_range[2] - 1,
        yanchor = "top"
      )
    ),
    legend = list(
      orientation = "h",
      itemsizing = "constant",
      xanchor = "left",
      y = -0.15,
      yanchor = "top"
    ),
    margin = list(l = 0, r = 0, t = 15, b = 0),
    barmode = "relative",
    bargap = 0.3
  ) %>% 
  plotly::config(displayModeBar = F)


# export chart ------------------------------------------------------------

# latest observation
ctg_exp_trf_tidy <- ctg_exp_trf %>% 
  pivot_longer(
    2:ncol(.),
    names_to = "expenditure_component",
    values_to = "contribution_to_growth"
  )

# path to ctg data
path_data_ctg <- "data/ier_gdp-ctg_cleaned.csv"

# export chart
if (nrow(ctg_exp_trf_tidy) != nrow(read_csv(path_data_ctg))) {
  
  # import functions to apply custom ggplot2 theme and add logo
  source("script/ggplot2_theme.R")
  
  # reorder factor
  ctg_exp_trf_tidy <- ctg_exp_trf_tidy %>% 
    mutate(expenditure_component = fct_reorder(expenditure_component, contribution_to_growth))
  
  # labels
  labs_ctg <- ctg_exp_trf_tidy %>% 
    select(date) %>% 
    mutate(
      q = quarter(date), 
      yr = year(date),
      labs = str_c("Q", q, format(date, "\n%Y"))
    ) %>% 
    dplyr::filter(q == 1, yr %in% seq(2012, 2020, 2)) %>%
    dplyr::filter(!duplicated(date)) %>% 
    select(labs) %>% 
    deframe()
  
  # plot
  ggplot(
    ctg_exp_trf_tidy, 
    aes(date, contribution_to_growth, fill = expenditure_component)
  ) +
    geom_hline(yintercept = 0, color = "#E68F7E") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      lty = "dashed"
    ) +
    geom_col(width = 60, color = "white", lwd = 0.25) +
    scale_x_date(
      breaks = seq(ymd("2012-01-01"), ymd("2020-01-01"), by = "2 year"),
      labels = labs_ctg
    ) +
    scale_y_continuous(
      breaks = seq(-12, 12, 4),
      limits = c(-12, 12),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_fill_manual(
      values = c(
        "Private consumption" = "#2477B3",
        "Investment" = "#36A3D9",
        "Net export" = "#E66439",
        "Government consumption" = "#F2AA61",
        "Statistic discrepancy" = "lightgrey"
      ) 
    ) +
    annotate(
      "text",
      x = ymd("2020-04-01"),
      y = 10,
      label = "COVID-19\npandemic \u2192",
      size = 3,
      hjust = 0,
      color = "#90A4AE"
    ) +
    labs(
      title = "Contribution to economic growth",
      subtitle = "By expenditure component (percentage point)",
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Statistics Indonesia (BPS)"
    ) +
    theme_ier() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = rel(0.6)),
      legend.key = element_rect(fill = "transparent"),
      legend.key.size = unit(0.25, "cm"),
      legend.position = c(0.45, 1.075),
      legend.direction = "horizontal"
    )
  
  # path to the plot
  path_plot_ctg <- "fig/ier_gdp-ctg_plot.png"
  
  # save the plot
  ggsave(path_plot_ctg, width = 6, height = 3.708, dpi = 300)
  
  # add ier logo to the plot
  add_ier_logo(path_plot_ctg)
  
  # message
  message("The contribution to GDP growth chart has been updated")
  
} else {
  
  message("The contribution to GDP growth chart is up to date")
  
}


# export data -------------------------------------------------------------

if (!file.exists(path_data_ctg)) {
  
  write_csv(ctg_exp_trf_tidy, path_data_ctg)
  
  message("The contribution to GDP growth dataset has been exported")
  
} else if (nrow(ctg_exp_trf_tidy) != nrow(read_csv(path_data_ctg))) {
  
  write_csv(ctg_exp_trf_tidy, path_data_ctg) 
  
  message("The contribution to GDP growth dataset has been updated")
  
} else {
  
  message("The contribution to GDP growth dataset is up to date")
  
}