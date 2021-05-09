# indonesia economic recovery

# gdp
# by expenditure
# contribution to annual growth

# author: dzulfiqar fathur rahman
# created: 2021-02-23
# last updated: 2021-05-08
# page: gdp


# setup -------------------------------------------------------------------

# packages
library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
library(plotly)
library(magick)

# api key
if (exists("BPS_KEY") == F) {
  BPS_KEY <- Sys.getenv("BPS_KEY")
}

# bps api url
if (exists("base_url") == F) {
  base_url <- "https://webapi.bps.go.id/v1/api/list"
}


# data --------------------------------------------------------------------

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
  pivot_longer(
    1:ncol(.),
    names_to = "key",
    values_to = "exp"
  ) %>% 
  separate(
    key,
    into = c("key_exp", "key_period"),
    sep = "19560"
  ) %>% 
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
      "Diskrepansi Statistik" = "Statistics discrepancy"
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
    `Statistics discrepancy`
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
  pivot_longer(
    2:(ncol(.) - 1),
    names_to = "comp",
    values_to = "exp"
  ) %>% 
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

# plot
plot_ctg <- plot_ly(
  data = ctg_exp_trf,
  type = "bar",
  name = "Private consumption",
  x = ~date,
  y = ~`Private consumption`,
  text = labs_q,
  marker = list(color = "#1d81a2"),
  hovertemplate = str_c(
    "<b>Component: private consumption</b><br><br>",
    "Contribution to growth: %{y} percentage points<br>",
    "Date: %{text} ",
    "%{x}",
    "<extra></extra>"
  ),
  height = 375
) %>% 
  add_trace(
    name = "Investment",
    y = ~Investment,
    marker = list(color = "#60b4d7"),
    hovertemplate = str_c(
      "<b>Component: investment</b><br><br>",
      "Contribution to growth: %{y} percentage points<br>",
      "Date: %{text} ",
      "%{x}",
      "<extra></extra>"
    )
  ) %>% 
  add_trace(
    name = "Government consumption",
    y = ~`Government consumption`,
    marker = list(color = "#ff725b"),
    hovertemplate = str_c(
      "<b>Component: government consumption</b><br><br>",
      "Contribution to growth: %{y} percentage points<br>",
      "Date: %{text} ",
      "%{x}",
      "<extra></extra>"
    )
  ) %>% 
  add_trace(
    name = "Net export",
    y = ~`Net export`,
    marker = list(color = "#ffd882"),
    hovertemplate = str_c(
      "<b>Component: net export</b><br><br>",
      "Contribution to growth: %{y} percentage points<br>",
      "Date: %{text} ",
      "%{x}",
      "<extra></extra>"
    )
  ) %>% 
  add_trace(
    name = "Statistics discrepancy",
    y = ~`Statistics discrepancy`,
    marker = list(color = "#B0BEC5"),
    hovertemplate = str_c(
      "<b>Component: Statistics discrepancy</b><br><br>",
      "Contribution to growth: %{y} percentage points<br>",
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
      range = c(-12, 13),
      fixedrange = T,
      dtick = 4,
      showline = F,
      showgrid = T,
      gridcolor = "#CFD8DC",
      zerolinecolor = "#ff856c",
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
        y0 = -12,
        y1 = 12,
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
        y = 12,
        yanchor = "top"
      )
    ),
    legend = list(
      orientation = "h",
      itemsizing = "constant",
      xanchor = "left",
      y = -.15,
      yanchor = "top"
    ),
    margin = list(l = 0, r = 0, t = 15, b = 0),
    barmode = "relative",
    bargap = 0.35
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

# export chart
if (nrow(ctg_exp_trf_tidy) != nrow(read_csv("data/ier_gdp-ctg_cleaned.csv"))) {
  
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
    geom_hline(yintercept = 0, color = "#ff856c") +
    geom_vline(
      xintercept = ymd("2020-03-01"),
      color = "#90A4AE",
      linetype = 2
    ) +
    geom_col(width = 60) +
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
    scale_fill_manual(values = c("#B0BEC5", "#ffd882", "#ff725b", "#60b4d7", "#1d81a2")) +
    annotate(
      "text",
      x = ymd("2020-04-01"),
      y = 10,
      label = "COVID-19\npandemic \u2192",
      size = 2,
      hjust = 0,
      color = "#90A4AE"
    ) +
    labs(
      title = "Economic growth",
      subtitle = "GDP, contribution to annual growth by expenditure component\n(percentage points)",
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Statistics Indonesia (BPS)"
    ) +
    guides(fill = guide_legend(reverse = T)) +
    theme(
      text = element_text(size = 12),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(color = "black"),
      legend.title = element_blank(),
      legend.key = element_rect(fill = "transparent"),
      legend.key.size = unit(0.3, "cm"),
      legend.position = c(0.495, 1.175),
      legend.direction = "horizontal",
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#CFD8DC"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 55)),
      plot.caption = element_text(
        color = "#757575",
        hjust = 0,
        margin = margin(t = 35)
      )
    ) +
    ggsave(
      "fig/ier_gdp-ctg_plot.png",
      width = 7,
      height = 4,
      dpi = 300
    )
  
  # add logo
  ier_logo <- image_read("images/ier_hexsticker_small.png")
  
  # add base plot
  base_plot <- image_read("fig/ier_gdp-ctg_plot.png")
  
  # get plot height
  plot_height <- magick::image_info(base_plot)$height
  
  # get plot width
  plot_width <- magick::image_info(base_plot)$width
  
  # get logo height
  logo_width <- magick::image_info(ier_logo)$width
  
  # get logo width
  logo_height <- magick::image_info(ier_logo)$height
  
  # position for the bottom 1.5 percent
  pos_bottom <- plot_height - logo_height - plot_height * 0.015
  
  # position for the right 1.5 percent
  pos_right <- plot_width - logo_width - 0.015 * plot_width
  
  # overwrite plot
  base_plot %>% 
    image_composite(ier_logo, offset = str_c("+", pos_right, "+", pos_bottom)) %>% 
    image_write("fig/ier_gdp-ctg_plot.png")
  
  # message
  message("The contribution to GDP growth chart has been updated")
  
} else {
  
  message("The contribution to GDP growth chart is up to date")
  
}


# export data -------------------------------------------------------------

# write csv
if (file.exists("data/ier_gdp-ctg_cleaned.csv") == F) {
  
  write_csv(ctg_exp_trf_tidy, "data/ier_gdp-ctg_cleaned.csv")
  
  message("The contribution to GDP growth dataset has been exported")
  
} else if (nrow(ctg_exp_trf_tidy) != nrow(read_csv("data/ier_gdp-ctg_cleaned.csv"))) {
  
  write_csv(ctg_exp_trf_tidy, "data/ier_gdp-ctg_cleaned.csv") 
  
  message("The contribution to GDP growth dataset has been updated")
  
} else {
  
  message("The contribution to GDP growth dataset is up to date")
  
}