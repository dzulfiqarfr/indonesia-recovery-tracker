# indonesia economic recovery

# unemployment rate by province

# author: dzulfiqar fathur rahman
# created: 2021-03-24
# last updated: 2021-07-14
# page: employment


# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(plotly)
library(ggtext)
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
unemp_prov_req <- GET(
  base_url,
  query = list(
    model = "data",
    domain = "0000",
    var = "543",
    key = BPS_KEY
  )
)

# parse response
unemp_prov_parsed <- content(unemp_prov_req, "text") %>% 
  fromJSON()

# extract keys
## province
unemp_key_prov <- as_tibble(unemp_prov_parsed$vervar)

### english province names
unemp_key_prov <- unemp_key_prov %>% 
  mutate(
    label_eng = unemp_key_prov$label %>% 
      str_replace_all(
        c(
          "JAWA BARAT" = "West Java",
          "JAWA TENGAH" = "Central Java",
          "JAWA TIMUR" = "East Java",
          "DI " = "",
          "DKI " = "",
          "KALIMANTAN BARAT" = "West Kalimantan",
          "KALIMANTAN SELATAN" = "South Kalimantan",
          "KALIMANTAN TENGAH" = "Central Kalimantan", 
          "KALIMANTAN TIMUR" = "East Kalimantan",
          "KALIMANTAN UTARA" = "North Kalimantan",
          "KEP. BANGKA BELITUNG" = "Bangka Belitung Islands",
          "KEP. RIAU" = "Riau Islands",
          "MALUKU UTARA" = "North Maluku",
          "NUSA TENGGARA BARAT" = "West Nusa Tenggara",
          "NUSA TENGGARA TIMUR" = "East Nusa Tenggara",
          "PAPUA BARAT" = "West Papua",
          "SULAWESI BARAT" = "West Sulawesi",
          "SULAWESI SELATAN" = "South Sulawesi",
          "SULAWESI TENGAH" = "Central Sulawesi",
          "SULAWESI TENGGARA" = "Southeast Sulawesi",
          "SULAWESI UTARA" = "North Sulawesi",
          "SUMATERA SELATAN" = "South Sumatra",
          "SUMATERA UTARA" = "North Sumatra",
          "SUMATERA BARAT" = "West Sumatra"
        )
      ) %>% 
      str_to_title()
  )

## year
unemp_prov_key_yr <- as_tibble(unemp_prov_parsed$tahun)

# extract data
unemp_prov_raw <- as_tibble(unemp_prov_parsed$datacontent)

# tidy data
unemp_prov_tidy <- unemp_prov_raw %>% 
  pivot_longer(1:ncol(.), names_to = "key", values_to = "unemployment_rate") %>% 
  separate(key, into = c("key_prov", "key_date"), sep = "5430") %>% 
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
  select(key_prov, key_date, key_yr, key_mo, unemployment_rate)

# remove national observations
unemp_prov_tidy <- unemp_prov_tidy %>% 
  dplyr::filter(key_prov != 9999)

# add anchor to year key
unemp_prov_key_yr$val <- str_c("^", unemp_prov_key_yr$val, "$")

# replace year key
unemp_prov_tidy$key_yr <- unemp_prov_tidy$key_yr %>% 
  str_replace_all(deframe(unemp_prov_key_yr))

# replace month key
unemp_prov_tidy$key_mo <- unemp_prov_tidy$key_mo %>% 
  str_replace_all(c("^189$" = "-02-01", "^190$" = "-08-01", "^191$" = "-01-01"))

# create date variable
unemp_prov_tidy <- unemp_prov_tidy %>% 
  mutate(date = ymd(str_c(key_yr, key_mo))) %>% 
  select(key_prov, date, key_yr, key_mo, unemployment_rate)

# add anchor to province key
unemp_key_prov$val <- str_c("^", unemp_key_prov$val, "$")

# add province labels
unemp_prov_tidy <- unemp_prov_tidy %>% 
  mutate(province = str_replace_all(unemp_prov_tidy$key_prov, deframe(unemp_key_prov[, -2]))) %>% 
  select(province, date, key_yr, key_mo, unemployment_rate)

# remove observations before 2005, calculate change in unemployment rate
unemp_prov_trf <- unemp_prov_tidy %>%
  dplyr::filter(key_yr >= 2005) %>% 
  group_by(province, key_mo) %>% 
  mutate(ppt_change_yoy = round(unemployment_rate - dplyr::lag(unemployment_rate, 1), 2)) %>% 
  ungroup() %>% 
  select(province, date, unemployment_rate, ppt_change_yoy)

# subset latest date
unemp_prov_date_latest <- unemp_prov_trf %>% 
  select(date) %>% 
  mutate(mo = month(date)) %>% 
  tail(4) %>% 
  dplyr::filter(mo == last(mo)) %>% 
  select(date) %>% 
  deframe()

# subset latest observations for table, reshape to wide format
unemp_prov_wide <- unemp_prov_trf %>% 
  dplyr::filter(date %in% unemp_prov_date_latest) %>%
  pivot_wider(
    names_from = date, 
    values_from = c(unemployment_rate, ppt_change_yoy)
  ) %>% 
  select(-4) %>% 
  rename(unemp_rate_1 = 2, unemp_rate_2 = 3, ppt_change_yoy = 4)


# plot --------------------------------------------------------------------

# reorder province based on change in unemployment rate
unemp_prov_wide <- unemp_prov_wide %>% 
  mutate(province = fct_reorder(province, ppt_change_yoy, .desc = F))

# province name for center position on x-axis
prov_plotly <- unemp_prov_wide %>% 
  arrange(desc(ppt_change_yoy)) %>% 
  slice(17) %>% 
  select(province)

# plot
plot_unemp_prov <- plot_ly(
  unemp_prov_wide,
  x = ~province,
  height = 450
) %>% 
  add_segments(
    xend = ~province,
    y = ~unemp_rate_1,
    yend = ~unemp_rate_2,
    showlegend = F,
    opacity = 0.5,
    line = list(width = 5, color = "#55CBF2")
  ) %>% 
  add_markers(
    y = ~unemp_rate_1,
    name = format(unemp_prov_date_latest[1], "%B %Y"),
    hovertemplate = str_c(
      "Province: %{x}<br>",
      "Unemployment rate: %{y} percent<br>",
      "Date: ",
      format(unemp_prov_date_latest[1], "%B %Y"),
      "<extra></extra>"
    ),
    marker = list(
      size = 10, 
      color = "#55CBF2",
      line = list(width = 1, color = "white")
    )
  ) %>% 
  add_markers(
    y = ~unemp_rate_2,
    name = format(unemp_prov_date_latest[2], "%B %Y"),
    hovertemplate = str_c(
      "Province: %{x}<br>",
      "Unemployment rate: %{y} percent<br>",
      "Date: ",
      format(unemp_prov_date_latest[2], "%B %Y"),
      "<extra></extra>"
    ),
    marker = list(
      size = 10, 
      color = "#2477B3", 
      line = list(width = 1, color = "white")
    )
  ) %>% 
  plotly::layout(
    xaxis = list(
      title = NA,
      autorange = T,
      fixedrange = T,
      ticks = "outside",
      tickmode = "auto",
      nticks = 17,
      automargin = T,
      tickangle = -90,
      showline = T,
      showgrid = T,
      gridcolor = "#CFD8DC"
    ),
    yaxis = list(
      title = NA,
      type = "linear",
      autorange = F,
      range = c(0, 12.5),
      fixedrange = T,
      dtick = 3,
      showline = F,
      showgrid = T,
      gridcolor = "#CFD8DC",
      side = "right"
    ),
    annotations = list(
      list(
        text = "&#8592; lower increase | higher increase &#8594;",
        font = list(size = 12, color = "#90A4AE"),
        align = "center",
        bgcolor = "white",
        showarrow = F,
        xref = "x",
        x = prov_plotly$province,
        xanchor = "center",
        yref = "y",
        y = 1.25,
        yanchor = "top"
      )
    ),
    showlegend = T,
    legend = list(
      orientation = "h",
      itemsizing = "constant",
      itemclick = F,
      itemdoubleclick = F,
      x = 0,
      xanchor = "left",
      y = 1.15,
      yanchor = "top"
    ),
    margin = list(l = 0, r = 0, t = 15, b = 0),
    autosize = T
  ) %>% 
  plotly::config(displayModeBar = F)


# export chart ------------------------------------------------------------

# path to unemployment rate by province data
path_data_unemp_prov <- "data/ier_unemployment-province_cleaned.csv"

# export chart
if (nrow(unemp_prov_trf) != nrow(read_csv(path_data_unemp_prov))) {
  
  # import functions to apply custom ggplot2 theme and add logo
  source("script/ggplot2_theme.R")
  
  # annotations
  ## province name for y-positions
  prov_ggplot <- unemp_prov_wide %>% 
    arrange(desc(ppt_change_yoy)) %>% 
    slice(5:6) %>% 
    select(province)
  
  ## legend
  anno_legend <- tibble(
    x = c(0.75, 0.75),
    y = prov_ggplot$province,
    label = c(
      str_c("\u25CF ", format(unemp_prov_date_latest[1], "%b '%y")),
      str_c("\u25CF ", format(unemp_prov_date_latest[2], "%b '%y"))
    ),
    color = c("#55CBF2", "#2477B3")
  )
  
  # plot
  ggplot(data = unemp_prov_wide, aes(y = province)) +
    geom_segment(
      aes(yend = province, x = unemp_rate_1, xend = unemp_rate_2),
      color = "#55CBF2",
      alpha = 0.25,
      lwd = 1.5
    ) +
    geom_point(
      aes(x = unemp_rate_1), 
      pch = 21,
      fill = "#55CBF2",
      color = "white",
      size = 2.5
    ) +
    geom_point(
      aes(x = unemp_rate_2), 
      pch = 21,
      fill = "#2477B3",
      color = "white",
      size = 2.5
    ) +
    geom_richtext(
      data = anno_legend,
      aes(x, y, label = label, color = factor(color)),
      fill = "white",
      label.color = NA,
      hjust = 0,
      size = 3,
      fontface = "bold",
      show.legend = F
    ) +
    scale_color_identity() +
    scale_x_continuous(
      breaks = seq(0, 12, 3),
      limits = c(0, 12),
      expand = c(0, 0),
      position = "top"
    ) +
    labs(
      title = "Unemployment rate",
      subtitle = "By province (percent)",
      caption = "Chart: Dzulfiqar Fathur Rahman | Source: Statistics Indonesia (BPS)"
    ) +
    theme_ier() +
    theme(
      axis.text.y = element_text(hjust = 0),
      axis.line.x = element_blank(),
      axis.line.y = element_line(color = "black"),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_line(color = "#CFD8DC"),
    )
  
  # path to the plot
  path_plot_unemp_prov <- "fig/ier_unemployment-province_plot.png"
  
  # save the plot
  ggsave(
    path_plot_unemp_prov,
    width = 5,
    height = 7,
    dpi = 300,
    scale = 1
  )
  
  # add logo
  logo <- image_read("images/ier_hexsticker_small.png")
  
  # import base plot
  base_plot <- image_read(path_plot_unemp_prov)
  
  # get the plot dimension
  plot_height <- magick::image_info(base_plot)$height
  plot_width <- magick::image_info(base_plot)$width
  
  # get the logo dimension
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # get number of pixels to be 1% from the bottom of the plot
  # while accounting for the logo height
  pos_bottom <- plot_height - logo_height - plot_height * 0.01
  
  # get number of pixels to be 0.025% from the left of the plot
  pos_right <- plot_width - logo_width - 0.0025 * plot_width
  
  # export the plot with a logo
  base_plot %>%
    image_composite(logo, offset = str_c("+", pos_right, "+", pos_bottom)) %>%
    image_write(path_plot_unemp_prov)
  
  # message
  message("The unemployment rate by province chart has been updated")
  
} else {
  
  message("The unemployment rate by province chart is up to date")
  
}


# export data -------------------------------------------------------------

if (!file.exists(path_data_unemp_prov)) {
  
  write_csv(unemp_prov_trf, path_data_unemp_prov)
  
  message("The unemployment rate by province dataset has been exported")
  
} else if (nrow(unemp_prov_trf) != nrow(read_csv(path_data_unemp_prov))) {
  
  write_csv(unemp_prov_trf, path_data_unemp_prov)
  
  message("The unemployment rate by province dataset has been updated")
  
} else {
  
  message("The unemployment rate by province dataset is up to date")
  
}