
# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(plotly)
library(gt)
library(scales)
library(ggthemes)
library(paletteer)

# tidy the data --------------------------------------------------------------------

# store base url of bps api
base_url <- "https://webapi.bps.go.id/v1/api/list"

# request the data
growth_req <- GET(
  base_url,
  query = list(
    model = "data",
    domain = "0000",
    var = "108",
    key = BPS_KEY
  )
)

# parse the response
growth_resp <- content(growth_req, "text")

growth_list <- fromJSON(
  growth_resp,
  simplifyDataFrame = T,
  flatten = T
)

# extract keys
growth_key_exp <- growth_list$vervar %>% 
  as_tibble()

growth_key_yr <- growth_list$tahun %>% 
  as_tibble()

growth_key_q <- growth_list$turtahun %>% 
  as_tibble()

growth_key_yoy <- growth_list$turvar %>% 
  dplyr::filter(str_detect(label, "y-on-y")) %>% 
  select(-2) %>% 
  deframe()

# extract data
growth_raw <- growth_list$datacontent %>% 
  as_tibble()

# tidy the data
growth_cleaned <- growth_raw %>% 
  pivot_longer(
    1:ncol(.),
    names_to = "key",
    values_to = "pct_change_yoy"
  ) %>% 
  separate(
    key,
    into = c("expenditure", "date"),
    sep = "108"
  ) %>% 
  dplyr::filter(
    expenditure %in% c(100, 200, 300, 400, 600, 700, 800) &
      str_detect(date, "^5") &
      !str_detect(date, "5$")
  ) %>% 
  arrange(date, expenditure)

# replace date key
growth_cleaned$date <- growth_cleaned$date %>% 
  str_replace_all(
    str_sub(growth_cleaned$date, 1, 4),
    str_c(
      rep(
        as.character(growth_key_yr$label),
        each = 28,
        length.out = length(growth_cleaned$date)
      ),
      "-"
    )
  ) %>% 
  str_replace_all(
    c(
      "31" = "01-01",
      "32" = "04-01",
      "33" = "07-01",
      "34" = "10-01"
    )
  ) %>% 
  ymd()

# subset expenditure key
growth_key_exp_sub <- growth_key_exp %>% 
  dplyr::filter(val %in% c(100, 200, 300, 400, 600, 700, 800))

# replace expenditure key
growth_cleaned$expenditure <- growth_cleaned$expenditure %>% 
  str_replace_all(
    as.character(growth_cleaned$expenditure),
    rep(
      as.character(growth_key_exp_sub$label),
      times = 28,
      length.out = length(growth_cleaned$expenditure)
    )
  ) %>% 
  as.factor()

# translate expenditure categories to english
exp_en <- c(
  "1. Pengeluaran Konsumsi Rumahtangga" = "Household consumption",
  "2. Pengeluaran Konsumsi LNPRT" = "Non-profit institutions serving households (NPISHs) consumption",
  "3. Pengeluaran Konsumsi Pemerintah" = "Government consumption",
  "4. Pembentukan Modal Tetap Domestik Bruto" = "Gross fixed capital formation",
  "6. Ekspor Barang dan Jasa" = "Exports of goods and services",
  "7. Dikurangi Impor Barang dan Jasa" = "Imports of goods and services",
  "8. PRODUK DOMESTIK BRUTO" = "GDP"
)

growth_cleaned$expenditure <- growth_cleaned$expenditure %>% 
  str_replace_all(exp_en)

# extract gdp growth
growth_py <- growth_cleaned %>% 
  dplyr::filter(expenditure == "GDP")

# create quarter labels
quarter_labels <- str_c("Q", quarter(growth_py$date))

# plot --------------------------------------------------------------------


# headline ----------------------------------------------------------------

# covid text annotation
covid_text <- list(
  x = "2020-01-02",
  xanchor = "right",
  y = -8,
  text = "COVID-19<br>pandemic",
  font = list(size = 10, color = "#90A4AE"),
  showarrow = F,
  bgcolor = "white"
) 

# plot
growth_plot <- growth_py %>% 
  plot_ly(
    x = ~date,
    y = ~pct_change_yoy,
    height = 300
  ) %>% 
  add_lines(
    text = quarter_labels,
    hovertemplate = str_c(
      "%{y} percent<br>",
      "%{text} ",
      "%{x}",
      "<extra></extra>"
    ),
    colors = "#1d81a2",
    line = list(width = 3)
  ) %>% 
  plotly::layout(
    xaxis = list (
      title = NA,
      fixedrange = T,
      range = c("2009-07-01", as.character(last(growth_py$date) + 180)),
      showgrid = F,
      showline = T,
      ticks = "outside",
      tickvals = c(
        "2010",
        "2012",
        "2014",
        "2016",
        "2018",
        "2020"
      ),
      hoverformat = "%Y",
      tickformat = "%Y",
      automargin = T
    ),
    yaxis = list(
      side = "right",
      title = NA,
      type = "linear",
      showgrid = T,
      gridcolor = "#CFD8DC",
      fixedrange = T,
      autorange = F,
      range = c(-12, 9),
      dtick = 4,
      zerolinecolor = "#ff856c"
    ),
    shapes = list(
      list(
        type = "line",
        line = list(color = "#90A4AE", dash = "dash"),
        xref = "x",
        yref = "y",
        x0 = "2020-03-02",
        x1 = "2020-03-02",
        y0 = -12,
        y1 = 8,
        layer = "below"
      )
    ),
    annotations = list(covid_text),
    margin = list(
      t = 15,
      b = 0,
      r = 0,
      l = 0
    )
  ) %>% 
  plotly::config(displayModeBar = F)


# by component ------------------------------------------------------------

# create df for gt
gt_growth_cln <- growth_cleaned %>% 
  dplyr::filter(str_detect(date, "2018|2019|2020")) %>%
  pivot_wider(
    names_from = date, 
    values_from = pct_change_yoy
  ) %>% 
  rename("Expenditure" = 1)

# quarter labels
gt_quarter_labs <- growth_cleaned %>% 
  dplyr::filter(str_detect(date, "2018|2019|2020") & !duplicated(date)) %>%
  select(date) %>% 
  arrange(date) %>% 
  mutate(
    q = str_c("Q", as.character(quarter(date)))
  ) %>%
  deframe()


# color palette
col_pal_gt <- function(x) {
  negative_values <- scales::col_numeric(
    palette = paletteer::paletteer_c("ggthemes::Red", n = 5) %>% 
      as.character(),
    domain = c(
      min(growth_cleaned$pct_change_yoy),
      0
    ),
    na.color = "white",
    reverse = T
  )
  positive_values <- scales::col_numeric(
    palette = paletteer::paletteer_c("ggthemes::Blue", n = 5) %>% 
      as.character(),
    domain = c(
      0,
      max(growth_cleaned$pct_change_yoy)
    ),
    na.color = "white"
  )
  ifelse(x < 0, negative_values(x), positive_values(x))
}

# table
growth_table <- gt_growth_cln %>%  
  gt() %>% 
  tab_spanner(
    label = "2018",
    columns = 2:5
  ) %>% 
  tab_spanner(
    label = "2019",
    columns = 6:9
  ) %>% 
  tab_spanner(
    label = "2020",
    columns = 10:ncol(gt_growth_cln)
  ) %>% 
  cols_label(.list = gt_quarter_labs) %>% 
  cols_align(
    align = "auto",
    columns = T
  ) %>% 
  data_color(
    columns = 2:ncol(gt_growth_cln),
    colors = col_pal_gt
  ) %>% 
  tab_header(
    title = html("<b>Declines in spending</b>"),
    subtitle = html(
      str_c(
        "GDP, by expenditure",
        "<br>",
        "(percent change on a year earlier)"
      )
    )
  ) %>% 
  tab_source_note(
    source_note = "Table: @dzulfiqarfr | Source: Statistics Indonesia (BPS)"
  ) %>% 
  tab_options(
    heading.align = "left",
    heading.border.bottom.color = "white",
    heading.title.font.size = 18,
    heading.subtitle.font.size = 16,
    table.border.top.color = "white",
    table_body.hlines.color = "white",
    column_labels.border.top.color = "white",
    column_labels.font.size = 14,
    table.border.bottom.color = "white",
    table.font.size = 13,
    data_row.padding = px(10),
    table.width = pct(100)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold", align = "left"),
      cell_borders(
        sides = "bottom", 
        color = "black",
        weight = px(2.5))
    ),
    locations = list(
      cells_column_labels(1)
    )
  ) %>%  
  tab_style(
    style = list(
      cell_text(align = "right", weight = "bold"),
      cell_borders(
        sides = "bottom", 
        color = "black",
        weight = px(2.5))
    ),
    locations = list(cells_column_labels(2:ncol(gt_growth_cln)))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(cells_column_spanners(everything()))
  )

