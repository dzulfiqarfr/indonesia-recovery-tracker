# Growth by expenditures

# Setup ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(plotly)


# Tidy the data -----------------------------------------------------------

# Import the data
exp_raw <- read_csv(
  "data/BPS_growth-exp_raw.csv",
  skip = 3,
  na = c("-", "")
)

# Rename indicator variable
names(exp_raw)[1] <- "Comp"

# Remove columns, rows containing missing values
exp_raw <- exp_raw %>% 
  select_if(function(x) {!all(is.na(x))})

# Create a sequence of quarterly date series for column names
seq10_20 <- seq(ymd("2010-01-01"), ymd("2020-04-01"), by = "quarter")

GDP_col_names <- seq10_20 %>%
  tibble() %>% 
  mutate(
    yr = year(seq10_20),
    mo = month(seq10_20)
  ) %>% 
  arrange(desc(yr), mo) %>% 
  rename(date = 1)

# Rename date columns
names(exp_raw)[2:ncol(exp_raw)] <- as.character(GDP_col_names$date)

# Extract main expenditure observations
exp <- c(
  "1. Pengeluaran Konsumsi Rumahtangga",
  "2. Pengeluaran Konsumsi LNPRT",
  "3. Pengeluaran Konsumsi Pemerintah",
  "4. Pembentukan Modal Tetap Domestik Bruto",
  "5. Perubahan Inventori",
  "6. Ekspor Barang dan Jasa",
  "7. Dikurangi Impor Barang dan Jasa",
  "Diskrepansi Statistik",
  "8. PRODUK DOMESTIK BRUTO"
)

# Subset observations
exp_raw <- exp_raw %>% 
  dplyr::filter(Comp %in% exp)

# Create component names
exp_en <- c(
  "hh_exp",
  "npih_exp",
  "govt_exp",
  "gfcf_exp",
  "inventory",
  "export",
  "import",
  "stat_disc",
  "gdp"
)

# Rewrite component names
exp_raw[[1]] <- exp_en

# Transform data
exp_trf <- exp_raw %>% 
  pivot_longer(
    2:ncol(.), 
    names_to = "date",
    values_to = "exp"
  ) %>% 
  pivot_wider(names_from = Comp, values_from = exp) %>% 
  mutate(
    private_consumption = hh_exp + npih_exp,
    investment = gfcf_exp + inventory,
    net_exp = export - import
  ) %>% 
  pivot_longer(
    c(2:9, 11:ncol(.)),
    names_to = "comp",
    values_to = "exp"
  ) %>% 
  select(comp, date, exp, gdp) %>% 
  arrange(date, comp)

# Correct data types
exp_trf[[1]] <- as.factor(exp_trf[[1]])
exp_trf[[2]] <- ymd(exp_trf[[2]])

# Calculate proportion, change, contribution to growth
exp_ctg <- exp_trf %>% 
  arrange(comp, date) %>% 
  group_by(comp) %>% 
  mutate(
    diff_pa = exp - dplyr::lag(exp, 4),
    pct_change_pa = diff_pa / dplyr::lag(exp, 4) * 100,
    prop = exp / gdp * 100,
    ctg = pct_change_pa * dplyr::lag(prop, 4) / 100
  ) %>%  
  arrange(date, comp) %>% 
  dplyr::filter(
    comp %in% c(
      "private_consumption",
      "govt_exp",
      "gfcf_exp",
      "inventory",
      "net_exp",
      "stat_disc"
    ),
    date != seq(ymd("2010-01-01"), ymd("2015-10-01"), by = "quarter")
  ) %>% 
  select(comp, date, ctg) %>% 
  pivot_wider(names_from = comp, values_from = ctg) %>% 
  arrange(date)

# Round decimal places to two digits
exp_ctg[, 2:ncol(exp_ctg)] <- lapply(
  exp_ctg[, 2:ncol(exp_ctg)],
  function(x) {
    round(x, digits = 2)
  }
)

# Create the plot --------------------------------------------------------------------

# create quarter labels
q_names <- str_c("Q", quarter(exp_ctg$date))

# Plot
ctg_plot <- plot_ly(
  data = exp_ctg,
  x = ~date,
  y = ~private_consumption,
  text = q_names,
  name = "Private consumption",
  type = "bar",
  marker = list(color = "#1d81a2"),
  hovertemplate = str_c(
    "%{y} percent<br>",
    "%{text} ",
    "%{x}",
    "<extra></extra>"
  ),
  height = 300
) %>% 
  add_trace(
    y = ~gfcf_exp, 
    name = "Gross fixed capital formation",
    marker = list(color = "#60b4d7")
  ) %>% 
  add_trace(
    y = ~govt_exp, 
    name = "Government consumption",
    marker = list(color = "#09bb9f")
  ) %>% 
  add_trace(
    y = ~net_exp, 
    name = "Net export",
    marker = list(color = "#ffd882")
  ) %>% 
  add_trace(
    y = ~inventory, 
    name = "Change in inventory",
    marker = list(color = "#ff725b")
  ) %>%
  add_trace(
    y = ~stat_disc, 
    name = "Statistics discrepancy",
    marker = list(color = "#607d8b")
  ) %>%
  plotly::layout(
    barmode = "relative",
    xaxis = list (
      title = NA,
      fixedrange = T,
      range = c("2015-07-01", as.character(last(exp_ctg$date) + 120)),
      showgrid = F,
      showline = T,
      ticks = "outside",
      tickvals = c(
        "2016",
        "2017",
        "2018",
        "2019",
        "2020"
      ),
      hoverformat = "%Y",
      tickformat = "%Y",
      automargin = T
    ),
    yaxis = list(
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
    legend = list(
      xanchor = "left",
      y = 1.01,
      yanchor = "top",
      font = list(
        size = 7.5
      ),
      itemsizing = "constant"
    ),
    margin = list(
      t = 5,
      b = 0,
      l = 0,
      r = 0
    ),
    bargap = 0.35
  ) %>% 
  plotly::config(displayModeBar = F)