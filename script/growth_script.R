# Overall economic growth plot


# Setup ----------------------------------------------------------------

library(conflicted)
library(tidyverse)
library(lubridate)
library(plotly)


# Tidy the data -----------------------------------------------------------

# Import
growth_raw <- read.csv("Data/BPS_growth-rate_raw.csv",
                       skip = 2, 
                       na.strings = c("-", "")
)

# Rename indicator variable
names(growth_raw)[1] <- "Comp"

# Remove columns, rows containing missing values
growth_raw <- growth_raw %>% 
  dplyr::filter(!is.na(Comp), Comp != "Diskrepansi Statistik") %>% 
  select_if(function(x) {!all(is.na(x))})

# Create a sequence of quarterly date series for column names
seq10_20 <- seq(ymd("2010-01-01"), ymd("2020-07-01"), by = "quarter")

GDP_col_names <- seq10_20 %>%
  tibble() %>% 
  mutate(
    yr = year(seq10_20),
    mo = month(seq10_20)
  ) %>% 
  arrange(desc(yr), mo) %>% 
  rename(date = 1)

# Rename date columns
names(growth_raw)[2:ncol(growth_raw)] <- as.character(GDP_col_names$date)

# Rename components
growth_comp_id <- c("8. PRODUK DOMESTIK BRUTO")

growth_comp_en <- c("Gross domestic product")

growth_raw$Comp[growth_raw$Comp %in% growth_comp_id] <- growth_comp_en

# Correct data types
growth_raw[, 2:ncol(growth_raw)] <- lapply(
  growth_raw[, 2:ncol(growth_raw)],
  as.numeric
)

# Reshape data
growth_tidy <- growth_raw %>%
  dplyr::filter(Comp %in% growth_comp_en) %>% 
  pivot_longer(
    2:ncol(growth_raw),
    names_to = "Year", values_to = "Growth"
  ) %>% 
  arrange(ymd(Year))

# Correct data types
growth_tidy$Year <- ymd(growth_tidy$Year)


# Create the plot -----------------------------------------------------------

# Byline, source annotations
byline_source_BPS <- list(
  x = 0,
  xref = "paper",
  xanchor = "left",
  xshift = 0,
  y = -0.1,
  yref = "paper",
  yanchor = "top",
  yshift = 0,
  text = "Chart: @dzulfiqarfr | Source: Statsitics Indonesia (BPS)",
  font = list(color = "darkgrey"),
  showarrow = F
)

# Quarter names
q_names <- rep(
  str_c("Q", 1:4),
  times = length(unique(format(growth_tidy$Year, "%Y")))
)

q_names <- q_names[-length(q_names)] 

# Plot
growth_plot <- plot_ly(
  growth_tidy[growth_tidy$Comp == "Gross domestic product", ],
  x = ~Year,
  y = ~Growth
) %>%
  add_lines(
    text = q_names,
    hovertemplate = str_c(
      "%{y} percent<br>",
      "%{text} ",
      "%{x}",
      "<extra></extra>"
    ),
    colors = "#1d81a2"
  ) %>% 
  plotly::layout(
    title = list(
      text = str_c(
        "<b>Uptick amid recession</b>",
        "<br>",
        "<sup>",
        "GDP",
        "<br>",
        "<sup>(percent change from a year earlier)</sup>",
        "</sup>"
      ),
      xref = "paper",
      x = 0,
      xanchor = "left",
      yref = "paper",
      y = 2
    ),
    xaxis = list (
      title = NA,
      fixedrange = T,
      showgrid = F,
      showline = T,
      ticks = "outside",
      nticks = 6,
      hoverformat = "%Y",
      automargin = T
    ),
    yaxis = list(
      side = "right",
      title = NA,
      type = "linear",
      showgrid = T,
      gridcolor = "lightgrey",
      fixedrange = T,
      autorange = F,
      range = c(-12, 9),
      dtick = 4,
      zerolinecolor = "#ff856c"
    ),
    annotations = list(byline_source_BPS),
    margin = list(
      t = 75,
      b = 75,
      l = 25,
      r = 25
    ),
    autosize = T
  ) %>% 
  config(displayModeBar = F)
