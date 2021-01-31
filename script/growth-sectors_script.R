# Growth by sectors plot


# Setup ----------------------------------------------------------------

library(conflicted)
library(tidyverse)
library(lubridate)
library(gt)
library(scales)
library(ggthemes)
library(paletteer)


# Tidy the data -----------------------------------------------------------

# Import
sectors_raw <- read_csv("Data/BPS_growth-sectors_raw.csv",
                       skip = 2, 
                       na = c("-", "")
)

# Rename indicator variable
names(sectors_raw)[1] <- "Sectors"

# Remove columns, rows containing missing values
sectors_raw2 <- sectors_raw %>% 
  dplyr::filter(!is.na(Sectors)) %>% 
  select_if(function(x) {!all(is.na(x))}) %>% 
  slice(c(1, 10, 15, 37:38, 41, 48, 51:52))

# Create a sequence of quarterly date series for column names
seq18_20 <- seq(
  ymd("2018-01-01"), ymd("2020-07-01"), by = "quarter"
)

sectors_col_names <- seq18_20 %>%
  tibble() %>% 
  mutate(
    yr = year(seq18_20),
    mo = month(seq18_20)
  ) %>% 
  arrange(desc(yr), mo) %>% 
  rename(date = 1)

# Rename date columns
names(sectors_raw2)[2:ncol(sectors_raw2)] <- as.character(sectors_col_names$date)

# Rename components
sectors_comp_en <- c(
  "Agriculture, forestry and fishery",
  "Mining",
  "Manufacturing",
  "Construction",
  "Wholesale and retail trade; car and motorcycle repair",
  "Transportation and warehouse",
  "Accommodation and food services",
  "Information and communication",
  "Financial services"
)

sectors_raw2$Sectors <- sectors_comp_en

# Reshape data
sectors_tidy <- sectors_raw2 %>%
  pivot_longer(
    2:ncol(sectors_raw2),
    names_to = "Year", values_to = "Growth"
  ) %>% 
  arrange(ymd(Year)) %>% 
  pivot_wider(names_from = Year, values_from = Growth)

# Correct data types
sectors_tidy[, 2:ncol(sectors_tidy)] <- lapply(
  sectors_tidy[, 2:ncol(sectors_tidy)],
  as.numeric
)

sectors_tidy$Sectors <- as.character(sectors_tidy$Sectors)


# Create the table --------------------------------------------------------

# Quarter names
q_names <- rep(
  str_c("Q", 1:4),
  times = length(unique(sectors_col_names$yr))
)

q_names <- q_names[-length(q_names)] 

gt_col_labels <- sectors_col_names %>% 
  select(-c(2:3)) %>% 
  arrange(date) %>% 
  mutate(
    q = q_names
  ) %>% 
  deframe()

# Color palette
col_pal_gt <- function(x) {
  negative_values <- scales::col_numeric(
    palette = paletteer::paletteer_c("ggthemes::Red", n = 5) %>% 
      as.character(),
    domain = c(
      min(sectors_tidy[, -1]),
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
      max(sectors_tidy[, -1])
    ),
    na.color = "white"
  )
  ifelse(x < 0, negative_values(x), positive_values(x))
}

# Table
sectors_gt <- sectors_tidy %>% 
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
    columns = 10:ncol(sectors_tidy)
  ) %>% 
  cols_label(.list = gt_col_labels) %>% 
  cols_align(
    align = "auto",
    columns = T
  ) %>% 
  data_color(
    columns = 2:ncol(sectors_tidy),
    colors = col_pal_gt
  ) %>% 
  tab_header(
    title = html("<b>Economy-wide downturn</b>"),
    subtitle = html(
      str_c(
        "GDP, selected sectors",
        "<br>",
        "(percent change on a year earlier)"
      )
    )
  ) %>% 
  tab_source_note(
    source_note = gt::html(str_c(
      "Table: @dzulfiqarfr | Source: Statistics Indonesia (BPS)"
    )
    )
  ) %>% 
  tab_options(
    heading.align = "left",
    heading.border.bottom.color = "white",
    heading.title.font.size = 14,
    heading.subtitle.font.size = 13,
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
    locations = list(
      cells_column_labels(2:ncol(sectors_tidy))
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_spanners(everything())
    )
  )
