# Summary table


# Setup -------------------------------------------------------------------

library(tidyverse)
library(conflicted)
library(gt)


# Tidy the data ---------------------------------------------------------------

# Input the data
summary <- tibble(
  Indicators = c(
    "GDP (percent change on a year earlier)",
    "Inflation (percent)",
    "Unemployment rate (percent)",
    "Poverty rate (percent)"
  ),
  Latest = c(
    "-3.49 (Q3)",
    "1.55 (Jan)",
    "7.07 (Aug)",
    "9.78 (Mar)"
  ),
  Projection = c(
    "4.4 (2021)*",
    "2.3 (2021)*",
    "9.2 (2020)",
    "12.37 (2020)"
  )
)



# Create the table --------------------------------------------------------------------

gt_summary <- summary %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(
      columns = vars(Projection),
      rows = Projection == "9.2 (2020)"
    ),
    fn = function(x) {
      str_c(x, "<sup>&#8224;</sup>")
    }
  ) %>% 
  text_transform(
    locations = cells_body(
      columns = vars(Projection),
      rows = Projection == "12.37 (2020)"
    ),
    fn = function(x) {
      str_c(x, "<sup>&#8225;</sup>")
    }
  ) %>% 
  tab_source_note(
    source_note = gt::html(str_c(
      '<p style="color: #a9a9a9;font-size: 10px;">',
      "<b>Note:</b>",
      "<br>",
      "* World Bank's December 2020 projection",
      "<br>",
      "<sup>&#8224;</sup> Government's worst-case scenario projection",
      "<br>",
      "<sup>&#8225;</sup> SMERU Research Institute's worst-case scenario projection",
      "</p>"
    )
    )
  ) %>% 
  cols_align("right", 2) %>% 
  cols_align("right", 3) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#CFD8DC",
      weight = 1
    ),
    locations = list(
      cells_body(columns = 2:3)
    )
  ) %>% 
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2),
    column_labels.font.size = 14,
    table_body.vlines.width = 1, 
    table.border.bottom.color = "white",
    table.font.size = 13,
    data_row.padding = px(7.5),
    table.width = pct(100)
  ) %>% 
  cols_width(vars(Indicators) ~ pct(60))