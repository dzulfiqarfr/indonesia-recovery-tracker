# indonesia economic recovery

# function to apply custom ggplot2 theme and add ier logo

# author: dzulfiqar fathur rahman
# created: 2021-02-24
# last updated: 2021-07-13


# functions ---------------------------------------------------------------

## base theme ----
theme_ier <- function(..., base_size = 12, rel_size = 0.8) {
  
  theme(
    text = element_text(size = base_size),
    axis.title = element_blank(),
    axis.text = element_text(size = rel(rel_size)),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y = element_line(color = "#CFD8DC"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold", size = rel(1)),
    plot.subtitle = element_text(size = rel(0.9), margin = margin(b = 25)),
    plot.title.position = "plot",
    plot.caption = element_text(
      size = rel(rel_size),
      color = "#757575",
      hjust = 0,
      margin = margin(t = 35)
    ),
    plot.caption.position = "plot",
    ...
  )
  
}

## preview theme ----
theme_ier_pre <- function(...) {
  
  theme(
    plot.background = element_rect(fill = "#ECEFF1", color = NA),
    plot.margin = margin(t = 50, r = 50, b = 50, l = 50),
    ...
  )
  
}

## logo ----
add_ier_logo <- function(path) {
  
  # add logo
  logo <- image_read("images/ier_hexsticker_small.png")
  
  # import base plot
  base_plot <- image_read(path)
  
  # get the plot dimension
  plot_height <- magick::image_info(base_plot)$height
  plot_width <- magick::image_info(base_plot)$width
  
  # get the logo dimension
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # get number of pixels to be 2% from the bottom of the plot
  # while accounting for the logo height
  pos_bottom <- plot_height - logo_height - plot_height * 0.025
  
  # get number of pixels to be 0.05% from the left of the plot
  pos_right <- plot_width - logo_width - 0.005 * plot_width
  
  # export the plot with a logo
  base_plot %>%
    image_composite(logo, offset = str_c("+", pos_right, "+", pos_bottom)) %>%
    image_write(path)
  
}
