#################################################################################################################
#################################################################################################################
########################################### Visualisation Functions #############################################
#################################################################################################################
#################################################################################################################

# Light theme elxn-qc2022 ----
# Interesting base themes
# - theme_classic <3
# - theme_minimal <3
# - hrbrthemes::theme_ipsum()
# Tutorial: https://statisticsglobe.com/ggplot2-themes-r

theme_classic_light <- function(base_size = 12) {
  sysfonts::font_add_google("Schoolbell", "bell")
  showtext::showtext_auto()
  # Base theme
  ggplot2::theme_classic() +
    # Changes to apply to base theme
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = NA),
      # Align caption with left
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title = ggplot2::element_text(family = "bell", face = "bold", size = base_size + 8, hjust = 0),
      plot.background = ggplot2::element_rect(fill = "#F8F8F8"),  # light, dark = #494949
      panel.background = ggplot2::element_rect(fill = NA)
    )
}

## Example ----

ggplot2::ggplot(data = ggplot2::mpg) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = displ, y = hwy, colour = manufacturer)) +
  ggplot2::labs(
    title = "J'ai trouvé comment modifier les fonts lol",
    subtitle = "Subtitle",
    caption = "Source: ggplot2::mpg"
  ) +
  ggplot2::xlab("x axis label") +
  ggplot2::ylab("y axis label") +
  # Custom theme
  theme_classic_light()

# Dark theme elxn-qc2022 ----

theme_classic_dark <- function(..., base_size = 12) {
  # Base theme
  ggplot2::theme_classic() +
    # Changes to apply to base theme
    ggplot2::theme(
      text = ggplot2::element_text(colour = "white"),
      axis.text = ggplot2::element_text(colour = "white"),
      axis.ticks = ggplot2::element_line(colour = "white"),
      axis.line = ggplot2::element_line(colour = "white"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = NA),
      # Align caption with left
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 10, hjust = 0),
      plot.background = ggplot2::element_rect(fill = "#494949"),  # light, dark = #494949
      panel.background = ggplot2::element_rect(fill = NA)
    )
}

## Example ----

ggplot2::ggplot(data = ggplot2::mpg) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = displ, y = hwy, colour = manufacturer)) +
  ggplot2::labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Source: ggplot2::mpg"
  ) +
  ggplot2::xlab("x axis label") +
  ggplot2::ylab("y axis label") +
  # Custom theme
  theme_classic_dark()

# Load image
# logo <- magick::image_read("icons/clessn_light.png")
# grid::grid.raster(logo, x = 0.90, y = 0.03, just = c('right', 'bottom'), width = grid::unit(1, 'inches'))

# QUadrillé: default, none, when quadrillé, major lines only
# CLESSN logo gris pale (voir github add logo symbol, only logo, droite)
