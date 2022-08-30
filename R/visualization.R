#################################################################################################################
#################################################################################################################
########################################### Visualisation Functions #############################################
#################################################################################################################
#################################################################################################################

#' CLESSN themes
#'
#' These are themes that correspond to the CLESSN's branding and related
#' projects.
#'
# hrbrthemes::theme_ipsum()
# Tutorial: https://statisticsglobe.com/ggplot2-themes-r

theme_classic_light <- function(base_size = 12) {
  # Add fonts
  sysfonts::font_add_google("Roboto", "roboto")
  showtext::showtext_auto()
  # Base theme
  ggplot2::theme_classic() +
    # Changes to apply to base theme
    ggplot2::theme(
      text = ggplot2::element_text(colour = "grey30"),
      axis.title = ggplot2::element_text(hjust = 1),
      axis.ticks = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = NA),
      # Align caption with left
      plot.caption = ggplot2::element_text(colour = "darkgrey", face = "italic", hjust = 0),
      plot.title = ggplot2::element_text(family = "roboto", colour = "black", face = "bold", size = base_size + 10, hjust = 0),
      #plot.background = ggplot2::element_rect(fill = "#F8F8F8"),  # light, dark = #494949
      panel.background = ggplot2::element_rect(fill = NA)
    )
}

theme_classic_dark <- function(base_size = 12) {
  sysfonts::font_add_google("Roboto", "roboto")
  showtext::showtext_auto()
  # Base theme
  ggplot2::theme_classic() +
    # Changes to apply to base theme
    ggplot2::theme(
      text = ggplot2::element_text(family = "roboto", colour = "white"),
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

theme_datagotchi <- function(base_size = 12){
  sysfonts::font_add_google("VT323", "vt323")
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
      plot.title = ggplot2::element_text(family = "vt323", size = base_size + 8, hjust = 0),
      plot.background = ggplot2::element_rect(fill = "#9ebaa5"), # Not actual colour
      panel.background = ggplot2::element_rect(fill = NA)
    )
}

## Example ----

ggplot2::ggplot(data = ggplot2::mpg) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = displ, y = cty, colour = class)) +
  ggplot2::labs(
    title = "Un très très beau graphique informatif",
    subtitle = "Un sous-titre explicatif qui en dit long sur ces données importantes",
    caption = "Données: API Twitter"
  ) +
  ggplot2::xlab("x axis label") +
  ggplot2::ylab("y axis label") +
  # Custom theme
  theme_classic_light()

# Load image
logo_light <- magick::image_read("icons/clessn_light.png")
logo_dark <- magick::image_read("icons/clessn_dark.png")
grid::grid.raster(logo_dark, x = 0.95, y = 0.03, just = c("right", "bottom"), width = grid::unit(0.7, "inches"))

# QUadrillé: default, none, when quadrillé, major lines only
# CLESSN logo gris pale (voir github add logo symbol, only logo, droite)
