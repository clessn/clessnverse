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
#' @param base_size base font size, given in pts.
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#'
#' @export
theme_classic_light <- function(base_size = 11, base_family = "",
                                base_line_size = base_size / 22,
                                base_rect_size = base_size / 22) {
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

apply_clessn_dark <- function(x = 0.95, y = 0.3, just = c("right", "bottom")) {
  logo_dark <- magick::image_read("icons/clessn_dark.png")
  grid::grid.raster(logo_dark, x = x, y = y, just = just, width = grid::unit(0.7, "inches"))
}

apply_clessn_light <- function(x = 0.95, y = 0.3, just = c("right", "bottom")) {
  logo_light <- magick::image_read("icons/clessn_light.png")
  grid::grid.raster(logo_light, x = x, y = y, just = just, width = grid::unit(0.7, "inches"))
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

# Datagotchi font idea: VT323 in Google Fonts
#
# To do list
# - Add package dependencies
