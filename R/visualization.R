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
#' @details
#' \describe{
#'
#' \item{`theme_classic_light()`}{
#' A classic-looking theme, with x axis line and no gridlines or axis ticks on a white background.}
#'
#' \item{`theme_classic_dark()`}{
#' A classic-looking theme, with x axis line and no gridlines or axis ticks on a dark grey background.}
#' }
#'
#' @export
theme_classic_light <- function(base_size = 11, base_family = "",
                                base_line_size = base_size / 22,
                                base_rect_size = base_size / 22,
                                base_margin = base_size * 2) {
  # Add fonts
  sysfonts::font_add_google("Roboto", "roboto")
  showtext::showtext_auto()
  # Base theme
  ggplot2::theme_classic() +
    # Changes to apply to base theme
    ggplot2::theme(
      text = ggplot2::element_text(colour = "grey30"),
      axis.title = ggplot2::element_text(hjust = 0.9),
      #axis.ticks.x = ggplot2::element_line(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = NA),
      panel.grid.major.y = ggplot2::element_line(colour = "#f7f7f7"),
      # Align caption with left
      plot.caption = ggplot2::element_text(colour = "darkgrey", face = "italic", hjust = 0),
      plot.margin = ggplot2::margin(t = base_size, r = base_margin, b = base_margin, l = base_margin, unit = "pt"),
      plot.title = ggplot2::element_text(family = "roboto", colour = "black", face = "bold", size = base_size + 10, hjust = 0),
      panel.background = ggplot2::element_rect(fill = NA)
    )
}

#' @export
theme_classic_dark <- function(base_size = 11, base_family = "",
                               base_line_size = base_size / 22,
                               base_rect_size = base_size / 22,
                               base_margin = base_size * 2) {
  sysfonts::font_add_google("Roboto", "roboto")
  showtext::showtext_auto()
  # Base theme
  ggplot2::theme_classic() +
    # Changes to apply to base theme
    ggplot2::theme(
      text = ggplot2::element_text(family = "roboto", colour = "white"),
      axis.text = ggplot2::element_text(colour = "white"),
      axis.ticks.x = ggplot2::element_line(colour = "white"),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(hjust = 0.9),
      axis.line.x = ggplot2::element_line(colour = "white"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = NA),
      # Align caption with left
      panel.grid.major.y = ggplot2::element_line(colour = "#525252"),
      plot.caption = ggplot2::element_text(hjust = 0, face = "italic"),
      plot.margin = ggplot2::margin(t = base_size, r = base_margin, b = base_margin, l = base_margin, unit = "pt"),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 10, hjust = 0),
      plot.background = ggplot2::element_rect(fill = "#494949"),  # light, dark = #494949
      panel.background = ggplot2::element_rect(fill = NA)
    )
}

## Example ----

ggplot2::ggplot(data = ggplot2::mpg) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = displ, y = cty, colour = class)) +
  ggplot2::labs(
    title = "Un très très beau graphique informatif",
    subtitle = "Un sous-titre explicatif qui en dit long sur ces données importantes",
    caption = "Données: API Twitter \nCLESSN"
  ) +
  ggplot2::xlab("x axis label") +
  ggplot2::ylab("y axis label") +
  # Custom theme
  theme_classic_light()

# Datagotchi font idea: VT323 in Google Fonts
#
# To do list
# - Add package dependencies
# genre de knit documentation
