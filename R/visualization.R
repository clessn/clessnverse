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
#' \item{`theme_clean_light()`}{
#' A clean-looking ggplot2 theme, with x axis line and y major gridlines on a white background.}
#'
#' \item{`theme_clean_dark()`}{
#' A clean-looking ggplot2 theme, with x axis line and y major gridlines or axis ticks on a dark grey background.}
#' }
#'
#' @export
#' @rdname theme
theme_clean_light <- function(base_size = 11,
                                base_family = "",
                                base_line_size = base_size / 22,
                                base_rect_size = base_size / 22) {

  # Set colours
  primary_colour <- "black"
  secondary_colour <- "grey30"
  minor_colour <- "#f7f7f7"
  background_colour <- "white"

  # Set parameters
  half_line <- base_size / 2
  base_margin <- base_size * 2

  # Add fonts
  sysfonts::font_add_google("Roboto", "roboto")
  showtext::showtext_auto()

  # Base theme
  ggplot2::theme_classic() +
    # Changes to apply to base theme
    ggplot2::theme(
      text = ggplot2::element_text(size = base_size, family = "roboto", colour = secondary_colour),
      axis.text = ggplot2::element_text(colour = secondary_colour),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(
          t = half_line,
          l = half_line)),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(
          r = half_line)),
      axis.ticks.x = ggplot2::element_line(colour = primary_colour),
      axis.ticks.length.x = grid::unit(half_line / 4, "pt"),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(
          r = half_line,
          t = half_line),
        hjust = 1),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(
          r = half_line,
          b = half_line),
        hjust = 1),
      axis.line.x = ggplot2::element_line(colour = primary_colour),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = NA),
      # Align caption with left
      panel.grid.major.y = ggplot2::element_line(colour = minor_colour),
      plot.caption = ggplot2::element_text(hjust = 0, face = "italic"),
      plot.margin = ggplot2::margin(t = base_size, r = base_margin, b = base_margin, l = base_margin, unit = "pt"),
      plot.title = ggplot2::element_text(face = "bold", size = base_size * 2, hjust = 0),
      plot.background = ggplot2::element_rect(fill = background_colour),  # light, dark = #494949
      panel.background = ggplot2::element_rect(fill = NA)
    )
}

#' @export
#' @rdname theme
theme_clean_dark <- function(base_size = 11,
                               base_family = "",
                               base_line_size = base_size / 22,
                               base_rect_size = base_size / 22) {

  # Set colours
  half_line <- base_size / 2
  primary_colour <- "white"
  secondary_colour <- "#f2f2f2"
  minor_colour <- "#525252"
  background_colour <- "#494949"

  # Set parameters
  half_line <- base_size / 2
  base_margin <- base_size * 2

  # Add fonts
  sysfonts::font_add_google("Roboto", "roboto")
  showtext::showtext_auto()

  # Base theme
  ggplot2::theme_classic() +
    # Changes to apply to base theme
    ggplot2::theme(
      text = ggplot2::element_text(size = base_size, family = "roboto", colour = secondary_colour),
      axis.text = ggplot2::element_text(colour = secondary_colour),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(
          t = half_line,
          l = half_line)),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(
          r = half_line)),
      axis.ticks.x = ggplot2::element_line(colour = primary_colour),
      axis.ticks.length.x = grid::unit(half_line / 4, "pt"),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(
          r = half_line,
          t = half_line),
        hjust = 1),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(
          r = half_line,
          b = half_line),
        hjust = 1),
      axis.line.x = ggplot2::element_line(colour = primary_colour),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = NA),
      # Align caption with left
      panel.grid.major.y = ggplot2::element_line(colour = minor_colour),
      plot.caption = ggplot2::element_text(hjust = 0, face = "italic"),
      plot.margin = ggplot2::margin(t = base_size, r = base_margin, b = base_margin, l = base_margin, unit = "pt"),
      plot.title = ggplot2::element_text(face = "bold", size = base_size * 2, hjust = 0),
      plot.background = ggplot2::element_rect(fill = background_colour),  # light, dark = #494949
      panel.background = ggplot2::element_rect(fill = NA)
    )
}

## Example ----

#ggplot2::ggplot(data = ggplot2::mpg) +
#  ggplot2::geom_point(mapping = ggplot2::aes(x = displ, y = cty, colour = class)) +
#  ggplot2::labs(
#    title = "Un très très beau graphique informatif",
#    subtitle = "Un sous-titre explicatif qui en dit long sur ces données importantes",
#    caption = "Données: API Twitter \nCLESSN"
#  ) +
#  ggplot2::xlab("x axis label") +
#  ggplot2::ylab("y axis label") +
#  # Custom theme
#  theme_classic_light()
#
# Datagotchi font idea: VT323 in Google Fonts
#
# To do list
# - Add package dependencies
# genre de knit documentation
