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

theme_elxn_qc2022_light <- function(..., base_size = 12) {
  # Base theme
  ggplot2::theme_classic() +
    # Changes to apply to base theme
    ggplot2::theme(
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(fill = NA),
      # Align caption with left
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0),
      plot.background = ggplot2::element_rect(fill = "#EEEEEE"),  # dark = #494949
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
  theme_elxn_qc2022_light()

# Dark theme elxn-qc2022 ----

theme_elxn_qc2022_dark <- function(..., base_size = 12) {
  # Base theme
  ggplot2::theme_classic() +
    # Changes to apply to base theme
    ggplot2::theme(
      plot.title = element_text(face = "bold", hjust = 0)
    )
}

# Arguments

# - mode: dark vs light
# - project: election-qc2022


# QUadrillé: default, none, when quadrillé, major lines only
# Pas de barre en bas
# Legend at bottom
# CLESSN logo gris pale (voir github add logo symbol, only logo, droite)
# Source gauche italique
# axis text on major line?
# Dimensions: twitter (16:9), facebook (2:3)
# Pas de métho
# Background: gris foncé et pâle
# Make a table theme?
