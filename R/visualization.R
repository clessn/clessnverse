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

theme_elxn_qc2022_light <- function(..., base_size = 12) {
  # Base theme
  ggplot2::theme_classic() +
    # Changes to apply to base theme
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0)
    )
}

## Example ----

ggplot2::ggplot(data = ggplot2::mpg) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = displ, y = hwy, colour = manufacturer)) +
  ggplot2::labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Caption"
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

