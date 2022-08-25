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

theme_elxn_qc2022 <- function(..., base_size = 12) {
  # Base theme
  theme_classic() %+replace%
    # Changes to apply to base theme
    theme(
      plot.title = element_text(face = "bold", hjust = 0)
    )
}

## Example ----

library("ggplot2")

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = manufacturer)) +
  labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Caption"
  ) +
  theme_elxn_qc2022()

# Dark theme elxn-qc2022 ----
