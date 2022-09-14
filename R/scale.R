#' Apply a discrete scale
#'
#' These functions allow you to apply the CLESSN's discrete scales.
#'
#' @param aesthetics Aesthetic(s) that works with the scale. You can apply colour
#'  apply `colour` and `fill` aesthetics simultaneously using `aesthetics = c("colour", "fill")`
#'
#' @author Judith Bourque
#' @examples
#' \dontrun{
#' p  <- ggplot2::ggplot(data = ggplot2::mpg) +
#'  ggplot2::geom_point(mapping = ggplot2::aes(x = displ, y = cty, colour = class)) +
#'  ggplot2::labs(
#'    title = "Look at this graph!",
#'    subtitle = "What a great look, eh?",
#'    caption = "Data: Twitter API \nCLESSN"
#'    )
#'
#'  p + scale_discrete_quorum()
#' }
#'
scale_discrete_quorum <- function(aesthetics, ..., values, breaks = waiver()) {
  ggplot2::scale_discrete_manual(
    values = c(
      "#73F956", # green
      "#FFA562", # orange
      "#65DAFF", # light blue
      "#FEADFF", # pink
      "#FF624D", # red
      "#88ADFF", # blue
      "#BA8FFF", # purple
      "#FEEC20" # yellow
    )
  )
}
