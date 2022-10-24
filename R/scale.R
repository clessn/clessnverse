#' Apply a discrete scale
#'
#' These functions allow you to apply the CLESSN's discrete scales.
#'
#' @inheritParams ggplot2::scale_discrete_manual
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
#'  p + scale_discrete_quorum(aesthetics = "colour")
#' }
#' @name scale
#' @importFrom ggplot2 scale_discrete_manual
#' @importFrom ggplot2 waiver
NULL
#' @export
#' @rdname scale
scale_discrete_quorum <- function(aesthetics, ..., values, breaks = ggplot2::waiver()) {
  ggplot2::scale_discrete_manual(aesthetics,
                                 values = c(
                                   "#FFA562", # orange
                                   "#73F956", # green
                                   "#65DAFF", # light blue
                                   "#FEADFF", # pink
                                   "#FF624D", # red
                                   "#88ADFF", # blue
                                   "#BA8FFF", # purple
                                   "#FEEC20" # yellow
                                 ),
                                 breaks,
                                 ...
  )
}
