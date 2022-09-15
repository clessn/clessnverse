quorum_palette <- c(
  "#FFA562", # orange
  "#73F956", # green
  "#65DAFF", # light blue
  "#FEADFF", # pink
  "#FF624D", # red
  "#88ADFF", # blue
  "#BA8FFF", # purple
  "#FEEC20" # yellow
)

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
#'  p + scale_discrete_clessn("colour", "quorum_palette")
#' }
#' @name scale
#' @importFrom ggplot2 scale_discrete_manual
#' @importFrom ggplot2 waiver
NULL
#' @export
#' @rdname scale
scale_discrete_clessn <- function(aesthetics, ..., values, breaks = ggplot2::waiver()) {
  ggplot2::scale_discrete_manual(aesthetics,
                                 values,
                                 breaks,
                                 ...
  )
}




quorum_pal <- function(){
  scales::manual_pal(quorum_palette)
}

#' @rdname quorum_pal
#' @export
scale_colour_quorum <- function(...) {
  ggplot2::discrete_scale("colour", "quorum", quorum_pal(), ...)
}

#' @rdname quorum_pal
#' @export
scale_color_quorum <- scale_colour_quorum


#' @rdname quorum_pal
#' @export
scale_fill_quorum <- function(...) {
  ggplot2::discrete_scale('fill', 'quorum', quorum_pal(), ...)
}
