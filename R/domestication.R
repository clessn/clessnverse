# Domestication

#' Normalize a continuous variable between 0 and 1
#'
#' @param vector Numeric vector.
#' @param remove_na Logical. Whether missing values should be removed.
#'
#' @return Numeric vector.
#' @export
#'
#' @examples
#' data <- tibble::tibble(a = c(2, 0, 0, 0), b = c(4, 0, 0, 0))
#'
#' # Base R
#' data_output <- sapply(data, normalize_min_max)
#'
#' # Dplyr
#' library("dplyr")
#'
#' data_output <- data %>%
#'   mutate(across(c(a, b), normalize_min_max))

normalize_min_max <- function(x, remove_na = T) {
  min <- min(x, na.rm = remove_na)
  max <- max(x, na.rm = remove_na)
  output <- (x - min )/(max - min)
  return(output)
}
