#' Count NA in a vector
#'
#' @param x a vector
#'
#' @return number of NA or NaN in `x` (integer)
#' @export
#'
#' @examples
#' x <- c(4, 6, NA, 3, NaN, 1)
#' count_na(x)
#'
#' z <- c(NA, NaN, "w", "a", "b", NA)
#' count_na(z)
#'
count_na <- function(x){
  return(sum(is.na(x)))
}
