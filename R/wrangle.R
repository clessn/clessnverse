#' Count NA in a vector
#'
#' @description
#' `r lifecycle::badge("experimental")`
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

#' Normalize a continuous variable between 0 and 1
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param x Numeric vector.
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

#' Reduce outliers with the interquartile range method
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Interquartile range method based off https://www.scribbr.com/statistics/outliers/
#'
#' @param vector Numeric vector.
#'
#' @return Numeric vector.
#' @export
#'
#' @examples
#'
#' # Create a random vector following a normal distribution
#' vector <- round(rnorm(20, mean = 5, sd = 3))
#' vector
#' hist(vector)
#'
#' # Add outliers
#' vector[c(4,11)] <- c(-24, 32)
#' vector
#' hist(vector)
#'
#' # Apply function
#' new_vector <- reduce_outliers(vector)
#' new_vector
#' hist(new_vector)
reduce_outliers <- function(vector) {
  q1 <- stats::quantile(vector, 0.25) # identify the first quartile
  q3 <- stats::quantile(vector, 0.75) # identify the first quartile
  iqr <- q3-q1 # calculate IQR
  lim_max <- q3 + 1.5*iqr # upper limit
  lim_min <- q1 - 1.5*iqr # lower limit
  vector[vector > lim_max] <- lim_max # each value that is bigger than the upper limit will take the value of the upper limit
  vector[vector < lim_min] <- lim_min # same thing with the lower limit
  return(vector)
}

#' Calculate the proportion of each category from one variable.
#'
#' This function creates a data.frame which includes 3 columns.
#' 1) a column containing the variable's categories;
#' 2) a column containing each category's frequency;
#' 3) a column containing each category's proportion.
#'
#' @param data An object of type data.frame.
#' @param variable The name of the variable from which to calculate
#' the proportions.
#'
#' @return A data.frame which includes 3 columns.
#' 1) `variable`: a column containing the variable's categories;
#' 2) `n`: a column containing each category's frequency;
#' 3) `prop`: a column containing each category's proportion.
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom rlang abort
#' @author CLESSN
#' @examples
#'
#' \dontrun{
#'
#' # Calculate the proportions of each cylinder configuration
#' # from mtcars.
#'
#' calculate_proportions(mtcars,cyl)
#' }
calculate_proportions <- function(data, variable) {
  if (!is.data.frame(data)) {
    rlang::abort("Argument `data` must be a data frame.")
  }
  else {
    D <- data %>%
      dplyr::group_by({
        {
          variable
        }
      }) %>%
      dplyr::summarize(n = dplyr::n()) %>% #category frequencies
      stats::na.omit() %>%
      dplyr::mutate(prop = n / sum(n))
  }
  if (length(table(D[, 1])) == 1) {
    warning(paste0("`", names(D[, 1]), "`", " only has one category."))
  }
  return(D)
}
