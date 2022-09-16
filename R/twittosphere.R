#' Make a twittosphere
#'
#' Functions used to generate data to make a twittosphere
#'
#' @param type 'journalist' or 'media'.
#' @return a dataframe containing twitter accounts infos.
#' @importFrom clessnhub create_filter
#' @importFrom clessnhub get_items
#' @examples
#' get_twitter_accounts(type = "journalist")
#' get_twitter_accounts(type = "media")
get_twitter_accounts <- function(type = c("journalist", "media")){
  filter <- clessnhub::create_filter(type = type)
  data <- clessnhub::get_items(table = 'persons', filter = filter, download_data = TRUE)
  return(data)
}


