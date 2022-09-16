#' Make a twittosphere
#'
#' Functions used to generate data to make a twittosphere.
#'
#' @param type 'journalist' or 'media'.
#' @return A dataframe containing twitter accounts infos.
#' @name twittosphere
#' @importFrom clessnhub create_filter
#' @importFrom clessnhub get_items
#' @importFrom clessnhub login
NULL
#' @author Hubert Cadieux
#' @examples
#' clessnhub::login(Sys.getenv("HUB2_USERNAME"), Sys.getenv("HUB2_PASSWORD"))
#' get_twitter_accounts(type = "journalist")
#' get_twitter_accounts(type = "media")
#' @export
#' @rdname twittosphere
get_twitter_accounts <- function(type = c("journalist", "media")){
  filter <- clessnhub::create_filter(type = type)
  data <- clessnhub::get_items(table = 'persons', filter = filter, download_data = TRUE)
  return(data)
}


