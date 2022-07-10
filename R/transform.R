###############################################################################
#' Retrieves a dictionary from hublot.
#'
#' Creates a dictionary object from a dictionary located in
#' the CLESSN data lake (hublot).
#' @param topic The name or topic of the dictionary to retrieve from hublot.
#' @param lang The language of the dictionary. "en" is for English,
#' "fr" is for French. Both are included by default.
#' @param credentials The user's personal credentials from hublot.
#' @return A quanteda type dictionary object.
#' @examples
#'
#' \dontrun{
#'  # get credentials from hublot
#'  credentials <- hublot::get_credentials(
#'    Sys.getenv("HUB3_URL"),
#'    Sys.getenv("HUB3_USERNAME"),
#'    Sys.getenv("HUB3_PASSWORD")
#'    )
#'  # retrieve the COVID dictionary in both EN and FR
#'  clessnverse::get_dictionary("covid", c("en", "fr"), credentials)
#' }
#' @export
#'
get_dictionary <- function(topic, lang = c("en", "fr"), credentials) {
  # Validate arguments
  file_info <- hublot::retrieve_file("config_dict", credentials)
  config_dict <- utils::read.csv2(file_info$file)

  if (is.null(credentials$auth) || is.na(credentials$auth)) stop(
    "hublot credentials in clessnverse::get_dictionary are invalid")

  if (!topic %in% config_dict$topic) stop (
    paste("invalid topic in clessnverse::get_dictionary function:",
          topic,
          "\nvalid topics are",
          paste(config_dict$topic, collapse = ", ")))

  if (!unique(unlist(strsplit(config_dict$lang, ","))) %vcontains% lang) stop (
    paste("invalid language in clessnverse::get_dictionary function:",
          lang))

  # Get dictionary file from lake
  file_key <- paste("dict_", topic, sep = "")
  file_info <- hublot::retrieve_file(file_key, credentials)
  dict_df <- utils::read.csv2(file_info$file)

  # Filter on language provided in lang if language is a dictionary feature
  if (!is.null(dict_df$language)) {
    dict_df <- dict_df[dict_df$language %in% lang,]

    # Remove language column
    dict_df$language <- NULL
  }

  dict_list <- list()
  for (c in unique(dict_df$category)) {
    dict_list[[c]] <- dict_df$item[dict_df$category == c]
  }

  # Convert dataframe to quanteda dict and return it
  qdict <- quanteda::dictionary(as.list(dict_list))
  return(qdict)
}
