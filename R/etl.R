######################################################
#' @title clessnverse::translateText
#' @description translates the text provided as a parameter using language autodetection in the language
#' @param text : the text to translate
#' @param engine : "azure" | "google"
#' @param target_lang : which language to translate to
#' @return a string containing the translated text
#' @examples example
#'
#'
#' @export
translateText <- function (text, engine, target_lang, fake) {
  # Translation

  if (engine == "azure") {
    base_url <- "https://api.cognitive.microsofttranslator.com/"
    path <- '/translate?api-version=3.0'
    params = paste('&to=', target_lang, sep="")
    url <- paste(base_url, path, params, sep="")

    # There atr characters that need to be escaped (or even removed) in order for the translator to
    # be able to take them
    text <- stringr::str_replace_all(text, "\\'", "\\\\'")
    text <- stringr::str_replace_all(text, "\\«", "")
    text <- stringr::str_replace_all(text, "\\»", "")
    text <- stringr::str_replace_all(text, "\\’", "\\\\'")

    headers <- httr::add_headers(`Ocp-Apim-Subscription-Key`="059a35dce0d24b99a8a5b176d95199be",
                                 `Ocp-Apim-Subscription-Region`= "canadacentral",
                                 `Content-Type` = "application/json")

    if(!fake) {
      response <- httr::POST(url, headers,
                           body = paste("[{'Text':'",text,"'}]", sep = ""),
                           encode = "json")
      response_json <- jsonlite::parse_json(httr::content(response, "text"))

      while (!is.null(response_json[1][[1]]$code) && response_json[1][[1]]$code == "429001") {
        Sys.sleep(30)

        response <- httr::POST(url, headers,
                               body = paste("[{'Text':'",text,"'}]", sep = ""),
                               encode = "json")

        response_json <- jsonlite::parse_json(httr::content(response, "text"))
      }

      return(response_json[1][[1]]$translations[[1]]$text)
    }

  } else {

    stop("not implemented")

  } # if (engine == "azure")

  return("")
}
