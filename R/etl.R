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
translateText <- function (text, engine = "azure", target_lang = "fr", fake = TRUE) {
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
    stop("google translation not implemented")
  } # if (engine == "azure")

  return("Fake translation text - use fake = false if you want to consume the translation service")
}


######################################################
#' @title clessnverse::commitToHub
#' @description Writes the observation of a dataframe to the CLESSN Hub.  Updates it if uuid already exists and writeMode = "update"
#' @param df : the obesrvation of the dataframe to write as a 1 row dataframe
#' @param uuid : the uuid of the observation to write
#' @param table : name of the table to be updated
#' @param writeMode : same as SQL - "insert" | "update"
#' @return
#' @examples example
#'
#'
#' @export
commitToHub <- function (df, uuid, table, writeMode = "insert") {

}

######################################################
#' @title clessnverse::deleteFromHub
#' @description Deletes an observation from the CLESSN Hub based on its uuid.
#' @param uuid : the uuid of the observation to delete
#' @param table : name of the table to delete the observation from
#' @return
#' @examples example
#'
#'
#' @export
deleteFromHub <- function (uuid, table) {

}


######################################################
#' @title clessnverse::commitAgoraInterventions
#' @description adds, inserts or deletes an observation to the Agora hub
#' @param df : the dataframe containing rows to be committed to the hub & local dataframe
#' @param table : the name of the table the data must be commited to
#' @param modeLocalData : "skip": totally skip the transaction
#'                        "update" : update the dataset with new observations only
#'                        "refresh" : updates the dataset with new observations and updates existing observations also
#'                        "rebuild" : rebuilds the local dataset from scratch at the execution of the script & refresh the hub data
#' @param modeHub : "skip"| "update" | "refresh" | "rebuild"
#' @return the updated dataframe
#' @examples example
#'
#'
#' @export
commitAgoraInterventions <- function (dfSource, dfDestination, hubTableName, modeLocalData = "skip", modeHub = "skip") {
  # Primary key for interventions are eventID + interventionSeqNum
  # use the fonction commitToHub

  # Let's handle the local data first
  for (i in 1:nrow(dfSource)) {
    current_event_id <- dfSource$eventID[i]
    current_seqnum <- dfSource$eventID[i]

    if ( modeLocalData != "skip"  &&
         nrow(dplyr::filter(dfDestination, eventID == current_event_id & interventionSeqNum == current_seqnum)) == 0) {
      # If the eventID+interventionSeqNum does not already exist
      # append it to the dataset
      dfDestination <- dfDeep %>% rbind(dfSource[i,])

      if ( modeHub != "skip" && dfSource$uuid == "") {
        hub_row <- dfSource[i,] %>%
          mutate_if(is.numeric , replace_na, replace = 0) %>%
          mutate_if(is.character , replace_na, replace = "") %>%
          mutate_if(is.logical , replace_na, replace = 0)

        clessnhub::create_item(as.list(hub_row), hubTableName)
      }
    }


    if ( modeLocalData == "refresh" &&
         nrow(dplyr::filter(dfDestination, eventID == current_event_id & interventionSeqNum == current_seqnum)) > 0) {
      # If the eventID+interventionSeqNum already exists and update_mode is "refresh"
      # Update the existing row with primary key eventID*interventionSeqNum
      matching_row_index <- which(dfDestination$eventID == current_id &
                                  dfDestination$interventionSeqNum == current_seqnum)

      dfDestination[matching_row_index,] <- dfSource[i,]

      if ( modeHub == "refresh" && dfSource$uuid == "") {
        hub_row <- dfSource[i,] %>%
          mutate_if(is.numeric , replace_na, replace = 0) %>%
          mutate_if(is.character , replace_na, replace = "") %>%
          mutate_if(is.logical , replace_na, replace = 0)

        clessnhub::edit_item(dfSource$uuid, as.list(hub_row), hubTableName)
      }
    }

  } #for (i in i:nrow(dfSource))

  return(dfDestination)

}
