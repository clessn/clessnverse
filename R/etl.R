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
    stop("google translation not implemented", call. = F)
  } # if (engine == "azure")

  return("Fake translation text - use fake = false if you want to consume the translation service")
}



######################################################
#' @title clessnverse::commitDeepRows
#' @description adds, inserts or deletes an observation to the Agora hub for tables having eventID+interventionSeqNum as primary key
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
commitDeepRows <- function (dfSource, dfDestination, hubTableName, modeLocalData = "skip", modeHub = "skip") {
  # Primary key is eventID + interventionSeqNum
  # use the fonction commitToHub

  # Let's handle the local data first
  for (i in 1:nrow(dfSource)) {
    current_event_id <- dfSource$eventID[i]
    current_seqnum <- dfSource$interventionSeqNum[i]
    matching_row_index <- which(dfDestination$eventID == current_event_id &
                                  dfDestination$interventionSeqNum == current_seqnum)

    # If the eventID+interventionSeqNum does not already exist append it to the dataset
    if ( (modeLocalData == "update" || modeLocalData == "rebuild" || modeLocalData == "refresh") &&
         nrow(dplyr::filter(dfDestination, eventID == current_event_id & interventionSeqNum == current_seqnum)) == 0 &&
         length(matching_row_index) == 0 ) {

      dfDestination <- dfDestination %>% rbind(dfSource[i,])
    }

    # If the eventID+interventionSeqNum already exists and update_mode is "refresh"
    # Update the existing row with primary key eventID*interventionSeqNum
    if ( modeLocalData == "refresh" &&
         nrow(dplyr::filter(dfDestination,eventID == current_event_id & interventionSeqNum == current_seqnum)) > 0 &&
         length(matching_row_index) > 0 ) {

      matching_row_index <- which(dfDestination$eventID == current_event_id &
                                  dfDestination$interventionSeqNum == current_seqnum)

      dfDestination[matching_row_index,-c(1:4)] <- dfSource[i,-c(1:4)]

    }


    # Then append it to the hub
    if ( (modeHub == "update" || modeHub == "rebuild" || modeHub == "refresh") && length(matching_row_index) == 0 ) {
      hub_row <- dfSource[i,] %>%
        mutate_if(is.numeric , replace_na, replace = 0) %>%
        mutate_if(is.character , replace_na, replace = "") %>%
        mutate_if(is.logical , replace_na, replace = 0)

      clessnhub::create_item(as.list(hub_row[1,-c(1:4)]), hubTableName)
    }

    if ( modeHub == "refresh" && length(matching_row_index) > 0 &&
         matching_row_index > 0 && dfDestination$uuid[matching_row_index] != "") {

      hub_row <- dfSource[i,-c(1:4)] %>%
        mutate_if(is.numeric , replace_na, replace = 0) %>%
        mutate_if(is.character , replace_na, replace = "") %>%
        mutate_if(is.logical , replace_na, replace = 0)

      clessnhub::edit_item(dfDestination$uuid[matching_row_index], as.list(hub_row[1,-c(1:4)]), hubTableName)
    }

  } #for (i in i:nrow(dfSource))
  return(dfDestination)
}


######################################################
#' @title clessnverse::commitSimpleRows
#' @description adds, inserts or deletes an observation to the Agora hub for tables having eventI as primary key
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
commitSimpleRows <- function (dfSource, dfDestination, hubTableName, modeLocalData = "skip", modeHub = "skip") {
  # Primary key is eventID
  # use the fonction commitToHub

  # Let's handle the local data first
  for (i in 1:nrow(dfSource)) {
    current_event_id <- dfSource$eventID[i]
    matching_row_index <- which(dfDestination$eventID == current_event_id)

    # If the eventID does not already exist append it to the dataset
    if ( (modeLocalData == "update" || modeLocalData == "rebuild" || modeLocalData == "refresh") &&
         nrow(dplyr::filter(dfDestination, eventID == current_event_id)) == 0 &&
         length(matching_row_index) == 0) {

      dfDestination <- dfDestination %>% rbind(dfSource[i,])
    }

    # If the eventID already exists and update_mode is "refresh"
    # Update the existing row with primary key eventID
    if ( modeLocalData == "refresh" &&
         nrow(dplyr::filter(dfDestination, eventID == current_event_id) > 0) &&
         length(matching_row_index) > 0 ) {

      dfDestination[matching_row_index,-c(1:4)] <- dfSource[i,-c(1:4)]
    }

    hub_row <- NULL
    # Then append it to the hub
    if ( (modeHub == "update" || modeHub == "rebuild" || modeHub == "refresh") && length(matching_row_index) == 0 ) {
      hub_row <- dfSource[i,] %>%
        mutate_if(is.numeric , replace_na, replace = 0) %>%
        mutate_if(is.character , replace_na, replace = "") %>%
        mutate_if(is.logical , replace_na, replace = 0)
      clessnverse::logit(.ChildEnv$logger, paste("creating new item in",hubTableName,":", dfSource$eventID))
      clessnhub::create_item(as.list(hub_row[1,-c(1:4)]), hubTableName)
    }

    if ( modeHub == "refresh" && length(matching_row_index) > 0 &&
         matching_row_index > 0 && dfDestination$uuid[matching_row_index] != "") {

      hub_row <- dfSource[i,-c(1:4)] %>%
        mutate_if(is.numeric , replace_na, replace = 0) %>%
        mutate_if(is.character , replace_na, replace = "") %>%
        mutate_if(is.logical , replace_na, replace = 0)

      clessnverse::logit(.ChildEnv$logger, paste("updating existing item in", hubTableName,":", dfDestination$uuid[matching_row_index]))
      clessnhub::edit_item(dfDestination$uuid[matching_row_index], as.list(hub_row[1,-c(1:4)]), hubTableName)
    }
  } #for (i in i:nrow(dfSource))

  if (is.null(hub_row)) clessnverse::logit(.ChildEnv$logger, paste(hubTableName,"not updated"))

  return(dfDestination)
}

######################################################
#' @title clessnverse::commitCacheRows
#' @description adds, inserts or deletes an observation to the Agora hub for tables having eventI as primary key
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
commitCacheRows <- function (dfSource, dfDestination, hubTableName, modeLocalData = "skip", modeHub = "skip") {
  return(clessnverse::commitSimpleRows(dfSource, dfDestination, hubTableName, modeLocalData, modeHub))
}

