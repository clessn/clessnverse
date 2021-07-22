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
#' @title clessnverse::removeSpeakerTitle
#' @description removes Mr. M. Mme from a string of characters
#' @param string : the string from which to remove the title
#' @return the updated string without the title
#' @examples example
#'
#'
#' @export
removeSpeakerTitle <- function(string) {
  result <- ""

  if ( grepl("M\\.", string) )  result <- gsub("M.", "", string)
  if ( grepl("Mme", string) ) result <- gsub("Mme", "", string)

  result <- sub("^\\s+", "", result)
  result <- sub("\\s+$", "", result)

  if ( result == "" ) result <- string

  return(result)
}

######################################################
#' @title clessnverse::splitWords
#' @description returns a list of words of a sentence
#' @param txt the string
#' @return a list of words
#' @examples example
#'
#'
#'
#' @export
splitWords <- function(txt) {
  #txt <- gsub("[[:punct:][:blank:]]+", " ", txt)
  txt <- gsub("[(\\!|\\'|\\#|\\%|\\&|\\'|\\(|\\)|\\+|\\,|\\/|\\:|\\;|\\<|\\=|\\>|\\?|\\@|\\[|\\/|\\]|\\^|\\_|\\{|\\||\\}|\\~)[:blank:]]+", " ", txt)
  #txt <- gsub("[(\\,|\\.|\\;)[:blank:]]+", " ", txt)

  txt <- trimws(txt, which="both")

  list <- strsplit(txt, "\\s+")[[1]]

  return(list)
}


######################################################
#' @title clessnverse::commitAgoraplusInterventions
#' @description adds, inserts or deletes an observation to the Agora hub for tables having eventID+interventionSeqNum as primary key
#' @param df : the dataframe containing rows to be committed to the hub & local dataframe
#' @param dataframe_mode : "skip": totally skip the transaction
#'                         "update" : update the dataset with new observations only
#'                         "refresh" : updates the dataset with new observations and updates existing observations also
#'                         "rebuild" : rebuilds the local dataset from scratch at the execution of the script & refresh the hub data
#' @param modeHub : "skip"| "update" | "refresh" | "rebuild"
#' @return the updated dataframe
#' @examples example
#'
#'
#' @export
commitAgoraplusInterventions <- function (dfDestination, type, schema, metadata, data, dataframe_mode, backend_update) {
  # Primary key is eventID + interventionSeqNum
  # use the fonction commitToHub

  # Let's handle the local data first
  for (i in 1:nrow(data)) {
    current_event_id <- data$eventID[i]
    current_seqnum <- data$interventionSeqNum[i]
    current_key <- paste(current_event_id, "-", current_seqnum, sep='')
    matching_row_index <- which(dfDestination$key == current_key)

    # If the eventID+interventionSeqNum does not already exist append it to the dataset
    if ( (dataframe_mode == "update" || dataframe_mode == "rebuild" || dataframe_mode == "refresh") &&
         nrow(dplyr::filter(dfDestination, key == current_key)) == 0 &&
         length(matching_row_index) == 0 ) {

      if ( !TRUE %in% grepl("^data.", names(dfDestination)) ) {
        # here we are working without the data part in the dataset
        metadata_to_commit <- metadata
        names(metadata_to_commit) <- paste("metadata.", names(metadata_to_commit), sep='')
        row_to_commit <- rbind(data.frame(key = current_key,
                                          type = type,
                                          schema = schema,
                                          uuid = "") %>%
                                 cbind(as.data.frame(metadata_to_commit)))
      } else {
        data_to_commit <- data[i,]
        colnames(data_to_commit) <- paste("data.", names(data_to_commit), sep='')
        metadata_to_commit <- metadata
        names(metadata_to_commit) <- paste("metadata.", names(metadata_to_commit), sep='')
        row_to_commit <- data.frame(key = current_key,
                                          schema = schema,
                                          type = type,
                                          uuid = "") %>%
                         cbind(as.data.frame(metadata_to_commit)) %>%
                         cbind(data_to_commit)
      }

      dfDestination <- dfDestination %>% rbind(row_to_commit)

    } else {
      # if refresh then replace the matching row
      if (dataframe_mode == "refresh" && nrow(dplyr::filter(dfDestination, key == current_key)) > 0 && length(matching_row_index) > 0) {
        if ( !TRUE %in% grepl("^data.", names(dfDestination)) ) {
          # we're working with download_data = false => a shortened version of what's in the hub
          metadata_to_commit <- metadata
          names(metadata_to_commit) <- paste("metadata.", names(metadata_to_commit), sep='')
          row_to_commit <- data.frame(key = current_key,
                                      type = type,
                                      schema = schema,
                                      uuid = dfDestination$uuid[matching_row_index]) %>%
                           cbind(as.data.frame(metadata_to_commit))

        } else {
          # we're working with the entire data from the hub
          data_to_commit <- data[i,]
          colnames(data_to_commit) <- paste("data.", names(data_to_commit), sep='')
          metadata_to_commit <- metadata
          names(metadata_to_commit) <- paste("metadata.", names(metadata_to_commit), sep='')
          row_to_commit <- data.frame(key = current_key,
                                            schema = schema,
                                            type = type,
                                            uuid = dfDestination$uuid[matching_row_index]) %>%
                           cbind(as.data.frame(metadata_to_commit)) %>%
                           cbind(data_to_commit)
        }
        dfDestination[matching_row_index,] <- row_to_commit

      }
    }

    # Then append it to the hub
    if ( (backend_update == "update" || backend_update == "rebuild" || backend_update == "refresh") && length(matching_row_index) == 0 ) {
      hub_row <- data[i,]
      clessnhub::create_item(table = 'agoraplus_interventions', key = current_key, type = type, schema = schema, metadata = metadata, data = as.list(hub_row))
      clessnverse::logit(paste("creating new item in","agoraplus_interventions",":", current_key), logger)
    }

    if ( backend_update == "refresh" && length(matching_row_index) > 0 &&
         matching_row_index > 0 && dfDestination$uuid[matching_row_index] != "") {

      hub_row <- data[i,]
      clessnhub::edit_item('agoraplus_interventions', current_key, type, schema, metadata, as.list(hub_row))
      clessnverse::logit(paste("updating existing item in", "agoraplus_interventions",":", dfDestination$uuid[matching_row_index], current_key), logger)
    }
  } #for (i in i:nrow(data))
  return(dfDestination)
}


######################################################
#' @title clessnverse::commitAgoraplusCache
#' @description adds, inserts or deletes an observation to the Agora hub for tables having eventID as primary key
#' @param df : the dataframe containing rows to be committed to the hub & local dataframe
#' @param dataframe_mode : "skip": totally skip the transaction
#'                         "update" : update the dataset with new observations only
#'                         "refresh" : updates the dataset with new observations and updates existing observations also
#'                         "rebuild" : rebuilds the local dataset from scratch at the execution of the script & refresh the hub data
#' @param modeHub : "skip"| "update" | "refresh" | "rebuild"
#' @return the updated dataframe
#' @examples example
#'
#'
#' @export
commitAgoraplusCache <- function (dfDestination, type, schema, metadata, data, dataframe_mode, backend_update) {
  # Primary key is eventID + interventionSeqNum
  # use the fonction commitToHub

  # Let's handle the local data first
  for (i in 1:nrow(data)) {
    current_event_id <- data$eventID[i]
    current_key <- current_event_id
    matching_row_index <- which(dfDestination$key == current_event_id)

    # If the eventID+interventionSeqNum does not already exist append it to the dataset
    if ( (dataframe_mode == "update" || dataframe_mode == "rebuild" || dataframe_mode == "refresh") &&
         nrow(dplyr::filter(dfDestination, key == current_key)) == 0 &&
         length(matching_row_index) == 0 ) {

      if ( !TRUE %in% grepl("^data.", names(dfDestination)) ) {
        # here we are working without the data part in the dataset
        metadata_to_commit <- metadata
        names(metadata_to_commit) <- paste("metadata.", names(metadata_to_commit), sep='')
        row_to_commit <- rbind(data.frame(key = current_event_id,
                                          type = type,
                                          schema = schema,
                                          uuid = "") %>%
                                 cbind(as.data.frame(metadata_to_commit)))
      } else {
        data_to_commit <- data[i,]
        colnames(data_to_commit) <- paste("data.", names(data_to_commit), sep='')
        metadata_to_commit <- metadata
        names(metadata_to_commit) <- paste("metadata.", names(metadata_to_commit), sep='')
        row_to_commit <- data.frame(key = current_key,
                                    schema = schema,
                                    type = type,
                                    uuid = "") %>%
          cbind(as.data.frame(metadata_to_commit)) %>%
          cbind(data_to_commit)
      }

      dfDestination <- dfDestination %>% rbind(row_to_commit)

    } else {
      # if refresh then replace the matching row
      if (dataframe_mode == "refresh" && nrow(dplyr::filter(dfDestination, key == current_key)) > 0 && length(matching_row_index) > 0) {
        if ( !TRUE %in% grepl("^data.", names(dfDestination)) ) {
          metadata_to_commit <- metadata
          names(metadata_to_commit) <- paste("metadata.", names(metadata_to_commit), sep='')
          row_to_commit <- data.frame(key = current_event_id,
                                      type = type,
                                      schema = schema,
                                      uuid = "") %>%
            cbind(as.data.frame(metadata_to_commit))

        } else {

          data_to_commit <- data[i,]
          colnames(data_to_commit) <- paste("data.", names(data_to_commit), sep='')
          metadata_to_commit <- metadata
          names(metadata_to_commit) <- paste("metadata.", names(metadata_to_commit), sep='')
          row_to_commit <- data.frame(key = current_key,
                                      schema = schema,
                                      type = type,
                                      uuid = "") %>%
            cbind(as.data.frame(metadata_to_commit)) %>%
            cbind(data_to_commit)
        }
        dfDestination[matching_row_index,] <- row_to_commit

      }
    }

    # Then append it to the hub
    if ( (backend_update == "update" || backend_update == "rebuild" || backend_update == "refresh") && length(matching_row_index) == 0 ) {
      hub_row <- data[i,]
      clessnhub::create_item('agoraplus_cache', current_key, type, schema, metadata, as.list(hub_row))
      clessnverse::logit(paste("creating new item in","agoraplus_cache",":", current_key), logger)
    }

    if ( backend_update == "refresh" && length(matching_row_index) > 0 &&
         matching_row_index > 0 && dfDestination$uuid[matching_row_index] != "") {

      hub_row <- data[i,]
      clessnhub::edit_item('agoraplus_cache', current_key, type, schema, metadata, as.list(hub_row))
      clessnverse::logit(paste("updating existing item in", "agoraplus_cache",":", dfDestination$uuid[matching_row_index], current_key), logger)
    }
  } #for (i in i:nrow(data))
  return(dfDestination)
}


######################################################
#' @title clessnverse::convertTextToNumberFR
#' @description
#' @param
#' @param
#' @param
#' @return
#' @examples
#'
#'
#' @export
convertTextToNumberFR <- function(word) {
  wsplit <- strsplit(tolower(word),"\\s+|\\-|et")[[1]]
  wsplit <- wsplit[wsplit != ""]
  one_digits <- list(zéro=0, un=1, une = 1, deux=2, trois=3, quatre=4, cinq=5,
                     six=6, sept=7, huit=8, neuf=9)
  teens <- list(onze=11, douze=12, treize=13, quatorze=14, quinze=15,
                seize=16, "dix-sept"=17, "dix-huit"=18, "dix-neuf"=19)
  ten_digits <- list(dix=10, dipex=10, vingt=20, trente=30, quarante=40, cinquante=50,
                     soixante=60, "soixante-dix"=70, "quatre-vingts"=80, "quatre-vingt-dix"=90)
  doubles <- c(teens,ten_digits)
  out <- 0
  i <- 1
  while(i <= length(wsplit)){
    j <- 1
    if(i==1 && wsplit[i]=="cent")
      temp <- 100
    else if(i==1 && wsplit[i]=="mille")
      temp <- 1000
    else if(wsplit[i] %in% names(one_digits))
      temp <- as.numeric(one_digits[wsplit[i]])
    else if(wsplit[i] %in% names(teens))
      temp <- as.numeric(teens[wsplit[i]])
    else if(wsplit[i] %in% names(ten_digits))
      temp <- (as.numeric(ten_digits[wsplit[i]]))
    if(i < length(wsplit) && wsplit[i+1]=="cent"){
      if(i>1 && wsplit[i-1] %in% c("cent","mille"))
        out <- out + 100*temp
      else
        out <- 100*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1]=="mille"){
      if(i>1 && wsplit[i-1] %in% c("cent","mille"))
        out <- out + 1000*temp
      else
        out <- 1000*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1] %in% names(doubles)){
      temp <- temp*100
      out <- out + temp
    }
    else{
      out <- out + temp
    }
    i <- i + j
  }
  return(list(word,out))
}


######################################################
######################################################
######################################################
#                     V1 Functions                   #
######################################################
######################################################
######################################################

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
        dplyr::mutate_if(is.numeric , tidyr::replace_na, replace = 0) %>%
        dplyr::mutate_if(is.character , tidyr::replace_na, replace = "") %>%
        dplyr::mutate_if(is.logical , tidyr::replace_na, replace = 0)

      clessnhub::v1_create_item(as.list(hub_row[1,-c(1:4)]), hubTableName)
      #clessnverse::logit(paste("creating new item in",hubTableName,":", dfSource$eventID), logger)
    }

    if ( modeHub == "refresh" && length(matching_row_index) > 0 &&
         matching_row_index > 0 && dfDestination$uuid[matching_row_index] != "") {

      hub_row <- dfSource[i,-c(1:4)] %>%
        dplyr::mutate_if(is.numeric , tidyr::replace_na, replace = 0) %>%
        dplyr::mutate_if(is.character , tidyr::replace_na, replace = "") %>%
        dplyr::mutate_if(is.logical , tidyr::replace_na, replace = 0)

      clessnhub::v1_edit_item(dfDestination$uuid[matching_row_index], as.list(hub_row[1,-c(1:4)]), hubTableName)
      #clessnverse::logit(paste("updating existing item in", hubTableName,":", dfDestination$uuid[matching_row_index]), logger)

    }

    #if (is.null(hub_row) && length(matching_row_index) > 0)
    #  clessnverse::logit(paste(hubTableName,"not updated because update mode is", modeHub, "and item already exists"), logger)

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
        dplyr::mutate_if(is.numeric , tidyr::replace_na, replace = 0) %>%
        dplyr::mutate_if(is.character , tidyr::replace_na, replace = "") %>%
        dplyr::mutate_if(is.logical , tidyr::replace_na, replace = 0)
      clessnverse::logit(paste("creating new item in",hubTableName,":", dfSource$eventID), logger)
      clessnhub::v1_create_item(as.list(hub_row[1,-c(1:4)]), hubTableName)
    }

    if ( modeHub == "refresh" && length(matching_row_index) > 0 &&
         matching_row_index > 0 && dfDestination$uuid[matching_row_index] != "") {

      hub_row <- dfSource[i,-c(1:4)] %>%
        dplyr::mutate_if(is.numeric , tidyr::replace_na, replace = 0) %>%
        dplyr::mutate_if(is.character , tidyr::replace_na, replace = "") %>%
        dplyr::mutate_if(is.logical , tidyr::replace_na, replace = 0)

      clessnverse::logit(paste("updating existing item in", hubTableName,":", dfDestination$uuid[matching_row_index]), logger)
      clessnhub::v1_edit_item(dfDestination$uuid[matching_row_index], as.list(hub_row[1,-c(1:4)]), hubTableName)
    }

    if (is.null(hub_row) && length(matching_row_index) > 0)
      clessnverse::logit(paste(hubTableName,"not updated because update mode is", modeHub, "and item already exists"), logger)

  } #for (i in i:nrow(dfSource))


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




