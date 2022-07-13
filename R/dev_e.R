###############################################################################
###############################################################################
###############################################################################
# Module      : dev_e.R
# Description : This module is used to gather all functions related to the 'E'
#               data EXTRACTION of the ELT methodology of the data pipelines at
#               the CLESSN
#
#               E: Extract is about extracting data from a source in order to
#               do something with it, such as transforming it, enriching it
#               and storing it somewhere else in order for it to be consumed
#               or simply move it forward in the data pipelines.
#
#               The source of the data entering the 'E' stage it typically the
#               web (tweets, web pages, youtube videos etc)
#
# WARNING     : The functions in this file HAVE NOT BEEN VERIFIED and HAVE NOT
#               been subject to the CLESSN package VERIFICATION checklist
#               Also, their relevance into the clessnverse package has not
#               been oconfirmes either




###############################################################################
###############################################################################
###############################################################################
# INTERNET (READ)

# Add functions useful for extracting data from the web here




###############################################################################
###############################################################################
###############################################################################
# DATALAKE ACCESS (WRITE)


###############################################################################
#' @title clessnverse::commit_lake_item
#' @description Adds or replaces an object in the lake in a specific path, with
#'              a specific key
#' @param data A named list object containing a key, a path and the item to be
#'             added to the data lake
#' @param metadata A named list object containing the metadata to be applied to
#'                 the lake item
#' @param mode A string object.
#'             If mode = "refresh", then if an object already exists with this
#'             key in the data lake, then it will be overwritten.
#'             If mode = "newonly", then if an object already exists with this
#'             key in the data lake, then it will be skipped and a warning will
#'             be logged
#' @param credentials These are your HUBLOT credentials.  They are obtained by
#'                    usung the hubblot::get_credentials function.
#'
#'                    ATTENTION:
#'                    DO NOT explicitely leave your cresential information
#'                    (username and password) in your code.  Use environment
#'                    variables instead, using the .Renviron file in your
#'                    user home directory.
#' @return the function returns
#'         - the hublot id of the element that was added to the datalake if
#'           the operation was successful.
#'         - 1 if there was an error.
#'         - 2 if there was a warning.
#' @examples example
#' @export
commit_lake_item <- function(data, metadata, mode, credentials) {

  if (!is.null(data$item)) {
    # This is the object "item" that we will commit to the lake item
    if (grepl("file", metadata$format)) {
      metadata$format <- gsub("file", "", metadata$format)
      file.rename(data$item, paste("file.", metadata$format, sep=""))
    } else {
      write(data$item, paste("file.", metadata$format, sep=""))
    }
  } else {

    if (!is.null(data$file)) {
      # This is an file_url to the file that we will commit to the lake item
      tryCatch(
        {
          r <- safe_httr_GET(data$file, httr::config(ssl_verifypeer=0), httr::timeout(30))
        },
        error = function(e) {
          stop(paste("could not retrieve url", data$file, "from data lake item with key", data$key))
        },
        finally = {}
      )

      if (!is.null(r$result) && r$result$status_code == 200){
        doc <- httr::content(r$result, as="text", encoding = "UTF-8")
        data$item <- doc

        write(data$item, paste("file.", metadata$format, sep=""))
      }
    } else {
      stop("invalid item or file provided in data to the clessnverse::commit_lake_item function")
    } #if (!is.null(data$file))

  } #if (!is.null(data$item))

  # check if an item with this key already exists
  existing_item <- hublot::filter_lake_items(credentials, list(key = data$key))

  if (length(existing_item$results) == 0) {
    #clessnverse::log_activity(message = paste("creating new item", data$key, "in data lake", data$path), logger = logger)
    hublot::add_lake_item(
      body = list(
        key = data$key,
        path = data$path,
        file = httr::upload_file(paste("file.", metadata$format, sep="")),
        metadata = jsonlite::toJSON(metadata, auto_unbox = T)),
      credentials)
  } else {
    if (mode == "refresh") {
      # hublot::remove_lake_item(existing_item$results[[1]]$id, credentials)
      #
      # hublot::add_lake_item(
      #   body = list(
      #     key = data$key,
      #     path = data$path,
      #     file = httr::upload_file(paste("file.", metadata$format, sep="")),
      #     metadata = jsonlite::toJSON(metadata, auto_unbox = T)),
      #   credentials)
      hublot::update_lake_item(
        id = existing_item$results[[1]]$id,
        body = list(
          key = data$key,
          path = data$path,
          file = httr::upload_file(paste("file.", metadata$format, sep="")),
          metadata = jsonlite::toJSON(metadata, auto_unbox = T)),
        credentials)

    } else {
      warning(paste("not updating existing item", data$key, "in data lake", data$path, "because mode is", mode))
    }
  }

  if (file.exists(paste("file.", metadata$format, sep=""))) file.remove(paste("file.", metadata$format, sep=""))
}







