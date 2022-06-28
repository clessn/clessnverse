##############################################################################
##                                                                          ##
##  This file contains function dedicated to data loading                   ##
##                                                                          ##
##  - Wrappers for hublotr package to write (commit data to hublot)         ##
##  - Other examples...???                                                  ##
##                                                                          ##
##############################################################################


######################################################
#' @title clessnverse::commit_mart_row
#' @description adds or replaces a rown in a datamart with a specific key
#' @param
#' @return
#' @examples example
#' @export
#'
commit_mart_row <- function(table, key, row = list(), mode = "refresh", credentials) {
    # If the row with the same key exist and mode=refresh then overwrite it with the new data
    # Otherwise, do nothing (just log a message)
    table <- paste("clhub_tables_datamart_", table, sep="")

    filter <- list(key__exact = key)
    item <- hublot::filter_table_items(table, credentials, filter)

    if(length(item$results) == 0) {
        # l'item n'existe pas déjà dans hublot
        hublot::add_table_item(table,
                             body = list(key = key, timestamp = Sys.time(), data = row),
                             credentials)
    } else {
        # l'item existe déjà dans hublot
        if (mode == "refresh") {
            hublot::update_table_item(table,
                                    id = item$result[[1]]$id,
                                    body = list(key = key, timestamp = as.character(Sys.time()), data = jsonlite::toJSON(row, auto_unbox = T)),
                                    credentials)
        } else {
            # Do nothing but log a message saying skipping
        } # if (mode == "refresh")
    } #if(length(item$results) == 0)
}





######################################################
#' @title clessnverse::commit_mart_table
#' @description adds or replaces a table in a datamart with a specific key
#' @param
#' @return
#' @examples example
#' @export
#'
commit_mart_table <- function(table_name, df, key_column, mode, credentials) {
  table_name <- paste("clhub_tables_mart_", table_name, sep="")

  df <- as.data.frame(df)

  for (i in 1:nrow(df)) {
    key <- df[[key_column]][i]

    filter <- list(key__exact = key)
    item <- hublot::filter_table_items(table_name, credentials, filter)


    row <- as.list(df[i,])

    if(length(item$results) == 0) {
      # l'item n'existe pas déjà dans hublot
      hublot::add_table_item(table_name,
                             body = list(key = key, timestamp = Sys.time(), data = row),
                             credentials)
    } else {
      # l'item existe déjà dans hublot
      if (mode == "refresh") {
        hublot::update_table_item(table_name,
                                  id = item$result[[1]]$id,
                                  body = list(key = key, timestamp = as.character(Sys.time()), data = jsonlite::toJSON(row, auto_unbox = T)),
                                  credentials)
      } else {
        # Do nothing but log a message saying skipping
      } # if (mode == "refresh")
    } #if(length(item$results) == 0)

  } #for (i in 1:nrow(df))
}





#####################################################
#' @title clessnverse::commit_warehouse_row
#' @description adds or replaces a rown in a warehouse with a specific key
#' @param
#' @return
#' @examples example
#' @export
#'
commit_warehouse_row <- function(table, key, row = list(), mode = "refresh", credentials) {
    # If the row with the same key exist and mode=refresh then overwrite it with the new data
    # Otherwise, do nothing (just log a message)
    table <- paste("clhub_tables_warehouse_", table, sep="")

    filter <- list(key__exact = key)
    item <- hublot::filter_table_items(table, credentials, filter)

    r <- list()

    if(length(item$results) == 0) {
        # l'item n'existe pas déjà dans hublot
        r <- hublot::add_table_item(table,
                             body = list(key = key, timestamp = Sys.time(), data = row),
                             credentials)
    } else {
        # l'item existe déjà dans hublot
        if (mode == "refresh") {
            r <- hublot::update_table_item(table,
                                    id = item$result[[1]]$id,
                                    body = list(key = key, timestamp = as.character(Sys.time()), data = jsonlite::toJSON(row, auto_unbox = T)),
                                    credentials)
        } else {
            # Do nothing but log a message saying skipping
            r <- list(id=0)
        } # if (mode == "refresh")
    } #if(length(item$results) == 0)

    return(r$id)
}





######################################################
#' @title clessnverse::commit_lake_item
#' @description adds or replaces an object in the lake in a specific path with a specific key
#' @param
#' @return
#' @examples example
#' @export
#'
commit_lake_item <- function(data, metadata, mode, credentials, logger = NULL) {

    write(data$item, paste("file.", metadata$format, sep=""))

    # check if an item with this key already exists
    existing_item <- hublot::filter_lake_items(credentials, list(key = data$key))

    if (length(existing_item$results) == 0) {
        clessnverse::logit(scriptname, paste("creating new item", data$key, "in data lake", data$path), logger)
        hublot::add_lake_item(
            body = list(
            key = data$key,
            path = data$path,
            file = httr::upload_file(paste("file.", metadata$format, sep="")),
            metadata = jsonlite::toJSON(metadata, auto_unbox = T)),
            credentials)
    } else {
        if (mode == "refresh") {
            clessnverse::logit(scriptname, paste("updating existing item", data$key, "in data lake", data$path), logger)

            hublot::remove_lake_item(existing_item$results[[1]]$id, credentials)

            hublot::add_lake_item(
                body = list(
                key = data$key,
                path = data$path,
                file = httr::upload_file(paste("file.", metadata$format, sep="")),
                metadata = jsonlite::toJSON(metadata, auto_unbox = T)),
                credentials)
        } else {
            clessnverse::logit(scriptname, paste("not updating existing item", data$key, "in data lake", data$path, "because mode is", mode), logger)
        }
    }

    file.remove(paste("file.", metadata$format, sep=""))
}
