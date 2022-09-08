###############################################################################
###############################################################################
###############################################################################
# Module      : dev_l.R
# Description : This module is used to gather all functions related to the L
#               of the ELT methodology of the data pipelines at the CLESSN
#
#               L: Load is about loading data previously collected from a
#               source and stored in our data lake, and converting it to a
#               structured representation in the form of a database table
#               (in a rectangular format) into our data warehouse.
#
#               As a reminder, the data stored in our data lake is stored in
#               its native format, which ost of the time is unstructured or
#               semi structured.
#
# WARNING     : The functions in this file HAVE NOT BEEN VERIFIED and HAVE NOT
#               been subject to the CLESSN package VERIFICATION checklist
#               Also, their relevance into the clessnverse package has not
#               been oconfirmes either





###############################################################################
###############################################################################
###############################################################################
#   DATALAKE (READ)




###############################################################################
###############################################################################
###############################################################################
# DATAWAREHOUSE ACCESS (READ/WRITE)


###############################################################################
#' @title clessnverse::commit_warehouse_row
#' @description adds or replaces a rown in a warehouse with a specific key
#' @param table be documented
#' @param key be documented
#' @param row be documented
#' @param mode be documented
#' @param credentials be documented
#' @return return
#' @examples # To be documented
#' @export
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



###############################################################################
#' @title clessnverse::commit_warehouse_table
#' @description adds or replaces a rown in a warehouse with a specific key
#' @param table_name be documented
#' @param df be documented
#' @param key_column be documented
#' @param mode be documented
#' @param credentials be documented
#' @return return
#' @examples # To be documented
#' @export
commit_warehouse_table <- function(table_name, df, key_column, key_encoding, non_null_constraint, refresh_data, credentials) {
  my_table <- paste("clhub_tables_warehouse_", table_name, sep = "")

  # Check if table exists
  table_check <- hublot::filter_tables(credentials, list(verbose_name = paste("warehouse_",table_name,sep="")))
  if (length(table_check$results) == 0) stop(paste("The warehouse table specified in clessnverse::commit_warehouse_table() does not exist:", table_name))

  if (stringr::str_detect(key_column, ",")) {
  key_column <- gsub(" ", "", key_column)
  key_column <- strsplit(key_column, ",")
  key_column_mode <- "one"
  }

  if (stringr::str_detect(key_column, "\\+")) {
  key_column <- gsub(" ", "", key_column)
  key_column <- strsplit(key_column, "\\+")
  key_column_mode <- "combined"
  } 

  # Compute the key
  key <- NULL
  i <- 1
  if (key_column_mode == "one") {
    while (NA %in% df[[key_column[[1]][i]]] || "" %in% df[[key_column[[1]][i]]]) {
      i <- i + 1
    }
    
    if (i <= length(key_column[[1]])) {
      key <- df[[key_column[[1]][i]]]
    } else {
      clessnverse::logit(scriptname, paste("none of the key_columns specified is totally filled with non NA or non empty values: ", paste(key_column[[1]], collapse = " ")), logger)
      stop("none of the key_columns specified is totally filled with non NA or non empty values: ", paste(key_column[[1]], collapse = " "))
    }
  }

  if (key_column_mode == "combined") {
    key <- rep("", 1, nrow(df))
    for (i in 1:length(key_column[[1]])) {
      if (is.null(df[[key_column[[1]][i]]])) {
        clessnverse::logit(scriptname, paste("column",  key_column[[1]][i], "does not exist in dataframe in function clessnverse::commit_warehouse_table()"), logger)
        stop(paste("column",  key_column[[1]][i], "does not exist in dataframe in function clessnverse::commit_warehouse_table()"))
      }

      if (NA %in% df[[key_column[[1]][i]]] || "" %in% df[[key_column[[1]][i]]]) {
        clessnverse::logit(scriptname, paste("column",  key_column[[1]][i], "contains NA or empty values and cannot be a key in function commit_warehouse_table "), logger)
        stop(paste("column",  key_column[[1]][i], "contains NA or empty values and cannot be a key in function commit_warehouse_table "))
      }

      key <- paste(key, df[[key_column[[1]][i]]], sep = "")
    }
  }

  if (key_encoding == "digest") key <- unlist(lapply(X = as.list(key), FUN = digest::digest))

  #json_combo <- df %>% mutate(json_combo = purrr::pmap(., ~jsonlite::toJSON(list(...),auto_unbox = T))) %>% select(json_combo)
  data <- df %>% mutate(data = purrr::pmap(., ~as.list(list(...)))) 
  data <- data$data

  new_df <- df %>% mutate(key = key, timestamp = as.character(Sys.time()), data=data) %>% select(key, timestamp, data)

  my_list <- do.call("mapply", c(list, new_df, SIMPLIFY = FALSE, USE.NAMES=FALSE))

  hublot::batch_create_table_items("clhub_tables_warehouse_test_batch_load", my_list, credentials)
}