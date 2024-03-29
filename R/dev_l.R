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
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Adds or replaces a rown in a warehouse with a specific key
#' @param table be documented
#' @param key be documented
#' @param row be documented
#' @param refresh_data be documented
#' @param credentials be documented
#' @return return
#' @examples # To be documented
#' @export
commit_warehouse_row <- function(table, key, row = list(), refresh_data, credentials) {
    # If the row with the same key exist and mode=refresh then overwrite it with the new data
    # Otherwise, do nothing (just log a message)
    table <- paste("clhub_tables_warehouse_", table, sep="")

    if (!exists("refresh_data") || length(refresh_data) == 0 || (refresh_data != TRUE && refresh_data != FALSE)) stop("refresh_data must be provided in call to clessnverse::commit_warehouse_row() and must be TRUE or FALSE)")

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
        if (refresh_data == TRUE) {
            r <- hublot::update_table_item(table,
                                    id = item$result[[1]]$id,
                                    body = list(key = key, timestamp = as.character(Sys.time()), data = jsonlite::toJSON(row, auto_unbox = T)),
                                    credentials)
        } else {
            # Do nothing but log a message saying skipping
            r <- list(id=0)
        } # if (refresh_data == TRUE)
    } #if(length(item$results) == 0)

    return(r$id)
}



###############################################################################
#' @title clessnverse::commit_warehouse_table
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Injects the content of a dataframe into a warehouse_table
#' @param table_name This is the warhouse_table name to inject the data into
#' @param df The data frame to be injected into the table
#' @param key_columns This is a character string containing  the column names of
#'  the df dataframe that will act as the *unique* key for each record in the
#'  table.  If the column names are separed with '+' then the columns will be
#'  combined together (concatenated) to form the key.  If separeted by a ','
#'  then the first column that does not contain NAs or empty values will be used
#' @param key_encoding if key_encoding = "digets", the value of the key_columns
#'  combined together will be scambeled into a uuid.
#' @param refresh_data This parameter is not yet used.
#' @param credentials your credential to hublot
#' @return return
#' @examples # To be documented
#' @export
commit_warehouse_table <- function(table_name, df, key_columns, key_encoding, refresh_data, credentials) {
  my_table <- paste("clhub_tables_warehouse_", table_name, sep = "")

  if (!exists("refresh_data") || length(refresh_data) == 0 || (refresh_data != TRUE && refresh_data != FALSE)) stop("refresh_data must be provided in call to clessnverse::commit_warehouse_row() and must be TRUE or FALSE)")

  # Check if table exists
  table_check <- hublot::filter_tables(credentials, list(verbose_name = paste("warehouse_",table_name,sep="")))
  if (length(table_check$results) == 0) stop(paste("The warehouse table specified in clessnverse::commit_warehouse_table() does not exist:", table_name))

  if (stringr::str_detect(key_columns, ",")) {
    key_columns <- gsub(" ", "", key_columns)
    key_columns <- strsplit(key_columns, ",")
    key_columns_mode <- "one"
  }

  if (stringr::str_detect(key_columns, "\\+")) {
    key_columns <- gsub(" ", "", key_columns)
    key_columns <- strsplit(key_columns, "\\+")
    key_columns_mode <- "combined"
  }

  # Compute the key
  key <- NULL
  i <- 1
  if (key_columns_mode == "one") {
    while (NA %in% df[[key_columns[[1]][i]]] || "" %in% df[[key_columns[[1]][i]]]) {
      i <- i + 1
    }

    if (i <= length(key_columns[[1]])) {
      key <- df[[key_columns[[1]][i]]]
    } else {
      stop("none of the key_columns specified is totally filled with non NA or non empty values: ", paste(key_columns[[1]], collapse = " "))
    }
  }

  if (key_columns_mode == "combined") {
    key <- rep("", 1, nrow(df))
    for (i in 1:length(key_columns[[1]])) {
      if (is.null(df[[key_columns[[1]][i]]])) {
        stop(paste("column",  key_columns[[1]][i], "does not exist in dataframe in function clessnverse::commit_warehouse_table()"))
      }

      if (NA %in% df[[key_columns[[1]][i]]] || "" %in% df[[key_columns[[1]][i]]]) {
        stop(paste("column",  key_columns[[1]][i], "contains NA or empty values and cannot be a key in function commit_warehouse_table "))
      }

      key <- paste(key, df[[key_columns[[1]][i]]], sep = "")
    }
  }

  if (key_encoding == "digest") key <- unlist(lapply(X = as.list(key), FUN = digest::digest))

  if (TRUE %in% duplicated(key)) {
    stop(
      paste(
        "\nThere are duplicated values in the unique key composed of the key_columns passed to clessnverse::commit_warehouse_table():",
        paste(key_columns, collapse = ","),
        "\nThe positions of duplicated keys in the dataframe are",
        paste(which(duplicated(key) == TRUE), collapse = ",")
      )
    )
  }

  timestamp <- rep(as.character(Sys.time()), 1, nrow(df))

  data <- purrr::pmap(df, ~as.list(list(...)))

  new_df <- df %>%
    mutate(key = key, timestamp = timestamp, data=data) %>%
    select(key, timestamp, data)

  my_list <- do.call("mapply", c(list, new_df, SIMPLIFY = FALSE, USE.NAMES=FALSE))

  ret <- hublot::batch_create_table_items(my_table, my_list, credentials)

  return(c(created=ret$created, errors=ret$errors))
}
