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
commit_warehouse_table <- function(table_name, df, key_column, mode, credentials) {
  stop("not implemented yet")
}






