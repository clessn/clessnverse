##############################################################################
##                                                                          ##
##  This file contains function dedicated to data extraction                ##
##                                                                          ##
##  - Wrappers for hublot package to read (extract data from hublot)        ##
##  - Common html and xml document extraction utilities                     ##
##                                                                          ##
##############################################################################

######################################################
#' @title clessnverse::get_lake_item
#' @description 
#' @param table : 
#' @param id : 
#' @return 
#' @examples example
#'
#'
#' @export
#' 
#' 


######################################################
#' @title clessnverse::get_warhouse_table
#' @description 
#' @param table : 
#' @param id : 
#' @return 
#' @examples example
#'
#'
#' @export
#' 
get_warehouse_table <- function(table, credentials, nbrows=0) {

    table <- paste("clhub_tables_warehouse_", table, sep="")

    hublot::count_table_items(table, credentials) 

    page <- hublot::list_table_items(table, credentials) 
    data <- list() 

    repeat {
        data <- c(data, page$results)
        page <- hublot::list_next(page, credentials)
        if (is.null(page) || (nbrows != 0 && length(data) >= nbrows)) {
            break
        }
    }

    if (nbrows != 0 && length(data) >= nbrows) data <- data[1:nbrows]

    datamart <- tidyjson::spread_all(data)

    return(datamart)
}



#' ######################################################
#' @title clessnverse::get_warhouse_item
#' @description 
#' @param table : 
#' @param id : 
#' @return 
#' @examples example
#'
#'
#' @export
#' 
#' 


######################################################
#' @title clessnverse::get_mart_table
#' @description 
#' @param table : the table name to fetch data from in the hub
#' @param credentials : your hub credential token
#' @param nbrows : the number of rows to fetch from the table
#' @return : a dataframe containing the columns and rows
#' @examples example
#'
#'
#' @export
#' 
get_mart_table <- function(table, credentials, nbrows=0) {

    table <- paste("clhub_tables_datamart_", table, sep="")

    hublot::count_table_items(table, credentials) 

    page <- hublot::list_table_items(table, credentials) 
    data <- list() 

    repeat {
        data <- c(data, page$results)
        page <- hublot::list_next(page, credentials)
        if (is.null(page) || (nbrows != 0 && length(data) >= nbrows)) {
            break
        }
    }

    if (nbrows != 0 && length(data) >= nbrows) data <- data[1:nbrows]

    datamart <- tidyjson::spread_all(data)

    return(datamart)
}



#' ######################################################
#' @title clessnverse::get_mart_item
#' @description 
#' @param table : 
#' @param id : 
#' @return 
#' @examples example
#'
#'
#' @export
#' 
#' 
