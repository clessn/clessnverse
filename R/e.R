##############################################################################
##                                                                          ##
##  This file contains function dedicated to data extraction                ##
##                                                                          ##
##  - Wrappers for hubr package to read (extract data from hublot)          ##
##  - Common html and xml document extraction utilities                     ##
##                                                                          ##
##############################################################################

######################################################
#' @title clessnverse::get_datalake_item
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
#' @title clessnverse::get_datamart_table
#' @description 
#' @param table : 
#' @param id : 
#' @return 
#' @examples example
#'
#'
#' @export
#' 
get_datamart_table <- function(my_table, my_credentials, max_nb_rows=0) {
    hubr::count_table_items(my_table, credentials) 

    page <- hubr::list_table_items(my_table, credentials) 
    data <- list() 

    repeat {
        data <- c(data, page$results)
        page <- hubr::list_next(page, credentials)
        if (is.null(page) || length(data) >= max_nb_rows) {
            break
        }
    }
    datamart <- tidyjson::spread_all(data)

    return(datamart)
}



#' ######################################################
#' @title clessnverse::get_datamart_item
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
