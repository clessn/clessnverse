###############################################################################
###############################################################################
###############################################################################
# Module : etl.R
# Description:  This module is used to gather all functions related to the ETL
#               methodology of the data pipelines at the CLESSN
#
#               E: Extract is about extracting data from a source in order to
#               do something with it, such as transforming it, enriching it
#               and storing it somewhere else in order for it to be consummed.
#
#               The source can be either in the Internet, in the CLESSN data 
#               lake or in the CLESSN data warehouse
#


###############################################################################
###############################################################################
###############################################################################
# DATA EXTRACTION 
#   DATAWARHOUSE


###############################################################################
#' @title clessnverse::get_warhouse_table
#' @description get_warehouse_table allows the programmer to retrieve a data
#'              table from the CLESSN data warehouse named hublot.
#' @param table : The name of the table to retrieve from the warhouse without 
#'                the 
#' @param credentials : The hublot credentials obtained from the hublot::
#' @param nbrows : If nbrows is greater than 0, the dataframe returned will be
#'                 limited to nbrows observations.  This is particularly useful
#'                 when trying to see if there are records in a table and what
#'                 how structured they are. 
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





###############################################################################
###############################################################################
###############################################################################
# DATA EXTRACTION 
#   DATAMART



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

    table <- paste("clhub_tables_mart_", table, sep="")

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






###############################################################################
###############################################################################
###############################################################################
# DATA EXTRACTION 
#   DICTIONARIES




###############################################################################
###############################################################################
###############################################################################
# DATA LOAD 
# DATAWAREHOUSE


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




###############################################################################
###############################################################################
###############################################################################
# DATA LOAD 
#   DATAMART



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
    table <- paste("clhub_tables_mart_", table, sep="")

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

  pb_chap <- utils::txtProgressBar(min = 0,      # Minimum value of the progress bar
                                   max = nrow(df), # Maximum value of the progress bar
                                   style = 3,    # Progress bar style (also available style = 1 and style = 2)
                                   width = 80,  # Progress bar width. Defaults to getOption("width")
                                   char = "=")   # Character used to create the bar


  for (i in 1:nrow(df)) {
    setTxtProgressBar(pb_chap, i)

    key <- df[[key_column]][i]

    filter <- list(key__exact = key)
    item <- hublot::filter_table_items(table_name, credentials, filter)

    data_row <- as.list(df[i,] %>% select(-c("key")))

    if(length(item$results) == 0) {
      # l'item n'existe pas déjà dans hublot
      hublot::add_table_item(table_name,
                             body = list(key = key, timestamp = Sys.time(), data = data_row),
                             credentials)
    } else {
      # l'item existe déjà dans hublot
      if (mode == "refresh") {
        hublot::update_table_item(table_name,
                                  id = item$result[[1]]$id,
                                  body = list(key = key, timestamp = as.character(Sys.time()), data = jsonlite::toJSON(data_row, auto_unbox = T)),
                                  credentials)
      } else {
        # Do nothing but log a message saying skipping
      } # if (mode == "refresh")
    } #if(length(item$results) == 0)

  } #for (i in 1:nrow(df))
}




###############################################################################
###############################################################################
###############################################################################
# DATA LOAD 
# DATALAKE


######################################################
#' @title clessnverse::commit_lake_item
#' @description adds or replaces an object in the lake in a specific path with a specific key
#' @param
#' @return
#' @examples example
#' @export
#'
commit_lake_item <- function(data, metadata, mode, credentials, logger = NULL) {

    if (grepl("file", metadata$format)) {
      metadata$format <- gsub("file", "", metadata$format)
      file.rename(data$item, paste("file.", metadata$format, sep=""))
    } else {
      write(data$item, paste("file.", metadata$format, sep=""))
    }

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








###############################################################################
###############################################################################
###############################################################################
# DATA TRANSFORMATION 
# 



######################################################
#' @title clessnverse::compute_nb_sentences
#' @description calculates the number of sentences in a bloc of text
#' @param
#' @return
#' @examples example
#' @export
#'
compute_nb_sentences <- function(txt_bloc) {
    df_sentences <- tibble::tibble(text = txt_bloc) %>%
                        tidytext::unnest_tokens(sentence, text, token="sentences",format="text", to_lower = T)
                    
    nb_sentences <- nrow(df_sentences)

    return(nb_sentences)
}


######################################################
#' @title clessnverse::compute_nb_words
#' @description calculates the number of words in a bloc of text
#' @param
#' @return
#' @examples example
#' @export
#'
compute_nb_words <- function(txt_bloc) {
    df_words <- tibble::tibble(text = txt_bloc) %>%
                        tidytext::unnest_tokens(sentence, text, token="words",format="text", to_lower = T)
                    
    nb_words <- nrow(df_words)

    return(nb_words)
}


######################################################
#' @title clessnverse::compute_relevance_score
#' @description calculates the relevance of a bloc of text against a topic dictionary 
#' @param
#' @return the function returns
#' @examples example
#' @export
#'
compute_relevance_score <- function(txt_bloc, dictionary) {
    # Prepare corpus
    txt <- stringr::str_replace_all(string = txt_bloc, pattern = "M\\.|Mr\\.|Dr\\.", replacement = "")
    tokens <- quanteda::tokens(txt, remove_punct = TRUE)
    tokens <- quanteda::tokens_remove(tokens, quanteda::stopwords("french"))
    tokens <- quanteda::tokens_remove(tokens, quanteda::stopwords("spanish"))
    tokens <- quanteda::tokens_remove(tokens, quanteda::stopwords("english"))

    tokens <- quanteda::tokens_replace(
                            tokens, 
                            quanteda::types(tokens), 
                            stringi::stri_replace_all_regex(quanteda::types(tokens), "[lsd]['\\p{Pf}]", ""))

    if (length(tokens[[1]]) == 0) {
        tokens <- quanteda::tokens("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.", remove_punct = TRUE)
    } 

    dfm_corpus <- quanteda::dfm(tokens)

    # Compute Relevance on the entire corpus
    lookup <- quanteda::dfm_lookup(dfm_corpus, dictionary = dictionary, valuetype = "glob")
    df_score <- quanteda::convert(lookup, to="data.frame")
    df_score$doc_id <- NULL

    return(df_score)
}


######################################################
#' @title clessnverse::commit_mart_table
#' @description adds or replaces a table in a datamart with a specific key
#' @param
#' @return
#' @examples example
#' @export
#'
#compute_sentiment_score <- function(txt_bloc, sentiment_dictionary)



######################################################
#' @title clessnverse::commit_mart_table
#' @description adds or replaces a table in a datamart with a specific key
#' @param
#' @return
#' @examples example
#' @export
#'
compute_catergory_sentiment_score <- function(txt_bloc, category_dictionary, sentiment_dictionary) {    
    # Build one corpus per category and compute sentiment on each corpus
    corpus <- data.frame(doc_id = integer(), category = character(), text = character())

    df_sentences <- tibble::tibble(text = txt_bloc) %>%
                        tidytext::unnest_tokens(sentence, text, token="sentences",format="text", to_lower = T)
                    
    toks <- quanteda::tokens(df_sentences$sentence)

    dfm_corpus <- quanteda::dfm(toks)
    lookup <- quanteda::dfm_lookup(dfm_corpus, dictionary = category_dictionary, valuetype = "glob")
    df <- quanteda::convert(lookup, to="data.frame") %>% select(-c("doc_id"))

    df_sentences <- df_sentences %>% cbind(df)
    df_sentences <- df_sentences %>% tidyr::pivot_longer(-c(sentence), names_to = "category", values_to = "relevance")
    df_sentences <- df_sentences %>% filter(relevance > 0)

    df_categories <- df_sentences %>% dplyr::group_by(category) %>% dplyr::summarise(text = paste(sentence, collapse = " "), relevance = sum(relevance))

    df_categories$text <- stringr::str_replace_all(string = df_categories$text, pattern = "M\\.|Mr\\.|Dr\\.", replacement = "") 

    toks <- quanteda::tokens(df_categories$text)
    toks <- quanteda::tokens(df_categories$text, remove_punct = TRUE)
    # On n'enlève pas les stopwords parce qu'on veut garder "pas" ou "ne" car connotation négative
    # toks <- quanteda::tokens_remove(toks, quanteda::stopwords("french"))
    # toks <- quanteda::tokens_remove(toks, quanteda::stopwords("spanish"))
    # toks <- quanteda::tokens_remove(toks, quanteda::stopwords("english"))
    toks <- quanteda::tokens_replace(
                                toks, 
                                quanteda::types(toks), 
                                stringi::stri_replace_all_regex(quanteda::types(toks), "[lsd]['\\p{Pf}]", ""))


    if (length(toks) == 0) {
        tokens <- quanteda::tokens("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.", remove_punct = TRUE)
    } 

    dfm_corpus <- quanteda::dfm(toks)
    lookup <- quanteda::dfm_lookup(dfm_corpus, dictionary = sentiment_dictionary, valuetype = "glob")
    df <- quanteda::convert(lookup, to="data.frame") %>% select(-c("doc_id"))

    df_categories <- df_categories %>% 
                     cbind(df)

    if (nrow(df_categories) > 0) {
        df_categories <- df_categories %>%
                            dplyr::mutate(sentiment = positive - neg_positive - negative + neg_negative) %>%
                            select(-c("text"))
    }

    df_category_pads <- data.frame(category = names(category_dictionary), relevance=rep(0L, length(category_dictionary)), 
                                negative=rep(0L, length(category_dictionary)), positive=rep(0L, length(category_dictionary)), 
                                neg_positive=rep(0L, length(category_dictionary)), neg_negative=rep(0L, length(category_dictionary)),
                                sentiment=rep(0L, length(category_dictionary)))

    df_sentiments <- df_categories %>% rbind(df_category_pads)  

    df_sentiments <- aggregate(df_sentiments[,-c(1)], list(df_sentiments$category), FUN=sum)
    names(df_sentiments)[1] <- "category"

    return(df_sentiments)
}