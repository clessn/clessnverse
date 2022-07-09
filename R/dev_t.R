###############################################################################
###############################################################################
###############################################################################
# Module      : dev_t.R
# Description : This module is used to gather all functions related to the T
#               of the ELT methodology of the data pipelines at the CLESSN
#
#               T: Stands for 'Tranform' and is about taking data from our data
#               warehouse, combining it with other data from our data warehouse
#               and enriching it and then store it in our datamarts area
#               in order to consume it in its most refined form to answer
#               questions, conduct scientific research, or visualize it in
#               graphics.
#
#               As a reminder, the data stored in our data warehouse is stored
#               in databases in tables (rectangular format).  Observations in
#               data warehouses tables represent a structured reality as it was
#               stored in the original raw data which was harvested in out data
#               lake
#
# WARNING     : The functions in this file HAVE NOT BEEN VERIFIED and HAVE NOT
#               been subject to the CLESSN package VERIFICATION checklist
#               Also, their relevance into the clessnverse package has not
#               been oconfirmes either





###############################################################################
###############################################################################
###############################################################################
#   DATAWARHOUSE ACCESS (READ)



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
#' @return returns a dataframe containing the data warhouse table with a JSON
#'         attribute as well as a document.id and creation & update time stamps
#' @examples example
#'
#'
#' @export
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

  datawarehouse_table <- tidyjson::spread_all(data)

  return(datawarehouse_table)
}



#' Compare x to 1
#' @param x an integer
#' @NoRd
http_post <- function(path, body, options=NULL, verify=T, hub_c = hub_c) {
  token <- hub_c$token
  token_prefix <- hub_c$token_prefix
  response <- httr::POST(
    url=paste0(hub_c$url, path),
    body=body, httr::accept_json(),
    httr::content_type_json(),
    config=httr::add_headers(Authorization=paste(token_prefix, token)),
    verify=verify,
    httr::timeout(30))
  return(response)
}

###############################################################################
#' @title clessnverse::get_hub2_table
#' @description get_hub2_table allows the programmer to retrieve a data
#'              table from the CLESSN hub2 data warehouse.
#' @param table : The name of the table to retrieve from the warhouse without
#'                the
#' @param credentials : The hublot credentials obtained from the hublot::
#' @param nbrows : If nbrows is greater than 0, the dataframe returned will be
#'                 limited to nbrows observations.  This is particularly useful
#'                 when trying to see if there are records in a table and what
#'                 how structured they are.
#' @return returns a dataframe containing the data warhouse table with a JSON
#'         attribute as well as a document.id and creation & update time stamps
#' @examples example
#'
#'
#' @export
get_hub2_table <- function(table_name, hubr_filter=list(), max_pages=-1, hub_conf) {

  hubr_filter <- jsonlite::toJSON(hubr_filter, auto_unbox = T)

  path <- paste("/data/", table_name, "/count/", sep="")
  response <- http_post(path, body=hubr_filter, hub_conf)
  result <- httr::content(response)
  count <- result$count
  print(paste("count:", count))

  path <- paste("/data/", table_name, "/filter/", sep="")
  response <- http_post(path, body=hubr_filter, hub_conf)
  page <- httr::content(response)
  data = list()

  repeat {

    data <- c(data, page$results)
    print(paste(length(data), "/", count))
    path <- page$"next"

    if (is.null(path)) {
      break
    }

    max_pages <- max_pages - 1
    if (max_pages == 0)
    {
      break
    }

    path <- strsplit(path, "science")[[1]][[2]]
    response <- http_post(path, body=hubr_filter, hub_config)
    page <- httr::content(response)
  }

  hub2_table <- tidyjson::spread_all(data)
  return(hub2_table)
}



###############################################################################
###############################################################################
###############################################################################
#   DATAMART ACCESS (READ/WRITE)



###############################################################################
#' @title clessnverse::get_mart_table
#' @description This function allows to extract data from a datamart
#' @param table : the table name to fetch data from in the hub
#' @param credentials : your hub credential token
#' @param nbrows : the number of rows to fetch from the table
#' @return : a dataframe containing the columns and rows
#' @examples example
#'
#'
#' @export
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
#' @title clessnverse::commit_mart_row
#' @description adds or replaces a rown in a datamart with a specific key
#' @param
#' @return
#' @examples example
#' @export
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





###############################################################################
#' @title clessnverse::commit_mart_table
#' @description adds or replaces a table in a datamart with a specific key
#' @param
#' @return
#' @examples example
#' @importFrom utils setTxtProgressBar
#' @export
commit_mart_table <- function(table_name, df, key_column, mode, credentials) {
  table_name <- paste("clhub_tables_mart_", table_name, sep="")

  df <- as.data.frame(df)

  pb_chap <- utils::txtProgressBar(min = 0,      # Minimum value of the progress bar
                                   max = nrow(df), # Maximum value of the progress bar
                                   style = 3,    # Progress bar style (also available style = 1 and style = 2)
                                   width = 80,  # Progress bar width. Defaults to getOption("width")
                                   char = "=")   # Character used to create the bar


  for (i in 1:nrow(df)) {
    utils::setTxtProgressBar(pb_chap, i)

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
#   DICTIONARIES ACCESS (READ)







###############################################################################
###############################################################################
###############################################################################
#   DATA TRANSFORMATION


###############################################################################
#' @title clessnverse::compute_nb_sentences
#' @description calculates the number of sentences in a bloc of text
#' @param
#' @return
#' @examples example
#' @export
compute_nb_sentences <- function(txt_bloc) {
    df_sentences <- tibble::tibble(txt = txt_bloc) %>%
                        tidytext::unnest_tokens("sentence", "txt", token="sentences",format="text", to_lower = T)

    nb_sentences <- nrow(df_sentences)

    return(nb_sentences)
}


###############################################################################
#' @title clessnverse::compute_nb_words
#' @description calculates the number of words in a bloc of text
#' @param
#' @return
#' @examples example
#' @export
compute_nb_words <- function(txt_bloc) {
    df_words <- tibble::tibble(txt = txt_bloc) %>%
                        tidytext::unnest_tokens("words", "txt", token="words",format="text", to_lower = T)

    nb_words <- nrow(df_words)

    return(nb_words)
}


###############################################################################
#' @title clessnverse::compute_relevance_score
#' @description calculates the relevance of a bloc of text against a topic dictionary
#' @param
#' @return the function returns
#' @examples example
#' @export
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




###############################################################################
#' @title clessnverse::commit_mart_table
#' @description adds or replaces a table in a datamart with a specific key
#' @param txt_bloc : the bloc of text to study
#' @param category_dictionary : a topic dictionary containing the categories to calculate the sentiment on
#' @param sentiment_dictionary : sentiment lexicoder dictionary
#' @return returns a dataframe containing the sentiment score of each category in the category dictionary
#' @examples example
#'
#' @importFrom stats  aggregate
#'
#' @export
compute_catergory_sentiment_score <- function(txt_bloc, category_dictionary, sentiment_dictionary) {
    # Build one corpus per category and compute sentiment on each corpus
    corpus <- data.frame(doc_id = integer(), category = character(), txt = character())

    df_sentences <- tibble::tibble(txt = txt_bloc) %>%
                        tidytext::unnest_tokens("sentence", "txt", token="sentences",format="text", to_lower = T)

    toks <- quanteda::tokens(df_sentences$sentence)

    dfm_corpus <- quanteda::dfm(toks)
    lookup <- quanteda::dfm_lookup(dfm_corpus, dictionary = category_dictionary, valuetype = "glob")
    df <- quanteda::convert(lookup, to="data.frame") %>% select(-c("doc_id"))

    df_sentences <- df_sentences %>% cbind(df)
    df_sentences <- df_sentences %>% tidyr::pivot_longer(-c(.data$sentence), names_to = "category", values_to = "relevance")
    df_sentences <- df_sentences %>% filter(.data$relevance > 0)

    df_categories <- df_sentences %>%
      dplyr::group_by(.data$category) %>%
      dplyr::summarise(txt = paste(.data$sentence, collapse = " "), relevance = sum(.data$relevance))

    df_categories$txt <- stringr::str_replace_all(string = df_categories$txt, pattern = "M\\.|Mr\\.|Dr\\.", replacement = "")

    toks <- quanteda::tokens(df_categories$txt)
    toks <- quanteda::tokens(df_categories$txt, remove_punct = TRUE)
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
                            dplyr::mutate(sentiment = .data$positive - .data$neg_positive - .data$negative + .data$neg_negative) %>%
                            select(-c("txt"))
    }

    df_category_pads <- data.frame(category = names(category_dictionary), relevance=rep(0L, length(category_dictionary)),
                                negative=rep(0L, length(category_dictionary)), positive=rep(0L, length(category_dictionary)),
                                neg_positive=rep(0L, length(category_dictionary)), neg_negative=rep(0L, length(category_dictionary)),
                                sentiment=rep(0L, length(category_dictionary)))

    df_sentiments <- df_categories %>% rbind(df_category_pads)

    df_sentiments <- stats::aggregate(df_sentiments[,-c(1)], list(df_sentiments$category), FUN=sum)
    names(df_sentiments)[1] <- "category"

    return(df_sentiments)
}
