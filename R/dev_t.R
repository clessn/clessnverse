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
#   DATAWAREHOUSE ACCESS (READ)



###############################################################################
#' @title clessnverse::get_warehouse_table
#'
#' @description get_warehouse_table allows the programmer to retrieve a data
#'              table from the CLESSN data warehouse named hublot.
#'
#' @param table_name The name of the table to retrieve from the warehouse without
#'                   the 'chlub_tables_warehouse' prefix
#' @param credentials The hublot credentials obtained from the
#'                    hublot::get_credentials function
#' @param data_filter a filter on the data to be selected in the query
#' @param nbrows   Optional argument
#'                 If nbrows is greater than 0, the dataframe returned will be
#'                 limited to nbrows observations.  This is particularly useful
#'                 when trying to see if there are records in a table and what
#'                 how structured they are.
#'                 If nbrows is omitted, then all rows of the table are returned
#'
#' @return returns a dataframe containing the data warehouse table content
#'         with the document.id and creation & update time stamps
#'
#' @examples
#'
#'  # connect to hublot
#'  credentials <- hublot::get_credentials(
#'    Sys.getenv("HUB3_URL"),
#'    Sys.getenv("HUB3_USERNAME"),
#'    Sys.getenv("HUB3_PASSWORD")
#'    )
#'
#'  # gets the entire warehouse table 'people'
#'  clessnverse::get_warehouse_table('people', credentials)
#'
#'  # gets the first 10 rows of the 'political_parties_press_releases' table
#'  clessnverse::get_warehouse_table(
#'    'political_parties_press_releases',
#'    credentials,
#'    nbrows=10
#'    )
#'
#' @export
#'
get_warehouse_table <- function(table_name, credentials, data_filter=list(), nbrows=0) {

  function_name <- "get_warehouse_table"
  # validate arguments
  if (is.null(credentials$auth) || is.na(credentials$auth)) stop(
    paste("You must supply valid hublot credentials in", function_name)
  )

  data <- hublot::list_tables(credentials)
  hublot_tables_list <- tidyjson::spread_all(data)

  if (!paste("warehouse_", table_name, sep="") %in% hublot_tables_list$table_name) stop(
    paste("This table is not in hublot:", table_name)
  )

  table_longname <- paste("clhub_tables_warehouse_", table_name, sep="")

  hublot::count_table_items(table_longname, credentials)

  if (length(data_filter) == 0) {
    page <- hublot::list_table_items(table_longname, credentials)
  } else {
    page <- hublot::filter_table_items(table_longname, credentials, data_filter)
  }
  data <- list()

  repeat {
    data <- c(data, page$results)
    if (length(data_filter) == 0) {
      page <- hublot::list_next(page, credentials)
    } else {
      page <- hublot::filter_next(page, credentials)
    }
    if (is.null(page) || (nbrows != 0 && length(data) >= nbrows)) {
      break
    }
  }

  if (nbrows != 0 && length(data) >= nbrows) data <- data[1:nbrows]

  #warehouse_json_tbl <- tidyjson::spread_all(data)
  #warehouse_df <- as.data.frame(warehouse_json_tbl)
  #warehouse_df$..JSON <- NULL
  warehouse_df <- clessnverse::spread_list_to_df(data)
  warehouse_df$..JSON <- NULL

  return(warehouse_df)
}




###############################################################################
#' @title clessnverse::get_hub2_table
#' @description get_hub2_table allows the programmer to retrieve a data
#'              table from the CLESSN hub2 data warehouse.
#'              ** WARNING hub2 will be decommissionned by end of 2022 **
#'
#' @param table_name The name of the table to retrieve from the hub2 warehouse
#' @param data_filter A list containing the filters to apply against the query
#'               to retrieve the data.  Only observations in the table
#'               complyingw with the filter conditions will be returned
#' @param max_pages The number of pages to return.  A page is 1000 rows.
#'                 Tu return the entire table use *max_pages = -1*
#' @param hub_conf The hub2.0 credentials obtained from the
#'                 clessnhub::login function
#'
#' @return returns a dataframe containing the data warehouse table content
#'
#' @examples
#'
#'  clessnhub::login(
#'    Sys.getenv("HUB_USERNAME"),
#'    Sys.getenv("HUB_PASSWORD"),
#'    Sys.getenv("HUB_URL"))
#'
#'  # get the journalists intervention in press conference from the
#'  # 'agoraplus_interventions' table from hub2
#'  data_filter = list(
#'    type = "press_conference",
#'    metadata__location = "CA-QC",
#'    data__speakerType = "journalist",
#'    data__eventDate__gte = "2021-01-01",
#'    data__eventDate__lte = "2022-06-23"
#'    )
#'
#'  df <- clessnverse::get_hub2_table(
#'    table_name = 'agoraplus_interventions',
#'    data_filter = data_filter,
#'    max_pages = -1,
#'    hub_conf = hub_config
#'    )
#'
#' @export
#'
get_hub2_table <- function(table_name, data_filter=NULL, max_pages=-1, hub_conf) {

  http_post <- function(path, body, options=NULL, verify=T, hub_c) {
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

  if (!is.null(data_filter) && !class(data_filter) == "list" || length(data_filter) == 0) data_filter <- NULL

  data_filter <- jsonlite::toJSON(data_filter, auto_unbox = T)

  path <- paste("/data/", table_name, "/count/", sep="")
  response <- http_post(path, body=data_filter, hub_c = hub_conf)
  result <- httr::content(response)
  count <- result$count
  print(paste("count:", count))

  path <- paste("/data/", table_name, "/filter/", sep="")
  response <- http_post(path, body=data_filter, hub_c = hub_conf)
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
    response <- http_post(path, body=data_filter, hub_c = hub_conf)
    page <- httr::content(response)
  }

  hub2_table <- clessnverse::spread_list_to_df(data)
  return(hub2_table)
}








###############################################################################
###############################################################################
###############################################################################
#   DATAMART ACCESS (READ)



###############################################################################
#' @title clessnverse::get_mart_table
#'
#' @description get_mart_table allows the programmer to retrieve a data
#'              table from a CLESSN data mart.
#'
#' @param table_name The name of the table to retrieve from the warehouse without
#'                   the 'chlub_tables_mart' prefix
#' @param credentials The hublot credentials obtained from the hublot::
#' @param data_filter a filter on the data to be selected in the query
#' @param nbrows Optional argument
#'               If nbrows is greater than 0, the dataframe returned will be
#'               limited to nbrows observations.  This is particularly useful
#'               when trying to see if there are records in a table and what
#'               how structured they are.
#'               If nbrows is omitted, then all rows of the table are returned
#'
#' @return returns a dataframe containing the data warehouse table with a JSON
#'         attribute as well as a document.id and creation & update time stamps
#'
#' @examples
#'  # connect to hublot
#'  credentials <- hublot::get_credentials(
#'    Sys.getenv("HUB3_URL"),
#'    Sys.getenv("HUB3_USERNAME"),
#'    Sys.getenv("HUB3_PASSWORD")
#'    )
#'
#'  # gets the entire datamart political_parties_press_releases_freq
#   datamart  <- clessnverse::get_mart_table('political_parties_press_releases_freq', credentials)
#'
#'  # gets the first 10 rows of the warehouse table 'people'
#   datamart  <- clessnverse::get_mart_table('people', credentials, nbrows=10)
#'
#' @export
#'
get_mart_table <- function(table_name, credentials, data_filter=list(), nbrows=0) {

  function_name <- "get_mart_table"

  # validate arguments
  if (is.null(credentials$auth) || is.na(credentials$auth)) stop(
    paste("You must supply valid hublot credentials in", function_name)
  )

  data <- hublot::list_tables(credentials)
  hublot_tables_list <- tidyjson::spread_all(data)
  if (!paste("mart_", table_name, sep="") %in% hublot_tables_list$table_name) stop(
    paste("This table is not in hublot:", table_name)
  )

  table_longname <- paste("clhub_tables_mart_", table_name, sep="")

  if (length(data_filter) == 0) {
    page <- hublot::list_table_items(table_longname, credentials)
  } else {
    page <- hublot::filter_table_items(table_longname, credentials, data_filter)
  }
  data <- list()

  repeat {
    data <- c(data, page$results)
    if (length(data_filter) == 0) {
      page <- hublot::list_next(page, credentials)
    } else {
      page <- hublot::filter_next(page, credentials)
    }
    if (is.null(page) || (nbrows != 0 && length(data) >= nbrows)) {
      break
    }
  }

  if (nbrows != 0 && length(data) >= nbrows) data <- data[1:nbrows]

  #mart_json_tbl <- tidyjson::spread_all(data)
  #mart_df <- as.data.frame(mart_json_tbl)
  mart_df <- clessnverse::spread_list_to_df(data)
  mart_df$..JSON <- NULL

  return(mart_df)
}








###############################################################################
###############################################################################
###############################################################################
#   DATAMART ACCESS (WRITE)

###############################################################################
#' @title clessnverse::commit_mart_row
#'
#' @description commit_mart_row allows the programmer to write a row in a data
#'              table of a CLESSN data mart.
#'
#' @param table_name The name of the data mart table to write an observation to
#'                   without the 'chlub_tables_mart' prefix.
#' @param key A character string containing the unique primary key of this
#'            observation in the table.  Data integrity of the CLESSN data
#'            model is maintained by having a unique key per observation in
#'            each table.
#' @param row A named list containing the observation to write to the datamart
#'            table.  The names of the list *are the columns* of the table.
#' @param mode A character string cintaining either "refresh" or "append".
#'             If mode = "refresh" then if an observation with a key = key
#'             already exists in the table, it will be overwritten with the
#'             new values.
#'             If mode = "append" then it will be added to the table.  However
#'             if an existing observation with a key = key already exists in the
#'             table, a warning will be returned.
#' @param credentials The hublot credentials obtained from the
#'                    hublot::get_credentials function
#' @return returns a dataframe containing the data warehouse table with a JSON
#'         attribute as well as a document.id and creation & update time stamps
#'
#' @examples
#'
#' # connect to hublot
#' credentials <- hublot::get_credentials(
#'   Sys.getenv("HUB3_URL"),
#'   Sys.getenv("HUB3_USERNAME"),
#'   Sys.getenv("HUB3_PASSWORD")
#'   )
#'
#'
#' clessnverse::commit_mart_row(
#'   table_name = "political_parties_press_releases_freq",
#'   key = "QS212022",
#'   row = list(week_num=21, count=6, political_party="QS"),
#'   mode = "refresh",
#'   credentials = credentials)
#'
#' @export
#'
commit_mart_row <- function(table_name, key, row = list(), mode = "refresh", credentials) {
  # If the row with the same key exist and mode=refresh then overwrite it with the new data
  # Otherwise, do nothing (just log a message)
  table_name <- paste("clhub_tables_mart_", table_name, sep="")

  data_filter <- list(key__exact = key)
  item <- hublot::filter_table_items(table_name, credentials, data_filter)

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
}





###############################################################################
#' @title clessnverse::commit_mart_table
#'
#' @description commit_mart_row allows the programmer to write a tabe as a
#'              CLESSN data mart.
#'
#' @param table_name The name of the data mart table to store without the
#'                   'chlub_tables_mart' prefix.
#' @param df blah
#' @param key_column blah
#' @param mode A character string cintaining either "refresh" or "append".
#'             If mode = "refresh" then if an observation with a key = key
#'             already exists in the table, it will be overwritten with the
#'             new values.
#'             If mode = "append" then it will be added to the table.  However
#'             if an existing observation with a key = key already exists in the
#'             table, a warning will be returned.
#' @param credentials The hublot credentials obtained from the
#'                    hublot::get_credentials function
#'
#' @return returns a dataframe containing the data warehouse table with a JSON
#'         attribute as well as a document.id and creation & update time stamps
#'
#' @examples
#'  # connect to hublot
#'  credentials <- hublot::get_credentials(
#'    Sys.getenv("HUB3_URL"),
#'    Sys.getenv("HUB3_USERNAME"),
#'    Sys.getenv("HUB3_PASSWORD")
#'    )
#'
#'  # gets the entire datamart political_parties_press_releases_freq
#   datamart  <- clessnverse::commit_mart_table('political_parties_press_releases_freq', credentials)
#'
#'  # gets the first 10 rows of the warehouse table 'people'
#   datamart  <- clessnverse::commit_mart_table('people', credentials, 10)
#'
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
    utils::setTxtProgressBar(pb_chap, i)

    key <- df[[key_column]][i]

    data_filter <- list(key__exact = key)
    item <- hublot::filter_table_items(table_name, credentials, data_filter)

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
#' Retrieves a dictionary from hublot.
#'
#' Creates a dictionary object from a dictionary located in
#' the CLESSN data lake (hublot).
#' @param topic The name or topic of the dictionary to retrieve from hublot.
#' @param lang The language of the dictionary. "en" is for English,
#' "fr" is for French. Both are included by default.
#' @param credentials The user's personal credentials from hublot.
#' @return A quanteda type dictionary object.
#' @author CLESSN
#' @examples
#'
#' \dontrun{
#'  # get credentials from hublot
#'  credentials <- hublot::get_credentials(
#'    Sys.getenv("HUB3_URL"),
#'    Sys.getenv("HUB3_USERNAME"),
#'    Sys.getenv("HUB3_PASSWORD")
#'    )
#'  # retrieve the COVID dictionary in both EN and FR
#'  clessnverse::get_dictionary("covid", c("en", "fr"), credentials)
#' }
#' @export
#'
get_dictionary <-
  function(topic, lang = c("en", "fr"), credentials) {
    lang <- match.arg(lang)
    # Validate arguments
    file_info <- hublot::retrieve_file("config_dict", credentials)
    config_dict <- utils::read.csv2(file_info$file)

    if (is.null(credentials$auth) || is.na(credentials$auth))
      stop("hublot credentials in clessnverse::get_dictionary are invalid")

    if (!topic %in% config_dict$topic)
      stop (
        paste(
          "invalid topic in clessnverse::get_dictionary function:",
          topic,
          "\nvalid topics are",
          paste(config_dict$topic, collapse = ", ")
        )
      )

    if (!unique(unlist(strsplit(config_dict$lang, ","))) %vcontains% lang)
      stop (paste(
        "invalid language in clessnverse::get_dictionary function:",
        lang
      ))

    # Get dictionary file from lake
    file_key <- paste("dict_", topic, sep = "")
    file_info <- hublot::retrieve_file(file_key, credentials)
    dict_df <- utils::read.csv2(file_info$file)

    # Filter on language provided in lang if language is a dictionary feature
    if (!is.null(dict_df$language)) {
      dict_df <- dict_df[dict_df$language %in% lang, ]

      # Remove language column
      dict_df$language <- NULL
    }

    dict_list <- list()
    for (c in unique(dict_df$category)) {
      dict_list[[c]] <- dict_df$item[dict_df$category == c]
    }

    # Convert dataframe to quanteda dict and return it
    qdict <- quanteda::dictionary(as.list(dict_list))
    return(qdict)
  }


###############################################################################
###############################################################################
###############################################################################
#   DATA TRANSFORMATION


###############################################################################
#' @title clessnverse::compute_nb_sentences
#' @description calculates the number of sentences in a bloc of text
#' @param txt_bloc be documented
#' @return return
#' @examples # To be documented
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
#' @param txt_bloc be documented
#' @return return
#' @examples # To be documented
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
#' @param txt_bloc blah
#' @param dictionary blah
#' @return return
#' @examples # To be documented
#'
#' @export
compute_relevance_score <- function(txt_bloc, dictionary) {
  # Prepare corpus
  txt <- stringr::str_replace_all(string = txt_bloc, pattern = "M\\.|Mr\\.|Dr\\.", replacement = "")
  txt <- stringr::str_replace_all(string = txt, pattern = "(l|L)\\'", replacement = "")
  txt <- stringr::str_replace_all(string = txt, pattern = "(s|S)\\'", replacement = "")
  txt <- stringr::str_replace_all(string = txt, pattern = "(d|D)\\'", replacement = "")
  txt <- gsub("\u00a0", " ", txt)
  txt <- stringr::str_replace_all(string = txt, pattern = "  ", replacement = " ")

  tokens <- quanteda::tokens(txt, remove_punct = TRUE)
  tokens <- quanteda::tokens_remove(tokens, quanteda::stopwords("french"))
  tokens <- quanteda::tokens_remove(tokens, quanteda::stopwords("spanish"))
  tokens <- quanteda::tokens_remove(tokens, quanteda::stopwords("english"))

  # tokens <- quanteda::tokens_replace(
  #   tokens,
  #   quanteda::types(tokens),
  #   stringi::stri_replace_all_regex(quanteda::types(tokens), "[lsd]['\\p{Pf}]", ""))

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
#' @examples # To be documented
#'
#' @importFrom stats  aggregate
#'
#' @export
compute_catergory_sentiment_score <- function(txt_bloc, category_dictionary, sentiment_dictionary) {
  # Build one corpus per category and compute sentiment on each corpus
  corpus <- data.frame(doc_id = integer(), category = character(), txt = character())

  txt_bloc <- stringr::str_replace_all(string = txt_bloc, pattern = "M\\.|Mr\\.|Dr\\.", replacement = "")
  txt_bloc <- stringr::str_replace_all(string = txt_bloc, pattern = "(l|L)\\'", replacement = "")
  txt_bloc <- stringr::str_replace_all(string = txt_bloc, pattern = "(s|S)\\'", replacement = "")
  txt_bloc <- stringr::str_replace_all(string = txt_bloc, pattern = "(d|D)\\'", replacement = "")
  txt_bloc <- gsub("\u00a0", " ", txt_bloc)

  txt_bloc <- stringr::str_replace_all(string = txt_bloc, pattern = "  ", replacement = " ")

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
  # toks <- quanteda::tokens_replace(
  #   toks,
  #   quanteda::types(toks),
  #   stringi::stri_replace_all_regex(quanteda::types(toks), "[lsd]['\\p{Pf}]", ""))


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
