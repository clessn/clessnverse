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