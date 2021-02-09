######################################################
#' @title clessnverse::countSentences
#' @description counts and returns the numbers of sentences in a text block
#' @param textblock : the text to analyse
#' @return sentence.count : the number of sentences in the block
#' @examples example
#'
#'
#' @export
countSentences <- function(textblock) {
  cleanCorpus <- stringr::str_replace_all(string = textblock, pattern = "M\\.", replacement = "")
  cleanCorpus <- stringr::str_replace_all(string = cleanCorpus, pattern = "Mr\\.", replacement = "")
  cleanCorpus <- stringr::str_replace_all(string = cleanCorpus, pattern = "Dr\\.", replacement = "")
  dfSentences <- tibble::tibble(text = cleanCorpus) %>% tidytext::unnest_tokens(sentence, text, token="sentences",
                                                              format="text")
  sentence.count <- nrow(dfSentences)
  return(sentence.count)
}


######################################################
#' @title clessnverse::countVecSentences
#' @description counts and returns the umber of sentences in a vector of text blocks (strings)
#' @param vecCorpus the vector of text strings
#' @return
#' @examples example
#' @importFrom "dplyr" "%>%"
#'
#'
#' @export
countVecSentences <- function(vecCorpus) {
  sentence.count <- 0

  for (i in 1:length(vecCorpus)) {
    cleanCorpus <- stringr::str_replace_all(string = vecCorpus[i], pattern = "M\\.", replacement = "")
    cleanCorpus <- stringr::str_replace_all(string = cleanCorpus, pattern = "Mr\\.", replacement = "")
    cleanCorpus <- stringr::str_replace_all(string = cleanCorpus, pattern = "Dr\\.", replacement = "")
    dfSentences <- tibble::tibble(text = cleanCorpus) %>% tidytext::unnest_tokens(sentence, text, token="sentences",
                                                                format="text")
    sentence.count <- sentence.count + nrow(dfSentences)# - sum(words(vecCorpus[i]) %in% patterns.titres == TRUE)
  }
  return(sentence.count)
}


######################################################
#' @title clessnverse::evaluateRelevanceIndex
#' @description Evaluates and returns the relevance index of a text block (string)
#' @param textToCheck - A character string
#' @param dictionary  - A vector of strings containing regex or words or words combinations
#'			               or an objet of type dictionary from the quanteda package
#' @param base	      - A string containing either "sentence" or "paragraph" which tells the
#'			                the function on which base to compute the index.
#'			                If base = "sentence" then every sentence containing one or more terms
#'			                matching in the dictionary will score 1 in the calculation.
#'			                At the end of the count the sum is devided by the number of sentences.
#'			                If base = "paragraph" the same logic applies but at the paragraph level
#'			                instead of the sentence level
#' @param method      - A string describing the method used for matching terms.
#'			                If method = "wordsmatch" or "regex" then dictionary must be a vector of
#'			                strings containing words, words sequences or regex.
#'			                If method = "dfm" then dictionary must be of type quanteda dictionary
#' @return a double objet representing the relevance index of a subjet in the text provided
#' @examples example
#'
#'
#'
#' @export
evaluateRelevanceIndex <- function (textToCheck, dictionary, base = "sentence", method = "dfm") {
  relevanceIndex <- 0

  if (is.na(textToCheck) || nchar(textToCheck) == 0) return(relevanceIndex)

  if (base == "paragraph") {
    vec.textToCheck <- vector()
    vec.textToCheck <- strsplit(textToCheck, "\n\n")[[1]]

    for (i in 1:length(vec.textToCheck)) {
      if (method == "dfm") {
        ###  DFM METHOD
        string.to.check <- vec.textToCheck[i]
        if (!is.na(string.to.check)) {
          # repérer le nombre de mots du dictionnaire
          # sur le sujet d'intérêt présents dans le paragraphe
          dfmA <- quanteda::dfm(string.to.check, dictionary = dictionary)
          # compter les phrases mentionnant le
          # sujet d'intérêt en excluant les NA
          if (length(dfmA@x) != 0 && dfmA@x > 0) relevanceIndex <- relevanceIndex + 1
        }
      }
      else {
        ### REGEX OU WORDS MATCHING
        string.to.check <- stringr::str_replace_all(vec.textToCheck[i], "[[:punct:]]", "")
        if ( TRUE %in% stringr::str_detect(string.to.check, dictionary) ) relevanceIndex <- relevanceIndex + 1
      }
    }

    relevanceIndex <- relevanceIndex / length(vec.textToCheck)
  } #if base == paragraph

  if (base == "sentence") {
    # M., Mr. et Dr. ne signifient pas une fin de phrase, donc supprimer ces mots
    textToCheck <- stringr::str_replace_all(string = textToCheck, pattern = "M\\.", replacement = "")
    textToCheck <- stringr::str_replace_all(string = textToCheck, pattern = "Mr\\.", replacement = "")
    textToCheck <- stringr::str_replace_all(string = textToCheck, pattern = "Dr\\.", replacement = "")

    # séparer le texte en phrases, tout en minuscules
    dfSentences <- tibble::tibble(text = textToCheck) %>%
      tidytext::unnest_tokens(sentence, text, token="sentences",format="text", to_lower = T)

    count <- 0

    for (i in 1:nrow(dfSentences)) {
      string.to.check <- dfSentences$sentence[i]
      if (method == "dfm") {
        ###  DFM METHOD
        if (!is.na(string.to.check)) {
          # repérer le nombre de mots du dictionnaire
          # sur le sujet d'intérêt présents dans la phrase
          dfmA <- quanteda::dfm(string.to.check, dictionary = dictionary)
          # compter les phrases mentionnant le
          # sujet d'intérêt en excluant les NA
          if (length(dfmA@x) != 0 && dfmA@x > 0) count <- count + 1
        }
      } else {
        ### REGEX OU WORDS MATCHING
        if ( TRUE %in% stringr::str_detect(stringr::str_replace_all(string.to.check, "[[:punct:]]", ""), dictionary) )
          count <- count + 1
      }
    }
    # pondérer les phrases concernant la COVID-19 en fonction
    # du nombre total de phrases dans l'intervention
    relevanceIndex <- count / nrow(dfSentences)
    if (is.nan(relevanceIndex)) relevanceIndex <- 0
  } #if base == sentence

  return(relevanceIndex)
}

