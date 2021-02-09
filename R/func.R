######################################################
#' @title clessnverse::getDictionary
#' @description retrives the clessn dictionnary related to a specific topic
#' @param topic : "covid" | "sentiment"
#' @param method : "wordmatch" | "regex" | "dfm"
#' @param language = "en" | "fr" | ""
#' @return Formal class 'dictionary2' of package "quanteda" for method "dfm" or a vector of regex for method "regex"
#' @examples
#'
#'
#'
#' @export
#'
getDictionary <- function(topic = "covid", method = "wordmatch", language = "") {
  #topic = "covid" | "sentiment"
  #method = "wordmatch" | "regex" | "dfm"
  #language = "en" | "fr" | ""

  if (topic == "covid" && method == "wordmatch" && language == "")
    return( c("covid", "covid-19", "coronavirus", "cas suspecté", "cas suspectés") )

  if (topic == "covid" && method == "regex" && language == "")
    return( c("accumulation de tests", "testing backlog",
              "antimas(ks?|ques?)",
              "(app(.*))?((covid(-|\\s)alerte?s?)|(alerte?s?(-|\\s)covid))(\\sapp(.*))?",
              "apla.ir\\s(la|les)\\s(courbes?|vagues?)", "flatten\\sthe\\s(curves?|waves?)",
              "asymptomati(ques?|c)",
              "covid(-?[:digit:]{2}?)?",
              "coronavirus",
              "cas(e|es)?\\s+confirm(ed|és?)", "confirm(ed)?\\s+cases?",
              "cas\\s+acti(fs?)|active\\s+cases?",
              "confin(ements?|ée?s?|ed?r?|és?)",
              "cliniques?\\s+de\\s+dépistage", "screening\\s+clinics?",
              "contagi(ons?|ous|eux)",
              "couvres?(-|\\s)visages?", "face(-|\\s)(cover(ings?|s?))|(masks?)",
              "consignes?\\s(de\\sdistanciation|sanitaires?|de\\sla\\ssanté\\spublique)",
              "augmentations?\\s+((des\\s+)|(du\\s+nombre\\s+des?\\s+))cas",
              "increases?\\s+(in\\s+|of\\s+)(the\\s+number\\s+of\\s+)?cases?",
              "autoisolement", "self isolation",
              "campagne aller mieux",
              "cas\\s+(cumulés|déclarés?|identifiés?|infirmés?|négatifs?|positifs?|signalés?)",
              "(cumulative|reported|identified|invalidated|negative|positive|reported)\\s+cases?",
              "centres?\\s+de\\s+(contrôle\\s+des?\\s+maladies?|dépistages?|tests?)",
              "(screening|testing|disease\\s+control)\\s+centers?(for\\s+disease\\s+control)",
              "(distancing|(public\\s+)?health)\\s+instructions?",
              "crise\\s+sanitaire", "sanita(ry|tion)\\s+crisis",
              "déclaré(s|e|es)?\\s+(positi|négati)(fs?|ves?)", "declared\\s+(positive|negative)",
              "d?r?(e|é)?confin(ements?|ant|ing|ée?s?|ed?r?|e)",
              "deuxième\\s+(vague|éclosion)", "second\\s+(wave|surge|outbreak|outburst)",
              "(negative|positive)\\s+diagnosis|diagnostics?\\s+(positifs?|négatifs?)",
              "(lignes?)?direct(eur|ives?|rices?)\\s+de\\s+la\\s+santé\\s+publique",
              "((direct(or|ives?)\\s+of(\\s+the)?)\\s+public\\s+health)|(public\\s+health\\s+(guidelines?|direct(or|ives?)))",
              "distanc(iation|e)\\s+(sociale|physique|efficace)", "(physical, effective, social)\\s+distanc(ing|e)",
              "test((ed)?é?e?s?)\\s+(quotidiens?|positi(f|vement|vely|ve)s?|n(é|e)gati(f|vement|vely|ve)s?|de\\s+prélèvements?)",
              "(negative|positive|daily)\\s+tests?",
              "(re|é)?infect(ed|ées?|é?)",
              "masques?\\s+(chirurgica(l|ux)|artisana(l|ux)|de\\s+qualité|jetables?|lavables?|obligatoires?|portés?|réutilisables?)",
              "(surgical|crafted|quality|disposable|washable|mandatory|worn|reusable|shortage\\s+of)\\s+masks?",
              "masks?(\\s+mandatory|\\s*wearing)", "mandatory\\s+masks?",
              "éclosions?\\s+dans\\s+(l|les\\s+)établissements?",
              "facilit(y|ies)\\s+outbreaks?", "outbreaks?\\s+in\\s+the\\facilit(y|ies)",
              "empêcher?\\s+la\\s+propagation", "prevent(ing)?\\s+the\\s+spread(ing?)",
              "isolements?\\s+volontaires?", "self\\s?isolat(ing|ion|e)",
              "(e|é)pid(e|é)mi(ologie|ology|es|cs|e|c)",
              "essais?\\s+cliniques?", "clinical\\s+(trials?|tests?)",
              "état\\s+dalerte\\s+maxim(ale?|um)", "maximum\\s+alert\\s+status",
              "(é|e)volution\\s+(des|of)\\s+cas(es)?",
              "freiner?\\s+la\\s+propagation", "(curb|slow\\s+down)\\s+the\\s+spread(ing)?",
              "(hausse|increase)\\s+(of|des)\\s+cas(es)?",
              "immunité\\s+(de\\s+groupe|collective)", "(herd|collective)\\s+immunity",
              "infections\\s+mondiales", "global\\s+infections",
              "investigations?\\s+de\\s+contacts?", "contacts?\\s+investigations?",
              "isolements?\\s+préventifs?", "preventive\\s+isolations?",
              "(loi\\s+sur\\s+la\\s+)?quarantaine", "quarantine(\\s+act)?",
              "(matériels?|processus|procédures?|résultats?)\\s+des?\\s+tests?", "test(ing|s)\\s+(supplies|process|results)",
              "mesures?\\s+(de\\s+(préventions?|dépistages?|distanciations?)|préventives?|sanitaires?|dhygiènes?)",
              "(prevent(tative|tion|tive)|sanitary|screening|distanci(ation|ing)|hygiene)\\s+measures?",
              "mis(es?)?\\s+en\\s+quarantaine", "quarantined?",
              "n95",
              "nouve(aux?|elles?)\\s+(cas|vagues?|infections?)","new\\s+(cases?|waves?|infections?)",
              "paliers?\\s+dalerte", "alert\\s+levels?",
              "pandemi(e|c)",
              "particules?\\s+en\\s+suspension", "airborne\\s+particles?",
              "pénurie\\s+de\\s+masques?",
              "person(nes?|s?)\\s+infect(ée?s?|ed)",
              "premières?\\s+(éclosions?|vagues?)", "first\\s+(surge|wave|outbreak)",
              "(services?|personnel|)\\s+essentiel?", "essential(services?|personnel)",
              "port(er?)\\s+(le|du|des|un)\\s+masques?", "wear(ing)?\\s+(the|a|)\\s*masks?",
              "zones?\\s+(vertes?|jaunes?|oranges?|rouges?)", "(green|yellow|orange|red)\\s+zones?",
              "lock\\s*downs?",
              "prestations?(\\s+canadienne)?\\s+durgence", "(canad(ian|a)\\s+)?emergency\\s+(response\\s+)?benefits?",
              "protocoles?\\s+de\\s+désinfections?", "sanitization\\s+protocols?",
              "reglementations?\\s+sanitaires?", "health\\s+regulations?",
              "restrictions?\\s+sanitaires?", "health\\s+restrictions?",
              "(risques?\\s+de)|sans\\s+contagions?", "risks?\\s+of\\s+contagions?",
              "contagion-?\\s?free",
              "sisoler(\\s+volontairement)?", "self-?isolat(e|ion)",
              "suivi\\s+des?\\contacts?", "contact-?\\s?tracings?",
              "((suiv(re|i)(\\s+de)?)|(track(ing)?))\\s+(la|les|the)\\s+contagions?",
              "système\\s+dalerte\\s+à\\s+code\\s+de\\s+couleurs?", "colou?red\\s+alert\\s+system",
              "trajectoire\\s+((de\\s+la\\s+maladie)|(du\\s+virus))", "trajectory\\s+of\\s+the\\s+(disease|virus)",
              "transmissions?\\s+(communautaires?|(par)?aérosol)", "(community|airborne)\\s+transmissions?",
              "trouver\\s+un\\s+vaccin", "find(ing)?\\s+a\\s+vaccine",
              "l?urgences?\\s+sanitaires?", "health\\s+emergenc(y|ies)",
              "vague\\s+1|2", "wave\\s+1|2",
              "vulnérable\\s+à\\s+la\\s+maladie", "vulnerable\\s+to\\s+the\\s+disease") )

  if (topic == "covid" && method == "dfm" && language == "") {
    dict.xlsx <- openxlsx::read.xlsx("../projet-quorum/_SharedFolder_projet-quorum/DictionnaireCOVID/DictionnaireDetaille.xlsx")
    return(quanteda::dictionary(list(covid=na.omit(c(dict.xlsx$`DICT-FR`,dict.xlsx$`DICT-EN`)))))
  }

  if (topic == "sentiment" && method == "dfm" && language == "fr")
    return( quanteda::dictionary(file = "../projet-quorum/_SharedFolder_projet-quorum/lexicoder_french/frlsd.cat") )

  if (topic == "sentiment" && method == "dfm" && language == "en")
    return( quanteda::data_dictionary_LSD2015 )

  stop("clessnverse::getDictionary() invalid parameters combination of topic/method/language
       Valid combinations currently are :
       . topic=\'covid\', method=\'wordmatch\', language=\'\'
       . topic=\'covid\', method=\'regex\', language=\'\'
       . topic=\'covid\', method=\'dfm\', language=\'\'
       . topic=\'sentiment\', method=\'dfm\', language=\'fr\'
       . topic=\'sentiment\', method=\'dfm\', language=\'en\'")
}




######################################################
#' @title clessnverse::runDictionary
#' @description Runs a dictionary against a text corpus and returns
#' @param corpusA the corpus
#' @param dataA the dataframe containing the words to check
#' @param word to be documented
#' @param dfmA to be documented
#' @param dataB to be documented
#' @param dictionaryA to be documented
#' @return dataframe
#' @examples
#'
#'
#'
#' @export
#'
runDictionary <- function(corpusA, dataA, word, dfmA, dataB, dictionaryA) {
  corpusA <- quanteda::corpus(dataA$word)
  dfmA    <- quanteda::dfm(corpusA, dictionary = dictionaryA)
  dataB   <- convert(dfmA, to = "data.frame")
  return(dataB)
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
#' @examples
#'
#'
#'
#' @export
#'
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


######################################################
#' @title clessnverse::countSentences
#' @description counts and returns the numbers of sentences in a text block
#' @param textblock : the text to analyse
#' @return sentence.count : the number of sentences in the block
#' @examples
#'
#'
#'
#' @export
#'
countSentences <- function(textblock) {
  cleanCorpus <- str_replace_all(string = textblock, pattern = "M\\.", replacement = "")
  cleanCorpus <- str_replace_all(string = cleanCorpus, pattern = "Mr\\.", replacement = "")
  cleanCorpus <- str_replace_all(string = cleanCorpus, pattern = "Dr\\.", replacement = "")
  dfSentences <- tibble(text = cleanCorpus) %>% unnest_tokens(sentence, text, token="sentences",
                                                              format="text")
  sentence.count <- nrow(dfSentences)
  return(sentence.count)
}


######################################################
#' @title clessnverse::countVecSentences
#' @description counts and returns the umber of sentences in a vector of text blocks (strings)
#' @param vecCorpus the vector of text strings
#' @return
#' @examples
#'
#'
#'
#' @export
#'
countVecSentences <- function(vecCorpus) {
  sentence.count <- 0

  for (i in 1:length(vecCorpus)) {
    cleanCorpus <- str_replace_all(string = vecCorpus[i], pattern = "M\\.", replacement = "")
    cleanCorpus <- str_replace_all(string = cleanCorpus, pattern = "Mr\\.", replacement = "")
    cleanCorpus <- str_replace_all(string = cleanCorpus, pattern = "Dr\\.", replacement = "")
    dfSentences <- tibble(text = cleanCorpus) %>% unnest_tokens(sentence, text, token="sentences",
                                                                format="text")
    sentence.count <- sentence.count + nrow(dfSentences)# - sum(words(vecCorpus[i]) %in% patterns.titres == TRUE)
  }
  return(sentence.count)
}


