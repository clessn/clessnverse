######################################################
#' @title clessnverse::getDictionary
#' @description
#' @param
#' @return dataframe
#' @examples
#'
#'
#'
#' @export
#'
getDictionary <- function(topic = "covid", method = "wordmatch", language = "") {
  #topic = "covid" | "posneg" | "sentiment"
  #method = "wordmatch" | "regex" | "dfm"
  #language = "en" | "fr" | ""

  if (topic == "covid" && method == "wordmatch" && language == "")
    return( c("covid(.*) ", "coronavirus", "épidémie", "épidémies", "pandémie", "pandémies", "confinement",
              "confinements", "quarantaine", "masque", "masques", "distanciation", "distanciations", "sars-cov2",
              "epidemic", "pandemic", "epidemics", "pandemics", "virus", "quarantine", "quarantines", "distancing",
              "lockdown", "vague", "vagues", "wave","waves", "hospitalisation", "hospitalisations",
              "hospitalization", "hospitalizations", "contagion", "contagions", "éclosion", "éclosions",
              "contamination", "contaminations", "dépistage", "dépistages", "tracing", "tracings", "tracage",
              "tracages", "test", "tests", "dépistage", "infection", "infections", "infecté", "infectés",
              "cas confirmé", "cas confirmés", "confirmed case", "confirmed cases", "cas suspecté", "cas suspectés",
              "suspected case", "suspected cases", "many cases", "plusieurs cas", "contamination", "contaminations",
              "zone verte", "zone orange", "zone rouge", "au vert", "à lorange", "au rouge","green zone","orange zone",
              "red zone", "couvre visage", "applatir la courbe", "couvre-visage", "couvre-visages","couvre visages",
              "goutelettes", "deux mètres", "two meters", "2 mètres", "2 meters") )

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

  if (topic == "covid" && method == "dfm" && language == "")
    return( quanteda::dictionary(file = "../projet-quorum/_SharedFolder_projet-quorum/DictionnaireCOVID/covidlsd.cat") )

  if (topic == "sentiment" && method == "regex" && language == "fr")
    return( quanteda::dictionary(file = "../quorum-agoraplus-graphiques/_SharedFolder_quorum-agoraplus-graphiques/lexicoder_french/frlsd.cat") )

  if (topic == "sentiment" && method == "regex" && language == "en")
    return( data_dictionary_LSD2015 )

  print("clessnverse::getDictionary() : invalid parameters combination of topic/method/language")
  print("Valid combinations currently are :")
  print(". topic=\'covid\', method=\'wordmatch\', language=\'\'")
  print(". topic=\'covid\', method=\'regex\', language=\'\'")
  print(". topic=\'covid\', method=\'dfm\', language=\'\'")
  print(". topic=\'sentiment\', method=\'dfm\', language=\'fr\'")
  print(". topic=\'sentiment\', method=\'dfm\', language=\'en\'")
}




######################################################
#' @title clessnverse::runDictionary
#' @description
#' @param
#' @return dataframe
#' @examples
#'
#'
#'
#' @export
#'
runDictionaryFunction <- function(corpusA, dataA, word, dfmA, dataB, dictionaryA) {
  corpusA <- corpus(dataA$word)
  dfmA    <- dfm(corpusA, dictionary = dictionaryA)
  dataB   <- convert(dfmA, to = "data.frame")
  return(dataB)
}




######################################################
#' @title clessnverse::evaluateRelevanceIndex
#' @description
#' @param textToCheck - A character string
#'        dictionary  - A vector of strings containing regex or words or words combinations
#'			               or an objet of type dictionary from the quanteda package
#'	      base	      - A string containing either "sentence" or "paragraph" which tells the
#'			                the function on which base to compute the index.
#'			                If base = "sentence" then every sentence containing one or more terms
#'			                matching in the dictionary will score 1 in the calculation.
#'			                At the end of the count the sum is devided by the number of sentences.
#'			                If base = "paragraph" the same logic applies but at the paragraph level
#'			                instead of the sentence level
#'	      method      - A string describing the method used for matching terms.
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
      string.to.check <- str_replace_all(vec.textToCheck[i], "[[:punct:]]", "")

      if ( TRUE %in% str_detect(string.to.check, dictionary) ) relevanceIndex <- relevanceIndex + 1
    }

    relevanceIndex <- relevanceIndex / length(vec.textToCheck)
  } #if base == paragraph

  if (base == "sentence") {

    textToCheck <- str_replace_all(string = textToCheck, pattern = "M\\.", replacement = "")
    textToCheck <- str_replace_all(string = textToCheck, pattern = "Mr\\.", replacement = "")
    textToCheck <- str_replace_all(string = textToCheck, pattern = "Dr\\.", replacement = "")

    dfSentences <- tibble(text = textToCheck) %>%
      unnest_tokens(sentence, text, token="sentences",format="text")

    count <- 0

    for (i in 1:nrow(dfSentences)) {
      if (method == "dfm") {
        ###  DFM METHOD
        if (!is.na(dfSentences[i,])) {
          dfmA <- dfm(dfSentences[i,], dictionary)
          if (length(dfmA@x) != 0 && dfmA@x > 0) count <- count + 1
        }
      } else {
        ### REGEX OU WORDS MATCHING
        if ( TRUE %in% str_detect(str_replace_all(dfSentences[i,], "[[:punct:]]", ""), dictionary = ) )
          count <- count + 1
      }
    }
    relevanceIndex <- count / nrow(dfSentences)
    if (is.nan(relevanceIndex)) relevanceIndex <- 0
  } #if base == sentence

  return(relevanceIndex)
}
