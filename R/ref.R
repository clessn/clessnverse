######################################################
#' @title clessnverse::getDictionary
#' @description retrives the clessn dictionnary related to a specific topic
#' @param topic : "covid" | "sentiment"
#' @param method : "wordmatch" | "regex" | "dfm"
#' @param language = "en" | "fr" | ""
#' @return Formal class 'dictionary2' of package "quanteda" for method "dfm" or a vector of regex for method "regex"
#' @examples example
#'
#'
#'
#' @export
getDictionary <- function(topic, method, language = "") {
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
    return(quanteda::dictionary(list(covid=stats::na.omit(c(dict.xlsx$`DICT-FR`,dict.xlsx$`DICT-EN`)))))
  }

  if (topic == "sentiment" && method == "dfm" && language == "fr")
    return( quanteda::dictionary(file = "../projet-quorum/_SharedFolder_projet-quorum/lexicoder_french/frlsd.cat") )

  if (topic == "sentiment" && method == "dfm" && language == "en")
    return( quanteda::data_dictionary_LSD2015 )

  if (topic == "ai" && method == "dfm" && (language == "en" || language == "fr" || language =="")) {
    dict.xlsx <- openxlsx::read.xlsx("../clessn-blend/_SharedFolder_clessn-blend/dict/DictionnaireIA.xlsx")
    return(quanteda::dictionary(list(ai=stats::na.omit(c(dict.xlsx$`FR`,dict.xlsx$`EN`)))))

  }

  stop("clessnverse::getDictionary() invalid parameters combination of topic/method/language
       Valid combinations currently are :
       . topic=\'covid\', method=\'wordmatch\', language=\'\'
       . topic=\'covid\', method=\'regex\', language=\'\'
       . topic=\'covid\', method=\'dfm\', language=\'\'
       . topic=\'sentiment\', method=\'dfm\', language=\'fr\'
       . topic=\'sentiment\', method=\'dfm\', language=\'en\'
       . topic=\'ai\', method=\'dfm\'")
}


######################################################
#' @title clessnverse::getEuropeMepData
#' @description retrieves attributes of a MEP in the european parliament
#' @param mep_full_name : the full name of the MEP to lookup
#' @return a dataframe
#' @examples example
#' @importFrom stringr str_replace_all
#'
#'
#' @export
getEuropeMepData <- function (mep_full_name) {
  url <- "https://www.europarl.europa.eu/meps/fr/download/advanced/xml?name="
  mep_full_name <- stringr::str_replace_all(mep_full_name, " ", "+")
  mep_full_name <- RCurl::curlEscape(mep_full_name)
  url <- paste(url, mep_full_name, "&groupCode=&countryCode=&bodyType=ALL", sep = "")
  url <- stringr::str_replace_all(url, "%2B", "+")
  html <- RCurl::getURL(url)
  xml <- XML::xmlTreeParse(html, useInternalNodes = TRUE)
  top <- XML::xmlRoot(xml)

  fullname <- XML::xmlValue(top[["mep"]][["fullName"]][[1]])
  country <- XML::xmlValue(top[["mep"]][["country"]][[1]])
  polgroup <- XML::xmlValue(top[["mep"]][["politicalGroup"]][[1]])
  mepid <- XML::xmlValue(top[["mep"]][["id"]][[1]])
  party <- XML::xmlValue(top[["mep"]][["nationalPoliticalGroup"]][[1]])

  return(data.frame(fullname = fullname, country = country, polgroup = polgroup, mepid = mepid, party = party))
}
