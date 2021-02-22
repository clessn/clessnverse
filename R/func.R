######################################################
#' @title clessnverse::version
#' @description prints the version of the package
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
version <- function() {
  cat("1.1.3")
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
#' @examples example
#'
#'
#'
#' @export
runDictionary <- function(corpusA, dataA, word, dfmA, dataB, dictionaryA) {
  corpusA <- quanteda::corpus(dataA$word)
  dfmA    <- quanteda::dfm(corpusA, dictionary = dictionaryA)
  dataB   <- quanteda::convert(dfmA, to = "data.frame")
  return(dataB)
}









