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
print("1.1.3")
}


######################################################
#' @title clessnverse::setenv
#' @description sets the env variables required for automation
#'              SCRIPT_FILENAME : the name of the script running
#'              LOG_FILENAME : the name of the logfile where the execution activity will be logged
#' @examples example
#'
#'
#'
#' @export
setenv <- function(scriptname = "UnknownScript") {

}

######################################################
#' @title clessnverse::log
#' @description logs a message into the clessnverse syslog for automation activity monitoring & debug purposes
#' @param message  : the message to be logged in the file
#' @param filename : this is the filename the message is going to be logged into
#'
#' @return
#' @examples example
#'
#'
#'
#' @export
log <- function(message) {

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









