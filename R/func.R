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
  return("1.1.5")
}


######################################################
#' @title clessnverse::loginit
#' @description
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
loginit <- function(script,backend) {
  available_backends <- clessnverse::getAgoraplusAvailableLogBackends()

  if (missing(backend) || !backend %in% available_backends)
    stop(paste("You must provide a backend in which to store the logs",
               "possible values are", paste(available_backends,collapse=' | ')), call. = F)

  if (backend == "file") return(file(paste("./log/",script,".log",sep=""), open = "at"))
  if (backend == "hub") stop("not yet implemented", call. = F)
  stop("Log backend not supported", call. = F)
}

######################################################
#' @title clessnverse::logit
#' @description
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
logit <- function(logger, message) {

  if (!is.null(logger))
    cat(format(Sys.time(), "%Y-%m-%d %X"), ":", message, "\n",
               append = T, file = logger)

}

######################################################
#' @title clessnverse::logclose
#' @description
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
logclose <- function(logger) {
  if (!is.null(logger)) close(logger)
  return(NULL)
}

######################################################
#' @title clessnverse::getAgoraplusAvailableContexts
#' @description returns the available contexts implemented in agoraplus in a vector of strings
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
getAgoraplusAvailableContexts <- function() {
  return(c("quebec" ,"canada", "europe"))
}

######################################################
#' @title clessnverse::getAgoraplusAvailableLogBackends
#' @description returns the available log backends
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
getAgoraplusAvailableLogBackends <- function() {
  return(c("hub" ,"file"))
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









