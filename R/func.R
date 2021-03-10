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
  version <- "1.1.7"
  clessnverse::logit(version, logger)
  return(version)
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
loginit <- function(script,backend,logpath=".") {
  available_backends <- clessnverse::getAgoraplusAvailableLogBackends()

  if (missing(backend) || !backend %in% available_backends)
    stop(paste("You must provide a backend in which to store the logs",
               "possible values are", paste(available_backends,collapse=' | ')), call. = F)

  if (backend == "file") return(file(paste(logpath, "/",script,".log",sep=""), open = "at"))
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
logit <- function(message, logger = NULL) {
  tryCatch(
    {
      if (getConnection(logger)) {
        cat(format(Sys.time(), "%Y-%m-%d %X"), ":", message, "\n", append = T, file = logger)
      }
    },
    error = function(e) {
      cat("invalid logger - using console instead : ",message,"\n")
    }
  )
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
  tryCatch(
    {
      if (getConnection(logger)) {
        close(logger)
      }
    },
    error = function(e) {
    }
  )
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









