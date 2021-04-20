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
  version <- "1.3.1"
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
  if (logpath=="") logpath <- "."

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
      cat("activity log : ",message,"\n")
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
#' @description Parse the command line options of the agora+ scrapers
# Which are the update modes of each database in the HUB or in the CSV backend
#
# Possible values : update, refresh, rebuild or skip
# - update : updates the dataset by adding only new observations to it
# - refresh : refreshes existing observations and adds new observations to the dataset
# - rebuild : wipes out completely the dataset and rebuilds it from scratch
# - skip : does not make any change to the dataset
# set which backend we're working with
# - CSV : work with the CSV in the shared folders - good for testing
#         or for datamining and research or messing around
# - HUB : work with the CLESSNHUB data directly : this is prod data
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
processCommandLineOptions <- function() {
  option_list = list(
    make_option(c("-c", "--cache_update"), type="character", default="rebuild",
                help="update mode of the cache [default= %default]", metavar="character"),
    make_option(c("-s", "--simple_update"), type="character", default="rebuild",
                help="update mode of the simple dataframe [default= %default]", metavar="character"),
    make_option(c("-d", "--deep_update"), type="character", default="rebuild",
                help="update mode of the deep dataframe [default= %Adefault]", metavar="character"),
    make_option(c("-h", "--hub_update"), type="character", default="skip",
                help="update mode of the hub [default= %default]", metavar="character"),
    make_option(c("-f", "--csv_update"), type="character", default="skip",
                help="update mode of the simple dataframe [default= %default]", metavar="character"),
    make_option(c("-b", "--backend_type"), type="character", default="HUB",
                help="type of the backend - either hub or csv [default= %default]", metavar="character")
  )

  opt_parser = OptionParser(option_list=option_list)
  opt = parse_args(opt_parser)

  # Process incompatible option sets
  if ( opt$hub_update == "refresh" &&
       (opt$simple_update == "rebuild" || opt$deep_update == "rebuild" ||
        opt$simple_update == "skip" || opt$deep_update == "skip") )
    stop(paste("this set of options:",
               paste("--hub_update=", opt$hub_update, " --simple_update=", opt$simple_update, " --deep_update=", opt$deep_update, sep=''),
               "will duplicate entries in the HUB, if you want to refresh the hub use refresh on all datasets"), call. = F)

  clessnverse::logit(paste("command line options: ",
                           paste(c(rbind(paste(" ",names(opt),"=",sep=''),opt)), collapse='')), logger)

  return(opt)
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









