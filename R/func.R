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
  version <- "1.4.4"
  clessnverse::logit(message = version, logger = logger)
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
loginit <- function(script, backend, logpath=".") {
  available_backends <- clessnverse::getAgoraplusAvailableLogBackends()
  if (logpath=="") logpath <- "."

  if (missing(backend) || FALSE %in% (backend %in% available_backends)) stop(paste("You must provide a backend in which to store the logs",
                                                                       "possible values are", paste(available_backends,collapse=' | ')), call. = F)

  file_logger <- NULL
  hub_logger <- NULL

  if ("file" %in% backend) file_logger <- file(paste(logpath, "/",script,".log",sep=""), open = "at")

  if ("hub" %in% backend) hub_logger <- "hub_log"

  if (!is.null(file_logger) || !is.null(hub_logger)) return(list(file_logger, hub_logger))

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
logit <- function(scriptname="clessnverse", message = "", logger = NULL) {
  tryCatch(
    {
      if ("hub_log" %in% logger) {
        clessnhub::logToHub(scriptname, data=message, metadata = format(Sys.time(), "%Y-%m-%d %X"))
      }

      if (getConnection(logger[[1]])) {
        cat(format(Sys.time(), "%Y-%m-%d %X"), ":", message, "\n", append = T, file = logger[[1]])
      }
    },
    error = function(e) {
      cat("console log: ",format(Sys.time(), "%Y-%m-%d %X"), "-", message)
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
      if (getConnection(logger[[1]])) {
        close(logger[[1]])
        logger <<- NULL
      }
    },
    error = function(e) {
      logger <<- NULL
      cat("")
    }
  )
}


######################################################
#' @title clessnverse::getAgoraplusAvailableLocations
#' @description returns the available contexts implemented in agoraplus in a vector of strings
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
getAgoraplusAvailableLocations <- function() {
  return(c("CA-QC" ,"EU", "CA"))
}


######################################################
#' @title clessnverse::getAgoraplusAvailableSchemas
#' @description returns the available dats strcture
#'              schema versions implemented in agoraplus
#'              in a vector of strings
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
getAgoraplusAvailableSchemas <- function() {
  return(c("v2"))
}


######################################################
#' @title clessnverse::getAgoraplusAvailableTypes
#' @description returns the available dats strcture
#'              schema versions implemented in agoraplus
#'              in a vector of strings
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
getAgoraplusAvailableTypes <- function() {
  return(c("parliament_debate" ,"press_conference", "mp", "public_service", "journalist"))
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


#####################################################
#' @title clessnverse::checkLocationchemaType
#' @description
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
checkLocationSchemaType <- function(type, location, schema) {
  available_locations <- clessnverse::getAgoraplusAvailableLocations()
  available_schemas <- clessnverse::getAgoraplusAvailableSchemas()
  available_types <- clessnverse::getAgoraplusAvailableTypes()

  if (missing(type) || !type %in% available_types)
    stop(paste("You must provide a type for which to create/load the agora dataframe. ",
               "Possible values are", paste(available_types,collapse=' | ')), call. = F)

  if (missing(location) || !location %in% available_locations)
    stop(paste("You must provide a location for which to create/load the agora dataframe. ",
               "Possible values are", paste(available_locations,collapse=' | ')), call. = F)

  if (missing(schema) || !schema %in% available_schemas)
    stop(paste("You must provide the schema version of the agora dataframe. ",
               "Possible values are", paste(available_schemas,collapse=' | ')), call. = F)
}



######################################################
#' @title clessnverse::processCommandLineOptions
#' @description Parse the command line options of the agora+ scrapers
# Which are the update modes of each database in the HUB
#
# Possible values : update, refresh, rebuild or skip
# - update : updates the dataset by adding only new observations to it
# - refresh : refreshes existing observations and adds new observations to the dataset
# - rebuild : wipes out completely the dataset and rebuilds it from scratch
# - skip : does not make any change to the dataset
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#' @return
#' @examples
#'
#'
#'
#' @export
processCommandLineOptions <- function() {
  option_list = list(
    optparse::make_option(c("-c", "--cache_mode"), type="character", default="rebuild",
                          help="update mode of the cache [default= %default]", metavar="character"),
    optparse::make_option(c("-s", "--simple_mode"), type="character", default="rebuild",
                          help="update mode of the simple dataframe [default= %default]", metavar="character"),
    optparse::make_option(c("-d", "--deep_mode"), type="character", default="rebuild",
                          help="update mode of the deep dataframe [default= %Adefault]", metavar="character"),
    optparse::make_option(c("-l", "--dataframe_mode"), type="character", default="rebuild",
                          help="update mode of the dataframe [default= %Adefault]", metavar="character"),
    optparse::make_option(c("-h", "--hub_mode"), type="character", default="skip",
                          help="update mode of the hub [default= %default]", metavar="character"),
    optparse::make_option(c("-t", "--download_data"), type="logical", default=TRUE,
                          help="download data from the hub [default= %default]", metavar="logical")
  )

  opt_parser = optparse::OptionParser(option_list=option_list)
  opt = optparse::parse_args(opt_parser)

  # Process incompatible option sets
  if ( opt$hub_mode == "refresh" &&
       (opt$simple_mode == "rebuild" || opt$deep_mode == "rebuild" ||  opt$dataframe_mode == "rebuild" ||
        opt$simple_mode == "skip" || opt$deep_mode == "skip" || opt$dataframe_mode == "skip") )
    stop(paste("this set of options:",
               paste("--hub_mode=", opt$hub_mode, " --simple_mode=", opt$simple_mode, " --deep_mode=", opt$deep_mode, " --dataframe_mode=", opt$dataframe_mode, sep=''),
               "will duplicate entries in the HUB, if you want to refresh the hub use refresh on all datasets"), call. = F)

  clessnverse::logit(message = paste("command line options: ",
                           paste(c(rbind(paste(" ",names(opt),"=",sep=''),opt)), collapse='')), logger = logger)

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


######################################################
######################################################
######################################################
######################################################
######################################################
#                     V1 Functions                   #
######################################################
######################################################
######################################################
######################################################
######################################################


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
  return(c("quebec" ,"europe", "canada"))
}



