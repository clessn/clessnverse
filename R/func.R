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
  version <- "1.4.6"
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

  if (missing(backend)) stop(paste("You must provide a backend in which to store the logs",
                                   "possible values are", paste(available_backends,collapse=' | ')), call. = F)

    if(length(backend) == 1 && grepl(",", backend)) {
      backend <- trimws(strsplit(backend, ",")[[1]])
    } else {
      if (length(backend) == 1) {
        # nothing to do we only have one string without csv
      } else {
        if (length(backend) > 1) {
          # nothing to do we already have a list
        }
      }
    }

  file_logger <- NULL
  hub_logger <- NULL
  console_logger <- NULL

  if ("file" %in% backend) file_logger <- file(paste(logpath, "/",script,".log",sep=""), open = "at")
  if ("hub" %in% backend) hub_logger <- "hub_log"
  if ("console" %in% backend) console_logger <- "console"

  backend_list <- list(file_logger, hub_logger, console_logger)
  backend_list[sapply(backend_list, is.null)] <- NULL

  if (!is.null(file_logger) || !is.null(hub_logger) || !is.null(console_logger)) return(backend_list)

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

      if ("console" %in% logger) {
        cat(format(Sys.time(), "%Y-%m-%d %X"), scriptname, "-", paste(message, collapse = " "), "\n")
      }

      if (is.numeric(logger[[1]][1])) {
        if (getConnection(logger[[1]][1])) cat(format(Sys.time(), "%Y-%m-%d %X"), scriptname, ":", paste(message, collapse = " "), "\n", append = T, file = logger[[1]][1])
      }

      if ("hub_log" %in% logger) {
        clessnhub::logToHub(scriptname, data=message, metadata = format(Sys.time(), "%Y-%m-%d %X"))
      }
    },
    error = function(e) {
      cat("console log: ",format(Sys.time(), "%Y-%m-%d %X"), "- Error logging", e[[1]], "-", paste(message, collapse = " "), "\n")
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
      if (is.numeric(logger[[1]][1])){
        if (getConnection(logger[[1]])) close(logger[[1]][1])
      }
    },
    error = function(e) {
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
  return(c("v1", "v2"))
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
  return(c("parliament_debate" ,"parliament_debate_archive", "press_conference", "mp", "public_service", "journalist"))
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
  return(c("hub" ,"file", "console"))
}


#####################################################
#' @title clessnverse::checkTypeSchema
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
checkTypeSchema <- function(type, schema) {
  available_schemas <- clessnverse::getAgoraplusAvailableSchemas()
  available_types <- clessnverse::getAgoraplusAvailableTypes()

  if (missing(type) || !type %in% available_types)
    stop(paste("You must provide a type for which to create/load the agora dataframe. ",
               "Possible values are", paste(available_types,collapse=' | ')), call. = F)

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
    optparse::make_option(c("-p", "--deep_mode"), type="character", default="rebuild",
                          help="update mode of the deep dataframe [default= %Adefault]", metavar="character"),
    optparse::make_option(c("-d", "--dataframe_mode"), type="character", default="rebuild",
                          help="update mode of the dataframe [default= %Adefault]", metavar="character"),
    optparse::make_option(c("-b", "--hub_mode"), type="character", default="skip",
                          help="update mode of the hub [default= %default]", metavar="character"),
    optparse::make_option(c("-l", "--download_data"), type="logical", default=TRUE,
                          help="download data from the hub [default= %default]", metavar="logical"),
    optparse::make_option(c("-t", "--translate"), type="logical", default=FALSE,
                          help="translate text using paid APIs [default= %default]", metavar="logical"),
    optparse::make_option(c("-o", "--log_output"), type="character", default="file,console",
                          help="where to output the logs [default= %default]", metavar="character")
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

  if (!exists("logger")) logger <- "console"

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



