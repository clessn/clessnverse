###############################################################################
###############################################################################
###############################################################################
# Module : utils.R
# Description:  This module is used to gather all functions related to
#
#               - Activity logging
#               - Command line options processing
#               - %vcontains%
#



###############################################################################
###############################################################################
###############################################################################
# ACTIVITY LOGGING
#
# Activity logging is about keeping a log of the activity of your code
# The log can be consigned on the screen, into the file on the computer
# which the script runs on, or in hublot directly.
#


######################################################
#' @title clessnverse::log_init
#' @description This function initializes the log
#' @param script blah
#' @param backend blah
#' @param logpath blah
#' @return return
#' @examples # To be documented
#'
#'
#'
#' @export
log_init <- function(script, backend, logpath=".") {

  available_backends <- c("hub" ,"file", "console")

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
#' @description desc
#' @param scriptname blah
#' @param message blah
#' @param logger blah
#' @return blah
#' @examples # To be documented
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
#' @title clessnverse::log_close
#' @description blah
#' @param logger blah
#' @return blah
#' @examples # To be documented
#'
#'
#'
#' @export
log_close <- function(logger) {
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




###############################################################################
###############################################################################
###############################################################################
# COMMAND LINE UTILITIES
#
#   When automating an R Script, it can be useful to pass paramets to it.
#   For instance, the same script could have a different behavior depending on
#   whether it runs automatically in a schedule, or whether you run it manually
#
#   One typical example of that is :
#   - a refiner build a datamart incrementally adding only new items from the
#     data warehouse
#   - but the same refiner could be ran manually to rebuild an entire datamart
#     from scratch with a new metadata or a new way to standardize a particular
#     variable
#
#   The clessnverse package offers therefore a way to parse command line
#   parameters which allows you to take those parameters values into account
#   in your code and program different behaviors of your script depending
#   on parameters values.


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
#' @return blah
#' @examples # To be documented
#'
#'
#'
#' @export
process_command_line_options <- function() {
  option_list = list(
    optparse::make_option(c("-d", "--dataframe_mode"), type="character", default="rebuild",
                          help="update mode of the dataframe [default= %Adefault]", metavar="character"),
    optparse::make_option(c("-h", "--hub_mode"), type="character", default="skip",
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

  #clessnverse::log_activity(message = paste("command line options: ",
  #                         paste(c(rbind(paste(" ",names(opt),"=",sep=''),opt)), collapse='')), logger = logger)

  return(opt)
}







###############################################################################
###############################################################################
###############################################################################
# BATCH CHANGE OF DATA OR METADATA IN LAKE, WAREHOUSE OR MARTS
#
# The functions below are used to change metadata or data of objects in the
# datalake, datawarehouse or the datamarts in batch.  This is useful when
# there was a mistake made in a pipeline metadata variables, or if the
# data governance committee decides of a change in the data management
# standards and cataloging
#


###############################################################################
#' Makes a batch change of the metadata applied to lake objects
#'
#' Batch changes of metadata can be usefule if there are hunderds or  thousands
#' of objects in the data lake which we need to change the metadata on.
#' @param path The path in the data lake which the objects are in
#' @param filter A filter used to select the objects in the data lake which the
#' metadata needs to be changed on.
#' @param new_metadata A list type objects containing the new metadata to be
#' applied on the lake objects.
#' @param mode The mode to apply the metadata with. Can take the following
#' values:
#'   - "overwrite": overwrites the entire existing metadata set with new_metadata
#'   - "merge": merges the new_metadata with the existing metadata
#'   - "add": only adds new variables from new_metadata to the existing metadata set
#' @param credentials A list object containing your Hublot credential.
#' @importFrom rlist list.merge
#' @examples # To be documented
#'
#' \dontrun{
#'  # get credentials from hublot
#'  credentials <- hublot::get_credentials(
#'    Sys.getenv("HUB3_URL"),
#'    Sys.getenv("HUB3_USERNAME"),
#'    Sys.getenv("HUB3_PASSWORD")
#'    )
#'
#'  # filter for selecting the lakes items to be changed
#'  filter <- list(
#'    path = "political_party_press_releases",
#'    metadata__political_party = "CAQ",
#'    metadata__province_or_state="QC",
#'    metadata__country="CAN",
#'    metadata__storage_class="lake"
#'  )
#'  # new metadata
#'
#'  # Change the metadata on the lake items complying with the filter
#'  clessnverse::change_lake_items_metadata(
#'    path = "political_party_press_releases",
#'    filter = list(
#'      metadata__province_or_state="QC",
#'      metadata__country="CAN",
#'      metadata__political_party="QS"
#'    ),
#'    new_metadata = list(
#'      "tags": "elxn-qc2022, vitrine_democratique, polqc",
#'      "format": "html",
#'      "source": "https://pq.org/nouvelles/lettre...",
#'      "country": "CAN",
#'      "description": "Communiqués de presse des partis politiques",
#'      "object_type": "raw_data",
#'      "source_type": "website",
#'      "content_type": "political_party_press_release",
#'      "storage_class": "lake",
#'      "political_party": "PQ",
#'      "province_or_state": "QC"
#'    ),
#'    mode = "merge",
#'    credentials = credentials
#'  )
#' }
#' @export
change_lake_items_metadata <- function(path, filter, new_metadata, mode, credentials) {

  data <- hublot::filter_lake_items(credentials, filter = filter)

  if (length(data$results) == 0) {
    stop("no lake item was retrived with this filter")
  }

  for (i in 1:length(data$results)) {
    row <- data$results[[i]]
    current_metadata <- row$metadata

    if (mode == "overwrite"){
      # nothing to do : the commit_lake_item line below will
      # use new_metadata entirely as the new matadata structure
    }

    if (mode == "merge"){
      # merging existing metadata with new metadata
      # if there are identical variable names, then
      # new_metadata wins
      replacement_metadata <- rlist::list.merge(current_metadata, new_metadata)
    }

    if (mode == "add"){
      # adding new metadata only to existing metadata
      # if there are identical variable names, then
      # existing metadata wins and generate warnings
      replacement_metadata <- rlist::list.merge(new_metadata, current_metadata)
    }

    clessnverse::commit_lake_item(data = list(
                                           key = row$key,
                                           path = row$path,
                                           file = row$file
                                         ),
                                  metadata = replacement_metadata,
                                  mode = "refresh",
                                  credentials = credentials
                                  )
  } #for (i in 1:length(data$results))

}









###############################################################################
###############################################################################
###############################################################################
# VARIOUS TOOLS
#



###############################################################################
#' @title %vcontains
#' @description check if a vector 'vector' contains all values specified in the
#'              vector 'values'
#' @param vector : A vector containing all possible values
#' @param values : each individual value, in the form of a vector, to check the
#'                 presence of in 'vector'
#' @return - TRUE if all the values in the vector 'values' are contained in the
#'           vector 'vector'
#'         = FALSE if all the values in 'values' are not contained in 'vector'
#' @examples # To be documented
#'
#' @export
"%vcontains%" <- function(vector, values) {
  tx <- table(values)
  tv <- table(vector)
  z <- tv[names(tx)] - tx
  all(z >= 0 & !is.na(z))
}




###############################################################################
#' @title spread_list_to_df
#' @description converts uneven nested lists to a dataframe
#' @param l : A list object can be a list of lists (nested lists) that can have
#' uneven geometries
#' @return - TRUE if all the values in the vector 'values' are contained in the
#'           vector 'vector'
#'         = FALSE if all the values in 'values' are not contained in 'vector'
#' @importFrom foreach %do%
#' @examples # To be documented
#'
#' @export
spread_list_to_df <- function(l) {

  element <- list()

  l <- l[lapply(l, class) == "list"]

  df <- foreach::foreach(element = l, .combine = bind_rows, .errorhandling = 'remove') %do% {
   df = unlist(element);
   df = as.data.frame(t(df));
   rm(element);
   return(df)
  }

  rm(l)
  return(df)
}


