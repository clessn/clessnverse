######################################################
#' @title clessnverse::createSimple
#' @description
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
createSimple <- function(context) {
  available_contexts <- clessnverse::getAgoraplusAvailableContexts()

  if (missing(context) || !context %in% available_contexts)
    stop(paste("You must provide a context in which to create the agora dataframe",
               "possible values are", paste(available_contexts,collapse=' | ')), call. = F)

  filename <- paste("../clessn-blend/_SharedFolder_clessn-blend/datastructure/agoraplus-",context,"/simple-datatypes.csv", sep='')
  classes <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE, colClasses = rep("character"), nrows = 1)
  classes <- unlist(classes[1,])
  filename <- paste("../clessn-blend/_SharedFolder_clessn-blend/datastructure/agoraplus-",context,"/simple-colnames.csv", sep='')
  dataset <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE, colClasses = classes, nrows=1)
  return(dataset)
}


######################################################
#' @title clessnverse::createDeep
#' @description
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
createDeep <- function(context) {
  available_contexts <- clessnverse::getAgoraplusAvailableContexts()

  if (missing(context) || !context %in% available_contexts)
    stop(paste("You must provide a context in which to create the agora dataframe",
               "possible values are", paste(available_contexts,collapse=' | ')), call. = F)

  filename <- paste("../clessn-blend/_SharedFolder_clessn-blend/datastructure/agoraplus-",context,"/deep-datatypes.csv", sep='')
  classes <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE, colClasses = rep("character"), nrows = 1)
  classes <- unlist(classes[1,])
  filename <- paste("../clessn-blend/_SharedFolder_clessn-blend/datastructure/agoraplus-",context,"/deep-colnames.csv", sep='')
  dataset <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE, colClasses = classes, nrows = 1)
  return(dataset)
}


######################################################
#' @title clessnverse::createCache
#' @description
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
createCache <- function(context) {
  available_contexts <- clessnverse::getAgoraplusAvailableContexts()

  if (missing(context) || !context %in% available_contexts)
    stop(paste("You must provide a context in which to create the agora dataframe",
               "possible values are", paste(available_contexts,collapse=' | ')), call. = F)

  filename <- paste("../clessn-blend/_SharedFolder_clessn-blend/datastructure/agoraplus-",context,"/cache-datatypes.csv", sep='')
  classes <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE, colClasses = rep("character"), nrows = 1)
  classes <- unlist(classes[1,])
  filename <- paste("../clessn-blend/_SharedFolder_clessn-blend/datastructure/agoraplus-",context,"/cache-colnames.csv", sep='')
  dataset <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE, colClasses = classes, nrows = 1)
  return(dataset)
}


######################################################
#' @title clessnverse::getSimpleFromHub
#' @description
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
loadSimpleFromHub <- function(context) {
  available_contexts <- clessnverse::getAgoraplusAvailableContexts()

  if (missing(context) || !context %in% available_contexts)
    stop(paste("You must provide a context in which to create the agora dataframe",
               "possible values are", paste(available_contexts,collapse=' | ')), call. = F)

  if (context == "quebec") return(clessnhub::download_table('agoraplus_warehouse_event_items'))
  if (context == "europe") return(clessnhub::download_table('agoraplus-eu_warehouse_event_items'))
  if (context == "canada") stop("context canada is unsupported yet", call. = F)
  stop("context is unsupported yet", call. = F)
}


######################################################
#' @title clessnverse::getDeepFromHub
#' @description
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
loadDeepFromHub <- function(context) {
  available_contexts <- clessnverse::getAgoraplusAvailableContexts()

  if (missing(context) || !context %in% available_contexts)
    stop(paste("You must provide a context in which to create the agora dataframe",
               "possible values are", paste(available_contexts,collapse=' | ')), call. = F)

  if (context == "quebec") return(clessnhub::download_table('agoraplus_warehouse_intervention_items'))
  if (context == "europe") return(clessnhub::download_table('agoraplus-eu_warehouse_intervention_items'))
  if (context == "canada") stop("context canada is unsupported yet", call. = F)
  stop("context is unsupported yet", call. = F)
}


######################################################
#' @title clessnverse::getCacheFromHub
#' @description
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
loadCacheFromHub <- function(context) {
  available_contexts <- clessnverse::getAgoraplusAvailableContexts()

  if (missing(context) || !context %in% available_contexts)
    stop(paste("You must provide a context in which to create the agora dataframe",
               "possible values are", paste(available_contexts,collapse=' | ')), call. = F)


  if (context == "quebec") return(clessnhub::download_table('agoraplus_warehouse_cache_items'))
  if (context == "europe") return(clessnhub::download_table('agoraplus-eu_warehouse_cache_items'))
  if (context == "canada") stop("context canada is unsupported yet", call. = F)
  stop("context is unsupported yet", call. = F)
}

