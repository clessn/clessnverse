######################################################
#' @title clessnverse::createAgoraplusInterventionsDf
#' @description
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
createAgoraplusInterventionsDf <- function(type, location, schema) {

  clessnverse::checkLocationSchemaType(type, location, schema)

  filename <- paste("../clessn-blend/_SharedFolder_clessn-blend/datastructure/agoraplus-",type,"-",schema,".json", sep='')
  list <- jsonlite::fromJSON(filename)
  dataset <- as.data.frame(list)[-c(1),]
  return(dataset)
}


######################################################
#' @title clessnverse::createAgoraplusPersonsDf
#' @description
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
createAgoraplusPersonsDf <- function(type, schema) {

  clessnverse::checkLocationSchemaType(type, "CA", schema)

  filename <- paste("../clessn-blend/_SharedFolder_clessn-blend/datastructure/agoraplus-",type,"-",schema,".json", sep='')
  list <- jsonlite::fromJSON(filename)
  dataset <- as.data.frame(list)[-c(1),]
  return(dataset)
}


######################################################
#' @title clessnverse::createAgoraplusCacheDf
#' @description
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
createAgoraplusCacheDf <- function(type, location, schema) {

  clessnverse::checkLocationSchemaType(type, location, schema)

  filename <- paste("../clessn-blend/_SharedFolder_clessn-blend/datastructure/agoraplus-cache-",schema,".json", sep='')
  list <- jsonlite::fromJSON(filename)
  dataset <- as.data.frame(list)[-c(1),]
  return(dataset)
}


######################################################
#' @title clessnverse::loadHubAgoraplusInterventionsDf
#' @description
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
loadAgoraplusInterventionsDf <- function(type, schema, location, download_data=F, token="") {

  # Check the compatibility of metadata and stop if there is an error
  clessnverse::checkLocationSchemaType(type, location, schema)

  clessnverse::logit(message = "getting Interventions from HUB", logger = logger)

  # Connect to the hub with token - if token doesnt exist check if hub_config exists and if not ask for login
  if (token != "" ) {
    clessnverse::logit(message = "connecting to hub with token", logger = logger)
    clessnhub::connect_with_token(token = token)
  } else {
    if ( exists("hub_config") && !is.null(hub_config$token) && hub_config$token != "") {
      clessnverse::logit(message = "refreshing token", logger = logger)
      clessnhub::refresh_token(hub_config$token, hub_config$url)
    }
    else {
      clessnverse::logit(message = "connecting to hub", logger = logger)
      clessnhub::connect()
    }
  }

  # Filter based on the type of data, the location of event/interventions and schema
  filter <- clessnhub::create_filter(type = type, schema = schema, metadata = list("location"=location))
  clessnverse::logit(message = paste("filtering data with", paste(filter, collapse = ' '), sep = ' '), logger = logger)

  # Get the data
  dataset <- clessnhub::get_items(table = 'agoraplus_interventions', filter = filter, download_data = download_data)

  return(dataset)
}

######################################################
#' @title clessnverse::loadHubAgoraplusPersonsDf
#' @description
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
loadAgoraplusPersonsDf <- function(type, schema, location, overwrite=F , download_data=F, token="") {

  # Check the compatibility of metadata and stop if there is an error
  clessnverse::checkLocationSchemaType(type, location, schema)

  clessnverse::logit(message = "getting Persons from HUB", logger = logger)

  # Connect to the hub with token - if token doesnt exist check if hub_config exists and if not ask for login
  if (token != "" ) {
    clessnverse::logit(message = "connecting to hub with token", logger = logger)
    clessnhub::connect_with_token(token = token)
  } else {
    if ( exists("hub_config") && !is.null(hub_config$token) && hub_config$token != "") {
      clessnverse::logit(message = "refreshing token", logger = logger)
      clessnhub::refresh_token(hub_config$token, hub_config$url)
    }
    else {
      clessnverse::logit(message = "connecting to hub", logger = logger)
      clessnhub::connect()
    }
  }

  # Filter based on the type of data, the location of persons and schema
  if (location == "all") {
    filter <- clessnhub::create_filter(type = type, schema = schema)
  } else {
    filter <- clessnhub::create_filter(type = type, schema = schema, metadata = list("location"=location))
  }

  clessnverse::logit(message = paste("filtering data with", paste(filter, collapse = ' '), sep = ' '), logger = logger)

  # Get the data
  dataset <- clessnhub::get_items(table = 'persons', filter = filter, download_data = download_data)

  return(dataset)
}

######################################################
#' @title clessnverse::loadHubAgoraplusCacheDf
#' @description
#' @param
#' @return
#' @examples example
#'
#'
#'
#' @export
loadAgoraplusCacheDf <- function(type, schema, location, download_data=F, token="") {

  # Check the compatibility of metadata and stop if there is an error
  clessnverse::checkLocationSchemaType(type, location, schema)

    clessnverse::logit(message = "getting Cache from HUB", logger = logger)

    # Connect to the hub with token - if token doesnt exist check if hub_config exists and if not ask for login
    if (token != "" ) {
      clessnverse::logit(message = "connecting to hub with token", logger = logger)
      clessnhub::connect_with_token(token = token)
    } else {
      if ( exists("hub_config") && !is.null(hub_config$token) && hub_config$token != "") {
        clessnverse::logit(message = "refreshing token", logger = logger)
        clessnhub::refresh_token(hub_config$token, hub_config$url)
      }
      else {
        clessnverse::logit(message = "connecting to hub", logger = logger)
        clessnhub::connect()
      }
    }

    # Filter based on the type of data, the location of persons and schema
    filter <- clessnhub::create_filter(type = type, schema = schema, metadata = list("location"=location))
    clessnverse::logit(message = paste("filtering data with", paste(filter, collapse = ' '), sep = ' '), logger = logger)

    # Get the data
    dataset <- clessnhub::get_items(table = 'agoraplus_cache', filter = filter, download_data = download_data)
    return(dataset)
}


######################################################
######################################################
######################################################
######################################################
######################################################
#                    V1 Functions                    #
######################################################
######################################################
######################################################
######################################################
######################################################

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

  if (context == "quebec") return(clessnhub::v1_download_table('agoraplus_warehouse_event_items'))
  if (context == "europe") return(clessnhub::v1_download_table('agoraplus-eu_warehouse_event_items'))
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

  if (context == "quebec") return(clessnhub::v1_download_table('agoraplus_warehouse_intervention_items'))
  if (context == "europe") return(clessnhub::v1_download_table('agoraplus-eu_warehouse_intervention_items'))
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


  if (context == "quebec") return(clessnhub::v1_download_table('agoraplus_warehouse_cache_items'))
  if (context == "europe") return(clessnhub::v1_download_table('agoraplus-eu_warehouse_cache_items'))
  if (context == "canada") stop("context canada is unsupported yet", call. = F)
  stop("context is unsupported yet", call. = F)
}


######################################################
#' @title clessnverse::loadAgoraplusHUBDatasets
#' @description Get all data from the HUB.
# If neither was successful, then create empty datasets from scratch
#
# - Cache which contains the raw html scraped from the assnat.qc.ca site
# - dfSimple which contains one observation per event (débat or press conf)
# - dfDeep which contains one observation per intervention per event
# - journalists : a reference dataframe that contains the journalists in order
#                 to add relevant data on journalists in the interventions
#                 dataset
# - deputes     : a reference dataframe that contains the deputes in order
#                 to add relevant data on journalists in the interventions
#                 dataset
#' @param context currently "quebec" | "canada" | "europe" are supported
#' @param opt see clessnverse::processCommandLineOtions
#' @param username your HUB username
#' @param password your HUB password
#' @param the url of the hub : "https://dev-clessn.apps.valeria.science" or "https://clessn.apps.valeria.science"
#' @return
#' @examples example
#'
#'
#'
#' @export
loadAgoraplusHUBDatasets <- function(context, opt, username, password, url) {

  # Connect to the HUB
  clessnverse::logit(message = paste("login to the HUB", url), logger = logger)
  clessnhub::v1_login(username = username, password = password, url = url)

  # Récuperer les données de Cache, Simple et Deep
  if (opt$cache_mode != "rebuild" && opt$cache_mode != "skip" &&
      (!exists("dfCache") || is.null(dfCache) || nrow(dfCache) == 0) ||
      opt$hub_mode == "refresh") {

    clessnverse::logit(message = "getting cache from HUB", logger = logger)
    dfCache <<- clessnverse::loadCacheFromHub(context)
  } else {
    clessnverse::logit(message = paste("not retrieving dfCache from HUB because update mode is",
                             opt$cache_mode,
                             dplyr::case_when(exists("dfCache") && !is.null(dfCache) ~ "and it aleady exists",
                                       TRUE ~ "")),
                       logger = logger)
  }

clessnverse::logit(message = "refreshing token", logger = logger)
clessnhub::refresh_token(configuration$token, configuration$url)

  if (opt$simple_mode != "rebuild" && opt$simple_mode != "skip" &&
      (!exists("dfSimple") || is.null(dfSimple) || nrow(dfSimple) == 0) ||
      opt$hub_mode == "refresh") {

    clessnverse::logit(message = "getting simple from HUB", logger = logger)
    dfSimple <<- clessnverse::loadSimpleFromHub(context)
  } else {
    clessnverse::logit(message = paste("not retrieving dfSimple from HUB because update mode is",
                             opt$simple_mode,
                             dplyr::case_when(exists("dfSimple") && !is.null(dfSimple) ~ "and it aleady exists",
                             TRUE ~ "")),
                       logger = logger)
  }

clessnverse::logit(message = "refreshing token", logger = logger)
clessnhub::refresh_token(configuration$token, configuration$url)


  if (opt$deep_mode != "rebuild" && opt$deep_mode != "skip" &&
      (!exists("dfDeep") || is.null(dfDeep) || nrow(dfDeep) == 0) ||
      opt$hub_mode == "refresh") {

    clessnverse::logit(message = "getting deep from HUB", logger = logger)
    dfDeep <<- clessnverse::loadDeepFromHub(context)
  } else {
    clessnverse::logit(message = paste("not retrieving dfDeep from HUB because update mode is",
                             opt$deep_mode,
                             dplyr::case_when(exists("dfDeep") && !is.null(dfDeep) ~ "and it aleady exists",
                             TRUE ~ "")),
                       logger = logger)
  }

  # We only do this if we want to rebuild those datasets from scratch to start fresh
  # or if then don't exist in the environment of the current R session
  if ( !exists("dfCache") || is.null(dfCache) || opt$cache_mode == "rebuild" ) {
    clessnverse::logit(message = "creating dfCache either because it doesn't exist or because its rebuild option", logger = logger)
    dfCache <<- clessnverse::createCache(context = context)
  } else {
    clessnverse::logit(message = "not creating dfCache either because it already exists", logger = logger)
  }

  if ( !exists("dfSimple") || is.null(dfSimple) || opt$simple_mode == "rebuild" ) {
    clessnverse::logit(message = "creating dfSimple either because it doesn't exist or because its rebuild option", logger = logger)
    dfSimple <<- clessnverse::createSimple(context = context)
  } else {
    clessnverse::logit(message = "not creating dfSimple either because it already exists", logger = logger)
  }

  if ( !exists("dfDeep") || is.null(dfDeep) || opt$deep_mode == "rebuild" ) {
    clessnverse::logit(message = "creating dfDeep either because it doesn't exist or because its rebuild option", logger = logger)
    dfDeep <<- clessnverse::createDeep(context = context)
  } else {
    clessnverse::logit(message = "not creating dfDeep either because it already exists", logger = logger)
  }
}
