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


######################################################
#' @title clessnverse::loadAgoraplusHUBDatasets
#' @description Get all data from the HUB or from CSV.
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
  clessnverse::logit("getting data from HUB", logger)

  # Connect to the HUB
  clessnverse::logit(paste("login to the HUB", url), logger)
  clessnhub::login(username = username, password = password, url = url)

  # Récuperer les données de Cache, Simple et Deep
  if (opt$cache_update != "rebuild" && opt$cache_update != "skip" &&
      (!exists("dfCache") || is.null(dfCache) || nrow(dfCache) == 0) ||
      opt$hub_update == "refresh") {

    clessnverse::logit("getting cache from HUB", logger)
    dfCache <<- clessnverse::loadCacheFromHub(context)
  } else {
    clessnverse::logit(paste("not retrieving dfCache from HUB because update mode is",
                             opt$cache_update,
                             case_when(exists("dfCache") && !is.null(dfCache) ~ "and it aleady exists",
                                       TRUE ~ "")),
                       logger)
  }

  if (opt$simple_update != "rebuild" && opt$simple_update != "skip" &&
      (!exists("dfSimple") || is.null(dfSimple) || nrow(dfSimple) == 0) ||
      opt$hub_update == "refresh") {

    clessnverse::logit("getting simple from HUB", logger)
    dfSimple <<- clessnverse::loadSimpleFromHub(context)
  } else {
    clessnverse::logit(paste("not retrieving dfSimple from HUB because update mode is",
                             opt$simple_update,
                             case_when(exists("dfSimple") && !is.null(dfSimple) ~ "and it aleady exists",
                             TRUE ~ "")),
                       logger)
  }

  if (opt$deep_update != "rebuild" && opt$deep_update != "skip" &&
      (!exists("dfDeep") || is.null(dfDeep) || nrow(dfDeep) == 0) ||
      opt$hub_update == "refresh") {

    clessnverse::logit("getting deep from HUB", logger)
    dfDeep <<- clessnverse::loadDeepFromHub(context)
  } else {
    clessnverse::logit(paste("not retrieving dfDeep from HUB because update mode is",
                             opt$deep_update,
                             case_when(exists("dfDeep") && !is.null(dfDeep) ~ "and it aleady exists",
                             TRUE ~ "")),
                       logger)
  }

  # We only do this if we want to rebuild those datasets from scratch to start fresh
  # or if then don't exist in the environment of the current R session
  if ( !exists("dfCache") || is.null(dfCache) || opt$cache_update == "rebuild" ) {
    clessnverse::logit("creating cache either because it doesn't exist or because its rebuild option", logger)
    dfCache <<- clessnverse::createCache(context = "quebec")
  }

  if ( !exists("dfSimple") || is.null(dfSimple) || opt$simple_update == "rebuild" ) {
    clessnverse::logit("creating Simple either because it doesn't exist or because its rebuild option", logger)
    dfSimple <<- clessnverse::createSimple(context = "quebec")
  }

  if ( !exists("dfDeep") || is.null(dfDeep) || opt$deep_update == "rebuild" ) {
    clessnverse::logit("creating Deep either because it doesn't exist or because its rebuild option", logger)
    dfDeep <<- clessnverse::createDeep(context = "quebec")
  }

  clessnverse::logit("getting deputes from HUB", logger)
  deputes <<- clessnhub::download_table('warehouse_quebec_mnas')
  deputes <<- deputes %>% tidyr::separate(lastName, c("lastName1", "lastName2"), " ")

  clessnverse::logit("getting journalists from HUB", logger)
  journalists <<- clessnhub::download_table('warehouse_journalists')

}


######################################################
#' @title clessnverse::loadAgoraplusCSVDatasets
#' @description Get all data from CSV files.
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
#' @param path the path that contains the CSV
#' @return
#' @examples example
#'
#'
#'
#' @export
loadAgoraplusCSVDatasets <- function(context, opt, path) {
  clessnverse::logit("getting data from CSV", logger)

  base_csv_folder <<- path

  if (opt$cache_update != "rebuild" && opt$cache_update != "skip") {
    clessnverse::logit("getting Cache from CSV", logger)
    dfCache <<- read.csv2(file = paste(base_csv_folder,"dfCacheAgoraPlus.csv",sep=''),
                         sep = ";", comment.char = "#")
  }

  if (opt$simple_update != "rebuild" && opt$simple_update != "skip") {
    clessnverse::logit("getting Simple from CSV", logger)
    dfSimple <<- read.csv2(file= paste(base_csv_folder,"dfSimpleAgoraPlus.csv",sep=''),
                          sep = ";", comment.char = "#", encoding = "UTF-8")
  }

  if (opt$deep_update != "rebuild" && opt$deep_update != "skip") {
    clessnverse::logit("getting Deep from CSV", logger)
    dfDeep <<- read.csv2(file=paste(base_csv_folder,"dfDeepAgoraPlus.csv",sep=''),
                        sep = ";", comment.char = "#", encoding = "UTF-8")
  }

  # We only do this if we want to rebuild those datasets from scratch to start fresh
  # or if then don't exist in the environment of the current R session
  if ( !exists("dfCache") || is.null(dfCache) || opt$cache_update == "rebuild" ) {
    clessnverse::logit("creating cache either because it doesn't exist or because its rebuild option", logger)
    dfCache <<- clessnverse::createCache(context = "quebec")
  }

  if ( !exists("dfSimple") || is.null(dfSimple) || opt$simple_update == "rebuild" ) {
    clessnverse::logit("creating Simple either because it doesn't exist or because its rebuild option", logger)
    dfSimple <<- clessnverse::createSimple(context = "quebec")
  }

  if ( !exists("dfDeep") || is.null(dfDeep) || opt$deep_update == "rebuild" ) {
    clessnverse::logit("creating Deep either because it doesn't exist or because its rebuild option", logger)
    dfDeep <<- clessnverse::createDeep(context = "quebec")
  }

  clessnverse::logit("getting deputes from CSV", logger)
  deputes <- read.csv(file = "../clessn-blend/_SharedFolder_clessn-blend/data/Deputes_Quebec_Coordonnees.csv", sep=";")
  deputes <- deputes %>% tidyr::separate(nom, c("firstName", "lastName1", "lastName2"), " ")
  names(deputes)[names(deputes)=="femme"] <- "isFemale"
  names(deputes)[names(deputes)=="parti"] <- "party"
  names(deputes)[names(deputes)=="circonscription"] <- "currentDistrict"
  names(deputes)[names(deputes)=="ministre"] <- "isMinister"

  clessnverse::logit("getting journalits from CSV", logger)
  journalists <- read.csv(file = "../clessn-blend/_SharedFolder_clessn-blend/data/journalist_handle.csv", sep = ";")
  journalists$X <- NULL
  names(journalists)[names(journalists)=="female"] <- "isFemale"
  names(journalists)[names(journalists)=="author"] <- "fullName"
  names(journalists)[names(journalists)=="selfIdJourn"] <- "thinkIsJournalist"
  names(journalists)[names(journalists)=="handle"] <- "twitterHandle"
  names(journalists)[names(journalists)=="realID"] <- "twittweJobTitle"
  names(journalists)[names(journalists)=="user_id"] <- "twitterID"
  names(journalists)[names(journalists)=="protected"] <- "twitterAccountProtected"
}
