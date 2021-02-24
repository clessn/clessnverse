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
  if (missing(context))
    stop("clessnverse::createSimple(): must provide a context in which to create the agora dataframe possible values are quebec | canada | europe")

  return(data.frame(uuid = character(),
                    created = character(),
                    modified = character(),
                    metedata = character(),
                    eventID = character(),
                    eventSourceType = character(),
                    eventURL = character(),
                    eventDate = character(),
                    eventStartTime = character(),
                    eventEndTime = character(),
                    eventTitle = character(),
                    eventSubtitle = character(),
                    eventWordCount = integer(),
                    eventSentenceCount = integer(),
                    eventParagraphCount = integer(),
                    eventContent = character(),
                    eventTranslatedContent = character(),
                    stringsAsFactors = FALSE))
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
  if (missing(context))
    stop("clessnverse::createDeep(): must provide a context in which to create the agora dataframe possible values are quebec | canada | europe")

  return(data.frame(uuid = character(),
                    created = character(),
                    modified = character(),
                    metedata = character(),
                    eventID = character(),
                    chapterNumber = character(),
                    chapterTitle = character(),
                    chapterTabledDocId = character(),
                    chapterAdoptedDocId = character(),
                    interventionSeqNum = integer(),
                    speakerFirstName = character(),
                    speakerLastName = character(),
                    speakerFullName = character(),
                    speakerGender = character(),
                    speakerIsMinister = character(),
                    speakerType = character(),
                    speakerCountry = character(),
                    speakerParty = character(),
                    speakerPolGroup = character(),
                    speakerDistrict = character(),
                    speakerMedia = character(),
                    speakerSpeechType = character(),
                    speakerSpeechLang = character(),
                    speakerSpeechWordCount = integer(),
                    speakerSpeechSentenceCount = integer(),
                    speakerSpeechParagraphCount = integer(),
                    speakerSpeech = character(),
                    speakerTranslatedSpeech = character(),
                    stringsAsFactors = FALSE))
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
  if (missing(context) || !context %in% c("quebec" ,"canada", "europe"))
    stop("clessnverse::createCache(): must provide a context in which to create the agora dataframe possible values are quebec | canada | europe")

    return(data.frame(uuid = character(),
                    created = character(),
                    modified = character(),
                    metedata = character(),
                    eventID = character(),
                    eventHtml = character(),
                    stringsAsFactors = FALSE))
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
  if (missing(context))
    stop("clessnverse::loadSimpleFromHub(): must provide a context in which to create the agora dataframe possible values are quebec | canada | europe")
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
  if (missing(context) || !context %in% c("quebec" ,"canada", "europe"))
    stop("clessnverse::loadDeepFromHub(): must provide a context in which to create the agora dataframe possible values are quebec | canada | europe")
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
  if (missing(context))
    stop("clessnverse::loadDeepFromHub(): must provide a context in which to create the agora dataframe possible values are quebec | canada | europe")
}

