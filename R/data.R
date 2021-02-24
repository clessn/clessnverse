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
  if (!exists(context))
    stop("clessnverse::createSimple(): must provide a context in which the agora dataframe")

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
  cat("1.1.4")
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
  cat("1.1.4")
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
  cat("1.1.4")
}

