runDictionaryFunction <- function(corpusA, dataA, word, dfmA, dataB, dictionaryA) {
  corpusA <- corpus(dataA$word)
  dfmA    <- dfm(corpusA, dictionary = dictionaryA)
  dataB   <- convert(dfmA, to = "data.frame")
  return(dataB)
}




evaluateRelevanceIndex <- function (textToCheck, dictionary, base = "sentence", method = "dfm") {
  relevanceIndex <- 0
  
  if (is.na(textToCheck) || nchar(textToCheck) == 0) return(relevanceIndex)
    
  if (base == "paragraph") {
    vec.textToCheck <- vector()
    vec.textToCheck <- strsplit(textToCheck, "\n\n")[[1]]
    
    for (i in 1:length(vec.textToCheck)) {
      string.to.check <- str_replace_all(vec.textToCheck[i], "[[:punct:]]", "")
      
      if ( TRUE %in% str_detect(string.to.check, dictionary) ) relevanceIndex <- relevanceIndex + 1
    }
    
    relevanceIndex <- relevanceIndex / length(vec.textToCheck)
  } #if base == paragraph
  
  if (base == "sentence") {
    
    textToCheck <- str_replace_all(string = textToCheck, pattern = "M\\.", replacement = "")
    textToCheck <- str_replace_all(string = textToCheck, pattern = "Mr\\.", replacement = "")
    textToCheck <- str_replace_all(string = textToCheck, pattern = "Dr\\.", replacement = "")

    dfSentences <- tibble(text = textToCheck) %>% 
      unnest_tokens(sentence, text, token="sentences",format="text")

    count <- 0
    
    for (i in 1:nrow(dfSentences)) {
      if (method == "dfm") {
        ###  DFM METHOD
        if (!is.na(dfSentences[i,])) {
          dfmA <- dfm(dfSentences[i,], dictionary)
          if (length(dfmA@x) != 0 && dfmA@x > 0) count <- count + 1
        }
      } else {
        ### REGEX OU WORDS MATCHING 
        if ( TRUE %in% str_detect(str_replace_all(dfSentences[i,], "[[:punct:]]", ""), dictionary = ) ) 
          count <- count + 1
      }
    }
    relevanceIndex <- count / nrow(dfSentences)
    if (is.nan(relevanceIndex)) relevanceIndex <- 0
  } #if base == sentence

  return(relevanceIndex)
}
