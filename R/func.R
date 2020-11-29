runDictionaryFunction <- function(corpusA, dataA, word, dfmA, dataB, dictionaryA) {
  corpusA <- corpus(dataA$word)
  dfmA    <- dfm(corpusA, dictionary = dictionaryA)
  dataB   <- convert(dfmA, to = "data.frame")
  return(dataB)
}


evaluateRelevanceIndex <- function (vecTerms, corpus, base) {
  relevanceIndex <- 0
  vec.corpus <- vector()
  
  if ( !is.null(base) ) {
    if (base == "paragraph") {
      vec.corpus <- strsplit(corpus, "\n\n")[[1]]

      for (i in 1:length(vec.corpus)) {
        string.to.check <- str_replace_all(vec.corpus[i], "[[:punct:]]", "")
        
        if ( TRUE %in% str_detect(string.to.check, vecTerms) ) relevanceIndex <- relevanceIndex + 1
      }
      
      relevanceIndex <- relevanceIndex / length(vec.corpus)
    } #if base == paragraph
    
    
    if (base == "sentence") {
      corpus <- str_replace_all(string = corpus, pattern = "M\\.", replacement = "")
      corpus <- str_replace_all(string = corpus, pattern = "Mr\\.", replacement = "")
      corpus <- str_replace_all(string = corpus, pattern = "Dr\\.", replacement = "")
      dfSentences <- tibble(text = corpus) %>% unnest_tokens(sentence, text, token="sentences",
                                                             format="text")
      count <- 0
      vecTerms <- tolower(vecTerms)
      corpus <- tolower(corpus)
      
      for (i in 1:nrow(dfSentences)) {
        if ( TRUE %in% str_detect(str_replace_all(dfSentences[i,], "[[:punct:]]", ""), vecTerms) ) 
          count <- count + 1
      }
      
      relevanceIndex <- count / nrow(dfSentences)
      if (is.nan(relevanceIndex)) relevanceIndex <- 0
    } #if base == sentence
  }
  
  return(relevanceIndex)
}
