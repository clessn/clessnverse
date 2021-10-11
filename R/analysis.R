#################################################################################################################
#################################################################################################################
############################################# Analysis Functions ################################################
#################################################################################################################
#################################################################################################################

#### 1. Geometry ####
#### ~1.1 Eucledian distance ####
get_EuDistance <- function(point1,point2){
  if (length(point1) > 3 | length(point2) > 3) {
    stop("Points must come from a 2D or 3D space. \n Point 1 has ",length(point1),
         " dimensions and point 2 has ",length(point2)," dimensions.")
  }
  else if (length(point1) != length(point2)) {
    stop("Points must have the same number of dimensions. \n Point 1 has ",length(point1),
         " dimensions and point 2 has ",length(point2)," dimensions.")
  }
  else if (length(point1) == 0) {
    stop("Point 1 has length 0.")
  }
  else if (length(point2) == 0) {
    stop("Point 2 has length 0.")
  }
  else if (length(point1) == 3) {
    euDistance <- sqrt((point2[1]-point1[1])^2 + (point2[2]-point1[2])^2 + (point2[3]-point1[3])^2)
  }
  else if (length(point1) == 2) {
    euDistance <- sqrt((point2[1]-point1[1])^2 + (point2[2]-point1[2])^2)
  }
  return(euDistance)
}

#### 2. Sampling ####
#### ~2.1 Creating multiple samples with probabilities biased by category ####
library(magrittr)
#' Title
#'
#' @param data 
#' @param x 
#' @param probs 
#' @param size 
#' @param iterations 
#' @param replace 
#'
#' @return
#' @export
#'
#' @examples
sample_biased <- function(data, x, probs, size, iterations = 1, replace = FALSE){
  if(!is.data.frame(data))
    rlang::abort("Argument `data` must be a data frame.")
  VariableData <- data %>%
    dplyr::select({{x}}) %>% # create a data frame with only the variable to bias
    na.omit() %>% # remove columns where x = NA
    dplyr::mutate(rowID = 1:nrow(data)) # create column numbers
  VariableData[, 1] <- factor(VariableData[, 1],
                              ordered = FALSE) # transform into factor variable
  if(size < 1){
    rlang::abort(
      "Argument `size` must be a positive number.")
  }
  else if(!is.numeric(size)){
    rlang::abort(
      "Argument `size` must be a number.")
  }
  else{
    if((size / nrow(VariableData)) > 0.8 & replace == FALSE)
      warning(paste("Warning message:\nArgument `size` is at least 80% as large as",
                    "`x` and sampling is done without replacement. Probabilities",
                    "for some categories are likely to drop to zero after repeated",
                    "sampling."))
    else if(size > min(table(VariableData[, 1])) & replace == FALSE)
      warning(paste("Warning message:\nArgument `size` is larger than some",
                    "categories of `x` and sampling is done without replacement.",
                    "Probabilities for these categories may drop to zero after",
                    "repeated sampling."))
    categoriesFactor <- as.factor(names(table(
      VariableData[, 1]))) # identify the variable's categories
    if(length(categoriesFactor) != length(probs)) # probs = probabilities vector
      rlang::abort(
        "Argument `probs` must have the same number of categories as variable `x`.")
    else{
      VariableData$prob <- NA # create empty probabilities vector
      for(i in VariableData$rowID){
        index <- which(
          categoriesFactor == VariableData[i, 1]) # associate row IDs to categories
        VariableData$prob[i] <- probs[index] # create probabilities vector
      }
      if(iterations < 1){
        rlang::abort(
          "Argument `iterations` must be a positive number.")
      }
      else if(!is.numeric(iterations)){
        rlang::abort(
          "Argument `iterations` must be a number.")
      }
      else if(iterations == 1){
        sampleRowIDs <- sample(x = VariableData$rowID, size = size, # sample size
                               prob = VariableData$prob, replace = replace)
        return(VariableData[sampleRowIDs, ])
      }
      else{
        Samples <- purrr::map_dfr(
          .x = replicate(
            n = iterations, 
            expr = VariableData[sample(x = VariableData$rowID,
                                       size = size, # sample size
                                       prob = VariableData$prob,
                                       replace = replace), ],
            simplify = F),
          .f = as.list)
        # assign an ordered sample ID to differentiate between different samples
        Samples$iterationID <- rep(1:(nrow(Samples) / size), each = size)
        return(Samples)
      }
    }
  }
}

#### tests ####
sample_biased(CO2, Plant, probs = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1,
                                    .01, .02), size = 5)
sample_biased(CO2, Plant, probs = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1,
                                    .01, .02), size = 5, iterations = 2.1)
sample_biased(CO2, Plant, probs = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1,
                                    .01, .02), size = "5", iterations = "2.1")
sample_biased(CO2, Plant, probs = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1,
                                    .01, .02), size = 5, iterations = "2.1")
Test <- sample_biased(CO2, Plant, probs = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1,
                                            .01, .02), size = 50, iterations = 2,
                      replace = TRUE)
sample_biased(CO2, Treatment, probs = c("nonchilled" = .1, "chilled" = .5),
              size = 5)
sample_biased(CO2, Treatment, probs = c("chilled" = .1, "nonchilled" = .5),
              size = 5)

VariableData <- CO2 %>%
  dplyr::select(Plant) %>% # create a data frame with only the variable to bias
  na.omit() %>% # remove columns where x = NA
  dplyr::mutate(rowID = 1:nrow(CO2)) # create column numbers
VariableData[, 1] <- factor(VariableData[, 1],
                            ordered = FALSE) # transform into factor variable
categoriesFactor <- as.factor(names(table(
  VariableData[, 1]))) # identify the variable's categories
VariableData$prob <- NA # create empty probabilities vector
for(i in VariableData$rowID){
  index <- which(
    categoriesFactor == VariableData[i, 1]) # associate row IDs to categories
  VariableData$prob[i] <- c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1, .01, .02)[
    index] # create probabilities vector
}
sampleRowIDs <- sample(x = VariableData$rowID, size = 5, # sample size
                       prob = VariableData$prob, replace = FALSE)
VariableData[sampleRowIDs, ]
