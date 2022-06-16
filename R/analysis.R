############################################# Analysis Functions #################
utils::globalVariables(c("n", "prob"))
#### 1. Geometry ####
#### ~1.1 Eucledian distance ####
#get_EuDistance <- function(point1,point2){
#  if (length(point1) > 3 | length(point2) > 3) {
#    stop("Points must come from a 2D or 3D space. \n Point 1 has ",length(point1),
#         " dimensions and point 2 has ",length(point2)," dimensions.")
#  }
#  else if (length(point1) != length(point2)) {
#    stop("Points must have the same number of dimensions. \n Point 1 has ",
#         length(point1),
#         " dimensions and point 2 has ",length(point2)," dimensions.")
#  }
#  else if (length(point1) == 0) {
#    stop("Point 1 has length 0.")
#  }
#  else if (length(point2) == 0) {
#    stop("Point 2 has length 0.")
#  }
#  else if (length(point1) == 3) {
#    euDistance <- sqrt((point2[1]-point1[1])^2 + (point2[2]-point1[2])^2 +
#                         (point2[3]-point1[3])^2)
#  }
#  else if (length(point1) == 2) {
#    euDistance <- sqrt((point2[1]-point1[1])^2 + (point2[2]-point1[2])^2)
#  }
#  return(euDistance)
#}

#### 2. Sampling ####
#### ~2.1 Creating multiple samples with probabilities biased by category ####
#' Create samples biased on the categories of one variable.
#'
#' This function creates a data.frame which includes one
#' or more samples taken from the original data. These samples
#' are biased on the categories of one variable: some of the
#' categories can be given a lower probability of inclusion
#' than others.
#'
#' @param data An object of type data.frame.
#' @param x A vector included in the data, which will be
#' used to bias the sample.
#' @param probs A vector of probabilities which has the
#' same length as the number of values x can take.
#' @param size The number of observations to include
#' in each sample.
#' @param iterations The number of samples to take from
#' the data.frame.
#' @param replace Should sampling be with replacement?
#'
#' @return The sampled data, with all columns from the
#' original data.frame and a `prob` column indicating
#' the probability of inclusion of each observation. If
#' `iterations > 1`, an `iterations` column is included
#' to identify which iteration the observation belongs to.
#' @export
#' @importFrom rlang abort
#' @import dplyr
#' @importFrom testthat context
#' @importFrom testthat test_that
#' @importFrom testthat expect_equal
#' @importFrom testthat expect_error
#' @importFrom testthat expect_warning
#' @importFrom purrr map_dfr
#' @author CLESSN
#' @examples
#'
#' \dontrun{
#'
#' # Create a 100-respondent sample where men are three
#' # times more likely to be included than women.
#'
#' sample_biased(Data, genderWoman,
#' probs = c(3, 1), size = 100)
#' }
#'
sample_biased <-
  function(data,
           x,
           probs,
           size,
           iterations = 1,
           replace = FALSE) {
    if (!is.data.frame(data))
      rlang::abort("Argument `data` must be a data frame.")
    VariableData <-
      dplyr::filter(data,!is.na({
        {
          x
        }
      })) # remove columns where x = NA
    VariableData$rowID <- 1:nrow(VariableData) # create column numbers
    columnNumber <-
      grep(deparse(substitute(x)), # get column number for x
           colnames(VariableData))
    VariableData[, columnNumber] <- factor(VariableData[, columnNumber], ordered = FALSE) # transform into factor variable
    if (size < 1) {
      rlang::abort("Argument `size` must be a positive number.")
    }
    else{
      if ((as.numeric(size) / nrow(VariableData)) > 0.8 &
          replace == FALSE &
          size <= nrow(VariableData))
        warning(
          paste(
            "Warning message:\nArgument `size` is at least 80% as large as",
            "`x` and sampling is done without replacement. Probabilities",
            "for some categories are likely to drop to zero after repeated",
            "sampling."
          )
        )
      else if (size > min(table(VariableData[, 1])) &
               replace == FALSE &
               size <= nrow(VariableData))
        warning(
          paste(
            "Warning message:\nArgument `size` is larger than some",
            "categories of `x` and sampling is done without replacement.",
            "Probabilities for these categories may drop to zero after",
            "repeated sampling."
          )
        )
      categoriesFactor <- as.factor(names(table(VariableData[, columnNumber]))) # identify the variable's categories
      for (k in probs) {
        # probs = probabilities vector
        if (k <= 0)
          rlang::abort("Argument `probs` must include positive numbers only.")
      }
      if (length(categoriesFactor) != length(probs))
        rlang::abort("Argument `probs` must have the same number of categories as variable `x`.")
      else{
        VariableData$prob <- NA # create empty probabilities vector
        for (i in VariableData$rowID) {
          index <- which(categoriesFactor == VariableData[i, columnNumber]) # associate row IDs to categories
          VariableData$prob[i] <-
            probs[index] # create probabilities vector
        }
        if (iterations < 1) {
          rlang::abort("Argument `iterations` must be a positive number.")
        }
        else if (iterations == 1) {
          sampleRowIDs <-
            sample(
              x = VariableData$rowID,
              size = size,
              # sample size
              prob = VariableData$prob,
              replace = replace
            )
          Sample <- VariableData[sampleRowIDs,]
          Sample$rowID <- NULL # remove row IDs from output
          return(Sample)
        }
        else{
          Samples <- purrr::map_dfr(
            .x = replicate(
              n = iterations,
              expr = VariableData[sample(
                x = VariableData$rowID,
                size = size,
                # sample size
                prob = VariableData$prob,
                replace = replace
              ),],
              simplify = F
            ),
            .f = as.list
          )
          Samples$iterationID <-
            rep(1:(nrow(Samples) / size), each = size)
          # assign an ordered sample ID to differentiate between different samples
          Samples$rowID <- NULL # remove row IDs from output
          return(Samples)
        }
      }
    }
  }

#### tests ####
VariableData <-
  dplyr::filter(CO2,!is.na(Plant)) # remove columns where x = NA
VariableData$rowID <- 1:nrow(VariableData) # create column numbers
columnNumber <-
  grep(deparse(substitute(Plant)), # get column number for x
       colnames(VariableData))
VariableData[, columnNumber] <- factor(VariableData[, columnNumber], ordered = FALSE) # transform into factor variable
categoriesFactor <- as.factor(names(table(VariableData[, columnNumber]))) # identify the variable's categories
VariableData$prob <- NA # create empty probabilities vector
for (i in VariableData$rowID) {
  index <- which(categoriesFactor == VariableData[i, columnNumber]) # associate row IDs to categories
  VariableData$prob[i] <-
    c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1, .01, .02)[index] # create probabilities vector
}
sampleRowIDs <-
  sample(
    x = VariableData$rowID,
    size = 5,
    # sample size
    prob = VariableData$prob,
    replace = FALSE
  )
Sample <- VariableData[sampleRowIDs,]
Sample$rowID <- NULL # remove row IDs from output



#### 3. Weighting ####
#### ~3.1 Calc proportions of categories for one variable ####
#' Calculate the proportion of each category from one variable.
#'
#' This function creates a data.frame which includes 3 columns.
#' 1) a column containing the variable's categories;
#' 2) a column containing each category's frequency;
#' 3) a column containing each category's proportion.
#'
#' @param data An object of type data.frame.
#' @param variable The name of the variable from which to calculate
#' the proportions.
#' @return A data.frame which includes 3 columns.
#' 1) `variable`: a column containing the variable's categories;
#' 2) `n`: a column containing each category's frequency;
#' 3) `prop`: a column containing each category's proportion.
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom rlang abort
#' @author CLESSN
#' @examples
#'
#' \dontrun{
#'
#' # Calculate the proportions of each cylinder configuration
#' # from mtcars.
#'
#' calculate_proportions(mtcars,cyl)
#' }
calculate_proportions <- function(data, variable) {
  if (!is.data.frame(data)) {
    rlang::abort("Argument `data` must be a data frame.")
  }
  else {
    D <- data %>%
      dplyr::group_by({
        {
          variable
        }
      }) %>%
      dplyr::summarize(n = dplyr::n()) %>% #category frequencies
      stats::na.omit() %>%
      dplyr::mutate(prop = n / sum(n))
  }
  if (length(table(D[, 1])) == 1) {
    warning(paste0("`", names(D[, 1]), "`", " only has one category."))
  }
  return(D)
}

#### 4. Dictionary analysis ####
#### ~4.1 Run dictionary ####
#' Calculate dictionary expression mentions in a text.
#'
#' This function creates a data.frame which includes one column
#' with each observation's ID as well as one column for each
#' category of the inputted dictionary.
#'
#' @param data An object of type data.frame.
#' @param text The name of the character variable for which
#' dictionary expressions are to be matched against.
#' @param dictionary An object of type dictionary.
#' @return A data.frame which includes one more column than
#' the number of dictionary categories. The first column is named
#' doc_id. Each other column is named after a dictionary category.
#' @export
#' @import dplyr
#' @importFrom crayon yellow
#' @importFrom crayon green
#' @import quanteda
#' @importFrom tictoc tic
#' @importFrom tictoc toc
#' @author CLESSN
#' @examples
#'
#' \dontrun{
#'
#' # Calculate the number of dictionary expression mentions in
#' a list of attitudes.
#'
#' run_dictionary(data.frame(colnames(attitude)),
#' text = colnames(attitude),
#' dictionary = quanteda::data_dictionary_LSD2015)
#' }
run_dictionary <- function(data, text, dictionary) {
  tictoc::tic() # calculate number of seconds for function execution
  if (is.data.frame(data) != "TRUE") {
    stop(crayon::yellow('the argument "data" needs to be a dataframe'))
  }
  data <- data %>% dplyr::mutate(text = {
    {
      text
    }
  })
  if (is.character(data$text) != "TRUE") {
    stop(crayon::yellow('The variable "text" needs to be a character vector'))
  }
  corpus <- quanteda::tokens(data$text) # transforms the vector into
  # tokens for dictionary analysis
  if (quanteda::is.dictionary(dictionary) != "TRUE") {
    stop(crayon::yellow(
      paste0(
        'Your "dictionary" needs to be in a dictionary format\n',
        ' For more information:"',
        ' https://quanteda.io/reference/dictionary.html'
      )
    ))
  }
  dfm <- # applies the dictionary to the variable corpus and creates a
    # dfm (document-feature matrix). Applies tolower() by default
    quanteda::dfm(quanteda::tokens_lookup(corpus, dictionary, nested_scope = "dictionary"))
  message(crayon::green("100% expressions/words found"))
  dataFinal <- quanteda::convert(dfm, to = "data.frame")
  tictoc::toc()
  return(dataFinal)
}
