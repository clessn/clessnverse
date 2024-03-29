utils::globalVariables(c("n", "prob"))

#' Create samples biased on the categories of one variable
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
#' @param verbose TRUE or FALSE Whether activity messages are displayed on
#' the screen.
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
run_dictionary <- function(data, text, dictionary, verbose=TRUE) {
  if (verbose) tictoc::tic() # calculate number of seconds for function execution
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
  if (verbose) message(crayon::green("100% expressions/words found"))
  dataFinal <- quanteda::convert(dfm, to = "data.frame")
  if (verbose) tictoc::toc()
  return(dataFinal)
}
