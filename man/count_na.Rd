% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangle.R
\name{count_na}
\alias{count_na}
\title{Count NA in a vector}
\usage{
count_na(x)
}
\arguments{
\item{x}{a vector}
}
\value{
number of NA or NaN in \code{x} (integer)
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\examples{
x <- c(4, 6, NA, 3, NaN, 1)
count_na(x)

z <- c(NA, NaN, "w", "a", "b", NA)
count_na(z)

}
