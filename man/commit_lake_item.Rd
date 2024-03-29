% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dev_e.R
\name{commit_lake_item}
\alias{commit_lake_item}
\title{clessnverse::commit_lake_item}
\usage{
commit_lake_item(data, metadata, mode, credentials)
}
\arguments{
\item{data}{A named list object containing a key, a path and the item to be
added to the data lake}

\item{metadata}{A named list object containing the metadata to be applied to
the lake item}

\item{mode}{A string object.
If mode = "refresh", then if an object already exists with this
key in the data lake, then it will be overwritten.
If mode = "newonly", then if an object already exists with this
key in the data lake, then it will be skipped and a warning will
be logged}

\item{credentials}{These are your HUBLOT credentials.  They are obtained by
usung the hubblot::get_credentials function.

\if{html}{\out{<div class="sourceCode">}}\preformatted{               ATTENTION:
               DO NOT explicitely leave your cresential information
               (username and password) in your code.  Use environment
               variables instead, using the .Renviron file in your
               user home directory.
}\if{html}{\out{</div>}}}
}
\value{
the function returns
- the hublot id of the element that was added to the datalake if
the operation was successful.
- 1 if there was an error.
- 2 if there was a warning.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Adds or replaces an object in the lake in a specific path, with
a specific key
}
\examples{
example
}
