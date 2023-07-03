# Suppress R CMD check note
#' @importFrom locateip locate_ip
#' @importFrom wikirest get_most_viewed_per_country
NULL

# Show message when loading package
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("DISCLAIMER: As of July 2023, `clessnverse` is no longer under active development.

To avoid breaking dependencies, the package remains available \"as is\" with no warranty of any kind.")
}
