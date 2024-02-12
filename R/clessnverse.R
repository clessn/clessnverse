#' @title Package Attach Hook Function
#' @description This function is automatically called when the `clessnverse` package is attached using `library(clessnverse)`.
#' It ensures that the `sondr` package is also loaded, making all its functions available in the user's session.
#' Additionally, it displays a startup message with a disclaimer about the maintenance status of `clessnverse`.
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  if (require("sondr", character.only = TRUE, quietly = TRUE)) {
    packageStartupMessage("DISCLAIMER: As of July 2023, `clessnverse` is no longer under active development.
To avoid breaking dependencies, the package remains available \"as is\" with no warranty of any kind.")
  } else {
    warning("Package 'sondr' is not available. Some functionality may be missing.")
  }
}