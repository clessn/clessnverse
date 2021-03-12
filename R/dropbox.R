######################################################
#' @title clessnverse::dbxListDir
#' @description
#' @param
#' @param
#' @param
#' @return
#' @examples
#'
#'
#' @export
dbxListDir <- function(dir, token) {
  body <- paste('{\"path\": \"',
                dir,
                '\",\"recursive\": false,\"include_media_info\": false,\"include_deleted\": false,\"include_has_explicit_shared_members\": false,\"include_mounted_folders\": true,\"include_non_downloadable_files\": true}',
                sep='')

  r <- httr::POST(url = 'https://api.dropboxapi.com/2/files/list_folder',
                  httr::add_headers('Authorization' = paste("Bearer", token),
                                    'Content-Type' = 'application/json'),
                  body = body,
                  encode = "form")

  if (r$status_code == 200) {
    l <- httr::content(r)

    df <- data.frame(objectType=character(),
                     objectName=character(),
                     objetcPath=character(),
                     objectID=character())


    for (i in 1:length(l$entries)) {
      df <- df %>% rbind(data.frame(objectType = l$entries[i][[1]]$.tag,
                                    objectName = basename(l$entries[i][[1]]$path_lower),
                                    objectPath = dirname(l$entries[i][[1]]$path_lower))
                                    objectID = l$entries[i][[1]]$id))
    }

    clessnverse::logit(paste("directory", dir, "sucessfully listed."), logger)
    return(df)
  } else {
    clessnverse::logit(paste(httr::content(r), "when attempting list content of folder", dir), logger)
    return(data.frame(objectType = r$entries[i][[1]]$.tag,
                      objectName = r$entries[i][[1]]$path_lower,
                      objectID = r$entries[i][[1]]$id))
  }

}


######################################################
#' @title clessnverse::dbxMoveFile
#' @description
#' @param
#' @param
#' @param
#' @return
#' @examples
#'
#'
#' @export
dbxMoveFile <- function(source, destination, token, overwrite=FALSE) {
  body <- paste('{\"from_path\": \"/',
                source,
                '\",\"to_path\": \"/',
                destination,
                '\",\"allow_shared_folder\": false,\"autorename\": false,\"allow_ownership_transfer\": false}',
                sep='')

  r <- httr::POST(url = 'https://api.dropboxapi.com/2/files/move_v2',
                  httr::add_headers('Authorization' = paste("Bearer", token),
                                    'Content-Type' = 'application/json'),
                  body = body,
                  encode = "form")

  if (r$status_code == 200) {

    clessnverse::logit(paste("file", fileName, "sucessfully moved"), logger)
    return(TRUE)

  } else {

    clessnverse::logit(paste("Error", httr::content(r)$error_summary, "when attempting to move file", source), logger)

    if (grepl("conflict", httr::content(r)$error_summary) && overwrite == TRUE) {
      # The destination file already exists : if overwrite == TRUE then let's delete destination and try again
      clessnverse::logit(paste("file", destination, "already exists in destination.  Deleting it from destination and trying to move again"), logger)

      if (overwrite == TRUE) result <- clessnverse::dbxDeleteFile(destination, token)

      if (result == TRUE) {
        clessnverse::logit("destination file deleted, trying to move again", logger)
        result1 <- clessnverse::dbxMoveFile(source, destination, token, overwrite)
        if (result1 == TRUE) {
          clessnverse::logit("file moved", logger)
          return(TRUE)
        } else {
          clessnverse::logit("something went wrong while trying to move file", logger)
          return(FALSE)
        }
      } else {
        clessnverse::logit("could not delete destination file", logger)
        return(FALSE)
      }
    }
  }
}
######################################################
#' @title clessnverse::dbxDeleteFile
#' @description
#' @param
#' @param
#' @param
#' @return
#' @examples
#'
#'
#' @export
dbxDeleteFile <- function(filename, token) {
  body <- paste('{\"path\": \"/',
                filename,
                '\"}',
                sep='')

  r <- httr::POST(url = 'https://api.dropboxapi.com/2/files/delete_v2',
                  httr::add_headers('Authorization' = paste("Bearer", token),
                                    'Content-Type' = 'application/json'),
                  body = body,
                  encode = "form")

  #httr::verbose(info = FALSE))

  if (r$status_code == 200) {
    clessnverse::logit(paste("file",filename,"deleted"), logger)
    return(TRUE)
  } else {
    clessnverse::logit(paste("error", http::content(r),"trying to delete file",filename), logger)
    return(FALSE)
  }
}

######################################################
#' @title clessnverse::dbxDownloadFile
#' @description
#' @param
#' @param
#' @param
#' @return
#' @examples
#'
#'
#' @export
dbxDownloadFile <- function(filename, local_path,token) {
  header <- paste('{\"path\": \"',
                  filename,
                  '\"}',
                  sep='')

  local_path <- file.path(local_path, basename(filename) )

  clessnverse::logit(paste("downloading file", fileName), logger)

  r <- httr::POST(url = 'https://content.dropboxapi.com/2/files/download',
                  httr::add_headers('Authorization' = paste("Bearer", token),
                                    'Dropbox-API-Arg'= header),
                  httr::write_disk(local_path, TRUE))

  if (r$status_code == 200) {
    clessnverse::logit(paste("file", fileName, "sucessfully downloaded"), logger)
    return(TRUE)
  } else {
    clessnverse::logit(paste("Error", httr::content(r), "when attempting to download file", filename), logger)
    return(FALSE)
  }
}


######################################################
#' @title clessnverse::dbxUploadFile
#' @description
#' @param
#' @param
#' @param
#' @return
#' @examples
#'
#'
#' @export


