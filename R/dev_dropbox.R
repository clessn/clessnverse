######################################################
#' @title clessnverse::dbxListDir
#'
#' @param dir Directory
#' @param token Token to pass on to 'httr'
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


    if (length(l$entries) > 0) {
      for (i in 1:length(l$entries)) {
        df <- df %>% rbind(data.frame(objectType = l$entries[i][[1]]$.tag,
                                      objectName = basename(l$entries[i][[1]]$path_display),
                                      objectPath = dirname(l$entries[i][[1]]$path_display),
                                      objectID = l$entries[i][[1]]$id))
      }
      clessnverse::logit(message=paste("directory", dir, "sucessfully listed."), logger = logger)
    } else {
      clessnverse::logit(message=paste("warning : directory", dir, "is empty"), logger = logger)
    }
    return(df)
  } else {
    if (TRUE %in% grepl("invalid_access_token", httr::content(r))) stop("invalid dropbox token")

    clessnverse::logit(message=paste(httr::content(r), "when attempting list content of folder", dir), logger = logger)
    return(data.frame(objectType = r$entries[i][[1]]$.tag,
                      objectName = r$entries[i][[1]]$path_lower,
                      objectID = r$entries[i][[1]]$id))
  }

}


######################################################
#' @title clessnverse::dbxMoveFile
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

    clessnverse::logit(message=paste("file", source, "sucessfully moved"), logger = logger)
    return(TRUE)

  } else {
    if (TRUE %in% grepl("invalid_access_token", httr::content(r))) stop("invalid dropbox token")

    if (grepl("conflict", httr::content(r)) && overwrite == TRUE) {
      # The destination file already exists : if overwrite == TRUE then let's delete destination and try again
      clessnverse::logit(message=paste("file", destination, "already exists in destination.  Deleting it from destination and trying to move again"), logger = logger)

      if (overwrite == TRUE) result <- clessnverse::dbxDeleteFile(destination, token)

      if (result == TRUE) {
        clessnverse::logit(message="destination file deleted, trying to move again", logger = logger)
        result1 <- clessnverse::dbxMoveFile(source, destination, token, overwrite)
        if (result1 == TRUE) {
          clessnverse::logit(message="file moved", logger = logger)
          return(TRUE)
        } else {
          clessnverse::logit(message="something went wrong while trying to move file", logger = logger)
          return(FALSE)
        }
      } else {
        clessnverse::logit(message="could not delete destination file", logger = logger)
        return(FALSE)
      }
    } else {
      clessnverse::logit(message=paste("Error", httr::content(r), "when attempting to move file", source), logger = logger)
    }
  }
}
######################################################
#' @title clessnverse::dbxDeleteFile
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
    clessnverse::logit(message=paste("file",filename,"deleted"), logger = logger)
    return(TRUE)
  } else {
    if (TRUE %in% grepl("invalid_access_token", httr::content(r))) stop("invalid dropbox token")

    clessnverse::logit(message=paste("error", httr::content(r),"trying to delete file",filename), logger = logger)
    return(FALSE)
  }
}

######################################################
#' @title clessnverse::dbxDownloadFile
#'
#' @export
dbxDownloadFile <- function(filename, local_path,token) {
  header <- paste('{\"path\": \"',
                  filename,
                  '\"}',
                  sep='')

  local_path <- file.path(local_path, basename(filename) )

  clessnverse::logit(message=paste("downloading file", filename), logger = logger)

  r <- httr::POST(url = 'https://content.dropboxapi.com/2/files/download',
                  httr::add_headers('Authorization' = paste("Bearer", token),
                                    'Dropbox-API-Arg'= header),
                  httr::write_disk(local_path, TRUE))

  if (r$status_code == 200) {
    clessnverse::logit(message=paste("file", filename, "sucessfully downloaded"), logger = logger)
    return(TRUE)
  } else {
    if (TRUE %in% grepl("invalid_access_token", httr::content(r))) stop("invalid dropbox token")
    clessnverse::logit(message=paste("Error", httr::content(r), "when attempting to download file", filename), logger = logger)
    return(FALSE)
  }
}


######################################################
#' @title clessnverse::dbxUploadFile
#'
#' @export
dbxUploadFile <- function(filename, remote_path, token, overwrite = FALSE) {

  if (overwrite == TRUE) mode <- "overwrite"
  else mode <- "add"

  header <- paste('{\"path\": \"',
                  remote_path,
                  filename,
                  '\",\"mode\": \"',mode,
                  '\",\"autorename\": true,\"mute\": false,\"strict_conflict\": false}',
                  sep='')

  clessnverse::logit(message=paste("uploading file", filename), logger = logger)

  r <- httr::POST(url = 'https://content.dropboxapi.com/2/files/upload',
                  httr::add_headers('Authorization' = paste("Bearer", token),
                                    'Dropbox-API-Arg'= header,
                                    'Content-Type' = "application/octet-stream"),
                  body = httr::upload_file(filename, "application/octet-stream"))

  if (r$status_code == 200) {
    clessnverse::logit(message=paste("file", filename, "sucessfully downloaded"), logger = logger)
    return(TRUE)
  } else {
    if (TRUE %in% grepl("invalid_access_token", httr::content(r))) stop("invalid dropbox token")
    clessnverse::logit(message=paste("Error", httr::content(r), "when attempting to download file", filename), logger = logger)
    return(FALSE)
  }
}

