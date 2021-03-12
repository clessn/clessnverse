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
    df <- data.frame(objectType=character(),
                     objectName=character(),
                     objectID=character())


    print(length(r$entries))

    for (i in 1:length(r$entries)) {
      print(i)
      cat(r$entries[i][[1]]$.tag,
          r$entries[i][[1]]$path_lower,
          r$entries[i][[1]]$id,
          "\n")
      df <- df %>% rbind(data.frame(objectType = r$entries[i][[1]]$.tag,
                                    objectName = r$entries[i][[1]]$path_lower,
                                    objectID = r$entries[i][[1]]$id))
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
        #body <- paste('{\"from_path\": \"/',source,'\",\"to_path\": \"/',destination,
        #              '\",\"allow_shared_folder\": false,\"autorename\": false,\"allow_ownership_transfer\": false}',
        #              sep='')
        #
        #t <- httr::POST(url = 'https://api.dropboxapi.com/2/files/move_v2',
        #                add_headers('Authorization' = paste("Bearer", token$credentials$access_token),
        #                            'Content-Type' = 'application/json'),
        #                body = body,
        #                encode = "form")
        #httr::verbose(info = FALSE))
        result1 <- clessnverse::dbxMoveFile(source, destination, token, overwrite)
        if (result1 == TRUE) {
          clessnverse::logit("file moved", logger)
        } else {
          stop("something went wrong while trying to move file", logger)
        }
      } else {
        stop("could not delete destination file", logger)
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

  s <- httr::POST(url = 'https://api.dropboxapi.com/2/files/delete_v2',
                  httr::add_headers('Authorization' = paste("Bearer", token),
                                    'Content-Type' = 'application/json'),
                  body = body,
                  encode = "form")

  #httr::verbose(info = FALSE))

  if (s$status_code == 200) {
    clessnverse::logit(paste("file",filename,"deleted"), logger)
    return(TRUE)
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


