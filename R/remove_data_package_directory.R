#' Remove the temporary data package directory
#'
#' This function removes the temporary data package directory created
#' by \code{download_data_package()} and to which the data package contents are
#' downloaded to. This task is required before a new data package is downloaded 
#' by the user.
#'
#' @details 
#'   The temporary data package directory is located at 
#'   \code{paste0(tempdir(), '/data_package')}
#'
#' @param download.dir
#'   (character) The directory to be removed.
#'
#' @return
#'   (message) Message indicating whether or not the directory has been 
#'   removed.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'   # Remove default data package directory
#'   remove_data_package_directory()
#' }
#' 

remove_data_package_directory <- function(download.dir = paste0(tempdir(), '/data_package')){
  
  if (dir.exists(paste0(tempdir(), '/data_package'))){
    unlink(
      paste0(tempdir(), '/data_package'),
      recursive = TRUE,
      force = TRUE
    )
    message('Data package directory has been removed')
  } else {
    message("Data package directory can't be removed because it doesn't exist.")
  }
  
}