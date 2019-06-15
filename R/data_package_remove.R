#' Remove a data package directory
#'
#' This function removes a data package directory.
#'
#' @usage 
#'   data_package_remove(
#'     pkg.dir = NULL
#'   )
#'
#' @param pkg.dir
#'   (character) The directory to be removed. Default is the temporary 
#'   directory specified by \code{paste0(tempdir(), '/data_package')}.
#'
#' @return
#'   (message) Message indicating whether or not the directory has been 
#'   removed.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'   # Remove the temporary data package directory
#'   data_package_remove()
#' }
#' 

data_package_remove <- function(pkg.dir = NULL){
  
  # Set pkg.dir
  
  if (is.null(pkg.dir)){
    pkg.dir <- paste0(tempdir(), '/data_package')
  }
  
  # Error if pkg.dir doesn't exist
  
  if (!dir.exists(pkg.dir)){
    stop("Data package directory doesn't exist.")
  }
  
  # Remove pkg.dir
  
  unlink(
    pkg.dir,
    recursive = TRUE,
    force = TRUE
  )
  
  message(
    paste0(
      'Removing data package directory "', 
      pkg.dir,
      '"'
    )
  )

}