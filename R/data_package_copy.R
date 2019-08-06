#' Copy data package from temporary directory to user specified directory
#'
#' This function copies the data package from the temporary directory
#' specified by paste0(tempdir(), '/data_package') to a user
#' specified directory.
#' 
#' @usage
#'   data_package_copy(
#'     copy.dir
#'   )
#' 
#' @param copy.dir
#'   (character) Directory to which the data package will be copied.
#'
#' @return
#'   (data package) Data package contents as parsed by \code{metajam}.
#'   
#' @export
#'
#' @examples
#' \dontrun{
#' # Download data package to temporary directory
#' data_package_download('doi:10.18739/A2DP3X')
#' 
#' # Copy data package to user specified directory
#' copy_data_package('/Users/csmith/Desktop/data_packages')
#' 
#' # Clean up
#' data_package_remove()
#' }
#' 

data_package_copy <- function(copy.dir){
  
  # Check user specified directory
  
  if (!dir.exists(copy.dir)){
    stop('Directory specified by "copy.dir" does not exist.')
  }
  
  # Copy data package to "copy.dir"
  
  if (dir.exists(paste0(tempdir(), '/data_package'))){
    
    file.copy(
      from = paste0(tempdir(), '/data_package/.'),
      to = copy.dir,
      overwrite = FALSE,
      recursive = TRUE
    )
    
  } else {
    
    stop('No data package exists in the temporary directory.')
    
  }
  
}


