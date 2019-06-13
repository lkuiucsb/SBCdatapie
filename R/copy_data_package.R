#' Copy data package from temporary directory to user specified directory
#'
#' This function copies the data package from the temporary directory
#' specified by paste0(tempdir(), '/data_package') and copies it to the user
#' specified directory.
#' 
#' @param copy.dir
#'   (character) Directory to which the data package will be copied.
#'
#' @return
#'   (data package) Data package contents as parsed by \code{metajam}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download data package to temporary directory
#' output <- read_data_archived(data.pkg.doi = 'doi:10.18739/A2DP3X')
#' 
#' # Copy data package to user specified directory
#' copy_data_package(copy.dir = '/Users/csmith/Desktop/data_packages')
#' }
#' 
#' 

copy_data_package <- function(copy.dir){
  
  # Check user specified directory
  
  if (!dir.exists(copy.dir)){
    stop('Directory specified by "copy.dir" does not exist.')
  }
  
  # Copy data package to "copy.dir"
  
  if (dir.exists(paste0(tempdir(), '/data_package'))){
    file.copy(
      from = paste0(tempdir(), '/data_package/.'),
      to = copy.dir, 
      recursive = TRUE
    )
  } else {
    stop('No data package exists in the temporary directory.')
  }
  
}


