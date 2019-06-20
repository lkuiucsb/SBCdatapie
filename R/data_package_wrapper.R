#' Download and read wrapper function
#' 
#' A wrapper for the \code{data_package_download} and \code{data_package_read}
#' functions. In some context, combining these functions makes them easier to
#' handle.
#'
#'
#' @param data.pkg.doi (character) Directory where a data package stored. Default (NULL) is
#'   the temporary data package directory defined by
#'   \code{paste0(tempdir(), 'data_package')}.
#' @param download.dir (character) Directory to which the data package will be downloaded. 
#'   Default is the temporary directory specified by 
#'   \code{paste0(tempdir(), '/data_package')}.
#'
#'
#' @return
#' Returns a list of tibbles containing data and metadata for a given data
#' package. Attributes include information about the doi and download folder.
#' 
#' 
#' @details
#' This function does two things:
#' 1. It combines the \code{data_package_download()} and
#' \code{data_package_read()} functions into a single function.
#' 2. It assigns attributes to the resulting data package, making it easier to
#' track where the data came from (doi) and where it is stored locally (folder).
#' 
#' 
#' @export
#'
#'
#' @examples
#' \dontrun{
#' #Download data to a temporary directory and then load it to the global
#' # environment.
#' example_package <- data_package_wrapper(data.pkg.doi = 'doi:10.18739/A2DP3X')
#' 
#' #Explicitly download to a temporary directory.
#' example_package <- data_package_wrapper(data.pkg.doi = 'doi:10.18739/A2DP3X',
#'   download.dir = tempdir())
#'   
#' #Download and load to current working directory
#' example_package <- data_package_wrapper(data.pkg.doi = 'doi:10.18739/A2DP3X',
#'   download.dir = getwd())
#' }
#' 
#' 
data_package_wrapper <- function(data.pkg.doi, download.dir = NULL) {
  #Remove the existing directory, if it exists
  data_package_remove()
  
  #Download the data
  data_package_download(data.pkg.doi, download.dir)
  
  #Read in the data
  data_package <- data_package_read(download.dir)
  
  #Assign a DOI attribute
  attributes(data_package)$doi <- data.pkg.doi
  
  #Record where the data was downloaded
  if(is.null(download.dir)) {
    #Default (NULL) is to use the temp directory
    attributes(data_package)$folder <- paste0(tempdir(), "/data_package")
  } else {
    #Use the specified directory
    attributes(data_package)$folder <- download.dir
  }
  
  return(data_package)
}
