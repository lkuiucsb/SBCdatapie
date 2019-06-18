#' Wrapper for the data_package_download and data_package_read functions
#'
#' @param data.pkg.doi 
#' @param download.dir 
#'
#' @return
#' @export
#'
#' @examples
data_package_wrapper <- function(data.pkg.doi, download.dir = NULL) {
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
