#' Read an archived data package into R as a list object (THIS FUNCTION IS DEPRECATED. PLEASE USE data_package_download() and data_package_read() INSTEAD.)
#' 
#' This function downloads and reads a data package available in the 
#' \href{https://search.dataone.org/data}{DataONE} network to the R environment 
#' as a list object.
#' 
#' @details
#'   This function is a wrapper to the \code{download_d1_data_pkg()} and 
#'   \code{read_d1_data_pkg()} functions of the 
#'   \href{https://github.com/NCEAS/metajam}{metajam} package, but configured
#'   to output a list object usable by this project.
#'
#' @param data.pkg.doi
#'   (character) DOI of a data package available in the DataONE network
#'   (e.g. "doi:10.18739/A2DP3X").
#' @param download.dir
#'   (character) Directory to which the data package will be downloaded. 
#'   Default is \code{paste0(tempdir(), '/data_package')}
#'   
#'
#' @return
#'   (data package) Data package objects (as rendered by \code{metajam}) in the
#'   download directory.
#'   (list of data objects) List of data objects created by \code{metajam}. 
#'   Readable data objects are defined by in the \code{metajam} package.
#'   Tabular data is read into tibbles. 
#' @export
#'
#' @examples
#' \dontrun{
#' # Download data package to temporary directory and read
#' output <- read_data_archived(data.pkg.doi = 'doi:10.18739/A2DP3X')
#' 
#' # View data package content
#' View(output)
#' }
#' 

read_data_archived <- function(data.pkg.doi, download.dir = NULL){
  
  # Send deprecation notice ---------------------------------------------------
  
  .Deprecated(
    new = c('data_package_download()', 'data_package_read()'),
    package = 'dummyPackageTitle',
    old = 'read_data_archived()'
  )
  
  # Create directory for data and metadata ------------------------------------
  
  message('Creating data package directory')
  
  if (is.null(download.dir)){
    download.dir <- paste0(tempdir(), '/data_package')
    dir.create(download.dir)
  }
  
  if (!dir.exists(download.dir)){
    stop('Download directory does not exist.')
  }
  
  # Download data and metadata to directory -----------------------------------
  
  message('Downloading data package to directory')
  
  pkg_dir_name <- metajam::download_d1_data_pkg(
    meta_obj = data.pkg.doi,
    path = download.dir
  )
  
  # Read data and metadata ----------------------------------------------------
  
  # Read data and metadata for each data object of the data package
  
  output <- lapply(
    pkg_dir_name,
    metajam::read_d1_files
  )
  
  # Set list object names as list names
  
  fname <- paste0(
    stringr::str_extract(
      pkg_dir_name,
      '(?<=__)[:graph:]*(?=__)'
    ),
    '.',
    stringr::str_extract(
      pkg_dir_name,
      '(?<=__)[:alpha:]*$'
    )
  )
  
  names(output) <- fname
  
  # Set class and add directory -----------------------------------------------
  
  class(output) <- 'online_data'
  
  output$data_package_path <- pkg_dir_name
  
  # Return data, metadata, and directory path(s) ------------------------------
  
  output
  
}
