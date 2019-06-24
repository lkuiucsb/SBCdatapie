#' A data_package_* handler for the shiny app
#'
#' @param data.pkg.doi The doi of the package being downloaded.
#' @param current.data Data current
#' @param download.dir The download directory from the read_data_archived
#' function.
#'
#' @return
#' Returns a list of tibbles containing data and metadata for a given data
#' package. Attributes include information about the doi and download folder.
#' 
#' @example 
#' #Load in the example data.
#' data("data_example", package = "ggplotgui")
#' 
data_package_shiny_handler <- function(data.pkg.doi, current.data = NULL,
  download.dir = NULL) {
  if(is.null(data.pkg.doi) || is.na(data.pkg.doi) || nchar(data.pkg.doi) < 1) {
    #Invalid conditions, so return the example data set
    cat("DPSH condition 1\n") #Debugging
    if(is.null(current.data) || !is.list(current.data)) {
      #Return the example data if some basic conditions aren't met
      cat("DPSH condition 1A\n") #Debugging
      data <- data_example
      return(data)
    } else {
      cat("DPSH condition 1B\n") #Debugging
      data <- current.data
      return(data)
    }
    #This is actually problematic. Keeping commented for now, but may delete
    # later
  # } else if (is.null(current.data) || !is.list(current.data)) {
  #   cat("DPSH condition 2\n") #Debugging
  #     data <- data_example
  #     return(data)
  } else {
    #Return the data package, unless there is an error, then the data example
    cat("DPSH condition 3\n") #Debugging
      data <- tryCatch(
        error = function(x) current.data,
        data_package_wrapper(data.pkg.doi, download.dir))
      return(data)
  }
  data
}

#TO DO:
# Add tests.
# Flesh out the roxygen skeleton
# Replace `read_data_archived` with `data_package_download`
