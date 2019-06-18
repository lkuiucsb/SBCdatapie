#' A data_package_* handler for the shiny app
#'
#' @param data.pkg.doi The doi of the package being downloaded.
#' @param download.dir The download directory from the read_data_archived
#' function.
#'
#' @return
#' 
data_package_shiny_handler <- function(data.pkg.doi, current.data,
  download.dir = NULL) {
  # Invalid conditions, so return the example data set
  if(is.null(data.pkg.doi) || is.na(data.pkg.doi) || nchar(data.pkg.doi) < 1) {
      #cat("Condition 3", "\n") #Debugging
      data <- current.data
  } else {
    # Return the data package, unless there is an error, then the data example
      #cat("Condition 4", "\n") #Debugging
      data <- tryCatch(
        error = function(x) current.data,
        data_package_wrapper(data.pkg.doi, download.dir)
      )
  }
  data
}

#TO DO:
# Add tests.
# Flesh out the roxygen skeleton
# Replace `read_data_archived` with `data_package_download`
