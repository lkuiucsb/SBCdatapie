#' A wrapper for data_package_download
#'
#' @param data.pkg.doi The doi of the package being downloaded.
#' @param download.dir The download directory from the read_data_archived
#' function.
#'
#' @return
#' 
data_package_download_wrapper <- function(data.pkg.doi, download.dir = NULL) {
  # Invalid conditions, so return the example data set
  if(is.null(data.pkg.doi) || is.na(data.pkg.doi) || nchar(data.pkg.doi) < 1) {
      #cat("Condition 3", "\n") #Debugging
      data <- data_example
  } else {
    # Return the data package, unless there is an error, then the data example
      #cat("Condition 4", "\n") #Debugging
      data <- tryCatch(
        error = function(x) data_example,
        read_data_archived(data.pkg.doi, download.dir))
      #If NOT returning the data example, then update the doi attribute.
      #Using a doi attribute makes it easier to check if the doi has changed
      if(!is.logical(all.equal(data, data_example))) {
        #cat("Condition 5", "\n") #Debugging
        attributes(data)$doi <- data.pkg.doi
      }
  }
  data
}

#TO DO:
# Add tests.
# Flesh out the roxygen skeleton
# Replace `read_data_archived` with `data_package_download`

