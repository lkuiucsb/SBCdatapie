#' A wrapper for read_data_archive
#'
#' @param data.pkg.doi The doi of the package being downloaded.
#' @param download.dir The download directory from the read_data_archived
#' function.
#'
#' @return
#' 
rda_wrapper <- function(data.pkg.doi, download.dir = NULL) {
  # Invalid conditions, so return the example data set
  if(is.null(data.pkg.doi) || is.na(data.pkg.doi) || nchar(data.pkg.doi) < 1) {
      cat("Condition 3", "\n")
      data <- data_example
  } else {
    # Return the data package, unless there is an error, then the data example
      cat("Condition 4", "\n")
      data <- tryCatch(
        error = function(x) data_example,
        read_data_archived(data.pkg.doi, download.dir))
      #If NOT returning the data example, then update the doi attribute.
      #Using a doi attribute makes it easier to check if the doi has changed
      if(!is.logical(all.equal(data, data_example))) {
        cat("Condition 5", "\n")
        attributes(data)$doi <- data.pkg.doi
      }
  }
  data
}

#TO DO:
# Add tests.

