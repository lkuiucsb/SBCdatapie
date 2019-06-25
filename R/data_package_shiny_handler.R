#' A data_package_* handler for the shiny app
#'
#' @param data.pkg.doi The doi of the package being downloaded.
#' @param current.data Current data loaded in the app, if any.
#' @param download.dir The download directory.
#'
#' @return
#' Returns a list of tibbles containing data and metadata for a given data
#' package. Attributes include information about the doi and download folder,
#' where available.
#' 
#' @details
#' This function is largely meant to interact with the main Shiny app used in
#' this package.
#' 
#' If no inputs are provided, the function returns a default data set. The name
#' of the tibble in this dataset is "Example_dataset.csv". The doi attribute is
#' set to NA. The folder attribute is also set to NA.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' #Load in the example data
#' data_package <- data_package_shiny_handler()
#' 
#' #Load an actual data set
#' doi_string <- "doi:10.6073/pasta/dd7955138eb963a847b861242390a48c"
#' data_package <- data_package_shiny_handler(data.pkg.doi = doi_string))
#' }
#' 
data_package_shiny_handler <- function(data.pkg.doi = NA, current.data = NULL,
  download.dir = NULL) {
  if(is.null(data.pkg.doi) || is.na(data.pkg.doi) || nchar(data.pkg.doi) < 1) {
    #Invalid conditions, so return the example data set
    if(is.null(current.data) || !is.list(current.data)) {
      #Return the example data if some basic conditions aren't met
      # cat("DPSH condition 1A\n") #Debugging
      data <- data_example
      return(data)
    } else {
      # cat("DPSH condition 1B\n") #Debugging
      data <- current.data
      return(data)
    }
  } else if(!is.null(current.data)){
    #Return the data package, unless there is an error, then the data example
    # cat("DPSH condition 2\n") #Debugging
      data <- tryCatch(
        error = function(x) current.data,
        data_package_wrapper(data.pkg.doi, download.dir))
      return(data)
  } else {
    # cat("DPSH condition 3\n") #Debugging
      data <- tryCatch(
        error = function(x) data_example,
        data_package_wrapper(data.pkg.doi, download.dir))
      return(data)
  }
  data
}

# proposed method for shorting file names
# shorten_file_name <- function(single_long_file_name){
#   file_name_split <- str_split(single_long_file_name, pattern = "\\.")[[1]]
#   file_name_with_ext <- file_name_split[str_detect(file_name_split, pattern = "__")]
#   file_name_without_ext <- str_split(file_name_with_ext, pattern = "__")[[1]][1]
#   return(file_name_without_ext)
# }
# file_names <- map_chr(names(data), shorten_file_name)