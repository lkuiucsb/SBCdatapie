#'
#' Column-wise substitute NAs for missing codes, if specified in attribute metadata.
#'
#' @param entity_list (list) A list object containing information on a single data entity in metajam output format.
#'
#' @return If successful: a list object but with NAs in the right places in data. Otherwise, a list object identical to input.
#' 
#' @export

use_missing_code <- function(entity_list) {
  
  # check for attribute metadata and missingValueCode field
  if ("attribute_metadata" %in% names(entity_list) & "missingValueCode" %in% names(entity_list[["attribute_metadata"]])) {
    
    # column-wise loop
    for (varname in colnames(entity_list[["data"]])) {
      
      # temporarily excluding dateTime variables: method not applicable to POSIXct variables
      if (subset(entity_list[["attribute_metadata"]],
                 attributeName == varname,
                 measurementScale) != "dateTime") {
        # get code
        code <-
          subset(entity_list[["attribute_metadata"]],
                 attributeName == varname,
                 missingValueCode)
        
        # substitute where data matches code with NAs
        is.na(entity_list[["data"]][[varname]]) <-
          as.character(entity_list[["data"]][[varname]]) == as.character(code)
      }
    }
    
    return(entity_list)
  }
  return(entity_list)
}
