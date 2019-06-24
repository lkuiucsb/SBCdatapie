#'
#'
#'
#'
#'
#'
#'
#'

use_missing_code <- function(data_entity_list) {
  
  # if there's missing codes specified in attribute metadata, then try and replace where these missing codes occur with NA's
  if ("attribute_metadata" %in% names(data_entity_list) & "missingValueCode" %in% names(data_entity_list[["attribute_metadata"]])) {
    for (varname in colnames(data_entity_list[["data"]])) {
      
      # temporarily excluding dateTime variables: method not applicable to POSIXct variables
      if (subset(data_entity_list[["attribute_metadata"]],
                 attributeName == varname,
                 measurementScale) != "dateTime") {
        # get code
        code <-
          subset(data_entity_list[["attribute_metadata"]],
                 attributeName == varname,
                 missingValueCode)
        
        # substitute where data matches code with NAs
        is.na(data_entity_list[["data"]][[varname]]) <-
          as.character(data_entity_list[["data"]][[varname]]) == as.character(code)
      }
    }
    
    return(data_entity_list)
  }
  return(data_entity_list)
}
