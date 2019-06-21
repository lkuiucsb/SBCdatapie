#'
#'
#'
#'
#'
#'
#'
#'

use_missing_code <- function(data_entity_list) {
  for (varname in colnames(data_entity_list[["data"]])) {
    if (subset(data_entity_list[["attribute_metadata"]],
               attributeName == varname,
               measurementScale) != "dateTime") {
      code <-
        subset(data_entity_list[["attribute_metadata"]],
               attributeName == varname,
               missingValueCode)
    is.na(data_entity_list[["data"]][[varname]]) <-
      as.character(data_entity_list[["data"]][[varname]]) == as.character(code)
    }
  }
  
  return(data_entity_list)
}
