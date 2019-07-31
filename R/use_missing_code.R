#'
#' Column-wise substitution of NAs for missing codes, if specified in attribute metadata.
#'
#' @param entity_list (list) A list object containing information on a single data entity in metajam output format.
#'
#' @return If successful: a list object but with NAs in the right places in data. Otherwise, a list object identical to input.
#'
#' @export

use_missing_code <- function(entity_list) {
  
  # ---
  # subset into metadata and data
  attrs <- entity_list[["attribute_metadata"]]
  dat <- entity_list[["data"]]
  
  # ---
  # check for attribute metadata and missingValueCode field
  if ("attribute_metadata" %in% names(entity_list) &
      "missingValueCode" %in% names(attrs)) {
    
    # ---
    # use match_names
    indices <- match_names(entity_list)
    
    # ---
    # column-wise loop
    for (i in 1:length(colnames(dat))) {
      
      # temporarily excluding dateTime variables: method not applicable to POSIXct variables
      if (attrs[1, "measurementScale"] != "dateTime") {
        
        # get missingValueCode
        code <- attrs[1, "missingValueCode"]
        
        # substitute where data matches code with NAs
        is.na(dat[[i]]) <-
          as.character(dat[[i]]) == as.character(code)
      }
    }
  }
  
  return(entity_list)
  
}

# ----------------------------------------------------------------------------------------------------------------

#' match_names
#' Match indices of column names in the data and attributeName's specified in metadata.
#'
#' @param entity_list (list) A list object containing information on a single data entity in metajam output format.
#'
#' @return (vector) A numeric vector with row indices in attribute metadata to match column indices in data.


match_names <- function(entity_list) {

  # ---
  # get params
  cols <- colnames(entity_list[["data"]])
  cols_attr <- entity_list[["attribute_metadata"]][["attributeName"]]
  indices <- seq(1:length(cols))
  
  # ---
  # get indices of mismatched names or order
  mismatches <- which(!(cols == cols_attr))
  
  
  # ---
  # proceed only if there are mismatches
  if (length(mismatches) != 0) {
  # fuzzy matching for matching indices, only where there wasn't an exact match earlier
  x <- c()
  for (i in 1:length(mismatches)) {
    x <-
      c(x, agrep(cols[mismatches][i], cols_attr[mismatches]))
  }
  # insert matched indices back in
  indices[mismatches] <- indices[mismatches[x]]
  }

  # ---
  # return correct vector of indices
  return(indices)
  
}
