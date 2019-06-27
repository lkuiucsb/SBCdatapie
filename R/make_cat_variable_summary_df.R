#' make_cat_variable_summary_df
#' 
#' Describe what this function does
#' 
#' @usage
#'   make_cat_variable_summary_df(var)
#'
#' @param var
#'   Define this parameter.
#'
#' @return
#'   What does this function return?
#' 
#' @export
#'

make_cat_variable_summary_df<-function(var){
  var_type<-class(var)
  num_levels<-length(levels(as.factor(var)))
  num_valid<-sum(!is.na(var))
  num_missing<-sum(is.na(var))
  df<-data.frame(var_type,num_levels,num_valid,num_missing)
  return(df)
}