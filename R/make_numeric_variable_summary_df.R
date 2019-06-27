#' make_numeric_variable_summary_df
#' 
#' Describe what this function does
#' 
#' @usage
#'   make_numeric_variable_summary_df(var)
#'
#' @param var
#'   Define this parameter.
#'
#' @return
#'   What does this function return?
#' 
#' @export
#'

make_numeric_variable_summary_df<-function(var){
  var_median<-median(var,na.rm=T)
  var_max<-max(var,na.rm=T)
  var_min<-min(var,na.rm=T)
  num_valid<-sum(!is.na(var))
  num_missing<-sum(is.na(var))
  df<-data.frame(var_median,var_max,var_min,num_valid,num_missing)
  return(df)
}