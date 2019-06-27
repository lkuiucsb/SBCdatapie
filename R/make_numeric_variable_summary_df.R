#' make_numeric_variable_summary_df
#' 
#' Creates a dataframe containing summary (median,max,min,num_valid,num_missing) information for numerical variables. 
#' 
#' @usage
#'   make_numeric_variable_summary_df(var)
#'
#' @param var
#'   Name of the vector containing the numbers to be summarized
#'
#' @return
#'   A dataframe containing the summary information. 
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