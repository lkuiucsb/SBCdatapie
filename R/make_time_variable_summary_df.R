#' make_time_variable_summary_df
#' 
#' This function writes a summary table of time (POSIX, Date) objects
#' 
#' @usage
#'   make_time_variable_summary_df(var)
#'
#' @param var
#'   Define this parameter.
#'
#' @return
#'   What does this function return?
#' 
#' @export
#'   (data frame) A summary table of time (POSIX, Date) objects.
#'

make_time_variable_summary_df<-function(var){
  var_type<-class(var)[1]
  num_times<-length(levels(as.factor(var)))
  num_valid<-sum(!is.na(var))
  num_missing<-sum(is.na(var))
  first_time <- min(var, na.rm = T)
  last_time <- max(var, na.rm = T)
  mode_time_step <- names(which.max(table(diff(unique(var[order(var)]), lag = 1))))
  #med_time_step <- median(diff(unique(var[order(var)]), lag = 1), na.rm = T)
  df<-data.frame(var_type,first_time,last_time,mode_time_step,num_times,num_valid,num_missing)
  return(df)
}