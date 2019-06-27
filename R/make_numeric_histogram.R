#' make_numeric_histogram
#' 
#' Describe what this function does
#' 
#' @usage
#'   make_numeric_histogram(df, var, varname)
#'
#' @param df
#'   Define this parameter.
#' @param var
#'   Define this parameter.
#' @param varname
#'   Define this parameter.
#'
#' @return
#'   What does this function return?
#' 
#' @export
#'

make_numeric_histogram<-function(df,var,varname){
  library(ggplot2)
  #varname=deparse(substitute(var))
  x<-ggplot(df,aes(x=var))+ xlab(varname)+
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") + theme(aspect.ratio = 1/3)
  return(x)
}