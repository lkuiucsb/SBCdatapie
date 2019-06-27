#' make_numeric_histogram
#' 
#' Creates a histogram with density plot for numeric vectors
#' 
#' @usage
#'   make_numeric_histogram(df, var, varname)
#'
#' @param df
#'   A dataframe 
#' @param var
#'   A numeric column contained in df that will be graphed
#' @param varname
#'   A string containing the name of the column to be graphed. 
#'   This is used only for labeling the graph, so alternative 
#'   wording can be used, if desired. 
#'
#' @return
#'   A ggplot2 object containing the histogram and density plot for that column
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