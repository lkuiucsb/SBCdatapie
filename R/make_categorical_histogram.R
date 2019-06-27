#' make_categorical_histogram
#' 
#' Creates a histogram of the frequency with which different codes occur for categorical variables. 
#' 
#' @usage
#'   make_categorical_histogram(df, var, varname)
#'
#' @param df
#'   A dataframe
#' @param var
#'   The col.name drawn from df of the vector to be graphed 
#' @param varname
#'   A string containing the name of the column to be graphed. 
#'   This is used only for labeling the graph, so alternative 
#'   wording can be used if desired. 
#'
#' @return
#'   A ggplot2 object containing the histogram for that column
#' 
#' @export
#'

make_categorical_histogram<-function(df,var,varname){
  library(ggplot2)
  #varname=deparse(substitute(var))
  x<-ggplot(df,aes(x=var))+ xlab(varname)+
    geom_bar() + theme(aspect.ratio = 1/3)
  return(x)
}