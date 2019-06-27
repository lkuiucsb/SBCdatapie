#' make_missing_plot
#' 
#' Creates a plot that shows where missing values occur in the data. 
#' The plot appears as a bar with vertical lines representing valid or NA observations for a single variable. 
#' 
#' @usage
#'   make_missing_plot(var)
#'
#' @param var
#'   Name of the vector whose missing values are to be graphed
#'
#' @return
#'   A ggplot2 graph object
#' 
#' @export
#'

make_missing_plot<-function(var){
  is_missing<-ifelse(is.na(var),"Missing","Valid")
  obs_number<-as.numeric(rownames(as.data.frame(is_missing)))
  allones=1
  is_missing_df<-data.frame(obs_number,is_missing,allones)
  x<-ggplot(is_missing_df,aes(obs_number,allones,fill=is_missing))+geom_col(width=1)+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_blank(),
          aspect.ratio = 1/3)+
    ylab("")+
    scale_fill_manual("legend", values = c("Missing" = "black", "Valid" = "lightgreen"))
  return(x)
}