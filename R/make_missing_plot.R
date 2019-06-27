#' make_missing_plot
#' 
#' Describe what this function does
#' 
#' @usage
#'   make_missing_plot(var)
#'
#' @param var
#'   Define this parameter.
#'
#' @return
#'   What does this function return?
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