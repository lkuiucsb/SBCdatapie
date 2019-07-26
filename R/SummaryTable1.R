#' SummaryTable1
#' 
#' Describe what this function does
#' 
#' @usage
#'   SummaryTable1(data.df1)
#'
#' @param data.df1
#'   Define this parameter.
#'
#' @return
#'   What does this function return?
#'   
#' @import magrittr
#' 
#' @export
#'

SummaryTable1<-function(data.df1)  {
  SummaryTable_Numeric <- data.df1 %>% 
    dplyr::select_if(is.numeric) %>%
    tidyr::gather(key="column_name", value='value') %>%
    dplyr::group_by(column_name) %>%
    dplyr::summarize(min=min(value, na.rm=TRUE),
              quantile_25 = quantile(value, 0.25, na.rm=TRUE),
              quantile_50 = quantile(value, 0.50, na.rm=TRUE),
              quantile_75 = quantile(value, 0.75, na.rm=TRUE),
              max = max(value, na.rm=TRUE), 
              mean = mean(value, na.rm=TRUE),
              Unique_Values = length(unique(value)),
              Total_Values = sum(is.finite(value)),
              NAs= sum(is.na(value))
    )
  
  return(SummaryTable_Numeric)
}