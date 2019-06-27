#' SummaryTable2
#' 
#' Describe what this function does
#' 
#' @usage
#'   SummaryTable2(data.df3)
#'
#' @param data.df3
#'   Define this parameter.
#'
#' @return
#'   What does this function return?
#' 
#' @export
#'

SummaryTable2<-function(data.df3)  {
  SummaryTable_Categorical_Individual <- data.df3 %>% 
    select_if(~!is.numeric(.) & length(unique(.)) < 50) %>%
    gather(key="column_name", value="level") %>%
    group_by(column_name) %>%
    summarize(Total_Values = length(level),
              n_Categories = length(unique(level)),
              missingCount = sum(is.na(level))
    )
  return(SummaryTable_Categorical_Individual)
}