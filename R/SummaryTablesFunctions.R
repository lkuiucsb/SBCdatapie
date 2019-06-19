#--------------------------------------------------------

#Function for 
#Table to display numerical summary information..

#This code was composed by Jocelyn Wardrup with the appreciated help of
#Kathe Todd-Brown, PhD.
#Review, comments provided by John Porter, PhD, and An T. Nguyen, PhD,

#Code created for Hackathon June 11-13, 2019, Albuquerque, New Mexico. 

#Goal of the project for the Hackathon is to create a GUI interface to allow for 
#the user to have an interactive or static experience with the variety of the data available. 
#The user chooses which dataset to use and the interface assists them with bringing in the
#data and displaying which variables.

#Part of this project involved two part, a static report and an interactive experience. These
#are geared toward separate users, level of data interest or processing. 

#These codes are for the beginning of the static report. In the very beginning of the static report
#a summary table of the data is displayed. Graphs of data and NA distribution charts follow and were 
#composed by John Porter, PhD. 

#code edited (function names revised, smalled summary table put as table #2), comments here and above added
#to describe contents. Uploaded to Hackathon gui GitHub on 6.18.2019. 

#--------------------------------------------------------

#Install library tidyverse
library(tidyverse)

#Initial summary 

SummaryTable1<-function(data.df1)  {
  SummaryTable_Numeric <- data.df1 %>% 
    select_if(is.numeric) %>%
    gather(key="column_name", value='value') %>%
    group_by(column_name) %>%
    summarize(min=min(value, na.rm=TRUE),
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

#Function for 
#Table to display categorical summary information....

SummaryTable3<-function(data.df2)  {
  SummaryTable_Categorical <- data.df2 %>% 
    select_if(~!is.numeric(.) & length(unique(.)) < 50) %>%
    gather(key="column_name", value='level') %>%
    group_by(column_name, level) %>%
    tally %>%
    ungroup() %>% group_by(column_name) %>%
    mutate(n_MainCategories = length(level),
           missingCount = ifelse(any(is.na(level)), n[is.na(level)], 0)
    )
  return(SummaryTable_Categorical)
}

#End
