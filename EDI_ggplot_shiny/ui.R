#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
library(magrittr)
library(stringr)
#library(tidyverse)

source('EDI_header.R')
source('EDI_leftPanel.R')
source('EDI_mainPanel.R')
source('EDI_rightPanel.R')
source('read_data_archived.R')

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  EDI_header(),
  
  EDI_leftPanel(),
  
  ##Tabs
  EDI_mainPanel(),
  
  ##
  EDI_rightPanel()
))
