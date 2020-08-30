## app.R ##
library (data.table)
library(shinydashboard)
library(DT)
library(ggplot2)


#BMI Module----------------------------------------------------------------------------------------------------------
#Libraries to Load for BMI Module
library(dplyr)
library(plyr)
library(data.table)

#---------------------------------------------------------------------------------------------------------------

ui <- dashboardPage(
  #HEADER# 
  ########
  dashboardHeader(
    title = "CGD Prospective Cohort Day 100 HCT Post Treatment Worksheet", #This is the title
    titleWidth = 600
  ),#bracket to close "dashboardHeader"
  
  #SIDEBAR#
  #########
  dashboardSidebar(
    width = 125,
    
    
    sidebarMenu( 
      menuItem("Help", tabName = "Help", icon=icon("question-circle"))
    )#closes sidebarMenu
    
  ),#closes "dashboardSidebar"
  
  #BODY#
  ######
  dashboardBody(
    tabItems(
      
      
      
      #second tab item
      tabItem(tabName = "Help",
              
              fluidRow(

              )
              
              
              
      )
    )
  )
)


#SERVER#
########
server <- function(input, output) {

  
}#closes Server
shinyApp(ui = ui, server = server)