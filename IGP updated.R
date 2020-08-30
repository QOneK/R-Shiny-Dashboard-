## app.R ##

library (data.table)
library(shinydashboard)
library(DT)
library(ggplot2)

# popn <- read.csv(file.choose(),header=T)
# attach (popn)
# popn <- data.table(popn)

Raw <- read.csv(file="01_Template.csv", header = T) #Select the raw csv file.
Labelled <- read.csv(file="02_PIDTC6903CGD.csv", header = T) #Select the labelled csv file.
final_headers <- names(Raw) #Gets the names of the header from Raw.
names(Labelled) <- NULL
colnames(Labelled) <- final_headers
popn <- data.table(Labelled)
attach(popn)
names(popn) <- NULL
new_headers <- read.csv(file="03_Custom Labelled Variable Position.csv", header = T)
new_headers <- names(new_headers)
new_headers <- gsub("\\."," ",new_headers)
colnames(popn) <- new_headers

#BMI Module----------------------------------------------------------------------------------------------------------

#Libraries to Load for BMI Module
library(dplyr)
library(data.table)

#Libraries to Load for BMI Module

library(dplyr)

library(plyr)

library(data.table)



BMI_read <- read.csv(file="02_PIDTC6903CGD.csv",header=T)#Open the file and name it popn variable

attach (BMI_read) #takes all the header names and the header names become the column variable that can be called



table <- data.table(BMI_read) #takes all the cloumns of the table and converts to a data table



BMI_table <- data.table (participant_id,weight,height)#Takes height and weight columns and puts into variable bmi_table

setnames(BMI_table, old = "participant_id", new = "Participant ID")

BMI_table$BMI <- (BMI_table$weight/(BMI_table$height/100)^2) #Creates BMI column in bmi_table



#BMI categories (From bmi_table, the BMI conditions are organzied)

BMI_underweight <- filter(BMI_table, BMI <18.5)

BMI_normalweight <- filter(BMI_table, BMI >= 18.5 & BMI <= 24.9 )

BMI_overweight <- filter(BMI_table, BMI >= 25 & BMI <= 29.9)

BMI_obese <- filter(BMI_table, BMI >= 30)



BMI_table$Category[BMI_table$BMI<18.5] <- "Underweight"

BMI_table$Category[BMI_table$BMI>=18.5 & BMI_table$BMI <=24.9] <- "Normal Weight"

BMI_table$Category[BMI_table$BMI>= 25 & BMI_table$BMI <=29.9 ] <- "Overweight"

BMI_table$Category[BMI_table$BMI>=30] <- "Obese"

#---------------------------------------------------------------------------------------------------------------

ui <- dashboardPage(
  
  skin = "black", #this changes the skin of the RShiny
  
  #HEADER# 
  
  ########
  
  dashboardHeader(
    title = "CGD Prospective Cohort Day 100 HCT Post Treatment Worksheet", #This is the title
    titleWidth = 600,
    #This is the Top Right notification icons (This does not need modification at output)
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "From: SickKids Team",        
                   message = "Hi Dr. Grunebaum and Shani!",
                   icon("smile")
                 )#bracket to close "messageItem" 
    )#bracket to close "dropdownMenu"
  ),#bracket to close "dashboardHeader"
  
  
  #SIDEBAR#
  
  #########
  
  dashboardSidebar(
    width = 125,
    sidebarMenu( 
      menuItem("Datatable", tabName = "Datatable", icon=icon("table")),
      menuItem("BMI", tabName = "BMI", icon=icon("weight")),
      menuItem("Statistics", tabName = "Statistics", icon=icon("chart-bar")),
      menuItem("Help", tabName = "Help", icon=icon("info")),
      menuItem("Contact us",tabName="Contact",icon=icon("id-card"))
    )#closes sidebarMenu
  ),#closes "dashboardSidebar"
  
  
  
  #BODY#
  
  ######
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Datatable",
              # Boxes need to be put in a row (or column)
              fluidPage(
                mainPanel(
                  width= "150%", 
                  tabsetPanel(
                    id = 'dataset',
                    tabPanel("Patient information", DT::dataTableOutput("mytable0"),style = " height:700px; overflow-y: scroll;overflow-x: scroll;"),
                    tabPanel("Date of HCT Day 100 Visit", DT::dataTableOutput("mytable1"),style = " height:700px; overflow-y: scroll;overflow-x: scroll;"),
                    
                    tabPanel("Height, Weight, and Nutrition", DT::dataTableOutput("mytable2"),style = " height:700px; overflow-y: scroll;overflow-x: scroll;"),
                    
                    tabPanel("Medical History and Clinical Assessments: Part 1", DT::dataTableOutput("mytable3"),style = " height:700px; overflow-y: scroll;overflow-x: scroll;"),
                    
                    tabPanel("Medical History and Clinical Assessments: Part 2", DT::dataTableOutput("mytable4"),style = " height:700px; overflow-y: scroll;overflow-x: scroll;"),
                    
                    tabPanel("Medical History and Clinical Assessments: Part 3.1", DT::dataTableOutput("mytable5"),style = "height:700px; overflow-y: scroll;overflow-x: scroll;"),
                    
                    tabPanel("Medical History and Clinical Assessments: Part 3.2", DT::dataTableOutput("mytable6"),style = " height:700px; overflow-y: scroll;overflow-x: scroll;"),
                    
                    tabPanel("Medical History and Clinical Assessments: Part 3.3", DT::dataTableOutput("mytable7"),style = " height:700px; overflow-y: scroll;overflow-x: scroll;"),
                    
                    tabPanel("Medical History and Clinical Assessments: Part 4", DT::dataTableOutput("mytable8"),style = "height:700px; overflow-y: scroll;overflow-x: scroll;"),
                    
                    tabPanel("Malignancy", DT::dataTableOutput("mytable9"),style = "height:700px; overflow-y: scroll;overflow-x: scroll;"),
                    
                    tabPanel("Laboratory Assessments", DT::dataTableOutput("mytable10"),style = "height:700px; overflow-y: scroll;overflow-x: scroll;"),
                    
                    tabPanel("Infectious Disease Markers", DT::dataTableOutput("mytable11"),style = "height:700px; overflow-y: scroll;overflow-x: scroll;"),
                    
                    tabPanel("Oxidative Burst", DT::dataTableOutput("mytable12"),style = "height:700px; overflow-y: scroll;overflow-x: scroll;"),
                    
                    tabPanel("Chimerism", DT::dataTableOutput("mytable13"),style = "height:700px; overflow-y: scroll;overflow-x: scroll;"),
                    
                    tabPanel("Comments", DT::dataTableOutput("mytable14"),style = "height:700px; overflow-y: scroll;overflow-x: scroll;")
                  )
                )
              )
      ),
      
      
      
      #second tab item
      
      tabItem(tabName = "BMI",
              
              fluidPage(    
                
                titlePanel(title = "BMI Measurement"),
                  
                  mainPanel(
                    
                    tabsetPanel(
                      
                      type="tab",
                      
                      tabPanel("Data Table of BMI",DT::dataTableOutput("data")),
                      
                      tabPanel("Bar Graph of Distribution",plotOutput("mybar"))
                      
                    )
                  )
              )
      ),
      
      #third tab item           
      
      tabItem(tabName = "Statistics",
              
              
              
              fluidRow(
                
                
                
                box(
                  
                  title = h3("Correlation Analysis"),
                  
                  # Select variable for y-axis
                  
                  selectInput(inputId = "x", 
                              
                              label = "X-axis:",
                              
                              choices = c("BMI"   = "BMI",
                                          
                                          "Height" = "height", 
                                          
                                          "Weight" = "weight",
                                          
                                          "WBC" = "wbc",
                                          
                                          "Lymphocytes" = "lymphocytes",
                                          
                                          "Eosinophils" = "eosinophils",
                                          
                                          "Polymorphonuclear Leukocytes" = "polymorphonuclear_leukocytes",
                                          
                                          "Hemoglobin" = "hemoglobin",
                                          
                                          "Platelets" = "platelets",
                                          
                                          "Percent of Oxidase normal (donor) neutrophils present" = "dhr",
                                          
                                          "Percent Donor Cells" = "donor",
                                          
                                          "Percent Host Cells" = "host"
                                          
                              )
                              
                  ),
                  
                  # Select variable for x-axis
                  
                  selectInput(inputId = "y", 
                              
                              label = "Y-axis:",
                              
                              choices = c("BMI"   = "BMI",
                                          
                                          "Height" = "height", 
                                          
                                          "Weight" = "weight",
                                          
                                          "WBC" = "wbc",
                                          
                                          "Lymphocytes" = "lymphocytes",
                                          
                                          "Eosinophils" = "eosinophils",
                                          
                                          "Polymorphonuclear Leukocytes" = "polymorphonuclear_leukocytes",
                                          
                                          "Hemoglobin" = "hemoglobin",
                                          
                                          "Platelets" = "platelets",
                                          
                                          "Percent of Oxidase normal (donor) neutrophils present" = "dhr",
                                          
                                          "Percent Donor Cells" = "donor",
                                          
                                          "Percent Host Cells" = "host"
                                          
                              )
                              
                  ),
                  
                  plotOutput(outputId = "scatterplot", click = "plot_click"),
                  
                  verbatimTextOutput("info"),
                  
                  plotOutput(outputId = "densityplot", height = 200)
                  
                )
                
              )
              
      ),
      
      #forth tab item
      
      tabItem(tabName = "Help",
              
              
              
              h2("Help"),
              
              h3("Help text")
              
              
              
      ),
      
      
      
      #fifth tab item
      
      tabItem(tabName = "Contact",
              
              
              
              h1("Contact us:"),
              
              h4("Abubaker Mohamed:",a("amohamed138@myseneca.ca", href="mailto: amohamed138@myseneca.ca"),br(),
                 
                 "Kyuhwan Kim:", a("kkim123@myseneca.ca", href="mailto: kkim123@myseneca.ca"),br(),
                 
                 "Michael Lau:", a("tymlau@myseneca.ca", href="mailto: tymlau@myseneca.ca")
                 
                 
                 
              )
              
              
              
      )
      
    )#closes tabItems under "dashboardBody"
    
  )#closes "dashboardBody"
  
)#closes "dashboardPage"


#SERVER#

########

server <- function(input, output) {
  
  
  
  popn0=popn[,2:6]
  
  output$mytable0 <- DT::renderDataTable({
    
    DT::datatable(popn0,rownames = FALSE)
    
  })
  
  
  
  popn1=popn[,c(3,8)]
  
  output$mytable1 <- DT::renderDataTable({
    
    DT::datatable(popn1,rownames = FALSE)
    
  })
  
  
  
  popn2=popn[,c(3,10:19)]
  
  output$mytable2 <- DT::renderDataTable({
    
    DT::datatable(popn2,rownames = FALSE)
    
  })
  
  
  
  # customize the length drop-down menu; display 5 rows per page by default
  
  popn3=popn[,c(3,21:79)]
  
  output$mytable3 <- DT::renderDataTable({
    
    DT::datatable(popn3,rownames = FALSE)
    
  })
  
  
  
  popn4=popn[,c(3,81:145)]
  
  output$mytable4 <- DT::renderDataTable({
    
    DT::datatable(popn4,rownames = FALSE)
    
  })
  
  
  
  popn5=popn[,c(3,147:157)]
  
  output$mytable5 <- DT::renderDataTable({
    
    DT::datatable(popn5,rownames = FALSE)
    
  })
  
  
  
  popn6=popn[,c(3,159:169)]
  
  output$mytable6 <- DT::renderDataTable({
    
    DT::datatable(popn6,rownames = FALSE)
    
  })
  
  
  
  popn7=popn[,c(3,171:181)]
  
  output$mytable7 <- DT::renderDataTable({
    
    DT::datatable(popn7,rownames = FALSE)
    
  })
  
  
  
  popn8=popn[,c(3,183:285)]
  
  output$mytable8 <- DT::renderDataTable({
    
    DT::datatable(popn8,rownames = FALSE)
    
  })
  
  
  
  popn9=popn[,c(3,287:323)]
  
  output$mytable9 <- DT::renderDataTable({
    
    DT::datatable(popn9,rownames = FALSE)
    
  })
  
  
  
  popn10=popn[,c(3,325:406)]
  
  output$mytable10 <- DT::renderDataTable({
    
    DT::datatable(popn10,rownames = FALSE)
    
  })
  
  
  
  popn11=popn[,c(3,408:441)]
  
  output$mytable11 <- DT::renderDataTable({
    
    DT::datatable(popn11,rownames = FALSE)
    
  })
  
  
  
  popn12=popn[,c(3,443:444)]
  
  output$mytable12 <- DT::renderDataTable({
    
    DT::datatable(popn12,rownames = FALSE)
    
  })
  
  
  
  popn13=popn[,c(3,446:455)]
  
  output$mytable13 <- DT::renderDataTable({
    
    DT::datatable(popn13,rownames = FALSE)
    
  })
  
  
  
  popn14=popn[,c(3,457)]
  
  output$mytable14 <- DT::renderDataTable({
    
    DT::datatable(popn14,rownames = FALSE)
    
  })
  
  
  
  output$data <- DT::renderDataTable({
    
    DT::datatable(BMI_table,rownames = FALSE)
    
  })
  
  output$mybar <- renderPlot({
    
    #colm <- as.numeric(input$checkGroup)
    
    counts <- c(length(BMI_underweight$BMI), length(BMI_normalweight$BMI), length(BMI_overweight$BMI), length(BMI_obese$BMI))
    
    names(counts) <- c("Underweight", "Normal Weight", "Overweight", "Obese")
    
    barplot(counts, col=c("dodgerblue1","dodgerblue2","dodgerblue3","dodgerblue4"), ylab = "Number of Participants", main = "Distribution of BMI Across Participants", xlab = "BMI Class" )
    
  })
  
  #Output for Statistics 
  # Create the scatterplot object the plotOutput function is expecting
  
  output$scatterplot <- renderPlot({
    
    ggplot(data = BMI_table, aes_string(x = input$x, y = input$y)) +
      
      geom_point(aes(colour=Category))
    
  })
  #mouse pointer
  output$info <- renderText({
    
    paste0("x= ", input$plot_click$x, "\ny=", input$plot_click$y)
    
  })
  
  
  # Create densityplot
  
  output$densityplot <- renderPlot({
    
    ggplot(data = BMI_table, aes_string(x = input$x)) +
      
      geom_density()
    
  })
  
  
}#closes Server

shinyApp(ui = ui, server = server)