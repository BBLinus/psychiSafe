#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)

# Define UI for application that draws a histogram

ui <- fluidPage(theme = shinytheme("united"),
                navbarPage(
                  theme = "cerulean",  # <--- To use a theme, uncomment this
                  "psychiSafe",
                  tabPanel("Home", "This panel is intentionally left blank"),
                  tabPanel("About", "This panel is intentionally left blank"),
                  tabPanel("Antidepressants",
                           sidebarPanel(
                             tags$h4("Patient's Information"),
                             textInput("txt1", "Patient's Name:", ""),
                             dateInput("txt1", 
                                       "Patient's Date of birth:", 
                                       format = "dd-mm-yyyy", 
                                       startview = "year",
                                       weekstart = 0,
                                       language = "en"),
                        
                             tags$h4("rs11143230:"),
                             selectInput("rs11143230_1", "",
                                         c("T" = "T",
                                           "C" = "C")
                                         ),
                             selectInput("rs11143230_2", "",
                                         c("T" = "T",
                                           "C" = "C")
                                         ),
                             
                             tags$h4("rs358592:"),
                             selectInput("rs358592_1", "",
                                         c("T" = "T",
                                           "C" = "C")
                             ),
                             selectInput("rs358592_2", "",
                                         c("T" = "T",
                                           "C" = "C")
                             ),
                             
                             tags$h4("rs4732812:"),
                             selectInput("rs4732812_1", "",
                                         c("T" = "T",
                                           "C" = "C")
                             ),
                             selectInput("rs4732812_2", "",
                                         c("T" = "T",
                                           "C" = "C")
                             ),
                             
                             actionButton('submitbutton', 'Submit')
                           ), # sidebarPanel
                           
                           
                           mainPanel(
                             h1("Prediction Report"),
                             h4("Output 1"),
                             verbatimTextOutput("txtout")
                             
                           )# mainPanel
                           
                  ) # Navbar 1, tabPanel
                  
                ) # navbarPage
) # fluidPage



# Define server function  
server <- function(input, output) {
  
  
  datasetInput <- reactive({
    odds_in_pop <- 244/462
    if(input$rs11143230_1 == 'C'){
      or1 <- 2.22
    } else {
      or1 <- 1
    }
  
    if(input$rs11143230_2 == 'C'){
      or2 <- 2.22
    } else {
      or2 <- 1
    }
  
    if(input$rs358592_1 == 'T'){
      or3 <- 2.56
    } else {
      or3 <- 1
    }
  
    if(input$rs358592_2 == 'T'){
      or4 <- 2.56
    } else {
      or4 <- 1
    }
  
    if(input$rs4732812_1 == 'C'){
      or5 <- 2.56
    } else {
      or5 <- 1
    }
  
    if(input$rs4732812_2 == 'C'){
      or6 <- 2.56
    } else {
      or6 <- 1
    }
  
  
    OR <- or1*or2*or3*or4*or5*or6
  
  
    prob_suicide_antidepressant <- OR*odds_in_pop/(1 + (OR*odds_in_pop))
  
    percent <- prob_suicide_antidepressant*100
  
    return (paste(percent))
    
  })
  
  
  
  
  
  
  
  
  output$txtout <- renderText({
    if(input$submitbutton>0) {
      isolate(datasetInput())
    }
  })
} # server



# Create Shiny object
shinyApp(ui = ui, server = server)