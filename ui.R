#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)


# Define UI for application that draws a histogram
shinyUI(
  fluidPage(theme = shinytheme("united"),
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
)
