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
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)


# Define UI for application that draws a histogram

b64 <- base64enc::dataURI(file="psychiSafe_logo.jpg", mime="image/png")


shinyUI(
  fluidPage(theme = shinytheme("united"),
            navbarPage(
              "psychiSafe™",
              id = "navbar",
              
              tabPanel(title = strong("Home"),
                       value = "Home",
                       fluidRow(
                         column(7,
                                img(src=b64, height = "70%", width = "70%", align = "center")
                         ),
                         
                         column(5, 
                                h3(strong("Enter Patient's Information to begin test")),
                                textInput("txt1", "Name:", ""),
                                selectInput("Sex", "Sex:",
                                            c("Male" = "Male",
                                              "Female" = "Female")
                                ),
                                dateInput("txt3", 
                                          "Date of Birth:",
                                          format = "dd-mm-yyyy", 
                                          startview = "year",
                                          weekstart = 0,
                                          language = "en"),
                                
                                selectInput("Race", "Race/Ethicity:",
                                            c("African" = "African",
                                              "European" = "European", 
                                              "East Asian" = "East Asian",
                                              "South Asian" = "South Asian",
                                              "Admixed American/Hispanic" = "Admixed American/Hispanic")
                                            ),
                                
                                selectInput("Prescribed", "Prescribed medication(s):",
                                            multiple = TRUE,
                                            c("Escitalopram (SSRI)" = "Escitalopram",
                                              "Nortriptyline (TCA)" = "Nortriptyline",
                                              "Paroxetine (SSRI)" = "Paroxetine",
                                              "Fluvoxamine (SSRI)" = "Fluvoxamine",
                                              "Milnacipram (SNRI)" = "Milnacipram",
                                              "Clozapine" = "Clozapine",
                                              "Olanzapine" = "Olanzapine")
                                            ),
                                
                                textInput("txt4", "Weight (in kg):", ""),
                                box(strong('Click on the ANTIDEPRESSANTS tab to continue test'))
                                
                                )
                         )
                       ),
            
              tabPanel(title = strong("ANTIDEPRESSANTS"), 
                       value = "depress",
                         fluidRow(
                           column(3,
                                  h4(strong('Suicidal ideation')), 
                                  tags$h5("rs11143230:"),
                                  selectInput("rs11143230_1", "",
                                              c("C" = "C",
                                                "T" = "T",
                                                "G" = "G",
                                                "A" = "A")
                                  ),
                                  selectInput("rs11143230_2", "",
                                              c("C" = "C",
                                                "T" = "T",
                                                "G" = "G",
                                                "A" = "A")
                                  ),
                                  
                                  tags$h5("rs358592:"),
                                  selectInput("rs358592_1", "",
                                              c("T" = "T",
                                                "C" = "C")
                                  ),
                                  selectInput("rs358592_2", "",
                                              c("T" = "T",
                                                "C" = "C")
                                  ),
                                  
                                  tags$h5("rs4732812:"),
                                  selectInput("rs4732812_1", "",
                                              c("C" = "C",
                                                "T" = "T")
                                  ),
                                  selectInput("rs4732812_2", "",
                                              c("C" = "C",
                                                "T" = "T")
                                  )
                           ),
                           
                           column(3,
                                  h4(strong('Sexual dysfunction')),
                                  tags$h5("rs1160351:"),
                                  selectInput("rs1160351_1", "",
                                              c("T" = "T",
                                                "A" = "A",
                                                "C" = "C")
                                  ),
                                  selectInput("rs1160351_2", "",
                                              c("T" = "T",
                                                "A" = "A",
                                                "C" = "C")
                                  ),
                                  tags$h5("rs225848:"),
                                  selectInput("rs225848_1", "",
                                              c("G" = "G",
                                                "A" = "A",
                                                "T" = "T")
                                  ),
                                  selectInput("rs225848_2", "",
                                              c("G" = "G",
                                                "A" = "A",
                                                "T" = "T")
                                  ),
                                  tags$h5("rs13436218:"),
                                  selectInput("rs13436218_1", "",
                                              c("C" = "C",
                                                "T" = "T",
                                                "G" = "G")
                                  ),
                                  selectInput("rs13436218_2", "",
                                              c("C" = "C",
                                                "T" = "T",
                                                "G" = "G")
                                  )
                           ),
                           
                           column(3,
                                  h4(strong('Sexual dysfunction')),
                                  tags$h5("rs6603109:"),
                                  selectInput("rs6603109_1", "",
                                              c("A" = "A",
                                                "C" = "C",
                                                "T" = "T")
                                  ),
                                  selectInput("rs6603109_2", "",
                                              c("A" = "A",
                                                "C" = "C",
                                                "T" = "T")
                                  ),
                                  tags$h5("rs857228:"),
                                  selectInput("rs857228_1", "",
                                              c("T" = "T",
                                                "A" = "A",
                                                "C" = "C")
                                  ),
                                  selectInput("rs857228_2", "",
                                              c("T" = "T",
                                                "A" = "A",
                                                "C" = "C")
                                  )
                           ),
                           column(3,
                                  h4(strong('.........................................')),
                                  box(h2(strong('Click on the ANTIPSYCHOTICS tab to continue test')))
                                  )
                         )
            ), # fluidPage
            
            tabPanel(title = strong("ANTIPSYCHOTICS"),
                     value = 'psych',
                       fluidRow(
                         column(2,
                                h4(strong('Agranulocytosis')),
                                tags$h5("rs3749448:"),
                                selectInput("rs3749448_1", "",
                                            c("A" = "A",
                                              "G" = "G",
                                              "T" = "T")
                                ),
                                selectInput("rs3749448_2", "",
                                            c("A" = "A",
                                              "G" = "G",
                                              "T" = "T")
                                ),
                                
                                tags$h5("rs1800625:"),
                                selectInput("rs1800625_1", "",
                                            c("G" = "G",
                                              "A" = "A")
                                ),
                                selectInput("rs1800625_2", "",
                                            c("G" = "G",
                                              "A" = "A")
                                )
                         ),
                         
                         column(2,
                                h4(strong('Weight gain')),
                                tags$h5("rs7720513:"),
                                selectInput("rs7720513_1", "",
                                            c("A" = "A",
                                              "C" = "C",
                                              "G" = "G")
                                ),
                                selectInput("rs7720513_2", "",
                                            c("A" = "A",
                                              "C" = "C",
                                              "G" = "G")
                                ),
                                tags$h5("rs117433199:"),
                                selectInput("rs117433199_1", "",
                                            c("G" = "G",
                                              "A" = "A",
                                              "C" = "C")
                                ),
                                selectInput("rs117433199_2", "",
                                            c("G" = "G",
                                              "A" = "A",
                                              "C" = "C")
                                ),
                                tags$h5("rs78129933:"),
                                selectInput("rs78129933_1", "",
                                            c("C" = "C",
                                              "G" = "G",
                                              "T" = "T")
                                ),
                                selectInput("rs78129933_2", "",
                                            c("C" = "C",
                                              "G" = "G",
                                              "T" = "T")
                                )
                                
                         ),
                         
                         column(2,
                                h4(strong('Weight gain')),
                                tags$h5("rs62097526:"),
                                selectInput("rs62097526_1", "",
                                            c("T" = "T",
                                              "G" = "G")
                                ),
                                selectInput("rs62097526_2", "",
                                            c("T" = "T",
                                              "G" = "G")
                                ),
                                
                                tags$h5("rs62344853:"),
                                selectInput("rs62344853_1", "",
                                            c("C" = "C",
                                              "A" = "A",
                                              "T" = "T")
                                ),
                                selectInput("rs62344853_2", "",
                                            c("C" = "C",
                                              "A" = "A",
                                              "T" = "T")
                                ),
                                
                                tags$h5("rs74820080:"),
                                selectInput("rs74820080_1", "",
                                            c("C" = "C",
                                              "T" = "T")
                                ),
                                selectInput("rs74820080_2", "",
                                            c("C" = "C",
                                              "T" = "T")
                                )
                         ),
                         
                         column(2,
                                h4(strong('Weight gain')),
                                tags$h5("rs7938982:"),
                                selectInput("rs7938982_1", "",
                                            c("C" = "C",
                                              "T" = "T")
                                ),
                                selectInput("rs7938982_2", "",
                                            c("C" = "C",
                                              "T" = "T")
                                ),
                                
                                tags$h5("rs191168:"),
                                selectInput("rs191168_1", "",
                                            c("T" = "T",
                                              "C" = "C",
                                              "G" = "G")
                                ),
                                selectInput("rs191168_2", "",
                                            c("T" = "T",
                                              "C" = "C",
                                              "G" = "G")
                                ),
                                
                                tags$h5("rs60232573:"),
                                selectInput("rs60232573_1", "",
                                            c("G" = "G",
                                              "A" = "A")
                                ),
                                selectInput("rs60232573_2", "",
                                            c("G" = "G",
                                              "A" = "A")
                                ),
                         ),
                         
                         column(2,
                                h4(strong('Myocarditis')),
                                tags$h5("rs2959223:"),
                                selectInput("rs2959223_1", "",
                                            c("A" = "A",
                                              "G" = "G")
                                ),
                                selectInput("rs2959223_2", "",
                                            c("A" = "A",
                                              "G" = "G")
                                ),
                                tags$h5("rs9463787:"),
                                selectInput("rs9463787_1", "",
                                            c("G" = "G",
                                              "A" = "A")
                                ),
                                selectInput("rs9463787_2", "",
                                            c("G" = "G",
                                              "A" = "A")
                                ),
                                tags$h5("rs117188076:"),
                                selectInput("rs117188076_1", "",
                                            c("T" = "T",
                                              "C" = "C")
                                ),
                                selectInput("rs117188076_2", "",
                                            c("T" = "T",
                                              "C" = "C")
                                )
                         ),
                         column(2,
                                h4(strong('Myocarditis')),
                                tags$h5("rs74675399:"),
                                selectInput("rs74675399_1", "",
                                            c("A" = "A",
                                              "G" = "G")
                                ),
                                selectInput("rs74675399_2", "",
                                            c("A" = "A",
                                              "G" = "G")
                                ),
                                h4(strong('.........................................')),
                                actionButton('submit', h3(strong("Click to run test"))),
                                box(h2(strong('Retrieve result on TEST REPORT tab')))
                         )
                       )
            ),
            tabPanel(strong("TEST REPORT"),
                     h1("       Prediction Report"),
                     verbatimTextOutput('report'),
                     downloadLink('downloadData', h1('Download test result'))
                     )
  )
  )
)