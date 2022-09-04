#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
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
})
