#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define server logic 
shinyServer(function(input, output, session) {
  
  ## render UI for biodata
  output$txt1 <- renderUI({
    textInput("Name", "Name:", "Corede Bala")
    
  })
  
  output$txt2 <- renderUI({
    selectInput("Sex", "Sex:",
                c("Male" = "Male",
                  "Female" = "Female")
    )
  })
  
  output$txt3 <- renderUI({
    textInput("Age", "Age (in years):", "45")
  })
  
  output$txt4 <- renderUI({
    selectInput("Race", "Race/Ethicity:",
                c("African" = "African",
                  "European" = "European", 
                  "East Asian" = "East Asian",
                  "South Asian" = "South Asian",
                  "Admixed American/Hispanic" = "Admixed American/Hispanic")
    )
  })
  
  
  output$txt5 <- renderUI({
    selectInput("Prescribed", "Prescribed medication(s):",
                selected = c("Nortriptyline", "Clozapine"),
                multiple = T,
                c("Escitalopram (SSRI)" = "Escitalopram",
                  "Nortriptyline (TCA)" = "Nortriptyline",
                  "Paroxetine (SSRI)" = "Paroxetine",
                  "Fluvoxamine (SSRI)" = "Fluvoxamine",
                  "Milnacipram (SNRI)" = "Milnacipram",
                  "Clozapine" = "Clozapine",
                  "Olanzapine" = "Olanzapine")
    )
  })
  
  

  
  output$txt6 <- renderUI({
    textInput("Weight", "Weight (in kg):", "65")
  })
  
  ## RISK OF SUICIDE FROM ESCITALOPRAM
  
  suicideEscitalopram <- reactive({
    
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
    
    
    
    OR <- exp(log(or1) + log(or2) + log(or3) + log(or4) 
              + log(or5) + log(or6))
    
    odds_in_pop <- 244/462
    
    prob_adr <- OR*odds_in_pop/(1 + (OR*odds_in_pop))
    
    prob_adr_percent <- prob_adr*100
      
    return (paste(round(prob_adr_percent, digits = 0)))
    
  })
  
  
  ## RISK OF SUICIDE FROM NORTRIPTYLINE
  
  suicideNortriptyline <- reactive({
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
    
    
    OR <- exp(log(or1) + log(or2))
    
    odds_in_pop <- 244/462
    
    prob_adr <- OR*odds_in_pop/(1 + (OR*odds_in_pop))
    
    prob_adr_percent <- prob_adr*100
    
    return (paste(round(prob_adr_percent, digits = 0)))
    
  })
  
  
  
  ## RISK OF SEXUAL DYSFUNCTION FROM SNRI (Milnacipran)/
  ## SSRI (Fluvoxamine;Paroxetine)
  
  sexDysfunction <- reactive({
    if(input$rs1160351_1 == 'T'){
      or1 <- 2.92
    } else {
      or1 <- 1
    }
    
    if(input$rs1160351_2 == 'T'){
      or2 <- 2.92
    } else {
      or2 <- 1
    }
    
    if(input$rs225848_1 == 'G'){
      or3 <- 4.42
    } else {
      or3 <- 1
    }
    
    if(input$rs225848_2 == 'G'){
      or4 <- 4.42
    } else {
      or4 <- 1
    }
    
    if(input$rs13436218_1 == 'C'){
      or5 <- 5.92
    } else {
      or5 <- 1
    }
    
    if(input$rs13436218_2 == 'C'){
      or6 <- 5.92
    } else {
      or6 <- 1
    }
    
    if(input$rs6603109_1 == 'A'){
      or7 <- 5.13
    } else {
      or7 <- 1
    }
    
    if(input$rs6603109_2 == 'A'){
      or8 <- 5.13
    } else {
      or8 <- 1
    }
    
    if(input$rs857228_1 == 'T'){
      or9 <- 2.56
    } else {
      or9 <- 1
    }
    
    if(input$rs857228_2 == 'T'){
      or10 <- 2.56
    } else {
      or10 <- 1
    }
    
    OR <- exp(log(or1) + log(or2) + log(or3) + log(or4) + log(or5) 
              + log(or6) + log(or7) + log(or8) + log(or9) + log(or10))
    
    odds_in_pop <- 36/165
        
    prob_adr <- OR*odds_in_pop/(1 + (OR*odds_in_pop))
    
    prob_adr_percent <- prob_adr*100
    
    return (paste(round(prob_adr_percent, digits = 0)))
    
  })

  
  
  
  
  
  
  ## RISK OF CLOZAPINE-INDUCED AGRANULOCYTOSIS
  
  agranulocytosis <- reactive({
    if(input$rs3749448_1 == 'A'){
      or1 <- 2.96
    } else {
      or1 <- 1
    }
    
    if(input$rs3749448_2 == 'A'){
      or2 <- 2.96
    } else {
      or2 <- 1
    }
    
    if(input$rs1800625_1 == 'G'){
      or3 <- 3.78
    } else {
      or3 <- 1
    }
    
    if(input$rs1800625_2 == 'G'){
      or4 <- 3.78
    } else {
      or4 <- 1
    }
    
    OR <- exp(log(or1) + log(or2) + log(or3) + log(or4))
    
    odds_in_pop <- 23/29
    
    
        
    prob_adr <- OR*odds_in_pop/(1 + (OR*odds_in_pop))
    
    prob_adr_percent <- prob_adr*100
    
    return (paste(round(prob_adr_percent, digits = 0)))
    
  })
  
  
  ## RISK OF CLOZAPINE-INDUCED MYOCARDITIS
  
  myocarditis <- reactive({
    if(input$rs2959223_1 == 'A'){
      or1 <- 5.55
    } else {
      or1 <- 1
    }
    
    if(input$rs2959223_2 == 'A'){
      or2 <- 5.55
    } else {
      or2 <- 1
    }
    
    if(input$rs9463787_1 == 'G'){
      or3 <- 7.67
    } else {
      or3 <- 1
    }
    
    if(input$rs9463787_2 == 'G'){
      or4 <- 7.67
    } else {
      or4 <- 1
    }
    
    if(input$rs117188076_1 == 'T'){
      or5 <- 13.74
    } else {
      or5 <- 1
    }
    
    if(input$rs117188076_2 == 'T'){
      or6 <- 13.74
    } else {
      or6 <- 1
    }
    
    if(input$rs74675399_1 == 'A'){
      or7 <- 6.36
    } else {
      or7 <- 1
    }
    
    if(input$rs74675399_2 == 'A'){
      or8 <- 6.36
    } else {
      or8 <- 1
    }
    
    OR <- exp(log(or1) + log(or2) + log(or3) + log(or4) 
              + log(or5) + log(or6) + log(or7) + log(or8))
    
    
    odds_in_pop <- 33/62
    
        
    prob_adr <- OR*odds_in_pop/(1 + (OR*odds_in_pop))

        
    prob_adr_percent <- prob_adr*100
    
    return (paste(round(prob_adr_percent, digits = 0)))
    
  })
  
  
  
  
  
  ## RISK OF ANTIPSYCHOTIC-INDUCED WEIGHT GAIN
  
  weightGain <- reactive({
    
    if(input$rs7720513_1 == 'A'){
      or1 <- 0.406
    } else {
      or1 <- 0
    }
    
    if(input$rs7720513_2 == 'A'){
      or2 <- 0.406
    } else {
      or2 <- 0
    }
    
    if(input$rs117433199_1 == 'G'){
      or3 <- 0.397
    } else {
      or3 <- 0
    }
    
    if(input$rs117433199_2 == 'G'){
      or4 <- 0.397
    } else {
      or4 <- 0
    }
    
    if(input$rs78129933_1 == 'C'){
      or5 <- 0.398
    } else {
      or5 <- 0
    }
    
    if(input$rs78129933_2 == 'C'){
      or6 <- 0.398
    } else {
      or6 <- 0
    }
    
    if(input$rs62097526_1 == 'T'){
      or7 <- 0.387
    } else {
      or7 <- 0
    }
    
    if(input$rs62097526_2 == 'T'){
      or8 <- 0.387
    } else {
      or8 <- 0
    }
    
    if(input$rs62344853_1 == 'C'){
      or9 <- -0.397
    } else {
      or9 <- 0
    }
    
    if(input$rs62344853_2 == 'C'){
      or10 <- -0.397
    } else {
      or10 <- 0
    }
    
    if(input$rs74820080_1 == 'C'){
      or11 <- 0.383
    } else {
      or11 <- 0
    }
    
    if(input$rs74820080_2 == 'C'){
      or12 <- 0.383
    } else {
      or12 <- 0
    }
    
    if(input$rs7938982_1 == 'C'){
      or13 <- 0.381
    } else {
      or13 <- 0
    }
    
    if(input$rs7938982_2 == 'C'){
      or14 <- 0.381
    } else {
      or14 <- 0
    }
    
    if(input$rs191168_1 == 'T'){
      or15 <- 0.39
    } else {
      or15 <- 0
    }
    
    if(input$rs191168_2 == 'T'){
      or16 <- 0.39
    } else {
      or16 <- 0
    }
    
    if(input$rs60232573_1 == 'G'){
      or17 <- 0.3788
    } else {
      or17 <- 0
    }
    
    if(input$rs60232573_2 == 'G'){
      or18 <- 0.3788
    } else {
      or18 <- 0
    }
    
    weight_change <- (or1 + or2 + or3 + or4 + or5 + or6 + or7 
                      + or8 + or9 + or10 + or11 + or12 + or13 
                      + or14 + or15 + or16 + or17 + or18)
    

    weight <- as.numeric(input$Weight)
    
        
    new_weight <- weight + weight_change
    
    return (paste(round(new_weight, digits = 1)))
    
  })
  
  

  
  output$presText <- renderText({
    if(input$submit>0) {
      paste(h1(strong('ORIGINAL PRESCRIPTION: ')))
    }
  })
  
  
  output$message <- renderText({
    if(input$submit>0) {
      paste(h2(strong('What is the risk profile of the prescribed drug(s)?')), 
            h2(strong('Can you find suitable alternatives with lower risk profiles?')))
    }
  })
  
  output$drugChoice <- renderText({
    if(input$submit>0) {
      paste(input$Prescribed)
    }
  })
  
  
  output$yourReport <- renderUI ({
    if(input$submit>0) {
      HTML(paste(h2(strong("RESULTS")),
                 paste(strong('NAME: '), input$Name), 
                 paste(strong('SEX: '), input$Sex), 
                 paste(strong('AGE: '), input$Age, ' years', sep = ''), 
                 paste(strong('RACE: '), input$Race), 
                 paste(strong('WEIGHT: '), input$Weight, 'kg', sep = ''),
                 h5(strong('')),
                 strong('ADVERSE DRUG REACTION RISK PROFILE'),
                 paste(h5('Risk of increasing suicidal ideation from Nortriptyline: '), 
                       strong(suicideNortriptyline()), 
                       strong('%'), 
                       sep = ''), 
                 paste(h5('Risk of increasing suicidal ideation from Escitalopram: '), 
                       strong(suicideEscitalopram()), 
                       strong('%'), 
                       sep = ''),
                 paste(h5('Risk of sexual dysfunction from SSRI/SNRI antidepressants: '), 
                       strong(sexDysfunction()), 
                       strong('%'), 
                       sep = ''), 
                 paste(h5('Risk of clozapine-induced agranulocytosis: '), 
                       strong(agranulocytosis()), 
                       strong('%'), 
                       sep = ''), 
                 paste(h5('Risk of clozapine-induced myocarditis: '), 
                       strong(myocarditis()), 
                       strong('%'), 
                       sep = ''),
                 paste(h5('Predicted weight if placed on clozapine or olanzapine: '), 
                       strong(weightGain()), 
                       strong('kg'), 
                       sep = ''),
                 sep = '<br/>'))
      }
    }
    )
  
  
  
  output$download <- renderUI ({
    if(input$submit>0) {
      actionButton('downloadData', h2(strong('DOWNLOAD TEST RESULTS')))
    }
  }
  )
  
  
  
  
  
  
  #observeEvent(input$Test, {
   # updateNavbarPage(session,
    #                 inputId = "navbar",
     #                selected = "depress")
    
  #})
  
  
  
})