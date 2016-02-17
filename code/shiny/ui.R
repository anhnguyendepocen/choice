library(shiny)


shinyUI(fluidPage(
  titlePanel("Utilization Models"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Model Utilization With Various Surveys"),
      
      selectInput('survey',
                  label='choose dataset to work with',
                  choices=list('Nigeria LSS 2010'),
                  selected='Nigeria LSS 2010'),
      
      selectInput('alternativevar',
                  label='choose nominal outcome (choice alternatives)',
                  choices=list('Util','Facility'),
                  selected='Util')),
      
    mainPanel(
      textOutput('text1')
    )
  )
  
  
  
))