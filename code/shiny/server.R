library(shiny)

source('../model.r')
data<-read.csv("J:/Project/phc/nga/choice/data/1f_fully_prepped.csv")


# Define server logic required to draw a histogram
shinyServer(
  function(input, output) {

    output$text1 <- renderText({ 
      sprintf("You have selected to work with %s, 
              using %s as your nominal outcome variable", 
              input$survey, input$alternativevar)
    })

    
    
    # run the model
    mnl.model(d=mnl.data.format(dat=data,
                                       id.variable='pid',
                                       choiceindicator='chosen',
                                       alternatives='facility'),
                     alternatives='alts',
                     id.variable='pid',
                     choiceindicator='chosen',
                     coefficients=c('male','age_gr'),
                     dummycoeffs=c('max_hh_edu'))
    
    
})

# run in one go. Fullly packed package.


