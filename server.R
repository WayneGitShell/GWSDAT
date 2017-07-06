#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

source("R/R_entry_point.R")

# Define server logic 
shinyServer(
  function(input, output) {
  
    #output$value <- renderPrint({ input$action })
    # output$value <- renderPrint({ input$action })
    output$text1 <- renderText({ "test test test" })
  
    observeEvent(input$run_gwsdat, {
      output$text1 <- renderText({ "clicked.." })
      R_entry_point()
      output$text1 <- renderText({ "Done action after click.." })
      # session$sendCustomMessage(type = 'testmessage', message = 'Thank you for clicking')
    })
    
})
