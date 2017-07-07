#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

source("R/GWSDAT_Setup.R")



# Define server logic 
shinyServer(
  function(input, output) {
  
    #output$value <- renderPrint({ input$action })
    # output$value <- renderPrint({ input$action })
    output$text1 <- renderText({ "test test test" })
  
    observeEvent(input$run_gwsdat, {
      output$text1 <- renderText({ "clicked.." })
      
      GWSDAT_Options = GWSDAT_Setup()
      
      ret = GWSDAT_Init(GWSDAT_Options)
      
      ## Get return status and display on page.
      if(class(ret$status) == "GWSDAT_Error")
        output$errors <- renderText({ ret$status$msg })
      if(class(ret$status) == "GWSDAT_Warning")
        output$warnings <- renderText({ ret$status$msg })
      
      if(class(ret$status) == "GWSDAT_OK") {
        output$errors <- renderText({ "No error for GWSDAT_Run_shiny()." })
        output$warnings <- renderText({ "No warnings for GWSDAT_Run_shiny()." })
      }
      
      ##
      ## Do the plotting 
      ##
      #plot.GWSDAT.Data(ret$Curr_Site_Data)
      
      # session$sendCustomMessage(type = 'testmessage', message = 'Thank you for clicking')
    
      })
    
})
