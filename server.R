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
  
    output$value <- renderPrint({ 
      input$action
      R_entry_point()
    })
  
})
