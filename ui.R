#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel(h2("Shiny GWDAT")),
  
  
  sidebarLayout(
    sidebarPanel(
      helpText("Execute the GWSDAT R script with the basic example."),
      actionButton("run_gwsdat", "Run"),
      h4("Status:"),
      textOutput("status")
      ),
    
    mainPanel(
      plotOutput("time_series")
      )
  )
))
  