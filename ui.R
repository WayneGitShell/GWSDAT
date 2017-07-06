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
  titlePanel(h1("Shiny GWDAT")),
  
  actionButton("action", label = "Run"),
  
  sidebarLayout(
    sidebarPanel(h3("sidebar panel")),
    mainPanel(h3("main panel"))
  )
))
  