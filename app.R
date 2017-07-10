

library(shiny)

source("R/GWSDAT_Setup.R")

GWSDAT_Options = GWSDAT_Setup()


ret = GWSDAT_Init(GWSDAT_Options)

## Get return status and display on page.
#if(class(ret$status) == "GWSDAT_Error")
#  output$errors <- renderText({ ret$status$msg })
#if(class(ret$status) == "GWSDAT_Warning")
#  output$warnings <- renderText({ ret$status$msg })

#if(class(ret$status) == "GWSDAT_OK") {
#  output$errors <- renderText({ "No error for GWSDAT_Run_shiny()." })
#  output$warnings <- renderText({ "No warnings for GWSDAT_Run_shiny()." })
#}



# Define server logic 
server <- function(input, output) {
    
    output$status <- renderText({ input$solute_select })
    
    #
    # Plot main window
    #
    output$time_series <- renderPlot({
      
      
      # Update control attributes from reactive variables. 
      #if(!is.null(input$Cont.rg))     ret$Curr_Site_Data$Cont.rg <- input$solute_select
      ret$Curr_Site_Data$Cont.rg <- input$solute_select
      #if(!is.null(input$solute_conc)) {
      ret$Curr_Site_Data$rgUnits <- input$solute_conc
      
      # Set all values to FALSE and then set activated elements to TRUE.
      ret$Curr_Site_Data$ts_options[1:length(ret$Curr_Site_Data$ts_options)] = FALSE
      ret$Curr_Site_Data$ts_options[input$ts_true_options] = TRUE
         
      ret$Curr_Site_Data$Well <- input$well_select
      
     
      Plot_SmoothTimeSeries(ret$Curr_Site_Data)
      
    })
    
    #observeEvent(input$solute_select, {
    #  browser()
    #  ret$Curr_Site_Data$Cont.rg <- input$solute_select
    #})
    
    # NOT USED RIGHT NOW: EXECUTES WHEN THE RUN BUTTON IS CLICKED.
    #observeEvent(input$run_gwsdat, {
      
      #output$status <- renderText({ "clicked.." })
      
      ##
      ## Do the plotting 
      ##
      #GWSDAT.Make.Panel(ret$Curr_Site_Data)
      # session$sendCustomMessage(type = 'testmessage', message = 'Thank you for clicking')
      
    #})
    
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(h2("Shiny GWDAT")),
  
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("solute_select", label = "Solute", choices = names(ret$Curr_Site_Data$Fitted.Data),
                  selected = ret$Curr_Site_Data$Cont.rg),
      #hr(),
      radioButtons("solute_conc", label = "Solute Conc. Unit",
                   choices = list("ng/l","ug/l","mg/l"), 
                   selected = ret$Curr_Site_Data$rgUnits),
      
      checkboxGroupInput("ts_true_options", label = "Time Series Plot Options", 
                         choices = names(ret$Curr_Site_Data$ts_options),
                         selected = names(which(ret$Curr_Site_Data$ts_options == TRUE))),
      # if enabled also include this Option: "Overlay NAPL Thickness"
      
      selectInput("well_select", label = "Select Monitoring Well", choices = sort(as.character(ret$Curr_Site_Data$All.Data$All.Wells)),
                  selected = ret$Curr_Site_Data$Well),
      
      #helpText("Execute the GWSDAT R script with the basic example."),
      #actionButton("run_gwsdat", "Run"),
      h4("Status:"),
      textOutput("status")
    ),
    
    mainPanel(
      plotOutput("time_series")
    )
  )
)

shinyApp(ui = ui, server = server)

