

library(shiny)

source("R/GWSDAT_Setup.R")

GWSDAT_Options = GWSDAT_Setup()


ret = GWSDAT_Init(GWSDAT_Options)
pnl = Create_PanelAttr(ret$Curr_Site_Data, RUNNING_SHINY = TRUE)



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
    # Plot time-series window
    #
    output$time_series <- renderPlot({
      
      #
      # Update control attributes from reactive variables. 
      #
      pnl$Cont.rg <- input$solute_select
      pnl$rgUnits <- input$solute_conc
      pnl$dlines[1:length(pnl$dlines)] = FALSE
      pnl$dlines[input$ts_true_options] = TRUE
      pnl$Well <- input$well_select
      
      # Make the plot.
      Plot_SmoothTimeSeries(pnl)
      
    })
    
    #
    # Plot ImagePlot
    #
    output$image_plot <- renderPlot({
      
      #
      # Update control attributes from reactive variables. 
      #
      pnl$shadow.jjj <-  input$time_steps
      pnl$ScaleCols[1:length(pnl$ScaleCols)] <-  FALSE
      pnl$ScaleCols[input$imageplot_options] <-  TRUE
      pnl$GW.disp <- input$gw_flows
      pnl$Color.type <- input$imageplot_type
      
      ## The following two also apply to time-series but they are copies
      ## I need the same input for both tabs!
      pnl$Cont.rg <- input$solute_select_contour
      pnl$rgUnits <- input$solute_conc_contour
      #browser()
      
      Plot_ImagePlot(pnl)
      
    })
    
}



# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(h2("Shiny GWDAT")),
  tabsetPanel(
    tabPanel("Smooth Time-Series", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput("well_select", label = "Select Monitoring Well", choices = sort(as.character(pnl$DRV$All.Data$All.Wells)),
                             selected = pnl$Well, width = "40%"),
                 
                 selectInput("solute_select", label = "Solute", choices = names(pnl$DRV$Fitted.Data),
                             selected = pnl$Cont.rg, width = '40%'),
                 #hr(),
                 radioButtons("solute_conc", label = "Solute Conc. Unit",
                              choices = pnl$rgUnits_choice, 
                              selected = pnl$rgUnits),
                 
                 checkboxGroupInput("ts_true_options", label = "Time Series Plot Options", 
                                    choices = names(pnl$dlines),
                                    selected = names(which(pnl$dlines == TRUE))),
                 
                 h4("Status:"),
                 textOutput("status")
                 
               ),
               mainPanel(
                 column(9, plotOutput("time_series"))
                 
               )
             )
    ),
    tabPanel("Contour Plot", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput("solute_select_contour", label = "Solute", choices = names(pnl$DRV$Fitted.Data),
                             selected = pnl$Cont.rg, width = '40%'),
                 
                 radioButtons("solute_conc_contour", label = "Solute Conc. Unit",
                              choices = pnl$rgUnits_choice, 
                              selected = pnl$rgUnits),
                 
                 selectInput("imageplot_type", label = "Plot Type", choices = pnl$Color.type_choice,
                             selected = pnl$Color.type, width = "40%"),
                 
                 sliderInput("time_steps", label = "Time Steps", 
                             min   = pnl$shadow.jjj.range[1], 
                             max   = pnl$shadow.jjj.range[2], 
                             value = pnl$shadow.jjj),
                 
                 checkboxGroupInput("imageplot_options", label = "Plot Options", 
                                    choices = names(pnl$ScaleCols),
                                    selected = names(which(pnl$ScaleCols == TRUE))),
                 
                 radioButtons("gw_flows", label = "Groundwater Flows",
                              choices = pnl$GW.disp_choice, 
                              selected = pnl$GW.disp)
                 
                 
               ),
               mainPanel(
                 column(9, plotOutput("image_plot"))
               )
           )
    )
  )
) 


shinyApp(ui = ui, server = server)

