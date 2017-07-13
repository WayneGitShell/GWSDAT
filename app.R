

library(shiny)

source("R/GWSDAT_Setup.R")

options(warn=1)


#
# Setup and init data and GWSDAT configuration.
#
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
server <- function(input, output, session) {
    
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
      
      # 'rg1' is also used in the Traffic Lights table (there it has
      # two threshold option. However, here it is only about selecting
      # to display the threshold or not. 
      # ? Might think of separating these two, i.e. creating extra variable
      #   here instead of rg1.
      if(input$check_threshold)
        pnl$rg1 <- "Threshold - Absolute"
      else 
        pnl$rg1 <- "Trend"
      
      
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

      pnl$timestep <- input$time_steps
      pnl$ScaleCols[1:length(pnl$ScaleCols)] <-  FALSE
      pnl$ScaleCols[input$imageplot_options] <-  TRUE
      pnl$GW.disp <- input$gw_flows
      pnl$Color.type <- input$imageplot_type
      
      ## The following two also apply to time-series but they are copies
      ## I need the same input for both tabs!
      pnl$Cont.rg <- input$solute_select_contour
      pnl$rgUnits <- input$solute_conc_contour
      
      ## Detect if aggregation of data was changed.
      if(pnl$DRV$GWSDAT_Options$Aggby != input$aggregate_data) {
        pnl$DRV$GWSDAT_Options$Aggby <- input$aggregate_data
        agg_data = GWSDAT_Aggregate_Data(pnl$DRV$GWSDAT_Options, 
                                         pnl$DRV$All.Data$All.Dates, 
                                         pnl$DRV$All.Data$GW.Data, 
                                         pnl$DRV$All.Data$Cont.Data, 
                                         pnl$DRV$All.Data$Well.Coords, 
                                         pnl$DRV$All.Data$NAPL.Thickness.Data)
        
        # Write back.
        pnl$DRV$All.Data$All.Agg.Dates = agg_data$All.Agg.Dates
        pnl$DRV$All.Data$Cont.Data = agg_data$Cont.Data
        pnl$DRV$All.Data$Agg_GW_Data = agg_data$Agg_GW_Data
        pnl$DRV$All.Data$NAPL.Thickness.Data = agg_data$NAPL.Thickness.Data
        
        # Update time step range.
        pnl$timestep_range = c(1, length(pnl$DRV$All.Data$All.Agg.Dates))
        browser()
        # Trigger update of time step slider.
        updateSliderInput(session, "time_steps", value = pnl$timestep_range[1],
                          min = pnl$timestep_range[1], max = pnl$timestep_range[2],
                          step = 1)
        
      }
         
      
      Plot_ImagePlot(pnl)
      
    })
    
    
    #
    # Plot Traffic Lights Table
    #
    output$traffic_table <- renderPlot({
      
      #
      # Update control attributes from reactive variables. 
      #
      
      pnl$rg1 <-  input$trend_or_threshold
      pnl$ColTrafficListbox <- input$traffic_color
      pnl$timestep <- input$time_steps_traffic
      #browser()
      
      Plot_TrafficTable(pnl)
      
    })
    
    # Mirror the selected solute in different tabs. 
    observe({ 
      active_tab <- input$plot_tabs
      
      if(active_tab == "Smooth Time-Series")
        updateSelectInput(session, "solute_select_contour", selected = input$solute_select ) 
      
      if(active_tab == "Contour Plot")
        updateSelectInput(session, "solute_select", selected = input$solute_select_contour )
    })
    
    # Mirror the selected solute concentration in different tabs. 
    observe({ 
      active_tab <- input$plot_tabs
      
      if(active_tab == "Smooth Time-Series")
        updateSelectInput(session, "solute_conc_contour", selected = input$solute_conc ) 
      
      if(active_tab == "Contour Plot")
        updateSelectInput(session, "solute_conc", selected = input$solute_conc_contour )
    })

    
}



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel(h2("Shiny GWSDAT")),
  
  tabsetPanel(id="plot_tabs",
    tabPanel("Smooth Time-Series", id = "ts_tab", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput("well_select", label = "Select Monitoring Well", choices = sort(as.character(pnl$DRV$All.Data$All.Wells)),
                             selected = pnl$Well, width = "50%"),
                 
                 selectInput("solute_select", label = "Solute", choices = names(pnl$DRV$Fitted.Data),
                             selected = pnl$Cont.rg, width = '50%'),
               
                 radioButtons("solute_conc", label = "Solute Conc. Unit",
                              choices = pnl$rgUnits_choice, 
                              selected = pnl$rgUnits),
                 
                 checkboxInput("check_threshold", label = "Display threshold", value = FALSE ),
                 
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
    tabPanel("Contour Plot", id = "contour_tab", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput("solute_select_contour", label = "Solute", choices = names(pnl$DRV$Fitted.Data),
                             selected = pnl$Cont.rg, width = '50%'),
                 
                 radioButtons("solute_conc_contour", label = "Solute Conc. Unit",
                              choices = pnl$rgUnits_choice, 
                              selected = pnl$rgUnits),
                 
                 selectInput("imageplot_type", label = "Plot Type", choices = pnl$Color.type_choice,
                             selected = pnl$Color.type, width = "50%"),
                 
                 
                 checkboxGroupInput("imageplot_options", label = "Plot Options", 
                                    choices = names(pnl$ScaleCols),
                                    selected = names(which(pnl$ScaleCols == TRUE))),
                 
                 radioButtons("gw_flows", label = "Groundwater Flows",
                              choices = pnl$GW.disp_choice, 
                              selected = pnl$GW.disp)
                 
                 
               ),
               mainPanel(
                 fluidPage(
                 column(9, plotOutput("image_plot")),
                 
                 #hr(),
                 
                 fluidRow(
                   column(3, 
                    # Time step slider with animation.
                    sliderInput("time_steps", "Time Step",
                                 min = pnl$timestep_range[1], 
                                 max = pnl$timestep_range[2], 
                                 value = pnl$timestep, 
                                 step = 1,
                                 #pre = "$", sep = ",", 
                                 animate=TRUE)
                   ),
                   
                   column(3,
                    selectInput("aggregate_data", label = "Aggregate Data", 
                                 choices = c("All Dates", "Monthly", "Quarterly"),
                                 selected = pnl$DRV$GWSDAT_Options$Aggby, 
                                 width = "100%")
                   )
                 )
                )
              )
           )
    ),
    tabPanel("Traffic Lights", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 radioButtons("trend_or_threshold", label = "Display Table",
                              choices = pnl$rg1_choice, 
                              selected = pnl$rg1),
                 
                 selectInput("traffic_color", label = "Show color", choices = pnl$ColTrafficListbox_choice,
                             selected = pnl$ColTrafficListbox, width = "40%")
                 
                 
                 
               ),
               mainPanel(
                 
                 column(10, plotOutput("traffic_table")),
                 # Time step slider with animation.
                 sliderInput("time_steps_traffic", "Time Step",
                             min = pnl$timestep_range[1], 
                             max = pnl$timestep_range[2], 
                             value = pnl$timestep, 
                             step = 1,
                             #pre = "$", sep = ",", 
                             animate=TRUE)
               )
               
             )
             )
  )
) 


shinyApp(ui = ui, server = server)

