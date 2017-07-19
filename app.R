
source("R/GWSDAT_Setup.R")

options(warn = 1)


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
    
  
  #well_data_tmp = NULL
  well_data_file = NULL
  
  #well_coords_tmp = NULL
  well_coord_file = NULL
  
    
    #
    # Plot time-series window
    #
    output$time_series <- renderPlot({
      
      #
      # Update control attributes from reactive variables. 
      #
      pnl$Cont.rg <<- input$solute_select
      pnl$rgUnits <<- input$solute_conc
      pnl$dlines[1:length(pnl$dlines)] <<- FALSE
      pnl$dlines[input$ts_true_options] <<- TRUE
      pnl$Well <<- input$well_select
      
      # 'rg1' is also used in the Traffic Lights table (there it has
      # two threshold option. However, here it is only about selecting
      # to display the threshold or not. 
      # ? Might think of separating these two, i.e. creating extra variable
      #   here instead of rg1.
      if(input$check_threshold)
        pnl$rg1 <<- "Threshold - Absolute"
      else 
        pnl$rg1 <<- "Trend"
      
      
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

      pnl$ScaleCols[1:length(pnl$ScaleCols)] <<-  FALSE
      pnl$ScaleCols[input$imageplot_options] <<-  TRUE
      pnl$GW.disp <<- input$gw_flows
      pnl$Color.type <<- input$imageplot_type
      
      ## The following two also apply to time-series but they are copies
      ## I need the same input for both tabs!
      pnl$Cont.rg <<- input$solute_select_contour
      pnl$rgUnits <<- input$solute_conc_contour
      
      
      ##
      ## Note: There are separate observeEvents() for the following variables. 
      ##       I have them here to have renderPlot() called whenever they change.
      ##       I don't know yet how to trigger this renderPlot from observeEvents().
      ##
      pnl$timestep <<- input$time_steps
      pnl$DRV$GWSDAT_Options$Aggby <<- input$aggregate_data
      
      
      
      Plot_ImagePlot(pnl)
      
    })
    
    
    #
    # Plot Traffic Lights Table
    #
    output$traffic_table <- renderPlot({
      
      #
      # Update control attributes from reactive variables. 
      #
      
      pnl$rg1 <<-  input$trend_or_threshold
      pnl$ColTrafficListbox <<- input$traffic_color
      
      
      ##
      ## Note: There are separate observeEvents() for the following variables. 
      ##       I have them here to have renderPlot() called whenever they change.
      ##       I don't know yet how to trigger this renderPlot from observeEvents().
      ##
      pnl$timestep <<- input$time_steps_traffic
      pnl$DRV$GWSDAT_Options$Aggby <<- input$aggregate_data_traffic
      
      Plot_TrafficTable(pnl)
      
    })
    
    
    # Plot the legend for the traffic lights table.
    output$plot_legend_traffic <- renderPlot({ Plot_Legend_TrafficLights()  })
    
    
    # Re-Aggregate the data in case the aggregation type was changed.
    Reaggregate_Data <- function(aggby) {

      
      if(pnl$DRV$GWSDAT_Options$Aggby != aggby) {
        pnl$DRV$GWSDAT_Options$Aggby <<- aggby # input$aggregate_data
        
        agg_data <<- GWSDAT_Aggregate_Data(pnl$DRV$GWSDAT_Options, 
                                           pnl$DRV$All.Data$All.Dates, 
                                           pnl$DRV$All.Data$GW.Data, 
                                           pnl$DRV$All.Data$Cont.Data, 
                                           pnl$DRV$All.Data$Well.Coords, 
                                           pnl$DRV$All.Data$NAPL.Thickness.Data)
        
        # Write back.
        pnl$DRV$All.Data$All.Agg.Dates <<- agg_data$All.Agg.Dates
        pnl$DRV$All.Data$Cont.Data <<- agg_data$Cont.Data
        pnl$DRV$All.Data$Agg_GW_Data <<- agg_data$Agg_GW_Data
        pnl$DRV$All.Data$NAPL.Thickness.Data <<- agg_data$NAPL.Thickness.Data
        
        # Fit data. 
        Fitted.Data = GWSDAT_Fit_Data(pnl$DRV$All.Data, pnl$DRV$GWSDAT_Options)
        
        if(class(Fitted.Data) != "gwsdat_fit") {
          stop("There was a problem with GWSDAT_Fit_Data() .. no fitted data returned, object class is: ", class(Fitted.Data), "\n")
        }
        
        pnl$DRV$Fitted.Data <<- Fitted.Data
        
        # Update time step range.
        pnl$timestep_range <<- c(1, length(pnl$DRV$All.Data$All.Agg.Dates))
        
        # Reset to first time step
        pnl$timestep <<- pnl$timestep_range[1]
      }      
      
    }
    
    
    # Mirror solute selection in different tabs.
    observeEvent(input$solute_select, updateSelectInput(session, "solute_select_contour", selected = input$solute_select ) )
    observeEvent(input$solute_select_contour, updateSelectInput(session, "solute_select", selected = input$solute_select_contour )  )

    
    # Mirror time step selection in different tabs.
    observeEvent(input$time_steps_traffic, updateSliderInput(session, "time_steps", value = input$time_steps_traffic) )
    observeEvent(input$time_steps, updateSliderInput(session, "time_steps_traffic", value = input$time_steps) )
    
    
    # Re-aggregate the data and mirror the controls in the 'Traffic Lights' tab.
    observeEvent(input$aggregate_data, {
      Reaggregate_Data(input$aggregate_data)
      updateSelectInput(session, "aggregate_data_traffic", selected = input$aggregate_data ) 
      updateSliderInput(session, "time_steps_traffic", value = pnl$timestep, min = pnl$timestep_range[1], max = pnl$timestep_range[2], step = 1)
    })
    
    
    # Re-aggregate the data and mirror the controls in the 'Contour Plot' tab.
    observeEvent(input$aggregate_data_traffic, {
      Reaggregate_Data(input$aggregate_data_traffic)
      updateSelectInput(session, "aggregate_data", selected = input$aggregate_data_traffic ) 
      updateSliderInput(session, "time_steps", value = pnl$timestep, min = pnl$timestep_range[1], max = pnl$timestep_range[2], step = 1)
    })
    
    
    
    output$download_timeseries_plot <- downloadHandler(
      
      filename = "shiny_timeseries_plot.png",
      
      content = function(file) {
        png(file, width=1200, height=800)
        Plot_SmoothTimeSeries(pnl)
        dev.off()
        
      }
    )
    
    output$download_contour_plot <- downloadHandler(
      
      filename = "shiny_contour_plot.png",
      
      content = function(file) {
        png(file, width=1200, height=1200)
        Plot_ImagePlot(pnl)
        dev.off()
        
      }
    )
    
    
    output$download_traffictable <- downloadHandler(
      
      filename = "shiny_trafficlights_plot.png",
      
      content = function(file) {
        png(file, width=1200, height=1500)
        Plot_TrafficTable(pnl)
        dev.off()
        
      }
    )
    
    
    #
    #
    # Data related stuff
    #
    #
 
    #
    # Table showing the well data.
    #
    output$table_well_data <- renderTable({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      inFile <- input$well_data_file
      
      if (is.null(inFile))
        return(NULL)
      
      well_data_file <<- inFile$datapath
      
      # Load the data.
      well_data_tmp <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
      
      #
      # Check if Excel date transform checkbox is active
      #
      #browser()
      if (input$excel_date && ("SampleDate" %in% names(well_data_tmp)) ) 
        well_data_tmp$SampleDate <- as.character(GWSDAT.excelDate2Date(floor(as.numeric(as.character(well_data_tmp$SampleDate))))) 
        
     
      return(well_data_tmp)
    })
    
    
    #
    # Table showing the well coordinates.
    #
    output$table_well_coord <- renderTable({
      
      inFile <- input$well_coord_file
      
      if (is.null(inFile))
        return(NULL)
      
      # Save this for the Import button.
      well_coord_file <<- inFile$datapath 
      
      well_coord_tmp <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
      
      return(well_coord_tmp)
    })

    
    #
    # Supposed to clear everything in the Import Data panel
    #  Thats a little more complicated, not working right now.
    #
    observeEvent(input$reset_button,  {
      
      #browser()
      #value = input$reset_button
      #gg = 9
      
    })
    
    
    observeEvent(input$import_button,  {
     
      #
      # Read the well data.
      #
      if (is.null(well_coord_file) || is.null(well_data_file))
        return(NULL)
      
      
      AGALL <- Read_Well_Data(well_data_file, header = input$header, sep = input$sep, quote = input$quote)
      
      
      #
      # Read the well coordinates.
      #
      well_coords <- Read_Well_Coords(well_coord_file, header = input$header, sep = input$sep, quote = input$quote)
  
      WellCoords <- well_coords$WellCoords
      GWSDAT_Options$WellCoordsLengthUnits <-  well_coords$WellCoordsLengthUnits
 
      
      #
      # Go back to Data Manager.
      #
      shinyjs::show(id = "data_manager", anim = TRUE);
      shinyjs::hide(id = "data_import", anim = TRUE)
      
    })
    
    
    #
    # Go to Data Import (Button click).
    #
    observeEvent(input$add_new_data,  {
      shinyjs::show(id = "data_import", anim = TRUE)
      shinyjs::hide(id = "data_manager", anim = TRUE)
    })
    
    
    #
    # Go to Data Import (Link).
    #
    shinyjs::onclick("toggleDataImport", {
      shinyjs::show(id = "data_import", anim = TRUE);
      shinyjs::hide(id = "data_manager", anim = TRUE)
    })
    
    
    #
    # Go to Data Manager.
    #
    shinyjs::onclick("toggleDataManager", {
      shinyjs::show(id = "data_manager", anim = TRUE);
      shinyjs::hide(id = "data_import", anim = TRUE)
    })
        
}






########################### UI Section #############################################################








ui <- dashboardPage(
  
  dashboardHeader(title = "Shiny GWSDAT"),
  dashboardSidebar(sidebarMenu(
    menuItem("Manage Data", tabName = "input_data", icon = icon("archive")),
    menuItem("Analyse", tabName = "analysis", icon = icon("bar-chart"))
  )),
  
  dashboardBody(

    tabItems(
     
      tabItem(tabName = "input_data", 
          
          # Init the shiny JS framework
          useShinyjs(),
          
          
          #
          # Data Manager panel
          #
          div(id = "data_manager",
            fluidPage(
               fluidRow(
                 
                 h3("Data Manager"),
                 hr(),
                 "No data is present.",
                 a(id = "toggleDataImport", "Add", href = "#"),
                 " new data set.",
                 hr(),
                 #p(HTML("No data is present. <a href='#import_data_page'>Add</a> new data set.")),
                 #a(id = "toggleDataImport", "Add", href = "#")#,
                 actionButton("add_new_data", label = " Import Data", icon = icon("plus"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
               )
             )
          ),
          
          
          #
          # Data Import panel
          #
          shinyjs::hidden(
          
            div(id = "data_import",
              fluidPage(
  
                column(3,
                       
                       a(id = "toggleDataManager", "<- Go back.", href = "#"),
                       
                       h3("Import New CSV Data"),
                       "Select the well data file including the solute values for each well, and the well coordinates file in .csv file format.",
                       
                       hr(),
                       fileInput('well_data_file', 'Well Data File (CSV)',
                                 accept = c('text/csv', 
                                          'text/comma-separated-values,text/plain', 
                                          '.csv')),
                       
                       fileInput('well_coord_file', 'Well Coordinates File (CSV)',
                                 accept = c('text/csv', 
                                          'text/comma-separated-values,text/plain', 
                                          '.csv')),
                       
                       tags$hr(),
                       
                       checkboxInput('header', 'Header', TRUE),
                       checkboxInput('excel_date', 'Transform Excel Date', FALSE),
                       radioButtons('sep', 'Separator',
                                    c(Comma=',',
                                      Semicolon=';',
                                      Tab='\t'),
                                    ','),
                       radioButtons('quote', 'Quote',
                                    c(None='',
                                      'Double Quote'='"',
                                      'Single Quote'="'"),
                                    '"'),
                       hr(),
                       actionButton("reset_button", label = "Reset"),
                       #tags$head(
                       #   tags$style(HTML('#run{background-color:orange}'))
                       # ),
                       actionButton("import_button", label = "Import Data", icon("arrow-down"), 
                                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                       )
                       
                       
                ), # end column
                
                column(4,
                       #h4("Well and Solute data"),
                       tableOutput('table_well_data')
                ), # end column
                column(4,
                       #h4("Well and Solute data"),
                       tableOutput('table_well_coord')
                )
                
              ) # end fluidPage
                  
            ) # end div (data_import)
            
          ) # end hidden
          
        ), # end tabItem
                 
      
    
      #
      #
      # Analysis Tab
      #
      #
      #
      
      
      
      
      
      tabItem(tabName = "analysis",
              
              tabsetPanel(id = "plot_tabs",
                          
                          
                          tabPanel("Smooth Time-Series", id = "ts_tab", fluid = TRUE,
                                   
                                   column(3,
                                          wellPanel(
                                            selectInput("well_select", label = "Select Monitoring Well", choices = sort(as.character(pnl$DRV$All.Data$All.Wells)),
                                                        selected = pnl$Well, width = "80%"),
                                            
                                            selectInput("solute_select", label = "Solute", choices = names(pnl$DRV$Fitted.Data),
                                                        selected = pnl$Cont.rg, width = '80%'),
                                            
                                            radioButtons("solute_conc", label = "Solute Conc. Unit",
                                                         choices = pnl$rgUnits_choice, 
                                                         selected = pnl$rgUnits),
                                            
                                            checkboxInput("check_threshold", label = "Display threshold", value = FALSE ),
                                            
                                            checkboxGroupInput("ts_true_options", label = "Time Series Plot Options", 
                                                               choices = names(pnl$dlines),
                                                               selected = names(which(pnl$dlines == TRUE)))
                                            
                                            
                                            #h4("Status:"),
                                            #textOutput("status")
                                            
                                          )
                                   ),
                                   
                                   column(7,
                                          plotOutput("time_series"),
                                          downloadButton("download_timeseries_plot", label = "Save Plot")
                                          
                                   )
                                   
                                   
                          ),
                          
                          
                          
                          
                          tabPanel("Contour Plot", id = "contour_tab", fluid = TRUE,
                                   
                                   column(3, 
                                          
                                          wellPanel(
                                            selectInput("solute_select_contour", label = "Solute", choices = names(pnl$DRV$Fitted.Data),
                                                        selected = pnl$Cont.rg, width = '80%'),
                                            
                                            radioButtons("solute_conc_contour", label = "Solute Conc. Unit",
                                                         choices = pnl$rgUnits_choice, 
                                                         selected = pnl$rgUnits),
                                            
                                            selectInput("imageplot_type", label = "Plot Type", choices = pnl$Color.type_choice,
                                                        selected = pnl$Color.type, width = "80%"),
                                            
                                            
                                            checkboxGroupInput("imageplot_options", label = "Plot Options", 
                                                               choices = names(pnl$ScaleCols),
                                                               selected = names(which(pnl$ScaleCols == TRUE))),
                                            
                                            radioButtons("gw_flows", label = "Groundwater Flows",
                                                         choices = pnl$GW.disp_choice, 
                                                         selected = pnl$GW.disp)
                                            
                                          )          
                                          
                                   ),
                                   
                                   column(7, 
                                          plotOutput("image_plot")
                                   ),
                                   
                                   column(2, 
                                          sliderInput("time_steps", "Time Step",
                                                      min = pnl$timestep_range[1], 
                                                      max = pnl$timestep_range[2], 
                                                      value = pnl$timestep, 
                                                      step = 1,
                                                      #pre = "$", sep = ",", 
                                                      animate = TRUE),
                                          selectInput("aggregate_data", label = "Aggregate Data", 
                                                      choices = c("All Dates", "Monthly", "Quarterly"),
                                                      selected = pnl$DRV$GWSDAT_Options$Aggby, 
                                                      width = "100%"),
                                          
                                          downloadButton("download_contour_plot", label = "Save Plot")
                                          
                                          
                                   )
                          ),
                          
                          
                          
                          tabPanel("Traffic Lights", fluid = TRUE,
                                   
                                   column(3,
                                          
                                          wellPanel(
                                            radioButtons("trend_or_threshold", label = "Display Table",
                                                         choices = pnl$rg1_choice, 
                                                         selected = pnl$rg1),
                                            
                                            selectInput("traffic_color", label = "Show color", choices = pnl$ColTrafficListbox_choice,
                                                        selected = pnl$ColTrafficListbox, width = "80%")
                                            
                                          )
                                          
                                   ),
                                   
                                   column(7,
                                          plotOutput("traffic_table"),
                                          plotOutput("plot_legend_traffic")
                                   ),
                                   column(2,
                                          sliderInput("time_steps_traffic", "Time Step",
                                                      min = pnl$timestep_range[1], 
                                                      max = pnl$timestep_range[2], 
                                                      value = pnl$timestep, 
                                                      step = 1,
                                                      animate = TRUE),
                                          selectInput("aggregate_data_traffic", label = "Aggregate Data", 
                                                      choices = c("All Dates", "Monthly", "Quarterly"),
                                                      selected = pnl$DRV$GWSDAT_Options$Aggby, 
                                                      width = "100%"),
                                          downloadButton("download_traffictable", label = "Save Plot")
                                   )
                                   
                          )
                          
              ) # end TabPanel
          ) # end tabItem
    ) # end tabItems
 ) # end dashboardBody 
) # end ui
 


shinyApp(ui = ui, server = server)

