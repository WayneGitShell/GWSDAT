



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
      pnl$GWSDAT_Options$Aggby <<- input$aggregate_data
      
      
      
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
      pnl$GWSDAT_Options$Aggby <<- input$aggregate_data_traffic
      
      Plot_TrafficTable(pnl)
      
    })
    
    
    # Plot the legend for the traffic lights table.
    output$plot_legend_traffic <- renderPlot({ Plot_Legend_TrafficLights()  })
    
    
    # Re-Aggregate the data in case the aggregation type was changed.
    Reaggregate_Data <- function(aggby) {

      
      if(pnl$GWSDAT_Options$Aggby != aggby) {
        pnl$GWSDAT_Options$Aggby <<- aggby # input$aggregate_data
        
        agg_data <<- GWSDAT_Aggregate_Data(pnl$GWSDAT_Options, 
                                           pnl$All.Data$All.Dates, 
                                           pnl$All.Data$GW.Data, 
                                           pnl$All.Data$Cont.Data, 
                                           pnl$All.Data$Well.Coords, 
                                           pnl$All.Data$NAPL.Thickness.Data)
        
        # Write back.
        pnl$All.Data$All.Agg.Dates <<- agg_data$All.Agg.Dates
        pnl$All.Data$Cont.Data <<- agg_data$Cont.Data
        pnl$All.Data$Agg_GW_Data <<- agg_data$Agg_GW_Data
        pnl$All.Data$NAPL.Thickness.Data <<- agg_data$NAPL.Thickness.Data
        
        # Fit data. 
        Fitted.Data = GWSDAT_Fit_Data(pnl$All.Data, pnl$GWSDAT_Options)
        
        if(class(Fitted.Data) != "gwsdat_fit") {
          stop("There was a problem with GWSDAT_Fit_Data() .. no fitted data returned, object class is: ", class(Fitted.Data), "\n")
        }
        
        pnl$Fitted.Data <<- Fitted.Data
        
        # Update time step range.
        pnl$timestep_range <<- c(1, length(pnl$All.Data$All.Agg.Dates))
        
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
      
      filename <-  "shiny_timeseries_plot.png",
      
      content <-  function(file) {
        png(file, width = 1000, height = 600)
        Plot_SmoothTimeSeries(pnl)
        dev.off()
      }
    )
    
    output$download_contour_plot <- downloadHandler(
      
      filename <-  "shiny_contour_plot.png",
      
      content <-  function(file) {
        png(file, width = 1000, height = 1000)
        Plot_ImagePlot(pnl)
        dev.off()
      }
    )
    
    
    output$download_traffictable <- downloadHandler(
      
      filename = "shiny_trafficlights_plot.png",
      
      content = function(file) {
        png(file, width = 1200, height = 1500)
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
        
        # Data Manager main panel.
        div(id = "data_manager", shiny_ui_datamanager()),
        
        # Data Import panel
        shinyjs::hidden(
          div(id = "data_import", shiny_ui_dataimport())
        )
      ),
                 
      
      # Analysis Tab
      tabItem(tabName = "analysis", shiny_ui_analysepanel()
      ) # end tabItem
    ) # end tabItems
 ) # end dashboardBody 
) # end ui
 

ui_analyse_only <- shinyUI(
  fluidPage(shiny_ui_analysepanel())
#  fluidPage(h1("test"))
)
  
  

if (!RUN_SINGLE_INSTANCE) {
  shinyApp(ui = ui, server = server)
} else {
  shinyApp(ui = ui_analyse_only, server = server)
}
