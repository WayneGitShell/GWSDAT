

source("R/GWSDAT_Setup.R")


# Print warnings when they happen.
options(warn = 1)


 
HeadlessMode <- FALSE
if (.Platform$OS.type == "unix")
    HeadlessMode <- TRUE

# The progress bar starts here because of lengthy package loading. 
# All the rest is done inside initSite(). 
progressBar = NULL
if (!HeadlessMode) {
    require(tcltk)
    progressBar <- tkProgressBar('GWSDAT Progress', 'Loading R packages...',0, 1, 0)
}

# Loads packages and R sources.
GWSDAT_Setup()


if (!exists("GWSDAT_Options", envir = .GlobalEnv)) {
  
  GWSDAT_Options <-  createOptions(HeadlessMode)
  
  #GWSDAT_Options[['SiteName']] <- 'Comprehensive Example'
  #GWSDAT_Options[['WellDataFilename']] <- 'data/ComprehensiveExample_WellData.csv'
  #GWSDAT_Options[['WellCoordsFilename']] <- 'data/ComprehensiveExample_WellCoords.csv'
  #GWSDAT_Options[['ShapeFileNames']] <- c(GWSDAT_Options[['ShapeFileNames']],'data/GIS_Files/GWSDATex2.shp')

}


# Set DevMode true on Andrej's computer.
GWSDAT_Options[['DevMode']] <- FALSE
if (Sys.info()["nodename"] == "LAPTOP-QU06V978")
  GWSDAT_Options[['DevMode']] <- TRUE


curr_site = initSite(GWSDAT_Options, progressBar = progressBar)


# Create a complete GWSDAT instance with data, model, and options. 
pnl <- createPanelAttr(curr_site)

if (!GWSDAT_Options$HeadlessMode) {
    try(close(progressBar))
}



  
# Put into global environment, so the shiny server can see it. 
.GlobalEnv$pnl <- pnl



########################### Server Section ######################################

# Define server logic 
server <- function(input, output, session) {
   
  
  #
  # Execute once per user:
  #
  
  #
  # Read GWSDAT instances.
  # E.g. GWSDAT_inst_lst = load_GWSDAT_inst(user_id)  # return array of GWSDAT_instance
  #      GWSDAT_inst_lst[[1]]     # access first GWSDAT_instance, holds data, fit, etc.
  #
  #GWSDAT_inst_lst = list(pnl)
  #GWSDAT_inst <- GWSDAT_inst_lst[[1]] # How to access this in ui space?
  
  
  # This goes into a GWSDAT_instance
  well_data_file <- NULL   
  well_coord_file <- NULL
  
  
  # 
  # Some usefull session objects:
  #
  # session$userData  - store user data here
  # onSessionEnded(callback), onEnded(callback)  - executes when user exits browser
  # isClosed()  - function that return TRUE if client has disconnected  
  #
  
  
  #
  # Clean-up user session.
  #
  session$onSessionEnded(function() {
    
    # Shuts down the server, O.K. for single user mode. 
    #   Change this when running on server?
    stopApp()
   
  })
  
  
  checkPlumeStats <- reactive({
    
    val <- getFullPlumeStats(pnl, 
                             input$solute_select_plume_pd, 
                             input$plume_threshold_pd,
                             input$ground_porosity_pd
                             )

    # If there is any plume mass, show the plot and hide the message text, and 
    #  vice versa. 
    if (all(is.na(val$mass))) {
      shinyjs::hide("plume_diagn_plot_div")
      shinyjs::show("plume_diagn_msg_div")
    } else {
      shinyjs::show("plume_diagn_plot_div")
      shinyjs::hide("plume_diagn_msg_div")
    }
    
    return(val)
  })
  
  # return
  updatePlumeTS <- reactive({
    
    # update plume threshold
    
    input$update_plume_ts
    
    })
  
  output$plume_diagn_msg <- renderText({
    
    # Detect press of update button
    updatePlumeTS()
    
    # Detect changes in the Options.
    optionsSaved()
    
    # Detect if stats can not be displayed (hides this text box).
    # Isolate reactive inputs. 
    isolate(checkPlumeStats())
    
    # Isolate the inputs (so a change in the sidebar does not trigger this fct.)
    isolate(
    paste(input$solute_select_plume_pd, ": Unable to calculate plume statistics for threshold value = ", 
          input$plume_threshold_pd, " ug/l.", sep = "")
    )
          #"\nUse the 'Estimate Plume Boundary' function for assistance in selecting a suitable plume threshold concentration value.")
    
  })
  
  
  output$plume_diagn_plot <- renderPlot({
    
    # Detect press of update button
    updatePlumeTS()
    
    # Detect changes in the Options.
    optionsSaved()

    # Re-evaluate plume statistics if any reactive expression changes. 
    # The return value is the full plume statistics (for all timesteps). 
    isolate(plume_stats <- checkPlumeStats())

    plotPlumeTimeSeries(plume_stats)

    
  })
  
  
  # Plot time-series window
  output$time_series <- renderPlot({
    
    #
    # Update control attributes from reactive variables. 
    #
    
    pnl$rgUnits <<- input$solute_conc
    pnl$dlines[1:length(pnl$dlines)] <<- FALSE
    pnl$dlines[input$ts_true_options] <<- TRUE
    
    
    # 'rg1' is also used in the Traffic Lights table (there it has
    # two threshold option. However, here it is only about selecting
    # to display the threshold or not. 
    # ? Might think of separating these two, i.e. creating extra variable
    #   here instead of rg1.
    if (input$check_threshold)
      pnl$rg1 <<- "Threshold - Absolute"
    else 
      pnl$rg1 <<- "Trend"
    
   
    plotTimeSeries(pnl, input$solute_select, input$well_select)
    
  })
  
  
  # Reactive element that will trigger inside an observer when Options are saved.
  optionsSaved <- reactive({ 
    input$save_analyse_options 
  })

  
  #
  # Plot ImagePlot
  #
  output$image_plot <- renderPlot({
    
    # renderPlot() is called when the content of this function is modified.
    # This is to detect changes in the Options panel.
    val <- optionsSaved() 
  
    #val <- plumeThreshChange()
    #browser()
    #
    # Update control attributes from reactive variables. 
    #

    pnl$ScaleCols[1:length(pnl$ScaleCols)] <<-  FALSE
    pnl$ScaleCols[input$imageplot_options] <<-  TRUE
    pnl$GW.disp <<- input$gw_flows
    pnl$Color.type <<- input$imageplot_type
    
    ## The following two also apply to time-series but they are copies
    ## I need the same input for both tabs!
    pnl$rgUnits <<- input$solute_conc_contour
    
    
    ##
    ## Note: There are separate observeEvents() for the following variables. 
    ##       I have them here to have renderPlot() called whenever they change.
    ##       I don't know yet how to trigger this renderPlot from observeEvents().
    ##
    #pnl$timestep <<- input$time_steps
    pnl$GWSDAT_Options$Aggby <<- input$aggregate_data
    
    # Input control in UI is commented out, thus input$aquifer_contour will be NULL
    #pnl$All.Data$Aq.sel <<- input$aquifer_contour
    
    # PROFILING EXECUTION TIMES:
    #require(profr)
    #require(ggplot2)
    #tprof <- profr(..)
    
    #Rprof("Rprof_plotSpatialImage.out")
    #replicate(n = 30, plotSpatialImage(pnl, input$solute_select_contour, input$time_steps))
    plotSpatialImage(pnl, input$solute_select_contour, input$time_steps)
    #Rprof(NULL)
    
    #png("tprofile.png")
    #ggplot(tprof)
    #dev.off()
    
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
    #pnl$timestep <<- input$time_steps_traffic
    pnl$GWSDAT_Options$Aggby <<- input$aggregate_data_traffic
    
    plotTrendTable(pnl, input$time_steps_traffic)
    
  })
  
  
  # Plot the legend for the traffic lights table.
  output$plot_legend_traffic <- renderPlot({ Plot_Legend_TrafficLights()  })
  
  
  #
  # Plot Well Report
  #
  output$well_report_plot <- renderPlot({
    
    use_log_scale    <- if (input$well_report_logscale == "Yes") {TRUE} else {FALSE}
    
    plotWellReport(pnl, input$solute_chooser$left, 
                     input$well_chooser$left, use_log_scale)
    
  })
  
  
  
  # Re-Aggregate the data in case the aggregation type was changed.
  reaggregateData <- function(aggby) {

    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Reaggregating data", value = 0)
    on.exit(progress$close())
    
    
    # Use this closure to update the progress object from an external function.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }

    
    pnl$GWSDAT_Options$Aggby <<- aggby # input$aggregate_data
      
    agg_data <- aggregateData(pnl$GWSDAT_Options, 
                              pnl$All.Data$All.Dates, 
                              pnl$All.Data$GW.Data, 
                              pnl$All.Data$Cont.Data, 
                              pnl$All.Data$well_coords$data, 
                              pnl$All.Data$NAPL.Thickness.Data)
      
    # Write back.
    pnl$All.Data$All.Agg.Dates <<- agg_data$All.Agg.Dates
    pnl$All.Data$Cont.Data <<- agg_data$Cont.Data
    pnl$All.Data$Agg_GW_Data <<- agg_data$Agg_GW_Data
    pnl$All.Data$NAPL.Thickness.Data <<- agg_data$NAPL.Thickness.Data
      
    # Fit data. 
    Fitted.Data = fitData(pnl$All.Data, pnl$GWSDAT_Options, 
                          updateProgress = updateProgress,
                          progressB = progress)
      
    if (class(Fitted.Data) != "gwsdat_fit") {
      stop("There was a problem with GWSDAT_Fit_Data() .. no fitted data returned, object class is: ", class(Fitted.Data), "\n")
    }
      
    pnl$Fitted.Data <<- Fitted.Data
      
    # Update time step range.
    pnl$timestep_range <<- c(1, length(pnl$All.Data$All.Agg.Dates))
    
  }
  
  
  updateNAPL <- function(location, substance) {
    
    tmp_napl <- existsNAPL(pnl$All.Data, location, substance) 
    
    # Update checkbox control if NAPL changed.
    if (tmp_napl != pnl$NAPL.Present) {
      
      
      if (tmp_napl) 
        pnl$dlines["Overlay NAPL Thickness"] <<- FALSE  # set to some value
      else 
        pnl$dlines <<- pnl$dlines[-which(names(pnl$dlines) == "Overlay NAPL Thickness")]
        
      updateCheckboxGroupInput(session, "ts_true_options", label = "Time Series Plot Options", 
                         choices = names(pnl$dlines),
                         selected = names(which(pnl$dlines == TRUE)))
      
      pnl$NAPL.Present <<- tmp_napl
    }
  }
  
  
#  observeEvent(input$well_select, {
#    
#    updateNAPL(input$well_select, input$solute_select)
#    
#  })
  
  
  # When solute or well changes, update NAPL setting and mirror solute selection
  # to Spatial Plot panel.
  observeEvent({input$solute_select; 
    input$well_select}, {
    
    updateNAPL(input$well_select, input$solute_select)  
  
    updateSelectInput(session, "solute_select_contour", selected = input$solute_select ) 
  })
  
  observeEvent(input$solute_select_contour, {
    updateSelectInput(session, "solute_select", selected = input$solute_select_contour )  
  })
  
  
  #
  # The following commented lines of code are meant to change the x or y resolution
  # of the image setting (see Analyse panel) if the aspect ratio should be kept. 
  # The problem is that shiny reacts to each individual key input, and the calculation
  # breaks because too many events occur. 
  # 
  # There are three alternatives: 
  #   1. use a timer such that observeEvent is only triggered late
  #   2. implement an numericInput that triggers only when the input is left or a Return key is pressed.
  #   3. Find another numericInput that does this. 
  # 
  # Point 1. is not very reliable and depends on the user. Point 2. has to be implemented, it was done
  # before, see https://groups.google.com/forum/#!topic/shiny-discuss/BFUgjICEQlc . Better would be 
  # Point 3... maybe another search will do.
  # 
  #
  # prev_img_width_px <- 800
  # prev_img_height_px <- 600
  # asp_action <- FALSE
  # 
  # observeEvent(input$img_width_px, {
  #   cat("in observeEvent - img_width_px\n")
  #   browser()
  #   
  #   keep_asp = input$img_asp_px
  #   
  #   if (keep_asp && !asp_action) {
  #     
  #     new_height <- floor(input$img_height_px * (input$img_width_px / prev_img_width_px))
  #     
  #     # Update the numericInput
  #     updateNumericInput(session, "img_height_px", value = new_height )
  #     
  #     asp_action <<- TRUE
  #   } else {
  #     asp_action <<- FALSE 
  #   }
  #   
  #   prev_img_width_px <<- input$img_width_px
  #   
  # })
  # 
  # observeEvent(input$img_height_px, {
  #   cat("in observeEvent - img_height_px\n")
  #   browser()
  #   
  #   keep_asp = input$img_asp_px
  #   
  #   if (keep_asp && !asp_action) {
  #     
  #     new_width <- floor(input$img_width_px * (input$img_height_px / prev_img_height_px))
  #     
  #     # Update the numericInput
  #     updateNumericInput(session, "img_width_px", value = new_width )
  #     
  #     asp_action <<- TRUE
  #     
  #   } else {
  #     asp_action <<- FALSE 
  #   }
  #   
  #   prev_img_height_px <<- input$img_height_px
  # })
  # 
  #
  # END OF IMAGE RESIZE CODE
  #
  
  
  #
  # These two observers are likely to be the cause for github issue #48.
  #  Time steps of sliders are not mirrored right now. Maybe something for later.
  #
  # Mirror time step selection in different tabs.
  # observeEvent(input$time_steps_traffic, {
  #   updateSliderInput(session, "time_steps", value = input$time_steps_traffic) 
  # })
  # 
  # observeEvent(input$time_steps, {
  #   browser()
  #   updateSliderInput(session, "time_steps_traffic", value = input$time_steps) 
  # })
  # 
  
  # Re-aggregate the data and mirror the controls in the 'Traffic Lights' tab.
  observeEvent(input$aggregate_data, {
    
    if (pnl$GWSDAT_Options$Aggby != input$aggregate_data) {
      reaggregateData(input$aggregate_data)
      
      #browser()
      # Update time step slider in this panel.
      updateSliderInput(session, "time_steps", value = pnl$timestep_range[1], 
                        min = pnl$timestep_range[1], max = pnl$timestep_range[2], step = 1)
      
      # Mirror aggregation type to trend table.
      updateSelectInput(session, "aggregate_data_traffic", selected = input$aggregate_data ) 
      
    }
  })
  
  
  # Re-aggregate the data and mirror the controls in the 'Spatial Plot' tab.
  observeEvent(input$aggregate_data_traffic, {
    
    if (pnl$GWSDAT_Options$Aggby != input$aggregate_data_traffic) {
      reaggregateData(input$aggregate_data_traffic)
    
      #browser()
      # Update time step slider in this panel.
      updateSliderInput(session, "time_steps_traffic", value = pnl$timestep_range[1], 
                        min = pnl$timestep_range[1], max = pnl$timestep_range[2], step = 1)
      
      # Mirror aggregation type to spatial plot.
      updateSelectInput(session, "aggregate_data", selected = input$aggregate_data_traffic ) 
      
    }
  })
  
  #
  # Triggers when the 'Aquifer Group' input selection is changed.
  #  Not in use right now: need to decide where to put the Aquifer decision:
  #   1. Into Analyse Tab - needs a recalculation per re-selection, and whole UI needs
  #       to be redrawn.
  #   2. Data Manager - extra data entry per Aquifer. This would make it cleaner.
  #
  # # Re-select the data in respect to the Aquifer group.
  # observeEvent(input$aquifer_contour, {
  #   
  #   tmpval = input$aquifer_contour
  #   
  #   # If the selected aquifer group changed, reload the data.
  #   if (tmpval != pnl$All.Data$Aq.sel) {
  #     
  #     # Prepare the input data with the selected Aquifer.
  #     pnl$All.Data <<- try(prepare_data(pnl$All.Data$solute_data, 
  #                                  pnl$All.Data$well_data, 
  #                                  pnl$GWSDAT_Options, 
  #                                  Aq_sel = tmpval))
  #     
  #     if (inherits(pnl$All.Data, 'try-error')) {
  #       stop("Error in inputting and formatting data.")
  #     }
  #     
  #     
  #     # Fit the data.
  #     pnl$Fitted.Data <<- GWSDAT_Fit_Data(pnl$All.Data, pnl$GWSDAT_Options)
  #     
  #     if (class(pnl$Fitted.Data) != "gwsdat_fit") {
  #       stop("There was a problem with GWSDAT_Fit_Data() .. no fitted data returned, object class is: ", 
  #            class(Fitted.Data), "\n")
  #     }
  #     
  #     tmp_Cont <- pnl$Cont.rg
  #     tmp_rgUnits <- pnl$rgUnits
  #   
  #     # Create a complete GWSDAT instance with data, model, and options. 
  #     pnl <<- Create_PanelAttr(pnl)
  #     
  #     pnl$Cont.rg <<- tmp_Cont 
  #     pnl$rgUnits <<- tmp_rgUnits
  #   }
  #   
  # 
  # })
    
  
  output$save_timeseries_plot <- downloadHandler(
    
    filename <- function() { 
      paste("timeseries_plot.", input$export_format_ts, sep = "")
    },

    content <-  function(file) {
      
      if (input$export_format_ts == "ppt") {
        
        makeTimeSeriesPPT(pnl, input$solute_select, input$well_select,
                          width = input$img_width_inch, height = input$img_height_inch)
        
      } else {
        
        if (input$export_format_ts == "png") png(file, width = input$img_width_px, height = input$img_height_px)
        if (input$export_format_ts == "pdf") pdf(file, width = input$img_width_inch, height = input$img_height_inch) 
        if (input$export_format_ts == "ps") postscript(file, width = input$img_width_inch, height = input$img_height_inch) 
        if (input$export_format_ts == "jpg") jpeg(file, width = input$img_width_px, height = input$img_height_px, quality = input$img_jpg_quality) 
        if (input$export_format_ts == "wmf") win.metafile(file, width = input$img_width_inch, height = input$img_height_inch) 
        
        plotTimeSeries(pnl, input$solute_select, input$well_select)
        dev.off()
      }
    }
  )
  
  
  output$save_spatial_plot <- downloadHandler(
    
    filename <- function() { 
      paste("spatial_plot.", input$export_format_sp, sep = "")
    },
     
    content <-  function(file) {
     
      if (input$export_format_sp == "ppt") {
        
        plotSpatialImagePPT(pnl, input$solute_select_contour, input$time_steps,
                       width = input$img_width_inch, height = input$img_height_inch)
      
        } else {
        
          if (input$export_format_sp == "png") png(file, width = input$img_width_px, height = input$img_height_px)
          if (input$export_format_sp == "pdf") pdf(file, width = input$img_width_inch, height = input$img_height_inch) 
          if (input$export_format_sp == "ps") postscript(file, width = input$img_width_inch, height = input$img_height_inch) 
          if (input$export_format_sp == "jpg") jpeg(file, width = input$img_width_px, height = input$img_height_px, quality = input$img_jpg_quality) 
          if (input$export_format_sp == "wmf") win.metafile(file, width = input$img_width_inch, height = input$img_height_inch) 
          
          plotSpatialImage(pnl, input$solute_select_contour, input$time_steps)
          dev.off()
      }
      
    }
  )
  
  
  output$save_trend_table <- downloadHandler(
    
    filename <- function() { 
      paste("trend_table.", input$export_format_tt, sep = "")
    },
    
    content <-  function(file) {
      
      if (input$export_format_tt == "ppt") {
        
        plotTrendTablePPT(pnl, input$time_steps_traffic, 
                          width = input$img_width_inch, height = input$img_height_inch)
        
      } else {
        
        if (input$export_format_tt == "png") png(file, width = input$img_width_px, height = input$img_height_px)
        if (input$export_format_tt == "pdf") pdf(file, width = input$img_width_inch, height = input$img_height_inch) 
        if (input$export_format_tt == "ps")  postscript(file, width = input$img_width_inch, height = input$img_height_inch) 
        if (input$export_format_tt == "jpg") jpeg(file, width = input$img_width_px, height = input$img_height_px, quality = input$img_jpg_quality) 
        if (input$export_format_tt == "wmf") win.metafile(file, width = input$img_width_inch, height = input$img_height_inch) 
        
        plotTrendTable(pnl, input$time_steps_traffic)
        dev.off()
      }
      
    }
  )
  
  output$save_wellreport_plot <- downloadHandler(
    
    filename <- function() { 
      paste("wellreport.", input$export_format_wr, sep = "")
    },
    
    content <-  function(file) {
      
      use_log_scale    <- if (input$well_report_logscale == "Yes") {TRUE} else {FALSE}
      
      if (input$export_format_wr == "ppt") {
        
        plotWellReportPPT(pnl, input$solute_chooser$left, 
                          input$well_chooser$left, use_log_scale,
                          width = input$img_width_inch_wide, height = input$img_height_inch_wide)
        
      } else {
        
        if (input$export_format_wr == "png") png(file, width = input$img_width_px_wide, height = input$img_height_px_wide)
        if (input$export_format_wr == "pdf") pdf(file, width = input$img_width_inch_wide, height = input$img_height_inch_wide) 
        if (input$export_format_wr == "ps") postscript(file, width = input$img_width_inch_wide, height = input$img_height_inch_wide) 
        if (input$export_format_wr == "jpg") jpeg(file, width = input$img_width_px_wide, height = input$img_height_px_wide, quality = input$img_jpg_quality) 
        if (input$export_format_wr == "wmf") win.metafile(file, width = input$img_width_inch_wide, height = input$img_height_inch_wide) 
        
        plotWellReport(pnl, input$solute_chooser$left, input$well_chooser$left, use_log_scale)
        
        
        dev.off()
      }
      
    }
  )
  
  
  output$save_plumestats_plot <- downloadHandler(
    
    filename <- function() { 
      paste("plumestats.", input$export_format_pd, sep = "")
    },
    
    content <-  function(file) {

      plume_stats <- checkPlumeStats()
      
      plume_thresh <- pnl$PlumeLimEntry[input$solute_select_plume_pd]
      
      if (input$export_format_pd == "ppt") {
        
        plotPlumeTimeSeriesPPT(plume_stats, 
                               width = input$img_width_inch_wide, 
                               height = input$img_height_inch_wide)
        
      } else {
        
        if (input$export_format_pd == "png") png(file, width = input$img_width_px_wide, height = input$img_height_px_wide)
        if (input$export_format_pd == "pdf") pdf(file, width = input$img_width_inch_wide, height = input$img_height_inch_wide) 
        if (input$export_format_pd == "ps")  postscript(file, width = input$img_width_inch_wide, height = input$img_height_inch_wide) 
        if (input$export_format_pd == "jpg") jpeg(file, width = input$img_width_px_wide, height = input$img_height_px_wide, quality = input$img_jpg_quality) 
        if (input$export_format_pd == "wmf") win.metafile(file, width = input$img_width_inch_wide, height = input$img_height_inch_wide) 
        
        plotPlumeTimeSeries(plume_stats)
        dev.off()
      }
      
    }
  )
  
  output$save_plumestats_csv <- downloadHandler(
    
    filename <- function() {
      paste("plumestats.csv")
    },
    
    content <- function(file) {
      plume_stats <- checkPlumeStats()
      
      tmp_out <- printPlumeStatsCSV(plume_stats)
      
      write.csv(tmp_out, file) 
    }
  )
  
  # Generate PPT with spatial animation.
  observeEvent(input$generate_spatial_anim_ppt, {
    makeSpatialAnimation(pnl, input$solute_select_contour,
                         input$img_width_inch,
                         input$img_height_inch,
                         input$img_width_inch_wide,
                         input$img_height_inch_wide)
  })
  
  
  # Generate PPT with trend table animation.
  observeEvent(input$generate_trendtable_anim_ppt, {
    makeTrendTableAnimation(pnl)
  })
  

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
  # hint: user renderUI() that waits for the reset input
  #
  #observeEvent(input$reset_button,  {
    
    #browser()
    #value = input$reset_button
    #gg = 9
    
  #})
  
  
  
  
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

    #
    # Do remaining data processing ... 
    #
    # .. 
    
    
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
  
  
  #
  # Display the "Generate PPT Animation" Button only when in ExcelMode.
  #
  observeEvent(input$analyse_panel, {
    if (input$analyse_panel == "Spatial Plot" || input$analyse_panel == "Trends & Thresholds") {
      if ( (.Platform$OS.type == "windows") || pnl$GWSDAT_Options$ExcelMode) {
        shinyjs::show(id = "save_spatial_ppt_anim")
        shinyjs::show(id = "save_trendtable_ppt_anim")
      }
    }
  })
  
  # These inputs will modify the plume threshold for each substance, 
  #  saved in pnl$PlumeLimEntry.
  output$thres_plume_select <- renderUI({
   
    num_subst <- length(pnl$PlumeLimEntry)
    
    lapply(1:num_subst, function(i) {
      div(style = "display: inline-block;", 
          
          #
          # Note, I use a number for the input id instead of the substance name,
          #  which could have unusual characters or whitespaces. I will need to 
          #  extract and match back the number to what is in PlumeLimEntry.
          #
          numericInput(paste("plume_thresh_", i, sep = ""), 
                       label = names(pnl$PlumeLimEntry)[i], 
                       value = pnl$PlumeLimEntry[i], 
                       width = "100px")
      )
    })
  })
  
  
  observeEvent(input$save_analyse_options, {
    
    num_subst <- length(pnl$PlumeLimEntry)
    
    for (i in 1:num_subst) {
      
      # Create input variable name and evaluate the string as variable. 
      input_var <- paste("input$plume_thresh_", i, sep = "")
      pnl$PlumeLimEntry[i] <<- eval(parse(text = input_var))
    }
    
    
    pnl$Porosity <- input$ground_porosity
    
  })
  
  
  
  
  #
  # Decide whether to display the Aquifer Group selection when entering an Analyse tab.
  #
  #observeEvent(input$plot_tabs, {
    
    # If there are less than 2 aquifer, hide the selection control.
  #  if (length(pnl$All.Data$Aq_list) < 2) {
  #    shinyjs::hide(id = "select_aquifer_timeseries")
  #    shinyjs::hide(id = "select_aquifer_contour")
  #    shinyjs::hide(id = "select_aquifer_traffic")
  #    
  #  }
  #})
  #observeEvent(input$solute_chooser, {
  #  
  #  browser()
  #  val = input$mychooser
  #}) 
  
    
    
    
   # Note: If the well_report_box is hidden, the output$well_report_plot will not trigger.
   #       Thus, on the push of the "Generate Report" button, first the "well_report_box"
  #        is shown, afterwards renderPlot() for the contained 'well_report_plot' is triggered.
  #observeEvent(input$actionBtn_wellreport, {
  #  
  #   shinyjs::show(id = "well_report_box", anim = TRUE)
  # 
  #  })
   
  #   
  #   selected_solutes <- input$solute_chooser$left
  #   selected_wells   <- input$well_chooser$left
  #   use_log_scale    <- if (input$well_report_logscale == "Yes") {TRUE} else {FALSE}
  #   
  #   if (length(selected_solutes == 0) && length(selected_wells) == 0) {
  #     # toogle warning
  #     # ...
  #   } else {
  #     
  #     ret <- plotWellReport(pnl, selected_solutes, selected_wells, use_log_scale)
  #       
  #     if (class(ret) == "GWSDAT_Warning") {
  #       # toogle warning
  #       shinyjs::toggle("")
  #       #..
  #       stop("Fixme: Include warning message, e.g. toogle red text.")
  #     }
  #   }
  #   
  # })
}






########################### UI Section #############################################################


#
# Define the Shiny dashboard header
#
dbHeader <- dashboardHeader(title = "GWSDAT",
#                            tags$li(a(href = 'http://shinyapps.company.com',
#                                      icon("power-off"),
#                                      title = "Back to Apps Home"),
#                                    class = "dropdown"))#,
                            tags$li(class = "dropdown", 
                                    tags$a(href = 'http://www.api.org/oil-and-natural-gas/environment/clean-water/ground-water/gwsdat',
                                      target = '_blank',
                                      tags$img(src = 'Final_GWSDAT_Logo.png',
                                          title = "GWSDAT Homepage", height = "40px"),
                                      style = "padding-top:5px; padding-bottom:5px;")
                                    ))




ui <- dashboardPage(
  
  dbHeader, 
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
        div(id = "data_manager", uiDataManager()),
        
        # Data Import panel
        shinyjs::hidden(
          div(id = "data_import", uiDataImport())
        )
      ),
                 
      
      # Analysis Tab
      tabItem(tabName = "analysis", uiAnalyse()
      ) # end tabItem
    ) # end tabItems
 ) # end dashboardBody 
) # end ui
 

ui_analyse_only <- shinyUI(
  fluidPage(uiAnalyse())
)








if (!pnl$GWSDAT_Options$ExcelMode) {
    shinyApp(ui = ui, server = server)
} else {
    shinyApp(ui = ui_analyse_only, server = server)
}
