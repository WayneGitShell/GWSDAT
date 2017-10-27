



server <- function(input, output, session) {
  DEBUG_MODE <- TRUE

  # Increase upload file size to 30MB (default: 5MB)
  options(shiny.maxRequestSize = 30*1024^2)
  
 
  if (!exists("APP_RUN_MODE", envir = .GlobalEnv)) 
    APP_RUN_MODE <- "MultiData"
  
  # Flag that indicates whether data was loaded or not.  
  LOAD_COMPLETE <- 100
  dataLoaded <- reactiveVal(0)
  
  
  # List of site data and currently selected site.
  csite_list <- NULL
  csite <- NULL
  csite_selected_idx <- 0
  
  # PSplines settings
  new_psplines_knots <- 0
  prev_psplines_resolution <- "Default"
  prev_psplines_knots <- 6
  
  # Default data set including the Basic and Comprehensive example. Loaded in
  # server mode. 
  default_session_file <- "GWSDAT_Examples.RData"
  
  # Default load options that will be overwritten by dialog boxes. 
  loadOptions <- list(aquifer = NULL, subst_napl = NULL)
  
  
  import_tables <- reactiveValues(DF_conc = NULL, 
                                  DF_well = NULL,
                                  Coord_unit = NULL,
                                  shape_files = NULL,
                                  new_csite = NULL)
  
  # Trigger table rendering only on specific events.
  renderRHandsonConc <- reactiveVal(0)
  renderRHandsonWell <- reactiveVal(0)
  
  # Define supported image formats
  img_frmt <- list("png", "jpg", "pdf", "ps", "wmf", "ppt")
        
  if (!existsPPT() && ("ppt" %in% img_frmt))
    img_frmt <- img_frmt[-which(img_frmt == "ppt")]
   
  # If it is not windows, win.metafile() (wmf) can't be used and ppt as well.
  if (.Platform$OS.type != "windows") {
    if ("wmf" %in% img_frmt) img_frmt <- img_frmt[-which(img_frmt == "wmf")]
    if ("ppt" %in% img_frmt) img_frmt <- img_frmt[-which(img_frmt == "ppt")]  
  }
    
  
  # 
  # Some usefull session objects:
  #
  # session$userData  - store user data here
  # onSessionEnded(callback), onEnded(callback)  - executes when user exits browser
  # isClosed()  - function that return TRUE if client has disconnected  
  #
  
  output$version_info <- renderPrint({
      print(sessionInfo())

      #cat("\n\n** Path to image logo: ", system.file("logo.gif", package = "GWSDAT"), "\n")
      #cat("\n\n** Content of .libPaths():\n\n")
      #sapply(.libPaths(), list.files)
  })
    
  # Clean-up user session.
  session$onSessionEnded(function() {
    
    # Browser Reload also triggers onSessionEnded(). Trying work-around by only
    # stopping server if app is not run in MultiData mode (i.e. in single a.k.a. ExcelMode).
    if (APP_RUN_MODE != "MultiData")
      stopApp()
  })
  
  
  # Reactive element that will trigger inside an observer when Options are saved.
  optionsSaved <- reactive({ 
    input$save_analyse_options 
  })
  
  
  
  ## Plume Diagnostics Panel ###################################################
  
  checkPlumeStats <- reactive({
    #cat("\n* checkPlumeStats()\n")
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Calculating Plume", value = 0)
    on.exit(progress$close())
    
    
    val <- getFullPlumeStats(csite, 
                             substance = input$solute_select_pd, 
                             plume_thresh = input$plume_thresh_pd,
                             ground_porosity = (input$ground_porosity_pd / 100),
                             progressBar = progress
                            )
    
    # If there is any plume mass, show the plot and hide the message text, and vice versa. 
    if (all(is.na(val$mass))) {
      shinyjs::show("plume_diagn_msg_div")
      shinyjs::hide("plume_diagn_plot_div")
      shinyjs::hide("plume_save_btn_div")
    } else {
      shinyjs::show("plume_diagn_plot_div")
      shinyjs::show("plume_save_btn_div")
      shinyjs::hide("plume_diagn_msg_div")
    }

    return(val)
  })
  
  
  output$plume_diagn_msg <- renderUI({
    #cat("* plume_diagn_msg <- renderUI()\n")
    
    # Detect changes in the Options.
    optionsSaved()
    
    # Detect if stats can not be displayed (hides this text box).
    checkPlumeStats()
    
    # Isolate the inputs (so a change in the sidebar does not trigger this fct.)
    isolate(
     HTML(paste0(tags$b(input$solute_select_pd), 
                 ": Unable to calculate plume statistics for a threshold value of ",
                 "<b>", input$plume_thresh_pd, " ug/l</b>. ",
                 # "Select a different plume threshold and retry.",
                 tags$p(),
                 tags$p("Use the ", tags$a(id = "togglePlumeBoundary", "Estimate Boundary", href = "#"), "tab for assistance in selecting a suitable plume threshold value.")
     ))
    )
  
  })
  
  
  output$plume_estimate_plot <- renderPlot({
    #cat("plume_estimate_plot <- renderPlot()\n")
    
    #isolate(plotPlumeEst(csite, input$solute_select_pd, input$plume_thresh_pd))
    plotPlumeEst(csite, input$solute_select_pd, input$plume_thresh_pd)
  })
  
  
  output$plume_diagn_plot <- renderPlot({
    #cat("plume_estimate_plot <- renderPlot()\n")
    
    # Detect changes in the Options.
    # optionsSaved()
    
    # Re-evaluate plume statistics if any reactive expression changes. 
    # The return value is the full plume statistics (for all timesteps). 
    #isolate(plume_stats <- checkPlumeStats())
    plume_stats <- checkPlumeStats()
    
    plotPlumeTimeSeries(plume_stats)
    
    
  })
  
  
  ## Time-Series Panel #########################################################
  
  # Plot time-series window
  output$time_series <- renderPlot({
    
    optionsSaved()
    
    # Update control attributes from reactive variables. 
    csite$ui_attr$conc_unit_selected <<- input$solute_conc
    csite$ui_attr$ts_options[1:length(csite$ui_attr$ts_options)] <<- FALSE
    csite$ui_attr$ts_options[input$ts_true_options] <<- TRUE
    
    plotTimeSeries(csite, input$solute_select_ts, input$sample_loc_select_ts, input$check_threshold)
    
  })

  
  # Re-Aggregate the data in case the aggregation type was changed.
  reaggregateData <- reactive({
    # cat("* entering reaggregateData()\n")
    
    # If 'input$aggregate_select_tt' is not put here, reaggregateData() will not
    # react for the trend table if: 
    #  1st Aggregation is changed in Spatial plot and  
    #  2nd Aggregation is change in trend table.
    input$aggregate_select_sp
    input$aggregate_select_tt
    
    # If nothing changed, return - happens only when session starts.     
    if ((tolower(csite$GWSDAT_Options$Aggby) == tolower(input$aggregate_select_sp)) &&
        (tolower(csite$GWSDAT_Options$Aggby) == tolower(input$aggregate_select_tt)))
      return(FALSE)
    
    # Flag which aggregation input was active.
    sp_changed <- FALSE
    tt_changed <- FALSE
    
    # Detect which aggregation input changed.
    if (tolower(csite$GWSDAT_Options$Aggby) != tolower(input$aggregate_select_sp)) {
      csite$GWSDAT_Options$Aggby <<- input$aggregate_select_sp
      sp_changed <- TRUE
    } else if (tolower(csite$GWSDAT_Options$Aggby) != tolower(input$aggregate_select_tt)) {
      csite$GWSDAT_Options$Aggby <<- input$aggregate_select_tt
      tt_changed <- TRUE
    }
    
    cat("  -> doing reaggregation..\n")
    
    tryCatch(
      agg_data <- aggregateData(csite$All.Data$Cont.Data, 
                                csite$All.Data$GW.Data, 
                                csite$All.Data$NAPL.Thickness.Data,
                                csite$All.Data$sample_loc$data, 
                                csite$GWSDAT_Options$Aggby, 
                                csite$GWSDAT_Options$AggMethod 
      ), error = function(e) {
        showModal(modalDialog(title = "Error", paste0("Failed to aggregate data: ", e$message), easyClose = FALSE))
        return(FALSE)                      
      })
    
    
    # Write back.
    csite$All.Data$Agg_GW_Data <<- agg_data$Agg_GW_Data
    csite$All.Data$NAPL.Thickness.Data <<- agg_data$NAPL.Thickness.Data
    csite$All.Data$Cont.Data <<- agg_data$Cont.Data
    csite$All.Data$All_Agg_Dates <<- agg_data$All_Agg_Dates
    
    
    # Update aggregation dates in the fitted data contaminant table.
    # Note: its a little ankward to fiddle inside the data structure this way. 
    # Maybe change it at some point. Also it assumes that the order of 'AggDate'
    # in 'csite$All.Data$Cont.Data' matches the one in 'csite$Fitted.Data[[cont]]$Cont.Data'.
    # This is how its done on first initialization in fitData(), but an explicit
    # date lookup would be saver.
    for (cont in csite$All.Data$cont_names) {
      # Extract aggregation dates created above for specific contaminant and copy to fitted data table.
      agg_col <- csite$All.Data$Cont.Data$AggDate[which(csite$All.Data$Cont.Data$Constituent == cont)]
      csite$Fitted.Data[[cont]]$Cont.Data$AggDate <<- agg_col
    }
    
    # Calculate Traffic Lights (depends on aggregation date input)
    csite$Traffic.Lights <<- NULL
    
    tryCatch(
      csite$Traffic.Lights <<- calcTrafficLights(csite$All.Data, csite$Fitted.Data, csite$GWSDAT_Options),
      error = function(e) {
        showNotification(paste0("Failed to calculate trend table: ", e$message), type = "error", duration = 10)
      }
    )

    # Calculate groundwater flows (depends on aggregation date input)
    csite$GW.Flows <<- NULL
    
    if (!is.null(csite$All.Data$Agg_GW_Data)) {
      
      tryCatch(
        csite$GW.Flows <<- do.call('rbind', by(csite$All.Data$Agg_GW_Data, csite$All.Data$Agg_GW_Data$AggDate, calcGWFlow)),
        error = function(e) {
          showNotification(paste0("Failed to calculate groundwater flows: ", e$message), type = "error", duration = 10)
        })
      
      if (!is.null(csite$GW.Flows)) {    
        csite$GW.Flows$R <<- csite$GW.Flows$R/quantile(csite$GW.Flows$R, p = 0.9, na.rm = T)
        csite$GW.Flows$R[csite$GW.Flows$R > 1] <<- 1
        csite$GW.Flows <<- na.omit(csite$GW.Flows)    
      }
    }

        
    # Update UI time points of slider.
    dates_tmp <- format(csite$All.Data$All_Agg_Dates, "%d-%m-%Y")
    csite$ui_attr$timepoints   <<- dates_tmp
    
    # Set new time point to last date.
    new_timepoint_idx <- length(dates_tmp)
    
    # Update slider inputs: Spatial plot and in Trend table.
    outp <- pasteAggLimit(csite$ui_attr$timepoints[new_timepoint_idx], csite$GWSDAT_Options$Aggby)
    
    updateSliderInput(session, "timepoint_sp_idx", value = new_timepoint_idx,
                      min = 1, max = length(csite$ui_attr$timepoints), label = paste0("Time: ", outp), step = 1)
    
    updateSliderInput(session, "timepoint_tt_idx", value = new_timepoint_idx,
                      min = 1, max = length(csite$ui_attr$timepoints), label = paste0("Time: ", outp), step = 1)
    
    
    # Update select input: Aggregation in other panel.
    if (sp_changed)
      updateSelectInput(session, "aggregate_select_tt", selected = csite$GWSDAT_Options$Aggby)
    
    if (tt_changed)
      updateSelectInput(session, "aggregate_select_sp", selected = csite$GWSDAT_Options$Aggby)

    return(TRUE)
  })

  
  #  
  # Update the label of the time slider, when slider changes.
  #
  observeEvent(input$timepoint_sp_idx, {
    #cat("* in observeEvent: timepoint_sp_idx\n")
    
    # Not updating here, because 'input$timepoint_sp_idx' is directly used for
    # plotting. Saving to 'csite$ui_attr$timepoint_sp_idx' is only used in 
    # 'Save Session' and reading from it inside rndAnalyse <- renderUI().
    #
    #csite$ui_attr$timepoint_sp_idx <<- input$timepoint_sp_idx
    
    timep <- csite$ui_attr$timepoints[input$timepoint_sp_idx]
    outp <- pasteAggLimit(timep, csite$GWSDAT_Options$Aggby)
    updateSliderInput(session, "timepoint_sp_idx", label = paste0("Time: ", outp))
  })

  observeEvent(input$timepoint_tt_idx, {
    # cat("* in observeEvent: timepoint_tt_idx\n")
    
    # Not updating here, because 'input$timepoint_sp_idx' is directly used for
    # plotting. Saving to 'csite$ui_attr$timepoint_sp_idx' is only used in 
    # 'Save Session' and reading from it inside rndAnalyse <- renderUI().
    #
    #csite$ui_attr$timepoint_tt_idx <<- input$timepoint_tt_idx
    
    timep <- csite$ui_attr$timepoints[input$timepoint_tt_idx]
    outp <- pasteAggLimit(timep, csite$GWSDAT_Options$Aggby)
    updateSliderInput(session, "timepoint_tt_idx", label = paste0("Time: ", outp))
  })
  

  #
  # Plot ImagePlot
  #
  output$image_plot <- renderPlot({
    
    # cat("* entering image_plot()\n")
    
    # React to changes in the Options panel.
    optionsSaved() 

    if (reaggregateData()) {
      #cat("  + (sp) aggregation took place, exiting image_plot()\n")
      return(NULL)
    }
    
    # cat(" + right after reaggregateData()\n")
   
    #Fixme: WHAT IS THIS FOR, NEED THIS HERE
    #val <- plumeThreshChange()
    
    # Update control attributes from reactive variables (Possibly integrate this
    # into function arguments of plotSpatialImage()?).
    csite$ui_attr$spatial_options[1:length(csite$ui_attr$spatial_options)] <<- FALSE
    csite$ui_attr$spatial_options[input$imageplot_options] <<-  TRUE
    csite$ui_attr$gw_selected <<- input$gw_flows
    csite$ui_attr$contour_selected <<- input$imageplot_type
    csite$ui_attr$conc_unit_selected <<- input$solute_conc_contour
   
    plotSpatialImage(csite, input$solute_select_sp, 
                     as.Date(csite$ui_attr$timepoints[input$timepoint_sp_idx], "%d-%m-%Y"))
                     
    
  })
    
  
  
  
  output$trend_table <- renderUI({
    
    # cat("* entering trend_table()\n")
    
    # React to changes in the Options panel.
    optionsSaved() 
  
    # If aggregation took place, return here because the timepoint index has to
    # be updated before the actual plotting happens.
    if (reaggregateData()) {
      cat("  + (tt) aggregation took place, exiting image_plot()\n")
      return(NULL)
    }

    plotTrendTable(csite, as.Date(csite$ui_attr$timepoints[input$timepoint_tt_idx], "%d-%m-%Y"),
               input$trend_or_threshold, input$color_select_tt)
  })

  
  # Plot the legend for the traffic lights table.
  output$trend_legend <- renderUI({plotTrendTableLegend()  })
  

  
  #
  # Plot Well Report
  #
  output$well_report_plot <- renderPlot({
    
    use_log_scale    <- if (input$logscale_wr == "Yes") {TRUE} else {FALSE}
    
    plotWellReport(csite, input$solute_select_wr, input$sample_loc_select_wr, use_log_scale)
    
  })
  
  #
  # Plot SpatioTemporal Predictions
  #
  output$stpredictions_plot <- renderPlot({
    
    use_log_scale <- if (input$logscale_stp == "Yes") {TRUE} else {FALSE}
    
    plotSTPredictions(csite, input$solute_select_stp, input$sample_loc_select_stp, use_log_scale, input$solute_conc_stp)
    
  })
  
  updateNAPL <- function(location, substance) {
    
    tmp_napl <- existsNAPL(csite$All.Data, location, substance) 
    
    # Update checkbox control if NAPL changed.
    if (tmp_napl != csite$ui_attr$napl_present) {
      
      
      if (tmp_napl) 
        csite$ui_attr$ts_options["Overlay NAPL Thickness"] <<- FALSE  # set to some value
      else 
        csite$ui_attr$ts_options <<- csite$ui_attr$ts_options[-which(names(csite$ui_attr$ts_options) == "Overlay NAPL Thickness")]
        
      updateCheckboxGroupInput(session, "ts_true_options", label = "Time Series Plot Options", 
                         choices = names(csite$ui_attr$ts_options),
                         selected = names(which(csite$ui_attr$ts_options == TRUE)))
      
      csite$ui_attr$napl_present <<- tmp_napl
    }
  }
  
  
  # When solute or well changes, update NAPL setting and mirror solute selection
  # to Spatial Plot panel.
  observeEvent({input$solute_select_ts; 
    input$sample_loc_select_ts}, {
    
    updateNAPL(input$sample_loc_select_ts, input$solute_select_ts)  
  
    updateSelectInput(session, "solute_select_sp", selected = input$solute_select_ts ) 
  })
  
  observeEvent(input$solute_select_sp, {
    updateSelectInput(session, "solute_select_ts", selected = input$solute_select_sp )  
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
  
  
    
  
  output$save_timeseries_plot <- downloadHandler(
    
    filename <- function() { 
      paste("timeseries_plot.", input$export_format_ts, sep = "")
    },

    content <-  function(file) {
      
      if (input$export_format_ts == "ppt") {
        
        makeTimeSeriesPPT(csite, input$solute_select_ts, input$sample_loc_select_ts,
                          width  = input$img_width_px  / csite$ui_attr$img_ppi, 
                          height = input$img_height_px / csite$ui_attr$img_ppi)
        
      } else {
        
        if (input$export_format_ts == "png") png(file, width = input$img_width_px, height = input$img_height_px)
        if (input$export_format_ts == "pdf") pdf(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
        if (input$export_format_ts == "ps")  postscript(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
        if (input$export_format_ts == "jpg") jpeg(file, width = input$img_width_px, height = input$img_height_px, quality = input$img_jpg_quality) 
        if (input$export_format_ts == "wmf") win.metafile(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
        
        plotTimeSeries(csite, input$solute_select_ts, input$sample_loc_select_ts)
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
        
        plotSpatialImagePPT(csite, input$solute_select_sp, as.Date(csite$ui_attr$timepoints[input$timepoint_sp_idx], "%d-%m-%Y"),
                       width  = input$img_width_px  / csite$ui_attr$img_ppi,
                       height = input$img_height_px / csite$ui_attr$img_ppi)
      
        } else {
        
          if (input$export_format_sp == "png") png(file, width = input$img_width_px, height = input$img_height_px)
          if (input$export_format_sp == "pdf") pdf(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
          if (input$export_format_sp == "ps") postscript(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
          if (input$export_format_sp == "jpg") jpeg(file, width = input$img_width_px, height = input$img_height_px, quality = input$img_jpg_quality) 
          if (input$export_format_sp == "wmf") win.metafile(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
         
          plotSpatialImage(csite, input$solute_select_sp, as.Date(csite$ui_attr$timepoints[input$timepoint_sp_idx], "%d-%m-%Y"))
          dev.off()
      }
      
    }
  )
  
  #
  # After changing Trend Table to HTML, saving was disabled.
  # -> Maybe create pdf of html table and offer save.
  #
  # output$save_trend_table <- downloadHandler(
  #   
  #   filename <- function() { 
  #     paste("trend_table.", input$export_format_tt, sep = "")
  #   },
  #   
  #   content <-  function(file) {
  #     
  #     if (input$export_format_tt == "ppt") {
  #       
  #       if (input$timepoint_tt == "")
  #         plotTrendTablePPT(csite, as.Date(csite$ui_attr$timepoints[input$timepoint_tt_idx], "%d-%m-%Y"),  input$trend_or_threshold, input$color_select_tt,  
  #                           width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi)
  #       else
  #         plotTrendTablePPT(csite, as.Date(input$timepoint_tt, "%d-%m-%Y"),  input$trend_or_threshold, input$color_select_tt,  
  #                         width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi)
  #       
  #     } else {
  #       
  #       if (input$export_format_tt == "png") png(file, width = input$img_width_px, height = input$img_height_px)
  #       if (input$export_format_tt == "pdf") pdf(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
  #       if (input$export_format_tt == "ps")  postscript(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
  #       if (input$export_format_tt == "jpg") jpeg(file, width = input$img_width_px, height = input$img_height_px, quality = input$img_jpg_quality) 
  #       if (input$export_format_tt == "wmf") win.metafile(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
  #       
  #       if (input$timepoint_tt == "")
  #         plotTrendTable(csite, as.Date(csite$ui_attr$timepoint_tt, "%d-%m-%Y"), input$trend_or_threshold, input$color_select_tt) 
  #       else        
  #         plotTrendTable(csite, as.Date(input$timepoint_tt, "%d-%m-%Y"),  input$trend_or_threshold, input$color_select_tt)
  #       
  #       dev.off()
  #     }
  #     
  #   }
  # )
  
  output$save_wellreport_plot <- downloadHandler(
    
    filename <- function() { 
      paste("wellreport.", input$export_format_wr, sep = "")
    },
    
    content <-  function(file) {
      
      use_log_scale    <- if (input$logscale_wr == "Yes") {TRUE} else {FALSE}
      
      if (input$export_format_wr == "ppt") {
        
        plotWellReportPPT(csite, input$solute_select_wr, input$sample_loc_select_wr, use_log_scale,
                       width  = input$img_width_px_wide  / csite$ui_attr$img_ppi, 
                       height = input$img_height_px_wide / csite$ui_attr$img_ppi)
      } else {
        
        if (input$export_format_wr == "png") png(file, width = input$img_width_px_wide, height = input$img_height_px_wide)
        if (input$export_format_wr == "pdf") pdf(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_wr == "ps") postscript(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_wr == "jpg") jpeg(file, width = input$img_width_px_wide, height = input$img_height_px_wide, quality = input$img_jpg_quality) 
        if (input$export_format_wr == "wmf") win.metafile(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 

        plotWellReport(csite, input$solute_select_wr, input$sample_loc_select_wr, use_log_scale)

        
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
      
      
      if (input$export_format_pd == "ppt") {
        
        plotPlumeTimeSeriesPPT(plume_stats, 
                               width = input$img_width_px_wide / csite$ui_attr$img_ppi, 
                               height = input$img_height_px_wide / csite$ui_attr$img_ppi)
        
      } else {
        
        if (input$export_format_pd == "png") png(file, width = input$img_width_px_wide, height = input$img_height_px_wide)
        if (input$export_format_pd == "pdf") pdf(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_pd == "ps")  postscript(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_pd == "jpg") jpeg(file, width = input$img_width_px_wide, height = input$img_height_px_wide, quality = input$img_jpg_quality) 
        if (input$export_format_pd == "wmf") win.metafile(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        
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
  
  output$save_stpredictions_plot <- downloadHandler(
    
    filename <- function() { 
      paste("stpredictions.", input$export_format_stp, sep = "")
    },
    
    content <-  function(file) {
      
      use_log_scale <- if (input$logscale_stp == "Yes") {TRUE} else {FALSE}
      
      if (input$export_format_stp == "ppt") {
        
        plotSTPredictionsPPT(csite, input$solute_select_stp, input$sample_loc_select_stp, 
                             use_log_scale, input$solute_conc_stp,
                             width = input$img_width_px_wide / csite$ui_attr$img_ppi, 
                             height = input$img_height_px_wide / csite$ui_attr$img_ppi)
        
      } else {
        
        if (input$export_format_stp == "png") png(file, width = input$img_width_px_wide, height = input$img_height_px_wide)
        if (input$export_format_stp == "pdf") pdf(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_stp == "ps")  postscript(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_stp == "jpg") jpeg(file, width = input$img_width_px_wide, height = input$img_height_px_wide, quality = input$img_jpg_quality) 
        if (input$export_format_stp == "wmf") win.metafile(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        
        plotSTPredictions(csite, input$solute_select_stp, input$sample_loc_select_stp, use_log_scale, input$solute_conc_stp)
        
        dev.off()
      }
      
    }
  )
  
  output$save_session_btn <- downloadHandler(
    
    filename <- function() {
      
      fn <- input$session_filename
      pa <- strsplit(fn, "\\.")[[1]]
      
      # If there is no RDS ending, append it.
      if (tolower(pa[length(pa)]) != "rds")
        #fn <- paste0(fn, ".RData")
        fn <- paste0(fn, ".rds")
      
      return(fn)
    },
    
    content <- function(file) {
    
      if (!is.null(csite)) {
        
        # Check if filename is ok.
        csite <<- saveUIAttr(csite, input)
        
        # Create temporary csite_list, that includes the current active data session.
        # This will not overwrite the server csite_list.
        csite_list <- list(csite = csite)
        
        class(csite_list) <- "GWSDAT_DATA_LIST"
        
        #save(file = file, "csite_list")
        saveRDS(csite_list, file = file)
      }
    }
    
  )
  
  
  # Generate PPT with spatial animation.
  observeEvent(input$generate_spatial_anim_ppt, {
    makeSpatialAnimation(csite, input$solute_select_sp,
                         input$img_width_px / csite$ui_attr$img_ppi,
                         input$img_height_px / csite$ui_attr$img_ppi,
                         input$img_width_px_wide / csite$ui_attr$img_ppi,
                         input$img_height_px_wide / csite$ui_attr$img_ppi)
  })
  
  
  # Generate PPT with trend table animation.
  observeEvent(input$generate_trendtable_anim_ppt, {
    makeTrendTableAnimation(csite, input$trend_or_threshold, input$color_select_tt)
  })
  
  
  ## General Import Routines ###################################################
  
  
  # Can I move parts (or all) of this function into importTables?
  importData <- function(dname) {
    
    if (is.null(DF_well <- parseTable(import_tables$DF_well, type = "wells"))) {
      showNotification("Aborting Save: Could not find at least one valid row entry in contaminant table.", 
                       type = "error", duration = 10)      
      return(NULL)
    }
    
    if (is.null(DF_conc <- parseTable(import_tables$DF_conc, type = "contaminant", 
                                 wells = unique(DF_well$WellName), units = conc_units,
                                 flags = conc_flags))) {
      showNotification("Aborting Save: Could not find at least one valid row entry in contaminant table.", 
                       type = "error", duration = 10)      
      return(NULL)
    }
    
    
    # Check if data name is valid, i.e. does not already exists. If getValidDataName()
    # returns a different name than the proposed one (this one), the data name is 
    # already taken. Warn the user and do nothing.
    check_name <- getValidDataName(csite_list, propose_name = dname)
    
    if (check_name != dname) {
      showNotification("Data name already exists. Please enter a unique name that is not present in the Data Manager.", 
                       type = "warning", duration = 10)
      return(NULL)
    }
    
    
    # Create the progress bar.
    progress <- shiny::Progress$new()
    progress$set(message = "Loading data", value = 0)
    on.exit(progress$close())
    
    progress$set(value = 0.1, detail = paste("reading data"))
    
    GWSDAT_Options <- createOptions(dname)
  
    # Change Well Table format to comply with internal format.  
    DF_well <- list(data = DF_well, coord_unit = import_tables$Coord_unit)
    all_data <- formatData(DF_conc, DF_well)
    
    
    
    # Add shape files to GWSDAT_Options if present.
    if (!is.null(import_tables$shape_files)) {
      if (length(import_tables$shape_files$shp_files) > 0) {
        GWSDAT_Options$ShapeFileNames <- import_tables$shape_files$shp_files
      } else {
        showNotification("Ignoring shape files because no .shp file was provided.", 
                         type = "error", duration = 10) 
      }  
    }
    
    
    # Create a unique data set 'csite' for each Aquifer.
    for (Aq_sel in unique(all_data$sample_loc$data$Aquifer)) {
      
      pr_dat <- processData(all_data$solute_data, all_data$sample_loc, GWSDAT_Options, 
                            Aq_sel)
      
      if (is.null(pr_dat)) next
      
      ui_attr <- createUIAttr(pr_dat, GWSDAT_Options)
      
      # Build list with all data.
      csite <<- list(All.Data       = pr_dat,
                     Fitted.Data    = NULL,
                     GWSDAT_Options = GWSDAT_Options,
                     Traffic.Lights = NULL,
                     ui_attr        = ui_attr,
                     Aquifer        = Aq_sel,
                     raw_contaminant_tbl = import_tables$DF_conc,
                     raw_well_tbl = import_tables$DF_well
      )
      
      csite_list[[length(csite_list) + 1]] <<- csite 
      
    }
    
    
    # Flag that the data was loaded.
    isolate(lstate <- dataLoaded())
    
    if (lstate >= LOAD_COMPLETE)
      dataLoaded(lstate + 1)
    else 
      dataLoaded(LOAD_COMPLETE)
    
    # Go back to Data Manager.
    shinyjs::show(id = "uiDataManager")
    shinyjs::hide(id = "uiDataAddNew")
    shinyjs::hide(id = "uiDataAddCSV")
    shinyjs::hide(id = "uiDataAddExcel")
    
    
  }
  
  ## Data Manager Landing ######################################################
  
  
  showDataMng <- function() {
    shinyjs::hide(id = "uiDataAddSession")
    shinyjs::hide(id = "uiDataAddCSV")
    shinyjs::hide(id = "uiDataAddNew")
    shinyjs::hide(id = "uiDataAddExcel")
    shinyjs::hide(id = "uiDataEdit")
    
    shinyjs::show(id = "uiDataManager")
    
  }
  
  
  # Go to Load Session Data (Button click).
  observeEvent(input$add_session_data,  {
    cat("* in observeEvent: add_session_data (line 1189)\n")
    shinyjs::show(id = "uiDataAddSession")
    shinyjs::hide(id = "uiDataManager")
  })
  
  # Go (back) to Data Manager.
  shinyjs::onclick("gotoDataManager_a", showDataMng())
  shinyjs::onclick("gotoDataManager_b", showDataMng())
  shinyjs::onclick("gotoDataManager_c", showDataMng())
  shinyjs::onclick("gotoDataManager_d", showDataMng())
  shinyjs::onclick("gotoDataManager_e", showDataMng())
  
  
  ## Load Session Data ().rds ##################################################
 
  
  output$tbl_conc_sess <- rhandsontable::renderRHandsontable({
    #cat("* in tbl_conc_sess <- renderRHandsontable()\n")
    
    if (is.null(import_tables$DF_conc)) {
      outDF <- data.frame(WellName = character(), Constituent = numeric(),
                         SampleDate = character(), Result = character(),
                         Units = character(), Flags = character())
      
    } else {
      
      # Create Preview DF
      outDF <- import_tables$DF_conc

      # Delete some of the columns
      outDF$ND <- NULL
      outDF$Result.Corr.ND <- NULL
      outDF$XCoord <- NULL
      outDF$YCoord <- NULL
      outDF$AggDate <- NULL

      outDF$SampleDate <- format.Date(outDF$SampleDate, "%d-%m-%Y")
    }
    
    tbl_height <- 700
    
    # Use smaller size for placeholder table (only header).
    if (nrow(outDF) == 0)  tbl_height <- 330 
    
    try(rhandsontable::rhandsontable(outDF, 
                                 useTypes = TRUE, rowHeaders = NULL, stretchH = "all",
                                 height = tbl_height, readOnly = TRUE), silent = T) 
      
    
    
  })
  
  
  output$tbl_well_sess <- rhandsontable::renderRHandsontable({
    cat("* in tbl_well_sess\n")
    
    # Create empty DF with header to display minimal table.
    if (is.null(import_tables$DF_well)) {
      outDF <- data.frame(WellName = character(), XCoord = numeric(),
                         YCoord = numeric(), Aquifer = character())
    } else {
      outDF <- import_tables$DF_well
    }
    
    # Use smaller size for placeholder table (only header).
    tbl_height <- 700
    if (nrow(outDF) == 0)  tbl_height <- 330 
    
    # Create the table with specific height. Always display it even if it has no entries.
    rhandsontable::rhandsontable(outDF, useTypes = TRUE, stretchH = "all",
                                 height = tbl_height, rowHeaders = NULL, readOnly = TRUE) 
    
  })
  
  
  observeEvent(input$data_session_file, {
    cat("* in observeEvent: session_data_file\n")
    
    inFile <- input$data_session_file
    
    if (is.null(inFile)) {
      showNotification("Upload of file failed.", type = "error", duration = 10)
      return(NULL)
    }
    
    # Attempt to read the .rds file into a temporary list.
    tryCatch(   
      csite_tmp <- readRDS(inFile$datapath)
      , error = function(e) {
        showNotification(paste0("Error reading uploaded .rds file ", inFile$name), type = "error", duration = 10 )
        return(NULL)
      })
    
    # Check if data object was read properly - The following checks could be
    #  moved into the tryCatch() above, but this way it is more specific.
    if (!exists("csite_tmp")) {
      showNotification(paste0("Uploaded .rds file ", inFile$name, " does not contain a GWSDAT object."), type = "error", duration = 10 )
      shinyjs::reset("data_session_file")
      return(NULL)
    }
    
    if (class(csite_tmp) != "GWSDAT_DATA_LIST") {
      showNotification(paste0("Uploaded .rds file ", inFile$name, " does not contain data of type GWSDAT (wrong class)."), type = "error", duration = 10 )
      shinyjs::reset("data_session_file")
      return(NULL)
    }
    
    
    # Create new data name if already exists. It needs to be unique.
    site_name <- csite_tmp[[1]]$GWSDAT_Options$SiteName
    new_name <- getValidDataName(csite_list, template = site_name, propose_name = site_name)
    csite_tmp[[1]]$GWSDAT_Options$SiteName <- new_name
    
    updateTextInput(session, "dname_sess", value = new_name)
    
    # Make it possible to delete this from the data manager. 
    csite_tmp[[1]]$DO_NOT_MODIFY <- FALSE
    
    # Set the preview tables displayed on the right of the import panel.
    import_tables$new_csite <- csite_tmp[[1]] 
    import_tables$DF_conc   <- csite_tmp[[1]]$All.Data$Cont.Data
    import_tables$DF_well   <- csite_tmp[[1]]$All.Data$sample_loc$data
    
    
  })
  
  
  # Go to New Data Import (Button click).
  observeEvent(input$add_session_data,  {
    cat("* in observeEvent: add_session_data (line 1680)\n")
    
    shinyjs::hide("uiDataManager")
    shinyjs::show("uiDataAddSession")
    
    import_tables$DF_conc <<- NULL
    import_tables$DF_well <<- NULL
    import_tables$new_csite <<- NULL
    
    output$uiDataAddSession <- renderUI(uiImportSessionData(getValidDataName(csite_list)))
  })
  
  
  observeEvent(input$reset_sess_import,  {
    
    import_tables$DF_well <<- NULL
    import_tables$DF_conc <<- NULL
    import_tables$new_csite <<- NULL
    
    output$uiDataAddSession <- renderUI(uiImportSessionData(getValidDataName(csite_list)))
    
  })
  
  # React to Import button click.
  observeEvent(input$import_button_sess, {
    
    # Check if a data object was loaded. 
    if (is.null(import_tables$new_csite)) { 
      showNotification("Nothing to import. Please upload a valid .rds GWSDAT session file.", 
                       type = "warning", duration = 10)
      return()
    }
    
    # Check if data name is valid, i.e. does not already exists. If getValidDataName()
    # returns a different name than the proposed one (this one), the data name is 
    # already taken. Warn the user and do nothing.
    check_name <- getValidDataName(csite_list, propose_name = input$dname_sess)
    
    if (check_name != input$dname_sess) {
      showNotification("Data name already exists. Please enter a unique name that is not present in the Data Manager.", 
                       type = "warning", duration = 10)
      return()
    }
    
    # Write data name and append to main data list.
    import_tables$new_csite$GWSDAT_Options$SiteName <<- input$dname_sess
    csite_list[[length(csite_list) + 1]] <<- import_tables$new_csite  
    
    shinyjs::show(id = "uiDataManager")
    shinyjs::hide(id = "uiDataAddSession")
    dataLoaded(dataLoaded() + 1)
  })
  
  ## Import New data ###########################################################
  
  createNewConcTable <- function() {
    
    import_tables$DF_conc <- data.frame(matrix("", nrow = 25, ncol = length(conc_header)),
                                        stringsAsFactors = FALSE)
    colnames(import_tables$DF_conc) <- conc_header
    
    class(import_tables$DF_conc$SampleDate) <- "Date"
    
    import_tables$DF_conc$WellName[1] <- "Sample Well"
    import_tables$DF_conc$SampleDate  <- Sys.Date()
    import_tables$DF_conc$Units[1] <- "ug/l"
    
  }
  
  createNewWellTable <- function() {
    
    well_tmp <- data.frame(matrix("", nrow = 25, ncol = length(well_header)),
                           stringsAsFactors = FALSE)
    colnames(well_tmp) <- well_header
    
    well_tmp$WellName[1] <- "Sample Well"
    well_tmp$XCoord[1] <- 50.12345
    well_tmp$YCoord[1] <- 20.12345
    
    import_tables$DF_well <- well_tmp
    import_tables$Coord_unit <- input$coord_unit_nd
    
  }
  
  # Go to New Data Import (Button click).
  observeEvent(input$add_new_data,  {
    cat("* in observeEvent: add_new_data\n")
    
    shinyjs::hide("uiDataManager")
    shinyjs::show("uiDataAddNew")
    
    createNewConcTable()
    createNewWellTable()
    import_tables$shape_files <<- NULL
    
    # Triggers re-rendering of rhandsontable.
    renderRHandsonConc(renderRHandsonConc() + 1)
    renderRHandsonWell(renderRHandsonWell() + 1)
    
    output$uiDataAddNew <- renderUI(uiImportNewData(getValidDataName(csite_list)))
  })
  
  
  # Go to New Data Import (Button click).
  observeEvent(input$reset_nd_import,  {
    cat("* in observeEvent: reset_nd_import\n")
    
    createNewConcTable()
    createNewWellTable()
    import_tables$shape_files <<- NULL
    
    # Triggers re-rendering of rhandsontable.
    renderRHandsonConc(renderRHandsonConc() + 1)
    renderRHandsonWell(renderRHandsonWell() + 1)
    
    # Reset data name.
    updateTextInput(session, "dname_nd", value = getValidDataName(csite_list))
    
  })
  
  
  output$tbl_shape_nd <- rhandsontable::renderRHandsontable({
    cat("* in tbl_shape_nd\n")
    if (is.null(import_tables$shape_files))
      return(rhandsontable::rhandsontable(data.frame(Name = character(), Size = numeric()), 
                                          useTypes = FALSE, rowHeaders = NULL, stretchH = "all",
                                          height = 400, readOnly = TRUE))
    
    createShapeFileList(import_tables$shape_files)
  })

  
  # This will cause setting of 'output$tbl_shape_nd' because import_tables is reactive.
  observeEvent(input$remove_shapefiles_nd, import_tables$shape_files <<- NULL )
  
  # Triggers each time input$tbl_conc_nd (the rhandsontable) changes.
  # Converts the hot table to the data.frame in import_tables.
  observe({
    #cat("* in observe: input$tbl_conc_nd\n")
    
    if (is.null(input$tbl_conc_nd)) {
      DF <- import_tables$DF_conc
    } else {
      DF <- hot_to_r(input$tbl_conc_nd)
    }
    
    import_tables$DF_conc <- DF  # update import tables
  })
  
  
  
  # For some reason double execution of this observer takes place after hitting 
  #  "Add New Data"
  output$tbl_conc_nd <- rhandsontable::renderRHandsontable({
    cat("* in tbl_conc_nd <- renderRHandsontable()\n")
    
    # Isolated because it shall not react to changes in 'import_tables$DF_conc'. 
    # Otherwise there will be too much rendering taking place.
    # As alternative, the reactive variable renderRHandsonConc() below is used to 
    # implement selective reactivity (on enter panel, reset, clear table)
    isolate(DF <- import_tables$DF_conc)
    
    # Observe changes triggered from another place.
    renderRHandsonConc()
    
    # Retrieve well choices (exclude empty string) - this reacts to changes in import_tables$DF_well.
    well_choices <- unique(import_tables$DF_well$WellName)
    well_choices <- as.list(well_choices[which(well_choices != "")]) 

    
    hot <- rhandsontable::rhandsontable(DF, #useTypes = FALSE, 
                                 stretchH = "all", height = 605) %>%
      hot_context_menu(allowColEdit = FALSE) %>% # if useTypes = TRUE, allowColEdit will be FALSE anyway
      hot_col(col = "WellName", type = "dropdown", source = well_choices, strict = TRUE) %>%
      hot_col(col = "Units", type = "dropdown", source = conc_units) %>%
      hot_col(col = "Flags", type = "dropdown", source = conc_flags) 

    # With this other formats still produce "Invalid date". correctFormat is set to TRUE.
    #hot <- hot %>% hot_col(col = "SampleDate", type = "date", allowInvalid = TRUE)
    
    # Tooltip (hot_cell) causes stretchH to be ignored (also in Dev-version 0.3.4.9).
    #hot <- hot %>% hot_cell(1, 1, "The Well name must also appear in the well coordinate table. If not, the row will be ignored.") #%>%
    #hot <- hot %>% hot_cell(1, 2, "The name of the constituent/contaminant can include white spaces and numbers.")

    return(hot)
  })
  
  
  # Triggers each time input$tbl_conc_nd (the rhandsontable) changes
  observe({
    #cat("* in observe: input$tbl_well_nd\n")
    
    if (is.null(input$tbl_well_nd)) {
      DF <- import_tables$DF_well
    } else {
      DF <- hot_to_r(input$tbl_well_nd)
    }
    
    import_tables$DF_well <- DF
  })
  
  
  output$tbl_well_nd <- rhandsontable::renderRHandsontable({
    #cat("\n* in tbl_well_nd <- renderRHandsontable()\n")
    
    isolate(DF <- import_tables$DF_well)
    
    renderRHandsonWell()
    
    hot <- rhandsontable::rhandsontable(DF, useTypes = TRUE, 
                                        stretchH = "all", height = 605) 
      #hot_context_menu(allowColEdit = FALSE) %>% # if useTypes = TRUE, allowColEdit will be FALSE anyway
      #hot_col(col = "WellName", type = "dropdown", source = well_choices, strict = TRUE)
    
     
  })
  
  
  
  observeEvent(input$shape_files_nd, {
    cat("* in observeEvent: shape_files_nd\n")
    
    import_tables$shape_files <<- addShapeFiles(input$shape_files_nd, import_tables$shape_files)
    
    # Switch to 'Shape Files' tab inside tabBox.
    updateTabsetPanel(session, "tabbox_nd_import", selected = "Shape Files")
    
    # Reset the file input, so more files can be added
    shinyjs::reset("shape_files_nd")
    shinyjs::show("removeshp_nd")
  })
  
  
  observeEvent(input$save_button_nd, {
    import_tables$Coord_unit <- input$coord_unit_nd
    
    importData(input$dname_nd)
  })
  
  observeEvent(input$clear_tbl_conc_nd, {
    createNewConcTable()  
    # Triggers re-rendering of rhandsontable.
    renderRHandsonConc(renderRHandsonConc() + 1)
  })
  
  observeEvent(input$clear_tbl_well_nd, {
    createNewWellTable()
    # Triggers re-rendering of rhandsontable.
    renderRHandsonWell(renderRHandsonWell() + 1)
  })
  
  
  observeEvent(input$addrow_tbl_conc_nd, {
    
    DF <- import_tables$DF_conc
    
    # Take last row, modify and append (rbind).
    new_row <- DF[nrow(DF),]
    new_row$Constituent <- ""
    new_row$Result <- ""
    rownames(new_row) <- (nrow(DF) + 1)
    import_tables$DF_conc <- rbind(import_tables$DF_conc, new_row)
    
    # Triggers re-rendering of rhandsontable.
    renderRHandsonConc(renderRHandsonConc() + 1)
    
  })
  
  observeEvent(input$addrow_tbl_well_nd, {
    
    DF <- import_tables$DF_well
    
    new_row <- DF[nrow(DF),]
    rownames(new_row) <- (nrow(DF) + 1)
    
    import_tables$DF_well <- rbind(import_tables$DF_well, new_row)
    
    # Triggers re-rendering of rhandsontable.
    renderRHandsonWell(renderRHandsonWell() + 1)
    
  })
  
  
  
  ## Import CSV data ###########################################################
  
  
  # Re-read uploaded files in case one of the CSV format settings changes.
  observeEvent(c(input$sep, input$quote), {
    cat("* in observeEvnet: sep and quote\n")
    
    # For the contaminant data:
    if (!is.null(input$well_data_csv)) {
      import_tables$DF_conc <<- readConcData(input$well_data_csv$datapath, conc_header, header = TRUE, #input$header, 
                                             sep = input$sep, quote = input$quote)
    }
    
    # For the well data:
    if (!is.null(input$well_coord_csv)) {
      ret <- readWellCoords(input$well_coord_csv$datapath, well_header, header = TRUE, #input$header, 
                                               sep = input$sep, quote = input$quote)
      
      import_tables$DF_well <- ret$data
      import_tables$Coord_unit <- ret$unit
    }
  })
  
  
  output$tbl_conc_csv <- rhandsontable::renderRHandsontable({
    cat("* in tbl_conc_csv\n")
  
    # Create empty table with only header as a placeholder.
    if (is.null(import_tables$DF_conc)) {
      mtmp <- data.frame(WellName = character(), Constituent = numeric(),
                         SampleDate = character(), Result = character(),
                         Units = character(), Flags = character())
      
      isolate(import_tables$DF_conc <<- mtmp)
    }
  
    tbl_height <- 700
 
    # Use smaller size for placeholder table (only header).
    if (nrow(import_tables$DF_conc) == 0)  tbl_height <- 400 
    
    rhandsontable::rhandsontable(import_tables$DF_conc, 
                                 useTypes = TRUE, rowHeaders = NULL, stretchH = "all",
                                 height = tbl_height, readOnly = TRUE)                             
   
  })
  
  
  output$tbl_well_csv <- rhandsontable::renderRHandsontable({
    cat("* in tbl_well_csv\n")
    
    # Create empty DF with header to display minimal table.
    if (is.null(import_tables$DF_well)) {
      mtmp <- data.frame(WellName = character(), XCoord = numeric(),
                         YCoord = numeric(), Aquifer = character())
      isolate( import_tables$DF_well <<- mtmp )
    }
    
    # Use smaller size for placeholder table (only header).
    
    tbl_height <- 700
    if (nrow(import_tables$DF_well) == 0)  tbl_height <- 400 
    
    # Create the table with specific height. Always display it even if it has no entries.
    rhandsontable::rhandsontable(import_tables$DF_well, useTypes = TRUE, stretchH = "all",
                                 height = tbl_height, rowHeaders = NULL, readOnly = TRUE) 
    
  })
  
  #
  # Empty table header does not display correctly after 'Reset'. It is not triggering
  #  for reactive import_tables$shape_files, although the output$
  #
  output$tbl_shape_csv <- rhandsontable::renderRHandsontable({
    #input$reset_csv_import
    cat("* in tbl_shape_csv\n")
    if (is.null(import_tables$shape_files))
      return(rhandsontable::rhandsontable(data.frame(Name = character(), Size = numeric()), 
                                          useTypes = TRUE, rowHeaders = NULL, stretchH = "all",
                                          height = 400, readOnly = TRUE))
    
    createShapeFileList(import_tables$shape_files)
  })
  
  
  observeEvent(input$well_data_csv, {
    
    inFile <- input$well_data_csv
    
    if (is.null(inFile))
      return(NULL)
    
    DF <- readConcData(inFile$datapath, conc_header, header = TRUE, #input$header, 
                       sep = input$sep, quote = input$quote)
    
    # If there was a problem reading the data, reset the file input control and return.
    if (is.null(DF)) {
      shinyjs::reset("well_data_csv")
      return(NULL)
    }
    
    # Save to reactive variable.
    import_tables$DF_conc <<- DF 
    
    # Switch to tabPanel with table
    updateTabsetPanel(session, "tabbox_csv_import", selected = "Contaminant Data")
  })
  
  
  observeEvent(input$well_coord_csv, {
    
    inFile <- input$well_coord_csv
    
    if (is.null(inFile))
      return(NULL)
    
    DF <- readWellCoords(inFile$datapath, well_header, header = TRUE, #input$header, 
                         sep = input$sep, quote = input$quote) 
    
    # If there was a problem reading the data, reset the file input control and return.
    if (is.null(DF)) {
      shinyjs::reset("well_coord_csv")
      return(NULL)
    }
    
    # Save to reactive variable.
    import_tables$DF_well <- DF$data
    import_tables$Coord_unit <- DF$unit
    
    
    # Switch to tabPanel with table.
    updateTabsetPanel(session, "tabbox_csv_import", selected = "Well Coordinates")
  })
  
  
  observeEvent(input$shape_files_csv, {
    cat("* in observeEvent: shape_files_csv\n")
    
    import_tables$shape_files <<- addShapeFiles(input$shape_files_csv, import_tables$shape_files)
    
    # Switch to 'Shape Files' tab inside tabBox.
    updateTabsetPanel(session, "tabbox_csv_import", selected = "Shape Files")
    
    # Reset the file input, so more files can be added
    shinyjs::reset("shape_files_csv")
    shinyjs::show("removeshp_csv")
  })
  
  
  # This will cause setting of 'output$tbl_shape_csv' because import_tables is reactive.
  observeEvent(input$remove_shapefiles_csv, {
    import_tables$shape_files <<- NULL
    shinyjs::hide("removeshp_csv")  
  })
  
  observeEvent(input$import_button_csv, importData(input$dname_csv))
  
  
  ## Import Excel data #########################################################
  
  output$tbl_conc_xls <- rhandsontable::renderRHandsontable({
    
    cat("* in tbl_conc_xls\n")
    
    # Create empty table with only header as a placeholder.
    if (is.null(import_tables$DF_conc)) {
      mtmp <- data.frame(WellName = character(), Constituent = numeric(),
                         SampleDate = character(), Result = character(),
                         Units = character(), Flags = character())
      
      isolate(import_tables$DF_conc <<- mtmp)
    }
    
    tbl_height <- 700
    
    # Use smaller size for placeholder table (only header).
    if (nrow(import_tables$DF_conc) == 0)  tbl_height <- 400 
    
    rhandsontable::rhandsontable(import_tables$DF_conc, 
                                 useTypes = TRUE, rowHeaders = NULL, stretchH = "all",
                                 height = tbl_height, readOnly = TRUE)  
  })
  
  
  output$tbl_well_xls <- rhandsontable::renderRHandsontable({
    cat("* in tbl_well_xls\n")
    
    # Create empty DF with header to display minimal table.
    if (is.null(import_tables$DF_well)) {
      mtmp <- data.frame(WellName = character(), XCoord = numeric(),
                         YCoord = numeric(), Aquifer = character())
      isolate( import_tables$DF_well <<- mtmp )
    }
    
    # Use smaller size for placeholder table (only header).
    tbl_height <- 700
    if (nrow(import_tables$DF_well) == 0)  tbl_height <- 400 
    
    # Create the table with specific height. Always display it even if it has no entries.
    rhandsontable::rhandsontable(import_tables$DF_well, useTypes = TRUE, stretchH = "all",
                                 height = tbl_height, rowHeaders = NULL, readOnly = TRUE) 
    
  })

  
  #
  # Empty table header does not display correctly after 'Reset'. It is not triggering
  #  for reactive import_tables$shape_files, although the output$
  #
  #
  output$tbl_shape_xls <- rhandsontable::renderRHandsontable({
    
    cat("* in tbl_shape_xls\n")
    if (is.null(import_tables$shape_files))
      return(rhandsontable::rhandsontable(data.frame(Name = character(), Size = numeric()), 
                                          useTypes = TRUE, rowHeaders = NULL, stretchH = "all",
                                          height = 400, readOnly = TRUE))
    
    createShapeFileList(import_tables$shape_files)
  })
  
 
  
  # This will cause setting of 'output$tbl_shape_xls' because import_tables is reactive.
  observeEvent(input$remove_shapefiles_xls, {
    import_tables$shape_files <<- NULL 
    shinyjs::hide("removeshp_xls")
    })
  
  observeEvent(input$shape_files_xls, {
    cat("* in observeEvent: shape_files_xls\n")
    
    import_tables$shape_files <<- addShapeFiles(input$shape_files_xls, import_tables$shape_files)
    
    # Switch to 'Shape Files' tab inside tabBox.
    updateTabsetPanel(session, "tabbox_xls_import", selected = "Shape Files")
    
    # Reset the file input, so more files can be added
    shinyjs::reset("shape_files_xls")
    shinyjs::show("removeshp_xls")
  })
  
  #
  #FIXME: Move this to readData.R or excel related file?
  #
  readExcelSheet <- function(filein, sheet) {
    cat("* in readExcelSheet\n")
    
    dtmp <- readExcel(filein, sheet)
    
    if (is.null(dtmp)) 
      return(FALSE)
    
    import_tables$DF_conc <- dtmp$conc_data
    import_tables$DF_well <- dtmp$well_data
    import_tables$Coord_unit <- dtmp$coord_unit
    
    # Disabled for now, because Shape Files are manually uploaded.
    # import_tables$shape_files <<- dtmp$shape_files
    # Switch to tabPanel with table
    updateTabsetPanel(session, "tabbox_xls_import", selected = "Contaminant Data")
    
    return(TRUE)
  }
  
  
  selectExcelSheetModal <- function(sheet_lst) {
    modalDialog(
      selectInput("excelsheet", "Choose data sheet", choices = sheet_lst),
      span('Select the Excel sheet that contains the GWSDAT data'),
      #if (failed)
      #    div(tags$b("Invalid name of data object", style = "color: red;")),
      
      footer = tagList(
        actionButton("cancelExcelSheet", "Cancel"),
        actionButton("okExcelSheet", "OK")
      )
    )
  }
  
  
  observeEvent(input$excel_import_file, {
    
    
    sheet_lst <- NULL
    
    # Attempt to read out sheets, which can be selected by user. 
    tryCatch(
      sheet_lst <- excel_sheets(input$excel_import_file$datapath),
    error = function(e) {
      showNotification(paste0("Failed to retrieve Excel sheets with error: ", e$message), 
                       type = "error", duration = 10)
      shinyjs::reset("excel_import_file")
    })
  
    # 'sheet_lst' will _stay_ NULL if excel_sheets() fails. 
    if (!is.null(sheet_lst)) {

      if (length(sheet_lst) > 1) {
        # select sheet from dropdown
        showModal(selectExcelSheetModal(sheet_lst))
      } else {
        readExcelSheet(input$excel_import_file, sheet_lst[[1]])
      }
    }
  }) 
  
  
  observeEvent(input$cancelExcelSheet, {
    
    removeModal()
    
    # If no data was previously loaded, reset the input file control.  
    if (is.null(import_tables$DF_conc))
      shinyjs::reset("excel_import_file")
    
  })
  
  observeEvent(input$okExcelSheet, {
    cat("* in observeEvent: input$okExcelSheet\n")
    
    # Attempt to read the sheet, if it succeeds, remove the modal dialog.      
    if (readExcelSheet(input$excel_import_file, input$excelsheet))
      removeModal()
    
  })
  
  
  observeEvent(input$import_button_xls, importData(input$dname_xls))
  
  
  ## Edit Data #################################################################
  
  # Triggers each time input$tbl_conc_nd (the rhandsontable) changes.
  # Converts the hot table to the data.frame in import_tables.
  observe({
    cat("* in observe: input$tbl_conc_ed\n")
    
    if (is.null(input$tbl_conc_ed)) {
      DF <- import_tables$DF_conc
    } else {
      DF <- hot_to_r(input$tbl_conc_ed)
    }
    
    import_tables$DF_conc <- DF  # update import tables
  })
  
  
  
  # For some reason double execution of this observer takes place after hitting 
  #  "Add New Data"
  output$tbl_conc_ed <- rhandsontable::renderRHandsontable({
    cat("* in tbl_conc_ed <- renderRHandsontable()\n")
    
    # Isolated because it shall not react to changes in 'import_tables$DF_conc'. 
    # Otherwise there will be too much rendering taking place.
    # As alternative, the reactive variable renderRHandsonConc() below is used to 
    # implement selective reactivity (on enter panel, reset, clear table)
    isolate(DF <- import_tables$DF_conc)
    
    # Observe changes triggered from another place.
    renderRHandsonConc()
    
    # Retrieve well choices (exclude empty string) - this reacts to changes in import_tables$DF_well.
    well_choices <- unique(import_tables$DF_well$WellName)
    well_choices <- as.list(well_choices[which(well_choices != "")]) 
    

    hot <- rhandsontable::rhandsontable(DF, #useTypes = FALSE, 
                                        stretchH = "all", height = 605) %>%
      hot_context_menu(allowColEdit = FALSE) %>% # if useTypes = TRUE, allowColEdit will be FALSE anyway
      hot_col(col = "WellName", type = "dropdown", source = well_choices, strict = TRUE) %>%
      hot_col(col = "Units", type = "dropdown", source = conc_units) %>%
      hot_col(col = "Flags", type = "dropdown", source = conc_flags) 
    
    # With this other formats still produce "Invalid date". correctFormat is set to TRUE.
    #hot <- hot %>% hot_col(col = "SampleDate", type = "date", allowInvalid = TRUE)
    
    # Tooltip (hot_cell) causes stretchH to be ignored (also in Dev-version 0.3.4.9).
    #hot <- hot %>% hot_cell(1, 1, "The Well name must also appear in the well coordinate table. If not, the row will be ignored.") #%>%
    #hot <- hot %>% hot_cell(1, 2, "The name of the constituent/contaminant can include white spaces and numbers.")
    
    return(hot)
  })
  

  observe({
    cat("* in observe: input$tbl_well_ed\n")
    
    if (is.null(input$tbl_well_ed)) {
      DF <- import_tables$DF_well
    } else {
      DF <- hot_to_r(input$tbl_well_ed)
    }
    
    import_tables$DF_well <- DF
  })
  
  
  output$tbl_well_ed <- rhandsontable::renderRHandsontable({
    cat("\n* in tbl_well_ed <- renderRHandsontable()\n")
    
    isolate(DF <- import_tables$DF_well)
    
    renderRHandsonWell()
    
    hot <- rhandsontable::rhandsontable(DF, useTypes = TRUE, stretchH = "all", height = 605) 
  })
  
  
  observeEvent(input$save_button_ed, {
    
    import_tables$Coord_unit <- input$coord_unit_ed

     if (!input$coord_unit_ed %in% coord_units) {
      showNotification("Coordinate unit is not valid. Use \'metres\' or \'feet\'.", type = "error", duration = 10)
      return(NULL)
    }
    
    if (input$dname_ed == "") {
      showNotification("Data name can not be an empty string.", type = "error", duration = 10)
      return(NULL)
    }
    
    # If the name changed, write to csite_list and notify that Data Manager 
    # needs a re-render.
    if (csite$GWSDAT_Options$SiteName != input$dname_ed) {
      
      # Check if any other data set has the new name. 
      check_name <- getValidDataName(csite_list, propose_name = input$dname_ed)
      
      if (check_name != input$dname_ed) {
        showNotification("Data name already exists. Please enter a unique name that is not taken by any other data set.", 
                         type = "warning", duration = 10)
        return(NULL)
      }
      
      # Change the name inside the original data list. 
      for (i in csite_selected_idx) 
        csite_list[[i]]$GWSDAT_Options$SiteName <<- input$dname_ed
      
      # Signal the Data Manager List to be re-rendered.
      dataLoaded(dataLoaded() + 1)
    }
     
    #FIXME: Do I really need to update everything when only the coordinate unit changes?
    # Force update to be on the save side.
    needs_processing <- FALSE
    if (input$coord_unit_ed != csite$All.Data$sample_loc$coord_unit)
      needs_processing <- TRUE
    
    # Check if contaminant table changed.
    if (!isTRUE( all.equal(import_tables$DF_conc, csite$raw_contaminant_tbl, check.attributes = FALSE)))
      needs_processing <- TRUE
    
    # Check if well table changed.
    if (!isTRUE( all.equal(import_tables$DF_well, csite$raw_well_tbl, check.attributes = FALSE)))
      needs_processing <- TRUE
    
    
    if (needs_processing) {
      
      # Do import by creating novel data sets. This is very similar to importData(),
      # but a little slimmer: 
      #    - no shape file handling instead the shape data is copied.
      #    - GWSDAT_Options is not created from scratch but copied.
      
      if (is.null(DF_well <- parseTable(import_tables$DF_well, type = "wells"))) {
        showNotification("Aborting Save: Could not find at least one valid row entry in contaminant table.", 
                         type = "error", duration = 10)      
        return(NULL)
      }
      
      if (is.null(DF_conc <- parseTable(import_tables$DF_conc, type = "contaminant", 
                                        wells = unique(DF_well$WellName), units = conc_units,
                                        flags = conc_flags))) {
        showNotification("Aborting Save: Could not find at least one valid row entry in contaminant table.", 
                         type = "error", duration = 10)      
        return(NULL)
      }
      
      # Copy Options. No need to keep information on shape file path. The actual
      # shape data is located in 'csite$All.Data$shape_file_data', which is copied
      # further below.
      GWSDAT_Options <- csite$GWSDAT_Options
      GWSDAT_Options$ShapeFileNames <- NULL
      
      # Change Well Table format to comply with internal format.  
      DF_well <- list(data = DF_well, coord_unit = import_tables$Coord_unit)
      all_data <- formatData(DF_conc, DF_well)
      
      # Create a unique data set 'csite' for each Aquifer.
      for (Aq_sel in unique(all_data$sample_loc$data$Aquifer)) {
        
        pr_dat <- processData(all_data$solute_data, all_data$sample_loc, 
                              GWSDAT_Options, Aq_sel, verbose = FALSE)
        
        # Copy shape data.
        pr_dat$shape_data <- csite$All.Data$shape_data
        
        if (is.null(pr_dat)) next
        
        ui_attr <- createUIAttr(pr_dat, GWSDAT_Options)
        
        # Build list with all data.
        ctmp <- list(All.Data       = pr_dat,
                     Fitted.Data    = NULL,
                     GWSDAT_Options = GWSDAT_Options,
                     Traffic.Lights = NULL,
                     ui_attr        = ui_attr,
                     Aquifer        = Aq_sel,
                     raw_contaminant_tbl = import_tables$DF_conc,
                     raw_well_tbl = import_tables$DF_well
        )
        
        csite_list[[length(csite_list) + 1]] <- ctmp 
        
      }
      
      # Now we need to delete the original data sets. We did not just replace the
      # original data sets because the number of Aquifer can change and, thus, the
      # number of data sets. 
      
      tmplist <- list()
      
      # Loop over the data list and copy those not matching csite_selected_idx.
      for (i in 1:length(csite_list)) {
        
        if (!(i %in% csite_selected_idx))
          tmplist[[length(tmplist) + 1]] <- csite_list[[i]]
      }
      
      # Write back the temporary buffer that contains the updated data list.
      csite_list <<- tmplist

      # Signal re-rendering of Data Manager List.
      dataLoaded(dataLoaded() + 1)
      
    } 
    
    shinyjs::show(id = "uiDataManager")
    shinyjs::hide(id = "uiDataEdit")
   
  })
  
  
  # Restore data.
  observeEvent(input$reset_ed_data,  {
    #cat("* in observeEvent: reset_ed_data\n")
     
     
    # Write back the original data.
    import_tables$DF_conc <- csite$raw_contaminant_tbl
    import_tables$DF_well <- csite$raw_well_tbl
    
    # Triggers re-rendering of rhandsontable.
    renderRHandsonConc(renderRHandsonConc() + 1)
    renderRHandsonWell(renderRHandsonWell() + 1)
    
    # Reset data name.
    updateTextInput(session, "dname_ed", value = csite$GWSDAT_Options$SiteName)
    
  })
  
  
  observeEvent(input$addrow_tbl_conc_ed, {
    
    DF <- import_tables$DF_conc
    
    # Take last row, modify and append (rbind).
    new_row <- DF[nrow(DF),]
    new_row$Constituent <- ""
    new_row$Result <- ""
    rownames(new_row) <- (nrow(DF) + 1)
    import_tables$DF_conc <- rbind(import_tables$DF_conc, new_row)
    
    # Triggers re-rendering of rhandsontable.
    renderRHandsonConc(renderRHandsonConc() + 1)
    
  })
  
  observeEvent(input$addrow_tbl_well_ed, {
    
    DF <- import_tables$DF_well
    
    new_row <- DF[nrow(DF),]
    rownames(new_row) <- (nrow(DF) + 1)
    
    import_tables$DF_well <- rbind(import_tables$DF_well, new_row)
    
    # Triggers re-rendering of rhandsontable.
    renderRHandsonWell(renderRHandsonWell() + 1)
    
  })
  
  
  ## Analyis Panel #############################################################
  
    
 
  
  
  # Follow link to 'Boundary Estimate' tabPanel.
  shinyjs::onclick("togglePlumeBoundary", {
    updateTabsetPanel(session, "plume_tab_box", selected = "plume_pnl_2")
  })



    
  #
  # Display the "Generate PPT Animation" button if Powerpoint is available.
  #
  observeEvent(input$analyse_panel, {
    #cat("* in observe input$analyse_panel") 
    # If Powerpoint export is possible, show the "Generate PPT Animation" button.
    #if (existsPPT()) {
    #  shinyjs::show(id = "save_spatial_ppt_anim")
    #  shinyjs::show(id = "save_trendtable_ppt_anim")
    #}
  
    
    # Update session file name with current time stamp.
    if (input$analyse_panel == "Save Session")
      updateTextInput(session, "session_filename", value = paste0("GWSDAT_", gsub(":", "_", gsub(" ", "_", Sys.time())), ".rds"))
      
    if (input$analyse_panel == "Options") {
      # Save parameters that might have to be restored later if they are invalid.
      prev_psplines_resolution <<- input$psplines_resolution
      prev_psplines_knots <<- input$psplines_knots
      
    }
  })
  
  
  # These inputs will modify the plume threshold for each substance, 
  #  saved in csite$ui_attr$plume_thresh.
  output$thres_plume_select <- renderUI({
   
    num_subst <- length(csite$ui_attr$plume_thresh)
    
    lapply(1:num_subst, function(i) {
      div(style = "display: inline-block;", 
          
          #
          # Note, I use a number for the input id instead of the substance name,
          #  which could have unusual characters or whitespaces. I will need to 
          #  extract and match back the number to what is in ui_attr$plume_thresh.
          #
          numericInput(paste("plume_thresh_", i, sep = ""), 
                       label = names(csite$ui_attr$plume_thresh)[i], 
                       value = csite$ui_attr$plume_thresh[i], 
                       width = "100px")
      )
    })
  })
  
  
  # These inputs will modify the concentration thresholds for each substance, 
  #  saved in csite$ui_attr$conc_thresh.
  output$thres_conc_select <- renderUI({
    
    num_subst <- length(csite$ui_attr$conc_thres)
    
    lapply(1:num_subst, function(i) {
      div(style = "display: inline-block;", 
          
          #
          # Note, I use a number for the input id instead of the substance name,
          #  which could have unusual characters or whitespaces. I will need to 
          #  extract and match back the number to what is in ui_attr$plume_thresh.
          #
          numericInput(paste("conc_thresh_", i, sep = ""), 
                       label = names(csite$ui_attr$conc_thresh)[i], 
                       value = csite$ui_attr$conc_thresh[i], 
                       width = "100px")
      )
    })
  })
  
  
  
  changeModelSettingorNotModal <- function(sheet_lst) {
    modalDialog(
      #selectInput("excelsheet", "Choose data sheet", choices = sheet_lst),
      span('You changed the model resolution. This will cause the model to be re-fitted. Do you want to continue?'),
      #if (failed)
      #    div(tags$b("Invalid name of data object", style = "color: red;")),
      
      footer = tagList(
        actionButton("cancelModSetting", "Cancel"),
        actionButton("okModSetting", "Proceed")
      )
      )
  }
  
  observeEvent(input$cancelModSetting, {
    
    # Revert input to previous resolution setting
    updateSelectInput(session, "psplines_resolution", selected = prev_psplines_resolution)
    updateTextInput(session, "psplines_knots", value = prev_psplines_knots)
    
    removeModal()
  })
    
  
  #' Re-fit the model with the new model resolution setting, i.e. number of knots.
  observeEvent(input$okModSetting, {
    
    if (new_psplines_knots == csite$GWSDAT_Options[['PSplineVars']][['nseg']]) 
     return()
    
    
    # Re-Fit the data with the new PSplines setting inside GWSDAT_Options.
    # previous_knots <- csite$GWSDAT_Options[['PSplineVars']][['nseg']]
    csite$GWSDAT_Options[['PSplineVars']][['nseg']] <<- new_psplines_knots
    
    fitdat = fitData(csite$All.Data, csite$GWSDAT_Options)
    
    # On failure, revert to previous settings
    if (is.null(fitdat)) {
     showNotification("Fitting data with updated model resolution failed. Reverting to previous resolution setting.", type = "error", duration = 10)
     
     csite$GWSDAT_Options[['PSplineVars']][['nseg']] <<- prev_psplines_knots
     updateSelectInput(session, "psplines_resolution", selected = prev_psplines_resolution)
     updateTextInput(session, "psplines_knots", value = prev_psplines_knots)
    } else {
      
      # Update the current data.
      csite$Fitted.Data    <<- fitdat$Fitted.Data
      csite$Traffic.Lights <<- fitdat$Traffic.Lights
      csite$GW.Flows       <<- fitdat$GW.Flows
      
      # Copy back the altered csite list.
      csite_list[[csite_selected_idx]] <<- csite  
      
      # Save the current state, in case it is changed again and fails.
      prev_psplines_resolution <<- input$psplines_resolution
      prev_psplines_knots <<- input$psplines_knots
    }      
    
    
    removeModal()
  })
  
  
  #' Update the number of knots in the text field to reflect the resolution.
  observeEvent(input$psplines_resolution, {
    
    nknots <- 6
    
    if (input$psplines_resolution == "Default")
      nknots <- 6
    if (input$psplines_resolution == "High")
      nknots <- 8
    
    updateTextInput(session, "psplines_knots", value = nknots)
  })
  
  
  observeEvent(input$save_analyse_options, {
    
    # cat("* in observeEvent: save_analyse_options\n")
   
    input_knots <- as.numeric(input$psplines_knots)
    # Check if the value changed, if so, refit all data.
    if ( input_knots != csite$GWSDAT_Options[['PSplineVars']][['nseg']]) {
      
      # Check if value is in boundaries.
      if (input_knots < 2 || input_knots > 12) {
        showNotification("Number of segments for the model is out of bounds. Minimum: 2, Maximum: 12.", type = "error", duration = 10)
        updateTextInput(session, "psplines_knots", value = prev_psplines_knots)
      } else {
        # Ask if to change it. The actual fit is calculated when the actionButton
        # is pressed inside the modal dialog.
        new_psplines_knots <<- input_knots
        showModal(changeModelSettingorNotModal())
      
        
      }
    }
   
    
    # Retrieve the substance concentration thresholds
    num_subst <- length(csite$ui_attr$conc_thresh)
    for (i in 1:num_subst) {
      
      # Create input variable name and evaluate the string as variable. 
      input_var <- paste("input$conc_thresh_", i, sep = "")
      csite$ui_attr$conc_thresh[i] <<- eval(parse(text = input_var))
    }
    
    
    # Retrieve the plume concentration thresholds
    num_subst <- length(csite$ui_attr$plume_thresh)
    for (i in 1:num_subst) {
      
      # Create input variable name and evaluate the string as variable. 
      input_var <- paste("input$plume_thresh_", i, sep = "")
      csite$ui_attr$plume_thresh[i] <<- eval(parse(text = input_var))
    }
    
    csite$ui_attr$ground_porosity <<- input$ground_porosity
    
    shinyjs::show(id = "options_save_msg", anim = TRUE, animType = "fade")
    
    shinyjs::delay(2000, shinyjs::hide(id = "options_save_msg", anim = TRUE, animType = "fade"))
    # Retrieve image settings .. 
    # I might only have to use this when saving a session. Right now the 
    # input$img_* attributes are used directly.
    #csite$ui_attr$img_jpg_quality <<- input$img_jpg_quality 
    
  })
  
  
  output$options_saved <- renderText({paste("Changes Saved") })
      
    
  shinyjs::onclick("GoToDataSelect", {
    shinyjs::hide("analyse_page")
    shinyjs::show("data_select_page")
  })
  
  observeEvent(input$sidebar_menu, {
    
    if (input$sidebar_menu == "menu_analyse") {
      shinyjs::hide("analyse_page")
      shinyjs::show("data_select_page")
    }
    
    # Click on side bar menu, shows main data manager and hides everything else.
    if (input$sidebar_menu == "menu_data_manager") {
      shinyjs::show(id = "uiDataManager")
      
      shinyjs::hide(id = "uiDataAddNew")
      shinyjs::hide(id = "uiDataAddCSV")
      shinyjs::hide(id = "uiDataAddExcel")
      shinyjs::hide(id = "uiDataAddSession") 
      shinyjs::hide(id = "uiDataEdit")
    }
    
  })
    
  
  
  
  loadDefaultSessions <- function() {
    #cat("* in loadDefaultSessions()\n")
    
    infile <- system.file("extdata", default_session_file, package = "GWSDAT")
    
    csite_list <- NULL
    
    # This should never trigger a warning, since I am putting the file there (only if package is broken).
    tryCatch( load(infile),
              warning = function(w) showNotification(paste0("Failed to load default_session_file \'", default_session_file, "\' from package GWSDAT."), type = "error", duration = 7))

    if (is.null(csite_list))
      return(NULL)
    
    csite_list <<- csite_list
    csite <<- csite_list[[1]]
      
    
    dataLoaded(LOAD_COMPLETE)
    
  }
  
  
  #
  # Would like to move this fct to another file, however,
  #   it uses the reactive variabled dataLoaded. How to fix this?
  #
  loadDataSet <- function() {

    cat("* in loadDataSet()\n")
    
    # Load 'session_file' if specified in launchApp().
    if (exists("session_file", envir = .GlobalEnv)) {
      csite_list <- NULL
      
      tryCatch( load(session_file), warning = function(w) 
        showModal(modalDialog(title = "Error", w$message, easyClose = FALSE))
      )
      
      if (is.null(csite_list))
        return(FALSE)
      
      csite_list <<- csite_list
      csite <<- csite_list[[1]]
      
      dataLoaded(LOAD_COMPLETE)
      return(TRUE)  
    }
    
    # Create Options in case they don't exist.
    if (!exists("GWSDAT_Options", envir = .GlobalEnv)) 
      GWSDAT_Options <-  createOptions()
    
    Aq_sel <- loadOptions$aquifer
    subst_napl <- loadOptions$subst_napl
    
    # Load the data from the .csv files.
    solute_data <- well_data <- NULL
    
    # Read Well data and coordinates from file.
    tryCatch({
      solute_data <- readConcData(GWSDAT_Options$WellDataFilename, conc_header)
      well_data <- readWellCoords(GWSDAT_Options$WellCoordsFilename, well_header)
    }, warning = function(w) showModal(modalDialog(title = "Error", w$message, easyClose = FALSE)))
    
    
  
    if (is.null(solute_data) || is.null(well_data))
      return(NULL)
    
    all_data <- formatData(solute_data, well_data)
    
    # Extract list of Aquifer. If there is more than one, return the list.
    Aq_list <- unique(all_data$sample_loc$data$Aquifer)
    
    if ((length(Aq_list) > 1) && is.null(Aq_sel)) {
      class(Aq_list) <- "Aq_list"
      return(Aq_list)
    }
    
    if (is.null(Aq_sel)) 
      Aq_sel <- Aq_list[[1]]

   
    pr_dat <- processData(all_data$solute_data, all_data$sample_loc, GWSDAT_Options, 
                          Aq_sel, subst_napl_vals = subst_napl)
    
    if (class(pr_dat) == "dialogBox")
      return(pr_dat)
      
    
    # Some Error occured.
    if (is.null(pr_dat))
      return(NULL)
    
    fitdat = fitData(pr_dat, GWSDAT_Options)
    
    if (is.null(fitdat))
      return(NULL)
    
    # Create UI attributes
    ui_attr <- createUIAttr(pr_dat, GWSDAT_Options)
    
    # Build list with all data.
    csite <<- list(All.Data       = pr_dat,
                   Fitted.Data    = fitdat$Fitted.Data,
                   GWSDAT_Options = GWSDAT_Options,
                   Traffic.Lights = fitdat$Traffic.Lights,
                   GW.Flows       = fitdat$GW.Flows,
                   ui_attr        = ui_attr,
		               Aquifer        = Aq_sel,
		               raw_contaminant_tbl = solute_data,
		               raw_well_tbl   = well_data$data
    )
    
    # Save csite to the list of csites and remember index.
    curr_idx <- length(csite_list) + 1
    csite_list[[curr_idx]] <<- csite 
    csite_selected_idx <<- curr_idx
    
    
    # Flag that data was fully loaded.
    dataLoaded(LOAD_COMPLETE)
    
    return(TRUE)
  }  
  
    
  
  # List of observers for Analyse buttons, one for each data set.
  obsAnalyseBtnList <- list()
  
  
  
  output$uiAnalyseDataList <- renderUI({
    
    
    if (dataLoaded() < LOAD_COMPLETE) 
      loadDataSet()
    
    html_out <- h3("Select Data Set")
    
    if (length(csite_list) == 0) {
      html_out <- tagList(html_out,
                         shinydashboard::box(width = 7, title = "Data Missing", status = "primary",
                             "Load session data (add link) or import data (add link)."
                         )
      )
      
    } else {
      # Data is present: Retrieve information on datasets and create an observer
      # (button select click) that selects a specific data set for analysis.
      data_sets <- getDataInfo(csite_list)

      databoxes <- as.list(1:length(data_sets))
      
      databoxes <- lapply(databoxes, function(i) {
        
          # Name of the Select button for each data set. 
          btName <- paste0("analyse_btn", i)
        
          #  Creates an observer only if it doesn't already exists.
          if (is.null(obsAnalyseBtnList[[btName]])) {
            
            # Store observer function in list of buttons.
            obsAnalyseBtnList[[btName]] <<- observeEvent(input[[btName]], {
              
              # Retrieve the aquifer select input value.
              aquifer <- eval(parse(text = paste0("input$aquifer_select_", i)))
              
              # Get list index of selected data and aquifer.
              j <- data_sets[[i]]$csite_idx[which(data_sets[[i]]$Aquifer == aquifer)]
              
              # If it was not fitted before, do it now.
              if (is.null(csite_list[[j]]$Fitted.Data)) {
               
                fitdat <- fitData(csite_list[[j]]$All.Data, csite_list[[j]]$GWSDAT_Options)
                
                if (is.null(fitdat)) {
                  showNotification("Fitting data failed. Aborting.", type = "error", duration = 10)
                }
                
                csite_list[[j]]$Fitted.Data    <<- fitdat$Fitted.Data
                csite_list[[j]]$Traffic.Lights <<- fitdat$Traffic.Lights
                csite_list[[j]]$GW.Flows       <<- fitdat$GW.Flows
                
              }

              # Make selected data set active and remember index (to save back 
              # altered csite objects, which are copies, not references).
              csite <<- csite_list[[j]]
              csite_selected_idx <<- j

              
              shinyjs::hide("data_select_page")
              shinyjs::show("analyse_page")
              
              # Triggers renderUI() of Analyse panel
              # Fixme: Also triggers observer. I tried a separate reactive variable 
              #        that is only observed by output$rndAnalyse, but it will also 
              #        trigger here again. 
              dataLoaded(dataLoaded() + 1)
              
              
            })
          }
      })
      
      # Create a shinydashboard box for each data set. Include a choice for the 
      # Aquifer and the select button. 
      for (i in 1:length(data_sets)) {
        
        set_name <- names(data_sets)[i]
        
        html_out <- tagList(html_out, fluidRow(
          shinydashboard::box(width = 7, status = "primary", collapsible = TRUE,
              title = set_name, 
                 
              div(style = "display: inline-block", 
                  selectInput(paste0("aquifer_select_", i), label = "Select Aquifer",
                          choices  = data_sets[[set_name]]$Aquifer,
                          selected = data_sets[[set_name]]$Aquifer[1], 
                          width = '150px')
              ),
              div(style = "display: inline-block; float : right", 
                  actionButton(paste0("analyse_btn", i), "Select")
              )
        )))
        
      }
      
    }
    
    return(html_out)
    
  })

  
  
  
  
  
  
  # Go to .CSV Data Import (Button click).
  observeEvent(input$add_csv_data,  {
    #cat("* in observeEvent: add_csv_data\n")
    
    shinyjs::hide("uiDataManager")
    shinyjs::show("uiDataAddCSV")
    
    import_tables$DF_conc <<- NULL
    import_tables$DF_well <<- NULL
    import_tables$shape_files <<- NULL
    
    output$uiDataAddCSV <- renderUI(uiImportCSVData(getValidDataName(csite_list)))
  })
  

  observeEvent(input$reset_csv_import,  {
    cat("* in observeEvent: reset_csv_import\n")

    import_tables$DF_conc <<- NULL
    import_tables$DF_well <<- NULL
    import_tables$shape_files <<- NULL
    
    output$uiDataAddCSV <- renderUI(uiImportCSVData(getValidDataName(csite_list)))
  })
  
  
  
  # Go to Excel Data Import (Button click).
  observeEvent(input$add_excel_data,  {
    cat("* in observeEvent: add_excel_data\n")

    shinyjs::hide(id = "uiDataManager")    
    shinyjs::show(id = "uiDataAddExcel")

    import_tables$DF_conc <<- NULL
    import_tables$DF_well <<- NULL
    
    output$uiDataAddExcel <- renderUI(uiImportExcelData(csite_list)) 
  })
  
  observeEvent(input$reset_xls_import, {
    cat("* in observeEvent: reset_xls_import\n")
    
    import_tables$DF_well <<- NULL
    import_tables$DF_conc <<- NULL
    import_tables$shape_files <<- NULL
    
    output$uiDataAddExcel <- renderUI(uiImportExcelData(csite_list))                             
  })
  
  obsDelBtnList <- list()
  obsEditBtnList <- list()
  
  
  createDelBtnObserver <- function(btns) {
    
    # Check if a Delete button was created.
    if (length(btns) > 0) {
      cat(" + creating del button observers\n")
      databoxes <- as.list(1:length(btns))
      databoxes <- lapply(databoxes, function(i) {
        
        # Extract the button name and the associated data name. Deletion is 
        #  going to occur based on the data name. 
        #FIXME: Maybe safer to use a unique ID.
        btn_name <- btns[[i]]$btn_name
        csite_name <- btns[[i]]$csite_name
        
        #  Creates an observer only if it doesn't already exists.
        if (is.null(obsDelBtnList[[btn_name]])) {
          
          # Store observer function in list of buttons.
          obsDelBtnList[[btn_name]] <<- observeEvent(input[[btn_name]], {
            cat("* observeEvent: button clicked: ", btn_name, "\n")
            
            # Copy to temporary buffer
            tmplist <- list()
            
            # Loop over the data list and copy names not matching 'del_csite_name'.
            for (i in 1:length(csite_list)) {
              
              # If the name is not matching, copy the data to the temporary list.
              if (csite_list[[i]]$GWSDAT_Options$SiteName != csite_name)
                tmplist[[length(tmplist) + 1]] <- csite_list[[i]]
            }
            
            # Write back the temporary buffer that contains the new data excluding
            # the data set specified in 'del_csite_name'.
            csite_list <<- tmplist
            
            # Need this to trigger observer that re-displays the new data list.
            dataLoaded(dataLoaded() + 1)
            
            
          })
        } else {
          # This should never happen but make sure the very unlikely case shows up.
          stop("Attempting to create Delete button with already existing ID. Aborting. Fix this!")
        }
      }) # end of lapply
    }
  }
  
  createEditBtnObserver <- function(btns) {
    
    # Check if a Delete button was created.
    if (length(btns) > 0) {
      cat(" + creating edit button observers\n")
      databoxes <- as.list(1:length(btns))
      databoxes <- lapply(databoxes, function(i) {
        
        btn_name <- btns[[i]]$btn_name
        csite_name <- btns[[i]]$csite_name
        
        #  Creates an observer only if it doesn't already exists.
        if (is.null(obsEditBtnList[[btn_name]])) {
          
          # Store observer function in list of buttons.
          obsDelBtnList[[btn_name]] <<- observeEvent(input[[btn_name]], {
            cat("* observeEvent: button clicked: ", btn_name, "\n")
            
            # Find data set by name.
            csite_selected_idx <<- c()
            for (i in 1:length(csite_list)) {
              if (csite_list[[i]]$GWSDAT_Options$SiteName == csite_name) {
                csite <<- csite_list[[i]]
                
                # One data set can contain multiple sub-sets (one for each Aquifer)
                csite_selected_idx <<- c(csite_selected_idx, i)
              }
            }
            
            # Copy tables
            import_tables$DF_conc <- csite$raw_contaminant_tbl
            import_tables$DF_well <- csite$raw_well_tbl
            
            # Copy shape data? Can't copy no files but maybe objects that can be 
            # deleted.
            # ...
            
            # Switch to Edit panel.
            shinyjs::show(id = "uiDataEdit")
            shinyjs::hide(id = "uiDataManager")
            
            output$uiDataEdit <- renderUI(uiEditData(csite))
            
          })
        } else {
          # This should never happen but make sure the very unlikely case shows up.
          stop("Attempting to create Delete button with already existing ID. Aborting. Fix this!")
        }
      }) # end of lapply
    }
  }
  
  #
  # Attempted to create generic function to create buttons, but content of 
  #  button observer changes (Delete or Edit action), so I won't go deeper here
  #  ,although, I think it is possible.
  #
  #
  # createBtnObserver <- function(del_btns) {
  #   
  #   if (length(del_btns) == 0)
  #     return(NULL)
  #   
  #   btn_list <- list()
  #   
  #   # Check if a Delete button was created.
  #   if (length(del_btns) > 0) {
  #     cat("* creating del button, see ", length(del_btns), "\n")
  #     databoxes <- as.list(1:length(del_btns))
  #     databoxes <- lapply(databoxes, function(i) {
  #       
  #       # Extract the button name and the associated data name. Deletion is 
  #       #  going to occur based on the data name. 
  #       #FIXME: Maybe safer to use a unique ID.
  #       btn_name <- del_btns[[i]]$btn_name
  #       del_csite_name <- del_btns[[i]]$csite_name
  #       
  #       #  Creates an observer only if it doesn't already exists.
  #       if (is.null(btn_list[[btn_name]])) {
  #         cat(" + creating del button ", btn_name, "\n")
  #         # Store observer function in list of buttons.
  #         btn_list[[btn_name]] <- observeEvent(input[[btn_name]], {
  #           cat("* observeEvent: button clicked: ", btn_name, "\n")
  #           
  #           # Copy to temporary buffer
  #           tmplist <- list()
  #           
  #           # Loop over the data list and copy names not matching 'del_csite_name'.
  #           for (i in 1:length(csite_list)) {
  #             
  #             # If the name is not matching, copy the data to the temporary list.
  #             if (csite_list[[i]]$GWSDAT_Options$SiteName != del_csite_name)
  #               tmplist[[length(tmplist) + 1]] <- csite_list[[i]]
  #           }
  #           
  #           # Write back the temporary buffer that contains the new data excluding
  #           # the data set specified in 'del_csite_name'.
  #           csite_list <<- tmplist
  #           
  #           # Need this to trigger observer that re-displays the new data list.
  #           dataLoaded(dataLoaded() + 1)
  #           
  #           
  #         })
  #       }
  #     }) # end of lapply
  #   }
  #   
  #   return(btn_list)
  # }
  
  output$uiDataManager <- renderUI({
    cat("* in uiDataManager <- renderUI()\n")
    
    # Observe load status of data.
    if (dataLoaded() < LOAD_COMPLETE) loadDefaultSessions()
    
    ret <- uiDataManagerList(csite_list, del_btns = names(obsDelBtnList),
                             edit_btns = names(obsEditBtnList))
    
    
    createDelBtnObserver(ret$del_btns)
    
    createEditBtnObserver(ret$edit_btns)
    
    return(ret$html_out)
    
  })
  
  
  output$rndAnalyse <- renderUI({
   
    # Observe load status of data.
    data_load_status <- dataLoaded()
    
    ret <- FALSE
    
    # Nothing loaded yet, start process.
    if (data_load_status < LOAD_COMPLETE) { 
      ret <- loadDataSet()
    
      if (is.null(ret)) {
        showModal(modalDialog(title = "Error", "Loading data failed, exiting."))
        return(NULL)
      }
    }
      
    
    if (class(ret) == "Aq_list" ) {
      return(div(style = "width: 50%; margin: 0 auto",
                      shinydashboard::box(
                        selectInput("aquifer", "Choose from list", choices = ret),     
                        div(style = "float: right", actionButton("aquifer_btn", "Next")),
                        status = "primary", 
                        solidHeader = TRUE, 
                        collapsible = FALSE, 
                        width = 6, 
                        title = "Select an Aquifer"
                      ))
      )
    }
    
    if (class(ret) == "dialogBox" ) {
      return(div(style = "width: 80%; margin: 0 auto",
                      shinydashboard::box(
                        div(style = "margin-top: 10px; margin-bottom: 25px",
                          paste0(ret$msg)),
                        div(style = "float: right", 
                            actionButton("diag_no" , "No"),
                            actionButton("diag_yes", "Yes")),
                        status = "primary", 
                        solidHeader = TRUE, 
                        collapsible = FALSE, 
                        width = 6, 
                        title = ret$title
                      ))
        )
    }
    
    #
    # The following code, unlocks the global variable img_formats_use and 
    #  modifies it. It can be later used in the renderUI sub methods.
    #  However, right now I am using the img_formats_use local variable of server()
    #  because I have trouble specifying the right environment, without using where()
    #  from the pryr package. I basically need the namespace environment to unlock the 
    #  binding and not the package environment (as return by environment("package:GWSDAT")).
    
    # require(pryr)
    # cat("*environment from where():\n")
    # print(where("img_formats_use"))
    
    # app_env <- as.environment("package:GWSDAT") # <- this won't work (need namespace)
    # app_env <- where("img_formats_use")         # <- this will work 
    # cat("*environment from as.environment():\n")
    # print(app_env)
    # if (bindingIsLocked("img_formats_use", app_env)) {  # <- app_env must look something like: <environment: namespace:GWSDAT>
    #  cat(" img_formats_use is locked binding, unlocking it..")
    #  unlockBinding("img_formats_use", app_env)
    #}
    
    # Take out the option to plot to Powerpoint .ppt, if PPT does not exists.
    # img_formats_use <<- img_formats
    # if (!existsPPT())
    #   img_formats_use <<- img_formats_use[-which(img_formats_use == "ppt")]
    # 
    # # If it is not windows, win.metafile() can't be used.
    # if (.Platform$OS.type != "windows") {
    #   img_formats_use <<- img_formats_use[-which(img_formats_use == "wmf")]
    #   img_formats_use <<- img_formats_use[-which(img_formats_use == "ppt")]  # the ppt method needs wmf
    # }
    
    
    # Completely loaded, display the Analyse UI.
    if (data_load_status >= LOAD_COMPLETE) {
      return(uiAnalyse(csite, img_frmt))
    }
    
  })
  
  # These observers catch the button press in the dialog boxes on startup
  observeEvent(input$aquifer_btn, {
    loadOptions$aquifer <<- input$aquifer
    dataLoaded(dataLoaded() + 1)
  })
  
  observeEvent(input$diag_no, { 
    loadOptions$subst_napl <<- "no"
    dataLoaded(dataLoaded() + 1)
  })
  
  observeEvent(input$diag_yes, { 
    loadOptions$subst_napl <<- "yes"
    dataLoaded(dataLoaded() + 1)
  })
  
 
} # end server section

