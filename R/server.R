



server <- function(input, output, session) {
  
 
  if (!exists("APP_RUN_MODE", envir = .GlobalEnv)) 
    APP_RUN_MODE <- "MultiData"
  
  # Flag that indicates whether data was loaded or not.  
  dataLoaded <- reactiveVal(0)
  #renderData <- reactiveVal(0)
  
  # List of site data and currently selected site.
  csite_list <- NULL
  csite <- NULL

  # This will become the default for new users.
  default_session_file <- "GWSDAT_Examples.RData"
  
  import_tables <- reactiveValues(DF_conc = NULL, DF_well = NULL)
 
 
  # 
  # Some usefull session objects:
  #
  # session$userData  - store user data here
  # onSessionEnded(callback), onEnded(callback)  - executes when user exits browser
  # isClosed()  - function that return TRUE if client has disconnected  
  #
  
  output$debug <- renderPrint({
      print(sessionInfo())

      cat("\n\n** Path to image logo: ", system.file("logo.gif", package = "GWSDAT"), "\n")

      cat("\n\n** Content of .libPaths():\n\n")
      sapply(.libPaths(), list.files)

  })
    
  # Clean-up user session.
  session$onSessionEnded(function() {
    stopApp()
  })
  
  
  # Reactive element that will trigger inside an observer when Options are saved.
  optionsSaved <- reactive({ 
    input$save_analyse_options 
  })
  
  checkPlumeStats <- reactive({
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Calculating Plume", value = 0)
    on.exit(progress$close())
    
   
    val <- getFullPlumeStats(csite, 
                             substance = input$solute_select_plume_pd, 
                             plume_thresh = input$plume_threshold_pd,
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
  
  # return
  updatePlumeTS <- reactive({
    
    # update plume threshold
    
    input$update_plume_ts
    
    })
  
  output$plume_diagn_msg <- renderUI({
    
    # Detect press of update button
    updatePlumeTS()
    
    # Detect changes in the Options.
    optionsSaved()
    
    # Detect if stats can not be displayed (hides this text box).
    # Isolate reactive inputs. 
    isolate(checkPlumeStats())
    
    # Isolate the inputs (so a change in the sidebar does not trigger this fct.)
    isolate(
     HTML(paste0(tags$b(input$solute_select_plume_pd), 
                 ": Unable to calculate plume statistics for a threshold value of ",
                 "<b>", input$plume_threshold_pd, " ug/l</b>. ",
                 # "Select a different plume threshold and retry.",
                 tags$p(),
                 tags$p("Use the ", tags$a(id = "togglePlumeBoundary", "Estimate Boundary", href = "#"), "tab for assistance in selecting a suitable plume threshold value.")
     ))
    )
  
  })
  
  
  output$plume_estimate_plot <- renderPlot({
    
    # Detect press of update button
    updatePlumeTS()
    
    isolate(plotPlumeEst(csite, input$solute_select_plume_pd, input$plume_threshold_pd))

  })
  
  
  output$plume_diagn_plot <- renderPlot({
    
    # Detect press of update button
    updatePlumeTS()
    
    # Detect changes in the Options.
    # optionsSaved()
    
    # Re-evaluate plume statistics if any reactive expression changes. 
    # The return value is the full plume statistics (for all timesteps). 
    isolate(plume_stats <- checkPlumeStats())
    
    plotPlumeTimeSeries(plume_stats)
    
    
  })
  
  
  # Plot time-series window
  output$time_series <- renderPlot({
    
    optionsSaved()
    
    #
    # Update control attributes from reactive variables. 
    #
    
    csite$ui_attr$conc_unit_selected <<- input$solute_conc
    csite$ui_attr$ts_options[1:length(csite$ui_attr$ts_options)] <<- FALSE
    csite$ui_attr$ts_options[input$ts_true_options] <<- TRUE
    
    
    # 'trend_thresh_selected' is also used in the Traffic Lights table (there it has
    # two threshold option. However, here it is only about selecting
    # to display the threshold or not. 
    # ? Might think of separating these two, i.e. creating extra variable
    #   here instead of trend_thresh_selected.
    if (input$check_threshold)
      csite$ui_attr$trend_thresh_selected <<- "Threshold - Absolute"
    else 
      csite$ui_attr$trend_thresh_selected <<- "Trend"
    
    
    plotTimeSeries(csite, input$solute_select, input$well_select)
    
  })

  
  # Re-Aggregate the data in case the aggregation type was changed.
  reaggregateData <- reactive({
    #cat("* reaggregateData()\n")
    
    # If 'input$aggregate_data_tt' is not put here, reaggregateData() will not
    # react for the trend table if: 
    #  1st Aggregation is changed in Spatial plot and  
    #  2nd Aggregation is change in trend table.
    input$aggregate_data_sp
    input$aggregate_data_tt
    
    # If nothing changed, return - happens only when session starts.     
    if ((tolower(csite$GWSDAT_Options$Aggby) == tolower(input$aggregate_data_sp)) &&
        (tolower(csite$GWSDAT_Options$Aggby) == tolower(input$aggregate_data_tt)))
      return()
    
    # Flag which aggregation input was active.
    sp_changed <- FALSE
    tt_changed <- FALSE
    
    # Detect which aggregation input changed.
    if (tolower(csite$GWSDAT_Options$Aggby) != tolower(input$aggregate_data_sp)) {
      csite$GWSDAT_Options$Aggby <<- input$aggregate_data_sp
      sp_changed <- TRUE
    } else if (tolower(csite$GWSDAT_Options$Aggby) != tolower(input$aggregate_data_tt)) {
      csite$GWSDAT_Options$Aggby <<- input$aggregate_data_tt
      tt_changed <- TRUE
    }
    
    # cat("  -> doing reaggregation..\n")
    
    tryCatch(
      agg_data <- aggregateData(csite$All.Data$Cont.Data, 
                                csite$All.Data$GW.Data, 
                                csite$All.Data$NAPL.Thickness.Data,
                                csite$All.Data$sample_loc$data, 
                                csite$GWSDAT_Options$Aggby, 
                                csite$GWSDAT_Options$AggMethod 
      ), error = function(e) {
        showModal(modalDialog(title = "Error", paste0("Failed to aggregate data: ", e$message), easyClose = FALSE))
        return(NULL)                      
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
    # This how it is done on first initializatioin in fitData(), but an explicit
    # date lookup would be more save.
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
    csite$ui_attr$timepoint_sp_idx <<- new_timepoint_idx
    csite$ui_attr$timepoint_tt_idx <<- new_timepoint_idx
    
    # Old way using real dates as timepoint indicator (together with sliderValues)
    #csite$ui_attr$timepoint_sp <<- dates_tmp[length(dates_tmp)]
    #csite$ui_attr$timepoint_tt <<- dates_tmp[length(dates_tmp)]
    
   
    # Update slider inputs: Spatial plot and in Trend table.
    outp <- pasteAggLimit(csite$ui_attr$timepoints[new_timepoint_idx], csite$GWSDAT_Options$Aggby)
    
    updateSliderInput(session, "timepoint_sp_idx", value = new_timepoint_idx,
                      min = 1, max = length(csite$ui_attr$timepoints), label = paste0("Time: ", outp), step = 1)
    
    updateSliderInput(session, "timepoint_tt_idx", value = new_timepoint_idx,
                      min = 1, max = length(csite$ui_attr$timepoints), label = paste0("Time: ", outp), step = 1)
    
    
    # Update select input: Aggregation in other panel.
    if (sp_changed)
      updateSelectInput(session, "aggregate_data_tt", selected = csite$GWSDAT_Options$Aggby)
    
    if (tt_changed)
      updateSelectInput(session, "aggregate_data_sp", selected = csite$GWSDAT_Options$Aggby)

  })

  
  #  
  # Update the label of the time slider, when slider changes.
  #
  observeEvent(input$timepoint_sp_idx, {
     
    csite$ui_attr$timepoint_sp_idx <<- input$timepoint_sp_idx
    
    timep <- csite$ui_attr$timepoints[input$timepoint_sp_idx]
    outp <- pasteAggLimit(timep, csite$GWSDAT_Options$Aggby)
    updateSliderInput(session, "timepoint_sp_idx", label = paste0("Time: ", outp))
  })

  observeEvent(input$timepoint_tt_idx, {
   
    csite$ui_attr$timepoint_tt_idx <<- input$timepoint_tt_idx
    
    timep <- csite$ui_attr$timepoints[input$timepoint_tt_idx]
    outp <- pasteAggLimit(timep, csite$GWSDAT_Options$Aggby)
    updateSliderInput(session, "timepoint_tt_idx", label = paste0("Time: ", outp))
  })
  

  #
  # Plot ImagePlot
  #
  output$image_plot <- renderPlot({
    
    #cat("* entering image_plot\n")
    
    # React to changes in the Options panel.
    optionsSaved() 
  
    timepoint_idx <- input$timepoint_sp_idx
    
    reaggregateData()
    
    # reaggregateData() might change csite$ui_attr$timepoint_sp_idx.
    if (csite$ui_attr$timepoint_sp_idx != input$timepoint_sp_idx)
      timepoint_idx <- csite$ui_attr$timepoint_sp_idx
    
    
    #Fixme: WHAT IS THIS FOR, NEED THIS HERE
    #val <- plumeThreshChange()
    
    
    # Update control attributes from reactive variables.
    #Fixme: CHECK IF NEED THIS HERE, BETTER TO JUST PASS ANYTHING DIRECTLY TO PLOTTING
    #       FUNCTION AND LEAVE WRITING BACK TO UI ATTRIBUTES TO SEPARATE FUNCTION. 
    csite$ui_attr$spatial_options[1:length(csite$ui_attr$spatial_options)] <<- FALSE
    csite$ui_attr$spatial_options[input$imageplot_options] <<-  TRUE
    csite$ui_attr$gw_selected <<- input$gw_flows
    csite$ui_attr$contour_selected <<- input$imageplot_type
    csite$ui_attr$conc_unit_selected <<- input$solute_conc_contour
    
    
    # cat(" -> time point idx active: ", timepoint_idx, ", size of timepoints vector: ", length(csite$ui_attr$timepoints), "\n")
    
    plotSpatialImage(csite, input$solute_select_contour, 
                     as.Date(csite$ui_attr$timepoints[timepoint_idx], "%d-%m-%Y"))
   
  })
    
  
  
  
  output$trend_table <- renderUI({
    
    # React to changes in the Options panel.
    optionsSaved() 
    
    timepoint_idx <- input$timepoint_tt_idx
    
    # React to data aggregation.
    reaggregateData()
    
    # reaggregateData() might change csite$ui_attr$timepoint_sp_idx.
    if (csite$ui_attr$timepoint_tt_idx != input$timepoint_tt_idx)
      timepoint_idx <- csite$ui_attr$timepoint_tt_idx
    
    plotTrendTable(csite, as.Date(csite$ui_attr$timepoints[timepoint_idx], "%d-%m-%Y"),
               input$trend_or_threshold, input$traffic_color)
  })

  
  # Plot the legend for the traffic lights table.
  output$trend_legend <- renderUI({ plotTrendTableLegend()  })
  

  
  #
  # Plot Well Report
  #
  output$well_report_plot <- renderPlot({
    
    use_log_scale    <- if (input$well_report_logscale == "Yes") {TRUE} else {FALSE}
    
    plotWellReport(csite, input$solute_mult_select, input$well_mult_select, use_log_scale)
    
  })
  
  #
  # Plot SpatioTemporal Predictions
  #
  output$stpredictions_plot <- renderPlot({
    
    use_log_scale <- if (input$logscale_stp == "Yes") {TRUE} else {FALSE}
    
    plotSTPredictions(csite, input$solute_select_stp, input$well_mult_select_stp, use_log_scale, input$solute_conc_stp)
    
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
        
        makeTimeSeriesPPT(csite, input$solute_select, input$well_select,
                          width  = input$img_width_px  / csite$ui_attr$img_ppi, 
                          height = input$img_height_px / csite$ui_attr$img_ppi)
        
      } else {
        
        if (input$export_format_ts == "png") png(file, width = input$img_width_px, height = input$img_height_px)
        if (input$export_format_ts == "pdf") pdf(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
        if (input$export_format_ts == "ps")  postscript(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
        if (input$export_format_ts == "jpg") jpeg(file, width = input$img_width_px, height = input$img_height_px, quality = input$img_jpg_quality) 
        if (input$export_format_ts == "wmf") win.metafile(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
        
        plotTimeSeries(csite, input$solute_select, input$well_select)
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
        
        plotSpatialImagePPT(csite, input$solute_select_contour, as.Date(csite$ui_attr$timepoints[input$timepoint_sp_idx], "%d-%m-%Y"),
                       width  = input$img_width_px  / csite$ui_attr$img_ppi,
                       height = input$img_height_px / csite$ui_attr$img_ppi)
      
        } else {
        
          if (input$export_format_sp == "png") png(file, width = input$img_width_px, height = input$img_height_px)
          if (input$export_format_sp == "pdf") pdf(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
          if (input$export_format_sp == "ps") postscript(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
          if (input$export_format_sp == "jpg") jpeg(file, width = input$img_width_px, height = input$img_height_px, quality = input$img_jpg_quality) 
          if (input$export_format_sp == "wmf") win.metafile(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
         
          plotSpatialImage(csite, input$solute_select_contour, as.Date(csite$ui_attr$timepoints[input$timepoint_sp_idx], "%d-%m-%Y"))
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
        
        if (input$timepoint_tt == "")
          plotTrendTablePPT(csite, as.Date(csite$ui_attr$timepoint_tt, "%d-%m-%Y"),  input$trend_or_threshold, input$traffic_color,  
                            width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi)
        else
          plotTrendTablePPT(csite, as.Date(input$timepoint_tt, "%d-%m-%Y"),  input$trend_or_threshold, input$traffic_color,  
                          width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi)
        
      } else {
        
        if (input$export_format_tt == "png") png(file, width = input$img_width_px, height = input$img_height_px)
        if (input$export_format_tt == "pdf") pdf(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
        if (input$export_format_tt == "ps")  postscript(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
        if (input$export_format_tt == "jpg") jpeg(file, width = input$img_width_px, height = input$img_height_px, quality = input$img_jpg_quality) 
        if (input$export_format_tt == "wmf") win.metafile(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
        
        if (input$timepoint_tt == "")
          plotTrendTable(csite, as.Date(csite$ui_attr$timepoint_tt, "%d-%m-%Y"), input$trend_or_threshold, input$traffic_color) 
        else        
          plotTrendTable(csite, as.Date(input$timepoint_tt, "%d-%m-%Y"),  input$trend_or_threshold, input$traffic_color)
        
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
        
        plotWellReportPPT(csite, input$solute_mult_select, input$well_mult_select, use_log_scale,
                       width  = input$img_width_px_wide  / csite$ui_attr$img_ppi, 
                       height = input$img_height_px_wide / csite$ui_attr$img_ppi)
      } else {
        
        if (input$export_format_wr == "png") png(file, width = input$img_width_px_wide, height = input$img_height_px_wide)
        if (input$export_format_wr == "pdf") pdf(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_wr == "ps") postscript(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_wr == "jpg") jpeg(file, width = input$img_width_px_wide, height = input$img_height_px_wide, quality = input$img_jpg_quality) 
        if (input$export_format_wr == "wmf") win.metafile(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 

        plotWellReport(csite, input$solute_mult_select, input$well_mult_select, use_log_scale)

        
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
        
        plotSTPredictionsPPT(csite, input$solute_select_stp, input$well_mult_select_stp, 
                             use_log_scale, input$solute_conc_stp,
                             width = input$img_width_px_wide / csite$ui_attr$img_ppi, 
                             height = input$img_height_px_wide / csite$ui_attr$img_ppi)
        
      } else {
        
        if (input$export_format_stp == "png") png(file, width = input$img_width_px_wide, height = input$img_height_px_wide)
        if (input$export_format_stp == "pdf") pdf(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_stp == "ps")  postscript(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_stp == "jpg") jpeg(file, width = input$img_width_px_wide, height = input$img_height_px_wide, quality = input$img_jpg_quality) 
        if (input$export_format_stp == "wmf") win.metafile(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        
        plotSTPredictions(csite, input$solute_select_stp, input$well_mult_select_stp, use_log_scale, input$solute_conc_stp)
        
        dev.off()
      }
      
    }
  )
  
  output$save_session_btn <- downloadHandler(
    
    filename <- input$session_filename,
    
    content <- function(file) {
    
      if (!is.null(csite)) {
        
        # Create temporary csite_list, that includes the current active data session.
        # This will not overwrite the server csite_list.
        csite_list <- list(csite = csite)
        
        save(file = file, "csite_list")
      }
    }
    
  )
  
  
  # Generate PPT with spatial animation.
  observeEvent(input$generate_spatial_anim_ppt, {
    makeSpatialAnimation(csite, input$solute_select_contour,
                         input$img_width_px / csite$ui_attr$img_ppi,
                         input$img_height_px / csite$ui_attr$img_ppi,
                         input$img_width_px_wide / csite$ui_attr$img_ppi,
                         input$img_height_px_wide / csite$ui_attr$img_ppi)
  })
  
  
  # Generate PPT with trend table animation.
  observeEvent(input$generate_trendtable_anim_ppt, {
    makeTrendTableAnimation(csite, input$trend_or_threshold, input$traffic_color)
  })
  
  
  
  observeEvent(input$excel_import_file, {
    
    dtmp <- readExcel(input$excel_import_file)
    
    if (!is.null(dtmp)) {
      import_tables$DF_conc <<- dtmp$conc_data
      import_tables$DF_well <<- dtmp$well_data
    } else {
      showNotification("Failed to load data from Excel file.", type = "error")
    }
    
  }) 
  
  
  observeEvent(input$well_data_file, {
    
    inFile <- input$well_data_file
    
    if (is.null(inFile))
      return(NULL)
    
    DF <- readConcData(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    
    if (is.null(DF)) {
      # If there was an error reading the data, empty the fileInput control.
      # Fixme: ...
      return(NULL)
    }
    
    
    # Save to reactive variable.
    import_tables$DF_conc <<- DF 
    
  })
  
  
  
  output$table_conc_data <- rhandsontable::renderRHandsontable({
    
    if (is.null(import_tables[["DF_conc"]]))
      return(NULL)

    useTypes = FALSE  # as.logical(input$useType)
    if (nrow(import_tables$DF_conc) > 100)
      rhandsontable::rhandsontable(import_tables$DF_conc[1:100,], useTypes = useTypes, stretchH = "all")
    else
      rhandsontable::rhandsontable(import_tables$DF_conc, useTypes = useTypes, stretchH = "all")
    
    
  })
  
  observeEvent(input$well_coord_file, {
    
    inFile <- input$well_coord_file
    
    if (is.null(inFile))
      return(NULL)
 

    DF <- readWellCoords(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote) 
    
    # Save to reactive variable.
    import_tables$DF_well <<- DF
    
  })
  
  output$table_well_coord <- rhandsontable::renderRHandsontable({
    
    if (is.null(import_tables$DF_well))
      return(NULL)

    if (is.null(import_tables$DF_well$data))
      return(NULL)
    
    
    useTypes = FALSE  # as.logical(input$useType)
    rhandsontable::rhandsontable(import_tables$DF_well$data, useTypes = useTypes, stretchH = "all")
    
  })
  
  
  
  #
  # Supposed to clear everything in the Import Data panel
  #  Thats a little more complicated, not working right now.
  # hint: user renderUI() that waits for the reset input
  #
  #observeEvent(input$reset_import,  {
    
    #value = input$reset_button
    #gg = 9
    
  #})
  
  
  
  
  observeEvent(input$import_button,  {
   
    
    if (is.null(import_tables[["DF_conc"]])) {
      showNotification("Contaminant concentration table was not loaded properly. Aborted.", type = "error")
      return(NULL)
    }
    
    if (is.null(import_tables[["DF_well"]])) {
      showNotification("Well coordinate table was not loaded properly. Aborted.", type = "error")
      return(NULL)
    }
    
    
    # Create the progress bar.
    progress <- shiny::Progress$new()
    progress$set(message = "Loading data", value = 0)
    on.exit(progress$close())
    
    progress$set(value = 0.1, detail = paste("reading data"))
    
    GWSDAT_Options <- createOptions(isolate(input$new_data_name))
    
    all_data <- formatData(import_tables[["DF_conc"]], import_tables[["DF_well"]])


    for (Aq_sel in unique(all_data$sample_loc$data$Aquifer)) {
    
      if (is.null(pr_dat <- processData(all_data$solute_data, all_data$sample_loc, GWSDAT_Options, Aq_sel))) next
      
      ui_attr <- createUIAttr(pr_dat, GWSDAT_Options)
    
      # Build list with all data.
      csite <<- list(All.Data       = pr_dat,
                     Fitted.Data    = NULL,
                     GWSDAT_Options = GWSDAT_Options,
                     Traffic.Lights = NULL,
                     ui_attr        = ui_attr,
                     Aquifer        = Aq_sel
      )
    
      csite_list[[length(csite_list) + 1]] <<- csite 
      
    }

    # Flag that data was loaded.
    dataLoaded(dataLoaded() + 1)
    
    # Go back to Data Manager.
    shinyjs::show(id = "uiDataManager")
    shinyjs::hide(id = "uiDataAddCSV")
    shinyjs::hide(id = "uiDataAddExcel")
    
  })
  
  
  
  # Go to .CSV Data Import (Button click).
  observeEvent(input$add_csv_data,  {
    shinyjs::hide("uiDataManager")
    shinyjs::show("uiDataAddCSV")
  })

  # Go to .CSV Data Import (Link).
  shinyjs::onclick("toggleDataImport", {
        #browser()
    shinyjs::show(id = "uiDataAddCSV");
    shinyjs::hide(id = "uiDataManager")
   
  })

    
  # Go (back) to Data Manager.
  shinyjs::onclick("gotoDataManager_a", showDataMng())
  shinyjs::onclick("gotoDataManager_b", showDataMng())
  shinyjs::onclick("gotoDataManager_c", showDataMng())

  showDataMng <- function() {
    shinyjs::show(id = "uiDataManager")
    shinyjs::hide(id = "uiDataAddCSV")
    shinyjs::hide(id = "uiDataAddNew")
    shinyjs::hide(id = "uiDataAddExcel")
  }
  
  
  # Go to Add New Data (Button click).
  observeEvent(input$add_new_data,  {
    shinyjs::show(id = "uiDataAddNew")
    shinyjs::hide(id = "uiDataManager")
  })
  
  # Go to Excel Data Import (Button click).
  observeEvent(input$add_excel_data,  {
    shinyjs::show(id = "uiDataAddExcel")
    shinyjs::hide(id = "uiDataManager")
  })
  
  # Follow link to 'Boundary Estimate' tabPanel.
  shinyjs::onclick("togglePlumeBoundary", {
    updateTabsetPanel(session, "plume_tab_box", selected = "plume_pnl_2")
  })



    
  #
  # Display the "Generate PPT Animation" button if Powerpoint is available.
  #
  observeEvent(input$analyse_panel, {
   
    # If Powerpoint export is possible, show the "Generate PPT Animation" button.
    if (existsPPT()) {
      shinyjs::show(id = "save_spatial_ppt_anim")
      shinyjs::show(id = "save_trendtable_ppt_anim")
    }
  
    # Take out the option to plot to Powerpoint .ppt, if PPT does not exists.
    imgs <- csite$ui_attr$img_formats
    if (!existsPPT())
      imgs <- imgs[-which(imgs == "ppt")]
   
    # If it is not windows, win.metafile() can't be used.
    if (.Platform$OS.type != "windows") {
      imgs <- imgs[-which(imgs == "wmf")]
      imgs <- imgs[-which(imgs == "ppt")]  # the ppt method needs wmf
    }
    
    # Update the image format inputs in each panel
    if (input$analyse_panel == "Time-Series")
      updateSelectInput(session, "export_format_ts", choices = imgs, selected = imgs[[1]])
  
    if (input$analyse_panel == "Spatial Plot")
      updateSelectInput(session, "export_format_sp", choices = imgs, selected = imgs[[1]])
    
    if (input$analyse_panel == "Well Report")
      updateSelectInput(session, "export_format_wr", choices = imgs, selected = imgs[[1]])

    if (input$analyse_panel == "Plume Diagnostic")
      updateSelectInput(session, "export_format_pd", choices = imgs, selected = imgs[[1]])
    
    if (input$analyse_panel == "Spatiotemporal Predictions")
      updateSelectInput(session, "export_format_stp", choices = imgs, selected = imgs[[1]])
    
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
  
  observeEvent(input$save_analyse_options, {
    
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
    
    if (input$sidebar_menu == "analysis") {
      shinyjs::hide("analyse_page")
      shinyjs::show("data_select_page")
      
    }
  })
    
  
  
  observeEvent(input$aquifer_btn, {
    dataLoaded(1)
  })
  
  
  loadDefaultSessions <- function() {
   
    infile <- system.file("extdata", default_session_file, package = "GWSDAT")
    
    csite_list <- NULL
    
    # This should never trigger a warning, since I am putting the file there (only if package is broken).
    tryCatch( load(infile),
              warning = function(w) showNotification(paste0("Failed to load default_session_file \'", default_session_file, "\' from package GWSDAT."), type = "error", duration = 7))

    if (is.null(csite_list))
      return(NULL)
    
    csite_list <<- csite_list
    csite <<- csite_list[[1]]
      
    
    dataLoaded(2)
    
  }
  
  
  #
  # Would like to move this fct to another file, however,
  #   it uses the reactive variabled dataLoaded. How to fix this?
  #
  loadDataSet <- function(Aq_sel = NULL) {

    
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
      
      dataLoaded(2)
      return(TRUE)  
    }
    
    # Create Options in case they don't exist.
    if (!exists("GWSDAT_Options", envir = .GlobalEnv)) 
      GWSDAT_Options <-  createOptions()
    
    
    
    # Load the data from the .csv files.
    
    solute_data <- well_data <- NULL
    
    # Read Well data and coordinates from file.
    tryCatch({
      solute_data <- readConcData(GWSDAT_Options$WellDataFilename)
      well_data <- readWellCoords(GWSDAT_Options$WellCoordsFilename)
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

    
    shape_data <- readShapeFiles_sf(GWSDAT_Options$ShapeFileNames)
    
    pr_dat <- processData(all_data$solute_data, all_data$sample_loc, GWSDAT_Options, 
                          Aq_sel, shape_data)
      
    
    
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
		               Aquifer        = Aq_sel
    )
    
    csite_list[[length(csite_list) + 1]] <<- csite 
    
    # Flag that data was fully loaded.
    dataLoaded(2)
    
    return(TRUE)
  }  
  
    
  
  # List of observers for Analyse buttons, one for each data set.
  obsAnayseBtnList <- list()
  
  
  
  output$data_overview <- renderUI({
   
     if (dataLoaded() == 0) 
      loadDataSet()
    
    html_out <- h3("Select Data Set")
    
    if (length(csite_list) == 0) {
      html_out <- tagList(html_out,
                         shinydashboard::box(width = 7, title = "Data Missing", status = "primary",
                             "Load session data (add link) or import data (add link)."
                         )
      )
      
    } else {
      
      data_sets <- getDataInfo(csite_list)

      databoxes <- as.list(1:length(data_sets))
      
      databoxes <- lapply(databoxes, function(i) {
        
          btName <- paste0("analyse_btn", i)
        
          # creates an observer only if it doesn't already exists
          if (is.null(obsAnayseBtnList[[btName]])) {
            
            obsAnayseBtnList[[btName]] <<- observeEvent(input[[btName]], {
              
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

              # Make selected data set active.
              csite <<- csite_list[[j]]

              
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
      
      
      for (i in 1:length(data_sets)) {
        
        set_name <- names(data_sets)[i]
        
        html_out <- tagList(html_out, fluidRow(
          shinydashboard::box(width = 7, status = "primary", collapsible = TRUE,
              title = set_name, 
              #p(paste("Contaminants: ", paste(csite_list[[i]]$All.Data$cont_names, collapse = ", "))),
              #p(paste("Wells: ", paste(csite_list[[i]]$All.Data$sample_loc$names, collapse = ", "))),
              #p(paste0("Model method: ", csite_list[[i]]$GWSDAT_Options$ModelMethod))
              
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

  output$uiDataAddNew <- renderUI({
    
    input$add_new_data
    
    conc_header <- list("WellName", "Constituent", "SampleDate", "Result", "Units", "Flags")
    well_header <- list("WellName", "XCoord", "YCoord", "Aquifer")
    
    isolate({
      import_tables$DF_conc <- data.frame(matrix(0, nrow = 20, ncol = length(conc_header)))
      colnames(import_tables$DF_conc) <- conc_header
      
      well_tmp <- data.frame(matrix(0, nrow = 20, ncol = length(well_header)))
      colnames(well_tmp) <- well_header
      
      import_tables$DF_well <- list(data = well_tmp, unit = input$coords_unit)
      
    })
    

    fluidPage(
      div(style = "margin-bottom: 10px", actionButton("gotoDataManager_a", label = "", icon = icon("arrow-left"))),
      
      shinydashboard::box(width = 3, solidHeader = TRUE, status = "primary", 
          
          
          h3("Add New Data"),
          "Enter the data directly or copy/paste into the tables.",
          hr(),
          
          textInput("new_data_name", label = "Data Name", value = getValidDataName(csite_list)),
          #actionButton("reset_import", label = "Reset"),
          actionButton("add_new_button", label = "Add Data", icon("arrow-down"), 
                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
      ),
      
      shinydashboard::tabBox(title = "New Tables", width = 9, 
           tabPanel("Contaminant Data", 
                    rhandsontable::rHandsontableOutput("table_conc_data")
           ), 
           tabPanel("Well Coordinates",
                    rhandsontable::rHandsontableOutput("table_well_coord")
           )
      )
    )
  })
    
  
  output$uiDataAddExcel <- renderUI({
    
    import_tables$DF_well <- NULL
    import_tables$DF_conc <- NULL
    
    input$add_excel_data
    
    conc_header <- list("WellName", "Constituent", "SampleDate", "Result", "Units", "Flags")
    well_header <- list("WellName", "XCoord", "YCoord", "Aquifer")
    
   
    fluidPage(
      #tags$head(
      #  tags$style(
      #    HTML(".shiny-notification { 
      #          width : 300px;
      #          left  : -200px; } ")
                                        #)),
      div(style = "margin-bottom: 10px", actionButton("gotoDataManager_b", label = "", icon = icon("arrow-left"))),
      
      shinydashboard::box(width = 3, solidHeader = TRUE, status = "primary", 
          
          
          h3("Import Excel Data"),
          "Select the Excel file containing the GWSDAT data.",
          hr(),
          textInput("new_data_name", label = "Data Name", value = getValidDataName(csite_list)),
          fileInput('excel_import_file', 'Excel File', accept = c('.xls', '.xlsx')),
          #actionButton("reset_import", label = "Reset"),
          actionButton("import_button", label = "Import Data", icon("arrow-down"), 
                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                       
          
          
      ),
      
      shinydashboard::tabBox(title = "Imported Tables", width = 9, 
             tabPanel("Contaminant Data", rhandsontable::rHandsontableOutput("table_conc_data")
             ), 
             tabPanel("Well Coordinates", rhandsontable::rHandsontableOutput("table_well_coord")
             )
      )
    )
  })
  
  
  
  output$uiDataAddCSV <- renderUI({
    
    
    
    import_tables$DF_well <- NULL
    import_tables$DF_conc <- NULL
    
    # React to add and reset events.
    input$add_csv_data
    input$reset_import
    
    fluidPage(
      div(style = "margin-bottom: 10px", actionButton("gotoDataManager_c", label = "", icon = icon("arrow-left"))),
      
      shinydashboard::box(width = 3, solidHeader = TRUE, status = "primary", 
          
          
          h3("Import .csv Data"),
          "Select the contaminant data and well coordinate files in .csv format. The tables on the right allow you to edit individual values.",
          hr(),
          
          textInput("new_data_name", label = "Data Name", value = getValidDataName(csite_list)),
          fileInput('well_data_file', 'Well Data File',
                    accept = c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv')),
          
          fileInput('well_coord_file', 'Well Coordinates File',
                    accept = c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv')),
          
          hr(),
          
          checkboxInput('header', 'Header', TRUE),
          #checkboxInput('excel_date', 'Transform Excel Date', TRUE),
          radioButtons('sep', 'Separator',
                       c(Comma = ',',
                         Semicolon = ';',
                         Tab = '\t'),
                       ','),
          radioButtons('quote', 'Quote',
                       c(None = '',
                         'Double Quote' = '"',
                         'Single Quote' = "'"),
                       '"'),
          hr(),
          actionButton("reset_import", label = "Reset"),
          actionButton("import_button", label = "Import Data", icon("arrow-down"), 
                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
          )
          
          
      ), # end box
      
      shinydashboard::tabBox(title = "Imported Tables", width = 9, 
             tabPanel("Contaminant Data", rhandsontable::rHandsontableOutput("table_conc_data")
             ), 
             tabPanel("Well Coordinates", rhandsontable::rHandsontableOutput("table_well_coord")
             )
      )
      
    ) # end fluidPage
  })
  
  
  
  output$uiDataManager <- renderUI({
    
    # Observe load status of data.
    if (dataLoaded() == 0) {
      loadDefaultSessions()
    }
    
    html_out <- tagList(
                        #shinydashboard::box(width = 3, 
                        div(style = "float : right; margin-bottom: 5px",
                            actionButton("add_new_data", label = "Add New Data", icon = icon("plus"), 
                                         style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            actionButton("add_csv_data", label = "Import .csv Data", icon = icon("arrow-down"), 
                                         style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            actionButton("add_excel_data", label = "Import Excel File", icon = icon("arrow-down"), 
                                         style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                           
                        ),
                        h2("Data Manager")
    )
                        
                        
    
    if (length(csite_list) == 0) {
      # No data exists.
      html_out <- tagList(html_out,
                          shinydashboard::box(width = 7, title = "No Data Present", status = "warning", "Import and or add to analyse."))
    } else {
     
      data_sets <- getDataInfo(csite_list)
     
      for (set_name in names(data_sets)) {
        
        html_out <- tagList(html_out, fluidRow(
          shinydashboard::box(width = 7, status = "primary", collapsible = TRUE,
              title = set_name, 
              #p(paste("Contaminants: ", paste(csite_list[[i]]$All.Data$cont_names, collapse = ", "))),
              #p(paste("Wells: ", paste(csite_list[[i]]$All.Data$sample_loc$names, collapse = ", "))),
              p(paste0("Aquifer: ", paste(data_sets[[set_name]]$Aquifer, collapse = ", ")))
              #p(paste0("Model method: ", csite_list[[i]]$GWSDAT_Options$ModelMethod))
              # div(style = "float : right", actionButton(btName, "Select"))
          )))
        
      }
      
    }
    
   
    
    return(html_out)
    
  })
  
    
  
  
  
  
  output$rndAnalyse <- renderUI({
   
    html_out <- NULL
    
    
    # Observe load status of data.
    data_load_status <- dataLoaded()
    
   
    # Nothing loaded yet, start process.
    if (data_load_status == 0) { 
      
      ret <- loadDataSet()
      
      if (class(ret) == "Aq_list" ) {
        html_out <- div(style = "width: 50%; margin: 0 auto",
          shinydashboard::box(
            selectInput("aquifer", "Choose from list", choices = ret),     
            div(style = "float: right", actionButton("aquifer_btn", "Next")),
            status = "primary", 
            solidHeader = TRUE, 
            collapsible = FALSE, 
            width = 6, 
            title = "Select an Aquifer"
          )
        )
      }
    }

    
    # Data partially loaded, continue with selected Aquifer.
    if (data_load_status == 1) {
      ret <- loadDataSet(Aq_sel = input$aquifer)
    }
    
    # Completely loaded, display the Analyse UI.
    if (data_load_status >= 2) {
      html_out <- uiAnalyse(csite)
    }
    
    return(html_out)
  })
  
} # end server section

