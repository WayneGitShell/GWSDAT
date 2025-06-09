


server <- function(input, output, session) {
  
  
  observe({
    
    urlArgs<-session$clientData$url_search
  
  
      if(urlArgs!=""){
    
        .GlobalEnv$GWSDAT_Options<-createOptions()
         urlArgsParsed<-parseQueryString(urlArgs)

          for(eachArg in 1:length(urlArgsParsed)){GWSDAT_Options[[names(urlArgsParsed)[eachArg]]]<-urlArgsParsed[[names(urlArgsParsed)[eachArg]]]}

          if(!is.null(GWSDAT_Options$ExcelDataFilename)){
            
            GWSDAT_Options$ExcelDataFilename<-list(datapath=GWSDAT_Options$ExcelDataFilename)
            
            if (grepl("^((http|ftp)s?|sftp)://", GWSDAT_Options$ExcelDataFilename$datapath)) { #if web URL download to temporary directory and save temp path. 
                   tmp_file <- tempfile(fileext = paste0(".",tools::file_ext(GWSDAT_Options$ExcelDataFilename$datapath))) 
                   utils::download.file(GWSDAT_Options$ExcelDataFilename$datapath, tmp_file, mode = "wb") 
                   GWSDAT_Options$ExcelDataFilename$datapath <- tmp_file 
            } 
            
          }

      .GlobalEnv$GWSDAT_Options<-GWSDAT_Options #Has to be a global variable to work... 
    ##Example: https://rconnect-dev.private.selfservice.shell.ai/GWSDATURL/?ExcelDataFilename=GWSDATExample.xlsx&ShapeFileNames=GWSDATex2.shp 
    ## https://rconnect-dev.private.selfservice.shell.ai/GWSDATURL/?WellDataFilename=https://raw.githubusercontent.com/WayneGitShell/GWSDAT/master/data/BasicExample_WellData.csv&WellCoordsFilename=https://raw.githubusercontent.com/WayneGitShell/GWSDAT/master/data/BasicExample_WellCoords.csv
    
  }
  
  
  })
  
  time.log <- ''
  
  DEBUG_MODE <- FALSE
 
  # Increase upload file size to 30MB (default: 5MB)
  options(shiny.maxRequestSize = 30*1024^2)
  
  si <- sessionInfo()
  tmplog <- paste0(si$R.version$version.string, "\nPlatform: ", si$platform, "\n",
                   "GWSDAT version: ", packageVersion("GWSDAT"), "\n")
  app_log <- reactiveVal(tmplog)
 
  # This is set inside launchApp()
  if (!exists("APP_RUN_MODE", envir = .GlobalEnv)) 
    APP_RUN_MODE <- "MultiData"
  
  # Moved into server from ui due to modal conflicts
  if (exists("APP_CUSTOM_COMPONENT", envir = .GlobalEnv)) 

  shiny::showModal(APP_CUSTOM_COMPONENT())

  
  # This is set inside launchApp()
  if (!exists("APP_LOGIN_MODE", envir = .GlobalEnv)) 
    APP_LOGIN_MODE <- FALSE

  # This is set inside launchApp()
  if (!exists("session_file", envir = .GlobalEnv)) {
    session_file <- NULL
  }
  
  # Flag that indicates whether data was loaded or not.  
  LOAD_COMPLETE <- 100
  dataLoaded <- reactiveVal(0)
  
  # List of site data and currently selected site.
  csite_list <- NULL
  csite <- NULL
  csite_selected_idx <- 0
  
  # Background-Process (BP) related variables. 
  BP_method <- 'simple'                     # Allowed values: 'simple', 'none', 'queue'
  BP_modelfit_outfile <- ""                 # Result from BP is written to this file.
  BP_modelfit_running <- reactiveVal(FALSE) # Inform observers that fitting is in progress.
  BP_modelfit_done    <- reactiveVal(1)     # Inform all depending functions that fitting is done.
  
  
  # PSplines settings
  new_psplines_nseg <- 0
  prev_psplines_resolution <- "Default"
  prev_psplines_knots <- 6
  
  # Default data set including the Basic and Comprehensive example. Loaded in
  # server mode. 
  default_session_file <- "GWSDAT_Examples.rds"
  
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
  
  # Define supported image formats for saving plots.
  img_frmt <- list("png", "jpg", "pdf", "ps", "pptx","tif","wmf")
        
  # Remove pptx (powerpoint) if no support was found. 
  if (!existsPPT())
    img_frmt <- img_frmt[-which(img_frmt == "pptx")]
  
  if (Sys.info()[['sysname']]!="Windows")
    img_frmt <- img_frmt[-which(img_frmt == "wmf")]
  
  
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
    input$save_Colour_Key
  })
  
  
  
  
  ## Login Logout ###############################################################
  
  user_id <- reactiveValues(id = -1, authenticated = FALSE, file = "")
  users_dbPath <- 'users.db'
  
  output$wrongPasswordMsg1 <- renderText({''})
  output$wrongPasswordMsg2 <- renderText({''})
  
  
  observeEvent(input$doLogin, {
    
    if (!user_id$authenticated) {
      
      user_info <- verifyUser(users_dbPath, input$login_email, input$login_password)
      
      if (is.null(user_info)) {
        output$wrongPasswordMsg1 <- renderText({'Login failed. Email or password do not match.'})
        return(NULL)
      }
      
      user_id$authenticated <- TRUE
      user_id$id <- user_info$id
      user_id$email <- user_info$email
      user_id$file <- user_info$data_path
      
      # Load the data and overwrite current data set.
      if (file.exists(user_id$file))
        csite_list <<- readRDS(user_id$file)
      else {
        # In case it was deleted on accident.
        file.copy(system.file("extdata", default_session_file, package = "GWSDAT"), user_id$file)
        showNotification("User data not found. Re-creating it from Example data set.", type = "warning", duration = 7)
        csite_list <<- readRDS(user_id$file)
      }        
        
        
      dataLoaded(dataLoaded() + 1)
      
      output$wrongPasswordMsg1 <- renderText({''})
      
      removeModal()
      
      showNotification('Successfully logged in with email ', user_id$email, '.', type = 'message', duration = 7 )
    }           
  })
  
  observeEvent(input$doSignup, {
    
    success <- TRUE
    new_email <- tolower(input$signup_email)
    
    if (input$signup_password != input$signup_password2) {
      output$wrongPasswordMsg2 <- renderText({'Passwords do not match.'})
      success <- FALSE
    } 
    
    #FIXME: Change to checking mda string equivalent to emptry string.
    if (nchar(input$signup_password) == 0 || nchar(new_email) == 0) {
      output$wrongPasswordMsg2 <- renderText({'Empty email or passwords are not allowed.'})
      success <- FALSE
    }
    
    if (userExists(users_dbPath, new_email)) {
      output$wrongPasswordMsg2 <- renderText({'You have already registered.'})
      success <- FALSE
    }
    
    if (success) {
      
      # Check if user data base file exists and open the data base.
      if (!file.exists(users_dbPath)) {
        con <- createUserDB(users_dbPath)
      } else {
        con <- DBI::dbConnect(DBI::dbDriver("SQLite"), users_dbPath)
      }
      
      # Create the user id and the its data file path.
      new_id <- createUserID(con)
      user_file <- paste0('user_data_', new_id, '.rds' )
      
      # Copy content Example data to the new user file.
      file.copy(system.file("extdata", default_session_file, package = "GWSDAT"), user_file)
      
      # Create new record for user and append to user data base. 
      user_rec <- data.frame(id = new_id, email = new_email, 
                             password = digest::digest(input$signup_password),
                             data_path = user_file)
      
      DBI::dbWriteTable(con, "users", user_rec, append = TRUE)
      DBI::dbDisconnect(con)
      
      user_id$authenticated <- TRUE
      user_id$id <- new_id
      user_id$email <- new_email
      user_id$file <- user_file
      
      # Load the data and overwrite current data set.
      csite_list <<- readRDS(user_id$file)
      dataLoaded(dataLoaded() + 1)
      
      output$wrongPasswordMsg2 <- renderText({''})
      
      #FIXME: Either directly login, or show a message that registration was successful 
      # and redisplay the loginPanel
      removeModal()
      
      showNotification('Signed up and logged in with email ', user_id$email, '.', type = 'message', duration = 7 )
    }
  })
  
  # Close login panel.
  observeEvent(input$cancelLogin, {
    # Remove the message informing about a bad password.
    output$wrongPasswordMsg1 <- renderText({''})
    removeModal() 
  })
  
  # Close Sign-up panel.
  observeEvent(input$cancelSignup, {
    # Remove the message informing about a bad password.
    output$wrongPasswordMsg2 <- renderText({''})
    removeModal() 
  })
  
  
  
  # Follow link to 'Boundary Estimate' tabPanel.
  shinyjs::onclick("gotoLogin", shiny::showModal(uiLoginModal()) )
  shinyjs::onclick("gotoSignup", shiny::showModal(uiSignupModal()) )
  
  shinyjs::onclick("doLogout", {
    
    # Deal with background processes? 
    # (terminate processes if requested)
    
    # Save user data back to disk.
    saveRDS(csite_list, user_id$file)
    
    # Load default session.
    infile <- system.file("extdata", default_session_file, package = "GWSDAT")
    csite_list <- readRDS(infile)
    
    if (length(csite_list) > 0) {
      csite <- csite_list[[1]]
      csite_selected_idx <- 1
    } else {
      csite <- NULL
    }
    
    dataLoaded(dataLoaded() + 1)
    
    # Set user information to logged out.
    user_id$authenticated <- FALSE
    user_id$id <- 0
    user_id$email <- ""
    user_id$file <- ""
    
    showModal(modalDialog(
      title = "Logged out",
      "You have been logged out successfully. The temporary session was restored.",
      easyClose = TRUE
    ))
    
    
  })
  
  # React to data load events: If a user is logged in, save the data set to his file.
  observe({
    dataLoaded()
    
    if (user_id$authenticated) {
      saveRDS(csite_list, user_id$file)
      if (DEBUG_MODE) cat(" --> saving user data to .rds file: ", user_id$file, ".\n")
    }
  })
  
  ## Plume Diagnostics Panel ###################################################
  
  checkPlumeStats <- reactive({
    cat("\n* checkPlumeStats()\n")
    cat("input$ground_porosity",input$ground_porosity)
    # Detect when model fit changed.
    BP_modelfit_done()
    input$UpdateReducedWellFittedModel
    input$aggregate_select_sp
    
    #input$solute_conc_contour
    csite$ui_attr$conc_unit_selected <<- input$solute_conc_contour
    print("react to change in units")
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Calculating Plume", value = 0)
    on.exit(progress$close())
    
    
    val <-                   getFullPlumeStats(csite, 
                             substance = input$solute_select_sp, 
                             plume_thresh = input$plume_thresh_pd,
                             ground_porosity = (input$ground_porosity / 1),
                             progressBar = progress,
                             UseReducedWellSet=FALSE
                             )
    
  if(isolate(input$ImplementReducedWellSet)){
      
      valreducedWellSet<-                getFullPlumeStats(csite, 
                                         substance = input$solute_select_sp, 
                                         plume_thresh = input$plume_thresh_pd,
                                         ground_porosity = (input$ground_porosity / 1),
                                         progressBar = progress,
                                         UseReducedWellSet=isolate(input$ImplementReducedWellSet)

                            )
      
  }else{
    
    valreducedWellSet<-NULL
    
  }  
    # If there is any plume mass, show the plot and hide the message text, and vice versa. 
    if (all(is.na(val$mass))) {
      shinyjs::show("plume_diagn_msg_div")
      shinyjs::hide("plume_diagn_plot_div")
      shinyjs::hide("plume_save_btn_div")
    } else {
      shinyjs::show("plume_diagn_plot_div", anim = FALSE)
      shinyjs::show("plume_save_btn_div", anim = FALSE)
      shinyjs::hide("plume_diagn_msg_div", anim = FALSE)
    }

    return(list(plume_stats=val,plume_statsreducedWellSet=valreducedWellSet))
  })
  

  output$plume_diagn_msg <- renderUI({
    #cat("* plume_diagn_msg <- renderUI()\n")
    
    # Detect changes in the Options.
    optionsSaved()
    
    # Detect if stats can not be displayed (hides this text box).
    checkPlumeStats()
    
    # Isolate the inputs (so a change in the sidebar does not trigger this fct.)
    isolate(
     HTML(paste0(tags$b(input$solute_select_sp), 
                 ": Unable to calculate plume statistics for a threshold value of ",
                 "<b>", input$plume_thresh_pd, " ug/l</b>. ",
                 # "Select a different plume threshold and retry.",
                 tags$p(),
                 tags$p("Use the ", tags$a(id = "togglePlumeBoundary", "Estimate Boundary", href = "#"), "tab for assistance in selecting a suitable plume threshold value.")
     ))
    )
  
  })
  
  
  output$plume_estimate_plot <- renderPlot({
    cat("plume_estimate_plot <- renderPlot()\n")
    #cat("KJKJ\n")
    # Detect with model fit changed.
    BP_modelfit_done()
    if (reaggregateData()) { return(NULL) }# Stops calling reaggregation twice...
    input$UpdateReducedWellFittedModel
    
    plotPlumeEst(csite, input$solute_select_sp, input$plume_thresh_pd,input$ImplementReducedWellSet)
    
  })
  
  
  output$plume_diagn_plot <- renderPlot({
    #cat("plume_estimate_plot <- renderPlot()\n")
    
    # Detect changes in the Options.
    # optionsSaved()
    
    # Re-evaluate plume statistics if any reactive expression changes. 
    # The return value is the full plume statistics (for all timesteps). 
    #isolate(plume_stats <- checkPlumeStats())
    #input$UpdateReducedWellFittedModel
    #input$solute_conc_contour  - replot when units are changed...
    reaggregateData()
   
    
    plume_stats <- checkPlumeStats()
    plotPlumeTimeSeries(plume_stats,input$ImplementReducedWellSet)
    
    
  })
  
  ########### Well Redundancy Analysis Section #######################
  
  ### Well Redundancy modal help dialog. 
  #The well redundancy feature allows a user to drop a well or a combination of
  # wells from the analysis and investigate the resultant impact. The primary intent of this tool is to understand
  # which wells may have the most influence and also provide supporting evidence that the conclusions of the
  # analysis would not be significantly different with the omission of some certain wells. 
  # 
  # observeEvent(input$show_WellRedundancy_help,{
  #   showModal(modalDialog("The well redundancy feature allows a user to drop a well or a combination of wells from the analysis and investigate the resultant impact.","kjhjhjh",tags$div("For more details the GWSDAT user manual ", tags$a(target="_blank",href = 'http://gwsdat.net/gwsdat_manual/', "here")), title = "Well Redundancy Analysis"))
  # })
  # 
  observeEvent(input$show_WellRedundancy_help,{
    showModal(
      modalDialog(HTML("<ul> <li><font size='+0.5'> The well redundancy feature allows a user to drop a well or a combination of wells from the analysis and investigate the resultant impact.
                   Select the wells to be omitted from the listbox and press the `Update Model` button to fit the reduced well data set. Use the checkbox to toggle between the full and reduced well data set. <br><br></font></li></ul>"), 
                  
                  HTML("<ul> <li><font size='+0.5'> The order of wells in the list box are presented such that the wells which have been estimated to have the least 
                  influence on the spatiotemporal solute concentration smoother are presented first. This order is established via a procedure fully documented 
                       <a href='https://github.com/peterradv/Well-Influence-Analysis' target='_blank'>here</a>.  This <u>approximation</u> procedure has been adopted to greatly increase computational speed and efficiency. 
                  For this reason, the order of wells is to be interpreted as a <u>guide</u> only.  </font></li></ul><br>"),
                  HTML("<ul> <li><font size='+0.5'> The well order is dependent on the current selection of omitted wells and will only be updated once the model is updated. For this reason, it is suggested to omit wells in an incremental one-by-one fashion, i.e. update the model every time a well is removed or added.  </font></li></ul><br>"),
                  HTML("<ul> <li><font size='+0.5'> Well redundancy analysis is substance specific which means the importance of wells may be different for each substance. </font></li></ul><br>"),
                  HTML("<font size='+0.5'> For more details see the GWSDAT user manual <a href='http://gwsdat.net/gwsdat_manual/' target='_blank'>here</a>.</font>"),
                  title = "GWSDAT Well Redundancy Analysis Feature",size="l",footer = modalButton("Close"),easyClose=T))
  })
  
  
  #HTML("I like <u>turtles</u>"), HTML("<a href='https://github.com/peterradv/Well-Influence-Analysis' target='_blank'>here</a>.")#tags$a(target="_blank",href = 'https://github.com/peterradv/Well-Influence-Analysis', "here"),
  
  ### Refit the spline model to all solutes with selected wells omitted.
  observeEvent(input$UpdateReducedWellFittedModel,{
    csite<<-RefitModel(csite,input$solute_select_sp,input$sample_Omitted_Wells)

    if(!inherits(csite$Reduced.Fitted.Data[[input$solute_select_sp]]$Model.tune,"try-error")){
      updateSelectInput(session,"sample_Omitted_Wells", selected=input$sample_Omitted_Wells,choices = c(input$sample_Omitted_Wells,csite$Reduced.Fitted.Data[[input$solute_select_sp]]$Model.tune$best.model$Imetrics$Wellorder))#csite$ui_attr$sample_loc_names)
    }
    
    })
  
  observeEvent(input$ImplementReducedWellSet,{
    
    #If no wells selected in the first instance copy over existing fitted model - save times. 
    if(is.null(csite$Reduced.Fitted.Data) & input$ImplementReducedWellSet & is.null(input$sample_Omitted_Wells)){
      csite[["Reduced.Fitted.Data"]]<<-csite[["Fitted.Data"]]
      csite[["Reduced.Fitted.Data.GW.Flows"]]<<-csite[["GW.Flows"]]
      
      if(!inherits(csite$Reduced.Fitted.Data[[input$solute_select_sp]]$Model.tune,"try-error")){
        updateSelectInput(session,"sample_Omitted_Wells", selected=input$sample_Omitted_Wells,choices = c(input$sample_Omitted_Wells,csite$Reduced.Fitted.Data[[input$solute_select_sp]]$Model.tune$best.model$Imetrics$Wellorder))#csite$ui_attr$sample_loc_names)
      }
      
    }
    
    # Refit the spline model on initial selection of ReducedWellset implementation
    if(is.null(csite$Reduced.Fitted.Data) & input$ImplementReducedWellSet){
      
      csite<<-RefitModel(csite,input$solute_select_sp,input$sample_Omitted_Wells)
      
      if(!inherits(csite$Reduced.Fitted.Data[[input$solute_select_sp]]$Model.tune,"try-error")){
        updateSelectInput(session,"sample_Omitted_Wells", selected=input$sample_Omitted_Wells,choices = c(input$sample_Omitted_Wells,csite$Reduced.Fitted.Data[[input$solute_select_sp]]$Model.tune$best.model$Imetrics$Wellorder))#csite$ui_attr$sample_loc_names)
      }
        
    }
    
  })
  
  
  ## Update corresponding plume threshold values in UIOptions when updated in Spatial Plot. 
  observeEvent(input$plume_thresh_pd, {

    updateNumericInput(session,paste0("plume_thresh_",which(input$solute_select_sp==csite$ui_attr$solute_names)),value=input$plume_thresh_pd)
    #### Make sure plume_thresh UI attr is updated - bit ugly but immediate save doesnt work as numeric input not updated immediately 
    csite$ui_attr$plume_thresh[input$solute_select_sp]<<-input$plume_thresh_pd

  })
  
  observeEvent(input$solute_select_sp, {
    #updateSelectInput(session, "solute_select_ts", selected = input$solute_select_sp )
    tr<-as.numeric(csite$ui_attr$plume_thresh[as.character(input$solute_select_sp)])
    updateNumericInput(session,"plume_thresh_pd",value=tr)
    
    
    
    if(is.null(csite$Reduced.Fitted.Data)){
      
      if(!inherits(csite$Fitted.Data[[input$solute_select_sp]]$Model.tune,"try-error")){
         updateSelectInput(session,"sample_Omitted_Wells", selected=input$sample_Omitted_Wells,choices = c(input$sample_Omitted_Wells,csite$Fitted.Data[[input$solute_select_sp]]$Model.tune$best.model$Imetrics$Wellorder))
      }else{
         updateSelectInput(session,"sample_Omitted_Wells", selected=input$sample_Omitted_Wells,choices = c(input$sample_Omitted_Wells,csite$ui_attr$sample_loc_names))
       }
      
    }else{
      
       #if(!inherits(try(csite$Reduced.Fitted.Data[[input$solute_select_sp]]$Model.tune$best.model$Imetrics$Wellorder),"try-error")){
       if(!inherits(csite$Reduced.Fitted.Data[[input$solute_select_sp]]$Model.tune,"try-error")){
         updateSelectInput(session,"sample_Omitted_Wells", selected=input$sample_Omitted_Wells,choices = c(input$sample_Omitted_Wells,csite$Reduced.Fitted.Data[[input$solute_select_sp]]$Model.tune$best.model$Imetrics$Wellorder))
       }else{
         updateSelectInput(session,"sample_Omitted_Wells", selected=input$sample_Omitted_Wells,choices = c(input$sample_Omitted_Wells,csite$ui_attr$sample_loc_names))
       }
      
    }
    
    # if(is.null(csite$Reduced.Fitted.Data)){ #No reduced model fit as yet attempt to use Full model fit well order instead.
    #   updateSelectInput(session,"sample_Omitted_Wells", selected=input$sample_Omitted_Wells,choices = c(input$sample_Omitted_Wells,csite$Fitted.Data[[input$solute_select_sp]]$Model.tune$best.model$Imetrics$Wellorder))
    # }
    # ## Update well order in Well Redundancy listbox for newly selected solutes. 
    # if(!inherits(try(csite$Reduced.Fitted.Data[[input$solute_select_sp]]$Model.tune$best.model$Imetrics$Wellorder),"try-error")){
    #   updateSelectInput(session,"sample_Omitted_Wells", selected=input$sample_Omitted_Wells,choices = c(input$sample_Omitted_Wells,csite$Reduced.Fitted.Data[[input$solute_select_sp]]$Model.tune$best.model$Imetrics$Wellorder))#csite$ui_attr$sample_loc_names)
    # }
    
  })
  
  #------------------------------------------------------------------#
  
  
  
  ## Time-Series Panel #########################################################
  
  # Plot time-series window
  output$time_series <- renderPlot({
    if (DEBUG_MODE)
      cat("* in time_series <- renderPlot()\n")
  
    optionsSaved()
    
    # Update control attributes from reactive variables. 
    csite$ui_attr$conc_unit_selected <<- input$solute_conc
    csite$ui_attr$ts_options[1:length(csite$ui_attr$ts_options)] <<- FALSE
    csite$ui_attr$ts_options[input$ts_true_options] <<- TRUE
    
    
    #plotTimeSeries(csite, input$solute_select_ts, input$sample_loc_select_ts, input$check_threshold)
    plotTimeSeries(
      csite=csite,
      substance=input$solute_select_ts,
      location=input$sample_loc_select_ts,
      show_thresh=input$check_threshold,
      timepoint=input$timepoint_ts_idx
    )
    
  })

  
  ## Simple Background Process #################################################
  
 
  # This function is triggered on startup and whenever the reactive variable
  # 'BP_modelfit_done()' changes its (boolean) value. As long as BP_modelfit_done == FALSE,
  # a background process (BP) is running and the function is re-run in intervals (see invalidateLater).
  # It checks if the BP produced its results into the file '  BP_modelfit_outfile'.
  #fitPSplineChecker <- reactive({
  observe({
    if (BP_method != 'simple')
      return()
    
    # For logging.
    isolate(alog <- app_log())
    
    # Do not re-execute (invalidate) this function if reactive flag 
    # 'BP_modelfit_done' is TRUE. Need 'BP_modelfit_done' to be reactive, so that
    # fitPSplineChecker() executes whenever 'BP_modelfit_done' changes its value.
    if (!BP_modelfit_running()) {
      return(TRUE)
    }
    
    # Re-execute this function every X milliseconds.
    invalidateLater(2000)
    
    if (!file.exists(BP_modelfit_outfile)) {
        app_log(paste0(alog, '[PSpline] Fitting in progress.\n'))
        return(TRUE)
    }
    
    # Only pass the file name. The data_id is saved inside this file, which is read by evalJobPspline.
    evalJobPspline(BP_modelfit_outfile)
    
    showNotification("P-Spline fit completed successfully.", type = "message", duration = 7)
    
    BP_modelfit_running(FALSE)
    BP_modelfit_done(BP_modelfit_done() + 1) # Notify observers that fitting took place.  
    cat("** end of fitPSplineChecker()\n")
    
    # 
    # OLD_Version <- FALSE
    # 
    # if (OLD_Version) {
    #   # Attempt to read output file, 'x' will not exist if this fails (usually when 
    #   # writing to the file was not completed by the external process).
    #   try(fitdat <- readRDS(BP_modelfit_outfile), silent = TRUE)
    #   
    #   # Evaluates to TRUE if file above was read successful.
    #   if (exists('fitdat')) { 
    #     
    #     
    #     BP_modelfit_running(FALSE)               # Stops re-execution of this observer.
    #     BP_modelfit_done(BP_modelfit_done() + 1) # Triggers render functions that depend on new model fit.
    #     
    #     app_log(paste0(alog, '[PSpline] Calcuation done. File read.\n'))
    #     showNotification("P-Spline fit completed successfully.", type = "message", duration = 7)
    #     
    #   
    #    
    #     # On failure (fitdat == NULL), revert to previous settings.
    #     if (is.null(fitdat)) {
    #       showNotification("P-Splines: Fitting data with new number of segments failed.", type = "error", duration = 10)
    #         
    #       csite$GWSDAT_Options[['PSplineVars']][['nseg']] <<- prev_psplines_knots
    #       updateSelectInput(session, "psplines_resolution", selected = prev_psplines_resolution)
    #       updateTextInput(session, "psplines_knots", value = prev_psplines_knots)
    #     } else {
    #          
    #       
    #       # Update the current data.
    #       csite$Fitted.Data    <<- fitdat$Fitted.Data
    #       csite$Traffic.Lights <<- fitdat$Traffic.Lights
    #       
    #       # Copy back the altered csite list.
    #       # Write back the fitted data
    #       # FIXME: Make sure to write to the right csite_list data set (use index or name)
    #       # 1. Either remember csite_selected_idx (pass to BP and back)
    #       #    Fails if content of csite_list changes (e.g. data deleted, index shifts).
    #       # 2. By data name: lookup data name 
    #       # 3. Use unique data ID, find it inside csite_list and update. <<--- Cleanest approach, would need to update other code too. 
    #       #
    #       csite_list[[csite_selected_idx]] <<- csite  
    #          
    #       # Save the current state, in case it is changed again and fails.
    #       prev_psplines_resolution <<- input$psplines_resolution
    #       prev_psplines_knots <<- input$psplines_knots
    #     } 
    #     
    #     return(TRUE)
    #   } 
    # 
    #   app_log(paste0(alog, '[PSpline] File exists but not completed.\n'))
    # }
    # 
    # return(FALSE)
    # 
  })
  
  
  ## DBI Job Queue ##################################################################
  
  # Contains the name of the database file and the connection handle.
  jq_db <- NULL
  
  # Find out if the Database packages are installed.
  if (requireNamespace('DBI', quietly = TRUE) && requireNamespace('RSQLite', quietly = TRUE)) {

    # Create job queue data base, currently also opens connection.
    jq_db <- createJobQueue()
    cat('Background process set to \'queue\' using DBI and RSQLite with DB file: ', jq_db$dbPath, '\n')
    
    # Set background process method to queue based.
    BP_method <- 'queue'
    
  }

  # Contains a reactive data.frame for each job type.
  job_queue <- reactiveValues(new = NULL, run = NULL, done = NULL)
  
  
  
  evalJobPspline <- function(result_file, data_id = 0) {
    
    cat('* inside evalJobPspline\n')
    
    # Attempt to read output file, 'x' will not exist if this fails (usually when 
    # writing to the file was not completed by the external process).
    try(results <- readRDS(result_file), silent = TRUE)
    
    # Evaluates to FALSE if file could not be read.
    if (!exists('results'))
      return(NULL)
    
    # Extract data_id from 'results' in case it was not passed to this function.
    # The BP_method == 'simple' procedure uses this approach.
    if (data_id == 0) data_id <- results$data_id
    
    # Lookup the affected data set by data_id, if it does not exist, raise a warning.
    # The likely cause for this is that the data set was deleted while the job was still
    # running.
    csite_idx <- getDataIndexByID(csite_list, data_id)
  
    if (csite_idx == -1) {
      showNotification(paste0("P-Splines: Failed to identify data set with ID ", data_id, ". Data might have been deleted."), 
                       type = "warning", duration = 10)
      return(FALSE)
    }
    
    fitdat <- results$fitdat
    params <- results$params
    
    # On failure (fitdat == NULL), revert to previous settings.
    if (is.null(fitdat)) {
      showNotification("P-Splines: Fitting data with new number of knots failed.", type = "error", duration = 10)
      return(FALSE)
    }
    
    # Update the current data.
    csite_list[[csite_idx]]$Fitted.Data    <<- fitdat$Fitted.Data
    csite_list[[csite_idx]]$Traffic.Lights <<- fitdat$Traffic.Lights
    csite_list[[csite_idx]]$GWSDAT_Options$PSplineVars$nseg <<- params$PSplineVars$nseg
    
    # If the altered data was the one that is currently selected, copy it back.
    if (csite_idx == csite_selected_idx) {
      csite <<- csite_list[[csite_selected_idx]]
    }
     
    # Update the inputs.
    #updateSelectInput(session, "psplines_resolution", selected = prev_psplines_resolution)
    updateTextInput(session, "psplines_knots", value = params$PSplineVars$nseg)
    
    # Save the current state, in case it is changed again and fails.
    #prev_psplines_resolution <<- input$psplines_resolution
    #prev_psplines_knots <<- input$psplines_knots
   
    return(TRUE)
  }
    
    
  # Periodically check the job queue and process new and finished jobs.
  observe({
    
    # If not enabled, this observer will not invalidate anymore
    if (is.null(jq_db))
      return()
    
    invalidateLater(5000)
    
    # Check if connection to db is still open.
    # .. (reconnect if not) --> move to watchQueue()
    
    done_jobs <- evalQueue(jq_db)
    
    # Each element in the list 'done_jobs' is a job that requires evaluation.
    if (!is.null(done_jobs)) {
      
      # Loop over the jobs..
      for (job in done_jobs) {
        
        # Select the proper evaluation method.
        if (job$script == "jqdb_pspline_fit.R") {
          
          # Attempt to evaluate result data, if it succeeds notify user and invalidate observers.
          if (evalJobPspline(job$outputfile, job$data_id)) {
            showNotification(paste0("P-Splines: Fit completed successfully for job ID ", job$job_id, "."), type = "message", duration = 10)
            BP_modelfit_done(BP_modelfit_done() + 1) # Notify observers that fitting took place.  
            
            if(isolate(input$ImplementReducedWellSet)){
            updateCheckboxInput(session,"ImplementReducedWellSet",value=FALSE)
            showNotification("Model resolution has been updated. Reselect Well Redundancy Analysis checkbox to update reduced well model.", type = "message", duration = 10)
            }
            
          }
          
        } else {
          stop("No evaluation routine found for script = ", job$script, ". Fix Me!\n")
        }
      }
    }
    
    # Fetch content of job queue. 
    queues <- infoQueue(con = jq_db$dbConn)  
    
    # Take out some columns to clean up display.
    queues$jq$inputfile <- NULL
    queues$jq$outputfile <- NULL
    queues$jq$Rcmd <- NULL
    
    queues$rq$inputfile <- NULL
    queues$rq$outputfile <- NULL
    queues$rq$Rcmd <- NULL
    
    # Update reactive variable, if it changes it changes it will trigger the job queue display.
    job_queue$new <- queues$jq
    job_queue$run <- queues$rq
    job_queue$done <- queues$dq

  })
  
  
  
  # Re-Aggregate the data in case the aggregation type was changed.
  reaggregateData <- reactive({
    cat("* entering reaggregateData()\n")
    
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
    
    if (DEBUG_MODE)
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
      try(csite$Fitted.Data[[cont]]$Cont.Data$AggDate <<- agg_col) ## encapsulate with try to handle GW and NAPL only data sets..
    }
    
    # Re-Calculate Traffic Lights (depends on aggregation date).
    csite$Traffic.Lights <<- NULL
    
    tryCatch(
      csite$Traffic.Lights <<- calcTrafficLights(csite$All.Data, csite$Fitted.Data, 
                                                 csite$GWSDAT_Options$smThreshSe, 
                                                 csite$GWSDAT_Options$smMethod),
      error = function(e) {
        showNotification(paste0("Failed to calculate trend table: ", e$message), type = "error", duration = 10)
      }
    )
    
    # Re-Calculate groundwater flows (depends on aggregation date).
    csite$GW.Flows <<- evalGWFlow(csite$All.Data$Agg_GW_Data)
    
    # Update UI time points of slider.
    dates_tmp <- format(csite$All.Data$All_Agg_Dates, "%d-%m-%Y")
    csite$ui_attr$timepoints   <<- dates_tmp

    # Set new time point to last date.
    new_timepoint_idx <- length(dates_tmp)
    
    # Update slider inputs: Spatial plot and in Trend table.
    outp <- pasteAggLimit(csite$ui_attr$timepoints[new_timepoint_idx], csite$GWSDAT_Options$Aggby)
    
    
    if(new_timepoint_idx==input$timepoint_sp_idx){ 
      
    #check for event when new length time points is unchanged. Manually change to something else - so update of slider label is performed.
    updateSliderInput(session, "timepoint_sp_idx", value = max(1,new_timepoint_idx-1),
                      min = 1, max = length(csite$ui_attr$timepoints), label ="", step = 1)
    updateSliderInput(session, "timepoint_tt_idx", value = max(1,new_timepoint_idx-1),
                      min = 1, max = length(csite$ui_attr$timepoints), label = "", step = 1) 
  
    }
    
    updateSliderInput(session, "timepoint_sp_idx", value = new_timepoint_idx,
                      #min = 1, max = length(csite$ui_attr$timepoints), label = paste0("Time: ", outp), step = 1)
                      min = 1, max = length(csite$ui_attr$timepoints), label ="", step = 1)
    
    updateSliderInput(session, "timepoint_tt_idx", value = new_timepoint_idx,
                      #min = 1, max = length(csite$ui_attr$timepoints), label = paste0("Time: ", outp), step = 1)
                      min = 1, max = length(csite$ui_attr$timepoints), label = "", step = 1)    
    
    
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
#  observeEvent(input$timepoint_sp_idx, {
#       
#    # Retrieve date and convert to the aggregation time interval. 
#    timep <- csite$ui_attr$timepoints[input$timepoint_sp_idx]
#    outp <- pasteAggLimit(timep, csite$GWSDAT_Options$Aggby)
        
# #   updateSliderInput(session, "timepoint_sp_idx", label = paste0("Time: ", outp))
#  })
    
 output$timepoint_sp_idx_label <- renderText({
    timep <- csite$ui_attr$timepoints[input$timepoint_sp_idx]
    outp <- pasteAggLimit(timep, csite$GWSDAT_Options$Aggby)
    paste0("Time: ", outp)
 })

 # observeEvent(input$timepoint_tt_idx, {
 #   # cat("* in observeEvent: timepoint_tt_idx\n")
 #   
 #   # Not updating here, because 'input$timepoint_sp_idx' is directly used for
 #   # plotting. Saving to 'csite$ui_attr$timepoint_sp_idx' is only used in 
 #   # 'Save Session' and reading from it inside rndAnalyse <- renderUI().
 #   #
 #   #csite$ui_attr$timepoint_tt_idx <<- input$timepoint_tt_idx
 #   
 #   timep <- csite$ui_attr$timepoints[input$timepoint_tt_idx]
 #   outp <- pasteAggLimit(timep, csite$GWSDAT_Options$Aggby)
 #   update
#    updateSliderInput(session, "timepoint_tt_idx", label = paste0("Time: ", outp))
 # })
  

    output$timepoint_tt_idx_label = renderText( {
        timep <- csite$ui_attr$timepoints[input$timepoint_tt_idx]
        outp <- pasteAggLimit(timep, csite$GWSDAT_Options$Aggby)
        paste0("Time: ", outp)
    })
 
    
  #
  # Plot ImagePlot
  #
  output$image_plot <- renderPlot({
    
    #cat("* entering image_plot()\n")

    # React to new fitted model.
    BP_modelfit_done()

    # React to changes in the Options panel.
    optionsSaved() 

    if (reaggregateData()) { return(NULL) }
    
    # Update control attributes from reactive variables (Possibly integrate this
    # into function arguments of plotSpatialImage()?).
    csite$ui_attr$spatial_options[1:length(csite$ui_attr$spatial_options)] <<- FALSE
    csite$ui_attr$spatial_options[input$imageplot_options] <<-  TRUE
    csite$ui_attr$gw_selected <<- input$gw_flows
    csite$ui_attr$contour_selected <<- input$imageplot_type
    csite$ui_attr$conc_unit_selected <<- input$solute_conc_contour
    
    ## Make Spatial plot reactive to Well Redunancy Analysis
    input$UpdateReducedWellFittedModel
    
    #start.time = Sys.time()
    plotSpatialImage(csite=csite, substance =input$solute_select_sp, 
                     timepoint=as.Date(csite$ui_attr$timepoints[input$timepoint_sp_idx], "%d-%m-%Y"),
                     app_log=app_log,UseReducedWellSet=input$ImplementReducedWellSet,sample_Omitted_Wells=isolate(input$sample_Omitted_Wells))
                     
    #end.time <- Sys.time()
    
    #time.passed <- (end.time - start.time) * 1000
    #time.log <- paste0("[TIME_MEASURE] plotSpatialImage(): ", time.passed, " milliseconds.\n")
    #if (DEBUG_MODE) cat(time.log)
    
    #isolate(alog <- app_log())
    #app_log(paste0(alog, time.log))
    
  })
    
  
  
  ### adding Disclaimer in the plottrendtable
  
  output$note <- renderText({
    "Disclaimer : The slider displays only the dates which are available in the data."
  })
  
  
  
  output$trend_table <- renderUI({
    
    cat("* entering trend_table()\n")
    
    # Detect changes in the Traffic.Lights (depends on model fit).
    BP_modelfit_done()
    
    # React to changes in the Options panel.
    optionsSaved() 
  
    # If aggregation took place, return here because the timepoint index has to
    # be updated before the actual plotting happens.
    if (reaggregateData()) {
      if (DEBUG_MODE) cat("[trend_table <- renderUI()] aggregation took place, exiting image_plot()\n")
      return(NULL)
    }

    plotTrendTable(csite, 
                   as.Date(csite$ui_attr$timepoints[input$timepoint_tt_idx], "%d-%m-%Y"),
                #input$timepoint_tt_idx,
               input$trend_or_threshold, 
               input$color_select_tt,
               input$substance_select_tt)
  })

  
  # Plot the legend for the traffic lights table.
  output$trend_legend <- renderUI({plotTrendTableLegend()  })
  

  
  #
  # Plot Well Report
  #
  output$well_report_plot <- renderPlot({
    
    use_log_scale    <- if (input$logscale_wr == "Yes") {TRUE} else {FALSE}
    
    # Detect changes in Traffic.Lights (depends on model fit). 
    BP_modelfit_done()
    
    input$update_wellreport_plot
    
    plotWellReport(csite, isolate(input$solute_select_wr), isolate(input$sample_loc_select_wr), use_log_scale)
    
  })
  
  #
  # Plot SpatioTemporal Predictions
  #
  output$stpredictions_plot <- renderPlot({
    
    use_log_scale <- if (input$logscale_stp == "Yes") {TRUE} else {FALSE}
    
    # Detect changes in model fit.
    BP_modelfit_done()
    
    input$update_stpredictions_plot
    plotSTPredictions(csite, input$solute_select_stp, isolate(input$sample_loc_select_stp), use_log_scale, input$solute_conc_stp)
    
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
    updateSelectInput(session, "solute_select_sp", selected = input$solute_select_ts[length(input$solute_select_ts)] ) 
  })
  
  #observeEvent(input$solute_select_sp, {
  #  updateSelectInput(session, "solute_select_ts", selected = input$solute_select_sp )  
  #})
  
  
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
      
      if (input$export_format_ts == "pptx") {
        
        makeTimeSeriesPPT(csite=csite, fileout=file, substance=input$solute_select_ts, location=input$sample_loc_select_ts,show_thresh=input$check_threshold,timepoint=input$timepoint_ts_idx,
                          width  = input$img_width_px, height = input$img_height_px)
        
      } 
      else {
        
        if (input$export_format_ts == "png") png(file, width = input$img_width_px, height = input$img_height_px)
        if (input$export_format_ts == "pdf") pdf(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
        if (input$export_format_ts == "ps")  postscript(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
        if (input$export_format_ts == "jpg") jpeg(file, width = input$img_width_px, height = input$img_height_px, quality = input$img_jpg_quality) 
        if (input$export_format_ts == "wmf") grDevices::win.metafile(file, width = input$img_width_px/100, height = input$img_height_px/100)
        
        plotTimeSeries(csite=csite, substance=input$solute_select_ts, location=input$sample_loc_select_ts,show_thresh=input$check_threshold,timepoint=input$timepoint_ts_idx)
        dev.off()
      }
    }
  )
  
  #-----------------------------------------------------------------------------------------------------------
  
  #--------------------------save_spatial_plot--------------------------------------------------------
  
  #Below syntax used to select the asc option from the drop-down list to the save the image ing ascii format. 
  #The resolution refers to the size of each grid cell in the spatial dataset.
  #Common Resolution Values:
  
  #High Resolution (Detailed)
  #1 meter (e.g., 1x1 m grid for small areas like urban planning)
  #10 meters (e.g., 10x10 m grid for detailed environmental studies)
  
  
  #Medium Resolution
  #30 meters (e.g., Landsat satellite data for regional analysis)
  #90 meters (e.g., global DEMs like SRTM for large-area analysis)
  
  
  #3. Low Resolution (Generalized)
  #100 meters (e.g., for large-scale terrain or climate models)
  #1 kilometer (e.g., global climate or topographic models)
  
  
  #Choosing Resolution:
  #Smaller Values: More detail, larger file sizes, and longer processing time.
  #Larger Values: Less detail, smaller file sizes, and faster processing.
  output$res_ui <- renderUI({
    if (input$export_format_sp == "asc") {
      div(style = "color:red",
          
          numericInput("resolution", "Enter Resolution (Smaller Values: More detail/Larger Values: Less detail)*" , value = ""))
    }
  })
  
  observeEvent(input$export_format_sp , {
    if (input$export_format_sp == "asc" ) {
      shinyjs::disable("save_spatial_plot")
    }
    if (input$export_format_sp != "asc"){
      shinyjs::enable("save_spatial_plot")
    }
  })
  observeEvent(input$resolution , {
    if (!is.na(input$resolution)) {
      shinyjs::enable("save_spatial_plot")
    }
    if (is.na(input$resolution)) {
      shinyjs::disable("save_spatial_plot")
    }
  })
  output$save_spatial_plot <- downloadHandler(
    
    filename <- function() { 
      paste("spatial_plot.", input$export_format_sp, sep = "")
    },
     
    content <-  function(file) {
     
      if (input$export_format_sp == "pptx") {
        

        plotSpatialImagePPT(csite, file, input$solute_select_sp, as.Date(csite$ui_attr$timepoints[input$timepoint_sp_idx], "%d-%m-%Y"),
                       width  = input$img_width_px, height = input$img_height_px,UseReducedWellSet=input$ImplementReducedWellSet,sample_Omitted_Wells=input$sample_Omitted_Wells)
      
        } else if (input$export_format_sp == "tif"){
         
          
          PlotSpatialImageTIF(csite, file, input$solute_select_sp, as.Date(csite$ui_attr$timepoints[input$timepoint_sp_idx], "%d-%m-%Y"),UseReducedWellSet=input$ImplementReducedWellSet)
          
        } else if (input$export_format_sp == "asc") {
          PlotSpatialImageAsc(
            csite,
            file,
            input$solute_select_sp,
            as.Date(csite$ui_attr$timepoints[input$timepoint_sp_idx], "%d-%m-%Y"),
            UseReducedWellSet = input$ImplementReducedWellSet,
            resolution = input$resolution
          )
          
        }
      
      else {
          
          if (input$export_format_sp == "png") png(file, width = input$img_width_px, height = input$img_height_px)
          if (input$export_format_sp == "pdf") pdf(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
          if (input$export_format_sp == "ps") postscript(file, width = input$img_width_px / csite$ui_attr$img_ppi, height = input$img_height_px / csite$ui_attr$img_ppi) 
          if (input$export_format_sp == "jpg") jpeg(file, width = input$img_width_px, height = input$img_height_px, quality = input$img_jpg_quality) 
          if (input$export_format_sp == "wmf") grDevices::win.metafile(file, width = input$img_width_px/100, height = input$img_height_px/100)
        
          plotSpatialImage(csite, input$solute_select_sp, as.Date(csite$ui_attr$timepoints[input$timepoint_sp_idx], "%d-%m-%Y"),UseReducedWellSet=input$ImplementReducedWellSet,sample_Omitted_Wells=input$sample_Omitted_Wells)
         
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
      
      if (input$export_format_wr == "pptx") {
        
        plotWellReportPPT(csite, file, input$solute_select_wr, input$sample_loc_select_wr, use_log_scale,
                       width  = input$img_width_px_wide, height = input$img_height_px_wide)
        
        
      } else {
        
        if (input$export_format_wr == "png") png(file, width = input$img_width_px_wide, height = input$img_height_px_wide)
        if (input$export_format_wr == "pdf") pdf(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_wr == "ps") postscript(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_wr == "jpg") jpeg(file, width = input$img_width_px_wide, height = input$img_height_px_wide, quality = input$img_jpg_quality) 
        if (input$export_format_wr == "wmf") grDevices::win.metafile(file, width = input$img_width_px_wide/100, height = input$img_height_px_wide/100)
        
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
      
      
      if (input$export_format_pd == "pptx") {
        
        plotPlumeTimeSeriesPPT(plume_stats, input$ImplementReducedWellSet, file,
                               width = input$img_width_px_wide, height = input$img_height_px_wide)
        
      } else {
        
        if (input$export_format_pd == "png") png(file, width = input$img_width_px_wide, height = input$img_height_px_wide)
        if (input$export_format_pd == "pdf") pdf(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_pd == "ps")  postscript(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_pd == "jpg") jpeg(file, width = input$img_width_px_wide, height = input$img_height_px_wide, quality = input$img_jpg_quality)
        if (input$export_format_pd == "wmf") grDevices::win.metafile(file, width = input$img_width_px_wide/100, height = input$img_height_px_wide/100)
        #if (input$export_format_pd == "wmf") win.metafile(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        
        plotPlumeTimeSeries(plume_stats,input$ImplementReducedWellSet)
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
      
      tmp_out <- printPlumeStatsCSV(plume_stats$plume_stats)
      
      if(!is.null(plume_stats$plume_statsreducedWellSet)){
        tmp_out$DataSet="Full"
        tmp_outreducedWellSet <- printPlumeStatsCSV(plume_stats$plume_statsreducedWellSet)
        tmp_outreducedWellSet$DataSet<-"Well Reduced"
        tmp_out<-rbind(tmp_out,tmp_outreducedWellSet)
      }
      
      write.csv(tmp_out, file,row.names=FALSE)
      
    }
  )
  
  output$save_stpredictions_plot <- downloadHandler(
    
    filename <- function() { 
      paste("stpredictions.", input$export_format_stp, sep = "")
    },
    
    content <-  function(file) {
      
      use_log_scale <- if (input$logscale_stp == "Yes") {TRUE} else {FALSE}
      
      if (input$export_format_stp == "pptx") {
        
        plotSTPredictionsPPT(csite, file, input$solute_select_stp, input$sample_loc_select_stp, 
                             use_log_scale, input$solute_conc_stp,
                             width = input$img_width_px_wide, 
                             height = input$img_height_px_wide)
        
      } else {
        
        if (input$export_format_stp == "png") png(file, width = input$img_width_px_wide, height = input$img_height_px_wide)
        if (input$export_format_stp == "pdf") pdf(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_stp == "ps")  postscript(file, width = input$img_width_px_wide / csite$ui_attr$img_ppi, height = input$img_height_px_wide / csite$ui_attr$img_ppi) 
        if (input$export_format_stp == "jpg") jpeg(file, width = input$img_width_px_wide, height = input$img_height_px_wide, quality = input$img_jpg_quality) 
        if (input$export_format_stp == "wmf")  grDevices::win.metafile(file, width = input$img_width_px_wide/100, height = input$img_height_px_wide/100)
        
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
        
        saveRDS(csite_list, file = file)
      }
    }
  )
  
    
  
  
  output$generate_spatial_anim_ppt <- downloadHandler(
    
    filename <- function() {
      paste("spatial_anim.pptx")
    },
    
    content <- function(file) {
      
      
      makeSpatialAnimation(csite, file, input$solute_select_sp,
                           input$img_width_px, input$img_height_px,
                           input$img_width_px_wide, input$img_height_px_wide,input$ImplementReducedWellSet,input$sample_Omitted_Wells)
      
    }
  )
  
########### GW Well Report Time Series Plotting ################################
output$generate_timeseries_anim_ppt <- downloadHandler(
    
    filename <- function() {
      paste("timeseries_anim.pptx")
    },
    
    content <- function(file) {
      
      makeTimeSeriesAnimationPPT(csite=csite, fileout=file, substance=input$solute_select_ts, location=input$gwwellreportsample_loc_select_wr,Layout=input$gwwellreportlayout,show_thresh=input$check_threshold,timepoint=input$timepoint_ts_idx,
                        width  = input$img_width_px, height = input$img_height_px)
      
    }
)
  
  
  
observeEvent(input$Optionsgenerate_timeseries_anim_ppt, {
                 showModal(GWWellReportModal(csite))
})
               
GWWellReportModal<-function(csite){
                 
              modalDialog(title=h3("Well Report"),
                  selectInput("gwwellreportsample_loc_select_wr", 'Select Monitoring Wells', choices = csite$ui_attr$sample_loc_names,selected =csite$ui_attr$sample_loc_names,multiple=T),
                  radioButtons("gwwellreportlayout", label = "Plot Layout (Rows x Columns)",
                               choices = c("1x1","1x2","2x1","2x2"), 
                               selected = "2x2"),
                  footer = tagList(downloadButton("generate_timeseries_anim_ppt", label = "Generate PowerPoint Report"),modalButton("Close")) #, icon = icon("file-movie-o")
                   
                 )
                 
 }            
               
  ## General Import Routines ###################################################
  
  
  # Can I move parts (or all) of this function into importTables?
  importData <- function(dname, dsource = "",subst_napl_vals=NULL) {
    
    ptm <- proc.time()
    
    
    if (is.null(DF_well <- parseTable(import_tables$DF_well, type = "wells"))) {
      showNotification("Nothing to import: Could not find at least one valid row entry in contaminant table.", 
                       type = "error", duration = 10)      
      return(NULL)
    }
    
    if (is.null(DF_conc <- parseTable(import_tables$DF_conc, type = "contaminant", 
                                      wells = unique(DF_well$WellName), 
                                      dsource = dsource))) {
      showNotification("Nothing to import: Could not find at least one valid row entry in contaminant table.", 
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
      
      pr_dat <- processData(all_data$solute_data, all_data$sample_loc, GWSDAT_Options, Aq_sel,subst_napl_vals)
      #if (class(pr_dat) == "dialogBox")
      if (inherits(pr_dat, "dialogBox"))
        return(pr_dat)
      
      if (is.null(pr_dat)) next
      
      ui_attr <- createUIAttr(pr_dat, GWSDAT_Options)
      
      # Build list with all data.
      csite <- list(All.Data       = pr_dat,
                    Fitted.Data    = NULL,
                    GWSDAT_Options = GWSDAT_Options,
                    Traffic.Lights = NULL,
                    ui_attr        = ui_attr,
                    Aquifer        = Aq_sel,
                    raw_contaminant_tbl = import_tables$DF_conc,
                    raw_well_tbl = import_tables$DF_well,
                    data_id = createDataID(csite_list)
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
    
    # Log the time it took to run this function
    time_passed <- (proc.time() - ptm)[1]
    app_log(paste0(app_log(), "Time to run formatData(): ", time_passed, " seconds\n"))
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
    if (DEBUG_MODE) cat("* in observeEvent: add_session_data (line 1189)\n")
    
    shinyjs::show(id = "uiDataAddSession")
    shinyjs::hide(id = "uiDataManager")
  })
  
  # Go (back) to Data Manager.
  shinyjs::onclick("gotoDataManager_a", showDataMng())
  shinyjs::onclick("gotoDataManager_b", showDataMng())
  shinyjs::onclick("gotoDataManager_c", showDataMng())
  shinyjs::onclick("gotoDataManager_d", showDataMng())
  shinyjs::onclick("gotoDataManager_e", showDataMng())

  shinyjs::onclick("restore_examples", {
    
    
    # Read Example Data
    ctmp <- readRDS(system.file("extdata", default_session_file, package = "GWSDAT"))
    
    # Extract data IDs from the current data set.
    data_ids <- c()
    for (ct in csite_list) {
      data_ids <- c(data_ids, ct$data_id)    
    }

    data_set_changes <- FALSE
    
    # Check if any Examples are already in the data set.
    for (ct in ctmp) {
      
      if (!(ct$data_id %in% data_ids)) {
        data_set_changes <- TRUE
        dataLoaded(dataLoaded() + 1)
        csite_list[[length(csite_list) + 1]] <- ct
      }
    }

  })
  
  
  ## Load Session Data ().rds ##################################################
 
  
  output$tbl_conc_sess <- rhandsontable::renderRHandsontable({
    
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
    
    #if (class(csite_tmp) != "GWSDAT_DATA_LIST") {
    if (!inherits(csite_tmp,"GWSDAT_DATA_LIST")) {
      showNotification(paste0("Uploaded .rds file ", inFile$name, " does not contain data of type GWSDAT (wrong class)."), type = "error", duration = 10 )
      shinyjs::reset("data_session_file")
      return(NULL)
    }
    
    
    # Create new data name if already exists. It needs to be unique.
    site_name <- csite_tmp[[1]]$GWSDAT_Options$SiteName
    new_name <- getValidDataName(csite_list, template = site_name, propose_name = site_name)
    csite_tmp[[1]]$GWSDAT_Options$SiteName <- new_name
    
    updateTextInput(session, "dname_sess", value = new_name)
    
    # Create a unique data ID if the one inside the new data set is already taken.
    if (getDataIndexByID(csite_list, csite_tmp[[1]]$data_id) != -1)
      csite_tmp[[1]]$data_id <- createDataID(csite_list)
   
    # Set the preview tables displayed on the right of the import panel.
    import_tables$new_csite <- csite_tmp[[1]] 
    import_tables$DF_conc   <- csite_tmp[[1]]$All.Data$Cont.Data
    import_tables$DF_well   <- csite_tmp[[1]]$All.Data$sample_loc$data
    
    
  })
  
  
  # Go to New Data Import (Button click).
  observeEvent(input$add_session_data,  {
    
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
    
    import_tables$DF_conc <- data.frame(matrix("", nrow = 1000, ncol = length(conc_header)),
                                        stringsAsFactors = FALSE)
    colnames(import_tables$DF_conc) <- conc_header
    
    class(import_tables$DF_conc$SampleDate) <- "Date"
    
    import_tables$DF_conc$WellName[1] <- "Sample Well"
    import_tables$DF_conc$SampleDate  <- Sys.Date()
    import_tables$DF_conc$Units[1] <- "ug/l"
    
  }
  
  createNewWellTable <- function() {
    
    well_tmp <- data.frame(matrix("", nrow = 200, ncol = length(well_header)),
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
    
    if (is.null(input$tbl_conc_nd)) {
      DF <- import_tables$DF_conc
    } else {
      tryCatch(
        DF <- hot_to_r(input$tbl_conc_nd),
      error = function(e) {
        showModal(modalDialog(
          title = "Crash Detected",
          HTML("You just encountered one of the known bugs in the table editing (rhandsontable). 
               <br> 1. After removing a row, the table crashes when using hot_to_r() because the row name indexing is not working properly.
               <br> 2. Pasting content that has more rows than exist in this table will crash hot_to_r() with the same reason as in point 1.
               "),
          easyClose = FALSE, footer = NULL))
      })
    }
    
    import_tables$DF_conc <- DF  # update import tables
  })
  
  
  
  # For some reason double execution of this observer takes place after hitting 
  #  "Add New Data"
  output$tbl_conc_nd <- rhandsontable::renderRHandsontable({
    
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
                                 stretchH = "all", height = 750, rowHeaders = TRUE) %>% #height = 605, rowHeaders = TRUE) %>%
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
    
    if (is.null(input$tbl_well_nd)) {
      DF <- import_tables$DF_well
    } else {
      tryCatch(
        DF <- hot_to_r(input$tbl_well_nd),
      error = function(e) {
        showModal(modalDialog(
          title = "Crash Detected",
          HTML("You just encountered one of the known bugs in the table editing (rhandsontable). 
               <br> 1. After removing a row, the table crashes when using hot_to_r() because the row name indexing is not working properly.
               <br> 2. Pasting content that has more rows than exist in this table will crash hot_to_r() with the same reason as in point 1.
               "),
          easyClose = FALSE, footer = NULL))
      })
    }
    
    import_tables$DF_well <- DF
  })
  
  
  output$tbl_well_nd <- rhandsontable::renderRHandsontable({

    isolate(DF <- import_tables$DF_well)
    
    renderRHandsonWell()
    
    hot <- rhandsontable::rhandsontable(DF, useTypes = TRUE, 
                                        stretchH = "all", height = 750) 
      #hot_context_menu(allowColEdit = FALSE) %>% # if useTypes = TRUE, allowColEdit will be FALSE anyway
      #hot_col(col = "WellName", type = "dropdown", source = well_choices, strict = TRUE)
    
     
  })
  
  
  
  observeEvent(input$shape_files_nd, {
    
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
      import_tables$Coord_unit <- ret$coord_unit
    }
  })
  
  
  output$tbl_conc_csv <- rhandsontable::renderRHandsontable({
    
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
    updateTabsetPanel(session, "tabbox_csv_import", selected = "Monitoring Data")
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
    import_tables$Coord_unit <- DF$coord_unit
    
    
    # Switch to tabPanel with table.
    updateTabsetPanel(session, "tabbox_csv_import", selected = "Well Coordinates")
  })
  
  
  observeEvent(input$shape_files_csv, {
    
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
  
  
  observeEvent(input$import_button_csv, {
    
    
    ret<-importData(input$dname_csv)
    
    #if(class(ret)=="dialogBox"){
    if(inherits(ret,"dialogBox")){
      
      showModal(modalDialog(
        title="NAPL Value Substitution",
        HTML(ret$msg),
        footer = tagList(
          actionButton("CsvImportNAPLSubsNo", "No"),
          actionButton("CsvImportNAPLSubsYes", "Yes")
        )
      ))
      
    }
    
    
    })
  
  observeEvent(input$CsvImportNAPLSubsNo, { 
    removeModal()
    importData(input$dname_csv, "",subst_napl_vals="no")
  })
  
  observeEvent(input$CsvImportNAPLSubsYes, { 
    removeModal()
    importData(input$dname_csv, "",subst_napl_vals="yes")
  })
  
  
  
  
  
  ## Import Excel data #########################################################
  
  output$tbl_conc_xls <- rhandsontable::renderRHandsontable({
    if (DEBUG_MODE)
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
    
    # Only show first 1000 rows (should be sufficient) in preview.
    # Large data set will take too much time to send the whole table
    # back to the client.
    if (nrow(import_tables$DF_conc) > 1000)
      DF <- import_tables$DF_conc[1:1000,]
    else
      DF <- import_tables$DF_conc
        
    rhandsontable::rhandsontable(DF, useTypes = TRUE, rowHeaders = NULL, 
                                 stretchH = "all", height = tbl_height, 
                                 readOnly = TRUE)  
  })
  
  
  output$tbl_well_xls <- rhandsontable::renderRHandsontable({
    
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
    
    import_tables$shape_files <<- addShapeFiles(input$shape_files_xls, import_tables$shape_files)
    
    # Switch to 'Shape Files' tab inside tabBox.
    updateTabsetPanel(session, "tabbox_xls_import", selected = "Shape Files")
    
    # Reset the file input, so more files can be added
    shinyjs::reset("shape_files_xls")
    shinyjs::show("removeshp_xls")
  })
  
  
  # Read a Excel file and set reactive data.frames 'import_tables' 
  # that contain the Excel data.
  readExcelSheet <- function(filein, sheet) {
    
    dtmp <- readExcel(filein, sheet)
    
    if (is.null(dtmp)) 
      return(FALSE)
    
    import_tables$DF_conc <- dtmp$conc_data
    import_tables$DF_well <- dtmp$well_data
    import_tables$Coord_unit <- dtmp$coord_unit
    
    # Disabled for now, because Shape Files are manually uploaded.
    # import_tables$shape_files <<- dtmp$shape_files
    # Switch to tabPanel with table
    updateTabsetPanel(session, "tabbox_xls_import", selected = "Monitoring Data")
    
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
    if (DEBUG_MODE)
      cat("* in observeEvent: input$okExcelSheet\n")
    
    # Attempt to read the sheet, if it succeeds, remove the modal dialog.      
    if (readExcelSheet(input$excel_import_file, input$excelsheet))
      removeModal()
    
  })
  
  
  observeEvent(input$import_button_xls, {
    
    ret<-importData(input$dname_xls, "excel")
    
    #if(class(ret)=="dialogBox"){
    if(inherits(ret,"dialogBox")){
      
      showModal(modalDialog(
        title="NAPL Value Substitution",
        HTML(ret$msg),
          footer = tagList(
          actionButton("ExcelImportNAPLSubsNo", "No"),
          actionButton("ExcelImportNAPLSubsYes", "Yes")
           )
      ))
      
    }
    
  })
  
  observeEvent(input$ExcelImportNAPLSubsNo, { 
    removeModal()
    importData(input$dname_xls, "excel",subst_napl_vals="no")
  })
  
  observeEvent(input$ExcelImportNAPLSubsYes, { 
    removeModal()
    importData(input$dname_xls, "excel",subst_napl_vals="yes")
  })
  
  
  ## Edit Data #################################################################
  
  # Triggers each time input$tbl_conc_nd (the rhandsontable) changes.
  # Converts the hot table to the data.frame in import_tables.
  observe({
    if (DEBUG_MODE)
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
    if (DEBUG_MODE)
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
    if (DEBUG_MODE)
      cat("* in observe: input$tbl_well_ed\n")
    
    if (is.null(input$tbl_well_ed)) {
      DF <- import_tables$DF_well
    } else {
      DF <- hot_to_r(input$tbl_well_ed)
    }
    
    import_tables$DF_well <- DF
  })
  
  
  output$tbl_well_ed <- rhandsontable::renderRHandsontable({
    if (DEBUG_MODE)
      cat("\n* in tbl_well_ed <- renderRHandsontable()\n")
    
    isolate(DF <- import_tables$DF_well)
    
    renderRHandsonWell()
    
    hot <- rhandsontable::rhandsontable(DF, useTypes = TRUE, stretchH = "all", height = 605) 
  })
  
  
  observeEvent(input$save_button_ed, {
    
    import_tables$Coord_unit <- input$coord_unit_ed

     if (!input$coord_unit_ed %in% coord_units) {
      showNotification("Coordinate unit is not valid. Leave blank or use \'metres\' or \'feet\'.", type = "error", duration = 10)
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
    # Force update to be on the safe side.
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
                                        wells = unique(DF_well$WellName)))) {
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
                     raw_well_tbl = import_tables$DF_well,
                     data_id = createDataID(csite_list)
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
    if (DEBUG_MODE)
      cat("* in observeEvent: reset_ed_data\n")
  
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



    
  # Triggers each time one of the tabs is clicked inside the 'Analyse' panel. 
  observeEvent(input$analyse_panel, {
  
    # If 'Save Session' is selected, update the session file name with the current time stamp.
    if (input$analyse_panel == "Save Session")
      updateTextInput(session, "session_filename", value = paste0("GWSDAT_", gsub(":", "_", gsub(" ", "_", Sys.time())), ".rds"))
    
    #cat('FIXME: Check what this is doing: line 2423.\n')
    #if (input$analyse_panel == "Options") {
      # Save parameters that might have to be restored later if they are invalid.
      #prev_psplines_resolution <<- input$psplines_resolution
      #prev_psplines_knots <<- input$psplines_knots
      
    #}
  })
  
  
  # These inputs will modify the plume threshold for each substance, 
  #  saved in csite$ui_attr$plume_thresh.
  output$thres_plume_select <- renderUI({
 
    dataLoaded() # Need this to re-execute whenever new data is loaded.
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
  
  ##Force evaluation of ui plume thresholds so it can be updated without being activated. 
  ## 
  outputOptions(output, "thres_plume_select", suspendWhenHidden = FALSE)
  
  # These inputs will modify the concentration thresholds for each substance, 
  #  saved in csite$ui_attr$conc_thresh.
  output$thres_conc_select <- renderUI({
    
    dataLoaded()
    
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
  
  outputOptions(output, "thres_conc_select", suspendWhenHidden = FALSE)
  
  changeModelSettingorNotModal <- function() {
    modalDialog(
      span('The settings for the model resolution has changed and the model requires refitting. This process can take some time and will be done in the background.'),
      div(style = "margin-top: 25px;", 
          'You will be notified about the progress and the model settings will be updated as soon as the calculation is completed.'),
      div(style = "margin-top: 25px;", 
          'Do you like to continue?'),
  
      
      footer = tagList(
        actionButton("cancelModSetting", "Cancel"),
        actionButton("okModSetting", "Proceed")
      )
    )
  }
  
  observeEvent(input$cancelModSetting, {
    cat('* in observeEvent: input$cancelModSetting\n')
    # Revert input to previous resolution setting
    updateSelectInput(session, "psplines_resolution", selected = prev_psplines_resolution)
    updateTextInput(session, "psplines_knots", value = prev_psplines_knots)
    
    removeModal()
  })
    
  
  
  
  
  
  
  # Re-fit the model with the new model resolution setting, i.e. number of knots.
  observeEvent(input$okModSetting, {
    
    cat("* in observeEvent: okModSetting\n")
    
    removeModal()
    
    if (new_psplines_nseg == csite$GWSDAT_Options$PSplineVars$nseg) 
      return()
    
   
    if (BP_method == 'simple') {
      
      # Create temporary file names
      BP_modelfit_outfile <<- tempfile(pattern = "LC_", tmpdir = tempdir(), fileext = ".rds")
      BP_modelfit_infile <- tempfile(pattern = "LC_", tmpdir = tempdir(), fileext = ".rds")
      
      # Save data object to file 
      csite$SavedlibPaths<-.libPaths()
      saveRDS(csite, file = BP_modelfit_infile)
      
      # Starts script as a background process.
      run_script <- system.file("application", "simple_pspline_fit.R", package = "GWSDAT")
      Rcmd <- paste0('Rscript ',"\"",run_script,"\"", ' ', new_psplines_nseg, ' ', csite$data_id, 
                     ' ', "\"",BP_modelfit_infile,"\"", ' ', "\"",BP_modelfit_outfile,"\"")
      cat("Starting R process: ", Rcmd, "\n")
      
      system(Rcmd, wait = FALSE, invisible = TRUE)
      
      # This will cause the observer to execute which checks if results are ready.
      BP_modelfit_running(TRUE)
      
    }
    
    if (BP_method == 'queue') {

      # Set new number of knots for the P-Spline model.
      tmp_opt <- csite$GWSDAT_Options
      tmp_opt$PSplineVars$nseg <- new_psplines_nseg
      tmp_opt$SavedlibPaths <- .libPaths()
      # Add job to queue.
      # Uses system.file() to retrieve full path of target script. 
      # Note: This script loads GWSDAT itself, so it can't be located inside the R folder.
      addQueueJob(jq_db, 'jqdb_pspline_fit.R', info = paste0('Fit P-Splines with ', new_psplines_nseg, ' segments.'),
                  data_name = csite$ui_attr$site_name, data_id = csite$data_id, pdata = csite, params = tmp_opt) 
      
    }
    
    showNotification("Started background process for P-Spline fit. This can take a view moments.", type = "message", duration = 10)
    
  })
  
  
  
  # Update the number of knots in the text field to reflect the resolution.
  observeEvent(input$psplines_resolution, {
    
    nknots <- 6
    
    if (input$psplines_resolution == "Default")
      nknots <- 6
    if (input$psplines_resolution == "High")
      nknots <- 8
    
    updateTextInput(session, "psplines_knots", value = nknots)
  })
  
  
  observeEvent(input$save_analyse_options, {
   

    new_psplines_nseg <<- as.numeric(input$psplines_knots)

    # Check if the value changed, if so, refit all data.
    if ( new_psplines_nseg != csite$GWSDAT_Options$PSplineVars$nseg) {
      
      # Check if value is in boundaries.
      if (new_psplines_nseg < 2 || new_psplines_nseg > 12) {
        showNotification("Number of segments for the model is out of bounds. Minimum: 2, Maximum: 12.", type = "error", duration = 10)
      } else {
        # Ask if to change it. The actual fit is calculated when the actionButton
        # is pressed inside the modal dialog.
        showModal(changeModelSettingorNotModal())
      }
      
      
      # Change the value back to the original one. Only update it when re-fitting
      # is completed which is done in the background.
      updateTextInput(session, "psplines_knots", value = csite$GWSDAT_Options$PSplineVars$nseg)
      
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
      
  # output$ColourKeyRHandsontable <- renderRHandsontable({
  #   #rhandsontable(data.frame(lev_cut=csite$ui_attr$lev_cut[-length(csite$ui_attr$lev_cut)]),rowHeaders = NULL,digits=0)
  #   rhandsontable(as.data.frame(csite$ui_attr$lev_cut_by_solute),rowHeaders = NULL,digits=0)
  # })
  
  output$ColourKeyRHandsontable <- rhandsontable::renderRHandsontable({
    
  if(is.null(csite$ui_attr$lev_cut_by_solute)){
    
    rhandsontable::rhandsontable(as.data.frame(create_lev_cut_by_solute(csite$ui_attr$lev_cut,csite$ui_attr$solute_names),check.names=F),rowHeaders = NULL,digits=0)
    
  }else{
    
    rhandsontable::rhandsontable(as.data.frame(csite$ui_attr$lev_cut_by_solute,check.names=F),rowHeaders = NULL,digits=0)
    
  }
  })
  
  output$options_saved_Colour_Key <- renderText({paste("Changes Saved") })
  
  observeEvent(input$save_Colour_Key, {
  
  ## Turn off Scale colours to data in Spatial plot to honour newly defined colour key.
  updateCheckboxGroupInput(session, "imageplot_options",selected=setdiff(input$imageplot_options,"Scale colours to Data"))
                             
  shinyjs::show(id = "options_save_msg_Colour_Key", anim = TRUE, animType = "fade")
  shinyjs::delay(2000, shinyjs::hide(id = "options_save_msg_Colour_Key", anim = TRUE, animType = "fade"))
  
  #csite$ui_attr$lev_cut<<-c(sort(hot_to_r(input$ColourKeyRHandsontable)$lev_cut),50000)
  csite$ui_attr$lev_cut_by_solute<<-as.list(hot_to_r(input$ColourKeyRHandsontable))
  
  })
  
  
  shinyjs::onclick("GoToDataSelect", {
    shinyjs::hide("analyse_page")
    shinyjs::show("data_select_page")
  })
  
  observeEvent(input$sidebar_menu, {
    
    # If the 'Analyse' side menu is clicked, always show
    # the data select landing page (disabled because it is counter
    # intuitive when a data set was previously selected).
    #if (input$sidebar_menu == "menu_analyse") {
    #  shinyjs::hide("analyse_page")
    #  shinyjs::show("data_select_page")
    #}
    
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
    
    if (DEBUG_MODE)
      cat("* in loadDefaultSessions()\n")
    
    infile <- system.file("extdata", default_session_file, package = "GWSDAT")

    if (exists('csite_list_tmp'))    
      rm('csite_list_tmp')
    
    # This should never trigger a warning, since I am putting the file there (only if package is broken).
    tryCatch( csite_list_tmp <- readRDS(infile),
              warning = function(w) showNotification(paste0("Failed to load default_session_file \'", default_session_file, "\' from package GWSDAT."), type = "error", duration = 7))

    if (!exists('csite_list_tmp'))
      return(NULL)
    
    csite_list <<- csite_list_tmp
    csite <<- csite_list[[1]]
    csite_selected_idx <<- 1
    
    dataLoaded(LOAD_COMPLETE)
    
  }
  
  
  #
  # Would like to move this fct to another file, however,
  #   it uses the reactive variabled dataLoaded. How to fix this?
  #
  loadDataSet <- function() {
  
    print("In load Data set")
    if (DEBUG_MODE) cat("* in loadDataSet()\n")
   
    # Load 'session_file' if specified in launchApp().
    #if (exists("session_file", envir = .GlobalEnv)) {
    if (!is.null(session_file)) {
    
      tryCatch( csite_list_tmp <- readRDS(session_file), warning = function(w) 
        showModal(modalDialog(title = "Error", w$message, easyClose = FALSE))
      )
      
      if (!exists('csite_list_tmp'))
        return(FALSE)
      
      csite_list <<- csite_list_tmp
      csite <<- csite_list[[1]]
      csite_selected_idx <<- 1
      
      dataLoaded(LOAD_COMPLETE)
      return(TRUE)  
    }
    
     if (!exists("GWSDAT_Options", envir = .GlobalEnv)){ 
      GWSDAT_Options <-  createOptions()
    }
    
    
    
    Aq_sel <- loadOptions$aquifer
    subst_napl <- loadOptions$subst_napl
    
    solute_data <- well_data <- NULL
    
    # Well data and coordinates inputted as R data frames. 
    if(!is.null(GWSDAT_Options[["WellData"]]) || !is.null(GWSDAT_Options[["WellCoords"]])){
      
      tryCatch({
      solute_data<-GWSDAT_Options[["WellData"]]
      well_data<-GWSDAT_Options[["WellCoords"]]  ### output well coordinate data is a list consisting of fields data and coord_unit
      well_data<-list(data=GWSDAT_Options[["WellCoords"]][,c("WellName","XCoord","YCoord","Aquifer")],coord_unit=GWSDAT_Options[["WellCoords"]]["CoordUnits"][1])
      }, error = function(w){showModal(modalDialog(title = "Error Inputting WellData and/or WellCoords.", w$message, easyClose = FALSE)); Sys.sleep(5)})

    }
    
    
    #  Read Well data and coordinates from Excel GWSDAT input Template
    if(!is.null(GWSDAT_Options[["ExcelDataFilename"]])){
      
      #ret<-readExcel(opt$ExcelDataFilename,sheet="GWSDAT Basic Example")
      ret<-readExcel(GWSDAT_Options$ExcelDataFilename)
      solute_data<-ret$conc_data
      well_data<-list(data=ret$well_data[,c("WellName","XCoord","YCoord","Aquifer")],coord_unit=ret$coord_unit[1])
      
    }
    
    
    #  # Read Well data and coordinates from csv files.
    if(is.null(solute_data)){ # if it hasnt found any data yet.
    
      tryCatch({
        solute_data <- readConcData(GWSDAT_Options$WellDataFilename, conc_header)
        well_data <- readWellCoords(GWSDAT_Options$WellCoordsFilename, well_header)
      }, error = function(w){showModal(modalDialog(title = "Error reading csv files", w$message, easyClose = FALSE)); Sys.sleep(5)})
    
    }
    
    # Check if reading the data failed. 
    if (is.null(solute_data) || is.null(well_data))
      return(NULL)
    
    all_data <- formatData(solute_data, well_data)
    
    # Extract list of Aquifer. If there is more than one, return the list.
    Aq_list <- unique(all_data$sample_loc$data$Aquifer)
    
    if ((length(Aq_list) > 1) && is.null(Aq_sel)) {
      class(Aq_list) <- "Aq_list"
      return(Aq_list)
    }
    
    # If no Aquifer was specified in loadOptions, pick the first one in the list. 
    if (is.null(Aq_sel)) 
      Aq_sel <- Aq_list[[1]]

   
    pr_dat <- processData(all_data$solute_data, all_data$sample_loc, GWSDAT_Options, 
                          Aq_sel, subst_napl_vals = subst_napl)
    
    #if (class(pr_dat) == "dialogBox")
    if (inherits(pr_dat, "dialogBox"))
      return(pr_dat)
      
    
    # Check if something went wrong while processing the data.
    if (is.null(pr_dat))
      return(NULL)
    
    # Fit the data and calculate the Traffic Lights (depends on fitting the data).
    fitdat <- fitData(pr_dat, GWSDAT_Options)
    
    if (is.null(fitdat))
      return(NULL)
    
    
    # Calculate the Groundwater flows.
    GW_flows <- evalGWFlow(pr_dat$Agg_GW_Data)
    
    # Create UI attributes.
    ui_attr <- createUIAttr(pr_dat, GWSDAT_Options)

    # Build list with all data.
    csite <<- list(All.Data      = pr_dat,
                   GWSDAT_Options = GWSDAT_Options,
                   Fitted.Data    = fitdat$Fitted.Data,
                   Traffic.Lights = fitdat$Traffic.Lights,
                   GW.Flows       = GW_flows,
                   ui_attr        = ui_attr,
		               Aquifer        = Aq_sel,
		               raw_contaminant_tbl = solute_data,
		               raw_well_tbl   = well_data$data,
		               data_id        = createDataID())
    
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
    
    # If nothing was loaded yet, attempt to do so.
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

      # Store generated control IDs for Select and Aquifer Select Input
      sel_ids <- c()
      aq_ids  <- c()
      
      # Create a shinydashboard box for each data set. Include a choice for the 
      # Aquifer and the select button. 
      for (i in 1:length(data_sets)) {
        
        set_name <- names(data_sets)[i]
        
        # Create unique button name with random ID.
        for (i in 1:1000) {
          
          tmpid <- sample(1:100000, 1)
          
          sel_id <- paste0("analyse_btn_", tmpid)
          aq_id  <- paste0("aquifer_select_", tmpid)  
          
          if (!sel_id %in% sel_ids)
            break
        }
        
        sel_ids <- c(sel_ids, sel_id)
        aq_ids  <- c(aq_ids, aq_id)
        
        html_out <- tagList(html_out, fluidRow(
          shinydashboard::box(width = 7, status = "primary", collapsible = TRUE,
                              title = set_name, 
                              
                              div(style = "display: inline-block", 
                                  selectInput(aq_id, label = "Select Aquifer",
                                              choices  = data_sets[[set_name]]$Aquifer,
                                              selected = data_sets[[set_name]]$Aquifer[1], 
                                              width = '150px')
                              ),
                              div(style = "display: inline-block; float : right", 
                                  actionButton(sel_id, "Select")
                              )
          )))
      } # end for loop
      
      # Create temporary list that will be used to create the observer
      databoxes <- as.list(1:length(data_sets))
      
      databoxes <- lapply(databoxes, function(i) {
        
        sel_id <- sel_ids[i]
        aq_id  <- aq_ids[i]
        
        # Store observer function in list of buttons.
        obsAnalyseBtnList[[sel_id]] <<- observeEvent(input[[sel_id]], {
              
          # Retrieve the aquifer select input value.
          #aquifer <- eval(parse(text = paste0("input$", aq_id)))
          aquifer <- input[[aq_id]] 
             
          # Get list index of selected data and aquifer.
          j <- data_sets[[i]]$csite_idx[which(data_sets[[i]]$Aquifer == aquifer)]
              
          # If it was not fitted before, do it now.
          if (is.null(csite_list[[j]]$Fitted.Data)) {
               
            fitdat <- fitData(csite_list[[j]]$All.Data, csite_list[[j]]$GWSDAT_Options)
                
            if (is.null(fitdat)) showNotification("Fitting data failed. Aborting.", type = "error", duration = 10)
            
            csite_list[[j]]$Fitted.Data    <<- fitdat$Fitted.Data
            csite_list[[j]]$Traffic.Lights <<- fitdat$Traffic.Lights
            csite_list[[j]]$GW.Flows       <<- evalGWFlow(csite_list[[j]]$All.Data$Agg_GW_Data)
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
      }) # end of lapply
    }  
     
    return(html_out)
    
  })

  
  
  
  
  
  
  # Go to .CSV Data Import (Button click).
  observeEvent(input$add_csv_data,  {
    
    shinyjs::hide("uiDataManager")
    shinyjs::show("uiDataAddCSV")
    
    import_tables$DF_conc <<- NULL
    import_tables$DF_well <<- NULL
    import_tables$shape_files <<- NULL
    
    output$uiDataAddCSV <- renderUI(uiImportCSVData(getValidDataName(csite_list)))
  })
  

  observeEvent(input$reset_csv_import,  {
    if (DEBUG_MODE)
      cat("* in observeEvent: reset_csv_import\n")

    import_tables$DF_conc <<- NULL
    import_tables$DF_well <<- NULL
    import_tables$shape_files <<- NULL
    
    output$uiDataAddCSV <- renderUI(uiImportCSVData(getValidDataName(csite_list)))
  })
  
  
  
  # Go to Excel Data Import (Button click).
  observeEvent(input$add_excel_data,  {
    if (DEBUG_MODE)
      cat("* in observeEvent: add_excel_data\n")

    shinyjs::hide(id = "uiDataManager")    
    shinyjs::show(id = "uiDataAddExcel")

    import_tables$DF_conc <<- NULL
    import_tables$DF_well <<- NULL
    
    output$uiDataAddExcel <- renderUI(uiImportExcelData(csite_list)) 
  })
  
  observeEvent(input$reset_xls_import, {
    if (DEBUG_MODE)
      cat("* in observeEvent: reset_xls_import\n")
    
    import_tables$DF_well <<- NULL
    import_tables$DF_conc <<- NULL
    import_tables$shape_files <<- NULL
    
    output$uiDataAddExcel <- renderUI(uiImportExcelData(csite_list))                             
  })
  
  
  # These are the observer lists that will hold the button click actions for 
  # the Delete and Edit button.
  obsDelBtnList <- list()
  obsEditBtnList <- list()
  
  
  createDelBtnObserver <- function(btns) {
    #cat("* creating Delete Buttons.\n")
    
    # Check if a Delete button was created.
    if (length(btns) > 0) {
      if (DEBUG_MODE)
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
      if (DEBUG_MODE)
        cat(" + creating edit button observers\n")
      
      databoxes <- as.list(1:length(btns))
      databoxes <- lapply(databoxes, function(i) {
        
        btn_name <- btns[[i]]$btn_name
        csite_name <- btns[[i]]$csite_name
        
        #  Creates an observer only if it doesn't already exists.
        if (is.null(obsEditBtnList[[btn_name]])) {
          
          # Store observer function in list of buttons.
          obsDelBtnList[[btn_name]] <<- observeEvent(input[[btn_name]], {
            if (DEBUG_MODE)
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
    if (DEBUG_MODE)
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
    if (DEBUG_MODE)
      cat("* in rndAnalyse <- renderUI()\n")
  
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
      
    
    #if (class(ret) == "Aq_list" ) {
    if (inherits(ret,"Aq_list")) {
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
    
    #if (class(ret) == "dialogBox" ) {
    if (inherits(ret,"dialogBox" )) {
      return(div(style = "width: 80%; margin: 0 auto",
                      shinydashboard::box(
                        div(style = "margin-top: 10px; margin-bottom: 25px",
                          HTML(paste0(ret$msg))),
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
    
    
    # Completely loaded, display the Analyse UI.
    if (data_load_status >= LOAD_COMPLETE) {
      return(uiAnalyse(csite, img_frmt, APP_RUN_MODE))
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
  
  
  # Output the version and log info.
  output$logs_view <- renderPrint({cat(app_log()) })
  
  # Maybe not use renderUI but standard client side ui????
  output$uiLogsJobs <- renderUI({
    uiLogsJobs()
  })
  
  output$job_queue_table <- renderTable({if (DEBUG_MODE) cat('* job_queue_table()\n'); job_queue$new })
  output$job_run_table   <- renderTable({if (DEBUG_MODE) cat('* job_run_table()\n'); job_queue$run })
  output$job_done_table  <- renderTable({if (DEBUG_MODE) cat('* job_done_table()\n'); job_queue$done })
  
  ## Dashboard Menu ############################################################
  
  output$welcomeMsg <- shinydashboard::renderMenu({
    
                                        # If a user is logged in, greet him with his email.
    if (APP_LOGIN_MODE) {
        if (user_id$authenticated) {
            tags$li(class = "dropdown",
              tags$div(style = 'margin-top: 15px; margin-right: 10px;', 
                          h4(paste0("Welcome ", user_id$email))))
        } else {
            tags$li(class = "dropdown",
                    tags$div(style = 'margin-top: 15px; margin-right: 10px;', 
                             h4("This is a temporary session.")))
        }
    } else {
        list()
    }
  })
  
  output$logAction <- shinydashboard::renderMenu({
    
    if (APP_LOGIN_MODE) {
                                        # If a user is not logged in, show the 'LOG IN' link.
        if (!user_id$authenticated) {
            tags$li(class = "dropdown",
                    tags$div(style = 'margin-top: 15px; margin-right: 10px;', 
                             tags$a(id = "gotoLogin", h4("LOG IN"), href = "#"))
                    )
        } else {
                                        # .. otherwise show the 'LOG OUT' link.
            tags$li(class = "dropdown",
                    tags$div(style = 'margin-top: 15px; margin-right: 10px;',
                             tags$a(id = "doLogout", h4("LOG OUT"), href = "#"))
                    )
      
        }
    } else {
        list()
    }  
  })
  
  output$signupAction <- shinydashboard::renderMenu({
   
      if (APP_LOGIN_MODE) {
          # If a user is not logged in, show the 'SIGN UP' link.
          if (!user_id$authenticated) {
              tags$li(class = "dropdown",
                      tags$div(style = 'margin-top: 15px; margin-right: 10px;', 
                               tags$a(id = "gotoSignup", h4("SIGN UP"), href = "#"))
                      )
          } else {
              # Use this as a placeholder, will keep the space empty in the state of 
              # being logged in.
              tags$li(class = "dropdown",
                      tags$div(style = 'margin-top: 15px; margin-right: 10px;')
                      )
          }
      } else {
          list()
      }
  })
  
  

  
} # end server section

