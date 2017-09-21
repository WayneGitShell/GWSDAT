 

fitData <- function(All.Data, GWSDAT_Options) {

  progress <- shiny::Progress$new()
  progress$set(message = "Fitting data", value = 0)
  on.exit(progress$close())

  Fitted.Data <- list()

  NumConts = length(All.Data$cont_names)

  # Progress bar value indicator.
  PctDone = 1 / (NumConts + 3)
  
 
  if (!tolower(GWSDAT_Options$ModelMethod) %in% c("svm","pspline")) {
    
    msg <- "No valid modelling method selected. Assuming pspline. (Choice will be added)"
    showNotification(msg, duration = 10 )
    
    GWSDAT_Options$ModelMethod <- "pspline"
  } 
  
  
  #
  # Fit each of the contaminants stored in 'ContNames'.
  #
  for (i in 1:NumConts) {
    
    # Show progress of fitting.
    if (!is.null(progress)) {
      progress$set(value = PctDone, detail = paste("contaminant ", All.Data$cont_names[i]))
    }
    
    # fitSVM() not working..
    #if (tolower(GWSDAT_Options$ModelMethod) == "svm") {
    #  
    #  temp.fit <- try(fitSVM(All.Data, All.Data$cont_names[i], GWSDAT_Options))
    #  
    #} else {
    ContData <- All.Data$Cont.Data[as.character(All.Data$Cont.Data$Constituent) == All.Data$cont_names[i],]
    ContData <- na.omit(ContData)
    
    temp.fit <- fitPSpline(ContData, GWSDAT_Options)
    
    # temp.fit_new <- fitPSpline_new(ContData, GWSDAT_Options)
       
    #}
    
    
    if (inherits(temp.fit, 'try-error')) {
    
      msg <- paste("Fitting", All.Data$cont_names[i], "data failed, ignoring it.")
      showNotification(msg, type = "error", duration = 20)
    } else {
      
      Fitted.Data[[All.Data$cont_names[i]]] <- temp.fit
    }
    
    # Progress the bar.
    PctDone = (1 + i) / (NumConts + 3)
    
  }
  
  # Abort if none of the substances was fitted.  
  if (length(Fitted.Data) == 0) {
    
    msg <- "None of the contaminants were fitted. Aborting calculation."
    showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
    return(NULL)
  }

  
  
  ################### Traffic Lights ###########################################
  
  
  if (!is.null(progress)) {
    progress$set(value = PctDone, detail = paste("calculating trends"))
  }
  
  Traffic.Lights <- NULL
  
  tryCatch(
    Traffic.Lights <- calcTrafficLights(All.Data, Fitted.Data, GWSDAT_Options),
    error = function(e) {
      showNotification(paste0("Failed to calculate trend table: ", e$message), type = "error", duration = 10)
    }
  )

  
  PctDone = (NumConts + 2) / (NumConts + 3)

 
  
  ################### Groundwater Calc##########################################
  
  
  if (!is.null(progress)) {
    progress$set(value = PctDone, detail = paste("calculating groundwater"))
  } 
  
  GW.Flows <- NULL
  
  if (!is.null(All.Data$Agg_GW_Data)) {
    
    tryCatch(
      GW.Flows <- do.call('rbind', by(All.Data$Agg_GW_Data, All.Data$Agg_GW_Data$AggDate, calcGWFlow)),
      error = function(e) {
        showNotification(paste0("Failed to calculate groundwater flows: ", e$message), type = "error", duration = 10)
      })
    
    if (!is.null(GW.Flows)) {    
      GW.Flows$R <- GW.Flows$R/quantile(GW.Flows$R, p = 0.9, na.rm = T)
      GW.Flows$R[GW.Flows$R > 1] <- 1
      GW.Flows <- na.omit(GW.Flows)    
    }
    
  }
 
  return(list(Fitted.Data = Fitted.Data, Traffic.Lights = Traffic.Lights, GW.Flows = GW.Flows))

}


