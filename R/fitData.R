
fitData <- function(All.Data, params, model = 'pspline', showProgress = TRUE) {

  if (showProgress) {
    progress <- shiny::Progress$new()
    progress$set(message = "Fitting data", value = 0)
    on.exit(progress$close())
  }
  
  Fitted.Data <- list()

  NumConts = length(All.Data$cont_names)

  # Progress bar value indicator.
  PctDone = 1 / (NumConts + 2)
  
  
  # Fit each of the contaminants stored in 'ContNames'.
  for (i in 1:NumConts) {
    
    # Show progress of fitting.
    if (showProgress) { # if (!is.null(progress)) {
      progress$set(value = PctDone, detail = paste("contaminant ", All.Data$cont_names[i]))
    }
    
    # 'model' has to be 'svm' for this .. 
    # fitSVM() # <-- This is not working.
    # temp.fit <- try(fitSVM(All.Data, All.Data$cont_names[i], GWSDAT_Options))
  
    ContData <- All.Data$Cont.Data[as.character(All.Data$Cont.Data$Constituent) == All.Data$cont_names[i],]
    ContData <- na.omit(ContData)
    
    temp.fit <- fitPSplines(ContData, params$PSplineVars)
    
    
    if (inherits(temp.fit, 'try-error')) {
    
      msg <- paste("Fitting", All.Data$cont_names[i], "data failed, ignoring it.")
      showNotification(msg, type = "error", duration = 20)
      
    } else {
      Fitted.Data[[All.Data$cont_names[i]]] <- temp.fit
    }
    
    # Progress the bar.
    PctDone = (1 + i) / (NumConts + 2)
    
  }
  
  # Abort if none of the substances was fitted.  
  if (length(Fitted.Data) == 0) {
    msg <- "None of the contaminants were fitted. Aborting calculation."
    showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
    return(NULL)
  }

  
  
  ################### Traffic Lights ###########################################
  
  if (showProgress) {
    progress$set(value = PctDone, detail = paste("calculating trends"))
  }

  Traffic.Lights <- NULL
  
  tryCatch(
    Traffic.Lights <- calcTrafficLights(All.Data, Fitted.Data, params$smThreshSe, params$smMethod),
    
    error = function(e) {
      showNotification(paste0("Failed to calculate trend table: ", e$message), type = "error", duration = 10)
    }
  )

  return(list(Fitted.Data = Fitted.Data, Traffic.Lights = Traffic.Lights))

}


