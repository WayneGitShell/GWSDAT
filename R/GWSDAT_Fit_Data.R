

GWSDAT_Fit_Data <- function(All.Data, GWSDAT_Options, progressBar) {

  
  ############################# Fit Model to each Contaminant ############################################################

  Fitted.Data <- list()

  NumConts = length(All.Data$All.Conts)

  # For the progress bar.
  PctDone = 1 / (NumConts + 3)
  
  
  if (!tolower(GWSDAT_Options$ModelMethod) %in% c("svm","pspline")){
    
    GWSDAT_Options$ModelMethod <- GWSDAT.select.list(c("pspline","svm"))
    
  }
  
  
  #
  # Fit each of the contaminants stored in 'ContNames'.
  #
  for (i in 1:NumConts) {
    
    # Show progress of fitting.
    setTkProgressBar(progressBar, PctDone, NULL, paste("Fitting ", All.Data$All.Conts[i], " Data."))
  
    
    if (tolower(GWSDAT_Options$ModelMethod) == "svm") {
      
      temp.fit <- try(GWSDAT.svmfit(All.Data,All.Data$All.Conts[i],GWSDAT_Options))
      
    } else {
      
      temp.fit <- GWSDAT.PSplinefit(All.Data,All.Data$All.Conts[i],GWSDAT_Options)
      
    }
    
    
    
    if (inherits(temp.fit, 'try-error')) {
      
      #TK stuff: 
      #tkmessageBox(title="Error!",message=paste("Error in fitting",All.Data$All.Conts[i],"data."),icon="error",type="ok")
      
      Run_status = GWSDAT_Warning(paste("Error in fitting",All.Data$All.Conts[i],"data."))
      
    } else {
      
      Fitted.Data[[All.Data$All.Conts[i]]]<-temp.fit
      
    }
    
    # Progress the bar.
    PctDone = (1 + i) / (NumConts + 3)
    
  }
  
  
  if (exists('temp.fit')) { try(rm(temp.fit)) }
  
  
  if (length(Fitted.Data) == 0) {
    
    tkmessageBox(title="Error!",message=paste("Error in fitting data."),icon="error",type="ok")
    try(close(progressBar))
    #stop("Error in fitting data.")
    
    return(GWSDAT_Error("Error in fitting data: None of the contaminants was fitted."))
  }
  #----------------------------------------------------------------------------------------------------------------------#
  
  
  
  ################### Traffic Lights #####################################################################################
  
  setTkProgressBar(progressBar, PctDone, NULL, "Calculating Traffic Lights")
  
  Traffic.Lights <- try(GWSDAT.Traffic.Lights(All.Data,Fitted.Data,GWSDAT_Options))
  attr(Fitted.Data,'TrafficLights') <- if (!inherits(Traffic.Lights, 'try-error')){Traffic.Lights}else{NULL}
  try(rm(Traffic.Lights))
  
  PctDone = (NumConts + 2) / (NumConts + 3)
  setTkProgressBar(progressBar, PctDone)
  #----------------------------------------------------------------------------------------------------------------------#
  
  
  
  ################### Groundwater Calc####################################################################################
  
  #TK stuff:
  setTkProgressBar(progressBar, PctDone, NULL, "Calculating GroundWater Flows")
  
  
  if (!is.null(All.Data$Agg_GW_Data)) {
    
    GW.Flows <- try(do.call('rbind',by(All.Data$Agg_GW_Data,All.Data$Agg_GW_Data$AggDate,GWSDAT.GW.Comp)))
    
    if (!inherits(GW.Flows, 'try-error')) {
      
      GW.Flows$R<-GW.Flows$R/quantile(GW.Flows$R,p=0.9,na.rm=T)
      GW.Flows$R[GW.Flows$R>1]<-1
      
    } 
    GW.Flows=na.omit(GW.Flows)
    attr(Fitted.Data,'GWFlows') <- if (!inherits(GW.Flows, 'try-error')){GW.Flows}else{NULL}
    try(rm(GW.Flows))
    
  }
  
  
  class(Fitted.Data) = "gwsdat_fit"
  
  return(Fitted.Data)

}