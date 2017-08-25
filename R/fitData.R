

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
    
    if (tolower(GWSDAT_Options$ModelMethod) == "svm") {
      
      temp.fit <- try(fitSVM(All.Data, All.Data$cont_names[i], GWSDAT_Options))
      
    } else {
      
      temp.fit <- fitPSpline(All.Data,All.Data$cont_names[i],GWSDAT_Options)
      
    }
    
    
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
  
  Traffic.Lights <- try(calcTrafficLights(All.Data, Fitted.Data, GWSDAT_Options))
  
  attr(Fitted.Data,'TrafficLights') <- if (!inherits(Traffic.Lights, 'try-error')) {
    Traffic.Lights
  } else {NULL}
  
  PctDone = (NumConts + 2) / (NumConts + 3)

 
  
  ################### Groundwater Calc##########################################
  
  
  if (!is.null(progress)) {
    progress$set(value = PctDone, detail = paste("calculating groundwater"))
  } 
  
  if (!is.null(All.Data$Agg_GW_Data)) {
    
    GW.Flows <- try(do.call('rbind', by(All.Data$Agg_GW_Data, All.Data$Agg_GW_Data$AggDate, calcGWFlow)))
    
    if (!inherits(GW.Flows, 'try-error')) {
      
      GW.Flows$R <- GW.Flows$R/quantile(GW.Flows$R, p = 0.9, na.rm = T)
      GW.Flows$R[GW.Flows$R > 1] <- 1
      
    } 
    
    GW.Flows = na.omit(GW.Flows)
    attr(Fitted.Data,'GWFlows') <- if (!inherits(GW.Flows, 'try-error')) {GW.Flows} else {NULL}
    
  }
  
  
  class(Fitted.Data) = "gwsdat_fit"
  
  return(Fitted.Data)

}



fitSVM <- function(All.Data, Cont.Name, GWSDAT_Options) {
  
  
  #rearranging names to cope with actual and aggregate dates
  Time.Eval <- sort(All.Data$All.Agg.Dates)
  
  temp.Cont.Data <- All.Data$Cont.Data[as.character(All.Data$Cont.Data$Constituent)==Cont.Name,]
  temp.Cont.Data <- na.omit(temp.Cont.Data)
  names(temp.Cont.Data)[names(temp.Cont.Data) == "AggDate"] <- "AggDatekeep"
  names(temp.Cont.Data)[names(temp.Cont.Data) == "SampleDate"] <- "AggDate"
  
  
  
  gamma <- GWSDAT_Options[["gamma"]]
  cost <- GWSDAT_Options[["cost"]]
  
  gamma <- try(selectGamma(temp.Cont.Data, gamma))
  
  if (inherits(gamma, "try-error")) { gamma = NA }#else{gamma=1/gamma}#Wayne
  
  
  svm.temp <- try(tune.svm(log(Result.Corr.ND) ~ AggDate + XCoord + YCoord,
                           data  = temp.Cont.Data, 
                           gamma = gamma, 
                           cost  = cost,
                           scale = TRUE,
                           tunecontrol = tune.control(cross = min(GWSDAT_Options[["cross"]], nrow(temp.Cont.Data)))))
  
  
  if (!inherits(svm.temp, "try-error")) {
    
    temp.Cont.Data$ModelPred <- exp(predict(svm.temp$best.model,newdata = temp.Cont.Data))
    
  } else {
    
    temp.Cont.Data$ModelPred <- rep(NA,nrow(temp.Cont.Data))
    
  }
  
  names(temp.Cont.Data)[names(temp.Cont.Data) == "AggDate"] <- "SampleDate"
  names(temp.Cont.Data)[names(temp.Cont.Data) == "AggDatekeep"] <- "AggDate"
  
  temp.Cont.Data$Result.Corr.ND[!is.finite(temp.Cont.Data$Result.Corr.ND)] <- NA #Wayne V3 coerce -inf to NA for NAPL only data sets. 
  
  list(Cont.Data = temp.Cont.Data, Model.tune = svm.temp, Time.Eval = Time.Eval)
  
}


selectGamma <- function(temp.Cont.Data,gamma){
  
  if (gamma[1] != 0) { return(gamma) }
  
  
  tempgamma <- matrix(nrow=50,ncol=2)
  
  for (i in 1:nrow(tempgamma)) {
    
    tempgamma[i,] <- GWSDAT.sigest(log(Result.Corr.ND)~AggDate+XCoord+YCoord,temp.Cont.Data)
    
  }
  
  if (length(gamma) == 1) {
    
    gamma <- mean(0.5*(tempgamma[,1]+tempgamma[,2]))
    #gamma<-median(1/apply(tempgamma,1,mean)) #Wayne 26th June 2009
    
  }else{
    
    #gamma<-quantile(apply(tempgamma,1,mean),p=c(0.1,0.5,0.9))
    #gamma<-c(mean(0.5*(tempgamma[,1]+tempgamma[,2])), quantile(tempgamma[,2],p=0.9))
    #gamma<-c(quantile(tempgamma[,2],p=0.95))
    #gamma<-sort(apply(tempgamma,2,mean))[1]+c(.3,.5,.7)*diff(sort(apply(tempgamma,2,mean)))
    #gamma<-quantile(1/apply(tempgamma,1,mean),p=c(.1,.5,.9)) #Wayne 26th June 2009
    gamma < -sort(apply(tempgamma,2,mean))[1]+c(.3,.5,.7)*diff(sort(apply(tempgamma,2,mean)))
    
    
    
    
  }
  
  return(gamma)
}



