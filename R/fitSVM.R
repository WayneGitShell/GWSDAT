


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



