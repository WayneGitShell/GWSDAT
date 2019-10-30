

plotSTPredictions <- function(csite, substance = NULL, Wells.to.Plot = NULL,  
                              UseLogScale = FALSE, solute_unit = "ug/l") {
  
 
  # Maybe show message that nothing is selected
  if (is.null(substance) || is.null(Wells.to.Plot))
    return(NULL)
  
  if (length(Wells.to.Plot) == 0) 
    return(NULL)
  
  
  Cont.Data <- csite$All.Data$Cont.Data
  
  # No model prediction possible without well coords!
  Cont.Data <- Cont.Data[!is.na(Cont.Data$XCoord),] 
  Cont.Data <- Cont.Data[!is.na(Cont.Data$YCoord),]

  SiteName <- csite$GWSDAT_Options$SiteName
  
  # Extract selected contaminants and wells. 
  Cont.Data <- Cont.Data[as.character(Cont.Data$Constituent) %in% substance,]
  Cont.Data$Constituent <- factor(as.character(Cont.Data$Constituent))
  
  Cont.Data <- Cont.Data[as.character(Cont.Data$WellName) %in% Wells.to.Plot,]
  
  # Check if there is data left to plot.
  if (nrow(Cont.Data) == 0) { 
    showNotification("No Data to Plot!", type = "error", duration = 10)
    return(NULL)
  }
  
  
  Cont.Data$WellName <- factor(as.character(Cont.Data$WellName), levels = sort(Wells.to.Plot))
  Cont.Data <- Cont.Data[order(Cont.Data$SampleDate),]
 
  

  # In the previous version, se.fit was set with this: 
  # 'se.fit = FALSE & panel$dlines["Conc. Trend Smoother"]' 
  # Seems, that it was always FALSE, so the trend lines are always plotted (which is good).
  # Setting it to true omits the trend lines but prints the following error:
  # "Error using packet X, Argument is of length zero". 
  # Thus, if disabling trend lines would be something we need, this is were we 
  # have to correct the code.
  plotModelPredictions(csite, Cont.Data, SiteName = SiteName,
                       se.fit = FALSE,
                       UseLogScale = UseLogScale, solute_unit = solute_unit)
  
}


#' @importFrom lattice xyplot panel.grid
plotModelPredictions <- function(csite, Cont.Data, SiteName = "", se.fit = FALSE, 
                                 UseLogScale = FALSE, solute_unit = "ug/l"){
  
  if (solute_unit == "mg/l") {
    Cont.Data$Result.Corr.ND <- Cont.Data$Result.Corr.ND / 1000
  }
  
  if (solute_unit == "ng/l") {
    Cont.Data$Result.Corr.ND <- Cont.Data$Result.Corr.ND * 1000
  }
  
  
  NAPL.Present <- any(tolower(as.character(na.omit(Cont.Data$Result))) == "napl")
  Cont <- as.character(unique(Cont.Data$Constituent))
  
  
  #my.xlim <- as.Date(range(c(csite$Cont.Data$SampleDate, csite$All.Data$GW.Data$SampleDate)))
  my.xlim <- as.Date(range(Cont.Data$SampleDate))
  
  my.xlim.orig = my.xlim
  my.xlim[1] <- my.xlim.orig[1] - 0.025*as.numeric(diff(my.xlim.orig))
  my.xlim[2] <- my.xlim.orig[2] + 0.025*as.numeric(diff(my.xlim.orig))
  
  
  my.ylim <- range(csite$Fitted.Data[[Cont]]$Cont.Data[,c("ModelPred","Upper95","Lower95")], na.rm = TRUE)
  
  if (solute_unit == "mg/l") {my.ylim <- my.ylim / 1000}
  if (solute_unit == "ng/l") {my.ylim <- my.ylim * 1000}
  
  my.key <- list( 
    space   = "top", 
    border  = FALSE, 
    columns = 3,
    lines = list(
      pch = c(0,19,19), lty = c(1,1,1), cex = rep(1.4,3), lwd = c(3), col = c("grey","black","orange"), type = c("l","p", "p")
    ),
    text = list( 
      lab = c("Spatiotemporal Prediction","Detectable Data","Non-Detect Data")
    ) 
  ) 
  
  if (NAPL.Present) {
    
    my.key <- list( 
      space   = "top", 
      border  = FALSE, 
      columns = 3,
      lines = list(
        pch = c(0,19,19,19), lty = c(1,1,1,1), cex = rep(1.4,4), lwd = c(3), col = c("grey","black","orange","red"), type = c("l","p", "p","p")
      ),
      text = list( 
        lab = c("Spatiotemporal Prediction","Detectable Data","Non-Detect Data","NAPL Substituted Data")
      ) 
    ) 
    
    Cont.Data$ND<-as.character(Cont.Data$ND)
    Cont.Data$ND[tolower(as.character(Cont.Data$Result))=="napl"]<-"NAPL"
  }
  
  
  #my.plot <- 
  plot(lattice::xyplot(Result.Corr.ND ~ as.Date(SampleDate) | WellName,
                             data = Cont.Data, groups = as.character(Cont.Data$ND),
                    panel = function(x, y,groups,subscripts) {
                    try( lattice::panel.grid(h = -1, v = 2) )
                    groupNDx <- x[groups[subscripts] == "TRUE"]
                    groupNDy <- y[groups[subscripts] == "TRUE"]
                    panel.xyplot(groupNDx, groupNDy, col = "orange", pch = 19, cex = 1.0)
                    
                    
                    groupx<-x[groups[subscripts]=="FALSE"]
                    groupy<-y[groups[subscripts]=="FALSE"]
                    
                    panel.xyplot(groupx,groupy,col="black",pch=19,cex=1.0)
                    
                    
                    groupNAPLx <- x[groups[subscripts]=="NAPL"]
                    groupNAPLy <- y[groups[subscripts]=="NAPL"]
                    
                    if(length(groupNAPLx) > 0) { panel.xyplot(groupNAPLx,groupNAPLy,col="red",pch=19,cex=1.0)}
                    
                    
                    #if(sm.fit && length(x)>1){
                    
                    if(length(x)>1){
                      
                      Model<-csite$Fitted.Data[[as.character(Cont)]]$Model.tune
                      
                      if(!inherits(Model,"try-error")){
                        
                        
                        Model<-Model$best.model
                        eval.df<-data.frame(AggDate=seq(min(x,na.rm=T),max(x,na.rm=T),l=50),XCoord=rep(Cont.Data[subscripts,"XCoord"][1],50),YCoord=rep(Cont.Data[subscripts,"YCoord"][1],50))
                        
                        pred <- predict(Model,eval.df, se = se.fit) ###
                        eval.df$pred<-pred$predicted
                        
                        if (se.fit) {
                          eval.df$upper<-pred$predicted+pred$predicted.sd*1.96
                          eval.df$lower<-pred$predicted-pred$predicted.sd*1.96
                        }
                        
                        if (solute_unit == "mg/l") {
                          
                          eval.df$pred <-log(exp(eval.df$pred)/1000)
                          
                          if (se.fit) {
                            eval.df$upper <- log(exp(eval.df$upper)/1000)
                            eval.df$lower <- log(exp(eval.df$lower)/1000)
                          }
                          
                        }
                        
                        if (solute_unit == "ng/l") {
                          
                          eval.df$pred <-log(exp(eval.df$pred)*1000)
                          
                          if(se.fit){
                            eval.df$upper<-log(exp(eval.df$upper)*1000)
                            eval.df$lower<-log(exp(eval.df$lower)*1000)
                          }
                          
                        }
                        
                        
                        if(UseLogScale){
                          
                          eval.df$pred <-log(exp(eval.df$pred),base=10)
                          if(se.fit){
                            eval.df$upper<-log(exp(eval.df$upper),base=10)
                            eval.df$lower<-log(exp(eval.df$lower),base=10)
                          }
                          
                        }else{
                          
                          eval.df$pred <-exp(eval.df$pred)
                          
                          if (se.fit) {
                            eval.df$upper <- exp(eval.df$upper)
                            eval.df$lower <- exp(eval.df$lower)
                          }
                          
                        }
                        
                        panel.xyplot(as.Date(eval.df$AggDate),eval.df$pred,type="l",col="grey",lwd=3)	
                        if (se.fit) {
                          panel.xyplot(as.Date(eval.df$AggDate), eval.df$upper,type="l",lty=2,col="grey",lwd=2)	
                          panel.xyplot(as.Date(eval.df$AggDate), eval.df$lower,type="l",lty=2,col="grey",lwd=2)	
                        }
                        
                      }
                    }
                    
                  },
                  scales = list(y = list(log = UseLogScale)),
                  xlab   = list("Sampling Date", cex = 1.5), ylab = list(paste("Solute concentration"," (",solute_unit,")",sep=""),cex=1.5),
                  #layout = if (length(levels(Cont.Data$Well)) > 30) { c(4,4)} else{NULL},
                  xlim   = my.xlim,
                  #ylim=my.ylim,
                  main   = if (csite$Aquifer == "") {paste("Spatiotemporal Predictions for ",Cont,"at",SiteName) } else {
                      paste("Spatiotemporal Predictions for ", Cont," at ", SiteName, ": Aquifer-",csite$Aquifer, sep = "")},
                  drop.unused.levels = FALSE, key = my.key)) 
}


plotSTPredictionsPPT <- function(csite, fileout, substance = NULL, Wells.to.Plot = NULL,  
                              UseLogScale = FALSE, solute_unit = "ug/l",
                              width = 900, height = 500) {
  
  # Initialize Powerpoint file.
  if (is.null(ppt_pres <- initPPT())) {
    return(NULL)
  }
  
  # Create temporary wmf file. 
  mytemp <- tempfile(fileext = ".png")
  
  png(mytemp, width = width, height = height) 
  plotSTPredictions(csite, substance, Wells.to.Plot, UseLogScale, solute_unit)
  dev.off()
  
  ppt_pres <- addPlotPPT(mytemp, ppt_pres, width, height) 
  
  print(ppt_pres, target = fileout) %>% invisible()
  
  try(file.remove(mytemp))
  
}


