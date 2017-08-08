

plotPlumeTimeSeries <- function(panel, substance, plume_thresh, PlumeStats.df) {
  
  # This calculation takes place before. 
  # PlumeStats.df <- getFullPlumeStats(panel, substance)
  

  if (is.null(PlumeStats.df))
    return(NULL)
  
  if (all(is.na(PlumeStats.df$PlumeMass))) 
    return(NULL)
  

  tempUnitHandle <- PlumeUnitHandlingFunc(panel$GWSDAT_Options$WellCoordsLengthUnits,
                                          panel$rgUnits,
                                          PlumeStats.df$PlumeMass,
                                          PlumeStats.df$PlumeArea)
  
  PlumeStats.df$PlumeMass <- tempUnitHandle$PlumeMass
  PlumeStats.df$PlumeArea <- tempUnitHandle$PlumeArea

  par(mfrow = c(1,3), oma = c(1.5,0,0,0))

      
  # Calculate plume mass
  my.ylim <- range(PlumeStats.df$PlumeMass, na.rm = T)
  
  lmPlumeMass <- try(lm(PlumeMass~Agg.Date, PlumeStats.df, na.action = na.omit))
  
  if (!inherits(lmPlumeMass,"try-error")) {
    
    lm.eval.points <- na.omit(PlumeStats.df[,c("Agg.Date","PlumeMass")])
    lm.eval.points <- data.frame(Agg.Date=as.Date(seq(range(lm.eval.points$Agg.Date)[1],range(lm.eval.points$Agg.Date)[2],length=100)))
    lm.pred <- predict(lmPlumeMass,newdata=lm.eval.points,interval="confidence")
    
    lm.eval.points$fit <- lm.pred[,"fit"]
    lm.eval.points$lwr <- lm.pred[,"lwr"]
    lm.eval.points$upr <- lm.pred[,"upr"]
    
    
    my.ylim<-c(min(c(lm.eval.points$lwr,my.ylim),na.rm=T),max(c(lm.eval.points$upr,my.ylim),na.rm=T))
    my.ylim[1]<-max(my.ylim[1],0)
    
  }
  
  
  my.ylab <- paste("Plume Mass",tempUnitHandle$PlumeMassUnits,sep="")
  
  try(plot(PlumeMass ~ Agg.Date,
           PlumeStats.df,
           ylim = my.ylim,
           cex.main = 1.3,
           type = "b",
           main = paste(substance, "\nPlume Mass", sep = ""),
           xlab = "Date",
           cex.lab = 1.4,
           pch = 19,
           cex = 1.5,
           ylab = my.ylab)
      )
  
  try(mtext(paste("Plume Threshold = ", plume_thresh, "ug/l", sep = ""),side=3,line=-1,cex=0.75))
  
  if(!inherits(lmPlumeMass,"try-error")){
    
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$fit,lwd=2,col="green"))
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$lwr,lwd=2,col="green",lty=2))
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$upr,lwd=2,col="green",lty=2))
    
    
  }
  
  Mann.testPlumeMass <- try(Kendall(PlumeStats.df$Agg.Date,PlumeStats.df$PlumeMass))
  
  if(!inherits(Mann.testPlumeMass,"try-error")){
    
    temp.tex1<-paste("(Mann-Kendall P.Value=",format.pval(Mann.testPlumeMass$sl,digits=3,eps=0.01),")",sep="")
    #try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeMass$sl<0.05 & Mann.testPlumeMass$tau<0.0){"darkgreen"}else{"red"}))
    #try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeMass$sl<0.05){"darkgreen"}else{"red"}))
    
  }
  
  #----------------------------------------------------------------------------------------------------------------------------------------#
  
  
  ####################### Plume Area #######################################################################################################
  
  my.ylim <- range(PlumeStats.df$PlumeArea,na.rm=T)
  
  lmPlumeArea <- try(lm(PlumeArea~Agg.Date,PlumeStats.df,na.action=na.omit))
  
  if(!inherits(lmPlumeArea,"try-error")){
    
    lm.eval.points<-na.omit(PlumeStats.df[,c("Agg.Date","PlumeArea")])
    lm.eval.points<-data.frame(Agg.Date=as.Date(seq(range(lm.eval.points$Agg.Date)[1],range(lm.eval.points$Agg.Date)[2],length=100)))
    lm.pred<-predict(lmPlumeArea,newdata=lm.eval.points,interval="confidence")
    
    lm.eval.points$fit <- lm.pred[,"fit"]
    lm.eval.points$lwr <- lm.pred[,"lwr"]
    lm.eval.points$upr <- lm.pred[,"upr"]
    
    
    my.ylim <- c(min(c(lm.eval.points$lwr,my.ylim), na.rm = T), max(c(lm.eval.points$upr,my.ylim),na.rm = T))
    my.ylim[1] <- max(my.ylim[1],0)
    
  }
  my.ylab <- paste("Plume Area", tempUnitHandle$PlumeAreaUnits, sep = "")
  
  
  
  plot(PlumeArea ~ Agg.Date, PlumeStats.df, 
       ylim = my.ylim, 
       cex.main = 1.3, 
       type = "b",
       main = paste(substance, "\nPlume Area", sep = ""),
       xlab = "Date",
       cex.lab = 1.4,
       pch = 19, 
       cex = 1.5, 
       ylab = my.ylab
      )
      
  
  try(mtext(paste("Plume Threshold = ", plume_thresh, "ug/l", sep = ""), side = 3, line = -1, cex = 0.75))
  
  if (!inherits(lmPlumeArea,"try-error")) {
    
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$fit,lwd=2,col="green"))
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$lwr,lwd=2,col="green",lty=2))
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$upr,lwd=2,col="green",lty=2))
    
    
  }
  
  Mann.testPlumeArea <- try(Kendall(PlumeStats.df$Agg.Date,PlumeStats.df$PlumeArea))
  
  if (!inherits(Mann.testPlumeArea,"try-error")){
    
    temp.tex1 <- paste("(Mann-Kendall P.Value=",format.pval(Mann.testPlumeArea$sl,digits=3,eps=0.01),")",sep="")
    #try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeArea$sl<0.05 & Mann.testPlumeArea$tau<0.0){"darkgreen"}else{"red"}))
    #try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeArea$sl<0.05){"darkgreen"}else{"red"}))
    
  }
  
  
  ####################### Plume Average ####################################################################################################
  
  my.ylim <- range(PlumeStats.df$PlumeAverageConc,na.rm = T)
  
  lmPlumeAverageConc <- try(lm(PlumeAverageConc ~ Agg.Date, PlumeStats.df, na.action = na.omit))
  
  if (!inherits(lmPlumeAverageConc,"try-error")) {
    
    lm.eval.points <- na.omit(PlumeStats.df[,c("Agg.Date","PlumeAverageConc")])
    lm.eval.points <- data.frame(Agg.Date = as.Date(seq(range(lm.eval.points$Agg.Date)[1],range(lm.eval.points$Agg.Date)[2],length = 100)))
    lm.pred <- predict(lmPlumeAverageConc,newdata = lm.eval.points,interval = "confidence")
    
    lm.eval.points$fit <- lm.pred[,"fit"]
    lm.eval.points$lwr <- lm.pred[,"lwr"]
    lm.eval.points$upr <- lm.pred[,"upr"]
    
    
    my.ylim <- c(min(c(lm.eval.points$lwr,my.ylim), na.rm = T), max(c(lm.eval.points$upr,my.ylim),na.rm = T))
    my.ylim[1] <- max(my.ylim[1],0)
    
  }
  
  my.ylab <- paste("Concentration",tempUnitHandle$PlumeAverageUnits,sep = "")
  
  try(plot(PlumeAverageConc ~ Agg.Date, PlumeStats.df,
           ylim = my.ylim,
           cex.main = 1.3,
           type = "b",
           main = paste(substance, "\nAverage Plume Concentration", sep = ""),
           xlab = "Date",
           cex.lab = 1.4,
           pch = 19,
           cex = 1.5,
           ylab = my.ylab))
  
  try(mtext(paste("Plume Threshold = ", plume_thresh, "ug/l", sep = ""),side = 3, line = -1, cex = 0.75))
  
  if (!inherits(lmPlumeAverageConc,"try-error")) {
    
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$fit,lwd=2,col="green"))
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$lwr,lwd=2,col="green",lty=2))
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$upr,lwd=2,col="green",lty=2))
    
    
  }
  
  Mann.testPlumeAverageConc<-try(Kendall(PlumeStats.df$Agg.Date,PlumeStats.df$PlumeAverageConc))
  
  if(!inherits(Mann.testPlumeAverageConc,"try-error")){
    
    temp.tex1<-paste("(Mann-Kendall P.Value=",format.pval(Mann.testPlumeAverageConc$sl,digits=3,eps=0.01),")",sep="")
    #try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeAverageConc$sl<0.05 & Mann.testPlumeAverageConc$tau<0.0){"darkgreen"}else{"red"}))
    #try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeAverageConc$sl<0.05){"darkgreen"}else{"red"}))
    
  }

}



plotPlumeTimeSeriesPPT <- function(panel, substance, plume_thresh, plume_stats){
  
  # Create temporary wmf file. 
  mytemp <- tempfile(fileext = ".wmf")
  
  
  win.metafile(mytemp) 
  plotPlumeTimeSeries(panel, substance, plume_thresh, plume_stats)
  dev.off()
  
  # Put into powerpoint slide.
  AddPlotPPV2(mytemp, asp = TRUE) 
  
  try(file.remove(mytemp))
  
  
}