

plotPlumeTimeSeries <- function(panel, substance){
  
  
  
  
  #keep.jjj<-panel$jjj
  #keep.Plume.Details<-panel$ScaleCols["Plume Diagnostics?"]
  #panel$ScaleCols["Plume Diagnostics?"]<-TRUE
  #PlumeLengthUnit<-GWSDAT.select.list(c("dimensionless","metres","feet"),title = "Select Plume units:")
  #if(PlumeLengthUnit==""){return(panel)}
  
  #graphics.off()
  #.SavedPlots<<-NULL
  #windows(record=T,width =11, height = 8)
  #try(bringToTop())
  
  
  PlumeStats.df <- data.frame(Agg.Date = panel$All.Data$All.Agg.Dates)
  PlumeStats.df$PlumeMass <- PlumeStats.df$PlumeArea <- PlumeStats.df$COMx <- PlumeStats.df$COMy <- PlumeStats.df$PlumeAverageConc <- rep(NA,nrow(PlumeStats.df))
  
  
  for (i in 1:length(panel$All.Data$All.Agg.Dates)) {
   
    # Fixme: Last argument for interpData (Col.Option) and lev.cut inside interpData()
    #  both change interp.pred, how important is this? Can I get rid of it? 
    #  I would need a "Scale colours to Data".  
    
    interp.pred <- interpData(panel, substance, i, panel$ScaleCols["Scale colours to Data"])
    
    TotalPlume <- getPlumeDiagnostics(panel, substance, timestep = i, interp.pred)
    
    PlumeStats.df$PlumeArea[i] <- TotalPlume$area
    PlumeStats.df$PlumeMass[i] <- TotalPlume$Mass
    PlumeStats.df$COMx[i] <- TotalPlume$PlumeCentreofMass[1]
    PlumeStats.df$COMy[i] <- TotalPlume$PlumeCentreofMass[2]
    PlumeStats.df$PlumeAverageConc[i] <- TotalPlume$volume / TotalPlume$area
    
  }
  

  if (!all(is.na(PlumeStats.df$PlumeMass))) {
    
    tempUnitHandle <- PlumeUnitHandlingFunc(panel$GWSDAT_Options$WellCoordsLengthUnits,
                                            panel$rgUnits,
                                            PlumeStats.df$PlumeMass,
                                            PlumeStats.df$PlumeArea)
    
    PlumeStats.df$PlumeMass <- tempUnitHandle$PlumeMass
    PlumeStats.df$PlumeArea <- tempUnitHandle$PlumeArea
  
    par(mfrow = c(1,3), oma = c(1.5,0,0,0))

        
    # Calculate plume mass
    my.ylim <- range(PlumeStats.df$PlumeMass, na.rm = T)
    
    lmPlumeMass <- try(lm(PlumeMass~Agg.Date, PlumeStats.df, na.action=na.omit))
    
    if (!inherits(lmPlumeMass,"try-error")){
      
      lm.eval.points<-na.omit(PlumeStats.df[,c("Agg.Date","PlumeMass")])
      lm.eval.points<-data.frame(Agg.Date=as.Date(seq(range(lm.eval.points$Agg.Date)[1],range(lm.eval.points$Agg.Date)[2],length=100)))
      lm.pred<-predict(lmPlumeMass,newdata=lm.eval.points,interval="confidence")
      
      lm.eval.points$fit<-lm.pred[,"fit"]
      lm.eval.points$lwr<-lm.pred[,"lwr"]
      lm.eval.points$upr<-lm.pred[,"upr"]
      
      
      my.ylim<-c(min(c(lm.eval.points$lwr,my.ylim),na.rm=T),max(c(lm.eval.points$upr,my.ylim),na.rm=T))
      my.ylim[1]<-max(my.ylim[1],0)
      
    }
    
    
    my.ylab <- paste("Plume Mass",tempUnitHandle$PlumeMassUnits,sep="")
    
    try(plot(PlumeMass ~ Agg.Date,
             PlumeStats.df,
             ylim = my.ylim,
             cex.main = 1.3,
             type = "b",
             main = paste(panel$GWSDAT_Options$SiteName,"\n Plume Mass: ", substance, sep = ""),xlab="Date",cex.lab=1.4,pch=19,cex=1.5,ylab=my.ylab))
    try(mtext(paste("Plume Threshold Conc =",as.numeric(panel$PlumeLimEntry[substance]),"ug/l",sep=""),side=3,line=-1,cex=0.75))
    
    if(!inherits(lmPlumeMass,"try-error")){
      
      #try(lines(lm.eval.points$Agg.Date,lm.eval.points$fit,lwd=2,col="green"))
      #try(lines(lm.eval.points$Agg.Date,lm.eval.points$lwr,lwd=2,col="green",lty=2))
      #try(lines(lm.eval.points$Agg.Date,lm.eval.points$upr,lwd=2,col="green",lty=2))
      
      
    }
    
    Mann.testPlumeMass<-try(Kendall(PlumeStats.df$Agg.Date,PlumeStats.df$PlumeMass))
    
    if(!inherits(Mann.testPlumeMass,"try-error")){
      
      temp.tex1<-paste("(Mann-Kendall P.Value=",format.pval(Mann.testPlumeMass$sl,digits=3,eps=0.01),")",sep="")
      #try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeMass$sl<0.05 & Mann.testPlumeMass$tau<0.0){"darkgreen"}else{"red"}))
      #try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeMass$sl<0.05){"darkgreen"}else{"red"}))
      
    }
    
    #----------------------------------------------------------------------------------------------------------------------------------------#
    
    
    ####################### Plume Area #######################################################################################################
    
    my.ylim<-range(PlumeStats.df$PlumeArea,na.rm=T)
    
    lmPlumeArea<-try(lm(PlumeArea~Agg.Date,PlumeStats.df,na.action=na.omit))
    
    if(!inherits(lmPlumeArea,"try-error")){
      
      lm.eval.points<-na.omit(PlumeStats.df[,c("Agg.Date","PlumeArea")])
      lm.eval.points<-data.frame(Agg.Date=as.Date(seq(range(lm.eval.points$Agg.Date)[1],range(lm.eval.points$Agg.Date)[2],length=100)))
      lm.pred<-predict(lmPlumeArea,newdata=lm.eval.points,interval="confidence")
      
      lm.eval.points$fit<-lm.pred[,"fit"]
      lm.eval.points$lwr<-lm.pred[,"lwr"]
      lm.eval.points$upr<-lm.pred[,"upr"]
      
      
      my.ylim<-c(min(c(lm.eval.points$lwr,my.ylim),na.rm=T),max(c(lm.eval.points$upr,my.ylim),na.rm=T))
      my.ylim[1]<-max(my.ylim[1],0)
      
    }
    my.ylab<-paste("Plume Area",tempUnitHandle$PlumeAreaUnits,sep="")
    
    
    
    try(plot(PlumeArea ~ Agg.Date, PlumeStats.df, 
             ylim = my.ylim, 
             cex.main = 1.3, 
             type = "b",
             main = paste(panel$GWSDAT_Options$SiteName,"\n Plume Area: ", substance, sep = ""),
             xlab = "Date",
             cex.lab = 1.4,
             pch = 19, 
             cex = 1.5, 
             ylab = my.ylab
             )
        )
    
    try(mtext(paste("Plume Threshold Conc =",as.numeric(panel$PlumeLimEntry[substance]),"ug/l",sep=""),side=3,line=-1,cex=0.75))
    
    if (!inherits(lmPlumeArea,"try-error")){
      
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
    
    my.ylim <- range(PlumeStats.df$PlumeAverageConc,na.rm=T)
    
    lmPlumeAverageConc <- try(lm(PlumeAverageConc~Agg.Date,PlumeStats.df,na.action=na.omit))
    
    if(!inherits(lmPlumeAverageConc,"try-error")){
      
      lm.eval.points <- na.omit(PlumeStats.df[,c("Agg.Date","PlumeAverageConc")])
      lm.eval.points <- data.frame(Agg.Date=as.Date(seq(range(lm.eval.points$Agg.Date)[1],range(lm.eval.points$Agg.Date)[2],length=100)))
      lm.pred <- predict(lmPlumeAverageConc,newdata=lm.eval.points,interval="confidence")
      
      lm.eval.points$fit <- lm.pred[,"fit"]
      lm.eval.points$lwr <- lm.pred[,"lwr"]
      lm.eval.points$upr <- lm.pred[,"upr"]
      
      
      my.ylim <- c(min(c(lm.eval.points$lwr,my.ylim),na.rm=T),max(c(lm.eval.points$upr,my.ylim),na.rm=T))
      my.ylim[1] <- max(my.ylim[1],0)
      
    }
    my.ylab <- paste("Concentration",tempUnitHandle$PlumeAverageUnits,sep="")
    
    try(plot(PlumeAverageConc ~ Agg.Date, PlumeStats.df,
             ylim = my.ylim,
             cex.main = 1.3,
             type="b",
             main= paste(panel$GWSDAT_Options$SiteName,"\n Average Plume Concentration: ", substance,sep=""),xlab="Date",cex.lab=1.4,pch=19,
             cex=1.5,ylab=my.ylab))
    
    try(mtext(paste("Plume Threshold Conc =",as.numeric(panel$PlumeLimEntry[substance]),"ug/l",sep=""),side=3,line=-1,cex=0.75))
    
    if(!inherits(lmPlumeAverageConc,"try-error")){
      
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
    
    
    
    
    # if( tclvalue(tkmessageBox(message = "Do you want to output Plume statistics to Excel csv file?",icon = "question", type = "yesno", default = "yes"))=="yes"){
    #   
    #   names(PlumeStats.df)[names(PlumeStats.df)=="PlumeAverageConc"]<-paste("PlumeAverageConc",tempUnitHandle$PlumeAverageUnits,sep="")
    #   names(PlumeStats.df)[names(PlumeStats.df)=="PlumeArea"]<-paste("PlumeArea",tempUnitHandle$PlumeAreaUnits,sep="")
    #   names(PlumeStats.df)[names(PlumeStats.df)=="PlumeMass"]<-paste("PlumeMass",tempUnitHandle$PlumeMassUnits,sep="")
    #   PlumeStats.df[,"Plume Threshold Conc (ug/l)"]<-rep("",nrow=PlumeStats.df)
    #   PlumeStats.df[1,"Plume Threshold Conc (ug/l)"]<-as.numeric(panel$PlumeLimEntry[substance])
    #   
    #   mytmpfile<-NULL
    #   mytmpfile<-dirname(tempfile())
    #   mytmpfile<-paste(mytmpfile,paste(panel$GWSDAT_Options$SiteName,"-",substance,"-Plume Diagnostic Table ",format(Sys.time(), " %Y-%b-%d %H-%M-%S"),".csv",sep=""),sep="/")
    #   try(write.csv(PlumeStats.df,file=mytmpfile,row.names=FALSE))
    #   try(shell.exec(mytmpfile))
    #   
    # }
    
  } else {
    
    return(NULL)
    #tkmessageBox(title="Error!",message=paste("Unable to calculate plume statistics for threshold value=",as.numeric(panel$PlumeLimEntry[substance]),"ug/l.","\nUse the 'Estimate Plume Boundary' function for assistance in selecting a suitable plume threshold concentration value."),icon="error",type="ok")
    
  }
 
  return(1)
}

