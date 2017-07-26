





Plot_SmoothTimeSeries <- function(panel, showvline = FALSE, RUNNING_SHINY = TRUE){
  
  
  Use.LogScale = panel$dlines["Log Conc. Scale"]
  
  Well.Data <- panel$All.Data$Cont.Data[as.character(panel$All.Data$Cont.Data$WellName) == panel$Well & panel$All.Data$Cont.Data$Constituent == panel$Cont.rg,]
  
  
  if (panel$rgUnits == "mg/l") { Well.Data$Result.Corr.ND <- Well.Data$Result.Corr.ND/1000 }
  if (panel$rgUnits == "ng/l") { Well.Data$Result.Corr.ND <- Well.Data$Result.Corr.ND*1000 }
  
  Num.Data.Pts <- nrow(Well.Data)
  smThreshSe <- panel$GWSDAT_Options$smThreshSe
  Det.Pts <- Well.Data$ND == FALSE
  ND.Pts <- Well.Data$ND == TRUE
  
  # Use extra variable because "Overlay NAPL Thickness" might not be defined in dlines
  show_napl_thickness <- FALSE
  if ("Overlay NAPL Thickness" %in% names(panel$dlines)) { show_napl_thickness <- panel$dlines["Overlay NAPL Thickness"] } 
  
  #
  # This is done now with napl_exists(). This function is also used by the server.
  #NAPL.Present <- any("napl" %in% tolower(as.character(Well.Data$Result))) || 
  #  nrow(panel$All.Data$NAPL.Thickness.Data[as.character(panel$All.Data$NAPL.Thickness.Data$WellName) == panel$Well,]) > 0
  #if (is.na( NAPL.Present)) { NAPL.Present <- FALSE }
  
  NAPL.Present <- napl_exists(panel$All.Data, panel$Well, panel$Cont.rg)
  
  
  # Adapt threshold value to different units.
  Local_Stat.Lim <- panel$Stat.Lim
  if (panel$rgUnits == "mg/l") { Local_Stat.Lim = Local_Stat.Lim / 1000 }
  if (panel$rgUnits == "ng/l") { Local_Stat.Lim = Local_Stat.Lim * 1000 }
  if (panel$rg1 == "Trend") { Local_Stat.Lim = NA }
  
  
  
  GWAxis <- panel$dlines["Overlay GW levels"] && "GWFlows" %in% 
    names(attributes(panel$Fitted.Data)) && 
    any(as.character(panel$All.Data$GW.Data$WellName) == panel$Well)
  NAPLAxis <- (show_napl_thickness && NAPL.Present)
  
  tempinc <- 0.4
  if (NAPLAxis | GWAxis) {
    
    if (NAPLAxis && GWAxis) {
      
      op <- par(mar = c(5.1,4.1,4.1,5.1 + tempinc))
      
    }else{
      
      op <- par(mar = c(5.1,4.1,4.1,3.1 + tempinc))
    }
    
    
  }else{
    
    
    op<-par(mar=c(5.1,4.1,4.1,2.15+tempinc))
    
  }
  
  
  
  
  
  if(panel$dlines["Scale to Conc. Data"] & nrow(Well.Data) > 0) {
    
    my.ylim <- range(Well.Data$Result.Corr.ND,na.rm = T)
    if (!is.finite(my.ylim[2])) {my.ylim[2] <- 100000}
    my.xlim <- range(Well.Data$SampleDate)
    
  } else {
    
    if (nrow(Well.Data) > 0) {my.ylim <- c(min(Well.Data$Result.Corr.ND, Local_Stat.Lim,na.rm = T),max(Well.Data$Result.Corr.ND,Local_Stat.Lim,na.rm=T))}
    else {my.ylim = c(0.01,100)}
    my.xlim <- range(c(panel$All.Data$Cont.Data$SampleDate, panel$All.Data$GW.Data$SampleDate),na.rm = T) #maybe change to AggDate!
  
    }
  
  
  sm.fit <- NULL
  sm.h <- panel$Traffic.Lights$h[panel$Well,panel$Cont.rg]
  
  
  if (panel$dlines["Conc. Trend Smoother"] & !is.na(sm.h)) {
    
    
    my.eval.points<-seq(range(Well.Data$SampleDate)[1],range(Well.Data$SampleDate)[2],length=40)
    sm.fit <- try(sm.regression(Well.Data$SampleDate, log(Well.Data$Result.Corr.ND), display = "none",h=sm.h,eval.points = my.eval.points))
    
    if(!inherits(sm.fit, "try-error")){
      
      sm.est.keep  <- sm.fit$estimate;
      sm.95up.keep <- exp(sm.est.keep+2*sm.fit$se)
      sm.95low.keep<- exp(sm.est.keep-2*sm.fit$se)
      sm.95up.keep[!sm.fit$se>smThreshSe]<-NA
      sm.95low.keep[!sm.fit$se>smThreshSe]<-NA
      sm.est.keep  <- exp(sm.est.keep)
      
      sm.fit$estimate[sm.fit$se>smThreshSe]<-NA
      sm.est	     <-exp(sm.fit$estimate)
      sm.95up      <-exp(sm.fit$estimate+2*sm.fit$se)
      sm.95low     <-exp(sm.fit$estimate-2*sm.fit$se)
      
      
      if(!panel$dlines["Scale to Conc. Data"]){
        
        my.ylim<-c(min(my.ylim[1],sm.95low,na.rm=T),max(my.ylim[2],sm.95up,na.rm=T))
        if(!is.finite(my.ylim[2])){my.ylim[2]<-100000}
      }
    }
  }
  
  
  if (panel$dlines["Conc. Linear Trend Fit"] & sum(Det.Pts) > 0 & panel$Cont.rg != " ") {
    
    
    lm.fit <- try(lm(log(Result.Corr.ND)~SampleDate,Well.Data))
    lm.eval.points <- data.frame(SampleDate = as.Date(seq(range(Well.Data$SampleDate)[1],range(Well.Data$SampleDate)[2],length = 100)))
    lm.pred <- predict(lm.fit,newdata = lm.eval.points,interval = "confidence")
    lm.eval.points$fit <- exp(lm.pred[,"fit"])
    lm.eval.points$lwr <- exp(lm.pred[,"lwr"])
    lm.eval.points$upr <- exp(lm.pred[,"upr"])
    
    if (!panel$dlines["Scale to Conc. Data"]) {
      
      my.ylim <- c(min(my.ylim[1],lm.eval.points$lwr,na.rm = T),max(my.ylim[2],lm.eval.points$upr,na.rm = T))
      if (!is.finite(my.ylim[2])) {my.ylim[2] <- 100000}
    }
    
    Mann.test <- try(Kendall(Well.Data$SampleDate, log(Well.Data$Result.Corr.ND)), silent = TRUE)
    
  }
  
  
  if (any(ND.Pts)) {#Segment plotting
    
    
    seg.lim <- Well.Data$Result.Corr.ND[ND.Pts]
    if (length(grep("half",tolower(panel$GWSDAT_Options$NDMethod))) > 0) {seg.lim = 2*seg.lim}	
    try(if (my.ylim[2] < max(seg.lim)) {my.ylim[2] <- max(seg.lim)})
    
  }
  
  if (any(!is.finite(my.ylim))) {my.ylim = c(1,100)}
  
  if (Use.LogScale) {
    
    if(panel$dlines["Show Legend"]){my.ylim[2]<-10^(log(my.ylim[1],base=10)+1.15*log(my.ylim[2]/my.ylim[1],base=10))}# make space for Key!
    plot(Result.Corr.ND~SampleDate,Well.Data,xlab="Date",
         ylab=if(panel$Cont.rg!=" "){paste(panel$Cont.rg," (",panel$rgUnits,")",sep="")}else{""},
         ylim=my.ylim,xlim=my.xlim,log="y",cex.lab=1,cex.main=1,axes=FALSE)
    rect(par("usr")[1], 10^par("usr")[3], par("usr")[2], 10^par("usr")[4], col = "white") 
    
  }else{
    
    if(panel$dlines["Show Legend"]){my.ylim[2]<-my.ylim[2]+diff(range(my.ylim))*.15}# make space for Key!
    plot(Result.Corr.ND~SampleDate,Well.Data,xlab="Date",
         ylab=if(panel$Cont.rg!=" "){paste(panel$Cont.rg," (",panel$rgUnits,")",sep="")}else{""},
         ylim=my.ylim,xlim=my.xlim,cex.lab=1,cex.main=1,axes=FALSE)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white") 
  }
  
  
  #axis.Date(1, my.xlim)
  axis.Date(1, seq(my.xlim[1],my.xlim[2],l=10))
  #,format="%b %d")
  
  
  if(nrow(panel$All.Data$Cont.Data[as.character(panel$All.Data$Cont.Data$Result) != "NAPL" & !is.na(panel$All.Data$Cont.Data$Result),]) != 0) {axis(2)} #if no Conc Data suppress Y-axis
  box()	
  title(main = paste(panel$Cont.rg, if (panel$Cont.rg != " ") {"in"}else{""}, panel$Well,if (panel$All.Data$Aq.sel != "") {paste(": Aquifer-",panel$All.Data$Aq.sel,sep = "")} else {""}), font.main = 4, cex.main = 1)
  
  
  grid(NA,NULL,lwd = 1,lty = 1,equilogs = FALSE)
  
  abline(v = as.Date(c(paste(1990:2030,c("-01-01"),sep=""),paste(1990:2030,c("-06-30"),sep=""))),lwd=1,lty=1,col = "lightgray")
  if (length(grep("Threshold",panel$rg1))>0){if(!is.na(Local_Stat.Lim)){abline(h=Local_Stat.Lim,col="red",lty=2,lwd=3)}}
  
  if (panel$dlines["Show Legend"]) {
    
    choose.vec = c(TRUE,TRUE, NAPL.Present, panel$dlines["Conc. Linear Trend Fit"],
                   panel$dlines["Conc. Trend Smoother"],
                   length(grep("Threshold",panel$rg1)) > 0,
                   panel$dlines["Overlay GW levels"], 
                   show_napl_thickness
                  )
    
    if(!all(choose.vec[4:8] == FALSE)) {
      try(legend("top",
                 c("Detectable Data","Non-Detect Data","NAPL Substituted Data","Linear Conc. Trend","Conc. Smoother","Threshold Limit","GW Elevation","NAPL Thickness")[choose.vec],
                 pch=c(19,19,19,-1,-1,-1,1,1)[choose.vec],
                 lty=c(-1,-1,-1,1,1,2,1,1)[choose.vec],
                 lwd=c(-1,-1,-1,2,2,3,1,1)[choose.vec],
                 pt.cex=c(1.2,1.2,1.2,1.2,1.2,1.2,1,1)[choose.vec],
                 col=c("black","orange","red","green","blue","red","black","red")[choose.vec],horiz = F,cex=.5,ncol=min(3,ceiling(sum(choose.vec)/2)))
      )
      
      
    }else{
      
      try(legend("top",
                 c("Detectable Data","Non-Detect Data","NAPL Substituted Data")[choose.vec[1:3]],
                 pch=c(19,19,19)[choose.vec[1:3]],
                 pt.cex=c(1.2,1.2,1.2)[choose.vec[1:3]],
                 col=c("black","orange","red")[choose.vec[1:3]],
                 horiz = F,cex=.5,ncol=min(3,ceiling(sum(choose.vec)/2))))
      
    }
    
  }
  
  
  
  
  
  if (showvline) {abline(v = panel$All.Data$All.Agg.Dates[panel$jjj],col = "grey",lwd = 3)}
  points(Result.Corr.ND~SampleDate,Well.Data[Det.Pts,],cex = 1.5,pch = 19,col = "black")
  points(Result.Corr.ND~SampleDate,Well.Data[ND.Pts, ],cex = 1.5,pch = 19,col = "orange")
  if (NAPL.Present) {points(Result.Corr.ND~SampleDate,Well.Data[tolower(as.character(Well.Data$Result)) == "napl", ],cex = 1.5,pch = 19,col = "red")}
  
  
  if (panel$dlines["Conc. Trend Smoother"] & !inherits(sm.fit, "try-error") & !is.null(sm.fit)) {
    
    try(lines(my.eval.points,sm.est.keep,col = "grey",lwd = 2))#15Sep
    try(lines(my.eval.points,sm.95up.keep,col = "grey",lwd = 2,lty = 2))
    try(lines(my.eval.points,sm.95low.keep,col = "grey",lwd = 2,lty = 2))
    
    try(lines(my.eval.points,sm.est,col = "blue",lwd = 2))
    try(lines(my.eval.points,sm.95up,col = "blue",lwd = 2,lty = 2))
    try(lines(my.eval.points,sm.95low,col = "blue",lwd = 2,lty = 2))
    
  }
  
  
  if (panel$dlines["Conc. Linear Trend Fit"] & sum(Det.Pts) > 1 & !inherits(lm.fit, "try-error") & panel$Cont.rg != " ") {
    
    
    try(lines(lm.eval.points$SampleDate, lm.eval.points$fit, lwd = 2, col = "green"))
    try(lines(lm.eval.points$SampleDate, lm.eval.points$lwr, lwd = 2, col = "green", lty = 2))
    try(lines(lm.eval.points$SampleDate, lm.eval.points$upr, lwd = 2, col = "green", lty = 2))
    
    
    temp.tex1 <- "Mann-Kendall test failed."
    temp.col  <- "red"
    
    try({
      temp.tex1 <- paste("Mann-Kendall P.Value=", 
                         format.pval(Mann.test$sl, digits = 3, eps = 0.01))
      temp.col = if (Mann.test$sl < 0.05) {"darkgreen"} else {"red"}
    }, silent = TRUE)


    temp.tex2 <- "Half-Life= None"
    try({
      
      half.life <- -round(log(2)/as.numeric(lm.fit$coeff[2]))
      temp.tex2 <- paste("Half-Life=",as.character(half.life),"days")
      
      if (abs(half.life) > 0.5*3650) { temp.tex2 <- paste("Half-Life> 5 Years") }
      if (half.life < (-0.5*3650)) {temp.tex2 <- paste("Half-Life> -5 Years") }
    })
    
    
    mtext(paste(temp.tex1,temp.tex2, sep = "; "), 
              side = 3, line = 0.4, cex = 0.75, col = temp.col)
  }
  
  kp <- par()
  
  GWInc <- FALSE
  
  if (panel$dlines["Overlay GW levels"]) {
    
    Well.GW.Data<-panel$All.Data$GW.Data[as.character(panel$All.Data$GW.Data$WellName)==panel$Well,]
    Well.GW.Data<-Well.GW.Data[order(Well.GW.Data$SampleDate),]
    
    if(nrow(Well.GW.Data)>0){
      
      par(new=T)
      GW.ylim=range(Well.GW.Data$Result,na.rm=T)
      if(panel$dlines["Show Legend"]){GW.ylim[2]<-GW.ylim[2]+diff(range(GW.ylim,na.rm=T))*.15}
      plot(Result~SampleDate,Well.GW.Data,yaxt='n',xaxt='n',xlab="",ylab="",xlim=my.xlim,ylim=GW.ylim,type="b",col="black")
      axis(4,axTicks(4),cex.axis=.7,padj=-2.2,tcl=-0.3)
      mtext(paste("Groundwater Elevation (",panel$All.Data$GW.Units,")",sep=""), side=4,line=0.75,cex=.7,col="black")
      GWInc<-TRUE
    }
    
  }
  

  
  if (show_napl_thickness & !is.null(panel$All.Data$NAPL.Thickness.Data)) {
    
    Well.NAPL.Thickness.Data <- panel$All.Data$NAPL.Thickness.Data[as.character(panel$All.Data$NAPL.Thickness.Data$WellName)==panel$Well,]
    Well.NAPL.Thickness.Data <- Well.NAPL.Thickness.Data[order(Well.NAPL.Thickness.Data$SampleDate),]
    
    if (nrow(Well.NAPL.Thickness.Data)>0 && GWInc == FALSE) {
      
      par(new = T)
      NAPL.ylim = range(Well.NAPL.Thickness.Data$Result.Corr.ND,na.rm = T)
      if(panel$dlines["Show Legend"]){NAPL.ylim[2] <- NAPL.ylim[2]+diff(range(NAPL.ylim,na.rm=T))*.15}
      
      plot(Result.Corr.ND~SampleDate,Well.NAPL.Thickness.Data,yaxt='n',xaxt='n',xlab="",ylab="",xlim=my.xlim,ylim=NAPL.ylim,type="b",col="red")
      axis(4,axTicks(4),cex.axis=.7,padj=-2.2,tcl=-0.3)
      mtext(paste("NAPL Thickness (",panel$All.Data$NAPL.Units,")",sep=""), side=4,line=0.75,cex=.7,col="black")
      
    }
    
    if(nrow(Well.NAPL.Thickness.Data)>0 && GWInc==TRUE){
      
      par(new=T)
      NAPL.ylim=range(Well.NAPL.Thickness.Data$Result.Corr.ND,na.rm=T)
      NAPL.ylim[1]=0
      if(panel$dlines["Show Legend"]){NAPL.ylim[2]<-NAPL.ylim[2]+diff(range(NAPL.ylim,na.rm=T))*.15}
      plot(Result.Corr.ND~SampleDate,Well.NAPL.Thickness.Data,yaxt='n',xaxt='n',xlab="",ylab="",xlim=my.xlim,ylim=NAPL.ylim,type="b",col="red")
      axis(4,axTicks(4),cex.axis=.7,col="red",line=2,padj=-2.2,tcl=-0.3)
      mtext(paste("NAPL Thickness (",panel$All.Data$NAPL.Units,")",sep=""), side=4,line=2.75,cex=.7,col="black")
      
    }
    
    
  }
  
  par(op)
  return(panel)
  
}


############################################### End Time Series Plot ###########################################################
