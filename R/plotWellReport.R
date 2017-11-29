

plotWellReport <- function(csite, Conts.to.plot = NULL, Wells.to.Plot = NULL,  
                             UseLogScale = FALSE){
  
  on.exit(palette("default"))
  
  Cont.Data <- csite$All.Data$Cont.Data
  SiteName <- csite$GWSDAT_Options$SiteName
       
  Cont.Data <- Cont.Data[as.character(Cont.Data$Constituent) %in% Conts.to.plot,]
  Cont.Data <- Cont.Data[as.character(Cont.Data$WellName) %in% Wells.to.Plot,]
  
  if (nrow(Cont.Data) == 0) {
    showNotification("Well Report: No data to plot.", type = "warning", duration = 10)
    return()
  }
  
  
  Cont.Data$WellName <- factor(as.character(Cont.Data$WellName), levels = sort(Wells.to.Plot))
  Cont.Data <- Cont.Data[order(Cont.Data$SampleDate),]
  
  if (csite$ui_attr$conc_unit_selected == "mg/l") {Cont.Data$Result.Corr.ND <- Cont.Data$Result.Corr.ND/1000}
  if (csite$ui_attr$conc_unit_selected == "ng/l") {Cont.Data$Result.Corr.ND <- Cont.Data$Result.Corr.ND*1000}
  
  if (length(Conts.to.plot) == 1) {
    
    myplot <- GWSDAT.xyplotWells(csite, Cont.Data, SiteName = SiteName, 
                                 sm.fit = csite$ui_attr$ts_options["Conc. Trend Smoother"], UseLogScale = UseLogScale)
    
  } else {
    
    myplot <- GWSDAT.xyplotAllContbyWells(csite, Cont.Data, SiteName = SiteName, 
                                          UseLogScale = UseLogScale)
    
  }
  
  # Make the plot. If called inside renderPlot this will send it into the 
  # corresponding csite. 
  print(myplot)
    
  
  
}



#' @importFrom sm sm.regression
#' @importFrom lattice xyplot panel.xyplot
GWSDAT.xyplotWells <- function(csite, Cont.Data, SiteName = "", sm.fit=TRUE, UseLogScale=FALSE){
  
  NAPL.Present <- any(tolower(as.character(na.omit(Cont.Data$Result))) == "napl")
  
  
  Cont <- unique(Cont.Data$Constituent)
  
  my.xlim <- range(c(csite$All.Data$Cont.Data$SampleDate, csite$All.Data$GW.Data$SampleDate)) 
  
  my.xlim.orig = my.xlim
  my.xlim[1] <- my.xlim.orig[1] - 0.025*as.numeric(diff(my.xlim.orig))
  my.xlim[2] <- my.xlim.orig[2] + 0.025*as.numeric(diff(my.xlim.orig))
  
  
  my.key <- list(
    space   = "top", 
    border  = FALSE, 
    columns = 2,
    points  = list( 
    pch     = rep(19,2), 
    cex     = rep(1.4,2), 
    col     = c("black","orange")
    ), 
    text = list( 
      lab = c("Detectable Data","Non-Detect Data")
    ) 
  ) 
  
  if (NAPL.Present) {
    
    my.key <- list( 
      space = "top", 
      border = FALSE, 
      columns=3,
      points = list( 
      pch = rep(19,3), 
      cex = rep(1.4,3), 
      col = c("black","orange","red")
      ), 
      text = list( 
        lab = c("Detectable Data","Non-Detect Data","NAPL Substituted Data")
      ) 
    ) 
    
    Cont.Data$ND <- as.character(Cont.Data$ND)
    Cont.Data$ND[tolower(as.character(Cont.Data$Result)) == "napl"] <- "NAPL"
  }
  
  #
  # There is too much going on in the function arguments. Untangle this..
  #
  
  my.plot <- lattice::xyplot(Result.Corr.ND ~ as.Date(SampleDate) | WellName,
                    data = Cont.Data,
                    groups = as.character(Cont.Data$ND),
                    panel = function(x, y,groups,subscripts) {
                      # try(csite.grid(h = -1, v = 2))
                      groupNDx <- x[groups[subscripts] == "TRUE"]
                      groupNDy <- y[groups[subscripts] == "TRUE"]
                      panel.xyplot(groupNDx,groupNDy,col = "orange", pch = 19, cex = 1.0)
                    
                      groupx<-x[groups[subscripts] == "FALSE"]
                      groupy<-y[groups[subscripts] == "FALSE"]
                      panel.xyplot(groupx,groupy,col = "black",pch=19,cex=1.0)
                    
                      groupNAPLx <- x[groups[subscripts] == "NAPL"]
                      groupNAPLy <- y[groups[subscripts] == "NAPL"]
                    
                      if (length(groupNAPLx)>0) {panel.xyplot(groupNAPLx, groupNAPLy, col = "red", pch = 19,cex = 1.0)}
                    
                      if (sm.fit && length(x) > 1) {
                      
                        h = try(csite$Traffic.Lights$h[as.character(unique(Cont.Data[subscripts,"WellName"])),as.character(Cont)])
                        try(eval.points<-seq(min(x,na.rm=T),max(x,na.rm=T),l=40))
                        try(if(UseLogScale){y=log(10^y)}else{y=log(y)})
                        try(sr <- sm::sm.regression(x,y,h=h,display="none",eval.points=eval.points))
                      
                        try(sr.keep<-sr$estimate)
                        try(sr.keep      <- if(UseLogScale){log(exp(sr$estimate),base=10)}else{exp(sr$estimate)})
                        try(sr.95up.keep <- if(UseLogScale){log(exp(sr$estimate+2*sr$se),base=10)}else{exp(sr$estimate+2*sr$se)})
                        try(sr.95low.keep<- if(UseLogScale){log(exp(sr$estimate-2*sr$se),base=10)}else{exp(sr$estimate-2*sr$se)})
                        try(panel.xyplot(as.Date(sr$eval.points), sr.keep,type="l",col="grey"))
                        try(panel.xyplot(as.Date(sr$eval.points), sr.95up.keep,type="l",col="grey"))
                        try(panel.xyplot(as.Date(sr$eval.points), sr.95low.keep,type="l",col="grey"))
                      
                      
                        try(sr$estimate[sr$se > csite$GWSDAT_Options$smThreshSe]<-NA)
                        try(sr.fit<-  if(UseLogScale){log(exp(sr$estimate),base=10)}else{exp(sr$estimate)})
                        try(sr.95up<- if(UseLogScale){log(exp(sr$estimate+2*sr$se),base=10)}else{exp(sr$estimate+2*sr$se)})
                        try(sr.95low<-if(UseLogScale){log(exp(sr$estimate-2*sr$se),base=10)}else{exp(sr$estimate-2*sr$se)})
                        try(panel.xyplot(as.Date(sr$eval.points), sr.fit,type="l",col="blue"))
                        try(panel.xyplot(as.Date(sr$eval.points), sr.95up,type="l",col="blue"))
                        try(panel.xyplot(as.Date(sr$eval.points), sr.95low,type="l",col="blue"))
                      }
                    },
                  scales = list(y=list(log = UseLogScale)),
                  xlab = list("Sampling Date",cex = 1.5),
                  ylab = list(paste("Solute concentration"," (", csite$ui_attr$conc_unit_selected,")", sep = ""),cex=1.5),
                  #layout = if(length(levels(Cont.Data$Well))>30){c(4,4)}else{NULL},
                  xlim = my.xlim,
                  main = if (csite$Aquifer == "") {paste(Cont,"at",SiteName)} else { 
                    paste(Cont," at ",SiteName,": Aquifer-", csite$Aquifer, sep = "")},
                  drop.unused.levels = FALSE, key = my.key) 
  
  return(my.plot)
  
  
  
}


################### All Wells all Conts #########################################

#' @importFrom lattice xyplot
GWSDAT.xyplotAllContbyWells <- function(csite, Cont.Data, SiteName = "", UseLogScale=FALSE) {
  
  my.xlim <- c(min(Cont.Data$SampleDate,na.rm = T),max(Cont.Data$AggDate,na.rm = T)) 
  
  # Add GW date range into xlim - Is this needed ? Turn off for now. 
  # my.xlim <- range(c(my.xlim, csite$All.Data$GW.Data$SampleDate), na.rm = T) 
  
  my.xlim.orig <-  my.xlim
  my.xlim[1]   <- my.xlim.orig[1] - 0.025 * as.numeric(diff(my.xlim.orig))
  my.xlim[2]   <- my.xlim.orig[2] + 0.025 * as.numeric(diff(my.xlim.orig))
  
  
  palette("default")
  palette(c(palette(),"purple","orange","deeppink4","springgreen","indianred"))
  Num.Conts <- nlevels(Cont.Data$Constituent)
  if (Num.Conts > length(palette())) {palette(rainbow(Num.Conts)) }
  
  my.key <- list( 
    space   = "top", 
    border  = FALSE, 
    columns = min(Num.Conts,4),
    points  = list( 
      pch     = rep(19,Num.Conts), 
      cex     = rep(1.2,Num.Conts), 
      col     = 1:Num.Conts
    ), 
    text = list( 
      lab = as.character(levels(Cont.Data$Constituent)) 
    ) 
  ) 
  
  
  
  myplot <- lattice::xyplot(Result.Corr.ND ~ SampleDate | WellName,
                 groups = Cont.Data$Constituent,
                 data   = Cont.Data,
                 scales = list( y = list(log = UseLogScale)),
                 #layout = if (length(unique(Cont.Data$WellName)) > 30) { c(4,4)} else {NULL},
                 type   = c("b"),
                 pch    = 19,
                 cex    = 0.75,
                 col    = 1:Num.Conts,
                 lwd    = 1,
                 key    = my.key,
                 xlab   = list("Sampling Date",cex = 1.5),
                 ylab   = list(paste("Solute concentration"," (", csite$ui_attr$conc_unit_selected, ")",sep = ""),cex = 1.5),
                 main   = if (csite$Aquifer == "") {SiteName} else {paste(SiteName, ": Aquifer-", csite$Aquifer, sep = "")},
                 drop.unused.levels = FALSE,
                 xlim   = my.xlim
  )
  
  
  return(myplot)
  
}





plotWellReportPPT <- function(csite, fileout, substances, locations, use_log_scale,
                              width = 900, height = 500){
  
  # Initialize Powerpoint file.
  if (is.null(ppt_pres <- initPPT())) {
    return(NULL)
  }
  
  # Create temporary wmf file. 
  mytemp <- tempfile(fileext = ".png")
  
  png(mytemp, width = width, height = height) 
  plotWellReport(csite, substances, locations, use_log_scale)
  dev.off()
  
  ppt_pres <- addPlotPPT(mytemp, ppt_pres, width, height) 
  
  print(ppt_pres, target = fileout) %>% invisible()
  
  try(file.remove(mytemp))

  try(file.remove(mytemp))
}
