




# Display concentration time-series for contaminant at specific well.
#
# @param csite monitoring site object.
# @param substance contaminant name
# @param location  well name
# @param show_thresh show the threshold line if TRUE
# 
#
#' @importFrom sm sm.regression
plotTimeSeries <- function(csite, 
                           substance = NULL, 
                           location = NULL,
                           show_thresh = FALSE,
                           timepoint = NULL, 
                           export = FALSE,
                           modify_plot_layout=TRUE
                           ) {
  
  showvline = FALSE
  
  #Modify the function to handle multiple wells and substances
  # Set up plotting layout
  date1 <- as.Date(timepoint[1])
  date2 <- as.Date(timepoint[2])
  diff_sec <- as.numeric(difftime(date2, date1, units = "secs"))

   if(timepoint[1] == timepoint[2]){
     stop("Please select different Time period")
   }
  
   
  sl <- length(location) * length(substance)
  
  if(modify_plot_layout){ #Plot layour is suppressed when calling PlotTimeSeries from PowerPoint Well Report. 
    
    if(sl < 3){
      op <- par(mfrow = c(length(substance),length(location)))
    }
    else{
      lr <- ceiling(sl/3)
      op <- par(mfrow = c(lr,3))
    }
  }
  
  Use.LogScale = csite$ui_attr$ts_options["Log Conc. Scale"]
 
  
  loc <- location
  subs <- substance
  
  #Well.Data <- csite$All.Data$Cont.Data[as.character(csite$All.Data$Cont.Data$WellName) == location & csite$All.Data$Cont.Data$Constituent == substance,]
  
  Well.Data <- csite$All.Data$Cont.Data[as.character(csite$All.Data$Cont.Data$WellName) %in% location & csite$All.Data$Cont.Data$Constituent %in% substance,]
  if (export == TRUE) return(Well.Data)
  
  ###################if the charts combinations are above 12 we need to restrict the combinations
  if(length(location)*length(substance)>12){
    stop("Please reduce the number of combinations of location or substance and retry")
  }
  ######################################################################################
  
  for (substance  in subs) {
    for (location in loc) { 
    
      Well.Data <- csite$All.Data$Cont.Data[as.character(csite$All.Data$Cont.Data$WellName) %in% location & 
                                              csite$All.Data$Cont.Data$Constituent %in% substance,]
     
      Well.Data<-Well.Data[Well.Data$SampleDate>=timepoint[1]&Well.Data$SampleDate<=timepoint[2], ]
      
      ###############Adding for the error in selecting multiple plots while no data is available#######
      #  if (nrow(Well.Data) < 0) {
      # #   # Plot an empty graph or display a message
      #    plot(1, type = "n", xlab = "Date", ylab = paste(substance, " (", csite$ui_attr$conc_unit_selected, ")", sep = ""),
      #         xlim = c(as.Date(timepoint[1], format = "%b %Y"), as.Date(timepoint[2], format = "%b %Y")),
      #         ylim = c(0, 1))
      #    title(main = paste("No data available for", substance, "at", location))
      #    next
      #  }
      
      # if(nrow(Well.Data)==0){
      #   stop("Data is not available related to the location -" ,location," and Substance- ",substance)
      # }
      ################################################################################################# 
  
  if (csite$ui_attr$conc_unit_selected == "mg/l") { Well.Data$Result.Corr.ND <- Well.Data$Result.Corr.ND/1000 }
  if (csite$ui_attr$conc_unit_selected == "ng/l") { Well.Data$Result.Corr.ND <- Well.Data$Result.Corr.ND*1000 }
  
  Num.Data.Pts <- nrow(Well.Data)
  smThreshSe <- csite$GWSDAT_Options$smThreshSe
  Det.Pts <- Well.Data$ND == FALSE
  ND.Pts <- Well.Data$ND == TRUE
  
  # Use extra variable because "Overlay NAPL Thickness" might not be defined in ui_attr$ts_options
  show_napl_thickness <- FALSE
  if ("Overlay NAPL Thickness" %in% names(csite$ui_attr$ts_options)) { show_napl_thickness <- csite$ui_attr$ts_options["Overlay NAPL Thickness"] } 
 
  NAPL.Present <- existsNAPL(csite$All.Data, location, substance)
  
  
  # Adapt threshold value to different units.
  Stat.Lim <- csite$ui_attr$conc_thres[substance]
  if (csite$ui_attr$conc_unit_selected == "mg/l") { Stat.Lim = Stat.Lim / 1000 }
  if (csite$ui_attr$conc_unit_selected == "ng/l") { Stat.Lim = Stat.Lim * 1000 }
  #if (csite$ui_attr$trend_thresh_selected == "Trend") { Stat.Lim = NA }
  if (!show_thresh) { Stat.Lim = NA }
  
  GWAxis <- csite$ui_attr$ts_options["Overlay GW levels"] && !is.null(csite$GW.Flows) && 
    any(as.character(csite$All.Data$GW.Data$WellName) == location)
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
  
  
  
  
  
  if (csite$ui_attr$ts_options["Scale to Conc. Data"] & nrow(Well.Data) > 0) {
    
    my.ylim <- range(Well.Data$Result.Corr.ND,na.rm = T)
    if (!is.finite(my.ylim[2])) {my.ylim[2] <- 100000}
    my.xlim <- range(Well.Data$SampleDate)
    
  } else {
    
    if (nrow(Well.Data) > 0) {my.ylim <- c(min(Well.Data$Result.Corr.ND, Stat.Lim,na.rm = T),max(Well.Data$Result.Corr.ND,Stat.Lim,na.rm=T))}
    else {my.ylim = c(0.01,100)}
    my.xlim <- range(c(csite$All.Data$Cont.Data$SampleDate, csite$All.Data$GW.Data$SampleDate),na.rm = T) #maybe change to AggDate!
  
    }
  
  
  sm.fit <- NULL
  #tryCatch(
  sm.h <- csite$Traffic.Lights$h[location, substance]
  #error = function(e) {
  #  browser()
  #})
  
  
  if (csite$ui_attr$ts_options["Conc. Trend Smoother"] & !is.na(sm.h)) {
    
    
    my.eval.points <- try(seq(range(Well.Data$SampleDate)[1],range(Well.Data$SampleDate)[2],length=40))
    sm.fit <- try(sm::sm.regression(Well.Data$SampleDate, log(Well.Data$Result.Corr.ND), display = "none",h=sm.h,eval.points = my.eval.points))
    
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
      
      
      if(!csite$ui_attr$ts_options["Scale to Conc. Data"]){
        
        my.ylim<-c(min(my.ylim[1],sm.95low,na.rm=T),max(my.ylim[2],sm.95up,na.rm=T))
        if(!is.finite(my.ylim[2])){my.ylim[2]<-100000}
      }
    }
  }
  
  
  if (csite$ui_attr$ts_options["Conc. Linear Trend Fit"] & sum(Det.Pts) > 0 & substance != " ") {
    
    
    lm.fit <- try(lm(log(Result.Corr.ND)~SampleDate,Well.Data))
    lm.eval.points <- data.frame(SampleDate = as.Date(seq(range(Well.Data$SampleDate)[1],range(Well.Data$SampleDate)[2],length = 100)))
    lm.pred <- predict(lm.fit,newdata = lm.eval.points,interval = "confidence")
    lm.eval.points$fit <- exp(lm.pred[,"fit"])
    lm.eval.points$lwr <- exp(lm.pred[,"lwr"])
    lm.eval.points$upr <- exp(lm.pred[,"upr"])
    
    if (!csite$ui_attr$ts_options["Scale to Conc. Data"]) {
      
      my.ylim <- c(min(my.ylim[1],lm.eval.points$lwr,na.rm = T),max(my.ylim[2],lm.eval.points$upr,na.rm = T))
      if (!is.finite(my.ylim[2])) {my.ylim[2] <- 100000}
    }
    
    Mann.test <- try(Kendall(Well.Data$SampleDate, log(Well.Data$Result.Corr.ND)), silent = TRUE)
    
  }
  
  
  if (any(ND.Pts)) {#Segment plotting
    
    
    seg.lim <- Well.Data$Result.Corr.ND[ND.Pts]
    if (length(grep("half",tolower(csite$GWSDAT_Options$NDMethod))) > 0) {seg.lim = 2*seg.lim}	
    try(if (my.ylim[2] < max(seg.lim)) {my.ylim[2] <- max(seg.lim)})
    
  }
  
  if (any(!is.finite(my.ylim))) {my.ylim = c(1,100)}
  
  if (Use.LogScale) {
    
    if (csite$ui_attr$ts_options["Show Legend"]) { 
      my.ylim[2] <- 10^(log(my.ylim[1], base = 10) + 1.15 * log(my.ylim[2]/my.ylim[1],base = 10))
    }# make space for Key!
    
    plot(Result.Corr.ND ~ SampleDate, Well.Data, 
         xlab = "Date",
         ylab = if (substance != " ") {paste(substance, " (", csite$ui_attr$conc_unit_selected, ")", sep = "")} else {""},
         ylim = my.ylim, 
         #xlim = my.xlim, 
         xlim = c(as.Date(timepoint[1],format = "%b %Y") ,as.Date(timepoint[2],format = "%b %Y")),
         log  = "y", 
         cex.lab  = 1, 
         cex.main = 1, 
         axes     = FALSE)
    rect(par("usr")[1], 10^par("usr")[3], par("usr")[2], 10^par("usr")[4], col = "white") 
    
  }else{
    
    if (csite$ui_attr$ts_options["Show Legend"]) {
      my.ylim[2] <- my.ylim[2] + diff(range(my.ylim)) * .15
    }# make space for Key!
    
    plot(Result.Corr.ND ~ SampleDate, Well.Data,
         xlab = "Date",
         ylab = if (substance != " ") {paste(substance," (", csite$ui_attr$conc_unit_selected, ")", sep="")} else {""},
         ylim = my.ylim,
         #xlim = my.xlim,
         xlim = c(as.Date(timepoint[1],format = "%b %Y") ,as.Date(timepoint[2],format = "%b %Y")),
         cex.lab  = 1,
         cex.main = 1,
         axes     = FALSE)
    
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white") 
  }
  
  
  #axis.Date(1, my.xlim)
  #axis.Date(1, seq(my.xlim[1],my.xlim[2],l=10))
  
  ###  adding Axis legend in the time series plots
  
  if (diff_sec < (365.25 * 24 * 60 * 60))
    
  {
    axis.Date(1, at = seq(as.Date(timepoint[1]), as.Date(timepoint[2]), by = "month"), format = "%b %Y")
    
  }
  
  else {
    axis.Date(1, at = seq(as.Date(timepoint[1]), as.Date(timepoint[2]), by = "year"), format = "%Y")
  }
  
  if (nrow(csite$All.Data$Cont.Data[as.character(csite$All.Data$Cont.Data$Result) != "NAPL" & !is.na(csite$All.Data$Cont.Data$Result),]) != 0) {axis(2)} #if no Conc Data suppress Y-axis
  graphics::box()	
  #title(main = paste(substance, if (substance != " ") {"in"}else{""}, location,if (csite$Aquifer != "") {paste(": Aquifer-", csite$Aquifer, sep = "")} else {""}), font.main = 4, cex.main = 1)
  # Existing title code
  main_title <- paste(substance, if (substance != " ") {"in"} else {""}, location,
                      if (csite$Aquifer != "") {paste(": Aquifer-", csite$Aquifer, sep = "")} else {""})
  
  # Add date range to the title
  date_range_title <- paste("Data Subset:", timepoint[1], "to", timepoint[2])
  
  # Combine titles with a newline character
  full_title <- paste(main_title, "\n", date_range_title)
  
  # Plot with the new title
  title(main = full_title, font.main = 4, cex.main = 1)
  
  grid(NA,NULL,lwd = 1,lty = 1,equilogs = FALSE)
  
  abline(v = as.Date(c(paste(1990:2030,c("-01-01"), sep = ""),paste(1990:2030,c("-06-30"),sep=""))),lwd=1,lty=1,col = "lightgray")
  #if (length(grep("Threshold",csite$ui_attr$trend_thresh_selected)) > 0) {if(!is.na(Stat.Lim)){abline(h=Stat.Lim,col="red",lty=2,lwd=3)}}
  if (show_thresh && !is.na(Stat.Lim)) abline(h = Stat.Lim, col = "red", lty = 2, lwd = 3)
    
  
  if (csite$ui_attr$ts_options["Show Legend"]) {
    
    choose.vec = c(TRUE,TRUE, NAPL.Present, csite$ui_attr$ts_options["Conc. Linear Trend Fit"],
                   csite$ui_attr$ts_options["Conc. Trend Smoother"],
                   length(grep("Threshold",csite$ui_attr$trend_thresh_selected)) > 0,
                   csite$ui_attr$ts_options["Overlay GW levels"], 
                   show_napl_thickness
                  )
    
    if(!all(choose.vec[4:8] == FALSE)) {
      try(legend("top",
                 c("Detectable Data","Non-Detect Data","NAPL Substituted Data","Linear Conc. Trend","Conc. Smoother","Threshold Limit","GW Elevation","NAPL Thickness")[choose.vec],
                 pch=c(19,19,19,-1,-1,-1,1,1)[choose.vec],
                 lty=c(-1,-1,-1,1,1,2,1,1)[choose.vec],
                 lwd=c(-1,-1,-1,2,2,3,1,1)[choose.vec],
                 pt.cex=c(1.2,1.2,1.2,1.2,1.2,1.2,1,1)[choose.vec],
                 col=c("black","orange","red","green","blue","red","black","red")[choose.vec],horiz = F,cex=.75,ncol=min(3,ceiling(sum(choose.vec)/2)))
      )
      
      
    }else{
      
      try(legend("top",
                 c("Detectable Data","Non-Detect Data","NAPL Substituted Data")[choose.vec[1:3]],
                 pch=c(19,19,19)[choose.vec[1:3]],
                 pt.cex=c(1.2,1.2,1.2)[choose.vec[1:3]],
                 col=c("black","orange","red")[choose.vec[1:3]],
                 horiz = F,cex=.75,ncol=min(3,ceiling(sum(choose.vec)/2))))
      
    }
    
  }
  
  
  
  
  
  if (showvline) {abline(v = csite$All.Data$All_Agg_Dates[csite$jjj],col = "grey",lwd = 3)}
  points(Result.Corr.ND~SampleDate,Well.Data[Det.Pts,],cex = 1.5,pch = 19,col = "black")
  points(Result.Corr.ND~SampleDate,Well.Data[ND.Pts, ],cex = 1.5,pch = 19,col = "orange")
  if (NAPL.Present) {points(Result.Corr.ND~SampleDate,Well.Data[tolower(as.character(Well.Data$Result)) == "napl", ],cex = 1.5,pch = 19,col = "red")}
  
  
  if (csite$ui_attr$ts_options["Conc. Trend Smoother"] & !inherits(sm.fit, "try-error") & !is.null(sm.fit)) {
    
    try(lines(my.eval.points,sm.est.keep,col = "grey",lwd = 2))#15Sep
    try(lines(my.eval.points,sm.95up.keep,col = "grey",lwd = 2,lty = 2))
    try(lines(my.eval.points,sm.95low.keep,col = "grey",lwd = 2,lty = 2))
    
    try(lines(my.eval.points,sm.est,col = "blue",lwd = 2))
    try(lines(my.eval.points,sm.95up,col = "blue",lwd = 2,lty = 2))
    try(lines(my.eval.points,sm.95low,col = "blue",lwd = 2,lty = 2))
    
  }
  
  
  if (csite$ui_attr$ts_options["Conc. Linear Trend Fit"] & sum(Det.Pts) > 1 & !inherits(lm.fit, "try-error") & substance != " ") {
    
    
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
  
  if (csite$ui_attr$ts_options["Overlay GW levels"]) {
    
    Well.GW.Data<-csite$All.Data$GW.Data[as.character(csite$All.Data$GW.Data$WellName)==location,]
    Well.GW.Data<-Well.GW.Data[order(Well.GW.Data$SampleDate),]
    
    if(nrow(Well.GW.Data)>0){
      
      par(new=T)
      GW.ylim=range(Well.GW.Data$Result,na.rm=T)
      if(csite$ui_attr$ts_options["Show Legend"]){GW.ylim[2]<-GW.ylim[2]+diff(range(GW.ylim,na.rm=T))*.15}
      plot(Result~SampleDate,Well.GW.Data,yaxt='n',xaxt='n',xlab="",ylab="",xlim=my.xlim,ylim=GW.ylim,type="b",col="black")
      axis(4,axTicks(4),cex.axis=.7,padj=-2.2,tcl=-0.3)
      mtext(paste("Groundwater Elevation (",csite$All.Data$GW.Units,")",sep=""), side=4,line=0.75,cex=.7,col="black")
      GWInc<-TRUE
    }
    
  }
  

  
  if (show_napl_thickness & !is.null(csite$All.Data$NAPL.Thickness.Data)) {
    
    Well.NAPL.Thickness.Data <- csite$All.Data$NAPL.Thickness.Data[as.character(csite$All.Data$NAPL.Thickness.Data$WellName) == location,]
    Well.NAPL.Thickness.Data <- Well.NAPL.Thickness.Data[order(Well.NAPL.Thickness.Data$SampleDate),]
    
    if (nrow(Well.NAPL.Thickness.Data) > 0 && GWInc == FALSE) {
      
      par(new = T)
      NAPL.ylim = range(Well.NAPL.Thickness.Data$Result.Corr.ND,na.rm = T)
      if (csite$ui_attr$ts_options["Show Legend"]) {NAPL.ylim[2] <- NAPL.ylim[2]+diff(range(NAPL.ylim,na.rm=T))*.15}
      
      plot(Result.Corr.ND~SampleDate,Well.NAPL.Thickness.Data,yaxt = 'n',xaxt='n',xlab="",ylab="",xlim=my.xlim,ylim=NAPL.ylim,type="b",col="red")
      axis(4,axTicks(4),cex.axis = .7,padj = -2.2,tcl=-0.3)
      mtext(paste("NAPL Thickness (",csite$All.Data$NAPL.Units,")",sep = ""), side=4,line=0.75,cex=.7,col="black")
      
    }
    
    if(nrow(Well.NAPL.Thickness.Data)>0 && GWInc==TRUE){
      
      par(new=T)
      NAPL.ylim=range(Well.NAPL.Thickness.Data$Result.Corr.ND,na.rm=T)
      NAPL.ylim[1]=0
      if(csite$ui_attr$ts_options["Show Legend"]){NAPL.ylim[2]<-NAPL.ylim[2]+diff(range(NAPL.ylim,na.rm=T))*.15}
      plot(Result.Corr.ND~SampleDate,Well.NAPL.Thickness.Data,yaxt='n',xaxt='n',xlab="",ylab="",xlim=my.xlim,ylim=NAPL.ylim,type="b",col="red")
      axis(4,axTicks(4),cex.axis=.7,col="red",line=2,padj=-2.2,tcl=-0.3)
      mtext(paste("NAPL Thickness (",csite$All.Data$NAPL.Units,")",sep=""), side=4,line=2.75,cex=.7,col="black")
      
    }
    
    
  }
    }
  }
  
  par(op)
  
}


# #' @import officer
# makeTimeSeriesPPT_officer <- function(file, csite, substance, location, width = 7, height = 5){
# 
#   # Create png file.
#   mytemp <- tempfile(fileext = ".wmf")
# 
#   png(mytemp, width = width, height = height)
#   plotTimeSeries(csite, substance, location)
#   dev.off()
# 
#   my_pres <- officer::read_pptx()
# 
#   my_pres <- my_pres %>%
#     officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
#     officer::ph_with_img(type = "body", index = 1, src = mytemp, height = 1.06, width = 1.39 )
# 
#   print(my_pres, target = file) %>% invisible()
# 
#   try(file.remove(mytemp))
# 
# }


makeTimeSeriesPPT <- function(csite, fileout, substance, location, show_thresh,timepoint,width = 600, height = 400){
  
  # Initialize Powerpoint file.
  if (is.null(ppt_pres <- initPPT())) {
    return(NULL)
  }
  
  # Create temporary wmf file. 
  mytemp <- tempfile(fileext = ".png")
  
  png(mytemp, width = width, height = height) 
  plotTimeSeries(csite=csite, substance=substance, location=location,show_thresh=show_thresh,timepoint=timepoint)
  dev.off()
  
  ppt_pres <- addPlotPPT(mytemp, ppt_pres, width, height) 
  
  print(ppt_pres, target = fileout) %>% invisible()
  
  try(file.remove(mytemp))
  
}




makeTimeSeriesAnimationPPT <- function(csite, fileout, substance, location, Layout,show_thresh,timepoint,width = 600, height = 400){
  
  # Initialize Powerpoint file.
  if (is.null(ppt_pres <- initPPT())) {
    return(NULL)
  }
  
  # Create temporary wmf file. 
  # Loop over each time step.. 
  progress <- shiny::Progress$new()
  progress$set(message = "Generating Powerpoint: ", value = 0)
  on.exit(progress$close())
  
  All.Wells<-location
 
  Layout= switch(Layout,"1x1"=c(1,1),"2x1"=c(2,1),"1x2"=c(1,2),"2x2"=c(2,2))
  
  for (i in 1:length(All.Wells)) {
    
  progress$set(value = i/length(All.Wells), detail = paste0("Slide ", i))
  
  if(i %in% seq(1,length(All.Wells),by= Layout[1] * Layout[2])){
    mytemp <- tempfile(fileext = ".png")
    png(mytemp, width = width, height = height) 
    par(mfrow=Layout)
  }
    
  plotTimeSeries(csite=csite, substance =substance, location = All.Wells[i],show_thresh = show_thresh,timepoint=timepoint,modify_plot_layout=FALSE)
  
  if(i %in% c(seq(Layout[1] * Layout[2],max(Layout[1] * Layout[2],length(All.Wells)),by=Layout[1] * Layout[2]),length(All.Wells))){
    dev.off()
    ppt_pres <- addPlotPPT(mytemp, ppt_pres, width, height) 
    print(ppt_pres, target = fileout) %>% invisible()
    try(file.remove(mytemp))
  }
  
  
  }
  
}

















