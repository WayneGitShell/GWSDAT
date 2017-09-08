


plotSpatialImage <- function(csite, substance, timepoint = NULL) {
  
  if (is.null(timepoint) || class(timepoint) != "Date")
    stop("Need to specify valid timepoint of class \"Date\".")
  
  # make interpolation
  interp.pred <- interpConc(csite, substance, timepoint)
  
  
  
  # Create plume statistics if needed.
  plume_stats <- NULL
  if (csite$ui_attr$spatial_options["Plume Diagnostics"]) {

    plume_stats <- getPlumeStats(csite, substance, timepoint, interp.pred$data, 
                                csite$ui_attr$plume_thresh[substance], 
                                csite$ui_attr$ground_porosity)
  }
  
  plotSpatialImage_main(csite, substance, timepoint, interp.pred, plume_stats)

}
  




plotSpatialImage_main <- function(csite, substance = " ", timepoint = NULL, 
                                  pred = NULL, plume_stats = NULL) { 
  
  interp.pred  <- pred$data
  Do.Image     <- pred$Do.Image
  Contour.xlim <- pred$Contour.xlim
  Contour.ylim <- pred$Contour.ylim
  
  if (csite$ui_attr$spatial_options["Plume Diagnostics"]) {
    op <- par(mar = c(3,4.1,2,2.1))
  }else{
    op <- par(mar = c(2,4.1,2,2.1))
  }

  
  Col.Option <- csite$ui_attr$spatial_options["Scale colours to Data"]
  Show.Values <- csite$ui_attr$spatial_options["Show Conc. Values"]
  Show.GW.Contour <- csite$ui_attr$spatial_options["Show GW Contour"]
  
  if (is.null(Show.GW.Contour) || length(Show.GW.Contour) == 0 || is.na(Show.GW.Contour)) {
    Show.GW.Contour <- FALSE
  }
  
  Show.Well.Labels <- csite$ui_attr$spatial_options["Show Well Labels"]
  
  Show.ShapeFile <- FALSE
  if ("Overlay ShapeFiles" %in% names(csite$ui_attr$spatial_options))
    Show.ShapeFile <- csite$ui_attr$spatial_options["Overlay ShapeFiles"]

  Well.Coords <- csite$All.Data$sample_loc$data

  temp.time.frac <- as.numeric(timepoint - min(csite$Fitted.Data[[substance]]$Time.Eval))/as.numeric(diff(range(csite$Fitted.Data[[substance]]$Time.Eval)))
  
  try(if (temp.time.frac == 1) {temp.time.frac = .999}) # to avoid plot issue with wmf format!
  try(if (temp.time.frac == 0) {temp.time.frac = .001})
  try(if (as.numeric(diff(range(csite$Fitted.Data[[substance]]$Time.Eval))) == 0 || is.nan(temp.time.frac)) {temp.time.frac = .999}) # Handle case when only one time point.
  

  
  date.to.print <-  format(timepoint, "%d-%b-%Y")
  
  if (csite$GWSDAT_Options$Aggby %in% c("Monthly","Quarterly")) {
    
    if (csite$GWSDAT_Options$Aggby == "Monthly") {
        date.range.to.print <- seq.Date(timepoint, by = "-1 month", length.out = 2)
    } else {
        date.range.to.print <- seq.Date(timepoint, by = "-3 month", length.out = 2)
    }	
    
    date.range.to.print[2] <- date.range.to.print[2] + 1
    date.range.to.print <- format(date.range.to.print,"%d-%b-%Y")[c(2,1)]
    date.to.print <- paste(date.range.to.print,collapse = " to ")
    
  }

  
  
  
  model.tune <- csite$Fitted.Data[[substance]][["Model.tune"]]
  temp.Cont.Data <- csite$Fitted.Data[[substance]]$Cont.Data
  temp.Cont.Data <- temp.Cont.Data[temp.Cont.Data$AggDate == timepoint,]
  temp.Cont.Data$log.Resid <- log(temp.Cont.Data$Result.Corr.ND) - log(temp.Cont.Data$ModelPred)
  
  if (csite$ui_attr$conc_unit_selected == "mg/l") {
    
    temp.Cont.Data$Result.Corr.ND <- temp.Cont.Data$Result.Corr.ND/1000
    temp.res <- as.character(temp.Cont.Data$Result)
    temp.res <- gsub("ND<","",temp.res)
    temp.res[tolower(temp.res) != "napl"] <- as.character(as.numeric(temp.res[tolower(temp.res) != "napl"])/1000)
    temp.res[temp.Cont.Data$ND] <- paste("ND<",temp.res[temp.Cont.Data$ND],sep = "")
    temp.Cont.Data$Result <- temp.res
    rm(temp.res)
  }
  
  if (csite$ui_attr$conc_unit_selected == "ng/l") {
    
    temp.Cont.Data$Result.Corr.ND <- temp.Cont.Data$Result.Corr.ND*1000
    temp.res <- as.character(temp.Cont.Data$Result)
    temp.res <- gsub("ND<","",temp.res)
    temp.res[tolower(temp.res) != "napl"] <- as.character(as.numeric(temp.res[tolower(temp.res) != "napl"])*1000)
    temp.res[temp.Cont.Data$ND] <- paste("ND<",temp.res[temp.Cont.Data$ND],sep = "")
    temp.Cont.Data$Result <- temp.res
    rm(temp.res)
  }
  

  #
  # Identify "Bad" Wells. 
  #
  Bad.Wells <- as.character(temp.Cont.Data$WellName[which(temp.Cont.Data$log.Resid > 1.75)])
  Bad.Wells <- Well.Coords[Well.Coords$WellName %in% Bad.Wells,]
  if (nrow(Bad.Wells) > 0) {Bad.Wells$WellName <- paste("<",Bad.Wells$WellName,">",sep = "")}
  
  
  
  lev_cut <- csite$ui_attr$lev_cut
  if (csite$ui_attr$pred_interval == "% sd") {
    lev_cut <- csite$ui_attr$sd_lev_cut
  } else {
  
    if (csite$ui_attr$conc_unit_selected == "mg/l") {lev_cut <- lev_cut/10}
    if (csite$ui_attr$conc_unit_selected == "ng/l") {lev_cut <- lev_cut*10}
    
  } 
  
  n.col <- length(lev_cut) - 1 #should be n.col-1
  
  
  GWFlows <- attr(csite$Fitted.Data,"GWFlows")
  if (!inherits(GWFlows, "try-error")) {
    
    
    temp.GW.Flows <- GWFlows[as.numeric(GWFlows$AggDate) == timepoint,]
    
    if (!is.null(csite$ui_attr$gw_selected) && csite$ui_attr$gw_selected != "None") {
      
      L <- 0.05 * sqrt(diff(Contour.xlim)^2 + diff(Contour.ylim)^2)
      
      
      GWFlows <- attr(csite$Fitted.Data,"GWFlows")
      
      
      if(!is.null(GWFlows)){
        
        
        if(nrow(temp.GW.Flows) > 0) {
          
          
          x0 = temp.GW.Flows$XCoord
          y0 = temp.GW.Flows$YCoord
          
          if(csite$ui_attr$gw_selected != "Same Length"){
            
            x1 = temp.GW.Flows$XCoord + L*temp.GW.Flows$R*cos(temp.GW.Flows$RAD)
            y1 = temp.GW.Flows$YCoord + L*temp.GW.Flows$R*sin(temp.GW.Flows$RAD)
            
          }
          else{
            
            x1 = temp.GW.Flows$XCoord + .65*L*cos(temp.GW.Flows$RAD)
            y1 = temp.GW.Flows$YCoord + .65*L*sin(temp.GW.Flows$RAD)
            
          }
          
          
          
          
        }else{
          
          Show.GW.Contour<-FALSE
          
        }
        
        
      }
    }
    
  }#if not try error!
  

 
  
  GWSDAT.Terrain <- function(n){
    
    terrain.colors(n + 1)[1:n] # to avoid white in extreme conc.
  }
  
  
  if(csite$ui_attr$contour_selected == "Conc-Terrain" & Col.Option){
    col.palette <- terrain.colors
  }
  if(csite$ui_attr$contour_selected=="Conc-Terrain" & !Col.Option){
    col.palette <- GWSDAT.Terrain(n.col)
  }
  if(csite$ui_attr$contour_selected=="Conc-Topo" & Col.Option){
    col.palette <- topo.colors
  }
  if(csite$ui_attr$contour_selected=="Conc-Topo" & !Col.Option){
    col.palette <- topo.colors(n.col)
  }
  if(csite$ui_attr$contour_selected=="Conc-GreyScale" & Col.Option){
    col.palette <- GWSDAT.GrayScale
  }
  if(csite$ui_attr$contour_selected=="Conc-GreyScale" & !Col.Option){
    col.palette <- GWSDAT.GrayScale(n.col)
  }
  
  
  
  
  if(csite$ui_attr$contour_selected == "Conc-Terrain-Circles"){
    
    col.palette <- GWSDAT.Terrain(n.col)
    Do.Image<-FALSE
    interp.pred$z[,]<-NA
    my.palette<-col.palette[(as.numeric(cut(temp.Cont.Data$Result.Corr.ND,breaks=lev_cut)))]
    #my.cex<-.5*as.numeric(cut(temp.Cont.Data$Result.Corr.ND,breaks=my.lev_cut))
    my.cex<-.8*log(temp.Cont.Data$Result.Corr.ND)
    if(csite$ui_attr$conc_unit_selected=="mg/l"){my.cex<-.5*log(1000*temp.Cont.Data$Result.Corr.ND)}
    if(csite$ui_attr$conc_unit_selected=="ng/l"){my.cex<-.5*log(0.001*temp.Cont.Data$Result.Corr.ND)}
    
    
    my.cex[my.cex<1.5]<-1.5
    
  }
  
  
  if(csite$ui_attr$contour_selected=="Conc-Topo-Circles"){
    
    col.palette <- topo.colors(n.col)
    Do.Image<-FALSE
    interp.pred$z[,]<-NA
    my.palette<-col.palette[(as.numeric(cut(temp.Cont.Data$Result.Corr.ND,breaks=csite$lev_cut)))]
    #my.cex<-.5*as.numeric(cut(temp.Cont.Data$Result.Corr.ND,breaks=my.lev_cut))
    #my.cex<-if(csite$ui_attr$conc_unit_selected=="mg/l"){.5*log(1000*temp.Cont.Data$Result.Corr.ND)}else{.8*log(temp.Cont.Data$Result.Corr.ND)}
    
    my.cex<-.8*log(temp.Cont.Data$Result.Corr.ND)
    if(csite$ui_attr$conc_unit_selected=="mg/l"){my.cex<-.5*log(1000*temp.Cont.Data$Result.Corr.ND)}
    if(csite$ui_attr$conc_unit_selected=="ng/l"){my.cex<-.5*log(0.001*temp.Cont.Data$Result.Corr.ND)}
    
    
    my.cex[my.cex<1.5]<-1.5
    
  }
  
  if(csite$ui_attr$contour_selected=="Conc-GreyScale-Circles"){
    
    col.palette <- GWSDAT.GrayScale(n.col)
    Do.Image<-FALSE
    interp.pred$z[,]<-NA
    my.palette<-col.palette[(as.numeric(cut(temp.Cont.Data$Result.Corr.ND,breaks=lev_cut)))]
    #my.cex<-.5*as.numeric(cut(temp.Cont.Data$Result.Corr.ND,breaks=my.lev_cut))
    #my.cex<-if(csite$ui_attr$conc_unit_selected=="mg/l"){.5*log(1000*temp.Cont.Data$Result.Corr.ND)}else{.8*log(temp.Cont.Data$Result.Corr.ND)}
    
    my.cex<-.8*log(temp.Cont.Data$Result.Corr.ND)
    if(csite$ui_attr$conc_unit_selected=="mg/l"){my.cex<-.5*log(1000*temp.Cont.Data$Result.Corr.ND)}
    if(csite$ui_attr$conc_unit_selected=="ng/l"){my.cex<-.5*log(0.001*temp.Cont.Data$Result.Corr.ND)}
    
    my.cex[my.cex<1.5]<-1.5
    
  }
  
  if (csite$ui_attr$contour_selected == "NAPL-Circles") {
    
    Do.Image <- FALSE
    interp.pred$z[,] <- NA
    NAPL.Thickness.Data <- csite$All.Data$NAPL.Thickness.Data
    temp.NAPL.Data <- NAPL.Thickness.Data[NAPL.Thickness.Data$AggDate == timepoint,]
    
    lev_cut <- attributes(NAPL.Thickness.Data)$lev_cuts
    NAPL.Wells <- attributes(NAPL.Thickness.Data)$NAPL.Wells
    col.palette <- heat.colors(length(lev_cut) - 1)
    
    
    my.palette <- col.palette[(as.numeric(cut(temp.NAPL.Data$Result.Corr.ND, breaks = attributes(NAPL.Thickness.Data)$lev_cuts)))]
    my.palette <- col.palette[(as.numeric(cut(temp.NAPL.Data$Result.Corr.ND, breaks = attributes(NAPL.Thickness.Data)$lev_cuts)))]
    my.cex <- 1.5 + (as.numeric(cut(temp.NAPL.Data$Result.Corr.ND, breaks = attributes(NAPL.Thickness.Data)$lev_cuts)))/2
    
  }
  
  
  if (!Col.Option || !Do.Image) {
    
    tmp_main <- paste(substance,
                    if (csite$ui_attr$contour_selected == "NAPL-Circles" & substance != " ") {paste("(",csite$ui_attr$conc_unit_selected,")", sep = "")} else {""},
                    if (substance != " ") {":"} else {""},
                    date.to.print,
                    if (csite$Aquifer != "") {paste(": Aquifer-",csite$Aquifer, sep = "")} else {""}
              )
    
    
    

    ktitle <- title(main = csite$ui_attr$conc_unit_selected)
    if (csite$ui_attr$contour_selected == "NAPL-Circles") 
        title(main = paste("NAPL \nThickness \n(", csite$All.Data$NAPL.Units, ")", sep = ""), cex.main = 0.7)
        

    

      
    plotFilledContour(interp.pred, asp = 1, 
                      ShapeFiles = if (Show.ShapeFile) {csite$All.Data$ShapeFiles} else {NULL},
                      fixedConcScale = if (csite$ui_attr$contour_selected == "NAPL-Circles") {FALSE} else {TRUE},
                      xlim   = Contour.xlim,
                      ylim   = Contour.ylim,
                      levels = lev_cut,
                      col    = col.palette,
                      plot.title = title(main = tmp_main, xlab = "", ylab = "", cex.main = .95),
                      key.title  = if (csite$ui_attr$contour_selected == "NAPL-Circles") {title(main = paste("NAPL \nThickness \n(", csite$All.Data$NAPL.Units, ")", sep = ""), cex.main = 0.7)} else {title(main = csite$ui_attr$conc_unit_selected)},
                          
                      plot.axes  = {axis(1);
          axis(2, las = 3); axis(3, at = par("usr")[1]+temp.time.frac*(diff(range(par("usr")[1:2]))),labels="",col="red",lwd=3,tck=-0.02); 
        if (csite$ui_attr$contour_selected == "Conc-Terrain-Circles" || csite$ui_attr$contour_selected=="Conc-Topo-Circles" || csite$ui_attr$contour_selected=="Conc-GreyScale-Circles"){points(temp.Cont.Data$XCoord[order(my.cex,decreasing=T)],temp.Cont.Data$YCoord[order(my.cex,decreasing=T)],pch=19,col=my.palette[order(my.cex,decreasing=T)],cex=my.cex[order(my.cex,decreasing=T)])}

        if (csite$ui_attr$contour_selected == "Conc-Terrain-Circles" || csite$ui_attr$contour_selected=="Conc-Topo-Circles" || csite$ui_attr$contour_selected=="Conc-GreyScale-Circles") {points(temp.Cont.Data$XCoord[order(my.cex,decreasing=T)],temp.Cont.Data$YCoord[order(my.cex,decreasing=T)],pch=1,col=1, cex = my.cex[order(my.cex,decreasing = T)])}                     
                            
        if (csite$ui_attr$contour_selected == "NAPL-Circles") {points(temp.NAPL.Data$XCoord[order(my.cex,decreasing=T)],temp.NAPL.Data$YCoord[order(my.cex,decreasing=T)],pch=19,col=my.palette[order(my.cex,decreasing=T)],cex=my.cex[order(my.cex,decreasing=T)])}

        if (csite$ui_attr$contour_selected == "NAPL-Circles") {points(temp.NAPL.Data$XCoord,temp.NAPL.Data$YCoord,pch=1,col=1,cex=my.cex)}
                            
        points(Well.Coords$XCoord,Well.Coords$YCoord,pch=19,cex=.7);
                            
        if (csite$ui_attr$contour_selected == "NAPL-Circles") {
            points(Well.Coords[as.character(Well.Coords$WellName) %in% attributes(NAPL.Thickness.Data)$NAPL.Wells,c("XCoord","YCoord")],col="red",pch=19,cex=0.7)
        }
                            
        if (Show.Well.Labels)text(Well.Coords$XCoord,Well.Coords$YCoord,Well.Coords$WellName,cex=0.75,pos=1)
                            
        if (Show.GW.Contour) {
            contour(GWSDAT.GW.Contour(temp.GW.Flows), add = T, labcex = .8)
        }

        if (Show.Values & length(as.character(temp.Cont.Data$Result))>0)try(text(temp.Cont.Data$XCoord,temp.Cont.Data$YCoord,as.character(temp.Cont.Data$Result),
                                                                                 cex=0.75,col=c("red","black")[as.numeric(temp.Cont.Data$ND)+1],pos=3),silent=T)
                            

        if (!is.null(plume_stats) & csite$ui_attr$spatial_options["Plume Diagnostics"]) {
                              
            try(contour(interp.pred, levels = plume_stats$conc_thresh, add = T, col = "red", lwd = 2, labcex = .8))
                              
            try(points(plume_stats$mass_centre_x, plume_stats$mass_centre_y, cex = 1.3, pch = 3, lwd = 2, col = "red"))
        }
                        
                            
                            
        if(nrow(Bad.Wells)>0 & Show.Well.Labels & Do.Image){text(Bad.Wells$XCoord,Bad.Wells$YCoord,Bad.Wells$WellName,cex=0.75,col="red",pos=1)}
        try(arrows(x0, y0, x1, y1, length = 0.1,lwd=2,col="blue"), silent = TRUE)
        
    }
                          
    )

    if (csite$ui_attr$spatial_options["Plume Diagnostics"]) {
      
      tempUnitHandle <- PlumeUnitHandlingFunc(csite$GWSDAT_Options$WellCoordsLengthUnits, csite$ui_attr$conc_unit_selected, plume_stats$mass, plume_stats$area)
      
      tp <- paste("Plume Mass=", signif(tempUnitHandle$PlumeMass,5),tempUnitHandle$PlumeMassUnits,";  Plume Area=",signif(tempUnitHandle$PlumeArea,5),tempUnitHandle$PlumeAreaUnits,sep = "")
      mtext(tp,side = 1,adj = -0.1, line = 2,cex = 0.85)
      
    }
    
    
  } else {
    
    
    plotFilledContour(interp.pred,
                      asp = 1,
                      ShapeFiles = if(Show.ShapeFile){csite$All.Data$ShapeFiles}else{NULL},
                      xlim = Contour.xlim,
                      ylim = Contour.ylim,
                      color.palette = col.palette,
                      plot.title = title(main = paste(substance,":",date.to.print,if(csite$Aquifer != ""){paste(": Aquifer-",csite$Aquifer, sep="")}else{""}),xlab = "", ylab = "",cex.main=.95),key.title = title(main=csite$ui_attr$conc_unit_selected),
                          plot.axes={ axis(1); axis(2,las=3);axis(3,at=par("usr")[1]+temp.time.frac*(diff(range(par("usr")[1:2]))),labels = "",col = "red",lwd=3,tck=-0.02);  
                            points(Well.Coords$XCoord,Well.Coords$YCoord,pch=19,cex=1.0);
                            if(Show.Well.Labels)text(Well.Coords$XCoord,Well.Coords$YCoord,Well.Coords$WellName,cex=0.75,pos=1)
                            
                            if (Show.GW.Contour)try(contour(GWSDAT.GW.Contour(temp.GW.Flows),add=T,labcex=.8),silent=T)
                            if (Show.Values & length(as.character(temp.Cont.Data$Result)) > 0) try(text(temp.Cont.Data$XCoord,temp.Cont.Data$YCoord,as.character(temp.Cont.Data$Result),
                                                                                                    cex=0.75,col=c("red","black")[as.numeric(temp.Cont.Data$ND)+1],pos=3),silent=T)
                            
                            
                            if (!is.null(plume_stats) & csite$ui_attr$spatial_options["Plume Diagnostics"]) {
                              
                              contour(interp.pred, levels = plume_stats$conc_thresh, add = T, col = "red", lwd = 2, labcex = .8)
                                  
                              points(plume_stats$mass_centre_x, plume_stats$mass_centre_y, cex = 1.3, pch = 3, lwd = 2, col = "red")
                            }
                            #--------------------------------------#
                            
                            if (nrow(Bad.Wells) > 0 & Show.Well.Labels & Do.Image) {text(Bad.Wells$XCoord,Bad.Wells$YCoord,Bad.Wells$WellName,cex=0.75,col="red",pos=1)}
                            try(arrows(x0, y0, x1, y1, length = 0.1,lwd = 2, col = "blue"), silent = TRUE)
                            
                          }
                          
    )
    
    if (csite$ui_attr$spatial_options["Plume Diagnostics"]) {
      
      tempUnitHandle <- PlumeUnitHandlingFunc(csite$GWSDAT_Options$WellCoordsLengthUnits,csite$ui_attr$conc_unit_selected,plume_stats$mass,plume_stats$area)
      
        tp <- paste("Plume Mass=",signif(tempUnitHandle$PlumeMass,5),tempUnitHandle$PlumeMassUnits,";  Plume Area=",signif(tempUnitHandle$PlumeArea,5),tempUnitHandle$PlumeAreaUnits,sep = "")
      mtext(tp,side = 1,adj = -0.1,line = 2, cex = 0.85)
      
    }
    
    
  }
    
  #par(op)
  #return(csite)
  
  
}


plotSpatialImagePPT <- function(csite, substance, timepoint,
                           width = 7, height = 5){
 
  # Create temporary wmf file. 
  mytemp <- tempfile(fileext = ".wmf")
  
  win.metafile(mytemp, width = width, height = height) 
  plotSpatialImage(csite, substance, timepoint)
  dev.off()
   
  # Put into powerpoint slide.
  AddPlotPPV2(mytemp, width, height) 
  
  try(file.remove(mytemp))
  
    
}



makeSpatialAnimation <- function(csite, substance,
                                 width = 7,
                                 height = 5,
                                 width_plume = 9, 
                                 height_plume = 5) {
  
  full_plume_stats <- NULL 
 
  # Loop over each time step.. 
  for (timepoint in csite$All.Data$All.Agg.Dates) {
 
    # Do the interpolation.
    interp.pred <- interpConc(csite, substance, timepoint)
    
    # Create plume statistics if needed.
    #
    # Note: This is a duplicate from function getFullPlumeStats(). It could be called 
    #       separately and before plotSpatialImage_main(). However, both functions
    #       depend on interpConc() and I don't like to call it twice.
    #       Fixme: Call interpConc() separately, and pass results for each timepoint
    #              to getPlumeStats() and plotSpatialImage_main().
    #
    plume_stats <- NULL
    if (csite$ui_attr$spatial_options["Plume Diagnostics"]) {
      
      plume_stats <- getPlumeStats(csite, substance, timepoint, interp.pred$data, 
                                   csite$ui_attr$plume_thresh[substance], 
                                   csite$ui_attr$ground_porosity)
    
      # Add date. 
      plume_stats = cbind(plume_stats, "Agg.Date" = timepoint)
      
      # Append to full plume stats table.
      if (is.null(full_plume_stats))
        full_plume_stats <- plume_stats
      else
        full_plume_stats <- rbind(full_plume_stats, plume_stats)
      
    }

    # Make the plot and save to ppt
    mytemp <- tempfile(fileext = ".wmf")
  
    win.metafile(mytemp, width = width, height = height) 
    plotSpatialImage_main(csite, substance, timepoint, interp.pred, plume_stats)
    dev.off()
    
    AddPlotPPV2(mytemp, width, height) 
    
    
  } # end of for
  
  
  # Add slide with plume statistics.
  if (csite$ui_attr$spatial_options["Plume Diagnostics"]) {
    win.metafile(mytemp, width = width_plume, height = height_plume) 
    plotPlumeTimeSeries(full_plume_stats)
    dev.off()
    
    AddPlotPPV2(mytemp, width = width_plume, height = height_plume)
  }
  
  try(file.remove(mytemp))
}






GWSDAT.GW.Contour <- function(temp.GW.Flows){
  
  options(warn = -1)
  
  my.lo <- try(loess(Result ~ XCoord + YCoord, 
                     temp.GW.Flows, 
                     span = 1, 
                     degree = if (nrow(temp.GW.Flows) < 20) {1} else {2},
                     control = loess.control(surface = c("interpolate", "direct")[1])),
               silent = T)
  
  if (inherits(my.lo, "try-error")) {options(warn = 0); stop("Unable to fit loess")}
  options(warn = 0)
  xo=seq(min(temp.GW.Flows$XCoord),max(temp.GW.Flows$XCoord),l=40)
  yo=seq(min(temp.GW.Flows$YCoord),max(temp.GW.Flows$YCoord),l=40)
  my.df<-expand.grid(XCoord=xo,YCoord=yo)
  
  lo.pred  <-predict(my.lo,my.df)
  my.hull  <-temp.GW.Flows[chull(temp.GW.Flows[,c("XCoord","YCoord")]),c("XCoord","YCoord")]
  temp.pip <- sp::point.in.polygon(my.df$XCoord,my.df$YCoord,my.hull$XCoor,my.hull$YCoor)==0
  lo.pred[matrix(temp.pip,nrow=length(xo))]<-NA
  return(list(x=xo,y=yo,z=lo.pred))
  
}

GWSDAT.GrayScale <- function(n){
  
  rev(grey(seq(0,1,length = n + 3)))[c(-1,-2,-(n+3))]
  
}





plotFilledContour <- function(x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
                              z, xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE),
                              zlim = range(z, finite = TRUE), levels = pretty(zlim, nlevels),
                              nlevels = 20, color.palette = cm.colors, col = color.palette(length(levels) - 1),
                              plot.title, plot.axes, key.title, key.axes, asp = NA,
                              xaxs = "i", yaxs = "i", las = 1, axes = TRUE, frame.plot = axes, 
                              ShapeFiles = NULL, fixedConcScale = FALSE, PlumeDetails=NULL, ...) {
  

  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq(0, 1, len = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  
  
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  on.exit(par(par.orig))
  w <- (3 + mar.orig[2]) * par("csi") * 2.54
  layout(matrix(c(2, 1), ncol = 2), widths = c(1, lcm(w)))
  par(las = las)
  mar <- mar.orig
  mar[4] <- mar[2]
  mar[2] <- 1
  par(mar = mar)
  plot.new()
  
  plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", yaxs = "i")
  #rect(0, levels[-length(levels)], 1, levels[-1], col = col)
  equal.cuts<-seq(range(levels)[1],range(levels)[2],length=length(levels))#GWSDAT Change!
  rect(0, equal.cuts[-length(equal.cuts)],1,equal.cuts[-1],col = col) #GWSDAT Change!
  
  if(fixedConcScale){#GWSDAT Change
    
    my.at<-equal.cuts[-length(equal.cuts)]
    my.at<-c(my.at,0.4*equal.cuts[length(equal.cuts)-1]+.6*equal.cuts[length(equal.cuts)])
    my.labs<-paste(" ",levels[-length(levels)],sep="")
    my.labs<-c(my.labs,paste(">",levels[length(levels)-1],sep=""))
    #my.labs<-c(my.labs,paste(levels[length(levels)-1],"+",sep=""))
    
  } else {
    
    my.at = equal.cuts
    my.labs <- as.numeric(levels)
    
  }
  
  
  axis(side = 4, at = my.at, labels = my.labs)
  
  
  if (missing(key.axes)) {
    if (axes) 
      NULL 
  }
  else key.axes
  graphics::box()
  if (!missing(key.title)) 
    key.title
  mar <- mar.orig
  mar[4] <- 1
  par(mar = mar)
  plot.new()
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
    stop("no proper 'z' matrix specified")
  if (!is.double(z)) 
    storage.mode(z) <- "double"
  
  .filled.contour(x, y, z, levels, col)

  
  ################## ShapeFile Plotting ########################################
  
  if (!is.null(ShapeFiles)) {
    for (i in 1:length(ShapeFiles)) {
      try(GWSDAT.PlotShapeFile(ShapeFiles[[i]]))
    }
  }
  
  
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  } else plot.axes
  
  if (frame.plot) 
    graphics::box()
  
  if (missing(plot.title)) 
    title(...)
  else plot.title
  
  invisible()
}


