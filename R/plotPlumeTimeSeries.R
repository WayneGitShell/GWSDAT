

# Display the time-series data of a plume. 
#
# @param plume_stats Plume statistics data frame.
# 
# @export
#' @importFrom Kendall Kendall
plotPlumeTimeSeries <- function(plume_stats,ImplementReducedWellSet) {
  

   if(ImplementReducedWellSet & is.null(plume_stats$plume_statsreducedWellSet)){
     showNotification("No plume statistics detected for Reduced Well Data set. Please update the model.")
     }
  
  if(ImplementReducedWellSet){plume_statsreducedWellSet<-plume_stats$plume_statsreducedWellSet}else{plume_statsreducedWellSet<-NULL}
  plume_stats<-plume_stats$plume_stats
  
  if (is.null(plume_stats))
    return(NULL)
  
  if (all(is.na(plume_stats$mass))) 
    return(NULL)
  
  substance <- plume_stats$substance[1]
  plume_thresh <- plume_stats$conc_thresh[1]
  ground_porosity <- plume_stats$ground_porosity[1]
  
  tempUnitHandle <- PlumeUnitHandlingFunc(plume_stats$coord_unit[1],
                                          plume_stats$conc_unit[1],
                                          plume_stats$mass,
                                          plume_stats$area)
  
  plume_stats$mass <- tempUnitHandle$PlumeMass
  plume_stats$area <- tempUnitHandle$PlumeArea

  par(mfrow = c(1,3), oma = c(1.5,0,0,0))
  my.ylim <- 1.2*range(plume_stats$mass, na.rm = T)
  my.ylab <- paste("Plume Mass", tempUnitHandle$PlumeMassUnits, sep = "")
  
  
  ####################### Plume Mass ####################################################################################################
  
  # Calculate plume mass

  if(ImplementReducedWellSet & !is.null(plume_statsreducedWellSet)){ 
    
    reducedWellSettempUnitHandle <- PlumeUnitHandlingFunc(plume_statsreducedWellSet$coord_unit[1],
                                            plume_statsreducedWellSet$conc_unit[1],
                                            plume_statsreducedWellSet$mass,
                                            plume_statsreducedWellSet$area)
    
    plume_statsreducedWellSet$mass <- reducedWellSettempUnitHandle$PlumeMass
    plume_statsreducedWellSet$area <- reducedWellSettempUnitHandle$PlumeArea
    my.ylim <- 1.2*range(plume_statsreducedWellSet$mass,plume_stats$mass, na.rm = T)
  }
  
  
  my.ylim[1]<-0
  
  plot(plume_stats$Agg.Date, plume_stats$mass,
       ylim = my.ylim,
       cex.main = 1.5,
       cex.axis=1.5,
       type = "b",
       main = paste(substance, "\nPlume Mass", sep = ""),
       xlab = "Date",
       cex.lab = 1.5,
       pch = 19,
       cex = 1.5,
       yaxs="i",
       ylab = my.ylab)
  
  if(ImplementReducedWellSet & !is.null(plume_statsreducedWellSet)){ 
  points(plume_statsreducedWellSet$Agg.Date, plume_statsreducedWellSet$mass,type="b",lty=1,pch = 19,cex = 1.5,col="green")
    legend("bottomleft",c("Full Data","Reduced Well Data"),pch=19,lty=1,col=c("black","green"))
  }
  
  
   mtext(paste("Plume Threshold = ", plume_thresh, plume_stats$conc_unit[1],", Ground Porosity = ", (ground_porosity * 100), "%", sep = ""),
        side = 3, line = -1.1, cex = 0.95)
  
  
  
  #----------------------------------------------------------------------------------------------------------------------------------------#
  
  
  ####################### Plume Area #######################################################################################################
  
  
  
  if(ImplementReducedWellSet & !is.null(plume_statsreducedWellSet)){ 
    my.ylim <- 1.2*range(plume_statsreducedWellSet$area,plume_stats$area,na.rm=T)
  }else{
    my.ylim <- 1.2*range(plume_stats$area,na.rm=T)
  }
  my.ylim[1]<-0
  
  my.ylab <- paste("Plume Area", tempUnitHandle$PlumeAreaUnits, sep = "")
  
  
  
  plot(plume_stats$Agg.Date, plume_stats$area,
       ylim = my.ylim, 
       cex.main = 1.5,
       cex.axis=1.5,
       type = "b",
       main = paste(substance, "\nPlume Area", sep = ""),
       xlab = "Date",
       cex.lab = 1.5,
       pch = 19, 
       cex = 1.5, 
       yaxs="i",
       ylab = my.ylab
      )
  
  if(ImplementReducedWellSet & !is.null(plume_statsreducedWellSet)){ 
     points(plume_statsreducedWellSet$Agg.Date, plume_statsreducedWellSet$area,type="b",lty=1,pch = 19,cex = 1.5,col="green")
     legend("bottomleft",c("Full Data","Reduced Well Data"),pch=19,lty=1,col=c("black","green"))
  }    
  
  try(mtext(paste("Plume Threshold = ", plume_thresh, plume_stats$conc_unit[1], sep = ""), side = 3, line = -1.1, cex = 0.95))
  
  
  
  
  ####################### Plume Average ####################################################################################################
  
  #my.ylim <- range(plume_stats$avg_conc, na.rm = T)
  if(!is.null(plume_statsreducedWellSet)){ 
    my.ylim <- 1.2*range(plume_statsreducedWellSet$avg_conc,plume_stats$avg_conc,na.rm=T)
  }else{
    my.ylim <- 1.2*range(plume_stats$avg_conc,na.rm=T)
  }
  my.ylim[1]<-0
  
  
  
  my.ylab <- paste("Concentration",tempUnitHandle$PlumeAverageUnits,sep = "")
  
  plot(plume_stats$Agg.Date, plume_stats$avg_conc,
           ylim = my.ylim,
           cex.main = 1.5,
           cex.axis=1.5,
           type = "b",
           main = paste(substance, "\nAverage Plume Concentration", sep = ""),
           xlab = "Date",
           cex.lab = 1.5,
           pch = 19,
           cex = 1.5,
           yaxs="i",
           ylab = my.ylab)
  
  
  if(!is.null(plume_statsreducedWellSet)){ 
    points(plume_statsreducedWellSet$Agg.Date, plume_statsreducedWellSet$avg_conc,type="b",lty=1,pch = 19,cex = 1.5,col="green")
    legend("bottomleft",c("Full Data","Reduced Well Data"),pch=19,lty=1,col=c("black","green"))
  }    
  
  
  try(mtext(paste("Plume Threshold = ", plume_thresh, plume_stats$conc_unit[1], sep = ""),side = 3, line = -1.1, cex = 0.95))
  
  
  
}



plotPlumeTimeSeriesPPT <- function(plume_stats,ImplementReducedWellSet, fileout, width = 800, height = 500){
  
  
  # Initialize Powerpoint file.
  if (is.null(ppt_pres <- initPPT())) {
    return(NULL)
  }
  
  # Create temporary wmf file. 
  mytemp <- tempfile(fileext = ".png")
  
 
  png(mytemp, width = width, height = height) 
  plotPlumeTimeSeries(plume_stats,ImplementReducedWellSet)
  dev.off()
 
  
  ppt_pres <- addPlotPPT(mytemp, ppt_pres, width, height) 
  
  print(ppt_pres, target = fileout) %>% invisible()
 
  try(file.remove(mytemp))
  
  
}


printPlumeStatsCSV <- function(plume_stats) {
  
  
  # Copy, in case it is passed by reference.
  stats_tbl <- plume_stats
  
  # Retrieve proper unit strings.
  tempUnitHandle <- PlumeUnitHandlingFunc(stats_tbl$coord_unit[1], 
                                          stats_tbl$conc_unit[1], 
                                          stats_tbl$mass, stats_tbl$area)
  
  stats_tbl$mass <- tempUnitHandle$PlumeMass
  stats_tbl$area <- tempUnitHandle$PlumeArea
  
  # Edit column names.
  names(stats_tbl)[names(stats_tbl) == "avg_conc"] <- paste("Plume Average Conc ", tempUnitHandle$PlumeAverageUnits, sep = "")
  names(stats_tbl)[names(stats_tbl) == "area"] <- paste("Plume Area ", tempUnitHandle$PlumeAreaUnits, sep = "")
  names(stats_tbl)[names(stats_tbl) == "mass"] <- paste("Plume Mass ", tempUnitHandle$PlumeMassUnits, sep = "")
  names(stats_tbl)[names(stats_tbl) == "mass_centre_x"] <- "Centre of Mass (x)"
  names(stats_tbl)[names(stats_tbl) == "mass_centre_y"] <- "Centre of Mass (y)"
  names(stats_tbl)[names(stats_tbl) == "volume"] <- "Volume"
  names(stats_tbl)[names(stats_tbl) == "conc_thresh"] <- paste0("Plume Threshold Conc (",stats_tbl$conc_unit[1],")")
  stats_tbl$ground_porosity<-100*stats_tbl$ground_porosity
  names(stats_tbl)[names(stats_tbl) == "ground_porosity"] <- "Ground Porosity (%)"
  
  
  
  # Remove some columns
  stats_tbl$conc_unit  <- NULL
  stats_tbl$coord_unit <- NULL
  
  
  return(stats_tbl)
  
}

#' @importFrom splancs gridpts
plotPlumeEst <- function(csite, substance, plume_thresh,UseReducedWellSet){
  
  
  #### Get Hull data points function ########
  getHullDataPoints <- function(myhull){
    
    myhull <- rbind(myhull, myhull[1, , drop = FALSE])
   
    Perimeters <- sqrt(apply((apply(myhull,2,diff)^2),1,sum))
    Npts <- 250
    Ptsperunitlength <- Npts/sum(Perimeters)
    Perimeter.Npts <- round(Ptsperunitlength*Perimeters,0)
    chullseglist <- list()
    
    for (i in 1:(nrow(myhull) - 1)) {
      
      # Sometimes the number of perimeter points is zero. This is caused
      # by identical coordinates in 'myhull'. If this happens, do not execute
      # approx() and just copy the single coordinate.
      if (Perimeter.Npts[i] != 0) {
      
        amh <- approx(myhull[i:(i + 1),], n = max(Perimeter.Npts[i],3))
        
      } else {
        # Copy the first coordinate (remaining should be identical).
        amh <- myhull[i,]
        colnames(amh) <- c("x", "y")
        rownames(amh) <- 1
      }
      
      chullseglist[[i]] <- as.data.frame(amh)
    }
    
    return(do.call("rbind", chullseglist))
  }
  
  
  temp.df <- data.frame(Time.Eval = csite$All.Data$All_Agg_Dates)
  temp.df$MaxConc <- rep(NA,nrow(temp.df))
  temp.df$MaxInteriorConc <- rep(NA,nrow(temp.df))
  
  if(UseReducedWellSet){
    model <- csite$Reduced.Fitted.Data[[substance]][["Model.tune"]]$best.model
  }else{
    model <- csite$Fitted.Data[[substance]][["Model.tune"]]$best.model
  }
  
  
  
  for (i in 1:length(csite$All.Data$All_Agg_Dates)) {
    
    temp.time.eval <- csite$All.Data$All_Agg_Dates[i]
    
    if(UseReducedWellSet){
      Good.Wells <- as.character(unique(csite$Reduced.Fitted.Data[[substance]]$Cont.Data[as.numeric(csite$Reduced.Fitted.Data[[substance]]$Cont.Data$AggDate) <= temp.time.eval,]$WellName))
      Good.Wells <- intersect(Good.Wells,as.character(unique(csite$Reduced.Fitted.Data[[substance]]$Cont.Data[as.numeric(csite$Reduced.Fitted.Data[[substance]]$Cont.Data$AggDate) >= temp.time.eval,]$WellName)))
    }else{
      Good.Wells <- as.character(unique(csite$Fitted.Data[[substance]]$Cont.Data[as.numeric(csite$Fitted.Data[[substance]]$Cont.Data$AggDate) <= temp.time.eval,]$WellName))
      Good.Wells <- intersect(Good.Wells,as.character(unique(csite$Fitted.Data[[substance]]$Cont.Data[as.numeric(csite$Fitted.Data[[substance]]$Cont.Data$AggDate) >= temp.time.eval,]$WellName)))
    }
    if (length(Good.Wells) > 2) {

      ### Calculate Max Conc on hull boundary
      my.area <- csite$All.Data$sample_loc$data[csite$All.Data$sample_loc$names %in% Good.Wells, c("XCoord","YCoord") ]

      myhull  <- my.area[chull(my.area),]

      # The hull points can become only one or two if the points in Good.Wells include duplicates.
      if (nrow(myhull) > 2) {
        
        hulldatapoints <- getHullDataPoints(myhull)

        my.df <- data.frame(XCoord = hulldatapoints$x,YCoord = hulldatapoints$y, AggDate = temp.time.eval)
        temp.df$MaxConc[i] = max(exp(predict(model, newdata = my.df)$predicted))
        ### Calculate Max Conc on interior points of hull.
        InteriorPoints <- splancs::gridpts(as.matrix(myhull), 200)
        my.df <- data.frame(XCoord = InteriorPoints[,1], YCoord = InteriorPoints[,2], AggDate = temp.time.eval)
        temp.df$MaxInteriorConc[i] = max(exp(predict(model, newdata = my.df)$predicted))
      }
    }
    
   
  }
  
  # To avoid trouble with NA entries use which:
  max_larger_row <- which(temp.df$MaxInteriorConc < temp.df$MaxConc)
  
  temp.df$MaxInteriorConc[max_larger_row] <- temp.df$MaxConc[max_larger_row]
  
  my.ylim = c(min(temp.df[,c("MaxInteriorConc","MaxConc")], na.rm = T), max(temp.df[,c("MaxInteriorConc","MaxConc")], na.rm = T))
  my.ylim[1]<-min(my.ylim[1],plume_thresh)
  my.ylim[2]<-max(my.ylim[2],plume_thresh)
  
 
  plot(MaxInteriorConc ~ Time.Eval, data = temp.df,
       log  = "y", type = "b", ylim = my.ylim, xlab = "Date", 
       ylab = "Concentration(ug/l)",
       main = paste0(substance, ": Estimated Plume Delineation Concentration Region"), 
       pch  = 19, cex.main = .85)
  
  lines(MaxConc ~ Time.Eval, data = temp.df, type = "b", pch = 19, col = "black")
  
  try(polygon(c(temp.df$Time.Eval, rev(temp.df$Time.Eval)), 
              c(temp.df$MaxInteriorConc, rev(temp.df$MaxConc)),col = "grey", border = NA))
  grid(NA,NULL,lwd = 1,lty = 1,equilogs = FALSE)
  abline(h = plume_thresh, col = "red", lwd = 2, lty = 2)
  if(UseReducedWellSet){mtext("Note: Well Redundancy Activated.",side = 3,adj = 0,line = 0,font=2,col=4)}
}
