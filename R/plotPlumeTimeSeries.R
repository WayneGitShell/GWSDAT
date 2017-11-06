

#' Display the time-series data of a plume. 
#'
#' @param plume_stats Plume statistics data frame.
#' 
#' @export
#' @importFrom Kendall Kendall
plotPlumeTimeSeries <- function(plume_stats) {
  
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

      
  # Calculate plume mass
  my.ylim <- range(plume_stats$mass, na.rm = T)
  
  lmmass <- try(lm(mass ~ Agg.Date, plume_stats, na.action = na.omit))
  
  if (!inherits(lmmass,"try-error")) {
    
    lm.eval.points <- na.omit(plume_stats[,c("Agg.Date", "mass")])
    lm.eval.points <- data.frame(Agg.Date=as.Date(seq(range(lm.eval.points$Agg.Date)[1],range(lm.eval.points$Agg.Date)[2],length=100)))
    lm.pred <- predict(lmmass,newdata=lm.eval.points,interval = "confidence")
    
    lm.eval.points$fit <- lm.pred[,"fit"]
    lm.eval.points$lwr <- lm.pred[,"lwr"]
    lm.eval.points$upr <- lm.pred[,"upr"]
    
    
    my.ylim <- c(min(c(lm.eval.points$lwr,my.ylim),na.rm=T),max(c(lm.eval.points$upr,my.ylim),na.rm=T))
    my.ylim[1] <- max(my.ylim[1],0)
    
  }
  
  
  my.ylab <- paste("Plume Mass", tempUnitHandle$PlumeMassUnits, sep = "")
  
  
  plot(plume_stats$Agg.Date, plume_stats$mass,
       ylim = my.ylim,
       cex.main = 1.3,
       type = "b",
       main = paste(substance, "\nPlume Mass", sep = ""),
       xlab = "Date",
       cex.lab = 1.4,
       pch = 19,
       cex = 1.5,
       ylab = my.ylab)
  
  mtext(paste("Plume Threshold = ", plume_thresh, "ug/l, Ground Porosity = ", (ground_porosity * 100), "%", sep = ""),
            side = 3, line = -1, cex = 0.75)
  
  
  if (!inherits(lmmass,"try-error")) {
    
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$fit,lwd=2,col="green"))
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$lwr,lwd=2,col="green",lty=2))
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$upr,lwd=2,col="green",lty=2))
    
    
  }
  
  Mann.testPlumeMass <- try(Kendall::Kendall(plume_stats$Agg.Date,plume_stats$mass))
  
  if(!inherits(Mann.testPlumeMass,"try-error")){
    
    temp.tex1<-paste("(Mann-Kendall P.Value=",format.pval(Mann.testPlumeMass$sl,digits=3,eps=0.01),")",sep="")
    #try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeMass$sl<0.05 & Mann.testPlumeMass$tau<0.0){"darkgreen"}else{"red"}))
    #try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeMass$sl<0.05){"darkgreen"}else{"red"}))
    
  }
  
  #----------------------------------------------------------------------------------------------------------------------------------------#
  
  
  ####################### Plume Area #######################################################################################################
  
  my.ylim <- range(plume_stats$area,na.rm=T)
  
  lmarea <- try(lm(area ~ Agg.Date, plume_stats, na.action=na.omit))
  
  if(!inherits(lmarea,"try-error")){
    
    lm.eval.points<-na.omit(plume_stats[,c("Agg.Date","area")])
    lm.eval.points<-data.frame(Agg.Date=as.Date(seq(range(lm.eval.points$Agg.Date)[1],range(lm.eval.points$Agg.Date)[2],length=100)))
    lm.pred<-predict(lmarea,newdata=lm.eval.points,interval="confidence")
    
    lm.eval.points$fit <- lm.pred[,"fit"]
    lm.eval.points$lwr <- lm.pred[,"lwr"]
    lm.eval.points$upr <- lm.pred[,"upr"]
    
    
    my.ylim <- c(min(c(lm.eval.points$lwr,my.ylim), na.rm = T), max(c(lm.eval.points$upr,my.ylim),na.rm = T))
    my.ylim[1] <- max(my.ylim[1],0)
    
  }
  my.ylab <- paste("Plume Area", tempUnitHandle$PlumeAreaUnits, sep = "")
  
  
  
  plot(plume_stats$Agg.Date, plume_stats$area,
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
  
  if (!inherits(lmarea,"try-error")) {
    
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$fit,lwd=2,col="green"))
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$lwr,lwd=2,col="green",lty=2))
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$upr,lwd=2,col="green",lty=2))
    
    
  }
  
  Mann.testPlumeArea <- try(Kendall::Kendall(plume_stats$Agg.Date, plume_stats$area))
  
  if (!inherits(Mann.testPlumeArea,"try-error")) {
    
    temp.tex1 <- paste("(Mann-Kendall P.Value=",format.pval(Mann.testPlumeArea$sl, digits = 3, eps = 0.01),")",sep="")
    #try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeArea$sl<0.05 & Mann.testPlumeArea$tau<0.0){"darkgreen"}else{"red"}))
    #try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeArea$sl<0.05){"darkgreen"}else{"red"}))
    
  }
  
  
  ####################### Plume Average ####################################################################################################
  
  my.ylim <- range(plume_stats$avg_conc, na.rm = T)
  
  lmavg_conc <- try(lm(avg_conc ~ Agg.Date, plume_stats, na.action = na.omit))
  
  if (!inherits(lmavg_conc,"try-error")) {
    
    lm.eval.points <- na.omit(plume_stats[,c("Agg.Date","avg_conc")])
    lm.eval.points <- data.frame(Agg.Date = as.Date(seq(range(lm.eval.points$Agg.Date)[1],range(lm.eval.points$Agg.Date)[2],length = 100)))
    lm.pred <- predict(lmavg_conc,newdata = lm.eval.points,interval = "confidence")
    
    lm.eval.points$fit <- lm.pred[,"fit"]
    lm.eval.points$lwr <- lm.pred[,"lwr"]
    lm.eval.points$upr <- lm.pred[,"upr"]
    
    
    my.ylim <- c(min(c(lm.eval.points$lwr,my.ylim), na.rm = T), max(c(lm.eval.points$upr,my.ylim),na.rm = T))
    my.ylim[1] <- max(my.ylim[1],0)
    
  }
  
  my.ylab <- paste("Concentration",tempUnitHandle$PlumeAverageUnits,sep = "")
  
  plot(plume_stats$Agg.Date, plume_stats$avg_conc,
           ylim = my.ylim,
           cex.main = 1.3,
           type = "b",
           main = paste(substance, "\nAverage Plume Concentration", sep = ""),
           xlab = "Date",
           cex.lab = 1.4,
           pch = 19,
           cex = 1.5,
           ylab = my.ylab)
  
  try(mtext(paste("Plume Threshold = ", plume_thresh, "ug/l", sep = ""),side = 3, line = -1, cex = 0.75))
  
  if (!inherits(lmavg_conc,"try-error")) {
    
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$fit,lwd=2,col="green"))
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$lwr,lwd=2,col="green",lty=2))
    #try(lines(lm.eval.points$Agg.Date,lm.eval.points$upr,lwd=2,col="green",lty=2))
    
    
  }
  
  Mann.testPlumeAverageConc <- try(Kendall::Kendall(plume_stats$Agg.Date, plume_stats$avg_conc))
  
  if(!inherits(Mann.testPlumeAverageConc,"try-error")){
    
    temp.tex1<-paste("(Mann-Kendall P.Value=",format.pval(Mann.testPlumeAverageConc$sl,digits=3,eps=0.01),")",sep="")
    #try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeAverageConc$sl<0.05 & Mann.testPlumeAverageConc$tau<0.0){"darkgreen"}else{"red"}))
    #try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeAverageConc$sl<0.05){"darkgreen"}else{"red"}))
    
  }

}



plotPlumeTimeSeriesPPT <- function(plume_stats, width = 9, height = 5){
  
  # Create temporary wmf file. 
  mytemp <- tempfile(fileext = ".wmf")
  
 
  win.metafile(mytemp, width = width, height = height) 
  plotPlumeTimeSeries(plume_stats)
  dev.off()
 
  
  # Put into powerpoint slide.
  if (is.null(ppt_lst <- initPPT())) {
    showNotification("Unable to initialize Powerpoint: package RDCOMClient might not be installed.", type = "error", duration = 10)
    return(NULL)
  }
  
  addPlotPPT(mytemp, ppt_lst, width, height) 
 
  try(file.remove(mytemp))
  
  
}


printPlumeStatsCSV <- function(plume_stats) {
  
  # Copy, in case it is passed by reference.
  stats_tbl <- plume_stats
  
  # Retrieve proper unit strings.
  tempUnitHandle <- PlumeUnitHandlingFunc(stats_tbl$coord_unit[1], 
                                          stats_tbl$conc_unit[1], 
                                          NaN, NaN)
  
  # Edit column names.
  names(stats_tbl)[names(stats_tbl) == "avg_conc"] <- paste("Plume Average Conc ", tempUnitHandle$PlumeAverageUnits, sep = "")
  names(stats_tbl)[names(stats_tbl) == "area"] <- paste("Plume Area ", tempUnitHandle$PlumeAreaUnits, sep = "")
  names(stats_tbl)[names(stats_tbl) == "mass"] <- paste("Plume Mass ", tempUnitHandle$PlumeMassUnits, sep = "")
  names(stats_tbl)[names(stats_tbl) == "mass_centre_x"] <- "Centre of Mass (x)"
  names(stats_tbl)[names(stats_tbl) == "mass_centre_y"] <- "Centre of Mass (y)"
  names(stats_tbl)[names(stats_tbl) == "volume"] <- "Volume"
  names(stats_tbl)[names(stats_tbl) == "conc_thresh"] <- "Plume Threshold Conc (ug/l)"
  names(stats_tbl)[names(stats_tbl) == "ground_porosity"] <- "Ground Porosity (%)"
  
  
  
  # Remove some columns
  stats_tbl$conc_unit  <- NULL
  stats_tbl$coord_unit <- NULL
  
  
  return(stats_tbl)
  
}

#' @importFrom splancs gridpts
plotPlumeEst <- function(csite, substance, plume_thresh){
  
  
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
  
  model <- csite$Fitted.Data[[substance]][["Model.tune"]]$best.model
  
  
  
  for (i in 1:length(csite$All.Data$All_Agg_Dates)) {
    
    temp.time.eval <- csite$All.Data$All_Agg_Dates[i]
    
    Good.Wells <- as.character(unique(csite$Fitted.Data[[substance]]$Cont.Data[as.numeric(csite$Fitted.Data[[substance]]$Cont.Data$AggDate) <= temp.time.eval,]$WellName))

    Good.Wells <- intersect(Good.Wells,as.character(unique(csite$Fitted.Data[[substance]]$Cont.Data[as.numeric(csite$Fitted.Data[[substance]]$Cont.Data$AggDate) >= temp.time.eval,]$WellName)))
    
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
 
}
