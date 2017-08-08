
getFullPlumeStats <- function(panel, substance, plume_thresh, ground_porosity) {

  full_plume_stats <- data.frame(Agg.Date = panel$All.Data$All.Agg.Dates)
  full_plume_stats$PlumeMass <- full_plume_stats$PlumeArea <- full_plume_stats$COMx <- full_plume_stats$COMy <- full_plume_stats$PlumeAverageConc <- rep(NA,nrow(full_plume_stats))


  for (i in 1:length(panel$All.Data$All.Agg.Dates)) {
    
    # Fixme: Last argument for interpData (Col.Option) and lev.cut inside interpData()
    #  both change interp.pred, how important is this? Can I get rid of it? 
    #  I would need a "Scale colours to Data".  
    
    interp.pred <- interpData(panel, substance, i, panel$ScaleCols["Scale colours to Data"])
    
    plume_stats <- getPlumeStats(panel, substance, timestep = i, interp.pred$data, plume_thresh, ground_porosity)
    
    full_plume_stats$PlumeArea[i] <- plume_stats$area
    full_plume_stats$PlumeMass[i] <- plume_stats$Mass
    full_plume_stats$COMx[i] <- plume_stats$PlumeCentreofMass[1]
    full_plume_stats$COMy[i] <- plume_stats$PlumeCentreofMass[2]
    full_plume_stats$PlumeAverageConc[i] <- plume_stats$volume / plume_stats$area
    
  }

  
  return(full_plume_stats)
}

#' Calculate the plume statistics including mass, area, center, and average concentration.
#'
#' @param panel
#' @param substance  
#' @param timestep
#' @param predicted_val
#'
#' @return
#' @export
#'
#' @examples
getPlumeStats <- function(panel, 
                          substance, 
                          timestep, 
                          predicted_val, 
                          plume_thresh, 
                          ground_porosity ) {
  
  if (panel$rgUnits == "mg/l") {plume_thresh <- plume_thresh/1000}
  if (panel$rgUnits == "ng/l") {plume_thresh <- plume_thresh*1000}
  
  cL <- contourLines(predicted_val, levels = plume_thresh)
  
  model.tune <- panel$Fitted.Data[[substance]][["Model.tune"]]
  temp.time.eval <- panel$Fitted.Data[[substance]]$Time.Eval[timestep]
  
  
  
  PlumeDetails = list()
  
  
  if (length(cL) > 0) {
    
    for (i in 1:length(cL)) {
      
      cL[[i]]$Closed <- checkPlumeClosure(cL[[i]])
      
      if (cL[[i]]$Closed) {
        
        cL[[i]]$area <- areapl(cbind(cL[[i]]$x,cL[[i]]$y))
        tempPlumeQuant <- CalcPlumeStats(model.tune$best.mod,
                                         AggDate = temp.time.eval,
                                         cL[[i]],
                                         plume_thresh = plume_thresh,
                                         type = as.character(panel$PredInterval),
                                         units = panel$rgUnits)
        
        cL[[i]]$Volume <- tempPlumeQuant$PlumeVol
        cL[[i]]$PlumeCentreofMass <- tempPlumeQuant$PlumeCentreofMass
        
      } else {
        
        cL[[i]]$area <- NA
        cL[[i]]$Volume <- NA
        cL[[i]]$PlumeCentreofMass <- NA
        
      }
      
    }
    
  }
  
  if (length(cL) > 0) { 
    PlumeDetails$cL = cL
  } else {
    PlumeDetails$cL = NULL
  }
  
  
  
  plume_stats <- list()
  plume_stats$plume_thresh <- plume_thresh
  
  if (length(PlumeDetails$cL) > 0) {
    
    plume_stats$area <- sum(unlist(lapply(PlumeDetails$cL,function(l){l$area})))
    plume_stats$volume <- sum(unlist(lapply(PlumeDetails$cL,function(l){l$Volume})))
    plume_stats$Mass <- plume_stats$volume * ground_porosity
    
    COMWeights <- unlist(lapply(PlumeDetails$cL,function(l){l$Volume}))/sum(unlist(lapply(PlumeDetails$cL,function(l){l$Volume})))
    plume_stats$PlumeCentreofMass <- rep(NA,2)
    plume_stats$PlumeCentreofMass[1] <- sum(COMWeights*unlist(lapply(PlumeDetails$cL,function(l){l$PlumeCentreofMass[1]})))
    plume_stats$PlumeCentreofMass[2] <- sum(COMWeights*unlist(lapply(PlumeDetails$cL,function(l){l$PlumeCentreofMass[2]})))
    
    
  } else {
    
    plume_stats$area <- NA
    plume_stats$volume <- NA
    plume_stats$Mass <- NA
    plume_stats$PlumeCentreofMass <- c(NA,NA)
    
  }
  
  
  return(plume_stats)
  
}

PlumeUnitHandlingFunc <- function(LengthUnit,rgUnits,PlumeMass,PlumeArea){
  
  if (is.null(LengthUnit)) {
    
    PlumeMass <- 1000*PlumeMass
    
    if (rgUnits == "ng/l") {PlumeMass <- PlumeMass*10^-12}
    if (rgUnits == "ug/l") {PlumeMass <- PlumeMass*10^-9}
    if (rgUnits == "mg/l") {PlumeMass <- PlumeMass*10^-6}
    
    PlumeMassUnits <- paste("(Mass/Unit Depth)",sep = "")
    PlumeAreaUnits <- paste("(Unit Area)",sep = "")
    PlumeAverageUnits <- paste("(",rgUnits,")",sep = "")
    
    
  } else {
    
    if (LengthUnit == "metres") {

      PlumeMass <- 1000*PlumeMass

      if (rgUnits == "ng/l") {PlumeMass <- PlumeMass * 10^-12}
      if (rgUnits == "ug/l") {PlumeMass <- PlumeMass * 10^-9}
      if (rgUnits == "mg/l") {PlumeMass <- PlumeMass * 10^-6}
      
      PlumeMassUnits <- paste("(kg/m)",sep = "")
      PlumeAreaUnits <- paste("(m^2)",sep = "")
      PlumeAverageUnits <- paste("(",rgUnits,")",sep = "")
      
    }
    
    
    if (LengthUnit == "feet") {
      
      PlumeMass <- 1000*PlumeMass/35.315 #per cubic ft
      
      if (rgUnits == "ng/l") {PlumeMass <- PlumeMass * 10^-12}
      if (rgUnits == "ug/l") {PlumeMass <- PlumeMass * 10^-9}
      if (rgUnits == "mg/l") {PlumeMass <- PlumeMass * 10^-6}
      
      PlumeMassUnits <- paste("(kg/ft)",sep = "")
      PlumeAreaUnits <- paste("(ft^2)",sep = "")
      PlumeAverageUnits <- paste("(",rgUnits,")",sep = "")
      
    }
    
  }
  
  return(list(PlumeMass = PlumeMass, PlumeArea = PlumeArea, 
              PlumeMassUnits = PlumeMassUnits, 
              PlumeAreaUnits = PlumeAreaUnits,
              PlumeAverageUnits = PlumeAverageUnits))
  
}


expandpoly <- function(mypol, fact) {
  
  m1 <- mean(mypol[, 1])
  m2 <- mean(mypol[, 2])
  cbind((mypol[, 1] - m1) * fact + m1, (mypol[, 2] - m2) * fact + m2)
  
}



CalcPlumeStats <- function(model, AggDate, cL, plume_thresh, type, units){
  
  temppts <- cbind(cL$x,cL$y)
  temppts <- gridpts(temppts,100)
  Plume.Tri.Points <- data.frame(XCoord = temppts[,1],YCoord = temppts[,2])
  Plume.Tri.Points$AggDate = as.numeric(AggDate)
  
  
  temppred <- predict(model, newdata = Plume.Tri.Points, se = (type != "Predicted"))
  if (type == "Lower 95% CI") {temppred$predicted <- temppred$predicted - temppred$predicted.sd*1.96}
  if (type == "Upper 95% CI") {temppred$predicted <- temppred$predicted + temppred$predicted.sd*1.96}
  
  Plume.Tri.Points$z <- exp(temppred$predicted)
  
  
  if(units=="mg/l"){Plume.Tri.Points$z<-Plume.Tri.Points$z/1000}
  if(units=="ng/l"){Plume.Tri.Points$z<-Plume.Tri.Points$z*1000}
  
  
  
  cL.Tri.Points<-data.frame(XCoord=cL$x,YCoord=cL$y,z=rep(plume_thresh,length(cL$x)))
  Vol.Tri.Points<-unique(rbind(Plume.Tri.Points[,c("XCoord","YCoord","z")],cL.Tri.Points))
  
  mydeldir<-deldir(x=Vol.Tri.Points$XCoord,y=Vol.Tri.Points$YCoord,z=Vol.Tri.Points$z)
  mytriangs<-triang.list(mydeldir)
  PlumeVol<-sum(unlist(lapply(mytriangs,VolIndTri)))
  xPlumeVol<-sum(unlist(lapply(mytriangs,xVolIndTri)))
  yPlumeVol<-sum(unlist(lapply(mytriangs,yVolIndTri)))
  PlumeCentreofMass<-c(xPlumeVol,yPlumeVol)/PlumeVol
  
  
  return(list(PlumeVol=PlumeVol,PlumeCentreofMass=PlumeCentreofMass))
  
}


xVolIndTri <- function(l){
  
  x <- l$x
  y <- l$y
  z <- l$z
  
  z = z*x
  
  0.5*(x[1]*(y[2] - y[3]) + x[2]*(y[3]- y[1]) + x[3]*(y[1]- y[2]))*(z[1] + z[2] + z[3]) / 3
}


yVolIndTri<-function(l){
  
  x<-l$x
  y<-l$y
  z<-l$z
  
  z=z*y
  
  0.5*(x[1]*(y[2] - y[3]) + x[2]*(y[3]- y[1]) + x[3]*(y[1]- y[2]))*(z[1] + z[2] + z[3]) / 3
}

checkPlumeClosure <- function(cl){
  
  cl$x[1] == cl$x[length(cl$x)] & cl$y[1] == cl$y[length(cl$y)]
  
}

VolIndTri <- function(l){
  
  x <- l$x
  y <- l$y
  z <- l$z
  
  0.5*(x[1]*(y[2] - y[3]) + x[2] * (y[3] - y[1]) + x[3] * (y[1] - y[2]))*(z[1] + z[2] + z[3]) / 3
}


printPlumeStatsCSV <- function(full_plume_stats, substance, length_unit, conc_unit, plume_thresh) {
  
  # Retrieve proper unit strings.
  tempUnitHandle <- PlumeUnitHandlingFunc(length_unit, conc_unit, NaN, NaN)
  
  # Add units to column names.
  names(full_plume_stats)[names(full_plume_stats) == "PlumeAverageConc"] <- paste("PlumeAverageConc", tempUnitHandle$PlumeAverageUnits, sep = "")
  names(full_plume_stats)[names(full_plume_stats) == "PlumeArea"] <- paste("PlumeArea", tempUnitHandle$PlumeAreaUnits, sep = "")
  names(full_plume_stats)[names(full_plume_stats) == "PlumeMass"] <- paste("PlumeMass", tempUnitHandle$PlumeMassUnits, sep = "")
 
  # Add a column with the plume threshold value.
  full_plume_stats[,"Plume Threshold Conc(ug/l)"] <- plume_thresh
  
  return(full_plume_stats)
 
}